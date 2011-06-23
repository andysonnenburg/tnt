{
{-# OPTIONS_GHC
    -fno-warn-lazy-unlifted-bindings
    -fno-warn-missing-signatures
    -fno-warn-name-shadowing
    -fno-warn-unused-binds
    -fno-warn-unused-matches#-}
module Language.TNT.Lexer
       ( Alex
       , AlexPosn (..)
       , alexError
       , alexGetInput
       , alexMonadScan
       , alexMonadScan'
       , runAlex
       ) where

import Data.Char

import Language.TNT.Token

import Prelude hiding (Ordering (..), and, or)
}

$unispace = \x05
$tab = \t
$whitechar = [\ \n\r\f\v $unispace $tab]

$ascdigit = 0-9
$unidigit = \x03
$decdigit = $ascdigit
$digit = [$ascdigit $unidigit]

$special = [\(\)\,\;\[\]\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol = \x04
$symbol = [$ascsymbol $unisymbol] # [$special \_\"\']

$unilarge = \x01
$asclarge = A-Z
$large = [$asclarge $unilarge]

$unismall = \x02
$ascsmall = a-z
$small = [$ascsmall $unismall \_]

$unigraphic = \x06
$graphic = [$small $large $symbol $digit $special $unigraphic \"\']

$symchar = $symbol
$nl = [\n\r]
$idchar = [$small $large $digit \']

@varid = [$small $large] $idchar*

@varsym = $symbol $symchar*

@decimal = $decdigit+
@exponent = [eE] [\-\+]? @decimal

@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent

@negative = \-
@signed = @negative?

tnt :-

$white+ ;

"//".* { lineCommentToken }

<0> {
  \( { special OpenParen }
  \) { special CloseParen }
  \[ { special OpenBracket }
  \] { special CloseBracket }
  \, { special Comma }
  \; { special Semi }
  \{ { special OpenBrace }
  \} { special CloseBrace }
  \. { special Period }
}

<0> {
  @varid { varid }
}

<0> {
  @varsym { varsym }
}

<0> {
  @decimal { tok_num positive 0 0 decimal }
  @floating_point { strtoken tok_float }
}

<0> {
  \' { lex_char_tok }
  \" { lex_string_tok }
}

<0> {
  "import" { special Import }
  "as" { special As }
  "var" { special Var }
  "fun" { special Fun }
  "if" { special If }
  "elif" { special Elif }
  "else" { special Else }
  "for" { special For }
  "in" { special In }
  "return" { special Return }
}

{

type Action = RealSrcSpan -> StringBuffer -> Int -> P (RealLocated Token)

special :: Token -> Action
special tok span _buf _len = return (L span tok)

strtoken :: (String -> Token) -> Action
strtoken f span buf len =
  return (L span $! (f $! lexemeToString buf len))

lineCommentToken :: Action
lineCommentToken span buf len = lexToken

varid :: Action
varid span buf len =
  fs `seq` return (L span (Name fs))
  where
    fs = lexemeToString buf len

varsym :: Action
varsym = sym Name

sym :: (String -> Token) -> RealSrcSpan -> StringBuffer -> Int ->
       P (RealLocated Token) 
sym con span buf len =
  return (L span $! con fs)
  where
    fs = lexemeToString buf len

tok_integral :: (Integer -> Token) -> (Integer -> Integer) -> Int -> Int ->
                (Integer, (Char -> Int)) -> Action
tok_integral itint transint transbuf translen (radix, char_to_int) span buf len =
  return $ L span $ itint $! transint $ parseUnsignedInteger
  (offsetBytes transbuf buf) (subtract translen len) radix char_to_int
  

tok_num :: (Integer -> Integer) -> Int -> Int -> (Integer, (Char -> Int)) ->
           Action
tok_num = tok_integral Integer

positive :: (Integer -> Integer)
positive = id

negative :: (Integer -> Integer)
negative = negate

decimal :: (Integer, Char -> Int)
decimal = (10, octDecDigit)

tok_float :: String -> Token
tok_float str = undefined

lex_string_tok :: Action
lex_string_tok span _buf _len = do
  tok <- lex_string ""
  end <- getSrcLoc
  return (L (mkRealSrcSpan (realSrcSpanStart span) end) tok)

lex_string :: String -> P Token
lex_string s = do
  i <- getInput
  case alexGetChar' i of
    Nothing -> lit_error i
    
    Just ('"', i) -> do
      setInput i
      return (String (reverse s))
    
    Just ('\\', i)
      | Just ('&', i) <- next -> do 
        setInput i
        lex_string s
      | Just (c, i) <- next, c <= '\x7f' && is_space c -> do
        setInput i
        lex_stringgap s
      where next = alexGetChar' i

    Just (c, i1) ->
      case c of
        '\\' -> do
          setInput i1
          c <- lex_escape
          lex_string (c:s)
        c | isAny c -> do
          setInput i1
          lex_string (c:s)
        _other -> lit_error i

lex_stringgap :: String -> P Token
lex_stringgap s = do
  i <- getInput
  c <- getCharOrFail i
  case c of
    '\\' -> lex_string s
    c | is_space c -> lex_stringgap s
    _other -> lit_error i

lex_char_tok :: Action
lex_char_tok span _buf _len = do
  i1 <- getInput
  let loc = realSrcSpanStart span
  case alexGetChar' i1 of
    Nothing -> lit_error i1
    
    Just ('\\', i2@(AI _end2 _)) -> do
      setInput i2
      lit_ch <- lex_escape
      i3 <- getInput
      mc <- getCharOrFail i3
      if mc == '\''
        then finish_char_tok loc lit_ch
        else lit_error i3
    
    Just (c, i2@(AI _end2 _))
      | not (isAny c) -> lit_error i1 
      | otherwise ->
        case alexGetChar' i2 of
          Just ('\'', i3) -> do
            setInput i3
            finish_char_tok loc c
          _other -> lit_error i2

finish_char_tok :: RealSrcLoc -> Char -> P (RealLocated Token)
finish_char_tok loc ch = do
  i@(AI end _) <- getInput
  return (L (mkRealSrcSpan loc end) (Char ch))

isAny :: Char -> Bool
isAny c | c > '\x7f' = isPrint c
        | otherwise = is_any c

lex_escape :: P Char
lex_escape = do
  i0 <- getInput
  c <- getCharOrFail i0
  case c of
    'a' -> return '\a'
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    'v' -> return '\v'
    '\\' -> return '\\'
    '"' -> return '\"'
    '\'' -> return '\''
    c1 -> lit_error i0

lit_error :: AlexInput -> P a
lit_error i = do
  setInput i
  lexError "lexical error in string/character literal"

getCharOrFail :: AlexInput -> P Char
getCharOrFail i =
  case alexGetChar' i of
    Nothing -> lexError "unexpected end-of-file in string/character literal"
    Just (c, i) -> do
      setInput i
      return c

data ParseResult a = POk PState a
                   | PFailed Src Span Message

data PState = PState
              { buffer :: StringBuffer
              , messages :: Messages
              , last_loc :: RealSrcSpan
              , last_len :: Int
              , loc :: RealSrcLoc
              , lex_state :: [Int]
              }

newtype P a = P { unP :: PState -> ParseResult a }

instance Monad P where
  return = return P
  (>>=) = thenP
  fail = failP

returnP :: a -> P a
returnP a = a `seq` (P $ \s -> POk s a)

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \s ->
  case m s of
    POk s1 a -> unP (k a) s1
    PFailed span err -> PFailed span err

failP :: String -> P a
failP msg = P $ \s -> PFailed (RealSrcSpan (last_loc s)) (text msg)

failLocMsgP :: RealSrcLoc -> RealSrcLoc -> String -> P a
failLocMsgP loc1 loc2 str = P $ \_ ->
  PFailed (RealSrcSpan (mkRealSrcSpan loc1 loc2)) (text str)

getSrcLoc :: P RealSrcLoc
getSrcLoc = P $ \s@PState { loc = l } -> POk s l

setLastToken :: RealSrcSpan -> Int -> P ()
setLastToken loc len = P $ \s -> POk s
  { last_loc = loc 
  , last_len = len
  } ()

data AlexInput = AI RealSrcLoc StringBuffer

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ buf) = prevChar buf '\n'

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (AI loc s)
  | atEnd s = Nothing
  | otherwise = adj_c `seq` loc' `seq` s' `seq`
                Just (adj_c, AI loc' s')
  where (c, s') = nextChar s
        loc' = advanceSrcLoc loc 
        
        non_graphic = '\x0'
        upper = '\x1'
        lower = '\x2'
        digit = '\x3'
        symbol = '\x4'
        space = '\x5'
        other_graphic = '\x6'
        
        adj_c
          | c <= '\x06' = non_graphic
          | c <= '\x7f' = c
          | otherwise =
            case generalCategory c of
              UppercaseLetter -> upper
              LowercaseLetter -> lower
              TitlecaseLetter -> upper
              ModifierLetter -> other_graphic
              OtherLetter -> lower
              NonSpacingMark -> other_graphic
              SpacingCombiningMark -> other_graphic
              EnclosingMark -> other_graphic
              DecimalNumber -> digit
              LetterNumber -> other_graphic
              OtherNumber -> digit
              ConnectorPunctuation -> symbol
              DashPunctuation -> symbol
              OpenPunctuation -> other_graphic
              ClosePunctuation -> other_graphic
              InitialQuote -> other_graphic
              FinalQuote -> other_graphic
              OtherPunctuation -> symbol
              MathSymbol -> symbol
              CurrencySymbol -> symbol
              ModifierSymbol -> symbol
              OtherSymbol -> symbol
              Space -> space
              _other -> non_graphic

alexGetChar' :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar' (AI loc s)
  | atEnd s = Nothing
  | otherwise = c `seq` loc' `seq` s' `seq`
                Just (c, AI loc' s')
  where (c, s') = nextChar s
        loc' = advanceSrcLoc loc c

getInput :: P AlexInput
getInput = P $ \s@PState { loc = l, buffer = b } -> POk s (AI l b)

setInput :: AlexInput -> P ()
setInput (AI l b) = P $ \s -> POk s { loc = l, buffer = b } ()

getLexState :: P Int
getLexState = P $ \s@PState { lex_state = ls:_ } -> POk s ls

lexError :: String -> P a
lexError str = do
  loc <- getSrcLoc
  AI end buf <- getInput
  reportLexError loc end buf str

lexToken :: P (RealLocated Token)
lexToken = do
  inp@(AI loc1 buf) <- getInput
  sc <- getLexState
  case alexScanUser inp sc of
    AlexEOF -> do
      let span = mkRealSrcSpan loc1 loc1
      setLastToken span 0
      return (L span EOF)
    AlexError (AI loc2 buf) ->
      reportLexError loc1 loc2 buf "lexical error"
    AlexSkip inp2 _ -> do
      setInput inp2
      lexToken
    AlexToken inp2@(AI end buf2) _ t -> do
      setInput inp2
      let span = mkRealSrcSpan loc1 end
      let bytes = byteDiff buf buf2
      span `seq` setLastToken span bytes
      setLastToken bytes
      t span buf bytes

reportLexError :: SrcLoc -> SrcLoc -> StringBuffer -> [Char] -> P a
reportLexError loc1 loc2 buf str
  | atEnd buf = failLocMsgP (str ++ " at end of input")
  | otherwise =
    let c = fst (nextChar buf)
    in if c == '\0'
       then failLocMsgP loc2 loc2 (str ++ " (UTF-8 decoding error)")
       else failLocMsgP loc1 loc2 (str ++ " at character " ++ show c) 
}