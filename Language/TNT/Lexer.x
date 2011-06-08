{
module Language.TNT.Lexer
}

%wrapper "monad-bytestring"

:-

{

data Token = EOF

alexEOF = return EOF
}