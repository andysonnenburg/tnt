test :: Monad m => m Int
test = do
  x <- return 1
  y <- return 2
  return y
  
main = undefined