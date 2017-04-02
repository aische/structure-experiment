module Dialog where


import           Data.Functor.Identity


import           Control.Structure


dialogPlain :: String -> IO String
dialogPlain s = putStrLn s >> getLine


dialogRead :: Read a => String -> IO a
dialogRead s = loop
  where
    loop = do
      putStrLn s
      s <- getLine
      case reads s of
        (n, ""):_ -> return n
        _         -> do
          putStrLn "wrong answer!"
          loop


runDialog :: Structure t => t IO -> IO (t Identity)
runDialog t = seqS (extendS t)
