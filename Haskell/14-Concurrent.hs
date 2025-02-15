{-# language ApplicativeDo #-}
import Control.Concurrent.Async
    ( Concurrently(Concurrently, runConcurrently) )
import Control.Applicative
-- To install parallel libraries:
-- cabal install --lib parallel
-- compile with:
-- ghc -threaded -O -o concurrent 14-Concurrent.hs
-- run with:
-- ./concurrent +RTS -N

f :: Concurrently Int
f = (+) <$> Concurrently (fileChars "1-Types.hs") 
        <*> Concurrently (fileChars "2-Composition.hs")
    where fileChars path = length <$> readFile path

f' :: Concurrently Int
f' = do
    m <- Concurrently (fileChars "1-Types.hs")
    n <- Concurrently (fileChars "2-Composition.hs")
    pure (m + n)
  where fileChars path = length <$> readFile path

main :: IO ()
main = runConcurrently f' >>= print

prompt :: String -> IO String
prompt str = putStrLn str *> getLine


greeting :: String -> String -> String
greeting s1 s2 = "Hi " ++ s1 ++ " " ++ s2 ++ "!"

getNamesAndGreet :: IO String
getNamesAndGreet = 
    greeting <$> prompt "First name: " <*> prompt "Last name: "
