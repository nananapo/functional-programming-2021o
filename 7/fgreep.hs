import System.Environment
import Data.List

main = getArgs >>= \args -> 
       getContents >>= \cs -> 
       putStr $ fgrep (head args)

fgrep pattern cs unlines $ filter match $ lines cs
  where {
    match line = any prefixp $ tails line;
    prefixp line = pattern `isPrefixOf` line
  }

