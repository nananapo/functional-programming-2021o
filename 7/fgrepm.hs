import System.Environment
import Data.List

main = getArgs >>= \args ->
       getContents >>= \cs ->
       putStr $ fgrepm args cs

fgrepm :: [String] -> String -> String
fgrepm [] cs = cs
fgrepm (p:ps) cs = fgrepm ps (fgrep p cs)

anny p [] = False
anny p (c:cs) = if p c then True else anny p cs

isPreefixOf :: String -> String -> Bool
isPreefixOf [] cs = True
isPreefixOf ps [] = False
isPreefixOf (p:ps) (c:cs) = if p == c then isPreefixOf ps cs else False

taails :: String -> [String]
taails [] = []
taails cs = [cs] ++ (taails $ tail cs)

match :: String -> String -> Bool
match p cs = anny (isPreefixOf p) $ taails cs

fgrep :: String -> String -> String
fgrep p = unlines . filter (match p) . lines
