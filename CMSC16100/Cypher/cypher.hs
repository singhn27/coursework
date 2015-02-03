module Main where
    
import Data.Char
import System.Environment
import System.Exit
import System.IO

rot :: Int -> String -> String
rot n = map rot_char where
    rot_char :: Char -> Char
    rot_char c
        | isLower c = rot_case 'a' c
        | isUpper c = rot_case 'A' c
        | otherwise = c
    rot_case cas c = chr (ord cas + (ord c - ord cas + (n `mod` 26))  `rem` 26)

usage :: IO ()
usage = do
        progname <- getProgName
        hPutStrLn stderr $ "usage: " ++ progname ++ " n"
        exitWith $ ExitFailure 255

rot_stdin :: Int -> IO ()
rot_stdin n = interact $ rot n
    
main :: IO ()
main = do
        args <- getArgs
        case args of
            [] -> rot_stdin 13
            [x] -> if (not (null x) && all isDigit x) then rot_stdin (read x)
                   else usage
            _ -> usage
