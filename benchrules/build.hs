module Main where

import Prelude hiding (catch)
import System.IO.Error hiding (catch)
import Control.Exception

import System.Process hiding (readProcessWithExitCode)
import Development.Shake hiding (systemOutput)
import Development.Shake.FilePath

import Control.Monad
import System.IO
import System.Exit
import Control.Concurrent
import System.Environment
import System.Directory
import Data.List
import Data.String.Utils
import qualified Data.Map as Map
import Data.Char (chr,ord,isDigit,digitToInt,isUpper)

import Text.Printf (printf)

import Debug.Trace

substringP :: String -> String -> Maybe Int
substringP _ []  = Nothing
substringP sub str = case isPrefixOf sub str of
    False -> fmap (+1) $ substringP sub (tail str)
    True  -> Just 0

isSubstring :: String -> String -> Bool
isSubstring sub str = case substringP sub str of
    Just _  -> True
    Nothing -> False

-- utility function, for testing
convertrule :: String -> String -> IO ()
convertrule src dst = do
            let hashcat = isSubstring "hashcat" src
            rules <- fmap (concatMap (parserule hashcat) . lines) (readFile src)
            let out =  unlines ("[List.Rules:bencher]": rules)
            -- putStr out
            writeFile dst out


-- rewrite of systemOutput to handle binary flows
systemOutput :: FilePath -> [String] -> Action (String, String)
systemOutput path args = do
    let path2 = toNative path
    let cmd = unwords $ path2 : args
    putLoud cmd
    (res,sout,serr) <- traced (takeBaseName path) $ readProcessWithExitCode path2 args ""
    when (res /= ExitSuccess) $
        error $ "System command failed:\n" ++ cmd ++ "\n" ++ serr
    return (sout, serr)

readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    outMVar <- newEmptyMVar

    hSetBinaryMode inh True
    hSetBinaryMode outh True
    hSetBinaryMode errh True

    -- fork off a thread to start consuming stdout
    out  <- hGetContents outh
    _ <- forkIO $ evaluate (length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err <- hGetContents errh
    _ <- forkIO $ evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)

-- done

potfile :: String
potfile = "local.pot"

localconf :: String
localconf = "bencher.conf"

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

getDir :: String -> IO [String]
getDir dir = fmap (sort . filter (\x -> validName x && noswap x)) $ System.Directory.getDirectoryContents dir
    where
    validName = not . all (== '.')
    noswap = not . endswith ".swp"

{- a bit complicated, this finds preprocessor blocks and unroll them -}
data Tok = C [Char] | B [Char]
    deriving (Show)
parserule :: Bool -> String -> [String]
parserule hashcat r = let
    aparts = split " " $ filter (not . (== '\r')) r
    tokenize :: String -> [Tok]
    tokenize str =
        let (a,b) = break (\x -> (x == '\\') || (x == '[') ) str
        in  case b of
                ""          -> [C a]
                "\\"        -> [C (a ++ "\\")]
                ('\\':x:xs) -> let ((C ls):xs') = tokenize xs
                                   bit | (x==']') || (x=='[') = [x]
                                       | otherwise            = ['\\',x]
                               in  C (a ++ (bit ++ ls)) : xs'
                ('[':xs)    -> C a : tokenizeB xs
                _ -> error (show (b,r))
    tokenizeB :: String -> [Tok]
    tokenizeB str =
        let (a,b) = break (\x -> (x == '\\') || (x == ']') ) str
        in  case b of
                ""          -> error "Preprocessor block not closed!"
                "\\"        -> [B (a ++ "\\")]
                ('\\':x:xs) -> let ((B ls):xs') = tokenizeB xs
                               in  B (a ++ (x:ls)) : xs'
                (']':xs)    -> B a : tokenize xs
                _           -> error $ "Did not match " ++ b ++ " at tokenizeB"
    -- must be [ [single element], [multiple elements], [single element], ... ]
    pblock :: String -> [String]
    pblock str =
        let tstr = map tok2str $ tokenize str
            tok2str :: Tok -> [[String]]
            tok2str (C x) = [[x]]
            tok2str (B x) = foldl' (\cur c -> cur ++ [[[c]]]) [] x
        in map (concat . concat) $ sequence tstr
    escapeBrackets :: String -> String
    escapeBrackets "" = ""
    escapeBrackets ('[':xs) = "\\[" ++ escapeBrackets xs
    escapeBrackets (']':xs) = "\\]" ++ escapeBrackets xs
    escapeBrackets (x:xs) = x : escapeBrackets xs
    preprocess :: [String] -> [String]
    preprocess x = map escapeBrackets $ pblock $ unwords x
    rparts = filter (not . startswith "NBPWD=") aparts -- this is done to accomodate the output of the previous tool
    parts | hashcat = hashcatcheck rparts
          | otherwise = rparts
    in case parts of
        (('#':_):_) -> []
        []          -> []
        [a]         -> preprocess [a]
        (":":xs)    -> preprocess xs
        _           -> preprocess parts

-- converts from an to integers JtR positions
jtrPosDecode :: Char -> Int
jtrPosDecode x | isDigit x = (ord x) - (ord '0')
               | isUpper x = (ord x) - (ord 'A') + 10
               | otherwise = error $ "Can't convert JtR position " ++ [x]
jtrPosEncode :: Int -> Char
jtrPosEncode x | (x>=0) && (x<=9)  = chr (x + ord '0')
               | (x>=10) && (x<36) = chr (x - 10 + ord 'A')
               | otherwise         = error $ "Can't convert to JtR position " ++ show x

-- converts or remove hashcat rules
hashcatcheck :: [String] -> [String]
hashcatcheck parts =
    let badparts = any isbadpart parts
        conv = concatMap convpart parts
        isbadpart ('+':_) = True
        isbadpart ('-':_) = True
        isbadpart ('L':_) = True
        isbadpart ('R':_) = True
        isbadpart "q"     = True
        isbadpart _ = False
        convpart r =
            let nrule = convpart' r
            in  if nrule /= [r]
                    then trace ("Converting " ++ r ++ " to " ++ unwords nrule) nrule
                    else nrule
        convpart' ("p1") = ["d"]
        convpart' ['p', d1] =
            let nb = jtrPosDecode d1
            in  "val0" : replicate nb "X0aa"
        convpart' ['*', d1, d2] =
            let d2' = jtrPosEncode ((jtrPosDecode d2) + 1)
            in  [ ['X', d1, '1', d2]
                , ['D', d1]
                , ['X', d2, '1', d1]
                , ['D', d2']
                ]
        convpart' "k" = ["X012", "D0"]
        convpart' ['Z', d1] =
            let nb = jtrPosDecode d1
            in  "val1" : replicate nb "Xa1a"
        convpart' ['z', d1] =
            let nb = jtrPosDecode d1
            in  replicate nb "X010"
        convpart' "$"  = ["Az\" \""]
        convpart' "$ " = ["Az\" \""]
        convpart' "^"  = ["A0\" \""]
        convpart' "^ " = ["A0\" \""]
        convpart' ['Y', d1] = [ "val" ++ [d1]
                              , "Xa" ++ [d1] ++ "l" ]
        convpart' ['i', d1]         = [ "A" ++ [d1] ++ "\" \"" ]
        convpart' ['i', d1, ' ']    = [ "A" ++ [d1] ++ "\" \"" ]
        convpart' ['o', d1]         = [ "D" ++ [d1], "A" ++ [d1] ++ "\" \"" ]
        convpart' ['o', d1, ' ']    = [ "D" ++ [d1], "A" ++ [d1] ++ "\" \"" ]
        convpart' x = [x]
    in if badparts
        then trace ("Removing rule " ++ unwords parts) []
        else conv

parseRuleFile :: String -> Action [String]
parseRuleFile fname = fmap (concatMap (parserule (isSubstring "hashcat" fname)) . lines) (readFile' fname)

checkrule :: FilePath -> [String] -> String -> String -> Action Int
checkrule johnexec args lconf currule = do
    writeFileLines lconf ["[List.Rules:bencher]", currule]
    (_, err) <- systemOutput johnexec args
    let lns = lines err
        firstline = if null lns
                        then error "Empty output for JtR"
                        else head lns
    let val = case firstline of
                    ('g':'u':'e':'s':'s':'e':'s':':':' ':xs) ->
                            foldl' (\c n -> c*10 + digitToInt n) 0 $ takeWhile isDigit xs
                    _ -> error $ "john didn't return as expected: " ++ firstline
    putNormal (currule ++ " -> " ++ show val)
    return val

todum :: String -> String
todum x = "$dummy$" ++ foldl' (\cur c -> cur ++ printf "%02x" (ord c)) "" x

toDummy :: String -> String -> IO ()
toDummy src dst = withBinaryFile src ReadMode $ \fh -> (
    fmap (unlines . map todum . lines) (hGetContents fh)
        >>= writeFile dst )

main :: IO ()
main = do
    (johnexec:wordlist:anbrules:_) <- getArgs
    lrules <- getDir "rules"
    tests <- getDir "tests"
    let totest = [("results/" ++ r ++ "-" ++ t ++ "-" ++ anbrules, (r, t)) | r <- lrules, t <- tests]
        testmap = Map.fromList totest
        nbrules = read anbrules :: Int
    shake (shakeOptions{shakeThreads=4, shakeReport=Just "/tmp/rulebench.html"}) $ do
        want $ map fst totest
        "tmp/*" *> \dst -> do
            let basename = dropDirectory1 dst
                testfile = combine "tests" basename
            need [testfile]
            liftIO $ toDummy testfile dst

        "results/*" *> \dst -> do
            let (brulefile, btestfile) = testmap Map.! dst
                rulefile = combine "rules" brulefile
                testfile = combine "tmp" btestfile
                localid  = brulefile ++ "." ++ btestfile ++ "." ++ show nbrules
                lconf    = localconf ++ "." ++ localid
                lpot     = potfile   ++ "." ++ localid
                args = ["--format=dummy", "-rules:bencher", "-w:" ++ wordlist , "--config=" ++ lconf, "--pot:" ++ lpot, "--sess:" ++ localid,  testfile]
            need [johnexec, wordlist, testfile]
            rules <- parseRuleFile rulefile
            liftIO $ removeIfExists lpot
            cracked <- mapM (checkrule johnexec args lconf) (take nbrules rules)
            writeFile' dst $ show cracked
            liftIO $ removeIfExists lconf
            liftIO $ removeIfExists lpot
