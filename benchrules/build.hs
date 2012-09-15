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
import Data.Char (ord,isDigit,digitToInt)

import Text.Printf (printf)

import Debug.Trace

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

data Tok = C [Char] | B [Char]
    deriving (Show)
parserule :: String -> [String]
parserule r = let
    aparts = split " " r
    tokenize :: String -> [Tok]
    tokenize str =
        let (a,b) = break (\x -> (x == '\\') || (x == '[') ) str
        in  case b of
                ""          -> [C a]
                ('\\':x:xs) -> let ((C ls):xs') = tokenize xs
                               in  C (x:ls) : xs'
                ('[':xs)    -> C a : tokenizeB xs
    tokenizeB :: String -> [Tok]
    tokenizeB str =
        let (a,b) = break (\x -> (x == '\\') || (x == ']') ) str
        in  case b of
                ""          -> error "Preprocessor block not closed!"
                ('\\':x:xs) -> let ((B ls):xs') = tokenizeB xs
                               in  B (x:ls) : xs'
                (']':xs)    -> B a : tokenize xs
    -- must be [ [single element], [multiple elements], [single element], ... ]
    pblock :: String -> [[String]]
    pblock str =
        let tstr = tokenize str
        in trace (show tstr) []
    preprocess :: [String] -> [String]
    preprocess x = let w = pblock $ unwords x
                    in concat w
    parts = filter (not . startswith "NBPWD=") aparts
    in case parts of
        (('#':_):_) -> []
        []          -> []
        [a]         -> preprocess [a]
        (":":xs)    -> preprocess xs
        _           -> preprocess parts


parseRuleFile :: String -> Action [String]
parseRuleFile fname = fmap (concatMap parserule . lines) (readFile' fname)

checkrule :: FilePath -> [String] -> String -> Action Int
checkrule johnexec args currule = do
    writeFileLines localconf ["[List.Rules:bencher]", currule]
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
    shake (shakeOptions{shakeThreads=1, shakeReport=Just "/tmp/rulebench.html"}) $ do
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
                args = ["--format=dummy", "--config=" ++ localconf, "-rules:bencher", "-w:" ++ wordlist , "--pot:" ++ potfile, testfile]
            need [johnexec, wordlist, testfile]
            rules <- parseRuleFile rulefile
            liftIO $ removeIfExists potfile
            cracked <- mapM (checkrule johnexec args) (take nbrules rules)
            writeFile' dst $ show cracked
