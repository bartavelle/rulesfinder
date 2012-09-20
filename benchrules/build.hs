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
import Data.Either

import Text.Printf (printf)

import Mangling

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

lparseRuleFile :: String -> Action [String]
lparseRuleFile fname = do
    let flavor = if isSubstring "hashcat" fname
                then HashCat
                else JTR
    rules <- liftIO $ parseRuleFile flavor fname
    let bad  = lefts rules
        good = rights rules
        rawshowns = map (showRule JTR) good
        gshows = rights rawshowns
        bshows = lefts  rawshowns
    mapM_ putNormal bad
    mapM_ putNormal bshows
    return gshows

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
            rules <- lparseRuleFile rulefile
            liftIO $ removeIfExists lpot
            cracked <- mapM (checkrule johnexec args lconf) (take nbrules rules)
            writeFile' dst $ show cracked
            liftIO $ removeIfExists lconf
            liftIO $ removeIfExists lpot
