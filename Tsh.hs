module Main (main) where

import System.Posix.Process
import System.Posix.Signals
import System.IO
import System.IO.Error
import Control.Concurrent
import Data.IORef
import Control.Exception
import Control.Monad
import System.Exit
import Data.Char
import System.Posix.Types

import Jobs

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

runProgram :: FilePath -> [String] -> IO ()
runProgram program args = do
    setProcessGroupIDOf 0 0
    catchJust
        (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
        (executeFile program True args Nothing)
        (\_ -> putStrLn $ program ++ ": Command not found")
    exitSuccess

parseCommand :: String -> (String, [String], Bool)
parseCommand command
    | last scommand == '&' = (cmd, args, False)
    | otherwise            = (cmd, args, True)
    where
        scommand = trim command
        split = words (if last scommand == '&' then init scommand else scommand)
        cmd = head split
        args = tail split

evalCommand :: String -> IORef [Job] -> IO Bool
evalCommand [] _ = return False
evalCommand line jobsvar = do
    case cmd of
        "quit" -> exitSuccess
        "jobs" -> do
            jobs <- readIORef jobsvar
            if length jobs > 0 then putStrLn $ printJobs jobs else return ()
            return False
        "fg" -> doFGBG True args jobsvar
        "bg" -> doFGBG False args jobsvar
        _ -> do
            pid <- forkProcess $ runProgram cmd args
            jobs <- readIORef jobsvar
            let (job, jobs') = addJob (Job pid 0 (if fg then Foreground else Background) line) jobs
            writeIORef jobsvar $ jobs'
            if not fg then putStrLn $ showJob job else return ()
            return fg
    where
        (cmd, args, fg) = parseCommand line

parseJobName :: String -> [Job] -> Maybe Job
parseJobName name jobs
    | head name == '%' =
        case reads (tail name) :: [(Int, String)] of
            [(num, "")] -> getJobByJID num jobs
            otherwise   -> Nothing
    | otherwise        =
        case reads name :: [(ProcessID, String)] of
            [(pid, "")] -> getJobByPID pid jobs
            otherwise   -> Nothing

doFGBG :: Bool -> [String] -> IORef [Job] -> IO Bool
doFGBG fg args jobsvar = do
    jobs <- readIORef jobsvar
    case parseJobName (head args) jobs of
        Just (Job pid _ _ _) -> do
            signalProcessGroup sigCONT pid
            writeIORef jobsvar $
                setJobStatus pid (if fg then Foreground else Background) jobs
            return fg
        _ -> do
            putStrLn "No job with that name"
            return False

sigChildHandler :: IORef [Job] -> IO ()
sigChildHandler jobsvar = do
    child <- tryJust (guard . isDoesNotExistError) $
        getAnyProcessStatus False True
    case child of
        Right (Just (pid, status)) -> do
            case status of
                System.Posix.Process.Exited _ -> do
                    jobs <- readIORef jobsvar
                    writeIORef jobsvar $ removeJob pid jobs
                System.Posix.Process.Terminated _ -> do
                    jobs <- readIORef jobsvar
                    writeIORef jobsvar $ removeJob pid jobs
                System.Posix.Process.Stopped _ -> do
                    jobs <- readIORef jobsvar
                    writeIORef jobsvar $ setJobStatus pid Jobs.Stopped jobs
            sigChildHandler jobsvar
        _                  -> return ()

sigIntHandler :: IORef [Job] -> IO ()
sigIntHandler jobsvar = do
    jobs <- readIORef jobsvar
    case getFGJob jobs of
        Just (Job pid _ _ _) -> signalProcessGroup sigINT pid
        Nothing -> return ()

sigStpHandler :: IORef [Job] -> IO ()
sigStpHandler jobsvar = do
    jobs <- readIORef jobsvar
    case getFGJob jobs of
        Just (Job pid _ _ _) -> signalProcessGroup sigTSTP pid
        Nothing -> return ()

setupHandlers :: IORef [Job] -> IO ()
setupHandlers jobsvar = do
    installHandler sigCHLD (Catch (sigChildHandler jobsvar)) Nothing
    installHandler sigINT (Catch (sigIntHandler jobsvar)) Nothing
    installHandler sigTSTP (Catch (sigStpHandler jobsvar)) Nothing
    return ()

waitFG :: IORef [Job] -> IO ()
waitFG jobsvar = do
    jobs <- readIORef jobsvar
    case getFGJob jobs of
        Just _  -> waitFG jobsvar
        Nothing -> return ()

doLoop :: IORef [Job] -> IO ()
doLoop jobsvar = do
    putStr "tsh> "
    hFlush stdout
    eof <- isEOF
    if eof then
        return ()
    else do
        line <- getLine
        wait <- evalCommand line jobsvar
        case wait of
            True -> waitFG jobsvar
            False -> return ()
        doLoop jobsvar

main :: IO ()
main = do
    jobsvar <- newIORef ([] :: [Job])
    setupHandlers jobsvar
    doLoop jobsvar

