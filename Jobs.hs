module Jobs (
    Status(Foreground, Background, Stopped),
    Job(Job),
    addJob,
    removeJob,
    getJobByPID,
    getJobByJID,
    setJobStatus,
    getFGJob,
    showJob,
    printJobs,
) where

import System.Posix.Types
import Data.List

type JobID = Int
type Command = String
data Status = Foreground | Background | Stopped deriving Show
data Job = Job ProcessID JobID Status Command deriving Show

nextJID :: JobID -> [Job] -> JobID
nextJID jid jobs =
    case getJobByJID jid jobs of
        Nothing   -> jid
        otherwise -> nextJID (jid + 1) jobs

addJob :: Job -> [Job] -> (Job, [Job])
addJob (Job pid _ status command) jobs = (job, (job:jobs))
    where job = Job pid (nextJID 1 jobs) status command

samePID :: ProcessID -> Job -> Bool
samePID pid (Job pid' _ _ _) = pid == pid'

removeJob :: ProcessID -> [Job] -> [Job]
removeJob pid jobs = filter (not . samePID pid) jobs

getJobByPID :: ProcessID -> [Job] -> Maybe Job
getJobByPID pid jobs = find (samePID pid) jobs

getJobByJID :: JobID -> [Job] -> Maybe Job
getJobByJID jid jobs = find (\(Job _ jid' _ _) -> jid == jid') jobs

setJobStatus :: ProcessID -> Status -> [Job] -> [Job]
setJobStatus pid newStatus jobs = map (setStatus pid newStatus) jobs
    where
        setStatus pid newStatus (Job pid' jid status command)
            | pid == pid' = Job pid jid newStatus command
            | pid /= pid' = Job pid jid status command

getFGJob :: [Job] -> Maybe Job
getFGJob jobs = find fgJob jobs
    where
        fgJob (Job _ _ Foreground _) = True
        fgJob _ = False

showJob :: Job -> String
showJob (Job pid jid status command) =
    "[" ++ (show jid) ++
    "] (" ++ (show pid) ++
    ") " ++ (show status) ++
    " " ++ command

printJobs :: [Job] -> String
printJobs jobs = intercalate "\n" $ map showJob jobs

