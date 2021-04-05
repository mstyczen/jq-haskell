module Test (tests) where

import Distribution.TestSuite

import Parsing.Parsing
import Jq.Main as Jq

import Paths_JqClone

import Control.Concurrent (forkIO)
import Data.Maybe (isJust, fromJust)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.Process ( CreateProcess(..)
                      , CmdSpec(..)
                      , StdStream(CreatePipe)
                      , createProcess
                      , waitForProcess
                      , cleanupProcess
                      )
import GHC.IO.Handle (hPutStr, hGetContents, hClose)

data JTest = JTest {program :: String, input :: String, output :: [String]} deriving Show

comment :: Parser ()
comment = fmap (const ()) $ char '\n'
                         <|> do char '#'
                                many (sat (/='\n'))
                                char '\n'

test :: Parser JTest
test = do
  p <- some (sat (/='\n'))
  char '\n'
  i <- some (sat (/='\n'))
  char '\n'
  o <- tillemptyline
  return $ JTest p i o
  where
    tillemptyline = do
      h <- some (sat (/='\n'))
      (char '\n') <|> return '\n'
      t <- (tillemptyline <|> return [])
      return $ h : t

testsAndComments :: Parser [JTest]
testsAndComments =
  fmap (map fromJust . filter isJust) . many $  (fmap (const Nothing) comment)
                                            <|> (fmap Just test)

toparse :: IO String
toparse = do
  file <- getDataFileName "jq.test"
  r <- readFile file
  return r

alltests :: IO [JTest]
alltests = do
  t <- toparse
  return $ case (parse testsAndComments t) of
    [] -> []
    [(ts, [])] -> ts

type Program = (String -> String -> IO (ExitCode,String))

shell s = CreateProcess { cmdspec = ShellCommand s,
                          cwd = Nothing,
                          env = Nothing,
                          std_in = CreatePipe,
                          std_out = CreatePipe,
                          std_err = CreatePipe,
                          close_fds = False,
                          create_group = False,
                          delegate_ctlc = False,
                          detach_console = False,
                          create_new_console = False,
                          new_session = False,
                          child_group = Nothing,
                          child_user = Nothing,
                          use_process_jobs = False }

getProgram :: Maybe String -> Program
getProgram Nothing filter i =
  return $ case (Jq.process [filter] i) of
    Left _ -> (ExitFailure 1, "")
    Right out -> (ExitSuccess, out)
getProgram (Just s) filter i = do
  (Just stdin, Just stdout, _, p) <- createProcess $ shell $ s ++ " '" ++ filter ++ "'"
  hPutStr stdin i
  v <- hGetContents stdout
  e <- waitForProcess p
  return (e, v)

prettifyJSON :: String -> IO String
prettifyJSON s = do
  (Just stdin, Just stdout, _, p) <- createProcess $ shell $ "jq --sort-keys \".\""
  hPutStr stdin s
  v <- hGetContents stdout
  waitForProcess p
  return v

runTest :: Program -> JTest -> Test
runTest program t = Test $ res
  where
    JTest p i o = t
    res = TestInstance {
      run = do (exitcode, out) <- program p i
               op <- fmap concat . traverse prettifyJSON $ o
               return . Finished $ case exitcode of
                 ExitFailure n ->
                   Fail $  "Program terminated abnormally"
                 ExitSuccess ->
                   case op == out of
                     False -> Fail $  "The outputs don't match: "
                                     ++ "Expected: `"
                                     ++ op
                                     ++ "`, "
                                     ++ "Got: `"
                                     ++ out
                                     ++ "`."
                     True -> Pass
      , name = "input: `" ++ i ++ "`, filter: `" ++ p ++ "` "
      , tags = []
      , options = []
      , setOption = \_ -> \_ -> Right res}

tests :: IO [Test]
tests = do
  v <- getArgs
  let program = getProgram Nothing
  ts <- alltests
  return $ map (runTest program) ts
