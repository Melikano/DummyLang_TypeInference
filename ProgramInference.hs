module ProgramInference where

import ClassDecInference
import DefnInference
import Dummy.Abs
import InstanceDecInference
import Tests
import Types
import Utils

runJudge :: Prog -> Either String DefnEnv
runJudge (Dummy_Prog classDecs instanceDecs exps) = do
  let cenv = judgeClassDec classDecs
  let tyrel = judgeInstDec instanceDecs
  traceMonad tyrel
  judgeDefns cenv tyrel exps

