module Types where

import Dummy.Abs

type ClassEnv = [Class]

data Class = Class
  { className :: String,
    typeConstraints :: [TyC],
    methods :: [(String, SType)]
  }

