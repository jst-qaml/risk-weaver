{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import RiskWeaver.Cmd.Core qualified as Core
import RiskWeaver.Cmd.BDD qualified as BDD

main :: IO ()
main = Core.baseMain BDD.bddCommand
