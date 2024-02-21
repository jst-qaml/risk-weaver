{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import RiskWeaver.Cmd.BDD qualified as BDD
import RiskWeaver.Cmd.Core qualified as Core

main :: IO ()
main = Core.baseMain BDD.bddCommand
