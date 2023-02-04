module Main (main) where

{-# LANGUAGE CPP #-}
import GHC
import GHC.Paths ( libdir )
import GHC.Driver.Session

import Control.Monad.IO.Class
import GHC.Core.Opt.Monad (getHscEnv)
import GHC.Core.Opt.Pipeline
import GHC.Plugins
import GHC.CoreToStg.Prep
import GHC.Driver.Main
import GHC.Driver.Config.Tidy (initTidyOpts)
import GHC.Iface.Tidy (tidyProgram)
import GHC.Driver.Pipeline
import GHC.Driver.Phases
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Pipeline.Execute


