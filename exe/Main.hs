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
import GHC.Driver.Session
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Pipeline.Execute
-- import DynFlags

ps :: (Show a, MonadIO m) => a -> m ()
ps = liftIO . putStrLn . show

pss :: (MonadIO m) => String -> m ()
pss = liftIO . putStrLn

main =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { workingDirectory = Just "rt-src" }
        -- setWorkingDirectory "rt-src"
        compileMainDotHs


-- Compile "Main.hs" to a .o file, without emitting an interface file
compileMainDotHs :: Ghc ()
compileMainDotHs = do
        -- Parse, rename, typecheck and desugar
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        target <- guessTarget "Main.hs" Nothing Nothing
        setTargets [target]
        mod_graph <- depanal [] True
        let mod = mkModuleName "Main"
        mod_sum <- getModSummary mod
        parsed <- parseModule mod_sum
        liftIO $ putStrLn "parsed"
        typed <- typecheckModule parsed
        desugared <- desugarModule typed
        let guts = dm_core_module desugared

        -- Optimize the core
        hsc_env <- getSession
        opt_guts <- liftIO $ core2core hsc_env guts
        pss "opt"
        pss $ showSDocUnsafe $ ppr (mg_binds opt_guts)

        -- Tidy core
        tidy_opts <- liftIO $ initTidyOpts hsc_env
        (cg_guts, mod_details) <- liftIO $ tidyProgram tidy_opts opt_guts

        -- Compile the core to a .s file
        -- let mod_name = moduleName (ms_mod mod_sum)
        -- let src_flavour = (ms_hsc_src mod_sum)
        let location = ms_location mod_sum
        --  (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)], Maybe StgCgInfos, Maybe CmmCgInfos )
        (asm_out_fname, m_file2, c_stuff, stg_cg_infos, cmm_cg_infos) <- liftIO $ hscGenHardCode hsc_env cg_guts location "Main.s"
        ps $ asm_out_fname
        ps $ m_file2
        ps c_stuff

        -- Compile the assembly to object code
        let pipe_env = mkPipeEnv NoStop asm_out_fname Nothing Persistent
        o_file <- liftIO $ runAsPhase {-no cpp #-}False pipe_env hsc_env (Just location) asm_out_fname
        pss o_file

        return ()
