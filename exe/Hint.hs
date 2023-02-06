{-# LANGUAGE TemplateHaskell #-}

module Hint where

import GHC
-- import GHC.Core

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib as TH
-- import GHC.IO (unsafePerformIO)
import GHC.Types.Basic
import GHC.ThToHs
import Control.Monad.IO.Class
import GHC.Utils.Outputable hiding ((<>))
import qualified GHC.Utils.Outputable as GHC
import GHC.Runtime.Interpreter
    -- in
    -- . GHC.setBackendToInterpreter
import Control.Monad.Catch as MC
import Data.Maybe
import GHC.Driver.Ppr
import GHC.Core.Opt.Pipeline
import GHC.Driver.Config.Tidy
import GHC.Iface.Tidy
import GHC.Driver.Main
import GHC.Iface.Make
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Phases
import GHC.Driver.Pipeline
import GHC.Driver.Pipeline.Execute
import Unsafe.Coerce
-- import GHC.Driver.Env (HscEnv(hsc_interp), hsc_home_unit)
-- import GHC.Linker.Loader (loadModule)
-- import qualified GHC.Unit.Module
-- import GHC.Plugins hiding (outSDoc, (<>))
-- import Data.Maybe

-----------------------------
-- Utility things
outShow :: (Show a, MonadIO m) => a -> m ()
outShow = liftIO . putStrLn . show

outSDoc :: (Outputable a, MonadIO m) => a -> m ()
outSDoc x = liftIO . putStrLn . showSDocUnsafe $ ppr x

outString :: (MonadIO m) => String -> m ()
outString = liftIO . putStrLn

fullEvalOpts :: ExecOptions
fullEvalOpts = ExecOptions
    { execSingleStep = RunToCompletion
    , execSourceFile = "sthSrc"
    , execLineNumber = 0
    , execWrap = EvalThis }

-- | configureIntDynFlags dynamic use_oneshot
configureIntDynFlags :: Bool -> Bool -> GHC.DynFlags -> GHC.DynFlags
configureIntDynFlags _todo_is_dynamic use_oneshot dflags =
    dflags{
                                  -- Don't recompile deps just load them
                                  GHC.ghcMode    = if use_oneshot then GHC.OneShot else GHC.CompManager,
                                  -- Underdocuments but seems right for what we wanna do
                                  GHC.ghcLink    = GHC.LinkInMemory,
                                  GHC.verbosity  = 0
                                --   ways = add_dyn (GHC.ways dflags),
                                --   backend        = Interpreter
                                   }

withSavedSession :: GhcMonad m => m a -> m a
withSavedSession m = do
  saved_session <- getSession
  m `MC.finally` setSession saved_session

valToThExpr :: Lift a => a -> String -> IO Dec
valToThExpr val name_in = do
    dec <- runQ $ do
        let th_exp = lift val
            name = mkName name_in
            body = TH.normalB th_exp :: Q Body
            f_clause = TH.clause [] body [] :: Q Clause
            fun = funD name [f_clause] :: Q Dec
        fun
    return dec

thDeclToHsSyn :: Dec -> Ghc [LHsDecl GhcPs]
thDeclToHsSyn th_decl = do
    let res = convertToHsDecls Generated noSrcSpan [th_decl]
    case res of
        Left err -> outSDoc err >> error "Failed to convert"
        Right hs_syn -> pure hs_syn

-- | Insert the list of declarations into the given module.
--
-- Also marks als bindings as exported.
insertDecl :: ParsedModule -> [LHsDecl GhcPs] -> Ghc (ParsedModule,SDoc)
insertDecl p_mod decls = do
    let ParsedModule { pm_mod_summary, pm_parsed_source } = p_mod
    let parsed_mod = unLoc pm_parsed_source
        mod_loc = getLoc pm_parsed_source
    let HsModule
                { hsmodExports = _hsmodExports -- Should be Nothing to indicate export all
                , hsmodDecls -- actual declarations
                } = parsed_mod

    let parsed_mod' = parsed_mod { hsmodDecls = hsmodDecls <> decls
                                 , hsmodExports = Nothing -- Export everything
                                 }
    -- outString
    outSDoc $ hang (text "Generated a module:") 4 $ ppr parsed_mod'
    let l_pmod = L mod_loc parsed_mod'

    let mod_sum = pm_mod_summary { ms_parsed_mod = Nothing }
    return  (   p_mod { pm_mod_summary = mod_sum, pm_parsed_source = l_pmod}
            ,   ppr parsed_mod')

mkIIDecl :: ModuleName -> InteractiveImport
mkIIDecl = IIDecl . simpleImportDecl


-- | compile content mod_name decls
--
compileAsFileWithExtraDecls :: String -> String -> [LHsDecl GhcPs] -> Ghc ()
compileAsFileWithExtraDecls content modname extra_decls = do
    hsc_env <- getSession
    dflags <- getSessionDynFlags
    let wd = fromMaybe "." (workingDirectory dflags)
        hs_path = (wd <> "/" <> modname <> ".hs")

    liftIO $ writeFile hs_path content
    -- Parse
    setSessionDynFlags dflags
    target <- guessTarget modname Nothing Nothing
    setTargets [target]
    mod_graph <- depanal [] True
    let mod = mkModuleName modname
    mod_sum <- getModSummary mod
    parsed_in <- parseModule mod_sum

    -- Add the extra decls, update the src file in case ghc rereads it
    (parsed,updated_file_doc) <- insertDecl parsed_in extra_decls
    liftIO $ writeFile hs_path (showSDoc dflags updated_file_doc)

    -- Typecheck
    typed <- typecheckModule parsed
    desugared <- desugarModule typed
    let guts = dm_core_module desugared
    let location = ms_location mod_sum

    -- Optimize the core
    hsc_env <- getSession
    opt_guts <- liftIO $ core2core hsc_env guts
    -- outString "opt"
    -- outString $ showSDocUnsafe $ ppr (mg_binds opt_guts)

    -- Tidy core
    tidy_opts <- liftIO $ initTidyOpts hsc_env
    (cg_guts, mod_details) <- liftIO $ tidyProgram tidy_opts opt_guts


    -- Compile the core to a .s file
    (asm_out_fname, m_file2, c_stuff, stg_cg_infos, cmm_cg_infos) <- liftIO $ hscGenHardCode hsc_env cg_guts location "Main.s"
    -- outShow $ asm_out_fname
    -- outShow $ m_file2
    -- outShow c_stuff

    -- create iface
    let iface_core = (mkPartialIface hsc_env mod_details mod_sum opt_guts)
    iface_cg <- liftIO $ mkFullIface hsc_env iface_core stg_cg_infos cmm_cg_infos
    logger <- getLogger
    liftIO $ hscMaybeWriteIface logger dflags True iface_cg Nothing location


    -- Compile the assembly to object code
    let pipe_env = mkPipeEnv NoStop asm_out_fname Nothing Persistent
    o_file <- liftIO $ runAsPhase {-no cpp #-}False pipe_env hsc_env (Just location) asm_out_fname
    -- outString o_file
    outSDoc $ text "Compiled " GHC.<> text modname GHC.<> GHC.text "\n"

    return ()


-- | runFunFromMod module fun_name
--
-- Load the given module and execute the given function.
runFunFromMod :: String -> String -> Ghc ()
runFunFromMod mod_str fun_name = withSavedSession $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ configureIntDynFlags True False dflags

    -- outString "PreLoad"
    let mods = [mod_str]
    targets <- mapM (\n -> guessTarget n Nothing Nothing) mods
    -- outSDoc targets
    setTargets targets
    _res <- GHC.load LoadAllTargets

    setContext $ map (mkIIDecl . mkModuleName) $ ["Prelude"] <> mods
    -- outString "LoadedModules"

    _ <- execStmt fun_name fullEvalOpts

    return ()

-- | runFunFromMod module bind_name
--
-- print module.bind_name
showFromMod :: String -> String -> Ghc ()
showFromMod mod_str bind_name = withSavedSession $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ configureIntDynFlags True False dflags

    let mods = [mod_str]
    targets <- mapM (\n -> guessTarget n Nothing Nothing) mods
    -- outSDoc targets
    setTargets targets
    res <- GHC.load LoadAllTargets

    setContext $ map (mkIIDecl . mkModuleName) $ ["Prelude","Data.Maybe"] <> mods
    -- outString "LoadedModules"

    _ <- execStmt ("print bar") fullEvalOpts


    return ()

getValFromMod :: String -> String -> Ghc a
getValFromMod mod_str bind_name = do
    -- dflags <- getSessionDynFlags
    -- setSessionDynFlags $ configureIntDynFlags True False dflags

    let mods = [mod_str]
    targets <- mapM (\n -> guessTarget n Nothing Nothing) mods
    -- outSDoc targets
    setTargets targets
    res <- GHC.load LoadAllTargets

    setContext $ map (mkIIDecl . mkModuleName) $ ["Prelude"] <> mods
    -- outString "LoadedModules"

    result <- compileExpr (bind_name)

    -- Trick your type checker with this one weird trick.
    return $ unsafeCoerce result


-- Load a module and run it in the interpreter.
-- runFile :: Ghc ()
-- runFile = do
--     -- Parse, rename, typecheck and desugar
--     dflags <- getSessionDynFlags
--     setSessionDynFlags dflags
--     target <- guessTarget "Main.hs" Nothing Nothing
--     setTargets [target]
--     mod_graph <- depanal [] True
--     let mod_name = mkModuleName "Main"
--     mod_sum <- getModSummary mod_name
--     parsed <- parseModule mod_sum
--     liftIO $ putStrLn "parsed"
--     typed <- typecheckModule parsed
--     desugared <- desugarModule typed
--     let guts = dm_core_module desugared
--     let location = ms_location mod_sum

--     -- Optimize the core
--     hsc_env <- getSession
--     opt_guts <- liftIO $ core2core hsc_env guts
--     outString "opt"
--     outString $ showSDocUnsafe $ ppr (mg_binds opt_guts)

--     -- Tidy core
--     tidy_opts <- liftIO $ initTidyOpts hsc_env
--     (cg_guts, mod_details) <- liftIO $ tidyProgram tidy_opts opt_guts

--     -- Create iface
--     let partial_iface = mkPartialIface hsc_env mod_details mod_sum opt_guts
--     final_iface <- liftIO $ mkFullIface hsc_env partial_iface Nothing Nothing
--     logger <- getLogger
--     liftIO $ hscMaybeWriteIface logger dflags True final_iface Nothing location

--     -- generate bytecode
--     (hasStub, comp_bc, spt_entries) <- liftIO $ hscInteractive hsc_env cg_guts location

--     stub_o <- liftIO $ case hasStub of
--         Nothing -> return []
--         Just stub_c -> do
--             stub_o <- compileStub hsc_env stub_c
--             return [DotO stub_o]

--     let hs_unlinked = [BCOs comp_bc spt_entries]
--     unlinked_time <- liftIO $ getCurrentTime
--     let !linkable = LM unlinked_time (mkHomeModule (hsc_home_unit hsc_env) mod_name)
--                     (hs_unlinked ++ stub_o)

--     return ([], final_iface, Just linkable, panic "interpreter")


--     return (  )