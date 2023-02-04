{-# LANGUAGE NamedFieldPuns #-}

module MyLib where

import GHC.Unit.Types
import GHC.Unit.Module.ModGuts as GHC.Guts
import GHC.Types.SrcLoc
import GHC.Core
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Data.FastString
import GHC.Unit.Module
import GHC.Unit.Module.Warnings
import GHC.Types.SourceFile
import GHC.Types.HpcInfo
import GHC.Types.Avail
import GHC.Types.Name.Reader
import GHC
import GHC.Types.ForeignStubs
import GHC.Unit.Module.Deps
import GHC.Types.Fixity.Env
-- import Language.Haskell.Syntax.Module.Name

jitUnitId :: UnitId
jitUnitId = UnitId (fsLit "jit")
jitUnit :: GenUnit UnitId
jitUnit = RealUnit (Definite jitUnitId)

mkJitModule :: Show a => a -> GenModule (GenUnit UnitId)
mkJitModule n = mkModule jitUnit (mkModuleName ("Jit" ++ show n))

mkSimpleAvails :: [Name] -> Avails
mkSimpleAvails names =
    let simplGreName name = NormalGreName name
    in map (Avail . simplGreName) names

mkGuts :: Int -> [Name] -> Dependencies
       -> [Usage]  -- I think we only need these for iface file creation
       -> [CoreBind]
       -> IO ModGuts
mkGuts index exports deps uses binds = do
    let mg_module = mkJitModule index
        mg_binds = binds

        mg_loc = noSrcSpan
        mg_hsc_src = HsSrcFile
        mg_exports = mkSimpleAvails exports
        mg_deps = deps
        mg_usages = uses
        mg_used_th = False -- For now no TH
        mg_rdr_env = emptyGlobalRdrEnv -- is this needed?
        mg_fix_env = emptyFixityEnv
        mg_tcs = [] -- TyCons from this module, not yet supported
        mg_insts = [] -- Class instances from this module
        mg_fam_insts = [] -- Data instances from this module
        mg_patsyns = [] -- Pat synonyms
        mg_rules = [] -- Rules
        mg_foreign = NoStubs
        mg_foreign_files = []
        mg_warns = NoWarnings
        mg_anns = []
        mg_complete_matches = []
        mg_hpc_info = emptyHpcInfo False -- Should be true if dependencies use hpc
        mg_modBreaks = Nothing
        mg_inst_env = emptyInstEnv
        mg_fam_inst_env = emptyFamInstEnv
        mg_boot_exports = mempty
        mg_safe_haskell = Sf_Ignore
        mg_trust_pkg = False
        mg_docs = Nothing

    return $
        ModGuts
            { mg_module
            , mg_binds
            , mg_loc
            , mg_hsc_src
            , mg_exports
            , mg_deps
            , mg_usages
            , mg_used_th
            , mg_rdr_env
            , mg_fix_env
            , mg_tcs
            , mg_insts
            , mg_fam_insts
            , mg_patsyns
            , mg_rules
            , mg_foreign
            , mg_foreign_files
            , mg_warns
            , mg_anns
            , mg_complete_matches
            , mg_hpc_info
            , mg_modBreaks
            , mg_inst_env
            , mg_fam_inst_env
            , mg_boot_exports
            , mg_safe_haskell
            , mg_trust_pkg
            , mg_docs
            }

-- mkModule :: Name -> CoreExpr ->

-- compileModule :: FilePath -> IO ()
-- compileModule file =


-- mkModule ::
someFunc :: IO ()
someFunc = putStrLn "someFunc"
