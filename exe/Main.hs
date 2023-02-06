{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import GHC
import GHC.Paths ( libdir )
import GHC.Driver.Session

import Control.Monad.IO.Class
-- import GHC.Core.Opt.Monad (getHscEnv)
import GHC.Core.Opt.Pipeline
import GHC.Plugins as GHC hiding (outSDoc, (<>))
import qualified GHC.Plugins as GHC
-- import GHC.CoreToStg.Prep
import GHC.Driver.Main
import GHC.Driver.Config.Tidy (initTidyOpts)
import GHC.Iface.Tidy (tidyProgram)
import GHC.Driver.Pipeline
import GHC.Driver.Phases
-- import GHC.Driver.Session
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Pipeline.Execute
import GHC.Iface.Make
import GHC.Linker.Types
import Data.Time (getCurrentTime)

import Hint
-- import DynFlags

import Unsafe.Coerce
import GHC.Runtime.Interpreter (EvalExpr(..))
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Lib as TH
import Language.Haskell.TH as TH

hiModuleContent :: String -> String
hiModuleContent mod_name = "module "<> mod_name <> " where \nfoo = print \"hi\"" :: String

main :: IO ()
main =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { workingDirectory = Just "rt-src" }

        -- Enable optimization
        dflags <- getSessionDynFlags
        (dflags,_,_) <- parseDynamicFlagsCmdLine dflags $
            map noLoc ["-O2", "-dno-typeable-binds", "-fobject-code"
                    --   , "-v"
                      ]
        setSessionDynFlags dflags

        -- Construct a regular haskell value *at runtime*
        number <- liftIO $ do
            putStrLn "Enter list length"
            n <- readLn :: IO Int
            return n

        -- Covert it to a parsed haskell expression via the Lift instance
        -- and give it the name "bar"
        val_desc <- liftIO $ valToThExpr (sum [1..number :: Int]) "bar"
        ps_desc <- thDeclToHsSyn val_desc

        -- Compile a string as a new module, inserting the generated declaration.
        compileAsFileWithExtraDecls (hiModuleContent "Jit2") "Jit2" ps_desc

        -- Load Jit2 into the interpreter, execute "bar" and return the result
        outString "Import the compiled module into the interpreter and return the value"
        int_result <- withSavedSession $ getValFromMod "Jit2" "bar" :: Ghc Int
        outShow int_result

        -- We can also construct functions to generate using TH of course
        let name = mkName "printGreeting"
            x_name = mkName "x"
            -- We can't just use `x` in brackets as printing the resulting names results in
            -- some odd UTF8 error.
            th_exp = [|\ $(varP x_name) -> print $ "Hello " ++ $(varE x_name) ++ "!"|]
            f_clause = TH.clause [] (TH.normalB th_exp) [] :: Q Clause
            fun = funD name [f_clause] :: Q Dec
        greet_decl <- liftIO $ runQ fun
        greet_hs <- thDeclToHsSyn greet_decl
        compileAsFileWithExtraDecls (hiModuleContent "Jit3") "Jit3" greet_hs

        printGreeting <- getValFromMod "Jit3" "printGreeting" :: Ghc (String -> IO ())
        outString "Enter a name for a greeting:"
        name <- liftIO $ getLine
        liftIO $ printGreeting name

        return ()

        -- withSavedSession $ runFunFromMod "Jit2" "foo"

_runStringWithMods :: [String] -> String -> Ghc ()
_runStringWithMods modules expr = do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ configureIntDynFlags True True dflags

    loadModules $ ["Prelude"] <> modules
    _ <- execStmt "import Jit1" fullEvalOpts
    result <- compileExpr expr :: Ghc HValue

    return ()

loadModules :: [String] -> Ghc ()
loadModules mods = do

    targets <- mapM (\n -> guessTarget n Nothing Nothing) mods
    outSDoc targets
    setTargets targets
    res <- GHC.load LoadAllTargets

    setContext $ map (mkIIDecl . mkModuleName) mods


    -- case res of
    --     Succeeded -> return ()
    --     Failed -> error "Foo_dagaeghasdfkhasdfwr4235"
    return ()

_runString :: Ghc ()
_runString = do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ configureIntDynFlags True True dflags

    outString "PreLoad"


    let mods = ["Jit1"]
    targets <- mapM (\n -> guessTarget n Nothing Nothing) mods
    outSDoc targets
    setTargets targets
    res <- GHC.load LoadAllTargets

    setContext $ map (mkIIDecl . mkModuleName) $ ["Prelude","Data.Maybe"] <> mods

    -- ,"Data.Maybe","Jit1"
    -- loadModules ["Prelude","Data.Maybe","Jit1"]

    outString "LoadedModules"
    -- expr <- liftIO $ do
    --     putStrLn "Enter expression evaling to a Int result"
    --     getLine
    -- let int_expr = "(" ++ expr ++ ") :: Int"
    -- result <- compileExpr int_expr :: Ghc HValue

    -- let result_int = (unsafeCoerce $! result) :: Int

    -- !exec_result <- execStmt int_expr fullEvalOpts

    -- expr <- liftIO $ do
    --     putStrLn "Enter expression evaling to a Int result"
    --     getLine
    -- let int_expr = "(" ++ expr ++ ") :: Int"

    -- !exec_result <- execStmt int_expr fullEvalOpts

    -- liftIO $ print $ result_int

    res_names <- runDecls "foo = fromMaybe"
    res_names <- runDecls "x = 1 :: Int"
    outSDoc $ res_names
    _ <- execStmt "print x" fullEvalOpts


    return ()

    -- runDecls ("foo = 1 :: Int")


