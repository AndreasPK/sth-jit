module Main (main) where

-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented."

{-# LANGUAGE CPP #-}
import GHC
import GHC.Paths ( libdir )
-- import DynFlags

main = return ()
-- main =
-- #if __GLASGOW_HASKELL__ > 704
--     defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
-- #else
--     defaultErrorHandler defaultLogAction $ do
-- #endif
--       runGhc (Just libdir) $ do
--         dflags <- getSessionDynFlags
--         setSessionDynFlags dflags
--         target <- guessTarget "rt-src/Main.hs" Nothing
--         setTargets [target]
--         mod_graph <- depanal [] True
--         undefined
--         -- load LoadAllTargets