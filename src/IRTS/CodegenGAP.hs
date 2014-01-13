{-# LANGUAGE PatternGuards #-}
module IRTS.CodegenGAP (codegenGAP, GAPTarget(..)) where

import Idris.AbsSyntax hiding (TypeCase)
import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.CodegenCommon
import Idris.Core.TT
import Paths_idris
import Util.System

import Control.Arrow
import Control.Applicative ((<$>), (<*>), pure)
import Data.Char
import Data.List
import Data.Maybe
import System.IO
import System.Directory

codegenGAP :: [(Name, SDecl)] ->
            String -> -- output file name
            OutputType ->   -- generate executable if True, only .o if False
            [FilePath] -> -- include files
            String -> -- extra object files
            String -> -- extra compiler flags (libraries)
            String -> -- extra compiler flags (anything)
            DbgLevel ->
            IO ()
codegenGAP defs out exec incs objs libs flags dbg
    = do -- print defs
         let bc = map toBC defs
         let h = concatMap toDecl (map fst bc)
         let cc = concatMap (uncurry toC) bc
         d <- getDataDir
         mprog <- readFile (d </> "rts" </> "idris_main" <.> "c")
         let cout = headers incs ++ debug dbg ++ h ++ cc ++
                     (if (exec == Executable) then mprog else "")
         case exec of
           MavenProject -> putStrLn ("FAILURE: output type not supported")
           Raw -> writeFile out cout
           _ -> do
             (tmpn, tmph) <- tempfile
             hPutStr tmph cout
             hFlush tmph
             hClose tmph
             let useclang = False
             comp <- getCC
             libFlags <- getLibFlags
             incFlags <- getIncFlags
             let gcc = comp ++ " " ++
                       gccDbg dbg ++ " " ++
                       gccFlags ++
                       " -I. " ++ objs ++ " -x c " ++
                       (if (exec == Executable) then "" else " -c ") ++
                       " " ++ tmpn ++
                       " " ++ libFlags ++
                       " " ++ incFlags ++
                       " " ++ libs ++
                       " " ++ flags ++
                       " -o " ++ out
--              putStrLn gcc
             exit <- system gcc
             when (exit /= ExitSuccess) $
                putStrLn ("FAILURE: " ++ gcc)



