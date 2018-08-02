{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Data.Char as Char
import qualified Data.List as List
import           Distribution.Package (PackageIdentifier(..), unPackageName)
import           Distribution.PackageDescription (PackageDescription(package))
import           Distribution.Pretty (prettyShow)
import           Distribution.Simple (defaultMainWithHooks, simpleUserHooks, buildHook)
import           Distribution.Simple.BuildPaths (autogenPackageModulesDir)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import           Distribution.Simple.Setup (BuildFlags(buildVerbosity), fromFlag)
import           Distribution.Simple.Utils (notice, rewriteFileEx)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), (<.>))
import           System.IO.Error (catchIOError)
import           System.Process (readProcess)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pd lbi uh bf -> do generateMeta lbi bf; buildHook simpleUserHooks pd lbi uh bf
  }

generateMeta :: LocalBuildInfo -> BuildFlags -> IO ()
generateMeta lbi bf = let
    verbosity = fromFlag (buildVerbosity bf)
    PackageIdentifier {pkgName, pkgVersion} = package (localPkgDescr lbi)
    metaName = "Meta_" ++ replace '-' '_' (unPackageName pkgName)
    metaPath = autogen </> metaName <.> "hs"
  in do
    hash <- gitHash
    createDirectoryIfMissing True autogen
    notice verbosity ("Generating " ++ metaPath ++ " ...")
    rewriteFileEx verbosity metaPath (unlines
      [ "module " ++ metaName
      , "  ( name"
      , "  , version"
      , "  , hash"
      , "  ) where"
      , ""
      , "import Data.String (IsString(fromString))"
      , ""
      , "name :: IsString str => str"
      , "name = fromString " ++ show (unPackageName pkgName)
      , ""
      , "version :: IsString str => str"
      , "version = fromString " ++ show (prettyShow pkgVersion)
      , ""
      , "hash :: IsString str => str"
      , "hash = fromString " ++ show hash
      ])
 where
  autogen = autogenPackageModulesDir lbi
  replace x y =
    map (\c -> if c == x then y else c)

gitHash :: IO String
gitHash =
  catchIOError (fmap sanitize (readProcess "git" ["describe", "--always", "--dirty=-dirty"] ""))
               (\_ -> return "unknown")
 where
  sanitize = List.dropWhileEnd Char.isSpace
