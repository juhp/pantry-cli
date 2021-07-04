-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Distribution.Pretty
import Distribution.Types.PackageName
import Network.HTTP.Query
import Pantry
import RIO
import SimpleCmd
import SimpleCmdArgs
import System.Cached.JSON

data Command = Compiler | List

main :: IO ()
main = do
  simpleCmdArgs Nothing "Pantry CLI" "Query Stackage snapshots with pantry" $
    subcommands
    [ Subcommand "compiler" "Get compiler version of snapshot" $
      runCmd Compiler <$> strArg "SNAPSHOT"
    , Subcommand "list" "List snapshot packages" $
      runCmd List <$> strArg "SNAPSHOT"
    , Subcommand "pkg" "Show snapshot package" $
      showPackage <$> strArg "SNAPSHOT" <*> strArg "PACKAGE"
    ]

-- FIXME dunno how to catch (try) for 404 exception
getSnapshot :: T.Text -> IO RawSnapshot
getSnapshot snap = do
  snapfinal <- resolveSnapshot
  runPantryApp $ do
    snapshot <- parseSnapName snapfinal
    rurl <- snapshotLocation snapshot
    url <- completeSnapshotLocation rurl
    loadSnapshot url
  where
    resolveSnapshot :: IO T.Text
    resolveSnapshot = do
        snapshots <- getCachedJSON "stackage-snapshots" "snapshots.json" "http://haddock.stackage.org/snapshots.json" 200
        return $ fromMaybe snap $ lookupKey snap snapshots

runCmd :: Command -> T.Text -> IO ()
runCmd com snap = do
  rsnap <- getSnapshot snap
  case com of
    Compiler -> T.putStrLn $ textDisplay $ rsCompiler rsnap
    List -> do
      let pkgmap = rsPackages rsnap
      mapM_ (putStrLn . packageVerId) $ M.elems pkgmap

packageVerId :: RawSnapshotPackage -> String
packageVerId rsp =
  case rspLocation rsp of
    RPLIHackage (PackageIdentifierRevision n v _) _ ->
      prettyShow (PackageIdentifier n v)
    _ -> error "non-Hackage package"

showPackage :: T.Text -> String -> IO ()
showPackage snap pkg = do
  rsnap <- getSnapshot snap
  let pkgmap = rsPackages rsnap
  case M.lookup (mkPackageName pkg) pkgmap of
    Just rsp -> putStrLn (packageVerId rsp)
    Nothing -> error' $ pkg ++ " not found in " ++ T.unpack snap
