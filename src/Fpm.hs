{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Fpm
  ( Arguments(..)
  , Command(..)
  , getArguments
  , start
  )
where

import           Build                          ( buildLibrary
                                                , buildProgram
                                                )
import           Control.Monad.Extra            ( concatMapM )
import           Data.List                      ( nub )
import qualified Data.Map                      as Map
import qualified Data.Text.IO                  as TIO
import           Development.Shake              ( FilePattern
                                                , (<//>)
                                                , getDirectoryFilesIO
                                                )
import           Development.Shake.FilePath     ( (</>)
                                                , (<.>)
                                                , exe
                                                )
import           Options.Applicative            ( Parser
                                                , (<**>)
                                                , (<|>)
                                                , command
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , progDesc
                                                , subparser
                                                , switch
                                                )
import           System.Directory               ( doesDirectoryExist
                                                , doesFileExist
                                                , makeAbsolute
                                                , withCurrentDirectory
                                                )
import           System.Process                 ( runCommand
                                                , system
                                                )
import           Toml                           ( TomlCodec
                                                , (.=)
                                                )
import qualified Toml

data Arguments = Arguments { command' :: Command, release :: Bool }

data TomlSettings = TomlSettings {
      tomlSettingsProjectName :: String
    , tomlSettingsLibrary :: (Maybe Library)
    , tomlSettingsExecutables :: [Executable]
    , tomlSettingsTests :: [Executable]
    , tomlSettingsDependencies :: (Map.Map String Version)
    , tomlSettingsDevDependencies :: (Map.Map String Version)
}

data AppSettings = AppSettings {
      appSettingsCompiler :: String
    , appSettingsProjectName :: String
    , appSettingsBuildPrefix :: String
    , appSettingsFlags :: [String]
    , appSettingsLibrary :: (Maybe Library)
    , appSettingsExecutables :: [Executable]
    , appSettingsTests :: [Executable]
    , appSettingsDependencies :: (Map.Map String Version)
    , appSettingsDevDependencies :: (Map.Map String Version)
}

data Library = Library { librarySourceDir :: String }

data Executable = Executable {
      executableSourceDir :: String
    , executableMainFile :: String
    , executableName :: String
    , executableDependencies :: (Map.Map String Version)
} deriving Show

data Version = SimpleVersion String | GitVersion GitVersionSpec | PathVersion PathVersionSpec deriving Show

data GitVersionSpec = GitVersionSpec { gitVersionSpecUrl :: String, gitVersionSpecRef :: Maybe GitRef } deriving Show

data GitRef = Tag String | Branch String | Commit String deriving Show

data PathVersionSpec = PathVersionSpec { pathVersionSpecPath :: String } deriving Show

data Command = Run | Test | Build

data DependencyTree = Dependency {
      dependencyName :: String
    , dependencyPath :: FilePath
    , dependencySourcePath :: FilePath
    , dependencyDependencies :: [DependencyTree]
}

start :: Arguments -> IO ()
start args = do
  fpmContents <- TIO.readFile "fpm.toml"
  let tomlSettings = Toml.decode settingsCodec fpmContents
  case tomlSettings of
    Left  err           -> print err
    Right tomlSettings' -> do
      appSettings <- toml2AppSettings tomlSettings' (release args)
      app args appSettings

app :: Arguments -> AppSettings -> IO ()
app args settings = case command' args of
  Build -> build settings
  Run   -> do
    build settings
    let buildPrefix = appSettingsBuildPrefix settings
    let
      executableNames = map
        (\Executable { executableSourceDir = sourceDir, executableMainFile = mainFile, executableName = name } ->
          sourceDir </> name
        )
        (appSettingsExecutables settings)
    let executables =
          map (buildPrefix </>) $ map (flip (<.>) exe) executableNames
    canonicalExecutables <- mapM makeAbsolute executables
    case canonicalExecutables of
      [] -> putStrLn "No Executables Found"
      _  -> mapM_ system canonicalExecutables
  Test -> do
    build settings
    let buildPrefix = appSettingsBuildPrefix settings
    let
      executableNames = map
        (\Executable { executableSourceDir = sourceDir, executableMainFile = mainFile, executableName = name } ->
          sourceDir </> name
        )
        (appSettingsTests settings)
    let executables =
          map (buildPrefix </>) $ map (flip (<.>) exe) executableNames
    canonicalExecutables <- mapM makeAbsolute executables
    case canonicalExecutables of
      [] -> putStrLn "No Tests Found"
      _  -> mapM_ system canonicalExecutables

build :: AppSettings -> IO ()
build settings = do
  let compiler    = appSettingsCompiler settings
  let projectName = appSettingsProjectName settings
  let buildPrefix = appSettingsBuildPrefix settings
  let flags       = appSettingsFlags settings
  let executables = appSettingsExecutables settings
  let tests       = appSettingsTests settings
  mainDependencyTrees <- fetchDependencies (appSettingsDependencies settings)
  builtDependencies   <- buildDependencies buildPrefix
                                           compiler
                                           flags
                                           mainDependencyTrees
  (executableDepends, maybeTree) <- case appSettingsLibrary settings of
    Just librarySettings -> do
      let librarySourceDir' = librarySourceDir librarySettings
      let thisDependencyTree = Dependency
            { dependencyName         = projectName
            , dependencyPath         = "."
            , dependencySourcePath   = librarySourceDir'
            , dependencyDependencies = mainDependencyTrees
            }
      thisArchive <- buildLibrary librarySourceDir'
                                  [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                                  (buildPrefix </> projectName)
                                  compiler
                                  flags
                                  projectName
                                  (map fst builtDependencies)
      return
        $ ( (buildPrefix </> projectName, thisArchive) : builtDependencies
          , Just thisDependencyTree
          )
    Nothing -> do
      return (builtDependencies, Nothing)
  mapM_
    (\Executable { executableSourceDir = sourceDir, executableMainFile = mainFile, executableName = name, executableDependencies = dependencies } ->
      do
        localDependencies <-
          fetchExecutableDependencies maybeTree dependencies
            >>= buildDependencies buildPrefix compiler flags
        buildProgram
          sourceDir
          ((map fst executableDepends) ++ (map fst localDependencies))
          [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
          (buildPrefix </> sourceDir)
          compiler
          flags
          name
          mainFile
          ((map snd executableDepends) ++ (map snd localDependencies))
    )
    executables
  devDependencies <- fetchExecutableDependencies maybeTree (appSettingsDevDependencies settings) >>= buildDependencies buildPrefix compiler flags
  mapM_
    (\Executable { executableSourceDir = sourceDir, executableMainFile = mainFile, executableName = name, executableDependencies = dependencies } ->
      do
        localDependencies <-
          fetchExecutableDependencies maybeTree dependencies
            >>= buildDependencies buildPrefix compiler flags
        buildProgram
          sourceDir
          ((map fst executableDepends) ++ (map fst devDependencies) ++ (map fst localDependencies))
          [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
          (buildPrefix </> sourceDir)
          compiler
          flags
          name
          mainFile
          ((map snd executableDepends) ++ (map snd devDependencies) ++ (map snd localDependencies))
    )
    tests

getArguments :: IO Arguments
getArguments = execParser
  (info
    (arguments <**> helper)
    (fullDesc <> progDesc "Work with Fortran projects" <> header
      "fpm - A Fortran package manager and build system"
    )
  )

arguments :: Parser Arguments
arguments =
  Arguments
    <$> subparser
          (  command "run"  (info runArguments (progDesc "Run the executable"))
          <> command "test" (info testArguments (progDesc "Run the tests"))
          <> command "build"
                     (info buildArguments (progDesc "Build the executable"))
          )
    <*> switch (long "release" <> help "Build in release mode")

runArguments :: Parser Command
runArguments = pure Run

testArguments :: Parser Command
testArguments = pure Test

buildArguments :: Parser Command
buildArguments = pure Build

getDirectoriesFiles :: [FilePath] -> [FilePattern] -> IO [FilePath]
getDirectoriesFiles dirs exts = getDirectoryFilesIO "" newPatterns
 where
  newPatterns = concatMap appendExts dirs
  appendExts dir = map ((dir <//> "*") ++) exts

settingsCodec :: TomlCodec TomlSettings
settingsCodec =
  TomlSettings
    <$> Toml.string "name"
    .=  tomlSettingsProjectName
    <*> Toml.dioptional (Toml.table libraryCodec "library")
    .=  tomlSettingsLibrary
    <*> Toml.list executableCodec "executable"
    .=  tomlSettingsExecutables
    <*> Toml.list executableCodec "test"
    .=  tomlSettingsTests
    <*> Toml.tableMap Toml._KeyString versionCodec "dependencies"
    .=  tomlSettingsDependencies
    <*> Toml.tableMap Toml._KeyString versionCodec "dev-dependencies" .= tomlSettingsDevDependencies

libraryCodec :: TomlCodec Library
libraryCodec = Library <$> Toml.string "source-dir" .= librarySourceDir

executableCodec :: TomlCodec Executable
executableCodec =
  Executable
    <$> Toml.string "source-dir"
    .=  executableSourceDir
    <*> Toml.string "main"
    .=  executableMainFile
    <*> Toml.string "name"
    .=  executableName
    <*> Toml.tableMap Toml._KeyString versionCodec "dependencies"
    .=  executableDependencies

matchSimpleVersion :: Version -> Maybe String
matchSimpleVersion = \case
  SimpleVersion v -> Just v
  _               -> Nothing

matchGitVersion :: Version -> Maybe GitVersionSpec
matchGitVersion = \case
  GitVersion v -> Just v
  _            -> Nothing

matchPathVersion :: Version -> Maybe PathVersionSpec
matchPathVersion = \case
  PathVersion v -> Just v
  _             -> Nothing

matchTag :: GitRef -> Maybe String
matchTag = \case
  Tag v -> Just v
  _     -> Nothing

matchBranch :: GitRef -> Maybe String
matchBranch = \case
  Branch v -> Just v
  _        -> Nothing

matchCommit :: GitRef -> Maybe String
matchCommit = \case
  Commit v -> Just v
  _        -> Nothing

versionCodec :: Toml.Key -> Toml.TomlCodec Version
versionCodec key =
  Toml.dimatch matchSimpleVersion SimpleVersion (Toml.string key)
    <|> Toml.dimatch matchGitVersion GitVersion (Toml.table gitVersionCodec key)
    <|> Toml.dimatch matchPathVersion
                     PathVersion
                     (Toml.table pathVersionCodec key)

gitVersionCodec :: Toml.TomlCodec GitVersionSpec
gitVersionCodec =
  GitVersionSpec
    <$> Toml.string "git"
    .=  gitVersionSpecUrl
    <*> Toml.dioptional gitRefCodec
    .=  gitVersionSpecRef

gitRefCodec :: Toml.TomlCodec GitRef
gitRefCodec =
  Toml.dimatch matchTag Tag (Toml.string "tag")
    <|> Toml.dimatch matchBranch Branch (Toml.string "branch")
    <|> Toml.dimatch matchCommit Commit (Toml.string "rev")

pathVersionCodec :: Toml.TomlCodec PathVersionSpec
pathVersionCodec =
  PathVersionSpec <$> Toml.string "path" .= pathVersionSpecPath

toml2AppSettings :: TomlSettings -> Bool -> IO AppSettings
toml2AppSettings tomlSettings release = do
  let projectName = tomlSettingsProjectName tomlSettings
  let compiler    = "gfortran"
  librarySettings    <- getLibrarySettings $ tomlSettingsLibrary tomlSettings
  executableSettings <- getExecutableSettings
    (tomlSettingsExecutables tomlSettings)
    projectName
  testSettings <- getTestSettings $ tomlSettingsTests tomlSettings
  buildPrefix  <- makeBuildPrefix compiler release
  let dependencies = tomlSettingsDependencies tomlSettings
  let devDependencies = tomlSettingsDevDependencies tomlSettings
  return AppSettings
    { appSettingsCompiler     = compiler
    , appSettingsProjectName  = projectName
    , appSettingsBuildPrefix  = buildPrefix
    , appSettingsFlags        = if release
                                  then
                                    [ "-Wall"
                                    , "-Wextra"
                                    , "-Wimplicit-interface"
                                    , "-fPIC"
                                    , "-fmax-errors=1"
                                    , "-O3"
                                    , "-march=native"
                                    , "-ffast-math"
                                    , "-funroll-loops"
                                    ]
                                  else
                                    [ "-Wall"
                                    , "-Wextra"
                                    , "-Wimplicit-interface"
                                    , "-fPIC"
                                    , "-fmax-errors=1"
                                    , "-g"
                                    , "-fbounds-check"
                                    , "-fcheck-array-temporaries"
                                    , "-fbacktrace"
                                    ]
    , appSettingsLibrary      = librarySettings
    , appSettingsExecutables  = executableSettings
    , appSettingsTests        = testSettings
    , appSettingsDependencies = dependencies
    , appSettingsDevDependencies = devDependencies
    }

getLibrarySettings :: Maybe Library -> IO (Maybe Library)
getLibrarySettings maybeSettings = case maybeSettings of
  Just settings -> return maybeSettings
  Nothing       -> do
    defaultExists <- doesDirectoryExist "src"
    if defaultExists
      then return (Just (Library { librarySourceDir = "src" }))
      else return Nothing

getExecutableSettings :: [Executable] -> String -> IO [Executable]
getExecutableSettings [] projectName = do
  defaultDirectoryExists <- doesDirectoryExist "app"
  if defaultDirectoryExists
    then do
      defaultMainExists <- doesFileExist ("app" </> "main.f90")
      if defaultMainExists
        then return
          [ Executable { executableSourceDir    = "app"
                       , executableMainFile     = "main.f90"
                       , executableName         = projectName
                       , executableDependencies = Map.empty
                       }
          ]
        else return []
    else return []
getExecutableSettings executables _ = return executables

getTestSettings :: [Executable] -> IO [Executable]
getTestSettings [] = do
  defaultDirectoryExists <- doesDirectoryExist "test"
  if defaultDirectoryExists
    then do
      defaultMainExists <- doesFileExist ("test" </> "main.f90")
      if defaultMainExists
        then return
          [ Executable { executableSourceDir    = "test"
                       , executableMainFile     = "main.f90"
                       , executableName         = "runTests"
                       , executableDependencies = Map.empty
                       }
          ]
        else return []
    else return []
getTestSettings tests = return tests

makeBuildPrefix :: String -> Bool -> IO String
makeBuildPrefix compiler release =
  -- TODO Figure out what other info should be part of this
  --      Probably version, and make sure to not include path to the compiler
  return $ "build" </> compiler ++ "_" ++ if release then "release" else "debug"

{-
    Fetching the dependencies is done on a sort of breadth first approach. All
    of the dependencies are fetched before doing the transitive dependencies.
    This means that the top level dependencies dictate which version is fetched.
    The fetchDependency function is idempotent, so we don't have to worry about
    dealing with half fetched, or adding dependencies.
    TODO check for version compatibility issues
-}
fetchDependencies :: Map.Map String Version -> IO [DependencyTree]
fetchDependencies dependencies = do
  theseDependencies <- mapM (uncurry fetchDependency) (Map.toList dependencies)
  mapM fetchTransitiveDependencies theseDependencies
 where
  fetchTransitiveDependencies :: (String, FilePath) -> IO DependencyTree
  fetchTransitiveDependencies (name, path) = do
    tomlSettings     <- Toml.decodeFile settingsCodec (path </> "fpm.toml")
    librarySettingsM <- withCurrentDirectory path
      $ getLibrarySettings (tomlSettingsLibrary tomlSettings)
    case librarySettingsM of
      Just librarySettings -> do
        newDependencies <- fetchDependencies
          (tomlSettingsDependencies tomlSettings)
        return $ Dependency
          { dependencyName         = name
          , dependencyPath         = path
          , dependencySourcePath   = path </> (librarySourceDir librarySettings)
          , dependencyDependencies = newDependencies
          }
      Nothing -> do
        putStrLn $ "No library found in " ++ name
        undefined

fetchExecutableDependencies
  :: (Maybe DependencyTree) -> Map.Map String Version -> IO [DependencyTree]
fetchExecutableDependencies maybeProjectTree dependencies =
  case maybeProjectTree of
    Just projectTree@(Dependency name _ _ _) ->
      if name `Map.member` dependencies {- map contains this project-}
        then fmap (projectTree :)
                  (fetchDependencies (Map.delete name dependencies)) {- fetch the other dependencies and include the project tree in the result -}
        else do {- fetch all the dependencies, passing the project tree on down -}
          theseDependencies <- mapM (uncurry fetchDependency)
                                    (Map.toList dependencies)
          mapM fetchTransitiveDependencies theseDependencies
    Nothing -> fetchDependencies dependencies
 where
  fetchTransitiveDependencies :: (String, FilePath) -> IO DependencyTree
  fetchTransitiveDependencies (name, path) = do
    tomlSettings     <- Toml.decodeFile settingsCodec (path </> "fpm.toml")
    librarySettingsM <- withCurrentDirectory path
      $ getLibrarySettings (tomlSettingsLibrary tomlSettings)
    case librarySettingsM of
      Just librarySettings -> do
        newDependencies <- fetchExecutableDependencies
          maybeProjectTree
          (tomlSettingsDependencies tomlSettings)
        return $ Dependency
          { dependencyName         = name
          , dependencyPath         = path
          , dependencySourcePath   = path </> (librarySourceDir librarySettings)
          , dependencyDependencies = newDependencies
          }
      Nothing -> do
        putStrLn $ "No library found in " ++ name
        undefined

fetchDependency :: String -> Version -> IO (String, FilePath)
fetchDependency name version = do
  let clonePath = "build" </> "dependencies" </> name
  alreadyFetched <- doesDirectoryExist clonePath
  if alreadyFetched
    then return (name, clonePath)
    else case version of
      SimpleVersion _ -> do
        putStrLn "Simple dependencies are not yet supported :("
        undefined
      GitVersion versionSpec -> do
        system
          ("git clone " ++ gitVersionSpecUrl versionSpec ++ " " ++ clonePath)
        case gitVersionSpecRef versionSpec of
          Just ref -> withCurrentDirectory clonePath $ do
            system
              (  "git checkout "
              ++ (case ref of
                   Tag    tag    -> tag
                   Branch branch -> branch
                   Commit commit -> commit
                 )
              )
            return (name, clonePath)
          Nothing -> return (name, clonePath)
      PathVersion versionSpec -> return (name, pathVersionSpecPath versionSpec)

{-
    Bulding the dependencies is done on a depth first basis to ensure all of
    the transitive dependencies have been built before trying to build this one
-}
buildDependencies
  :: String
  -> String
  -> [String]
  -> [DependencyTree]
  -> IO [(FilePath, FilePath)]
buildDependencies buildPrefix compiler flags dependencies = do
  built <- concatMapM (buildDependency buildPrefix compiler flags) dependencies
  return $ nub built

buildDependency
  :: String -> String -> [String] -> DependencyTree -> IO [(FilePath, FilePath)]
buildDependency buildPrefix compiler flags (Dependency name path sourcePath dependencies)
  = do
    transitiveDependencies <- buildDependencies buildPrefix
                                                compiler
                                                flags
                                                dependencies
    let buildPath = buildPrefix </> name
    thisArchive <- buildLibrary sourcePath
                                [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                                buildPath
                                compiler
                                flags
                                name
                                (map fst transitiveDependencies)
    return $ (buildPath, thisArchive) : transitiveDependencies
