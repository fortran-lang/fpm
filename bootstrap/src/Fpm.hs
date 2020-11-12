{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Fpm
  ( Arguments(..)
  , getArguments
  , start
  )
where

import           Build                          ( CompilerSettings(..)
                                                , buildLibrary
                                                , buildProgram
                                                , buildWithScript
                                                )
import           Control.Monad.Extra            ( concatMapM
                                                , forM_
                                                , when
                                                )
import           Data.Hashable                  ( hash )
import           Data.List                      ( intercalate
                                                , isInfixOf
                                                , isSuffixOf
                                                , find
                                                , nub
                                                )
import qualified Data.Map                      as Map
import qualified Data.Text.IO                  as TIO
import           Development.Shake              ( FilePattern
                                                , (<//>)
                                                , getDirectoryFilesIO
                                                )
import           Development.Shake.FilePath     ( (</>)
                                                , (<.>)
                                                , exe
                                                , splitDirectories
                                                )
import           Numeric                        ( showHex )
import           Options.Applicative            ( Parser
                                                , (<**>)
                                                , (<|>)
                                                , auto
                                                , command
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , many
                                                , metavar
                                                , option
                                                , optional
                                                , progDesc
                                                , short
                                                , showDefault
                                                , strArgument
                                                , strOption
                                                , subparser
                                                , switch
                                                , value
                                                )
import           System.Directory               ( createDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                , makeAbsolute
                                                , withCurrentDirectory
                                                )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           System.Process                 ( readProcess
                                                , system
                                                )
import           Toml                           ( TomlCodec
                                                , (.=)
                                                )
import qualified Toml

data Arguments =
    New
      { newName :: String
      , newWithExecutable :: Bool
      , newWithTest :: Bool
      , newWithLib :: Bool
      }
  | Build
      { buildRelease :: Bool
      , buildCompiler :: FilePath
      , buildFlags :: [String]
      }
  | Run
      { runRelease :: Bool
      , runCompiler :: FilePath
      , runFlags :: [String]
      , runRunner :: Maybe String
      , runTarget :: Maybe String
      , runArgs :: Maybe [String]
      }
  | Test
      { testRelease :: Bool
      , testCompiler :: FilePath
      , testFlags :: [String]
      , testRunner :: Maybe String
      , testTarget :: Maybe String
      , testArgs :: Maybe [String]
      }

data TomlSettings = TomlSettings {
      tomlSettingsProjectName :: String
    , tomlSettingsLibrary :: (Maybe Library)
    , tomlSettingsExecutables :: [Executable]
    , tomlSettingsTests :: [Executable]
    , tomlSettingsDependencies :: (Map.Map String Version)
    , tomlSettingsDevDependencies :: (Map.Map String Version)
}

data AppSettings = AppSettings {
      appSettingsCompiler :: CompilerSettings
    , appSettingsProjectName :: String
    , appSettingsBuildPrefix :: String
    , appSettingsLibrary :: (Maybe Library)
    , appSettingsExecutables :: [Executable]
    , appSettingsTests :: [Executable]
    , appSettingsDependencies :: (Map.Map String Version)
    , appSettingsDevDependencies :: (Map.Map String Version)
}

data Library = Library { librarySourceDir :: String, libraryBuildScript :: Maybe String }

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

data DependencyTree = Dependency {
      dependencyName :: String
    , dependencyPath :: FilePath
    , dependencySourcePath :: FilePath
    , dependencyBuildScript :: Maybe String
    , dependencyDependencies :: [DependencyTree]
}

start :: Arguments -> IO ()
start args = case args of
  New { newName = name, newWithExecutable = withExecutable, newWithTest = withTest, newWithLib = withLib }
    -> createNewProject name withExecutable withTest withLib
  _ -> do
    fpmContents <- TIO.readFile "fpm.toml"
    let tomlSettings = Toml.decode settingsCodec fpmContents
    case tomlSettings of
      Left  err           -> print err
      Right tomlSettings' -> do
        appSettings <- toml2AppSettings tomlSettings' args
        app args appSettings

app :: Arguments -> AppSettings -> IO ()
app args settings = case args of
  Build{} -> build settings
  Run { runTarget = whichOne, runArgs = runArgs, runRunner = runner } -> do
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
      _ ->
        let commandPrefix = case runner of
              Nothing -> ""
              Just r  -> r ++ " "
            commandSufix = case runArgs of
              Nothing -> ""
              Just a  -> " " ++ (intercalate " " a)
        in  case whichOne of
              Nothing -> do
                exitCodes <- mapM
                  system
                  (map (\exe -> commandPrefix ++ exe ++ commandSufix)
                       canonicalExecutables
                  )
                forM_
                  exitCodes
                  (\exitCode -> when
                    (case exitCode of
                      ExitSuccess -> False
                      _           -> True
                    )
                    (exitWith exitCode)
                  )
              Just name -> do
                case find (name `isSuffixOf`) canonicalExecutables of
                  Nothing        -> putStrLn "Executable Not Found"
                  Just specified -> do
                    exitCode <- system
                      (commandPrefix ++ specified ++ commandSufix)
                    exitWith exitCode
  Test { testTarget = whichOne, testArgs = testArgs, testRunner = runner } ->
    do
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
        _ ->
          let commandPrefix = case runner of
                Nothing -> ""
                Just r  -> r ++ " "
              commandSufix = case testArgs of
                Nothing -> ""
                Just a  -> " " ++ (intercalate " " a)
          in  case whichOne of
                Nothing -> do
                  exitCodes <- mapM
                    system
                    (map (\exe -> commandPrefix ++ exe ++ commandSufix)
                         canonicalExecutables
                    )
                  forM_
                    exitCodes
                    (\exitCode -> when
                      (case exitCode of
                        ExitSuccess -> False
                        _           -> True
                      )
                      (exitWith exitCode)
                    )
                Just name -> do
                  case find (name `isSuffixOf`) canonicalExecutables of
                    Nothing        -> putStrLn "Test Not Found"
                    Just specified -> do
                      exitCode <- system
                        (commandPrefix ++ specified ++ commandSufix)
                      exitWith exitCode
  _ -> putStrLn "Shouldn't be able to get here"

build :: AppSettings -> IO ()
build settings = do
  let compilerSettings = appSettingsCompiler settings
  let projectName      = appSettingsProjectName settings
  let buildPrefix      = appSettingsBuildPrefix settings
  let executables      = appSettingsExecutables settings
  let tests            = appSettingsTests settings
  mainDependencyTrees <- fetchDependencies (appSettingsDependencies settings)
  builtDependencies   <- buildDependencies buildPrefix
                                           compilerSettings
                                           mainDependencyTrees
  (executableDepends, maybeTree) <- case appSettingsLibrary settings of
    Just librarySettings -> do
      let librarySourceDir' = librarySourceDir librarySettings
      let thisDependencyTree = Dependency
            { dependencyName         = projectName
            , dependencyPath         = "."
            , dependencySourcePath   = librarySourceDir'
            , dependencyBuildScript  = libraryBuildScript librarySettings
            , dependencyDependencies = mainDependencyTrees
            }
      thisArchive <- case libraryBuildScript librarySettings of
        Just script -> buildWithScript script
                                       "."
                                       (buildPrefix </> projectName)
                                       compilerSettings
                                       projectName
                                       (map fst builtDependencies)
        Nothing -> buildLibrary librarySourceDir'
                                [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                                (buildPrefix </> projectName)
                                compilerSettings
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
            >>= buildDependencies buildPrefix compilerSettings
        buildProgram
          sourceDir
          ((map fst executableDepends) ++ (map fst localDependencies))
          [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
          (buildPrefix </> sourceDir)
          compilerSettings
          name
          mainFile
          ((map snd executableDepends) ++ (map snd localDependencies))
    )
    executables
  devDependencies <-
    fetchExecutableDependencies maybeTree (appSettingsDevDependencies settings)
      >>= buildDependencies buildPrefix compilerSettings
  mapM_
    (\Executable { executableSourceDir = sourceDir, executableMainFile = mainFile, executableName = name, executableDependencies = dependencies } ->
      do
        localDependencies <-
          fetchExecutableDependencies maybeTree dependencies
            >>= buildDependencies buildPrefix compilerSettings
        buildProgram
          sourceDir
          (  (map fst executableDepends)
          ++ (map fst devDependencies)
          ++ (map fst localDependencies)
          )
          [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
          (buildPrefix </> sourceDir)
          compilerSettings
          name
          mainFile
          (  (map snd executableDepends)
          ++ (map snd devDependencies)
          ++ (map snd localDependencies)
          )
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
arguments = subparser
  (  command
      "new"
      (info (newArguments <**> helper)
            (progDesc "Create a new project in a new directory")
      )
  <> command
       "build"
       (info (buildArguments <**> helper) (progDesc "Build the project"))
  <> command
       "run"
       (info (runArguments <**> helper) (progDesc "Run the executable(s)"))
  <> command "test"
             (info (testArguments <**> helper) (progDesc "Run the test(s)"))
  )

newArguments :: Parser Arguments
newArguments =
  New
    <$> strArgument
          (  metavar "NAME"
          <> help "Name of new project (must be a valid Fortran identifier)"
          )
    <*> switch (long "app" <> help "Include an executable")
    <*> switch (long "test" <> help "Include a test")
    <*> switch (long "lib" <> help "Include a library")

buildArguments :: Parser Arguments
buildArguments =
  Build
    <$> switch
          (  long "release"
          <> help "Build with optimizations instead of debugging"
          )
    <*> strOption
          (  long "compiler"
          <> metavar "COMPILER"
          <> value "gfortran"
          <> help "specify the compiler to use"
          <> showDefault
          )
    <*> many
          (strOption
            (  long "flag"
            <> metavar "FLAG"
            <> help
                 "specify an addional argument to pass to the compiler (can appear multiple times)"
            )
          )

runArguments :: Parser Arguments
runArguments =
  Run
    <$> switch
          (  long "release"
          <> help "Build with optimizations instead of debugging"
          )
    <*> strOption
          (  long "compiler"
          <> metavar "COMPILER"
          <> value "gfortran"
          <> help "specify the compiler to use"
          <> showDefault
          )
    <*> many
          (strOption
            (  long "flag"
            <> metavar "FLAG"
            <> help
                 "specify an addional argument to pass to the compiler (can appear multiple times)"
            )
          )
    <*> optional
          (strOption
            (long "runner" <> metavar "RUNNER" <> help
              "specify a command to be used to run the executable(s)"
            )
          )
    <*> optional
          (strOption
            (long "target" <> metavar "TARGET" <> help
              "Name of the executable to run"
            )
          )
    <*> optional
          (many
            (strArgument
              (  metavar "ARGS"
              <> help "Arguments to the executable(s) (should follow '--')"
              )
            )
          )

testArguments :: Parser Arguments
testArguments =
  Test
    <$> switch
          (  long "release"
          <> help "Build with optimizations instead of debugging"
          )
    <*> strOption
          (  long "compiler"
          <> metavar "COMPILER"
          <> value "gfortran"
          <> help "specify the compiler to use"
          <> showDefault
          )
    <*> many
          (strOption
            (  long "flag"
            <> metavar "FLAG"
            <> help
                 "specify an addional argument to pass to the compiler (can appear multiple times)"
            )
          )
    <*> optional
          (strOption
            (long "runner" <> metavar "RUNNER" <> help
              "specify a command to be used to run the test(s)"
            )
          )
    <*> optional
          (strOption
            (long "target" <> metavar "TARGET" <> help "Name of the test to run"
            )
          )
    <*> optional
          (many
            (strArgument
              (  metavar "ARGS"
              <> help "Arguments to the test(s) (should follow '--')"
              )
            )
          )

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
    <*> Toml.tableMap Toml._KeyString versionCodec "dev-dependencies"
    .=  tomlSettingsDevDependencies

libraryCodec :: TomlCodec Library
libraryCodec =
  Library
    <$> Toml.string "source-dir"
    .=  librarySourceDir
    <*> Toml.dioptional (Toml.string "build-script")
    .=  libraryBuildScript

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

toml2AppSettings :: TomlSettings -> Arguments -> IO AppSettings
toml2AppSettings tomlSettings args = do
  let release = case args of
        Build { buildRelease = r } -> r
        Run { runRelease = r }     -> r
        Test { testRelease = r }   -> r
  let projectName = tomlSettingsProjectName tomlSettings
  let compiler = case args of
        Build { buildCompiler = c } -> c
        Run { runCompiler = c }     -> c
        Test { testCompiler = c }   -> c
  let specifiedFlags = case args of
        Build { buildFlags = f } -> f
        Run { runFlags = f }     -> f
        Test { testFlags = f }   -> f
  when (release && (length specifiedFlags > 0)) $ do
    putStrLn "--release and --flag are mutually exclusive"
    exitWith (ExitFailure 1)
  librarySettings    <- getLibrarySettings $ tomlSettingsLibrary tomlSettings
  executableSettings <- getExecutableSettings
    (tomlSettingsExecutables tomlSettings)
    projectName
  testSettings <- getTestSettings $ tomlSettingsTests tomlSettings
  compilerSettings <- defineCompilerSettings specifiedFlags compiler release
  buildPrefix <- makeBuildPrefix (compilerSettingsCompiler compilerSettings)
                                 (compilerSettingsFlags compilerSettings)
  let dependencies    = tomlSettingsDependencies tomlSettings
  let devDependencies = tomlSettingsDevDependencies tomlSettings
  return AppSettings { appSettingsCompiler        = compilerSettings
                     , appSettingsProjectName     = projectName
                     , appSettingsBuildPrefix     = buildPrefix
                     , appSettingsLibrary         = librarySettings
                     , appSettingsExecutables     = executableSettings
                     , appSettingsTests           = testSettings
                     , appSettingsDependencies    = dependencies
                     , appSettingsDevDependencies = devDependencies
                     }

defineCompilerSettings :: [String] -> FilePath -> Bool -> IO CompilerSettings
defineCompilerSettings specifiedFlags compiler release
  | "gfortran" `isInfixOf` compiler
  = let flags = case specifiedFlags of
          [] -> if release
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
          fs -> fs
    in  return $ CompilerSettings { compilerSettingsCompiler    = compiler
                                  , compilerSettingsFlags       = flags
                                  , compilerSettingsModuleFlag  = "-J"
                                  , compilerSettingsIncludeFlag = "-I"
                                  }
  | "caf" `isInfixOf` compiler
  = let flags = case specifiedFlags of
          [] -> if release
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
          fs -> fs
    in  return $ CompilerSettings { compilerSettingsCompiler    = compiler
                                  , compilerSettingsFlags       = flags
                                  , compilerSettingsModuleFlag  = "-J"
                                  , compilerSettingsIncludeFlag = "-I"
                                  }
  | otherwise
  = do
    putStrLn $ "Sorry, compiler is currently unsupported: " ++ compiler
    exitWith (ExitFailure 1)

getLibrarySettings :: Maybe Library -> IO (Maybe Library)
getLibrarySettings maybeSettings = case maybeSettings of
  Just settings -> return maybeSettings
  Nothing       -> do
    defaultExists <- doesDirectoryExist "src"
    if defaultExists
      then return
        (Just
          (Library { librarySourceDir = "src", libraryBuildScript = Nothing })
        )
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

makeBuildPrefix :: FilePath -> [String] -> IO FilePath
makeBuildPrefix compiler flags = do
  -- TODO Figure out what other info should be part of this
  --      Probably version, and make sure to not include path to the compiler
  versionInfo <- readProcess compiler ["--version"] []
  let compilerName = last (splitDirectories compiler)
  let versionHash  = abs (hash versionInfo)
  let flagsHash    = abs (hash flags)
  return
    $   "build"
    </> compilerName
    ++  "_"
    ++  showHex versionHash ""
    ++  "_"
    ++  showHex flagsHash ""

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
          , dependencyBuildScript  = libraryBuildScript librarySettings
          , dependencyDependencies = newDependencies
          }
      Nothing -> do
        putStrLn $ "No library found in " ++ name
        undefined

fetchExecutableDependencies
  :: (Maybe DependencyTree) -> Map.Map String Version -> IO [DependencyTree]
fetchExecutableDependencies maybeProjectTree dependencies =
  case maybeProjectTree of
    Just projectTree@(Dependency name _ _ _ _) ->
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
          , dependencyBuildScript  = libraryBuildScript librarySettings
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
        system ("git init " ++ clonePath)
        case gitVersionSpecRef versionSpec of
          Just ref -> do
            system
              (  "git -C "
              ++ clonePath
              ++ " fetch "
              ++ gitVersionSpecUrl versionSpec
              ++ " "
              ++ (case ref of
                   Tag    tag    -> tag
                   Branch branch -> branch
                   Commit commit -> commit
                 )
              )
          Nothing -> do
            system
              (  "git -C "
              ++ clonePath
              ++ " fetch "
              ++ gitVersionSpecUrl versionSpec
              )
        system ("git -C " ++ clonePath ++ " checkout -qf FETCH_HEAD")
        return (name, clonePath)
      PathVersion versionSpec -> return (name, pathVersionSpecPath versionSpec)

{-
    Bulding the dependencies is done on a depth first basis to ensure all of
    the transitive dependencies have been built before trying to build this one
-}
buildDependencies
  :: String -> CompilerSettings -> [DependencyTree] -> IO [(FilePath, FilePath)]
buildDependencies buildPrefix compilerSettings dependencies = do
  built <- concatMapM (buildDependency buildPrefix compilerSettings)
                      dependencies
  return $ reverse (nub (reverse built))

buildDependency
  :: String -> CompilerSettings -> DependencyTree -> IO [(FilePath, FilePath)]
buildDependency buildPrefix compilerSettings (Dependency name path sourcePath mBuildScript dependencies)
  = do
    transitiveDependencies <- buildDependencies buildPrefix
                                                compilerSettings
                                                dependencies
    let buildPath = buildPrefix </> name
    thisArchive <- case mBuildScript of
      Just script -> buildWithScript script
                                     path
                                     buildPath
                                     compilerSettings
                                     name
                                     (map fst transitiveDependencies)
      Nothing -> buildLibrary sourcePath
                              [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                              buildPath
                              compilerSettings
                              name
                              (map fst transitiveDependencies)
    return $ (buildPath, thisArchive) : transitiveDependencies

createNewProject :: String -> Bool -> Bool -> Bool -> IO ()
createNewProject projectName withExecutable withTest withLib = do
  createDirectory projectName
  writeFile (projectName </> "fpm.toml")   (templateFpmToml projectName)
  writeFile (projectName </> "README.md")  (templateReadme projectName)
  writeFile (projectName </> ".gitignore") "build/*\n"
  when withLib $ do
    createDirectory (projectName </> "src")
    writeFile (projectName </> "src" </> projectName <.> "f90")
              (templateModule projectName)
  when withExecutable $ do
    createDirectory (projectName </> "app")
    writeFile (projectName </> "app" </> "main.f90")
              (templateProgram projectName withLib)
  when withTest $ do
    createDirectory (projectName </> "test")
    writeFile (projectName </> "test" </> "main.f90") templateTest
  withCurrentDirectory projectName $ do
    system "git init"
    return ()

templateFpmToml :: String -> String
templateFpmToml projectName =
  "name = \""
    ++ projectName
    ++ "\"\n"
    ++ "version = \"0.1.0\"\n"
    ++ "license = \"license\"\n"
    ++ "author = \"Jane Doe\"\n"
    ++ "maintainer = \"jane.doe@example.com\"\n"
    ++ "copyright = \"2020 Jane Doe\"\n"

templateModule :: String -> String
templateModule projectName =
  "module "
    ++ projectName
    ++ "\n"
    ++ "  implicit none\n"
    ++ "  private\n"
    ++ "\n"
    ++ "  public :: say_hello\n"
    ++ "contains\n"
    ++ "  subroutine say_hello\n"
    ++ "    print *, \"Hello, "
    ++ projectName
    ++ "!\"\n"
    ++ "  end subroutine say_hello\n"
    ++ "end module "
    ++ projectName
    ++ "\n"

templateReadme :: String -> String
templateReadme projectName =
  "# " ++ projectName ++ "\n" ++ "\n" ++ "My cool new project!\n"

templateProgram :: String -> Bool -> String
templateProgram projectName withLib =
  "program main\n"
    ++ (if withLib then "  use " ++ projectName ++ ", only: say_hello\n" else ""
       )
    ++ "\n"
    ++ "  implicit none\n"
    ++ "\n"
    ++ "  call say_hello\n"
    ++ "end program main\n"

templateTest :: String
templateTest =
  "program main\n"
    ++ "  implicit none\n"
    ++ "\n"
    ++ "  print *, \"Put some tests in here!\"\n"
    ++ "end program main\n"
