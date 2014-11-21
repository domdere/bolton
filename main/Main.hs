module Main where

import Control.Monad.Bolton

import Prelude
import Control.Applicative ( Applicative(..), (<**>), (<|>) )
import Control.Monad.Trans.Either ( runEitherT )
import Data.Monoid ( (<>), mempty )
import Data.Functor ( (<$>) )
import Options.Applicative
    (   CommandFields
    ,   Mod
    ,   Parser
    ,   command
    ,   customExecParser
    ,   execParser
    ,   flag'
    ,   help
    ,   helper
    ,   info
    ,   long
    ,   metavar
    ,   optional
    ,   prefs
    ,   progDesc
    ,   short
    ,   showHelpOnError
    ,   strOption
    ,   subparser
    )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(..), exitWith )

versionString :: String
versionString = "bolton: 0.0.1"

data Command =
        Version
    |   Init
    |   InstallHackage String (Maybe String)
    |   List
        deriving (Show, Eq)

foldCommand :: a -> a -> (String -> Maybe String -> a) -> a -> Command -> a
foldCommand v i ih l c = case c of
    Version -> v
    Init    -> i
    InstallHackage package version -> ih package version
    List    -> l


commandParser :: Parser Command
commandParser =
        flag' Version (short 'v' <> long "version" <> help "version info.")
    <|> (subparser $
            command' "init" "Initialise the bolton store" (pure Init)
        -- <>  command' "list" "List the installed apps" (pure List)
        <>  command' "install-hackage" "Install a package from Hackage" (InstallHackage <$> packageName <*> optVersion)
    )

command' :: String -> String -> Parser a -> Mod CommandFields a
command' name description parser = command name (info (parser <**> helper) (progDesc description))

packageName :: Parser String
packageName = strOption (short 'p' <> long "package" <> metavar "PACKAGE" <> help "The package to install")

optVersion :: Parser (Maybe String)
optVersion = (optional . strOption) (short 'v' <> long "version" <> metavar "VERSION" <> help "(Optional) The version of the package to install")

parseAndRun :: Parser a -> (a -> IO b) -> IO b
parseAndRun p f = do
    x <- getArgs
    case x of
        -- If there were no commands, and the flags (the only valids ones in this case should be --version/-v and --help)
        -- are not recognised, then show the help msg.
        []  -> customExecParser (prefs showHelpOnError) (info (p <**> helper) mempty) >>= f
        _   -> execParser (info (p <**> helper) mempty) >>= f

runCommand :: Command -> IO ()
runCommand = foldCommand printVersion initCommand installHackageCommand (pure ())

initCommand :: IO ()
initCommand = do
    res <- runBolton (runEitherT initialiseBolton)
    case res of
        Left (InitMakeDir (DirectoryAlreadyExists p))   -> putStrLn $ "Directory already exists: '" ++ p ++ "'"
        Left (InitEnv (EnvVariableDoesntExist v))       -> do
            putStrLn $ "Missing Environment Variable: '" ++ v ++ "'"
            exitWith (ExitFailure 1)
        Right binDir                                    -> putStrLn $ unlines
            [   "Successfully created bolton store"
            ,   "Make sure '" ++ binDir ++ "' is in your PATH"
            ]


installHackageCommand :: String -> Maybe String -> IO ()
installHackageCommand package version = do
    res <- (runBolton . runEitherT) $ installHackagePackage package version
    either
        (\e -> putStrLn (show e) >> exitWith (ExitFailure 1))
        (\pInfo -> putStrLn $ "Package Installed Successfully: " ++ show pInfo)
        res

printVersion :: IO ()
printVersion = putStrLn versionString

main :: IO ()
main = parseAndRun commandParser runCommand
