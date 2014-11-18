module Main where

import Control.Monad.Bolton

import Prelude
import Control.Applicative ( Applicative(..), (<**>), (<|>) )
import Control.Monad.Trans.Either ( runEitherT )
import Data.Either ( Either(..) )
import Data.Functor ( (<$>) )
import Data.Monoid ( (<>), mempty )
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
    ,   prefs
    ,   progDesc
    ,   short
    ,   showHelpOnError
    ,   subparser
    )
import System.Environment ( getArgs )

versionString :: String
versionString = "bolton: 0.0.1"

data Command = Version | Init | List deriving (Show, Eq)

foldCommand :: a -> a -> a -> Command -> a
foldCommand v i l c = case c of
    Version -> v
    Init    -> i
    List    -> l


commandParser :: Parser Command
commandParser =
        flag' Version (short 'v' <> long "version" <> help "version info.")
    <|> (subparser $
            command' "init" "Initialise the bolton store" (pure Init)
        <>  command' "list" "List the installed apps" (pure List)
    )

command' :: String -> String -> Parser a -> Mod CommandFields a
command' name description parser = command name (info (parser <**> helper) (progDesc description))

parseAndRun :: Parser a -> (a -> IO b) -> IO b
parseAndRun p f = do
    x <- getArgs
    case x of
        -- If there were no commands, and the flags (the only valids ones in this case should be --version/-v and --help)
        -- are not recognised, then show the help msg.
        []  -> customExecParser (prefs showHelpOnError) (info (p <**> helper) mempty) >>= f
        _   -> execParser (info (p <**> helper) mempty) >>= f

runCommand :: Command -> IO ()
runCommand = foldCommand printVersion initCommand (pure ())

initCommand :: IO ()
initCommand = do
    res <- runBolton (runEitherT initialiseBolton)
    case res of
        Left (BoltonInitialiseError (DirectoryAlreadyExists p)) -> putStrLn $ "Directory already exists: '" ++ p ++ "'"
        Right ()                                                -> return ()

printVersion :: IO ()
printVersion = putStrLn versionString

main :: IO ()
main = parseAndRun commandParser runCommand
