module Main where

import Control.Applicative ( Applicative(..), (<**>), (<|>) )
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

--coreOptions :: [OptDescr CommandLineOption]
--coreOptions = 
--    [   Option "h?" ["help"]    (NoArg Help)    "print this usage message"
--    ,   Option "V"  ["version"] (NoArg Version) "output the version"
--    ]
--
--argOrder :: ArgOrder a
--argOrder = Permute
--
---- Define the additional options for your app here...
--options :: [OptDescr CommandLineOption]
--options = []

usageString :: String
usageString = "Usage: bolton [OPTIONS] args"

versionString :: String
versionString = "bolton: 0.0.1"

usageMsg :: String
usageMsg = ""
--usageMsg = usageInfo usageString (coreOptions ++ options)

-- | prints the usage message
printUsageMsg :: IO ()
--printUsageMsg = putStrLn $ usageInfo usageString (coreOptions ++ options)
printUsageMsg = return ()

-- | checks the core flags of the app and if help and version dont appear passes control onto appMain where
-- | the user can do their own opt checks
--checkCoreFlagsAndRunMain :: [CommandLineOption] -> [String] -> IO ()
--checkCoreFlagsAndRunMain opts args
--    | Help `elem` opts      = printUsageMsg
--    | Version `elem` opts   = putStrLn versionString
--    | otherwise             = appMain opts args

data Command = Init | List | Version deriving (Show, Eq)

commandParser :: Parser Command
commandParser =
        (flag' Version (short 'v' <> long "version" <> help "version info."))
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
runCommand = const (pure ())

main :: IO ()
main = parseAndRun commandParser runCommand
--    (opts, args, errorMsgs) <- getOpt argOrder (coreOptions ++ options) `fmap` getArgs
--    if null errorMsgs then
--        checkCoreFlagsAndRunMain opts args
--    else
--        ioError $ userError $ concat errorMsgs ++ usageMsg

-- Put the actual App code in here
appMain :: [a] -> [String] -> IO ()
appMain opts args = do
    putStrLn "Command Line Args: "
    mapM_ putStrLn args
