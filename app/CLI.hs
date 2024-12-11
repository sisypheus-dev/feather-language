module CLI where

import Options.Applicative

data Options = MkOptions 
  { fileInput :: String
  , output :: Maybe String
  , doNotIncludePrelude :: Bool
  }

options :: Parser Options
options = MkOptions
  <$> argument str
    ( metavar "FILE"
    <> help "The input file"
    )
  <*> optional (strOption
    ( long "output"
    <> short 'o'
    <> metavar "OUTPUT"
    <> help "The output file"
    ))
  <*> switch
    ( long "no-prelude"
    <> help "Remove the prelude from the output"
    <> showDefault
    )

parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
      <> progDesc "Compile a Plume file"
      <> header "plume - a compiler for the Plume programming language"
      )
      