{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import           Data.Semigroup      ((<>))
import           Data.Set
import           Options.Applicative
import           Text.Pandoc
import           Text.Read
import qualified Text.Read.Lex       as L
-- import           Text.Blaze.Html

data Options = Options
  { toFormat       :: ToFormat
  , outputFilename :: String
  , inputFilename  :: String
  }

data ToFormat = RevealJs
  deriving (Show)

main :: IO ()
main = mpandoc =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "pandoc with custom configuration"
     <> header "mpandoc - my-pandoc" )

options :: Parser Options
options = Options
  <$> option parseToFormat
      ( long "to"
     <> short 't'
     <> metavar "FORMAT"
     <> help "Format to convert to" )
  <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILENAME"
     <> help "File to write to" )
  <*> argument str
      ( metavar "FILE" )

parseToFormat :: ReadM ToFormat
parseToFormat = eitherReader $ \s -> case s of
  "revealjs" -> Right RevealJs
  _ -> Left "invalid format to convert to, available formats are: revealjs"

mpandoc :: Options -> IO ()
mpandoc (Options RevealJs outputFilename inputFilename) = do
  putStrLn $ "outputFilename: " ++ outputFilename ++ ", inputFilename: " ++ inputFilename
  putStrLn $ markdownToRevealJs "hello"

markdownToRevealJs :: String -> String
markdownToRevealJs =
  writeHtmlString
    def
    { writerSlideVariant = RevealJsSlides
    , writerIncremental = True
    , writerHTMLMathMethod = MathJax
    , writerVariables = ["revealjs-url", ""]
    } .
  readPandoc

readPandoc :: String -> Pandoc
readPandoc =
  handleError .
  readMarkdown
    def
    { readerExtensions = singleton Ext_east_asian_line_breaks
    , readerStandalone = True
    }
