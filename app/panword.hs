{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import           Data.String
import           Data.Text           (Text)
-- import qualified Data.Text           as T
import           Options.Applicative
import           Shelly
import           System.Directory
import           System.FilePath

import           Prelude             hiding (FilePath)

main :: IO ()
main = execParser opts >>= panpdf
  where
    opts =
        info
            (helper <*> arg)
            (fullDesc <>
             progDesc "Convert FILE format from markdown to pdf by Pandoc" <>
             header "panpdf - pandoc markdown to pdf")

arg :: Parser String
arg = argument str (metavar "FILE")

panpdf :: String -> IO ()
panpdf filename =
    shelly $ verbosely $ do
       templateConf <- liftIO getTemplateConf
       cslConf <- liftIO getCslConf
       crossRefConf <- liftIO getCrossRefConf
       run_
           "pandoc"
           [ "--to=docx"
           , "--bibliography=ref.bib"
           , cslConf
           , "--filter", "pandoc-crossref"
           , crossRefConf
           -- , "--toc"
           , "-o", getNewName filename
           , fromString filename]

getNewName :: String -> Text
getNewName filename =
    fromString $ takeBaseName (fromString filename) ++ ".docx"

getTemplateConf :: IO Text
getTemplateConf = do
   home <- getHomeDirectory
   return $ fromString $ "--template=" ++ home ++ "/.config/pandoc/pdf.tex"

getCslConf :: IO Text
getCslConf = do
   home <- getHomeDirectory
   return $ fromString $ "--csl=" ++ home ++ "/.config/pandoc/chinese-gb7714-2005-numeric.csl"

getCrossRefConf :: IO Text
getCrossRefConf = do
   home <- getHomeDirectory
   return $ fromString $ "--metadata=crossrefYaml:" ++ home ++ "/.config/pandoc/pandoc-crossref-zh.yaml"
