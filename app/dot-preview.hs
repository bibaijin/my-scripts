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

data Arguments = Arguments {
       language :: Language
     , file     :: String  }

data Language = Zh | En deriving (Eq, Show, Read)

arg :: Parser Arguments
arg = Arguments
  <$> option auto
                  ( long "language"
                  <> short 'l'
                  <> value Zh
                  <> metavar "LANGUAGE"
                  <> help "Which language's template to use(Zh or En)" )
  <*> argument str (metavar "FILE")

panpdf :: Arguments -> IO ()
panpdf (Arguments language filename) =
    shelly $ verbosely $ do
       templateConf <- liftIO $ getTemplateConf language
       cslConf <- liftIO $ getCslConf language
       crossRefConf <- liftIO $ getCrossRefConf language
       run_
           "pandoc"
           [ "--latex-engine=xelatex"
           , templateConf
           -- , "--toc"
           , "--bibliography=ref.bib"
           , cslConf
           , "--filter", "pandoc-crossref"
           , crossRefConf
           , "-o", getNewName filename
           , fromString filename]

getNewName :: String -> Text
getNewName filename =
    fromString $ takeBaseName (fromString filename) ++ ".pdf"

getTemplateConf :: Language -> IO Text
getTemplateConf Zh = do
   home <- getHomeDirectory
   return $ fromString $ "--template=" ++ home ++ "/.dotfiles/pandoc/styles/pdf-zh.tex"
getTemplateConf En = do
   home <- getHomeDirectory
   return $ fromString $ "--template=" ++ home ++ "/.dotfiles/pandoc/styles/pdf-en.tex"

getCslConf :: Language -> IO Text
getCslConf Zh = do
   home <- getHomeDirectory
   return $ fromString $ "--csl=" ++ home ++ "/.dotfiles/pandoc/styles/chinese-gb7714-2005-numeric.csl"
getCslConf En = do
   home <- getHomeDirectory
   return $ fromString $ "--csl=" ++ home ++ "/.dotfiles/pandoc/styles/ieee.csl"

getCrossRefConf :: Language -> IO Text
getCrossRefConf Zh = do
   home <- getHomeDirectory
   return $ fromString $ "--metadata=crossrefYaml:" ++ home ++ "/.dotfiles/pandoc/styles/pandoc-crossref-zh.yaml"
getCrossRefConf En = do
   home <- getHomeDirectory
   return $ fromString $ "--metadata=crossrefYaml:" ++ home ++ "/.dotfiles/pandoc/styles/pandoc-crossref-en.yaml"
