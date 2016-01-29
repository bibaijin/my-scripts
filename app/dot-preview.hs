{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import           Data.String
import           Data.Text           (Text)
-- import qualified Data.Text           as T
import           Options.Applicative
import           Shelly

main :: IO ()
main = execParser opts >>= dotPreview
  where
    opts =
        info
            (helper <*> arg)
            (fullDesc <>
             progDesc "convert FILE to png by dot and preview by feh" <>
             header "dot-preview - dot .gv file and preview")

data Arguments = Arguments {
     filename :: String  }

arg :: Parser Arguments
arg = Arguments
  <$> argument str (metavar "FILE")

dotPreview :: Arguments -> IO ()
dotPreview (Arguments filename) =
    shelly $ verbosely $ do
       run_ "dot" [ "-Tpng", fromString filename
                  , "-o", "tmp.png"
                  ]
       run_ "feh" [ "-.", "tmp.png" ]
       run_ "rm" [ "tmp.png" ]
