module Main where

import           Options.Applicative
import           Data.Semigroup ((<>))
import           Codec.Encryption.OpenPGP.ASCIIArmor
import           Codec.Encryption.OpenPGP.ASCIIArmor.Types
import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString.Char8 as BS8L hiding (putStrLn)

import Lib

instance Read Armor where
  readsPrec = undefined

attoReadM :: AB.Parser a -> ReadM a
attoReadM p = eitherReader (AB.parseOnly p . BS8L.pack)

data Person = Person
  { name :: String
  , armor :: Armor}

person :: Parser Person
person = Person 
  <$> strOption
      ( long "name"
     <> metavar "NAME"
     <> help "Name of the person" )
  <*> option (attoReadM parseArmor)
      ( long "armor"
     <> help "The armor ascii" )

greet :: Person -> IO ()
greet (Person name _) = putStrLn $ "Hello, " ++ name
greet _ = return ()

main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (helper <*> person)
      ( fullDesc
     <> progDesc "Print a greeting for NAME"
     <> header "hello - a test for optparse-applicative" )

