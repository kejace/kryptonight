module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Codec.Encryption.OpenPGP.ASCIIArmor.Types

import Lib

instance Read Armor where
  readsPrec = undefined

data Person = Person
  { name :: String
  , armor :: Armor}

person :: Parser Person
person = Person 
  <$> strOption
      ( long "name"
     <> metavar "NAME"
     <> help "Name of the person" )
  <*> option auto 
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

