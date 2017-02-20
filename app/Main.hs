{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import           Options.Applicative
import           Control.Monad
import           Codec.Encryption.OpenPGP.ASCIIArmor
import           Codec.Encryption.OpenPGP.ASCIIArmor.Types
import qualified Data.Attoparsec.ByteString    as AB
import           Data.ByteString.Char8         as BS8L hiding (putStrLn, concat)
import           Codec.Encryption.OpenPGP.Serialize
import           Codec.Encryption.OpenPGP.Types
import           Data.Text                     as T

import Lib()

-- instance Read Armor where
--   readsPrec = undefined

attoReadM :: AB.Parser a -> ReadM a
attoReadM p = eitherReader (AB.parseOnly p . BS8L.pack)

data Person = Person
  { 
  file :: String
  --, armor :: Armor
  }

person :: Parser Person
person = Person 
  <$> strOption
      ( long "file"
     <> metavar "FILE"
     <> help "Filename for armor" )
--   <*> option (attoReadM parseArmor)
--       ( long "armor"
--      <> help "The armor ascii" )

main :: IO ()
main = do
  personParsed <- execParser opts
  contents <- BS8L.readFile (file personParsed)
 
  let armorBS = case decode contents of
        Left e -> error e 
        Right ((Armor _ _ bs):_) -> bs 
        Right _ -> error "Couldn't parse"
  
  (parsePkts armorBS) `forM_` \case
      (UserIdPkt u) -> putStrLn . T.unpack $ "PubKey belongs to " <> u 
      _             -> return ()
  
    where
      opts = info (helper <*> person)
        ( fullDesc
       <> progDesc "Print a greeting for NAME"
       <> header "hello - a test for optparse-applicative" )
