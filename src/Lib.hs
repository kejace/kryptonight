module Lib
    ( someFunc
    ) where

import Codec.Encryption.OpenPGP.ASCIIArmor as AA

someFunc :: IO ()
someFunc = putStrLn "someFunc"

readArmor :: IO ()
readArmor = undefined
