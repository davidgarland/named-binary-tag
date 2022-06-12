module Main where

import Data.Nbt
import Data.Serialize
import Data.Text
import Test.Tasty
import Test.Tasty.QuickCheck

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [convs]

hmm :: IO ()
hmm = do
  shouldBeNbt <- readCompressed "level.dat" :: IO (Either String Nbt')
  case shouldBeNbt of
    Left err -> putStrLn err
    Right nbt -> do
      print nbt
      writeCompressed "anotherlevel.dat" nbt -- see also "Nbt.writeUncompressed"

convs :: TestTree
convs = testGroup "Conversions"
  [ testProperty "idByte" (\x y -> (Nbt (pack x) (Byte y) :: Nbt') == (either undefined id . runGet get . runPut . put $ (Nbt (pack x) (Byte y) :: Nbt')))
  ]
