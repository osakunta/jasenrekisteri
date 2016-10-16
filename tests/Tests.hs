import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Aeson

import SatO.Jasenrekisteri.Command

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QuickCheck"
    [ testProperty "Command aeson roundtrip" commandRoundtrip
    ]

commandRoundtrip :: Command -> Property
commandRoundtrip cmd = Right cmd === eitherDecode (encode cmd)
