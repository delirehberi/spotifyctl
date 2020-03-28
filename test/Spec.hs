import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SpotifyApi

main :: IO ()
main = hspec $ do
  describe "SpotifyApiUtils" $ do
    it "removes the prefix from string" $ do
      prefixRemover "device_" "device_id" `shouldBe` ("id":: String)
    it "generates query string with given list of string, string tuple" $ do
      (foldl queryString "" [("key","value")] ) `shouldBe` ("&key=value"::String)
      