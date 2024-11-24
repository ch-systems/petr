import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (liftIO)
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec             (before, describe, hspec, it, shouldBe)
import           Test.Hspec.Hedgehog    (PropertyT, diff, forAll, hedgehog,
                                         (/==), (===))

import Compiler (compile)

testSource = "fn something (a in 'int, b in 'int) returns 'nothing ()"

main :: IO ()
main = hspec $ do
    describe "regular tests" $ do
        it "compiles" $ do
            let sources = [testSource]
            let result = compile sources
            (show result) `shouldBe` "yeet"
            
    describe "hedgehog" $ do
        it "is useful if you get an ambiguous error" $ hedgehog $ do
            "no ambiguity" === "no ambiguity"

    describe "hedgehog tests" $ do
        it "lets you use PropertyT directly" $ hedgehog $ do
            x <- forAll $ Gen.integral (Range.linear 0 1000)
            y <- forAll $ Gen.integral (Range.linear 0 5000)
            diff (x + y) (>=) x

        it "renders a progress bit" $ hedgehog $ do
            x <- forAll $ Gen.integral (Range.linear 0 1000)
            y <- forAll $ Gen.integral (Range.linear 1 5000)
            liftIO $ threadDelay (100 * x + y)

    describe "with hooks" $ do
        before (pure "Hello!") $ do
            it "has functions" $ \str -> hedgehog $
                str === "Hello!"

            it "goes before or after" $ \str -> do
                pure () :: PropertyT IO ()
                str === "Hello!"

            it "generates" $ \str -> hedgehog $ do
                wrongLen <- forAll $ Gen.integral (Range.linear 0 3)
                length str /== wrongLen