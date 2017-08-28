import Text.Deburr
import Test.Hspec
import Test.QuickCheck

newtype CharsWeCoolWith = CharsWeCoolWith String deriving (Show)

instance Arbitrary CharsWeCoolWith where
    arbitrary = CharsWeCoolWith <$> listOf (arbitrary `suchThat` isCool)
    shrink (CharsWeCoolWith x) = CharsWeCoolWith <$> shrink x


isCool :: Char -> Bool
isCool = flip elem $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


main :: IO ()
main = hspec $ do
    it "Deburrs single characters into their basic representation." $ do
        deburr "çåéïøù" `shouldBe` "caeiou"

    it "Deburrs double characters into their basic representation." $ do
        deburr "æ þ œ" `shouldBe` "ae th oe"

    it "Respects capitalization in single characters." $ do
        deburr "ÇçÅåÉéÏïØøÙù" `shouldBe` "CcAaEeIiOoUu"

    it "Respects capitalization in double characters." $ do
        deburr "Æ Þ Œ" `shouldBe` "Ae Th Oe"

    it "Respects capitalization in regards to the next character." $ do
        deburr "Æsthetic" `shouldBe` "Aesthetic"
        deburr "ÆSTHETIC" `shouldBe` "AESTHETIC"
        deburr "æsthetic" `shouldBe` "aesthetic"
        deburr "æSTHETIC" `shouldBe` "aeSTHETIC"
        deburr "ÆÞŒ" `shouldBe` "AETHOe"

    it "Doesn't touch standard characters." $
        property $ \(CharsWeCoolWith s) -> deburr s == s

    it "Never reduces the length of a string" $
        property $ \s -> length (deburr s) >= length s
