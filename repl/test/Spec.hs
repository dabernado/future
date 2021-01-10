import Data.Vector (fromList)
import Data.Ratio ((%))
import Test.Hspec
import Text.ParserCombinators.Parsec hiding (spaces)

import Parser

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "correctly parses an atom" $
      (parse parseAtom "test" "var") `shouldBe` Right (Atom "var")

    it "correctly parses a boolean" $
      (parse parseAtom "test" "true") `shouldBe` Right (Bool True)

    it "correctly parses an integer" $
      (parse parseNumber "test" "123") `shouldBe` Right (Integer 123)

    it "correctly parses a float" $
      (parse parseNumber "test" "1.23") `shouldBe` Right (Float 1.23)

    it "correctly parses a ratio" $
      (parse parseNumber "test" "1/23") `shouldBe` Right (Ratio $ 1 % 23)

    it "correctly parses a character" $
      (parse parseChar "test" "\\e") `shouldBe` Right (Char 'e')

    it "correctly parses a string" $
      (parse parseString "test" "\"string\"") `shouldBe` Right (String "string")

    it "correctly parses a list" $
      (parse parseAnyList "test" "(1 2 3)")
      `shouldBe`
      Right (List [Integer 1, Integer 2, Integer 3])

    it "correctly parses a dotted list" $
      (parse parseAnyList "test" "(1 2 . 3)")
      `shouldBe`
      Right (DottedList [Integer 1, Integer 2] (Integer 3))

    it "correctly parses a vector" $
      (parse parseVector "test" "[1 2 3]")
      `shouldBe`
      Right (Vector $ fromList [Integer 1, Integer 2, Integer 3])

    it "correctly parses a set" $
      (parse parseSet "test" "#{1 2 3}")
      `shouldBe`
      Right (List [Atom "set", List [Integer 1, Integer 2, Integer 3]])

    it "correctly parses a map" $
      (parse parseMap "test" "{:a 1, :b 2}")
      `shouldBe`
      Right (List [Atom "hash-map", List [Atom ":a", Integer 1, Atom ":b", Integer 2]])

    it "correctly parses a quoted list" $
      (parse parseQuoted "test" "'(1 2 3)")
      `shouldBe`
      Right (List [Atom "quote", List [Integer 1, Integer 2, Integer 3]])

    it "correctly parses a quasiquoted list" $
      (parse parseQuasiQuoted "test" "`(1 ~2 ~@(3 4))")
      `shouldBe`
      Right (List [Atom "quasiquote", List [Integer 1, List [Atom "unquote", Integer 2], List [Atom "unquote-splicing", List [Integer 3, Integer 4]]]])
