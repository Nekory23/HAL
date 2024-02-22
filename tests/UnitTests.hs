import Test.HUnit

import Parser

parserTests :: Test
parserTests =
    TestList [
        "parseChar" ~: Just ('a', "bcd") ~=? runParser (parseChar 'a') "abcd",
        "parseChar" ~: Nothing ~=? runParser (parseChar 'a') "zbcd",
        "parseChar" ~: Nothing ~=? runParser (parseChar 'b') "abcd",
        "parseChar" ~: Just ('a', "aaa") ~=? runParser (parseChar 'a') "aaaa",
        "parseAnyChar" ~: Just ('a', "bcd") ~=? runParser (parseAnyChar "a") "abcd",
        "parseAnyChar" ~: Nothing ~=? runParser (parseAnyChar "zxy") "abcd",
        "parseAnyChar" ~: Just ('c', "def") ~=? runParser (parseAnyChar "bca") "cdef",
        "parseOr" ~: Just('a', "bcd") ~=? runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd",
        "parseOr" ~: Just('b', "cda") ~=? runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda",
        "parseOr" ~: Nothing ~=? runParser (parseOr (parseChar 'a') (parseChar 'b')) "xyz",
        "parseAnd" ~: Just(('a','b'), "cd") ~=? runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd",
        "parseAnd" ~: Nothing ~=? runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda",
        "parseAnd" ~: Nothing ~=? runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd",
        "parseAndWith" ~: Just ("ab", "cd") ~=? runParser (parseAndWith (\ x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "abcd",
        "parseMany" ~: Just("   ", "foobar") ~=? runParser (parseMany (parseChar ' ')) "   foobar",
        "parseMany" ~: Just("", "foobar   ") ~=? runParser (parseMany (parseChar ' ')) "foobar   ",
        "parseSome" ~: Just("42", "foobar") ~=? runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar",
        "parseSome" ~: Nothing ~=? runParser (parseSome(parseAnyChar ['0'..'9'])) "foobar42",
        "parseUInt" ~: Just(42, "foobar") ~=? runParser parseUInt "42foobar",
        "parseUInt" ~: Nothing  ~=? runParser parseUInt "foobar34",
        "parseInt" ~: Just(42, "foobar") ~=? runParser parseInt "42foobar",
        "parseInt" ~: Just(-42, "foobar") ~=? runParser parseInt "-42foobar",
        "parseInt" ~: Nothing  ~=? runParser parseInt "foobar34",
        "parseFloat" ~: Just(42.0, "foobar") ~=? runParser parseFloat "42foobar",
        "parseFloat" ~: Just(-42.0, "foobar") ~=? runParser parseFloat "-42foobar",
        "parseFloat" ~: Just(42.3, "foobar") ~=? runParser parseFloat "42.3foobar",
        "parseFloat" ~: Just(-42.3, "foobar") ~=? runParser parseFloat "-42.3foobar",
        "parseFloat" ~: Nothing  ~=? runParser parseFloat "foobar-6",
        "parseUfloat" ~: Just(42.0, "foobar") ~=? runParser parseUFloat "42foobar",
        "parseUfloat" ~: Just(42.69, "foobar") ~=? runParser parseUFloat "42.69foobar",
        "parseUfloat" ~: Nothing  ~=? runParser parseUFloat  "foobar34"
    ]

main :: IO Counts
main =
    runTestTT parserTests