module Ciphers where
  {- 
    Functions for encoding and decoding messages based on the use of Ciphers for COM2108 2019.
    Author: Rob Bowland
  -}

  import AssignmentHelp -- needed for Cipher type, alphaPos etc
  import Data.Char -- needed for various Char Functions
  import Data.List -- needed for various List Functions

  {-
  ---------------------  
    TESTING METHODOLOGY:
    Testing was performed by checking manually determined 'Expected' outputs with 
    the 'Actual' obtained results from running the functions with various inputs.
    Functions were concluded to be 'correct' if 'Expected' and 'Actual' results were 
    matching for the various input cases. Note that only valid inputs were used for testing.
    Test results are listed below each of the tested functions.
  --------------------- 
  -}

  {-| 
    The 'validateCipher' function checks whether a supplied Cipher is valid or not.
    Valid Ciphers contain each letter of the English alphabet only once (in uppercase).
    Will return True if the Cipher is valid, False otherwise.
  -}
  validateCipher :: Cipher -> Bool
  validateCipher cipher = sort cipher == ['A'..'Z']
  {-
    TESTING:
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    Outputs:
      Expected: True
      Actual:   True
      -----------------------------
    Inputs: "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    Outputs:
      Expected: True
      Actual:   True
    -----------------------------
    Inputs: ""
    Outputs:
      Expected: False
      Actual:   False
    -----------------------------
    Inputs: "ABC"
    Outputs:
      Expected: False
      Actual:   False
    -----------------------------
  -}
  
  -- Assume all functions with recieve *valid* ciphers from here on or uppercase letter strings --

  {-| 
    The 'shift' function shifts a given String by a given Int offset.
    Strings are shifted in a cyclical manner with letters at the end of the 
    String 'looping' to the front of the string or visa versa.
    Positive Int offsets shift the String to the right, negative Int offsets
    shift the String to the left.
    Returns the shifted String.
  -}
  shift:: String -> Int -> String
  shift str offset = take (length str) (drop ((length str) - offset) (cycle str))

  {-|
    The 'encode' function encodes a single Char based on a supplied Cipher and Int offset.
    Add details about how shifting works
    Returns the encoded Char.
  -}
  encode :: Cipher -> Int -> Char -> Char
  encode cipher offset char = shift cipher offset !! alphaPos char
  {-
    TESTING:
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 'A'
    Outputs:
      Expected: 'E'
      Actual:   'E'
    -----------------------------
    Inputs: "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 2 'A'
    Outputs:
      Expected: 'Y'
      Actual:   'Y'
    -----------------------------
    Inputs: "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (-2) 'A'
    Outputs:
      Expected: 'C'
      Actual:   'C'
    -----------------------------
    Inputs: "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (26) 'A'
    Outputs:
      Expected: 'A'
      Actual:   'A'
    -----------------------------
  -}
     
  {-| 
    The 'encodeMessage' function encodes a full String based on a supplied Cipher and Int offset.
    Returns the encoded String.
  -}            
  encodeMessage :: Cipher -> Int -> String -> String
  encodeMessage cipher offset str = [encode cipher offset char | char <- str]
  {-
    TESTING:
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 "SPANGLES"
    Outputs:
      Expected: "SHEWDTLS"
      Actual:   "SHEWDTLS"
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 "A"
    Outputs:
      Expected: "E"
      Actual:   "E"
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 "A"
    Outputs:
      Expected: "E"
      Actual:   "E"
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 2 "HELLO"
    Outputs:
      Expected: "GMZZO"
      Actual:   "GMZZO"
    -----------------------------
  -}

  {-| 
    The 'reverseEncode' function takes an encoded Char as well as the Cipher and Int offset
    that was used to encode it.
    Returns the original Char before it was encoded.
  -}
  reverseEncode :: Cipher -> Int -> Char -> Char
  reverseEncode cipher offset char = ['A'..'Z'] !! encdCharIndex
      where encdCharIndex = head [index | (index,b) <- zip[0..] (shift cipher offset), char == b] -- the index of the encoded character in the shifted cipher
  {-
    TESTING:
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 'E'
    Outputs:
      Expected: 'A'
      Actual:   'A'
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 2 'C'
    Outputs:
      Expected: 'A'
      Actual:   'A'
    -----------------------------
    Inputs: "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (-4) 'E'
    Outputs:
      Expected: 'A'
      Actual:   'A'
    -----------------------------
    Inputs: "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 4 'E'
    Outputs:
      Expected: 'I'
      Actual:   'I'
    -----------------------------
    Inputs: "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (-2) 'C'
    Outputs:
      Expected: 'A'
      Actual:   'A'
    -----------------------------
  -}

  {-| 
    The 'reverseEncodeMessage' function takes an encoded String as well as the Cipher and Int offset
    that was used to encode it.
    Returns the original String before it was encoded.
  -}
  reverseEncodeMessage :: Cipher -> Int -> String -> String
  reverseEncodeMessage cipher offset str = [reverseEncode cipher offset char | char <- str]
  {-
    TESTING:
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 SHEWDTLS"
    Outputs:
      Expected: "SPANGLES"
      Actual:   "SPANGLES"
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 "A"
    Outputs:
      Expected: "A"
      Actual:   "A"
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0 "A"
    Outputs:
      Expected: "E"
      Actual:   "E"
    -----------------------------
    Inputs: "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 2 "GMZZO"
    Outputs:
      Expected: "HELLO"
      Actual:   "HELLO"
    -----------------------------
  -}

  {-| 
    The 'charFreq' function calculates the frequency of a given Char in a given
    String as a percentage of the total length of the String.
    Returns an Int represent the (rounded) percentage of the String that the given Char makes up.
  -}
  charFreq :: Char -> String -> Int
  charFreq char str = charCount char str `percent` length str

  {-| 
    The 'charCount' function counts the numbers of occurences of a given Char
    in a given String.
    Returns an Int representing the above.
  -}
  charCount :: Char -> String -> Int
  charCount char str = length (filter (== char) str) 

  {-| 
    The 'removeDups' function removes duplicates from a given String.
    The first instance of each Char is kept.
    Returns a String with duplicates removed.
  -}
  removeDups :: String -> String
  removeDups [] = []
  removeDups(x:xs) = x : removeDups (filter (\n -> n /= x) xs)

  {-| 
    The 'letterStats' function gives the frequency of each letter in a given String 
    as a percentage of the total number letters in the String.
    Returns a List of (Char, Int) tuples each representing a given letter and 
    it's frequency, returned in descending order of frequency, ommitting letters with
    calculated frequences of 0.
  -}
  letterStats :: String -> [(Char, Int)]
  letterStats str = mergesort (\(_,a) (_,b)-> a > b) [ (char, charFreq char str) | char <- removeDups str, charFreq char str > 0]
  {-
    TESTING:
    -----------------------------
    Inputs: "A"
    Outputs:
      Expected: [('A',100)]
      Actual:   [('A',100)]
    -----------------------------
    Inputs: "AAAAAA"
    Outputs:
      Expected: [('A',100)]
      Actual:   [('A',100)]
    -----------------------------
    Inputs: "ABC"
    Outputs:
      Expected: [('A',33),('B',33),('C',33)]
      Actual:   [('A',33),('B',33),('C',33)]
    -----------------------------
    Inputs: "STDDWSD"
    Outputs:
      Expected: [('D',43),('S',29),('W',14),('T',14)]
      Actual:   [('D',43),('S',29),('W',14),('T',14)]
    -----------------------------
  -} 

  {-| 
    The 'partialCharacterDecode' function decodes a given Char based on give List of encoding
    guesses represented as (Char, Char) tuples where the first element of the tuple pair 
    is an uppercase letter and the second is the guess at its encoding, another upper case letter.
    In the case of duplicate guesses for the same letter, the first to occur in the list will be used.
    Returns a lowercase decoded letter if a guess for given Char to decode is providided, else return the 
    given Char.
  -}
  partialCharacterDecode::[(Char, Char)] -> Char -> Char
  partialCharacterDecode [] char = char
  partialCharacterDecode ((letter,guess):rest) char
      | char == guess = toLower letter
      | otherwise = partialCharacterDecode rest char
    
  {-| 
    The 'partialDecode' function decodes a given String based on give List of encoding
    guesses represented as (Char, Char) tuples where the first element of the tuple pair 
    is an uppercase letter and the second is the guess at its encoding, another upper case letter.
    In the case of duplicate guesses for the same letter, the first to occur in the list will be used.
    Returns a String with Chars that have a decoding guess replaced with the lowercase letter of their
    suggested decoded form.
  -}
  partialDecode :: [(Char, Char)] -> String -> String
  partialDecode guesses str = [partialCharacterDecode guesses char | char <- str]
  {-
    TESTING:
    -----------------------------
    Inputs: [('E','X'),('S','W')] "DXPWXW"
    Outputs:
      Expected: "DePses"
      Actual:   "DePses"
    -----------------------------
    Inputs: [('H','X'),('E','W'),('L','Z'),('O','Y')] "XWZZY"
    Outputs:
      Expected: "hello"
      Actual:   "hello"
    -----------------------------
    Inputs: [('T','F'),('E','W'),('S','Z'),('P','F')] "FWZF"
    Outputs:
      Expected: "test"
      Actual:   "test"
    -----------------------------
  -}

  {-|
    DECODING MYSTERY MESSAGE:
    Guesses input: 
      [('S','A'),('T','J'),('O','F'),('P','V'),('E', 'W'),('A','X'),('I','Q'),('Y','R'),('N','Y'),('U','D'),('B','B'),('C','L'),('R','E'),('K','Z'),('H','C'),('G','M'),('M','P'),('F','T')] 
    Output:
      "itseasytobreakasubstitutioncipherproKiHeHyouhaKeaNongenoughmessagestopNetsmakethisoneaNittNebitNongerstopokitshouNHbetherightsortofsiUenoSstopmaybenotNetsincreasethemessageNengthabitmorestopkeepthismessagesecretorshareifyouSanttheShoNecNasstogetthebonusmarksstop"
    Formatted Output:
      "it's easy to break a substitution cipher proKiHeH you haKe a Nong enoug hmessage stop 
      Nets make this one a NittNe bit Nonger stop 
      ok it shouNH be the right sort of siUe noS stop
      maybe not Nets increase the message Nength a bit more stop
      keep this message secret or share if you Sant the ShoNe cNass to get the bonus marks stop"
    Fully Formatted + Completed Output:
      It's easy to break a substitution cipher provided you have a long enough message. 
      Let's make this one a little bit longer. 
      Ok it should be the right sort of size now. 
      Maybe not let's increase the message length a bit more. 
      Keep this message secret or share if you want the whole class to get the bonus marks."
  -}

  