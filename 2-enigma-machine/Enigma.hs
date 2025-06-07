module Enigma where
  {- 
    Functions for encoding and decoding messages based on the simulation of Enigma Machines, for COM2108 2019.
    Author: Rob Bowland
  -}

  import AssignmentHelp -- needed for Cipher type, alphaPos etc

  -- Data Types:
  type Rotor = Cipher 
  type Reflector = [(Char,Char)] -- Reflectors are expected to contain 13 tuple pairs at maximum. Where used it is assumed this will be the case.
  type Offsets = (Int, Int, Int) -- Offsets should take values from (0,0,0) to (25,25,25). Where used it is assumed this will be the case.
  type Steckerboard = [(Char,Char)] -- Steckboards are expected to contain 10 tuple pairs at maximum. Where used it is assumed this will be the case.
  type Crib = [(Char,Char)]
  type Menu = [Int]

  -- Data Structures:
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Steckerboard

  {-|
    The 'incrementOffsets' function increments a given set of Offsets in accordance to several rules in order to 
    simulate the rotor advancement of an Enigma Machine, with each 'column' of the input Offsets corresponding to a particular rotor.
    i.e. the Offsets correspond as follows: 

      (Int, Int, Int) => (leftOffset, middleOffset, rightOffset)

    Individual Offsets are incremented from right to left and can take values 0 - 25. All offsets apart from the 'rightOffset' will increment once the 
    offset to the right of them has reached 25 and then been incremented (undergone a full revolution in the context of a rotor). 
    i.e.  

      incrementOffsets (0, 0, 25) => (0, 1, 0)

    If all offsets are set to 25, calling incrementOffsets will reset all offsets to 0.
    
    This function expects Offsets with range (0,0,0) - (25,25,25).
    Returns Offsets incremented by 1.
  -}
  incrementOffsets :: Offsets -> Offsets
  incrementOffsets ((leftOffset, middleOffset, rightOffset)) = 
    case (leftOffset, middleOffset, rightOffset) of
      (25,25,25) -> (0,0,0)
      (_,25,25) -> (leftOffset + 1 , 0, 0)
      (_,_,25) -> (leftOffset, middleOffset + 1, 0)
      (_,_,_) -> (leftOffset, middleOffset, rightOffset + 1)

  {-|
    The 'offsetLetter' function offsets a given alphabetic Char by a given Int offset.
    Positive Int offsets will offset the Char to the 'right' in the alphabet.
    i.e.
      
      offsetLetter 1 'A' => 'B' 

    Negative Int offsets will offset the Char to the 'left' in the alphabet.
    i.e.

      offsetLetter (-1) 'B' => 'A'
    
    If a given Char is offset by an Int greater than the number of remaining characters in the alphabet, it
    will 'cycle' around.
    i.e. 
      
      offsetLetter 1 'Z' => 'A'  

    Returns the offset Char.
  -}    
  offsetLetter :: Int -> Char -> Char
  offsetLetter offset letter  = ['A'..'Z'] !! (((alphaPos letter) + offset) `mod` 26)
  
  {-|
    The 'rightToLeftEncode' function simulates the encoding of a Char by an Enigma Machine Rotor cipher that is offset by a given Int, 
    from a right to left direction.
    Returns the encoded Char.
  -}
  rightToLeftEncode :: Rotor -> Int -> Char -> Char
  rightToLeftEncode cipher offset c = offsetLetter (-offset) $ cipher !! (((alphaPos c) + offset) `mod` 26) 
  
  {-|
    The 'leftToRightEncode' function simulates the encoding of a Char by an Enigma Machine Rotor cipher that is offset by a given Int, 
    from a left to right direction. This is an inverse method the rightToLeftEncode.
    Returns the encoded Char.
  -}
  leftToRightEncode :: Rotor -> Int -> Char -> Char
  leftToRightEncode cipher offset c = ['A'..'Z'] !! (((findLoc (offsetLetter offset c) cipher) - offset) `mod` 26)
  
  {-|
    Written by: Emma Norling.
    The 'findLoc' function will find the location of a given Item in a given List.
    This function assumes that the Item in present in the List.
    Return the index of the Item in the List.
  -}  
  findLoc :: Eq a => a -> [a] -> Int
  findLoc item [single] = 0
  findLoc item (x:xs)
    | item == x = 0
    | otherwise = 1 + findLoc item xs

  {-|
    The 'partialSequenceEncode' function simulates a complete single direction encoding of a given Char through a Enigma Machine with three given Rotors,
    offset by given Offsets, encoded via a given encodeMethod. 
    The given Char is encoded using each of the Rotors, with the encoded Char output from a Rotor being used as the input for the next rotor.
    Returns the encoded Char.
  -}
  partialSequenceEncode :: (Rotor, Rotor, Rotor) -> Offsets -> (Rotor -> Int -> Char -> Char) -> Char -> Char
  partialSequenceEncode (firstRotor, secondRotor, thirdRotor) (fstOffset,sndOffset,thrdOffset) encodeMethod  char =  
    encodeMethod thirdRotor thrdOffset 
      $ encodeMethod secondRotor sndOffset 
        $ encodeMethod firstRotor fstOffset char 

  {-|
    The 'reflect' function returns a reflected Char by returning the other Char in a (Char, Char) tuple it is present in.
    i.e. 
      
      reflect [('A','Z'), ('C','D')] 'A' => 'Z'
    
    If the given Char is present in multiple (Char, Char) tuples, the first it is present in will be used to reflect it.
    Returns the reflected Letter if it present in one of the (Char, Char) tuples, otherwise returns the original Char.
  -}
  reflect :: [(Char,Char)] -> Char -> Char
  reflect [] char = char
  reflect ((left,right):rest) char
      | char == left = right
      | char == right = left
      | otherwise = reflect rest char

  {-|
    The 'sequenceEncode' function simulates the full encoding sequence of an Enigma Machine, containing three given Rotors offset by given Offsets,
    a given Reflector and a given Char to be encoded.
    First encoding occurs using partialSequence encode moving from right to left through the supplied Rotors. The output of this encode sequence is 
    then reflected using the supplied Reflector and the reflected Char is then ecoded moving from a left to right direction through the Rotors.
    Returns the encoded Char.
  -}
  sequenceEncode :: (Rotor, Rotor, Rotor) -> Offsets -> Reflector -> Char -> Char
  sequenceEncode (leftRotor, middleRotor, rightRotor) (leftOffset, middleOffset, rightOffset) reflector char = 
      partialSequenceEncode (leftRotor, middleRotor, rightRotor) (leftOffset, middleOffset, rightOffset) leftToRightEncode 
        $ reflect reflector 
          $ partialSequenceEncode (rightRotor, middleRotor, leftRotor) (rightOffset, middleOffset, leftOffset) rightToLeftEncode char

  {-|
    The 'enigmaEncode' function encodes a given Char using a given Engima Machine (SimpleEnigma or SteckeredEnigma). 
    Given Chars are expected to be alphabetic and capital.
    Returns the encoded Char.
  -}
  enigmaEncode :: Char -> Enigma -> Char
  enigmaEncode char enigma = 
    case enigma of 
      SimpleEnigma leftRotor middleRotor rightRotor reflector offsets 
        -> sequenceEncode (leftRotor, middleRotor, rightRotor) (incrementOffsets offsets) reflector char
      SteckeredEnigma leftRotor middleRotor rightRotor reflector offsets steckerboard 
        -> reflect steckerboard 
          $ sequenceEncode (leftRotor, middleRotor, rightRotor) (incrementOffsets offsets) reflector (reflect steckerboard char)

  {-|
    The 'enigmaEncodeMessage' function encodes a given String using a given Engima Machine (SimpleEnigma or SteckeredEnigma). 
    The process of encoding is symmetrical, meaning that encoded Strings can be used as inputs to recive the decoded String, provided
    the correct Offsets (and Steckerboard if applicable) are supplied when constructing the Enigma Machine. 
    Given Strings are expected to contain alphabetic and capital Chars only.
    Returns the encoded String.
  -}
  enigmaEncodeMessage :: String -> Enigma -> String
  enigmaEncodeMessage [] _ = []
  enigmaEncodeMessage (x:xs) enigma = 
    case enigma of
      SimpleEnigma leftRotor middleRotor rightRotor reflector offsets ->
        enigmaEncode x enigma: enigmaEncodeMessage xs (SimpleEnigma leftRotor middleRotor rightRotor reflector (incrementOffsets offsets))
      SteckeredEnigma leftRotor middleRotor rightRotor reflector offsets steckerboard ->
        enigmaEncode x enigma: enigmaEncodeMessage xs (SteckeredEnigma leftRotor middleRotor rightRotor reflector (incrementOffsets offsets) steckerboard)
  
  {-|
    The 'makeLinks' function takes a given Crib and generates a list of Menus representing the possible connections between Plain text and 
    Cipher text elements in the given Crib.
    Return List of Menus representing all possible links (two element Menus) that can be constructed from a given Crib.
  -}
  makeLinks :: Crib -> [Menu]
  makeLinks crib = [[idx1, idx2] |  (idx1, (plain1, cipher1)) <- indexedCrib, (idx2, (plain2, cipher2)) <- indexedCrib, cipher1 == plain2]
    where 
      indexes = [0..((length crib)-1)]
      indexedCrib = zip indexes crib

  {-|
    The 'findDeadEnds' function finds all the links for which there is no Plain text element that will connect to 
    the Cipher text element of the link, indicating that this link will be a termination point of a chain.
    Returns a List of Menus representing deadEnds.
  -}
  findDeadEnds :: [Menu] -> [Menu]
  findDeadEnds links = [[x,y] | [x,y] <- links, (y `elem` (map head links)) == False]

  {-|
    The 'makeChains' function take a given List of startingMenus and continually expands them using links from a given linkSet
    until they can no longer be expanded, at which point the Menus are returned. This function also ensures infinite loops are avoided.
    Returns List of Menus representing all the longest possible menus that can be constructed from a given linkSet.
  -}
  makeChains :: [Menu] -> [Menu] -> [Menu]
  makeChains startingMenus linkSet = 
    if expandedMenus == []
      then startingMenus
      else makeChains expandedMenus linkSet
    where expandedMenus = [concat [ys, tail xs] | xs <- startingMenus, ys <- linkSet, head xs == last ys && ((head ys `elem` xs) == False)]

  {-|
    The 'longestMenu' function generates the longest Menu that can be produced for a given Crib.
    If there are multiple longest menus of the same length, the first of these will be returned.
    Return the longest Menu for a given Crib.
  -}
  longestMenu:: Crib -> Menu
  longestMenu [] = []
  longestMenu crib
    | makeChains (findDeadEnds $ makeLinks crib) (makeLinks crib) == [] = []
    | otherwise = head $ makeChains (findDeadEnds $ makeLinks crib) (makeLinks crib)

  enigmaMachine = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0)
