module Bombe where
    {- 
    Functions for breaking enigma encodings based on the simulation of Enigma Machines and Bombes, for COM2108 2019.
    Author: Rob Bowland
  -}


  import Enigma -- Needed for Steckerboard, Crib, Menu data types, enigmaEncode etc.
  import AssignmentHelp -- Needed for rotor definitions
  import Data.Maybe -- Needed to deal with the input of Maybe elements in functions requiring definites

  -- Data Types:
  type SteckerPair = (Char, Char)


  {-|
    The 'steckerAdd' function takes a SteckerPair and adds it to a given Steckerboard provided
    that the pair does not already exist in the Steckerboard or if the Chars in the SteckerPair 
    do not already exist in other SteckerPairs in the Steckerboard.
    Returns: 
      - The given Steckerboard if the given SteckerPair is already present in it (with the letters in either order).
      - Nothing if the given SteckerPair does not exist in the given Steckerboard BUT the one or both of the letters exist
        in other SteckerPairs in the given Steckerboard (i.e a contradiction).
      - A new Steckerboard with the given SteckerPair added to it neither of the above conditions are met.
  -}
  steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
  steckerAdd steckerPair steckerboard
    | pairInStecker steckerPair steckerboard = Just steckerboard
    | otherwise = 
      if lettersUsed
      then Nothing
      else Just (steckerboard ++ [steckerPair])
    where lettersUsed = letterInStecker (fst steckerPair) steckerboard || letterInStecker (snd steckerPair) steckerboard
  {-
    TESTING:
    -----------------------------
    Inputs: ('A','B') []
    Outputs:
      Expected: Just [('A','B')]
      Actual:   Just [('A','B')]
    -----------------------------
    Inputs: ('A','B') [('A','B')]
    Outputs:
      Expected: Just [('A','B')]
      Actual:   Just [('A','B')]
    -----------------------------
    Inputs: ('A','B') [('B','A')]
    Outputs:
      Expected: Just [('B','A')]
      Actual:   Just [('B','A')]
    -----------------------------
    Inputs: ('A','B') [('C','D')]
    Outputs:
      Expected: Just [('C','D'), ('A','B')]
      Actual:   Just [('C','D'), ('A','B')]
    -----------------------------
    Inputs: ('A','B') [('C','A')]
    Outputs:
      Expected: Nothing
      Actual:   Nothing
    -----------------------------
  -}

  {-|
    The 'pairInStecker' function determines whether or not a given SteckerPair exists in a given 
    Steckerboard.
    Returns a Bool indicating whether or not the SteckerPair exists in the Steckerboard.
  -}
  pairInStecker :: SteckerPair -> Steckerboard -> Bool
  pairInStecker steckerPair@(a,b) steckerboard
    | steckerPair `elem` steckerboard || (b,a) `elem` steckerboard = True
    | otherwise = False

  {-|
    The 'letterInStecker' function determines whether or not a given Char is used 
    in a given Steckerboard.
    Expects a capital alphabetic Char as input.
    Returns a Bool indicating whether or not the Char is used in the Steckerboard.
  -}
  letterInStecker :: Char -> Steckerboard -> Bool
  letterInStecker c steckerboard
    | c `elem` map fst steckerboard || c `elem` map snd steckerboard = True
    | otherwise = False 

  {-|
    The 'repeatIncrementOffsets' function increments offsets n times.
    Returns the input Offsets after being incremeneted n times.
  -}
  repeatIncrementOffsets :: Offsets -> Int -> Offsets
  repeatIncrementOffsets offsets n = (iterate incrementOffsets offsets) !! n

  {-|
    The 'followMenu' function takes a given Menu and uses it to investigate possible Steckerboards that
    are compatible with the given Crib and Offsets.
    Returns a Steckerboard (representing a possible solution) if the Menu was able to be followed with a contradiction occuring, 
    otherwise returns Nothing.
  -}  
  followMenu :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
  followMenu _ [] steckerboard _ = Just steckerboard
  followMenu crib (x:xs) steckerboard offsets
    | expandedSteckerboard == Nothing = Nothing
    -- unsteckerd pairs e.g ('A','A') are removed at each stage here, initially I removed them at the end in breakEA once there was a completed Steckerboard, but since I was unsure about how the auto-testing would 
    -- work if these pairs were left in at this stage, I've moved it to here. I'm aware it's no doubt less efficient than doing a single pass at the end.
    | otherwise = followMenu crib xs (removeUnsteckeredPairs $ fromJust expandedSteckerboard) offsets 
    where 
      enigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (repeatIncrementOffsets offsets x)
      -- Enigma encode the steckered (if applicable) plain Char of the crib and pair it with the cipher Char of the crib in the same position for form a new steckerPair
      possibleStecker = (enigmaEncode (reflect steckerboard $ (map fst crib)!!x) enigma, (map snd crib)!!x)
      expandedSteckerboard =  steckerAdd possibleStecker steckerboard
  {-
    TESTING:
    This is somewhat difficult to test in isolation and so most of the testing for this function was considered to be included in 
    the testing of later functions, most notably in breakEnigma.
    -----------------------------
     Inputs: 
      Crib:
        Plain: "ZGXWAUTS"
        Cipher: "XKGZWAUT"
      Menu: [7,6,5,4,3,0,2,1]
      Steckerboard: [('S','S')]
      Offsets: (0,0,0)
    Outputs:
      Expected: Just [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')]
      Actual:   Just [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')]
    -----------------------------
    -----------------------------
     Inputs: 
      Crib:
        Plain: "TESTINGTESTINGONETWOTHREEFOURFIVESIXSEVENEIGTHNINEIREPEATTHISISATEST"
        Cipher: "OOPFHSZHJOMTMZXLYEQJUCFVJSWZHVUMWRZPICYYANMNWBFPPUCIFBMKWIKQQMFTIQLG"
      Menu: [67,43,40,63,17,41,5,33,22,25,36,11,0,14,35,53]
      Steckerboard: [('T','T')]
      Offsets: (0,0,0)
    Outputs:
      Expected: Just [('E','I'), ('N','X'), ('R','H')]
      Actual:   Just [('X','N'),('I','E'),('H','R')]
    -----------------------------
    -----------------------------
     Inputs: 
      Crib:
        Plain: "PERFORMINGTESTINGISIMPORTANTINORDERTOSEEIFTHINGSWORK"
        Cipher: "IYDGULNVGVNZZYRPFXUQOVLTLOHDFIXUGMMDNIVMVPBVQTBYXFUH"
      Menu: [33,20,36,45,10,29,28,41,0,14,34,6,8,16,3,9]
      Steckerboard: [('E','E')]
      Offsets: (0,0,6)
    Outputs:
      Expected: [('P','I'), ('M','T'), ('O','G'),('S','F')]
      Actual:   [('T','M'),('G','O'),('P','I'),('S','F')]
    -----------------------------
  -}

  {-|
    The 'removeUnsteckeredPairs' function removes any SteckerPairs that are equivalent to being unsteckered from a given Steckerboard.
    i.e. ('A','A'), ('B','B'), ('C','C') would all be considered as unsteckered SteckerPairs.
    Returns the given Steckerboard with all unsteckered SteckerPairs removed. 
  -}
  removeUnsteckeredPairs:: Steckerboard -> Steckerboard
  removeUnsteckeredPairs steckerboard = [pair | pair@(a,b) <- steckerboard, a /= b]

  {-|
    The 'findStecker' function takes a given Menu and uses it to investigate possible Steckerboards that
    are compatible with the given Crib and Offsets.
    This function expects a Steckerboard conatining only one SteckerPair as an input, if this initial Steckerboard does not privide a solution
    the second element of the SteckerPair will be incremented by one and the process repeated. This SteckerPair is expected to be unsteckered e.g. ('A','A').
    Returns a Steckerboard (representing a possible solution) if the Menu was able to be followed with a contradiction occuring for one of the permutations
    of the inital SteckerPair, otherwise returns Nothing.
  -}
  findStecker :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
  findStecker crib menu initialSteckerboard@[(a,b)] offsets
    | offsetLetter 1 b == a = possibleStecker
    | possibleStecker == Nothing = findStecker crib menu [(a, offsetLetter 1 b)] offsets
    | otherwise = possibleStecker
    where
      possibleStecker = followMenu crib menu initialSteckerboard offsets
  {-
    TESTING:
    This is somewhat difficult to test in isolation and so the testing for this function was considered to be included in 
    the testing of later functions, most notably in breakEnigma.
  -}

  {-|
    The 'breakEA' function takes a given Menu and uses it to investigate possible Steckerboards that
    are compatible with the given Crib. If no solution is found for the given initial Offsets, these offsets are incremented
    and the process repeated.
    For all possibilitied to be tried, this function expects and initial Offsets input of (0,0,0)
    Returns an (Offsets, Steckerboard) tuple if a Steckerboard is found for a given set of initial Offsets, returns Nothing if no solution 
    if found for any offsets in the interval (0,0,0) - (25,25,25).
  -}
  breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard)
  breakEA crib menu steckerboard offsets
        | incrementOffsets offsets == (0,0,0) = 
          -- The below accounts for the final Offsets (25,25,25), without this they will be skipped.
          if possibleStecker == Nothing
          then Nothing
          else possibleSettings
        | possibleStecker == Nothing = breakEA crib menu steckerboard (incrementOffsets offsets)
        | otherwise = possibleSettings
          where
            possibleStecker = findStecker crib menu steckerboard offsets
            possibleSettings = Just (offsets, fromJust (possibleStecker))
  {-
    TESTING:
    This is somewhat difficult/ annoying to test in isolation and so the testing for this function was considered to be included in 
    the testing of later functions, most notably in breakEnigma.
    -----------------------------
     Inputs: 
      Crib:
        Plain: "TESTINGTESTINGONETWOTHREEFOURFIVESIXSEVENEIGTHNINEIREPEATTHISISATEST"
        Cipher: "OOPFHSZHJOMTMZXLYEQJUCFVJSWZHVUMWRZPICYYANMNWBFPPUCIFBMKWIKQQMFTIQLG"
      Menu: [67,43,40,63,17,41,5,33,22,25,36,11,0,14,35,53]
      Steckerboard: [('T','T')]
      Offsets: (0,0,0)
    Outputs:
      Expected: Just ((0,0,0), [('E','I'),('N','X'),('R','H')])
      Actual:   Just ((0,0,0), [('X','N'),('I','E'),('H','R')])
    -----------------------------
    -----------------------------
     Inputs: 
      Crib:
        Plain: "PERFORMINGTESTINGISIMPORTANTINORDERTOSEEIFTHINGSWORK"
        Cipher: "IYDGULNVGVNZZYRPFXUQOVLTLOHDFIXUGMMDNIVMVPBVQTBYXFUH"
      Menu: [33,20,36,45,10,29,28,41,0,14,34,6,8,16,3,9]
      Steckerboard: [('E','E')]
      Offsets: (0,0,0)
    Outputs:
      Expected: Just ((0,0,6), [('P','I'), ('M','T'), ('O','G'),('S','F')])
      Actual:   Just ((0,0,6),[('T','M'),('G','O'),('P','I'),('S','F')])
    -----------------------------

  -}
        
  {-|
    The 'breakEnigma' function attempts to find a possible Steckerboard that is compatible with the given Crib.
    This function computes the longestMenu that can be constructed from a given Crib and uses it to determine and initial SteckerPair
    for a Steckerboard, using the result to investigate possible 'complete' Steckerboards. This initial SteckerPair will be an 
    'unsteckered SteckerPair' (e.g. ('A','A')) containing the plain Char in the Crib that is located at the index of the first Menu position.
    Returns an (Offsets, Steckerboard) tuple if a Steckerboard is found for a given set of initial Offsets, otherwise returns Nothing.
  -}
  breakEnigma :: Crib -> Maybe (Offsets, Steckerboard)
  breakEnigma crib
    | menu == [] = Nothing
    | otherwise = breakEA crib menu [initialSteckerPair] (0,0,0)
    where 
      menu = longestMenu crib
      initialSteckerChar = (map fst crib)!!(menu!!0)
      initialSteckerPair = (initialSteckerChar, initialSteckerChar)
  {-
    TESTING:
    Tested by attempting to break encodings by determining Steckerboards and Offsets a then decoding messages. 
    If the message is readable enough, the verdict is that the function has worked successfully.
    -----------------------------
    ------ STAGE 1: BREAKING
    breakEnigma Inputs: 
          Crib:
            Plain: "COMPUTERSCIENCECALIBRATIONSTRINGTESTINGONETWOTHREE"
            Cipher: "QWAVMZPNGFQVGWGYCKCXXHMEXTCGWPFOCWCSYXAEFUNXQFIZJW"
    breakEnigma Outputs: 
          Offsets: (0,0,0)
          Steckerboard: [('R','M'),('G','T'),('H','S'),('U','E'),('X','A'),('W','I'),('Q','C')]
    ----- STAGE 2: DECODING
    enigmaEncodeMessage Inputs:
          Encoded Message: "QWAVMZPNGFQVGWGYCKCXXHMEXTCGWPFOCWCSYXAEFUNXQFIZJW"
          * With a SteckeredEnigma using the initial offsets + Steckerboard from the breakEnigma output.
    enigmaEncodeMessage Output:
          Decoded Message: "COMPUTERSCIENCECALIBRATIONSTRINGTESTINGONETWOTHREE"
    ----- VERDICT:
    Message was decoded successfully, matching perfectly with the plain string of the Crib as expected.
    -----------------------------
    -----------------------------
    ------ STAGE 1: BREAKING
    breakEnigma Inputs: 
          Crib:
            Plain: "TURINGBOMBEHASKELLSIMULATIONSTOP"
            Cipher: "FDLQIYHKFXSYEEXAYTWJBNNMFCHUACVM"
    breakEnigma Outputs: 
          Offsets: (1,2,14)
          Steckerboard: [('S','X'),('B','E'),('N','A'),('J','M'),('C','H'),('V','Y')]
    ----- STAGE 2: DECODING
    enigmaEncodeMessage Inputs:
          Encoded Message: "FDLQIYHKFXSYEEXAYTWJBNNMFCHUACVMERSLXIXVWCCOBSVUESKCQGKSCXSQUMCWLWXCWNDEKHCGRKAUWLSCNUUROQVOTZCWUICNEXDCQPKQKHSTFTCQJXEFKDLKOTH"
          * With a SteckeredEnigma using the initial offsets + Steckerboard from the breakEnigma output.
    enigmaEncodeMessage Output:
          Decoded Message: "TURINGBOMBEHASKELLSIMULATIONSTOPYOUSHOULDHAVEAMENUFORTHISTESTTHATISSUFFICIENTTODECODETHISMESSAGESTOPIFNOTITISALLEMMASFAULTAGAIN"
    ----- VERDICT:
    Message appears to be decoded perfectly, the full 'true' decoded message is unknown but given that the decoded english is perfectly readable this is considered a success. 
    -----------------------------
    -----------------------------
    ------ STAGE 1: BREAKING
    breakEnigma Inputs: 
          Crib:
            Plain: "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP"
            Cipher: "YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHL"
    breakEnigma Outputs: 
          Offsets: (4,3,7)
          Steckerboard: [('K','C'),('N','E'),('O','M'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')]
    ----- STAGE 2: DECODING
    enigmaEncodeMessage Inputs:
          Encoded Message: "YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUSSXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOULMTQNUFBVHFUSXYCYPWFKBYW"
          * With a SteckeredEnigma using the initial offsets + Steckerboard from the breakEnigma output.
    enigmaEncodeMessage Output:
          Decoded Message: "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP"
    ----- VERDICT:
    Message appears to be decoded perfectly, the full 'true' decoded message is unknown but given that the decoded english is perfectly readable this is considered a success. 
    -----------------------------
    NOTE: BombeTesting functions were ignored as advised in tutorial sessions.
  -}


  m = "PERFORMINGTESTINGISIMPORTANTINORDERTOSEEIFTHINGSWORK"
  s = [('P','I'), ('M','T'), ('O','G'),('S','F')]
  e = SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,6) s
  c = enigmaEncodeMessage m e
  z = zip m c
  lm= longestMenu z
  is = m!!(lm!!0)
  fm = followMenu z lm [(is,is)] (0,0,6) 
  bea = breakEA z lm [(is,is)] (0,0,0)
  be = breakEnigma z 

---------------------------------------------------------------------------------------------------  
---------- EXPERIMENT: Extending functionality to include Rotor configuration changing ------------

  type RotorList = [Rotor] -- List of possible Rotor options
  type RotorConfig = (Int, Int, Int) -- Rotor configuration, each number indicates the index of a Rotor in a RotorList
  type RotorSet = (Rotor, Rotor, Rotor) -- Represents a particular rotor configuration, to be used for returning possible Rotor configs with Steckerboards and Offsets

  standardRotorList = [rotor1, rotor2, rotor3, rotor4, rotor5] -- List of standard Rotors, for configs using this List the rotor number is its index +1


  {-|
    The 'incrementRotors' function increments a RotorConfig to move through the possible permutations.
    Returns the incremented RotorConfig.
    Note: Ideally this function would allow for incrementation for various lengths of RotorLists, however
    due to binding errors when using case I couldn't implement this.
  -}
  incrementRotors :: RotorConfig -> RotorConfig
  incrementRotors ((leftRotor, middleRotor, rightRotor)) = 
      case (leftRotor, middleRotor, rightRotor) of
        (4,4,4) -> (0,0,0)
        (4,4,_) -> (0, 0, rightRotor + 1)
        (4,_,_) -> (0, middleRotor + 1, rightRotor)
        (_,_,_) -> (leftRotor + 1, middleRotor, rightRotor)

  {-|
    The 'changeRotors' function increments a RotorConfig to move through the possible permutations, avoiding invalid configurations that
    contain duplicate Rotors.
    Return sthe incremented RotorConfig.
  -}
  changeRotors :: RotorConfig -> RotorConfig
  changeRotors rotorConfig
    | containsDuplicateRotors $ incrementRotors rotorConfig = changeRotors $ incrementRotors rotorConfig
    | otherwise = incrementRotors rotorConfig
  
  {-|
    The 'containsDuplicateRotors' is a helper function to determine if a given RotorConfig contains duplicate Rotors.
    Returns a Bool indicating whether or not duplicate rotors are present in the Rotor Config.
  -}
  containsDuplicateRotors :: RotorConfig -> Bool
  containsDuplicateRotors rotorConfig@(a,b,c) = a == b || a == c || b == c 

  {-|
    The 'followMenuWithRotors' function is an extension of the 'followMenu' function to allow for alterations to the 'internal' enigma machine used for encoding.
  -}
  followMenuWithRotors :: Crib -> Menu -> Steckerboard -> Offsets -> RotorConfig -> RotorList -> Maybe Steckerboard
  followMenuWithRotors _ [] steckerboard _ _ _ = Just steckerboard
  followMenuWithRotors crib (x:xs) steckerboard offsets rotorConfig@(lNum,mNum,rNum) rotorList
    | expandedSteckerboard == Nothing = Nothing
    | otherwise = followMenuWithRotors crib xs (removeUnsteckeredPairs $ fromJust expandedSteckerboard) offsets rotorConfig rotorList
    where 
      enigma = SimpleEnigma (rotorList!!lNum) (rotorList!!mNum) (rotorList!!rNum) reflectorB (repeatIncrementOffsets offsets x)
      possibleStecker = (enigmaEncode (reflect steckerboard $ (map fst crib)!!x) enigma, (map snd crib)!!x)
      expandedSteckerboard =  steckerAdd possibleStecker steckerboard
  {-
    TESTING:
    Note the the RotorList used for testing is the standardRotorList.
    -----------------------------
    -----STAGE 1: Encoding
    Inputs:
      Message: "THISISATESTFORROTARYENIGMABREAKINGNEEDSTOBELONGER"
      Enigma: SteckeredEnigma rotor4 rotor2 rotor1 reflectorB (0,0,0) [('T','H'),('F','R')]
    Outputs:
      Encoded Message: "LCYMBVDKXHLNSYCCWHQKPZJUDUNBLGRXQJMAWXDRTFCSTPBSJ"
    -----STAGE 2: Working in reverse, finding the Stecker
    Inputs: 
      Crib:
        Plain: "THISISATESTFORROTARYENIGMABREAKINGNEEDSTOBELONGER"
        Cipher: "LCYMBVDKXHLNSYCCWHQKPZJUDUNBLGRXQJMAWXDRTFCSTPBSJ"
      Menu: [28,33,5,16,1,11,13,6,24,17,19,21,32]
      Steckerboard: [('E','E')]
      Offsets: (0,0,0)
      RotorConfig: (3,1,0)
    Outputs:
      Expected: Just [('H','T'),('F','R')]
      Actual:   Just [('H','T'),('F','R')]
    -----------------------------
    -----------------------------
    -----STAGE 1: Encoding
    Inputs:
      Message: "ANOTHERTESTOFDECENTLENGTHTOSEEWHATHAPPENSWITHFUNCTIONS"
      Enigma: SteckeredEnigma rotor4 rotor2 rotor3 reflectorB (0,0,3) [('A','P'), ('H','C')]
    Outputs:
      Encoded Message: "MXPAQTTVMHLMGIVXURJXBUQADXROJNYDKVUIAWSCCFOJOAVHIRQEQI"
    -----STAGE 2: Working in reverse, finding the Stecker
    Inputs: 
      Crib:
        Plain: "ANOTHERTESTOFDECENTLENGTHTOSEEWHATHAPPENSWITHFUNCTIONS"
        Cipher: "MXPAQTTVMHLMGIVXURJXBUQADXROJNYDKVUIAWSCCFOJOAVHIRQEQI"
      Menu: [29,47,44,2,37,41,45,35,42,51,38,27,26,6,3,0]
      Steckerboard: [('O','O')]
      Offsets: (0,0,3)
      RotorConfig: (3,1,2)
    Outputs:
      Expected: Just [('C','H'),('A','P')]
      Actual:   Just [('C','H'),('A','P')]
    -----------------------------
  -}

  {-|
    The 'findSteckerWithRotors' function is an extension of the 'findStecker' function to allow for alterations to the 'internal' enigma machine used for encoding.
  -}    
  findSteckerWithRotors :: Crib -> Menu -> Steckerboard -> Offsets -> RotorConfig -> RotorList -> Maybe Steckerboard
  findSteckerWithRotors crib menu initialSteckerboard@[(a,b)] offsets rotorConfig rotorList
    | offsetLetter 1 b == a = possibleStecker
    | possibleStecker == Nothing = findSteckerWithRotors crib menu [(a, offsetLetter 1 b)] offsets rotorConfig rotorList
    | otherwise = possibleStecker
    where
      possibleStecker = followMenuWithRotors crib menu initialSteckerboard offsets rotorConfig rotorList
  {-
    TESTING:
    This is somewhat difficult/ annoying to test in isolation and so the testing for this function was considered to be included in 
    the testing of later functions, most notably in breakEnigma.
  -}
  
  {-|
    The 'breakEAWithRotors' function is an extension of the 'findStecker' function to allow for alterations to the 'internal' enigma machine used for encoding.
  -} 
  breakEAWithRotors :: Crib -> Menu -> Steckerboard -> Offsets -> RotorConfig -> RotorList -> Maybe (Offsets, Steckerboard)
  breakEAWithRotors crib menu steckerboard offsets rotorConfig rotorList
    | incrementOffsets offsets == (0,0,0) = 
      -- The below accounts for the final Offsets (25,25,25), without this they will be skipped.
      if possibleStecker == Nothing
      then Nothing
      else possibleSettings
    | possibleStecker == Nothing = breakEAWithRotors crib menu steckerboard (incrementOffsets offsets) rotorConfig rotorList
    | otherwise = possibleSettings
      where
        possibleStecker = findSteckerWithRotors crib menu steckerboard offsets rotorConfig rotorList
        possibleSettings = Just (offsets, fromJust (possibleStecker))
  {-
    TESTING:
    Note the the RotorList used for testing is the standardRotorList.
    -----------------------------
    -----STAGE 1: Encoding
    Inputs:
      Message: "THISISATESTFORROTARYENIGMABREAKINGNEEDSTOBELONGER"
      Enigma: SteckeredEnigma rotor4 rotor2 rotor1 reflectorB (0,0,0) [('T','H'),('F','R')]
    Outputs:
      Encoded Message: "LCYMBVDKXHLNSYCCWHQKPZJUDUNBLGRXQJMAWXDRTFCSTPBSJ"
    -----STAGE 2: Working in reverse, finding the Stecker
    Inputs: 
      Crib:
        Plain: "THISISATESTFORROTARYENIGMABREAKINGNEEDSTOBELONGER"
        Cipher: "LCYMBVDKXHLNSYCCWHQKPZJUDUNBLGRXQJMAWXDRTFCSTPBSJ"
      Menu: [28,33,5,16,1,11,13,6,24,17,19,21,32]
      Steckerboard: [('E','E')]
      Offsets: (0,0,0)
      RotorConfig: (3,1,0)
    Outputs:
      Expected: Just ((0,0,0),[('H','T'),('F','R')])
      Actual:   Just ((0,0,0),[('H','T'),('F','R')])
    -----------------------------
    -----------------------------
    -----STAGE 1: Encoding
    Inputs:
      Message: "ANOTHERTESTOFDECENTLENGTHTOSEEWHATHAPPENSWITHFUNCTIONS"
      Enigma: SteckeredEnigma rotor4 rotor2 rotor3 reflectorB (0,0,3) [('A','P'), ('H','C')]
    Outputs:
      Encoded Message: "MXPAQTTVMHLMGIVXURJXBUQADXROJNYDKVUIAWSCCFOJOAVHIRQEQI"
    -----STAGE 2: Working in reverse, finding the Stecker
    Inputs: 
      Crib:
        Plain: "ANOTHERTESTOFDECENTLENGTHTOSEEWHATHAPPENSWITHFUNCTIONS"
        Cipher: "MXPAQTTVMHLMGIVXURJXBUQADXROJNYDKVUIAWSCCFOJOAVHIRQEQI"
      Menu: [29,47,44,2,37,41,45,35,42,51,38,27,26,6,3,0]
      Steckerboard: [('O','O')]
      Offsets: (0,0,0)
      RotorConfig: (3,1,2)
    Outputs:
      Expected: Just ((0,0,3),[('C','H'),('A','P')])
      Actual:   Just ((0,0,3),[('C','H'),('A','P')])
    -----------------------------
  -}
  
  {-|
    The 'rotaryBreak' provides an additional 'layer' of functionality to the previously seen bombe simulation.
    When all Offsets and Initial SteckerPair permutations have been investigated this function will increment the Rotor Configuration
    to further investigate possible Steckerboard solutions.
  -}          
  rotaryBreak :: Crib -> Menu -> Steckerboard -> Offsets -> RotorConfig -> RotorList -> Maybe (RotorSet, (Offsets, Steckerboard))
  rotaryBreak crib menu steckerboard offsets rotorConfig@(lNum, mNum, rNum) rotorList
    | changeRotors rotorConfig == (0,1,2) = 
      -- The below accounts for the final Offsets (25,25,25), without this they will be skipped.
      if findSteckerWithRotors crib menu steckerboard offsets rotorConfig rotorList == Nothing
      then Nothing
      else possibleSettings
      -- breakEAWithRotors will only return nothing if it has gone through all offsets, so we need to reset offsets as well as the rotors
    | breakEAWithRotors crib menu steckerboard offsets rotorConfig rotorList == Nothing = rotaryBreak crib menu steckerboard (0,0,0) (changeRotors rotorConfig) rotorList
    | otherwise = possibleSettings
      where
        rotorSet = (rotorList!!lNum,rotorList!!mNum,rotorList!!rNum )
        possibleSettings = Just (rotorSet, fromJust (breakEAWithRotors crib menu steckerboard offsets rotorConfig rotorList))
  {-
    TESTING:
    Note the the RotorList used for testing is the standardRotorList.
    -----------------------------
    -----STAGE 1: Encoding
    Inputs:
      Message: "THISISATESTFORROTARYENIGMABREAKINGNEEDSTOBELONGER"
      Enigma: SteckeredEnigma rotor4 rotor2 rotor1 reflectorB (0,0,0) [('T','H'),('F','R')]
    Outputs:
      Encoded Message: "LCYMBVDKXHLNSYCCWHQKPZJUDUNBLGRXQJMAWXDRTFCSTPBSJ"
    -----STAGE 2: Working in reverse, finding the Stecker
    Inputs: 
      Crib:
        Plain: "THISISATESTFORROTARYENIGMABREAKINGNEEDSTOBELONGER"
        Cipher: "LCYMBVDKXHLNSYCCWHQKPZJUDUNBLGRXQJMAWXDRTFCSTPBSJ"
      Menu: [28,33,5,16,1,11,13,6,24,17,19,21,32]
      Steckerboard: [('E','E')]
      Offsets: (0,0,0)
      RotorConfig: (3,1,0)
    Outputs:
      Expected: Just (("ESOVPZJAYQUIRHXLNFTGKDCMWB","AJDKSIRUXBLHWTMCQGZNPYFVOE","EKMFLGDQVZNTOWYHXUSPAIBRCJ"),((0,0,0),[('H','T'),('F','R')]))
      Actual:   Just (("ESOVPZJAYQUIRHXLNFTGKDCMWB","AJDKSIRUXBLHWTMCQGZNPYFVOE","EKMFLGDQVZNTOWYHXUSPAIBRCJ"),((0,0,0),[('H','T'),('F','R')]))
    -----------------------------
    Inputs: 
      Crib:
        Plain: "THISISATESTFORROTARYENIGMABREAKINGNEEDSTOBELONGER"
        Cipher: "LCYMBVDKXHLNSYCCWHQKPZJUDUNBLGRXQJMAWXDRTFCSTPBSJ"
      Menu: [28,33,5,16,1,11,13,6,24,17,19,21,32]
      Steckerboard: [('E','E')]
      Offsets: (25,25,0)
      RotorConfig: (2,1,0)
    Outputs:
      Expected: Just (("ESOVPZJAYQUIRHXLNFTGKDCMWB","AJDKSIRUXBLHWTMCQGZNPYFVOE","EKMFLGDQVZNTOWYHXUSPAIBRCJ"),((0,0,0),[('H','T'),('F','R')]))
      Actual:   Just (("ESOVPZJAYQUIRHXLNFTGKDCMWB","AJDKSIRUXBLHWTMCQGZNPYFVOE","EKMFLGDQVZNTOWYHXUSPAIBRCJ"),((0,0,0),[('H','T'),('F','R')]))
    -----------------------------
    Inputs: 
      Crib:
        Plain: "THISISATESTFORROTARYENIGMABREAKINGNEEDSTOBELONGER"
        Cipher: "LCYMBVDKXHLNSYCCWHQKPZJUDUNBLGRXQJMAWXDRTFCSTPBSJ"
      Menu: [28,33,5,16,1,11,13,6,24,17,19,21,32]
      Steckerboard: [('E','E')]
      Offsets: (0,0,0)
      RotorConfig: (2,1,0)
    Outputs:
      Expected: Just (("ESOVPZJAYQUIRHXLNFTGKDCMWB","AJDKSIRUXBLHWTMCQGZNPYFVOE","EKMFLGDQVZNTOWYHXUSPAIBRCJ"),((0,0,0),[('H','T'),('F','R')]))
      Actual:   Just (("BDFHJLCPRTXVZNYEIWGAKMUSQO","AJDKSIRUXBLHWTMCQGZNPYFVOE","EKMFLGDQVZNTOWYHXUSPAIBRCJ"),((3,8,4),[('U','T'),('S','Y'),('L','K'),('Z','R'),('N','B'),('C','F'),('Q','M'),('O','D'),('W','X')]))
    COMMENTS: 
        Function appears to be working as anticipated, incrementing through both offsets and Rotors until a solution is found.
        As found previously this does not guarantee the CORRECT solution due to the possibility of multiple possible Steckerboards existing.
        It appears that the menu needs to be 'constrained' enough to had enough contradictions for only one result to be possible if a correct solution is desired.
    -----------------------------
    -----------------------------
    -----STAGE 1: Encoding
    Inputs:
      Message: "ANOTHERTESTOFDECENTLENGTHTOSEEWHATHAPPENSWITHFUNCTIONS"
      Enigma: SteckeredEnigma rotor4 rotor2 rotor3 reflectorB (0,0,3) [('A','M'), ('H','C')]
    Outputs:
      Encoded Message: "MXPAQTTVMHLMGIVXURJXBUQADXROJNYDKVUIAWSCCFOJOAVHIRQEQI"
    -----STAGE 2: Working in reverse, finding the Stecker
    Inputs: 
      Crib:
        Plain: "ANOTHERTESTOFDECENTLENGTHTOSEEWHATHAPPENSWITHFUNCTIONS"
        Cipher: "MXPAQTTVMHLMGIVXURJXBUQADXROJNYDKVUIAWSCCFOJOAVHIRQEQI"
      Menu: [29,47,44,2,37,41,45,35,42,51,38,27,26,6,3,0]
      Steckerboard: [('O','O')]
      Offsets: (25,25,0)
      RotorConfig: (0,1,2)
    Outputs:
      Expected: Just (("ESOVPZJAYQUIRHXLNFTGKDCMWB","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"),((0,0,3),[('A','M'), ('H','C')]))
      Actual:   Just (("ESOVPZJAYQUIRHXLNFTGKDCMWB","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"),((0,0,3),[('C','H'),('A','P')]))
    -----------------------------
  -}
  
  {-|
    The 'breakEnigmaWithRotors' function is an extension of the 'breakEnigma' function to allow for alterations to the 'internal' enigma machine used for encoding.
  -}       
  breakEnigmaWithRotors :: Crib -> RotorList -> Maybe (RotorSet, (Offsets, Steckerboard))
  breakEnigmaWithRotors crib rotorList
    | menu == [] = Nothing
    | otherwise = rotaryBreak crib menu [initialSteckerPair] (0,0,0) (0,1,2) rotorList
    where 
      menu = longestMenu crib
      initialSteckerChar = (map fst crib)!!(menu!!0)
      initialSteckerPair = (initialSteckerChar, initialSteckerChar)
  {-|
    TESTING: 
      Tricky to test as need to find a config that gives an answer that's possible to find and also complete in reasonable time.
    -----------------------------
    -----STAGE 1: Encoding
    Inputs:
      Message: "ATESTTHATSHOULDHOPEFULLYBESHORTENOUGHTOTESTBREAKENIGMA"
      Enigma: SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,3) [('E','I'), ('L','Y'), ('R','H')]
    Outputs:
      Encoded Message: "GXYVHRXKIJMNJEMSJHNRWQYMLMECTNGWPVHUYHFPAZILKVOTQJXOZO"
    -----STAGE 2: Working in reverse, finding the RotorSet, Offsets and Stecker
    Inputs: 
      Crib:
        Plain: "ANOTHERTESTOFDECENTLENGTHTOSEEWHATHAPPENSWITHFUNCTIONS"
        Cipher: "GXYVHRXKIJMNJEMSJHNRWQYMLMECTNGWPVHUYHFPAZILKVOTQJXOZO"
    Outputs:
      Expected: Just (("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"),((0,0,3),[('E','I'), ('L','Y'), ('R','H')]))
      Actual:   Just (("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"),((0,0,3),[('H','R'),('I','E'),('L','Y')]))
    -----------------------------
  -}
