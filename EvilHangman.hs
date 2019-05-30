-- Bradley Olson HW6
-- Evil Hangman

import Data.List
import Control.Monad
import Data.IORef


-- Put your top-level code in the body of main, and define functions as
-- necessary to help implement the game.

main =
  do
    -- Getting word length
    putStrLn "Word length?"
    wordLengthStr <- getLine
    let wordLengthInt = read wordLengthStr :: Int
    wordLength <- newIORef wordLengthInt

    -- Getting number of guesses
    putStrLn "Number of guesses?"
    numGuessStr <- getLine
    let numGuessInt = read numGuessStr :: Int
    numGuess <- newIORef numGuessInt

    --Setting lettersGuessed, currDict, and currPattern
    lettersGuessed <- newIORef ""
    currDict <- newIORef (filter (\x-> (length x)== wordLengthInt) mediumDict)
    currPattern <- newIORef (startPattern wordLengthInt "")

    -- Defining iterator
    let guessIterator = do
          -- Creating temps from IO
          tempWordLength <- readIORef wordLength
          tempNumGuess <- readIORef numGuess
          tempLettersGuessed <- readIORef lettersGuessed
          tempCurrDict <- readIORef currDict
          tempCurrPattern <- readIORef currPattern

          putStrLn ("You have "++(show tempNumGuess)++" guesses left.")
          putStrLn ("Letters guessed: "++ tempLettersGuessed)
          putStrLn ("Word: "++ tempCurrPattern)
          putStrLn ("Guess a letter:")
          guessString <- getLine
          let guess = head guessString
          let setOfPatterns = patternSet tempCurrPattern guess tempCurrDict
          let filteredSet = rmdups setOfPatterns
          let patternWordMatches = patternGetNewDict filteredSet guess tempCurrDict
          putStrLn (stringRelations patternWordMatches)
          let chosenOne = chooseBest patternWordMatches
          putStrLn ("  Using pattern " ++ (fst chosenOne) ++ ", which matches " ++ (show(length(snd chosenOne))) ++ " words")
          putStrLn (responseIterator (fst chosenOne) guess)

          -- Updating state vars
          let victory = if (elem '_' (fst chosenOne)) then False else True
          writeIORef lettersGuessed (guess:tempLettersGuessed)
          writeIORef currPattern (fst chosenOne)
          writeIORef currDict (snd chosenOne)
          writeIORef numGuess (tempNumGuess - 1)

          if (tempNumGuess-1 == 0)
             then putStrLn ("Better luck next time!")
             else putStrLn ("")
          when ((tempNumGuess-1) /= 0 && victory /= True) guessIterator
    guessIterator



-- Helper functions
startPattern :: Int -> [Char] -> [Char]
startPattern wordLength partialPattern = if (length partialPattern) == wordLength then partialPattern else startPattern wordLength ('_':partialPattern)

-- Applies pattern parts to a word and returns its pattern relative to the guess
wordPattern :: [Char] -> Char -> [Char] -> [Char]
wordPattern [] newGuess [] = []
wordPattern (patternFirst:patternRest) newGuess (firstLetter:restLetters)
  | firstLetter == newGuess = [newGuess] ++ (wordPattern patternRest newGuess restLetters)
  | otherwise = [patternFirst] ++ (wordPattern patternRest newGuess restLetters)

-- Gets the set of possible patterns expressed in the current dict
patternSet :: [Char] -> Char -> [[Char]] -> [[Char]]
patternSet currPattern newGuess [dictLast] = [(wordPattern currPattern newGuess dictLast)]
patternSet currPattern newGuess (dictFirst:dictRest) = ((wordPattern currPattern newGuess dictFirst):(patternSet currPattern newGuess dictRest))

-- Function for removing duplicates from patternSet
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

-- Determines if a word matches the given pattern
patternMatch :: [Char] -> Char -> [Char] -> Bool
patternMatch [patternLast] newestGuess [wordLast]
  | wordLast == patternLast || ((patternLast == '_') && (wordLast /= newestGuess)) = True
  | otherwise = False
patternMatch (patternFirst:patternRest) newestGuess (wordFirst:wordRest)
  | wordFirst == patternFirst || ((patternFirst == '_') && (wordFirst /= newestGuess)) = patternMatch patternRest newestGuess wordRest
  | otherwise = False

-- Determines matching word set for each pattern in patternSet
patternGetNewDict :: [[Char]] -> Char -> [[Char]] -> [([Char],[[Char]])]
patternGetNewDict [patternSetLast] newestGuess currDict = [(patternSetLast,(filter (patternMatch patternSetLast newestGuess) currDict))]
patternGetNewDict (patternSetFirst:patternSetRest) newestGuess currDict= ((patternSetFirst,(filter (patternMatch patternSetFirst newestGuess) currDict)): (patternGetNewDict patternSetRest newestGuess currDict))

-- Unloads relations between patterns and dict words to print
stringRelations [] = ""
stringRelations ((pattern,wordSet):remainingPairs) =
  "   " ++ pattern ++ " matches " ++ (show wordSet) ++ "\n" ++ (stringRelations remainingPairs)

-- Finding pattern with most matches
chooseBest [(pattern,wordSet)] = (pattern,wordSet)
chooseBest ((pattern,wordSet):remainingPairs)
  | (length wordSet) >= (length(snd(chooseBest remainingPairs))) = (pattern,wordSet)
  | otherwise = chooseBest remainingPairs

-- Response iterator chooses good guess, sorry or, you guessed it message
responseIterator chosenPattern guess
  | not(elem '_' chosenPattern) = "You guessed it! The word was \"" ++ chosenPattern ++ "\""
  | elem guess chosenPattern = "Good guess!"
  | otherwise = "Sorry, there are no " ++ [guess] ++ "'s"

-- dict definitions
------------------------------------------------------------------------------

-- These words match the ones used in the online writeup of the Evil Hangman
-- assignment.  You can use this dictionary to test the cases shown in the assignment.

trivialDict = ["ally", "beta", "cool", "deal", "else", "flew", "good", "hope", "ibex"]


-- The four-letter words in this dictionary contain only the letters 'e', 'a', 'l',
-- 's', 'r', and 't'.  You can take advantage of the limited character selection to
-- do some testing.

smallDict = ["alae", "alee", "ales", "area", "ares", "arse", "asea", "ates", "earl", "ears", "ease", "east", "eats", "eras", "etas", "lase", "late", "leal", "lear", "leas", "rale", "rare", "rase", "rate", "real", "rear", "sale", "sate", "seal", "sear", "seas", "seat", "sera", "seta", "tael", "tale", "tare", "tate", "teal", "tear", "teas", "tela"]


-- This is a larger group of four-letter words if you want a greater challenge.

mediumDict = ["abbe", "abed", "abet", "able", "abye", "aced", "aces", "ache", "acme", "acne", "acre", "adze", "aeon", "aero", "aery", "aged", "agee", "ager", "ages", "ague", "ahem", "aide", "ajee", "akee", "alae", "alec", "alee", "alef", "ales", "alme", "aloe", "amen", "amie", "anes", "anew", "ante", "aped", "aper", "apes", "apex", "apse", "area", "ares", "arse", "asea", "ates", "aver", "aves", "awed", "awee", "awes", "axed", "axel", "axes", "axle", "ayes", "babe", "bade", "bake", "bale", "bane", "bare", "base", "bate", "bead", "beak", "beam", "bean", "bear", "beat", "beau", "bema", "beta", "blae", "brae", "cade", "cafe", "cage", "cake", "came", "cane", "cape", "care", "case", "cate", "cave", "ceca", "dace", "dale", "dame", "dare", "date", "daze", "dead", "deaf", "deal", "dean", "dear", "deva", "each", "earl", "earn", "ears", "ease", "east", "easy", "eath", "eats", "eaux", "eave", "egad", "egal", "elan", "epha", "eras", "etas", "etna", "exam", "eyas", "eyra", "face", "fade", "fake", "fame", "fane", "fare", "fate", "faze", "feal", "fear", "feat", "feta", "flea", "frae", "gaed", "gaen", "gaes", "gage", "gale", "game", "gane", "gape", "gate", "gave", "gaze", "gear", "geta", "hade", "haed", "haem", "haen", "haes", "haet", "hake", "hale", "hame", "hare", "hate", "have", "haze", "head", "heal", "heap", "hear", "heat", "idea", "ilea", "jade", "jake", "jane", "jape", "jean", "kaes", "kale", "kame", "kane", "keas", "lace", "lade", "lake", "lame", "lane", "lase", "late", "lave", "laze", "lead", "leaf", "leak", "leal", "lean", "leap", "lear", "leas", "leva", "mabe", "mace", "made", "maes", "mage", "make", "male", "mane", "mare", "mate", "maze", "mead", "meal", "mean", "meat", "mesa", "meta", "nabe", "name", "nape", "nave", "neap", "near", "neat", "nema", "odea", "olea", "pace", "page", "pale", "pane", "pare", "pase", "pate", "pave", "peag", "peak", "peal", "pean", "pear", "peas", "peat", "plea", "race", "rage", "rake", "rale", "rape", "rare", "rase", "rate", "rave", "raze", "read", "real", "ream", "reap", "rear", "rhea", "sabe", "sade", "safe", "sage", "sake", "sale", "same", "sane", "sate", "save", "seal", "seam", "sear", "seas", "seat", "sera", "seta", "shea", "spae", "tace", "tael", "take", "tale", "tame", "tape", "tare", "tate", "teak", "teal", "team", "tear", "teas", "teat", "tela", "tepa", "thae", "toea", "twae", "urea", "uvea", "vale", "vane", "vase", "veal", "vela", "vena", "vera", "wade", "waes", "wage", "wake", "wale", "wame", "wane", "ware", "wave", "weak", "weal", "wean", "wear", "weka", "yare", "yeah", "yean", "year", "yeas", "zeal", "zeta", "zoea"]
