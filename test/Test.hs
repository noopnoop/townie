{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import           BasicMafia
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import           Debug.Trace                    ( traceShowId )
import           Game                           ( Emission(..)
                                                , Game
                                                , InputHandler
                                                , play
                                                , playDebug
                                                )
import           Json                           ( parseData )
import           Test.HUnit                     ( Assertable(assert)
                                                , Test
                                                  ( TestCase
                                                  , TestLabel
                                                  , TestList
                                                  )
                                                , assertBool
                                                , runTestTT
                                                )
import           Voting                         ( Vote(..)
                                                , VoteResult(..)
                                                , mkVotingState
                                                , vote
                                                , voting
                                                )

type Description = String

testGame
  :: (Show r, Eq r, Traversable t)
  => Description
  -> ([Emission r] -> Bool)
  -> s
  -> InputHandler a s r
  -> t a
  -> Test
testGame desc cond init handler inputs =
  TestCase $ assertBool (desc <> " got " <> show output) $ cond output
  where output = play init handler inputs

testGameTrace
  :: (Show r, Eq r, Traversable t)
  => Description
  -> ([Emission r] -> Bool)
  -> s
  -> InputHandler a s r
  -> t a
  -> Test
testGameTrace desc cond init handler inputs =
  TestCase $ assertBool (desc <> " got " <> show output) $ cond output
  where output = traceShowId $ play init handler inputs

testGameDebug
  :: (Show s, Show r, Eq r, Traversable t)
  => Description
  -> ([Emission r] -> Bool)
  -> s
  -> InputHandler a s r
  -> t a
  -> Test
testGameDebug desc cond init handler inputs =
  TestCase $ assertBool (desc <> " got " <> show output) $ cond output
  where output = playDebug init handler inputs

emitted :: (Eq r) => r -> [Emission r] -> Bool
emitted = elem . Emission

didNothing :: [Emission r] -> Bool
didNothing []              = True
didNothing [Start]         = True
didNothing [Start, Finish] = True
didNothing _               = False

{-
  Tests for the voting game
-}
votingPlayers :: [Text]
votingPlayers = ["p1", "p2", "p3"]
votingInit = mkVotingState votingPlayers votingPlayers
votes = [("p1", For "p2"), ("p3", For "p2")]

testVotingEmpty :: Test
testVotingEmpty =
  testGame "for voting game with no input," didNothing votingInit vote []

testVoting :: Test
testVoting =
  testGame "for voting game," (emitted $ Voted "p2") votingInit vote votes

{-
  Tests for the basic version of mafia
-}
mafiaPlayers :: Players
mafiaPlayers = Map.fromList
  [ ("maf", Player Mafia True)
  , ("t1" , Player Town True)
  , ("t2" , Player Town True)
  , ("t3" , Player Town True)
  , ("t4" , Player Town True)
  ]

testEmptyMafia :: Test
testEmptyMafia = testGame "for basic mafia (no inputs),"
                          didNothing
                          (nightStart mafiaPlayers)
                          basicMafia
                          []

testSimpleTownWin :: Test
testSimpleTownWin = testGame "for basic mafia (simple town victory),"
                             (emitted $ TeamWin Town)
                             (nightStart mafiaPlayers)
                             basicMafia
                             [("maf", For "maf")]

testTownWin :: Test
testTownWin = testGameTrace
  "for basic mafia (more complex town win),"
  (emitted $ TeamWin Town)
  (nightStart mafiaPlayers)
  basicMafia
  [("maf", For "t4"), ("t1", For "maf"), ("t2", For "maf"), ("t3", For "maf")]

testMafiaWin :: Test
testMafiaWin = testGameTrace
  "for basic mafia (mafia win),"
  (emitted $ TeamWin Mafia)
  (nightStart mafiaPlayers)
  basicMafia
  [ ("maf", For "t4")
  , ("maf", For "t3")
  , ("t1" , For "t3")
  , ("t2" , For "t3")
  , ("maf", For "t2")
  ]


-- testValidateVotes :: Test
-- testValidateVotes = testGame
--   "for basic mafia (validating votes),"
--   didNothing
--   (nightStart mafiaPlayers)
--   basicMafia
--   [ ("maf", For "t4")
--   , ("maf", For "t3")
--   , ("t1" , For "t3")
--   , ("t2" , For "t3")
--   , ("maf", For "t3")
--   , ("maf", For "t4")
--   ]

testJson :: Test
testJson = TestCase $ do
  testData <- BS.readFile "test.json"
  assertBool "for parsing json" $ parseData testData == Map.singleton
    "572603298566111241"
    (Player Town True)


tests :: Test
tests = TestList
  [ TestLabel "test voting"      testVoting
  , TestLabel "test voting"      testVotingEmpty
  , TestLabel "test basic mafia" testEmptyMafia
  , TestLabel "test basic mafia" testSimpleTownWin
  , TestLabel "test basic mafia" testTownWin
  , TestLabel "test basic mafia" testMafiaWin
  -- , TestLabel "test basic mafia" testValidateVotes
  , TestLabel "test json parser" testJson
  ]

main :: IO ()
main = runTestTT tests >>= print
