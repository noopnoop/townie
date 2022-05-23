{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import           BasicMafia
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import           Game                           ( Game
                                                , HasElement
                                                , InputHandler
                                                , play
                                                , playDebug
                                                )
import           Test.HUnit                     ( Test
                                                  ( TestCase
                                                  , TestLabel
                                                  , TestList
                                                  )
                                                , assertBool
                                                , runTestTT
                                                )
import           Util                           ( isDefault )
import           Voting                         ( Vote(..)
                                                , VoteResult(..)
                                                , mkVotingState
                                                , vote
                                                , voting
                                                )

type Description = String

testGame
  :: (Show r, Eq r, Traversable t, HasElement r)
  => Description
  -> (r -> Bool)
  -> s
  -> InputHandler a s r
  -> t a
  -> Test
testGame desc cond init handler inputs =
  TestCase $ assertBool (desc <> " got " <> show output) $ cond output
  where output = play init handler inputs

{-
  Tests for the voting game
-}
votingPlayers :: [Text]
votingPlayers = ["p1", "p2", "p3"]
votingInit = mkVotingState votingPlayers votingPlayers
votes = [("p1", For "p2"), ("p3", For "p2")]

testVotingEmpty :: Test
testVotingEmpty =
  testGame "for voting game with no input," isDefault votingInit vote []

testVoting :: Test
testVoting =
  testGame "for voting game," ((==) $ Voted "p2") votingInit vote votes

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
                          isDefault
                          (nightStart mafiaPlayers)
                          basicMafia
                          []

testSimpleTownWin :: Test
testSimpleTownWin = testGame "for basic mafia (simple town victory),"
                             ((==) $ TeamWin Town)
                             (nightStart mafiaPlayers)
                             basicMafia
                             [("maf", For "maf")]

testTownWin :: Test
testTownWin = testGame
  "for basic mafia (more complex town win),"
  ((==) $ TeamWin Town)
  (nightStart mafiaPlayers)
  basicMafia
  [("maf", For "t4"), ("t1", For "maf"), ("t2", For "maf"), ("t3", For "maf")]

testMafiaWin :: Test
testMafiaWin = testGame
  "for basic mafia (mafia win),"
  ((==) $ TeamWin Mafia)
  (nightStart mafiaPlayers)
  basicMafia
  [ ("maf", For "t4")
  , ("maf", For "t3")
  , ("t1" , For "t3")
  , ("t2" , For "t3")
  , ("maf", For "t2")
  ]


testValidateVotes :: Test
testValidateVotes = testGame
  "for basic mafia (validating votes),"
  isDefault
  (nightStart mafiaPlayers)
  basicMafia
  [ ("maf", For "t4")
  , ("maf", For "t3")
  , ("t1" , For "t3")
  , ("t2" , For "t3")
  , ("maf", For "t3")
  , ("maf", For "t4")
  ]

tests :: Test
tests = TestList
  [ TestLabel "test voting"      testVoting
  , TestLabel "test voting"      testVotingEmpty
  , TestLabel "test basic mafia" testEmptyMafia
  , TestLabel "test basic mafia" testSimpleTownWin
  , TestLabel "test basic mafia" testTownWin
  , TestLabel "test basic mafia" testMafiaWin
  , TestLabel "test basic mafia" testValidateVotes
  ]

main :: IO ()
main = runTestTT tests >>= print
