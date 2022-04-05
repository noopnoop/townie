{-# LANGUAGE OverloadedStrings #-}
module Main where
import           BasicMafia
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import           Test.HUnit                     ( Test
                                                  ( TestCase
                                                  , TestLabel
                                                  , TestList
                                                  )
                                                , assertBool
                                                , runTestTT
                                                )
import           Types                          ( Game
                                                , foldGame
                                                , foldGameDebug
                                                , play
                                                )
import           Voting                         ( Vote(..)
                                                , VoteResult(..)
                                                , mkVotingState
                                                , voting
                                                )

type Description = String

testGame
  :: (Show r, Eq r) => Description -> (Maybe r -> Bool) -> s -> Game s r -> Test
testGame desc cond init game =
  TestCase $ assertBool (desc <> " got " <> show output) $ cond output
  where output = play init game

{-
  Tests for the voting game
-}
votingPlayers :: [Text]
votingPlayers = ["p1", "p2", "p3"]
votingInit = mkVotingState votingPlayers votingPlayers
votes = [("p1", For "p2"), ("p3", For "p2")]

testVotingEmpty :: Test
testVotingEmpty = testGame "for voting game with no input,"
                           isNothing
                           votingInit
                           (voting ([] :: [(Text, Vote Text)]))

testVoting :: Test
testVoting = testGame "for voting game,"
                      ((==) $ Just $ Voted "p2")
                      votingInit
                      (voting votes)

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
simpleTownWinVotes :: [(PlayerName, Vote PlayerName)]
simpleTownWinVotes = [("maf", For "maf")]

townWinVotes :: [(PlayerName, Vote PlayerName)]
townWinVotes =
  [("maf", For "t4"), ("t1", For "maf"), ("t2", For "maf"), ("t3", For "maf")]

mafiaWinVotes :: [(PlayerName, Vote PlayerName)]
mafiaWinVotes =
  [ ("maf", For "t4")
  , ("maf", For "t3")
  , ("t1" , For "t3")
  , ("t2" , For "t3")
  , ("maf", For "t2")
  ]

validateVoteVotes :: [(PlayerName, Vote PlayerName)]
validateVoteVotes =
  [ ("maf", For "t4")
  , ("maf", For "t3")
  , ("t1" , For "t3")
  , ("t2" , For "t3")
  , ("maf", For "t3")
  , ("maf", For "t4")
  ]

testEmptyMafia :: Test
testEmptyMafia = testGame "for basic mafia (no inputs),"
                          isNothing
                          (nightStart mafiaPlayers)
                          (foldGame basicMafia [])

testSimpleTownWin :: Test
testSimpleTownWin = testGame "for basic mafia (simple town victory),"
                             ((==) $ Just $ TeamWin Town)
                             (nightStart mafiaPlayers)
                             (foldGame basicMafia simpleTownWinVotes)

testTownWin :: Test
testTownWin = testGame "for basic mafia (more complex town win),"
                       ((==) $ Just $ TeamWin Town)
                       (nightStart mafiaPlayers)
                       (foldGame basicMafia townWinVotes)

testMafiaWin :: Test
testMafiaWin = testGame "for basic mafia (mafia win),"
                        ((==) $ Just $ TeamWin Mafia)
                        (nightStart mafiaPlayers)
                        (foldGame basicMafia mafiaWinVotes)

testValidateVotes :: Test
testValidateVotes = testGame "for basic mafia (validating votes),"
                             isNothing
                             (nightStart mafiaPlayers)
                             (foldGame basicMafia validateVoteVotes)

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
