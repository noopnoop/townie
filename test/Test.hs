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
  TestCase $ assertBool (desc <> ", got " <> show output) $ cond output
  where output = play init game

{-
  Tests for the voting game
-}
votingPlayers :: [Text]
votingPlayers = ["p1", "p2"]
votingInit = mkVotingState votingPlayers
votes = [("p1", For "p1"), ("p2", For "p2")]

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
mafiaPlayers = Map.fromList [("p1", Player Mafia), ("p2", Player Town)]
mafiaVotes :: [(PlayerName, Vote PlayerName)]
mafiaVotes = [("p1", For "p1")]

testMafia :: Test
testMafia = testGame "for basic mafia,"
                     ((==) $ Just $ Map.singleton "p1" $ Player Mafia)
                     (nightStart mafiaPlayers)
                     (foldGameDebug basicMafia mafiaVotes)

tests :: Test
tests = TestList
  [ TestLabel "test voting"              testVoting
  , TestLabel "test voting (empty case)" testVotingEmpty
  , TestLabel "test basic mafia"         testMafia
  ]

main :: IO ()
main = runTestTT tests >>= print
