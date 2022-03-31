{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Lens                   ( (%~)
                                                , (^.)
                                                )
import           Control.Monad.State            ( execState )
import           Data.Either                    ( isLeft
                                                , isRight
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Test.HUnit                     ( Test
                                                  ( TestCase
                                                  , TestLabel
                                                  , TestList
                                                  )
                                                , assertBool
                                                , assertEqual
                                                , runTestTT
                                                )
import           Types                          ( Action(Action)
                                                , Game
                                                , InProgress(InProgress)
                                                , Player
                                                , PlayerName
                                                , Players
                                                , act
                                                , getActions
                                                , play, foldGame, foldGameDebug
                                                )
import           Util                           ( mapIndicesToSet )
import           Voting                         ( Vote (..)
                                                , voting, mkVotingState, VoteResult (..)
                                                )
import BasicMafia (basicMafia, MafiaState (MafiaState))
import Data.Map (Map)

type Description = String

testGame
  :: (Show r, Eq r)
  => Description
  -> (Maybe r -> Bool)
  -> s
  -> Game s r
  -> Test
testGame desc cond init game = TestCase $ assertBool (desc <> ", got " <> show output) $ cond  $ output
  where output = play init game

{-
  Tests for a very basic game
-}
-- flipIt :: Action Bool
-- flipIt = Action (const True) not

-- flipTheSwitch :: Game Bool Bool
-- flipTheSwitch = mkGame "flipTheSwitch" (== True) id

-- testPlay :: Test
-- testPlay = testGameOnInputs "for flipTheSwitch,"
--                             ((==) $ Just True)
--                             False
--                             [flipIt]
--                             flipTheSwitch

-- testPlayEmpty :: Test
-- testPlayEmpty = testGameOnInputs "for flipTheSwitch with no input,"
--                                  isNothing
--                                  False
--                                  []
--                                  flipTheSwitch

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
                                   (voting ([]::[(Text,Vote Text)]))

testVoting :: Test
testVoting = testGame "for voting game,"
                              ((==) $ Just $ Voted "p2")
                              votingInit
                              (voting votes)

{-
  Tests for the basic version of mafia
-}

-- mafiaPlayers = Map.fromList [("p1", Player Town), ("p2", Player Town)]
-- mafiaActions = mkVotingActions (mapIndicesToSet mafiaPlayers) mafiaPlayers
-- mafiaInput =
--   getActions mafiaActions ["vote p1 for p2", "vote p2 for p2", "vote p1 for p1"]
-- mafiaSt = MafiaState (mkInitial mafiaPlayers) mafiaPlayers

-- >>> length mafiaInput
-- 3

-- >>> act (head mafiaInput) mafiaSt
-- MafiaState {_votingState = Votes {_getVotes = fromList [("p1",For "p2"),("p2",HasntVoted)]}, _players = fromList [("p1",True),("p2",True)]}

-- >>> import BasicMafia
-- >>> kill (Just "p1") mafiaSt
-- MafiaState {_votingState = Votes {_getVotes = fromList [("p1",HasntVoted),("p2",HasntVoted)]}, _players = fromList [("p2",True)]}

-- >>> import Control.Monad.Except (throwError)
-- >>> import BasicMafia
-- >>> import Types
-- >>> play mafiaSt mafiaInput $ (throwError $ InProgress "guh") >>= (handleKill $ throwError $ InProgress "wuh")
-- Left (InProgress "guh")

mafiaPlayers :: Map PlayerName PlayerName
mafiaPlayers = Map.fromList [("p1","p1"), ("p2","p2")]
mafiaInit :: MafiaState
mafiaInit = MafiaState (mkVotingState $ Map.keys mafiaPlayers) mafiaPlayers
mafiaVotes :: [(PlayerName, Vote PlayerName)]
mafiaVotes = [("p1", For "p1"), ("p2", For "p2"), ("p2", For "p1")]

testMafia :: Test
testMafia = testGame "for basic mafia,"
                             ((==) $ Just Passed)
                             mafiaInit
                             (foldGame basicMafia mafiaVotes)

tests :: Test
tests = TestList
  [ TestLabel "test voting"              testVoting
  , TestLabel "test voting (empty case)" testVotingEmpty
  , TestLabel "test basic mafia"         testMafia
  ]

main :: IO ()
main = runTestTT tests >>= print
