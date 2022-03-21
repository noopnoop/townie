{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Lens                   ( (%~)
                                                , (^.)
                                                )
import           Control.Monad.State            ( execState )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Test.HUnit                     ( Test
                                                  ( TestCase
                                                  , TestLabel
                                                  , TestList
                                                  )
                                                , assertEqual
                                                , runTestTT
                                                )
import           Types                          ( Action(Action)
                                                , Game(Game)
                                                , GameError
                                                  ( InProgress
                                                  , InvalidAction
                                                  )
                                                , Players
                                                , act
                                                , gameState
                                                , getAction
                                                , initial
                                                , play, mkGame
                                                )
import           Voting                         ( mkVoting )

type Description = String

testSimpleGameOnInputs
  :: (Show r, Eq r)
  => Description
  -> Either GameError r
  -> [Text]
  -> Game s r
  -> Test
testSimpleGameOnInputs desc expected inputs game =
  TestCase $ assertEqual desc expected $ play inputs game
  where acns = game ^. initial

{-
  Tests for a very basic game
-}
flipIt :: Action Bool
flipIt = Action (const True) $ gameState %~ not

flipTheSwitch :: Game Bool Bool
flipTheSwitch = mkGame (Map.singleton "flip" flipIt) False (== True) id

testPlay :: Test
testPlay = testSimpleGameOnInputs "for flipTheSwitch,"
                                  (Right True)
                                  ["flip"]
                                  flipTheSwitch

testPlayEmpty :: Test
testPlayEmpty = testSimpleGameOnInputs "for flipTheSwitch with no input,"
                                       (Left InProgress)
                                       []
                                       flipTheSwitch

testBadAction :: Test
testBadAction = testSimpleGameOnInputs
  "for flipTheSwitch with a misspelled action,"
  (Left InvalidAction)
  ["fip"]
  flipTheSwitch

{-
  Tests for the voting game
-}
votingPlayers :: Players Bool
votingPlayers = Map.fromList [("p1", True), ("p2", True)]
voting = mkVoting votingPlayers

votingInput :: [Text]
votingInput = ["vote p1 for p2", "vote p2 for p2"]

testVotingEmpty :: Test
testVotingEmpty = testSimpleGameOnInputs "for voting game with no input,"
                                         (Left InProgress)
                                         []
                                         voting

testVoting :: Test
testVoting = testSimpleGameOnInputs "for voting game,"
                                    (Right $ Just "p2")
                                    votingInput
                                    voting

tests :: Test
tests = TestList
  [ TestLabel "test play"                testPlay
  , TestLabel "test play (empty case)"   testPlayEmpty
  , TestLabel "test invalid action"      testBadAction
  , TestLabel "test voting"              testVoting
  , TestLabel "test voting (empty case)" testVotingEmpty
  ]

main :: IO ()
main = runTestTT tests >>= print
