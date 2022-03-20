module SimpleGame (mkSimpleGame, SimpleGame) where
-- We define a 'Simple Game' to be a game where the actions that can be taken are always the same
-- Simple Games are easier to code as we don't have to pass around the logic for actions everywhere
import Types
import Control.Lens ((^.))

type SimpleGame s r = Game s r

mkSimpleGame ::
  ActionSet s
  -> s
  -> (s -> Bool)
  -> (s -> r)
  -> SimpleGame s r
mkSimpleGame acns init cond res =
  Game (GameState acns init) (promoteStateFn cond acns) (promoteStateFn res acns)

promoteState :: s -> ActionSet s -> GameState s
promoteState st acns = GameState acns st

promoteStateFn :: (s -> a) -> ActionSet s -> GameState s -> a
promoteStateFn fn acns st = fn $ st^.gameState