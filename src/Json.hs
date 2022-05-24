{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Json
  ( parseData
  ) where
import           BasicMafia                     ( Player(Player)
                                                , Players
                                                , Team(..)
                                                )
import           Data.Aeson                     ( FromJSON
                                                , decode
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data JsonPlayer = JsonPlayer
  { name  :: Text
  , team  :: Text
  , alive :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON JsonPlayer

type PlayerId = Text
type JsonData = Map PlayerId JsonPlayer

parseData :: ByteString -> Players
parseData bs = case decode bs :: Maybe JsonData of
  Nothing   -> error "parse error"
  Just plrs -> convertData plrs


convertTeam :: Text -> Team
convertTeam "Town"  = Town
convertTeam "Mafia" = Mafia
convertTeam _       = error "error in convert team"

convertData :: JsonData -> Players
convertData = fmap doOnce
  where doOnce (JsonPlayer _ tem alv) = Player (convertTeam tem) alv

