module App.Lenses
  ( _Fixture
  , _Link
  , _LinkGroup
  , _Result
  , result
  , goalsHomeTeam
  , goalsAwayTeam
  ) where

import Prelude
import Optic.Core
import App.Types
import Data.Maybe (Maybe(..))
import Data.DateTime (DateTime(..))

-- Lenses

_Fixture :: Lens' Fixture {_links:: LinkGroup, date :: DateTime, status :: String, matchday :: Number, homeTeamName :: String, awayTeamName :: String, result:: Result}
_Fixture f (Fixture b) = Fixture <$> f b

_Link :: Lens' Link { href :: String}
_Link f (Link b) = Link <$> f b

_LinkGroup :: Lens' LinkGroup {self :: Link, competition :: Link, homeTeam :: Link, awayTeam :: Link}
_LinkGroup f (LinkGroup b) = LinkGroup <$> f b

_Result :: Lens' Result {goalsHomeTeam :: Maybe Number, goalsAwayTeam :: Maybe Number}
_Result f (Result b) = Result <$> f b

result :: forall a b r. Lens { result :: a | r } { result :: b | r } a b
result = lens _.result (_ { result = _ })

goalsHomeTeam :: forall b a r. Lens {goalsHomeTeam :: a | r} {goalsHomeTeam :: b | r} a b
goalsHomeTeam = lens _.goalsHomeTeam (_ { goalsHomeTeam = _})

goalsAwayTeam :: forall b a r. Lens {goalsAwayTeam :: a | r} {goalsAwayTeam :: b | r} a b
goalsAwayTeam = lens _.goalsAwayTeam (_ { goalsAwayTeam = _})
