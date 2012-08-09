{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module FourSquare (Credentials(..), searchLocation, Location(..), FQResult(..)) where
import Data.Aeson
import Data.Aeson.TH
import Data.Conduit
import Network.HTTP.Conduit
import Network.HTTP.Types
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.Text (Text)

data Credentials = Credentials {
    client_id :: ByteString,
    client_secret :: ByteString
} deriving (Show, Read, Eq)
$(deriveJSON id ''Credentials)

data Location = Location {
	fqid :: Maybe Text,
	name :: Maybe Text,
	address :: Maybe Text,
	latlon :: Maybe (Double, Double),
	poscode :: Maybe Text,
	city :: Maybe Text,
	state :: Maybe Text,
	country :: Maybe Text
} deriving (Show, Eq)

newtype FQResult = FQResult [Location]

-- | Takes an address, and a vicinity, and a search result limit n (e.g. "Art Institute", "Chicago", "3")
searchLocation :: Manager -> ByteString -> ByteString -> Int -> IO FQResult
searchLocation m ad vicinity limit = runResourceT $ do
	a <- liftIO $ parseUrl "https://api.foursquare.com/v2/venues/search"
	let ascii = renderSimpleQuery True [
		("limit", fromString (show limit)),
		("near", vicinity),
		("qualified", ad)]
	resp <- httpLbs (a { queryString = ascii}) m
	case decode (responseBody resp) of
		Just val -> return val
		Nothing -> error "Could not parse Foursquare response"

instance FromJSON FQResult where
	parseJSON (Object o) = do
		resp <- o .: "response"
		vens <- resp .: "venues"
		liftM FQResult $
			forM vens $ \v-> do
				add <- v .: "location"
				Location
					<$> v .:? "id"
					<*> v .:? "name"
					<*> add .:? "address"
					<*> liftM Just ((,) <$> add .: "lat" <*> add .: "lng")
					<*> add .:? "postalCode"
					<*> add .:? "city"
					<*> add .:? "state"
					<*> add .:? "country"
	parseJSON _ = mzero
