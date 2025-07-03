module GetURL (getURL) where

import Control.Applicative -- for GHC < 7.10
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Network.HTTP.Conduit

getURL :: String -> IO ByteString
getURL url = L.toStrict <$> simpleHttp url
