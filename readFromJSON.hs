{-# LANGUAGE OverloadedStrings #-}

module Main where
	import Data.Aeson
	import qualified Data.Aeson.Types as T
	import Database.Redis
	import Control.Monad.IO.Class
	import System.Environment
	import System.IO
	import qualified Data.ByteString.Char8 as BS
	import qualified Data.ByteString.Lazy.Char8 as BL
	import qualified Data.HashMap.Strict as Hash
	import Data.Attoparsec

	main :: IO()
	main = do
		args <- getArgs
		let fileName = head args
		withFile fileName ReadMode (\handle -> do
			contents <- hGetContents handle
			let hash = case parseHash contents of
				Just result -> result
				Nothing -> Hash.empty
			conn <- connect defaultConnectInfo
			runRedis conn $ do
				storeToRedis $ Hash.toList hash
				)
		return ()

	parseHash :: [Char] -> Maybe (Hash.HashMap BS.ByteString (Hash.HashMap BS.ByteString BS.ByteString))
	parseHash s = let bs = BS.pack s
		in case parse json bs of
			(Done rest r) -> T.parseMaybe parseJSON r :: Maybe (Hash.HashMap BS.ByteString (Hash.HashMap BS.ByteString BS.ByteString))
			_ -> Nothing

	testString' :: [Char]
	testString' = "{\"case:1\":{\"id\":\"1\",\"address\":\"asdf\"},\"case:2\":{\"id\":\"2\",\"address\":\"fdsa\"}}"

	storeToRedis :: [(BS.ByteString, Hash.HashMap BS.ByteString BS.ByteString)] -> Redis (Either Reply Status)
	storeToRedis [] = return $ Right Ok
	storeToRedis (x:xs) = do
		st <- hmset (fst x) (Hash.toList $ snd x)
		storeToRedis xs















