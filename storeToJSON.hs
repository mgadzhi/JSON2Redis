{-# LANGUAGE OverloadedStrings #-}

module Main where
	import Data.Aeson
	import qualified Data.Aeson.Types as T
	import Database.Redis
	import Control.Monad.IO.Class
	import System.Environment
	import qualified Data.ByteString.Char8 as BS
	import qualified Data.ByteString.Lazy.Char8 as BL
	import qualified Data.HashMap.Strict as Hash
	import Data.Text (Text)

	main = do
		args <- getArgs
		let key = args !! 0
		hash <- getHash key
		print $ encodeHash hash

	getHash :: [Char] -> IO (Hash.HashMap BS.ByteString BS.ByteString)
	getHash key = do
		conn <- connect defaultConnectInfo
		runRedis conn $ do
			respond <- hgetall $ BS.pack key
			let result = case respond of 
				Left x -> Hash.empty
				Right r -> Hash.fromList r
			return result

	encodeHash :: Hash.HashMap BS.ByteString BS.ByteString -> [Char]
	encodeHash h = BL.unpack $ encode h