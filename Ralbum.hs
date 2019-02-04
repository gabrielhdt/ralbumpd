module Ralbum
  ( act
  , enqueueAlbum
  , plAction
  ) where

import           Network.MPD
import qualified System.Random as R
import           Data.List
import           Control.Monad.Trans
import           Options.Applicative



plAction :: Parser PlAction
plAction = PlAction
           <$> switch ( long "refill"
                        <> short 'r'
                        <> help "Add an album if playlist near empty" )
           <*> switch ( long "add"
                      <> short 'a'
                      <> help "Add a random album to the playlist" )
           <*> switch ( long "next"
                        <> short 'n'
                        <> help "Go to next album in playlist" )



idling :: Response ()
idling = Left $ Custom "idle"

data PlAction = PlAction
                { refill      :: Bool
                , add         :: Bool
                , nextAlbum   :: Bool }

-- |Process final response from MPD
dealWithFailure :: Response () -> IO ()
dealWithFailure (Left m) = print m
dealWithFailure (Right _) = putStrLn "succeeded"

-- |Performs the IO action in function of the parsed command line
-- arguments
act :: PlAction -> IO ()
act (PlAction True False False) =
  let remAlbums :: MPD Int
      remAlbums = fmap countAlbums remainingCurrentPlaylist
      addNeeded :: MPD Bool
      addNeeded = remAlbums >>= \i -> return $ i <= 2
      resp = withMPD $ addNeeded
        >>= \b ->
              if b
              then liftIO $ randomAlbum >>= enqueueAlbum
              else return idling
  in resp >>= \r
               -> case r of
                    Left _ -> putStrLn "failed"
                    Right _ -> putStrLn "succeeded"

act (PlAction False True False) =
  randomAlbum >>= enqueueAlbum >>= dealWithFailure

act (PlAction False False True) =
  let remSongs = fmap (Just . remBeforeNext) remainingCurrentPlaylist
      currPos = fmap stSongPos status
      nextAlbPos :: MPD (Maybe Position)
      nextAlbPos = liftA2 (+) <$> currPos <*> remSongs
  in withMPD (nextAlbPos >>= play) >>= dealWithFailure

act PlAction {} = putStrLn "Not implemented yet"

-- |Number of albums in the database.  Allows to evaluate lazily the
-- list of albums since we do not have to compute its length.
-- Could be a IO Response Integer
cardAlbums :: IO Integer
cardAlbums =
  let resp = withMPD stats
  in resp
     >>= \es -> either
                (\_ -> return (-1))
                (return . stsAlbums)
                es

-- |Choose a random album among all available
randomAlbum :: IO (Response Value)
randomAlbum =
  let albums = withMPD $ list Album Nothing
      card = cardAlbums
      r_ind = card >>= \c -> R.randomRIO (0, c - 1)
  in (\irlv ri -> (\xs -> xs !! fromInteger ri) <$> irlv)
     <$> albums <*> r_ind

-- |Enqueue an album in the current playlist
enqueueAlbum :: Response Value -> IO (Response ())
enqueueAlbum a =
  let rqu = (=?) Album <$> a :: Response Query
      resp = (rqu >>= \q -> return $ findAdd q) :: MonadMPD m => Response (m ())
  in case resp of
       Left l -> return $ Left l
       Right m -> withMPD m

-- |Compute the remaining of the playlist
remainingCurrentPlaylist :: MPD [Song]
remainingCurrentPlaylist =
  let plLengthPos :: MPD (Maybe Position)
      plLengthPos = status >>= \st ->
        return $ Just $ fromInteger (stPlaylistLength st)
      cSongPos = fmap stSongPos status
      plRange ml mu = (\l u -> (l, u)) <$> ml <*> mu
      mPlRange = plRange <$> cSongPos <*> plLengthPos
  in mPlRange >>= playlistInfoRange

-- |Count number of albums in a list of songs
countAlbums :: [Song] -> Int
countAlbums songs =
  let mayalbums = map (sgGetTag Album) songs
      albums = foldl (\acc elt ->
                        case elt of
                          Nothing -> acc
                          Just x -> head x : acc) [] mayalbums
      uniqalb = nub albums
  in length uniqalb

-- |Count number of songs before next album in the playlist
remBeforeNext :: [Song] -> Int
remBeforeNext ss =
  let currAlb = sgGetTag Album $ head ss
      loop :: Int -> [Song] -> Int
      loop k [] = k
      loop k (x : xs) =
        let mayalb = sgGetTag Album x
        in if mayalb == currAlb
           then loop (k + 1) xs
           else k
  in loop 0 ss
