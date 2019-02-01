import Network.MPD
import System.Random
import Data.List
import Control.Monad.Trans
import Control.Monad.Except (throwError)

idling :: Response ()
idling = Left $ Custom "idle"

-- |For now only refills playlist
main ::  IO (MPD ())
main =
  let remAlbums :: MPD Int
      remAlbums = fmap countAlbums remainingCurrentPlaylist
      addNeeded :: MPD Bool
      addNeeded = remAlbums >>= \i -> return $ i <= 2
      resp ::  MPD (Response ())
      resp = addNeeded >>= \b ->
                             if b
                             then liftIO $ randomAlbum >>= enqueueAlbum
                             else liftIO $ return idling
  in return $ resp >>= \r ->
    case r of
      Left err -> throwError err
      Right _ -> return ()

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

-- |Chooses a random album among all available
randomAlbum :: IO (Response Value)
randomAlbum =
  let albums = withMPD $ list Album Nothing
      card = cardAlbums
      r_ind = card >>= \c -> randomRIO (0, c - 1)
  in (\irlv ri -> (\xs -> xs !! fromInteger ri) <$> irlv)
     <$> albums <*> r_ind

-- |Enqueues an album in the current playlist
enqueueAlbum :: Response Value -> IO (Response ())
enqueueAlbum a =
  let rqu = (=?) Album <$> a :: Response Query
      resp = (rqu >>= \q -> return $ findAdd q) :: MonadMPD m => Response (m ())
  in case resp of
       Left l -> return $ Left l
       Right m -> withMPD m

-- |Computes the remaining of the playlist
remainingCurrentPlaylist :: MPD [Song]
remainingCurrentPlaylist =
  let plLengthPos :: MonadMPD m => m (Maybe Position)
      plLengthPos = status >>= \st ->
        return $ Just $ fromInteger (stPlaylistLength st)
      cSongPos :: MonadMPD m => m (Maybe Position)
      cSongPos = fmap stSongPos status
      plRange :: Maybe Position -> Maybe Position -> Maybe (Position, Position)
      plRange ml mu = (\l u -> (l, u)) <$> ml <*> mu
      mPlRange :: MonadMPD m => m (Maybe (Position, Position))
      mPlRange = plRange <$> cSongPos <*> plLengthPos
  in mPlRange >>= playlistInfoRange

-- |Counts number of albums in a list of songs
countAlbums :: [Song] -> Int
countAlbums songs =
  let mayalbums = map (sgGetTag Album) songs
      albums = foldl (\acc elt ->
                        case elt of
                          Nothing -> acc
                          Just x -> head x : acc) [] mayalbums
      uniqalb = nub albums
  in length uniqalb
