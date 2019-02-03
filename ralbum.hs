import Network.MPD
import System.Random
import Data.List
import Control.Monad.Trans
import Control.Monad.Except (throwError)
import Options.Applicative
import Data.Semigroup ((<>))

idling :: Response ()
idling = Left $ Custom "idle"

data PlAction = PlAction
                { refill :: Bool
                , add    :: Bool
                , next   :: Bool }

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

-- |For now only refills playlist
main ::  IO ()
main = act =<< execParser opts
  where
    opts = info (plAction <**> helper)
      ( fullDesc
      <> progDesc "Manipulate the Music Player Daemon via albums"
      <> header "What the header should be?" )

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
act ( PlAction False True False) =
  randomAlbum >>= enqueueAlbum
  >>= \r -> case r of
              Left _ -> putStrLn "failed"
              Right _ -> putStrLn "succeeded"
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
  let plLengthPos :: MPD (Maybe Position)
      plLengthPos = status >>= \st ->
        return $ Just $ fromInteger (stPlaylistLength st)
      cSongPos = fmap stSongPos status
      plRange ml mu = (\l u -> (l, u)) <$> ml <*> mu
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
