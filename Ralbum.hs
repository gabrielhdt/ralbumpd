module Ralbum
  ( addRandom
  , playNextAlbum
  , refillPlaylist
  , dealWithFailure
  ) where

import Control.Applicative
import Control.Monad.Trans
import Data.List
import Network.MPD
import System.IO
import System.Random (randomRIO)

minAlbum :: Int
minAlbum = 2

-- |Process final response from MPD
dealWithFailure :: Response () -> IO ()
dealWithFailure (Left m) = hPutStr stderr $ show m
dealWithFailure (Right _) = return ()

-- |Add a random album to the playlist
addRandom :: IO (Response ())
addRandom = withMPD $ randomAlbum >>= enqueueAlbum

-- |Play next album in the playlist
playNextAlbum :: IO (Response ())
playNextAlbum =
  let remSongs = fmap (Just . remBeforeNext) remainingCurrentPlaylist
      currPos = fmap stSongPos status
      nextAlbPos = liftA2 (+) <$> currPos <*> remSongs
  in withMPD $ nextAlbPos >>= play

-- |Add a random album in playlist only if playlist almost
-- exhausted
refillPlaylist :: IO (Response ())
refillPlaylist =
  let remAlbums = fmap countAlbums remainingCurrentPlaylist
      todo = (\remA op ->
                if remA <= minAlbum
                then ()
                else op)
             <$> remAlbums
             <*> (randomAlbum >>= enqueueAlbum)
  in withMPD todo

-- |Choose a random album among all available
randomAlbum :: MPD Value
randomAlbum =
  let albums = list Album Nothing :: MPD [Value]
      r_ind = albums >>= \as -> liftIO $ randomRIO (0, length as - 1)
  in (!!) <$> albums <*> r_ind

-- |Return the MPD action to enqueue an album
enqueueAlbum :: Value -> MPD ()
enqueueAlbum = findAdd . (=?) Album

-- |Compute the remaining of the playlist
remainingCurrentPlaylist :: MPD [Song]
remainingCurrentPlaylist =
  let plLengthPos :: MPD (Maybe Position)
      plLengthPos = status >>= \st ->
        return $ Just $ fromInteger (stPlaylistLength st)
      cSongPos = fmap stSongPos status
      mPlRange = liftA2 (,) <$> cSongPos <*> plLengthPos
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
