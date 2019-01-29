import qualified Network.MPD as MPD
import           System.Random

-- |For now only refills playlist
main :: IO ()
main = randomAlbum >>= print

-- |Number of albums in the database.
cardAlbums :: IO Integer
cardAlbums =
  let resp = MPD.withMPD MPD.stats
  in resp
     >>= \es -> either
                (\_ -> return (-1))
                (return . MPD.stsAlbums)
                es

-- |Chooses a random album among all available
randomAlbum :: IO (MPD.Response MPD.Value)
randomAlbum =
  let albums = MPD.withMPD $ MPD.list MPD.Album Nothing
      card = cardAlbums
      r_ind = card >>= \c -> randomRIO (0, c - 1)
  in (\irlv ri -> (\xs -> xs !! fromInteger ri) <$> irlv)
     <$> albums <*> r_ind
