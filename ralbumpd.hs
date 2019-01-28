import Network.MPD

-- |For now only refills playlist
main :: IO ()
main = cardAlbums >>= print

-- |Number of albums in the database.
cardAlbums :: IO Integer
cardAlbums =
  let resp = withMPD $ stats
  in resp
     >>= \es -> either
                (\l -> return (-1))
                (\r -> return $ stsAlbums r)
                es
