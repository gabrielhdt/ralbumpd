import Data.Semigroup ((<>))
import Options.Applicative
import Ralbum

data PlAction = PlAction
                { refill      :: Bool
                , add         :: Bool
                , nextAlbum   :: Bool }

main ::  IO ()
main = act =<< execParser opts
  where
    opts = info (plAction <**> helper)
      ( fullDesc
      <> progDesc "Manipulate the Music Player Daemon via albums"
      <> header "Ralbum 0.1.0 - Random album Music Player Daemon client" )

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

-- |Performs the IO action in function of the parsed command line
-- arguments
act :: PlAction -> IO ()
act (PlAction True False False) = refillPlaylist >>= dealWithFailure
act (PlAction False True False) = addRandom >>= dealWithFailure
act (PlAction False False True) = playNextAlbum >>= dealWithFailure
act PlAction {} = putStrLn "Not implemented yet"
