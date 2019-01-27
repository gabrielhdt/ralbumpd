import           Options.Applicative
import           Data.Semigroup ((<>))
import qualified Network.MPD as MPD

data ClArgs = ClArgs
  { action :: String }

clArgs :: Parser ClArgs
clArgs = ClArgs
  <$> strOption
    ( long "action"
    <> metavar "ACTION"
    <> help "Action to perform" )

main :: IO ()
main = populate =<< execParser opts
  where
    opts = info (clArgs <**> helper)
      ( fullDesc
      <> progDesc "Manage MPD playlist based on albums"
      <> header "RalbuMPD - an album based mpd client" )

populate :: ClArgs -> IO ()

-- |Get currently playing album
getCurrentAlbum :: MonadMPD m => m -> m String
getCurrentAlbum m =
  let song = currentSong m -- m Maybe song
   in
    song =<< (\x ->
                 case x of Just s -> MonadMPD sgGetTag Album s
                           Maybe -> error)
         =<< \s -> return s

-- |Remove songs from song list until album is found
clearAnteAlbum :: MonadMPD m => m Album -- ^ Album from where to keep
               -> [String] -- ^ Song list to reduce
               -> m [String]  -- ^ Shortened song list

-- |Skip first album of a list of songs
skipAlbum :: MonadMPD m => m [String] -> m [String]
