import           Options.Applicative
import           Data.Semigroup ((<>))
import           Ralbum

main ::  IO ()
main = act =<< execParser opts
  where
    opts = info (plAction <**> helper)
      ( fullDesc
      <> progDesc "Manipulate the Music Player Daemon via albums"
      <> header "Ralbum 0.0.4 - Random album Music Player Daemon client" )
