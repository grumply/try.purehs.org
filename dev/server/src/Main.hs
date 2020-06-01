module Main where

import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp

import Options.Applicative

-- import qualified Data.ByteString.Char8          as S8
-- import qualified Data.ByteString.Lazy           as L
-- import           Network.HTTP.Types
-- import           Network.Wai.Middleware.Gzip

data Opts = Opts
  { root :: FilePath
  }

optsParser :: ParserInfo Opts
optsParser = 
  info 
    ( opts_parser <**> helper )
    ( fullDesc
    <> progDesc "Serve a directory with a fallback to index.html"
    <> header "server - a simple HTML5 server" 
    )
  where
    opts_parser :: Parser Opts
    opts_parser = Opts <$>
      strOption 
        ( long "root"
        <> short 'r'
        <> metavar "DIRECTORY"
        <> help "Directory to serve." 
        )

main :: IO ()
main = execParser optsParser >>= staticHTML5Server

staticHTML5Server :: Opts -> IO ()
staticHTML5Server os = run 80 app -- (compressing app)
  where
    -- compressing = gzip def { gzipFiles = GzipCacheFolder "/cache" }
    app req send =
      case pathInfo req of
        ["robots.txt"]  -> fileServer req send
        ["all.js"]      -> fileServer req send
        ["favicon.ico"] -> fileServer req send
        ( "static" : _) -> fileServer req send
        _               -> fileServer req { pathInfo = ["index.html"] } send

    fileServer = staticApp (defaultFileServerSettings (root os))

