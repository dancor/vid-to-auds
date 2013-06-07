import Control.Applicative
import Control.Monad
import Data.Either.Unwrap
import Data.List
import Data.List.Utils
import HSH
import System.Directory
import System.Environment
import System.FilePath

-- Track list file format:
--
--   Audio - Album Name 1
--   0:00:00 - Song Name 1
--   0:03:45 - Song Name 2
--   ..
--   Album - Album Name 2
--   ..
--
main :: IO ()
main = do
    args <- getArgs
    case args of
      trackListFile:videoFile:[] -> vidToAuds trackListFile videoFile
      _ -> putStrLn "Usage: vid-to-auds [track-list-file] [video-file]"

vidToAuds :: FilePath -> FilePath -> IO ()
vidToAuds trackListFile videoFile = do
    trackList <- readTrackList <$> readFile trackListFile
    mapM_ (uncurry $ doAlbum videoFile) trackList

readTrackList :: String -> [(String, [((Int, Maybe Int), String)])]
readTrackList =
    eithGrp . findEnds . map readTrackLine . lines
  where
    readTrackLine :: String -> Either String (Int, String)
    readTrackLine l
        | albumPrefix `isPrefixOf` l =
            Left $ drop (length albumPrefix) l
        | otherwise =
            Right (timeSec, name)
          where
            h1:':':m2:m1:':':s2:s1:' ':'-':' ':name = l
            timeSec = (read [h1] * 60 + read [m2, m1]) * 60 + read [s2, s1]

    findEnds
        :: [Either String (Int, String)]
        -> [Either String ((Int, Maybe Int), String)]
    findEnds [] = []
    findEnds (Left x : rest) = Left x : findEnds rest
    findEnds (Right (t, name) : rest) =
        Right ((t, nextTime rest), name) : findEnds rest

    nextTime [] = Nothing
    nextTime (Left x : rest) = nextTime rest
    nextTime (Right (t, _) : _) = Just t

    albumPrefix = "Album - "

eithGrp :: (Show a, Show b) => [Either a b] -> [(a, [b])]
eithGrp [] = []
eithGrp (Left x : xs) =
    (,) x (map fromRight rs) : eithGrp rest
  where
    (rs, rest) = span isRight xs
eithGrp x = error $ "eithGrp: starts on Right: " ++ show x

doAlbum :: FilePath -> String -> [((Int, Maybe Int), String)] -> IO ()
doAlbum videoInputFile albumName trackList = do
    createDirectoryIfMissing True albumName
    let doSong n ((startSecs, endSecsMb), songName) = do
            let songNumStr = padl '0' 2 $ show n
                songFile =
                    albumName </> (songNumStr ++ "-" ++
                                  replace " " "-" songName ++ ".mp3")
            pullAudio videoInputFile songFile startSecs endSecsMb
            doTags songFile albumName songName songNumStr
    zipWithM_ doSong [1..] trackList

padl :: a -> Int -> [a] -> [a]
padl c l s = replicate (l - length s) c ++ s

pullAudio :: String -> String -> Int -> Maybe Int -> IO ()
pullAudio videoInputFile audioOutputFile startSecs endSecsMb =
    HSH.runIO ("mencoder" :: String,
        [videoInputFile, "-o", audioOutputFile, "-of", "rawaudio",
        "-oac", "mp3lame", "-ovc", "copy", "-ss", show startSecs] ++
        (case endSecsMb of
          Just endSecs -> ["-endpos", show $ endSecs - startSecs]
          _ -> []
        )
      )

doTags :: FilePath -> String -> String -> String -> IO ()
doTags songFile albumName songName songNumStr = do
    HSH.runIO ("mp3info" :: String,
        ["-l", albumName, "-t", songName, "-n", songNumStr, songFile]
        )
