{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Prelude
import Prelude hiding
	( lines
	, putStrLn
	, readFile
	, sequence
	, unlines
	, unwords
	, words
	, writeFile )
import Control.Monad (when)
import Data.Maybe (listToMaybe, catMaybes)
import Data.List (sort, intersperse, partition)
import Data.Ratio ((%))
import qualified Data.Text as Text
import Data.Text
	( Text
	, justifyLeft
	, isPrefixOf
	, lines
	, unlines
	, pack
	, unpack
	, words
	, unwords )
import Control.Applicative
import Data.Text.IO (readFile, writeFile, putStrLn)
import Data.Monoid ((<>))
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Traversable (sequence)
import Options.Applicative
	( Parser
	, ParserInfo
	, argument
	, auto
	, execParser
	, fullDesc
	, header
	, help
	, helper
	, info
	, long
	, metavar
	, option
	, progDesc
	, short
	, switch
	, value
	, str )
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import System.Environment (getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath.Posix ((</>))
import HSH.ShellEquivs (glob)
import System.IO (hPrint, hFlush, stderr, stdout)
import System.Locale (TimeLocale, defaultTimeLocale)
import Text.Printf (printf)
import Text.Read (readMaybe)

programDescription :: String
programDescription =
	"Processes TagTime ping data.\n" ++
	"Designed for use on a weekly basis.\n" ++
	"Use -x -w1 to consume the ping file and only use data from last week."

defaultFile :: FilePath
defaultFile = "~/Downloads/Timepie- your timepie log"

defaultBackupDir :: FilePath
defaultBackupDir = "~/Dropbox/Growth/Logs/TagTime"

locale :: TimeLocale
locale = defaultTimeLocale

datetimeFormats :: [String]
datetimeFormats = ["%F %T", "%F", "%Y/%m/%d %T", "%Y/%m/%d"]

data Options = Options
	{ optFile :: FilePath
	, optStart :: Maybe ZonedTime
	, optEnd :: Maybe ZonedTime
	, optWeek :: Maybe Integer
	, optDestroy :: Bool
	, optBackupTo :: Maybe FilePath
	} deriving Show

optionParser :: Day -> TimeZone -> Parser Options
optionParser day tz = Options
	<$> argument auto (metavar "FILE" <> value defaultFile)
	<*> option (justed $ dateTimeReader day tz)
		(  long "start"
		<> short 's'
		<> metavar "TIME"
		<> help "Read only pings at or after TIME"
		<> value Nothing )
	<*> option (justed $ dateTimeReader day tz)
		(  long "end"
		<> short 'e'
		<> metavar "TIME"
		<> help "Read only pings strictly before TIME"
		<> value Nothing )
	<*> option (justed auto)
		(  long "week"
		<> short 'w'
		<> metavar "N"
		<> help "Read pings from N weeks ago (Mon to Sun, 0 is this week)"
		<> value Nothing )
	<*> switch
		(  long "destroy"
		<> short 'x'
		<> help "Whether to destroy FILE after reading it" )
	<*> option backupDirReader
		(  long "backup"
		<> short 'b'
		<> metavar "DIR"
		<> help (
			"Write the pings to DIR for storage. " <>
			"Defaults to somewhere nice, use NONE to prevent backup." )
		<> value (Just defaultBackupDir) )

justed :: (Functor m, Monad m) => (String -> m a) -> String -> m (Maybe a)
justed = (fmap Just .)

backupDirReader :: Monad m => String -> m (Maybe FilePath)
backupDirReader "NONE" = return Nothing
backupDirReader path = return $ Just path

dateTimeReader :: Monad m => Day -> TimeZone -> String -> m ZonedTime
dateTimeReader day tz str = firstFrom attempts where
	attempts = daysAgo : map fmtReader datetimeFormats
	fmtReader fmt = maybe Nothing convert $ parseTime locale fmt str
	convert = Just . utcToZonedTime tz
	firstFrom xs = case listToMaybe $ catMaybes xs of
		Nothing -> fail $ "Unrecognized date: " ++ str
		Just zt -> return zt
	-- TODO
	daysAgo = case Prelude.words str of
		[n, "days", "ago"] -> (flip ZonedTime tz . flip LocalTime midnight . flip addDays day . negate) <$> readMaybe n
		[n, "day", "ago"] -> (flip ZonedTime tz . flip LocalTime midnight . flip addDays day . negate) <$> readMaybe n
		_ -> Nothing

options :: String -> Day -> TimeZone -> ParserInfo Options
options name day tz = info (helper <*> optionParser day tz)
	(  fullDesc
	<> progDesc programDescription
	<> header (name ++ " - TagTime ping cruncher" ) )

data Error
	= FileNotFound String
	| BadDateSpecification
	| InvalidSyntax FilePath
	| BackupFileExists FilePath
	| Generic String
	deriving Eq
instance Show Error where
	show (FileNotFound spec) = "No such file: " ++ spec
	show BadDateSpecification = "You cannot give a start or end with --week"
	show (InvalidSyntax filename) = "Not a ping file: " ++ filename
	show (BackupFileExists filename) = "Backup file exists: " ++ filename
	show (Generic str) = str

die :: Show s => s -> IO a
die msg = hPrint stderr msg >> exitFailure

data Config = Config
	{ confFiles :: [FilePath]
	, confBounds :: TimeBounds
	, confBackupDir :: Maybe FilePath
	} deriving Show

optionsToConfig :: Options -> IO Config
optionsToConfig opts = do
	files <- glob $ optFile opts
	when (null files) (die $ FileNotFound $ optFile opts)
	bounds <- timeBounds opts
	dir <- sequence $ ensureDir <$> optBackupTo opts
	return $ Config files bounds dir

timeBounds :: Options -> IO TimeBounds
timeBounds opts = case (optStart opts, optEnd opts, optWeek opts) of
	(Nothing, Nothing, Nothing) -> return (Nothing, Nothing)
	(Nothing, Nothing, Just w) -> weekBound w
	(s, e, Nothing) -> return (zonedTimeToUTC <$> s, zonedTimeToUTC <$> e)
	_ -> die BadDateSpecification

weekBound :: Integer -> IO TimeBounds
weekBound n = do
	ZonedTime dateTime tz <- getZonedTime
	let today = localDay dateTime
	let (_, _, weekday) = toWeekDate today
	let lastMonday = addDays (toInteger $ weekday - 1) today
	let startDay = addDays (n * negate 7) lastMonday
	let endDay = addDays 7 startDay
	let start = ZonedTime (LocalTime startDay midnight) tz
	let end = ZonedTime (LocalTime endDay midnight) tz
	return (Just $ zonedTimeToUTC start, Just $ zonedTimeToUTC end)

ensureDir :: FilePath -> IO FilePath
ensureDir dir = do
	createDirectoryIfMissing True dir
	return dir

type TimeBounds = (Maybe UTCTime, Maybe UTCTime)

happenedBefore :: TimeBounds -> UTCTime -> Bool
happenedBefore (start, _) time = maybe False (time <) start

happenedAfter :: TimeBounds -> UTCTime -> Bool
happenedAfter (_, end) time = maybe False (time >=) end

type Tag = Text

data Ping = Ping
	{ pingTime :: UTCTime
	, pingTags :: [Tag]
	} deriving (Eq, Ord, Show)

tagged :: Tag -> Ping -> Bool
tagged t p = t `elem` pingTags p

taggedAny :: [Tag] -> Ping -> Bool
taggedAny ts p = any (`tagged` p) ts

taggedAll :: [Tag] -> Ping -> Bool
taggedAll ts p = all (`tagged` p) ts

renderPing :: TimeZone -> Ping -> Text
renderPing tz (Ping utct tags) = unwords (t1 : tags ++ [t2]) where
	time = utcToZonedTime tz utct
	t1 = pack $ formatTime defaultTimeLocale "%s" time
	t2 = pack $ formatTime defaultTimeLocale " [%Y.%m.%d %H:%M:%S %a]" time

parsePing :: Text -> Maybe Ping
parsePing line = do
	-- Pings are of the form NNNNNNNNNN TAG TAG ...  [YYYY.MM.DD HH:MM:SS aaa]
	-- The last three words must exist but will be ignored.
	-- (They could be used to determine what timezone the ping was sent in,
	--  but we don't use that information right now.)
	timestr : rest@(_:_:_:_) <- Just $ words line
	time <- parseTime locale "%s" $ unpack timestr
	let tags = init $ init $ init rest
	return $ Ping time tags

filePings :: TimeBounds -> FilePath -> IO [Ping]
filePings bounds filepath = do
	contents <- lines <$> readFile filepath
	let allpings = map parsePing contents
	let tooEarly = maybe False (happenedBefore bounds . pingTime)
	let tooLate = maybe False (happenedAfter bounds . pingTime)
	let	candidates = takeWhile (not . tooLate) $ dropWhile tooEarly allpings
	case sequence candidates of
		Nothing -> die $ InvalidSyntax filepath
		Just pings -> return pings

loadPings :: TimeBounds -> [FilePath] -> IO [Ping]
loadPings bounds filepaths = sort . concat <$> mapM (filePings bounds) filepaths

main :: IO ()
main = do
	name <- getProgName
	tz <- getCurrentTimeZone
	day <- localDay . zonedTimeToLocalTime <$> getZonedTime
	opts <- execParser $ options name day tz
	conf <- optionsToConfig opts
	pings <- loadPings (confBounds conf) (confFiles conf)
	maybe (return ()) (doBackup tz pings) (confBackupDir conf)
	when (optDestroy opts) (mapM_ removeFile $ confFiles conf)
	when (null pings) (putStrLn "No pings!" >> exitSuccess)

	printHarvestChart tz pings
	putStrLn ""
	printQualityGraph tz pings
	putStrLn ""
	printLevelGraph tz pings
	putStrLn ""
	printCounts tz pings

backupName :: TimeZone -> [Ping] -> FilePath
backupName tz pings = dayOf start ++ "-" ++ dayOf end where
	(start, end) = localDayBounds tz pings
	dayOf = formatTime locale "%Y%m%d"

askProceed :: FilePath -> IO ()
askProceed filename = do
	putStrLn $ (pack filename) <> " already exists."
	putStr "Proceed? [y to continue] » "
	hFlush stdout
	cont <- getLine
	when (cont /= "y") (die $ BackupFileExists filename)
	putStrLn ""

doBackup :: TimeZone -> [Ping] -> FilePath -> IO ()
doBackup tz pings filepath = mapM_ backupTo =<< glob filepath where
	backupTo root = do
		let filename = root </> backupName tz pings
		exists <- doesFileExist filename
		when exists (askProceed filename)
		let pinglines = map (renderPing tz) pings
		writeFile filename $ unlines pinglines where

restrictToDay :: TimeZone -> Day -> [Ping] -> [Ping]
restrictToDay tz = filter . onSameDayAs where
	onSameDayAs d = (d ==) . localDay . utcToLocalTime tz . pingTime

roughMinutes :: [Ping] -> Double
roughMinutes = fromIntegral . (45 *) . length

roughHours :: [Ping] -> Double
roughHours pings = fromIntegral (halfHours pings) / 2
	where halfHours pings = round (roughMinutes pings / 30) :: Integer

localDayBounds :: TimeZone -> [Ping] -> (Day, Day)
localDayBounds tz pings = (first, final) where
	first = localDay $ utcToLocalTime tz $ pingTime $ head pings
	final = localDay $ utcToLocalTime tz $ pingTime $ last pings

localDaySpan :: TimeZone -> [Ping] -> [Day]
localDaySpan tz pings = [addDays i start | i <- [0 .. diffDays end start]]
	where (start, end) = localDayBounds tz pings

printHarvestChart :: TimeZone -> [Ping] -> IO ()
printHarvestChart tz pings = do
	let labels = ["      ", "OPS   ", "OTHER ", "FAI   "] :: [Text]
	let separator = replicate 4 "|" :: [Text]
	let days = localDaySpan tz pings
	let results = map (harvestData tz pings) days
	let chart = intersperse separator (labels : results)
	mapM_ putStrLn $ foldr1 (zipWith (<>)) chart

harvestData :: TimeZone -> [Ping] -> Day -> [Text]
harvestData tz pings day = [weekday, opsTime, otherTime, faiTime] where
	weekday = pack $ formatTime locale "  %a " day
	ps = restrictToDay tz day pings
	(fai, nonfai) = partition (taggedAny ["*FAI", "*LRN"]) ps
	(other, nonother) = partition (tagged "@GROWTH") nonfai
	ops = filter (tagged "*OPS") nonother
	opsTime = pack $ printf " %4.1f " (roughHours ops)
	otherTime = pack $ printf " %4.1f " (roughHours other)
	faiTime = pack $ printf " %4.1f " (roughHours fai)

stats' :: (Day, Day) -> [Ping] -> [(Ping, Rational)] -> Text
stats' (start, end) allPings weightedPings = pack str where
	str = printf "%02.0f%% (~%02dh%02d = %02d:%02d/day, %d total)" p h m hd md c
	p = 100 * (fromRational fc / fromIntegral (length allPings)) :: Double
	mins = round $ 45 * fc :: Int
	(h, m) = mins `quotRem` 60
	dayspan = max 1 $ diffDays end start
	minpd = fromIntegral mins / fromIntegral dayspan :: Double
	hd = floor (minpd / 60) :: Int
	md = round (minpd - fromIntegral (hd * 60)) :: Int
	fc = sum $ map snd weightedPings :: Rational
	c = ceiling fc :: Int

stats :: (Day, Day) -> [Ping] -> [Ping] -> Text
stats bounds allPings pings = stats' bounds allPings (map (flip (,) 1) pings)

pingGraph :: (Day, Day) -> [Ping] -> [(Text, [Ping])] -> [Text]
pingGraph bounds pings = map Text.concat . squareUp . map (uncurry row) where
	row label ps = cells where
		cells = [label, bar, stats bounds pings ps]
		p = 100 * (fromIntegral (length ps) / fromIntegral (length pings))
		bar = pack $ replicate (round $ p / 3) '*'

tagGraph :: (Day, Day) -> [Tag] -> [Ping] -> [Text]
tagGraph bs ts ps = pingGraph bs ps [(t, filter (tagged t) ps) | t <- ts]

squareUp :: [[Text]] -> [[Text]]
squareUp rows = map justify paddedRows where
	paddedRows = [row <> replicate (maxrowlen - length row) "" | row <- rows]
	maxrowlen = maximum [length row | row <- rows]
	justify = map (uncurry justifyCell) . zip [0..]
	justifyCell i = justifyLeft (colwidth i) ' '
	colwidth i = 1 + maximum [Text.length (row !! i) | row <- paddedRows]

printQualityGraph :: TimeZone -> [Ping] -> IO ()
printQualityGraph tz pings = do
	let bounds = localDayBounds tz pings
	mapM_ putStrLn $ tagGraph bounds ["#1", "#2", "#3", "#4", "#5"] pings

printLevelGraph :: TimeZone -> [Ping] -> IO ()
printLevelGraph tz pings = do
	let bounds = localDayBounds tz pings
	let (meta, nonmeta) = partition (tagged "%META") pings
	let (upk, nonupk) = partition (tagged "@UPKEEP") nonmeta
	let (off, obj) = partition (null . pingTags) nonupk
	let content = [("META", meta), ("OBJ", obj), ("UPKEEP", upk), ("OFF", off)]
	mapM_ putStrLn $ pingGraph bounds pings content

pingFraction :: Ping -> Rational
pingFraction = (1 %) . toInteger . length . filter objtag . pingTags where
	objtag t = "*" `isPrefixOf` t || "@" `isPrefixOf` t

objTags :: [Text]
objTags =
	[ "*FAI"
	, "*LRN"
	, "*OPS"
	, "@EXERCISE"
	, "@GROWTH"
	, "@HOBBY"
	, "@REST"
	, "@SOCIAL"
	, "@UPKEEP" ]

printCounts :: TimeZone -> [Ping] -> IO ()
printCounts tz pings = do
	let bounds = localDayBounds tz pings
	let weighted tag = [(p, pingFraction p) | p <- filter (tagged tag) pings]
	let makeRow tag = [Text.drop 1 tag, stats' bounds pings (weighted tag)]
	let offRow = ["OFF", stats bounds pings (filter (null . pingTags) pings)]
	let rows = map makeRow objTags ++ [offRow]
	let lines = map Text.concat $ squareUp rows
	mapM_ putStrLn lines
