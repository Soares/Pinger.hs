{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Prelude hiding (putStr, putStrLn, readFile, sequence, writeFile)
import Control.Applicative
import Control.Monad (when)
import Data.List (sort, sortBy, intersperse, partition)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO (putStr, putStrLn, readFile, writeFile)
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Traversable (sequence)
import HSH.ShellEquivs (glob)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import System.Environment (getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath.Posix ((</>))
import System.IO (hPrint, hFlush, stderr, stdout)
import System.Locale (TimeLocale, defaultTimeLocale)
import Text.Printf (printf)
import Text.Read (readMaybe)

programDescription :: String
programDescription =
	"Processes TagTime ping data. Designed for use on a weekly basis. " <>
	"Automatically backs up the file somewhere useful. " <>
	"Suggested usage is -x to remove the original file, and -w1 to " <>
	"restrict consideration to pings made in the last week."

defaultFile :: FilePath
defaultFile = "~/Downloads/Timepie- your timepie log"

locale :: TimeLocale
locale = defaultTimeLocale

datetimeFormats :: [String]
datetimeFormats = ["%F %T", "%F", "%d/%m/%Y %T", "%d/%m/%Y"]

data Options = Options
	{ optFile :: FilePath
	, optStart :: Maybe ZonedTime
	, optEnd :: Maybe ZonedTime
	, optWeek :: Maybe Integer
	, optDestroy :: Bool
	, optBackupTo :: Maybe FilePath
	} deriving Show

optionParser :: ZonedTime -> Parser Options
optionParser hereAndNow = Options
	<$> argument str
		(  metavar "FILE"
		<> value defaultFile )
	<*> option (fmap Just . dateTimeReader hereAndNow)
		(  long "start"
		<> short 's'
		<> metavar "TIME"
		<> help "Read only pings at or after TIME"
		<> value Nothing )
	<*> option (fmap Just . dateTimeReader hereAndNow)
		(  long "end"
		<> short 'e'
		<> metavar "TIME"
		<> help "Read only pings strictly before TIME"
		<> value Nothing )
	<*> option (fmap Just . auto)
		(  long "week"
		<> short 'w'
		<> metavar "N"
		<> help "Read pings from N weeks ago (Mon to Sun, 0 is this week)"
		<> value Nothing )
	<*> switch
		(  long "destroy"
		<> short 'x'
		<> help "Whether to destroy FILE after reading it" )
	<*> option (fmap Just . str)
		(  long "backup"
		<> short 'b'
		<> metavar "DIR"
		<> help (
			"Write the pings to DIR for storage. " <>
			"Defaults to somewhere nice, use NONE to prevent backup." )
		<> value Nothing )

dateTimeReader :: Monad m => ZonedTime -> String -> m ZonedTime
dateTimeReader hereAndNow input = result $ listToMaybe $ catMaybes parses where
	result = maybe (fail $ "unrecognized date \"" ++ input ++ "\"") return
	parses = byOffset : map byFormat datetimeFormats
	byFormat format = maybe Nothing toLocalZone $ parseTime locale format input
	byOffset = case words input of
		[n, offset, "ago"]
			| offset `elem` ["day", "days"] -> daysAgo <$> readMaybe n
			| offset `elem` ["week", "weeks"] -> daysAgo . (7 *) <$> readMaybe n
		_ -> Nothing
	ZonedTime (LocalTime today _) here = hereAndNow
	toLocalZone = Just . utcToZonedTime here
	daysAgo n = ZonedTime (LocalTime (addDays (negate n) today) midnight) here

options :: String -> ZonedTime -> ParserInfo Options
options name hereAndNow = info (helper <*> optionParser hereAndNow)
	(  fullDesc
	<> progDesc programDescription
	<> header (name ++ " - TagTime ping cruncher" ) )

data Error
	= FileNotFound String
	| BadDateSpecification
	| InvalidSyntax FilePath
	| Generic String
	deriving Eq
instance Show Error where
	show (FileNotFound spec) = "No such file: " ++ spec
	show BadDateSpecification = "You cannot give a start or end with --week"
	show (InvalidSyntax filename) = "Not a ping file: " ++ filename
	show (Generic msg) = msg

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
	let lastMonday = addDays (negate $ toInteger $ weekday - 1) today
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
	, pingTimeZone :: TimeZone
	, pingTags :: Set Tag
	} deriving (Eq, Ord)

instance Show Ping where
	show p = unwords (t1 : tags ++ [t2]) where
		t1 = formatTime defaultTimeLocale "%s" $ pingTime p
		tags = map Text.unpack $ Set.toAscList $ pingTags p
		t2 = formatTime locale " [%Y.%m.%d %H:%M:%S %a]" $ pingTimeHere p

pingTimeHere :: Ping -> ZonedTime
pingTimeHere = utcToZonedTime <$> pingTimeZone <*> pingTime

pingDayHere :: Ping -> Day
pingDayHere = localDay . zonedTimeToLocalTime . pingTimeHere

tagged :: Tag -> Ping -> Bool
tagged t p = t `Set.member` pingTags p

parsePing :: TimeZone -> Text -> Maybe Ping
parsePing here line = do
	-- Pings are of the form NNNNNNNNNN TAG TAG ...  [YYYY.MM.DD HH:MM:SS aaa]
	-- The last three words must exist but will be ignored.
	timestr : rest@(_:_:_:_) <- Just $ Text.words line
	time <- parseTime locale "%s" $ Text.unpack timestr
	let tags = init $ init $ init rest
	return $ Ping time here (Set.fromList tags)

filePings :: TimeBounds -> TimeZone -> FilePath -> IO [Ping]
filePings bounds here filepath = do
	contents <- Text.lines <$> readFile filepath
	let allpings = map (parsePing here) contents
	let tooEarly = maybe False (happenedBefore bounds . pingTime)
	let tooLate = maybe False (happenedAfter bounds . pingTime)
	let	candidates = takeWhile (not . tooLate) $ dropWhile tooEarly allpings
	maybe (die $ InvalidSyntax filepath) return $ sequence candidates

loadPings :: TimeBounds -> TimeZone -> [FilePath] -> IO [Ping]
loadPings bounds here filepaths = sort . concat <$> getAllPings where
	getAllPings = mapM (filePings bounds here) filepaths

main :: IO ()
main = do
	name <- getProgName
	hereAndNow@(ZonedTime _ here) <- getZonedTime
	opts <- execParser $ options name hereAndNow
	conf <- optionsToConfig opts
	pings <- loadPings (confBounds conf) here (confFiles conf)
	maybe (return ()) (doBackup pings) (confBackupDir conf)
	when (optDestroy opts) (mapM_ removeFile $ confFiles conf)
	when (null pings) (putStrLn "No pings!" >> exitSuccess)
	when (not $ optDestroy opts) (printData pings)

printData :: [Ping] -> IO ()
printData pings = do
	printHarvestChart pings
	putStrLn ""
	printQualityGraph pings
	putStrLn ""
	printLevelGraph pings
	putStrLn ""
	printCounts objTags pings
	putStrLn ""
	printCounts projTags pings

doBackup :: [Ping] -> FilePath -> IO ()
doBackup pings filepath = mapM_ backupTo =<< glob filepath where
	backupTo root = do
		let filename = root </> backupName
		exists <- doesFileExist filename
		proceed <- if exists then askProceed filename else pure True
		when proceed (writeFile filename contents)
	askProceed filename = do
		putStrLn $ Text.pack filename <> " already exists."
		putStr "[y to overwrite] » "
		hFlush stdout
		cont <- getLine
		putStrLn ""
		return (cont == "y")
	(start, end) = localDayBounds pings
	backupName = yyyymmdd start ++ "-" ++ yyyymmdd end
	yyyymmdd = formatTime locale "%Y%m%d"
	contents = Text.unlines $ map (Text.pack . show) pings

localDayBounds :: [Ping] -> (Day, Day)
localDayBounds pings = (pingDayHere $ head pings, pingDayHere $ last pings)

localDaySpan :: [Ping] -> [Day]
localDaySpan pings = [addDays i start | i <- [0 .. diffDays end start]]
	where (start, end) = localDayBounds pings

printHarvestChart :: [Ping] -> IO ()
printHarvestChart pings = do
	let labels = ["      ", "OPS   ", "OTHER ", "FAI   "] :: [Text]
	let separator = replicate 4 "|" :: [Text]
	let days = localDaySpan pings
	let results = map (harvestData pings) days
	let chart = intersperse separator (labels : results)
	mapM_ putStrLn $ foldr1 (zipWith (<>)) chart

harvestData :: [Ping] -> Day -> [Text]
harvestData pings day = map Text.pack [wkd, opsTime, otherTime, faiTime] where
	wkd = formatTime locale "  %a " day
	miri = filter (\p -> tagged "@MIRI" p && pingDayHere p == day) pings
	(fai, nonfai) = partition ((&&) <$> (tagged "FAI") <*> (tagged "WRI")) miri
	(ops, other) = partition (tagged "UPK") nonfai

	opsTime = printf " %4.1f " (roughHours ops)
	otherTime = printf " %4.1f " (roughHours other)
	faiTime = printf " %4.1f " (roughHours fai)

	roughMinutes xs = fromIntegral $ 45 * length xs :: Double
	halfHours xs = round (roughMinutes xs / 30) :: Integer
	roughHours xs = fromIntegral (halfHours xs) / 2 :: Double

statistics :: [Ping] -> [(Ping, Rational)] -> Text
statistics pings weighted = Text.pack $ printf fmt h m hpd mpd cnt where
	fmt = "(~%02dh%02d = %02d:%02d/day, %d total)"
	(h, m) = totalMinutes `quotRem` 60
	hpd = floor (minutesPerDay / 60) :: Int
	mpd = round (minutesPerDay - fromIntegral (hpd * 60)) :: Int
	cnt = ceiling fractionalCount :: Int

	dayspan = max 1 $ uncurry (flip diffDays) $ localDayBounds pings
	totalMinutes = round $ 45 * fractionalCount :: Int
	minutesPerDay = fromIntegral totalMinutes / fromIntegral dayspan :: Double
	fractionalCount = sum $ map snd weighted :: Rational

unitWeight :: [Ping] -> [(Ping, Rational)]
unitWeight = map (flip (,) 1)

percentage :: [Ping] -> [(Ping, Rational)] -> Double
percentage pings weighted = 100 * fromRational w / fromIntegral n where
	w = sum $ map snd weighted
	n = length pings

pingRow :: [Ping] -> Text -> [(Ping, Rational)] -> [Text]
pingRow pings label weighted = [label, bar, percent, stats] where
	p = percentage pings weighted
	bar = Text.pack $ replicate (round $ p / 3) '*'
	percent = Text.pack $ printf "%02.0f%%" p
	stats = statistics pings weighted

pingGraph :: [Ping] -> [(Text, [(Ping, Rational)])] -> [Text]
pingGraph pings = map Text.concat . squareUp . map (uncurry $ pingRow pings)

squareUp :: [[Text]] -> [[Text]]
squareUp rows = map justify paddedRows where
	paddedRows = [row <> replicate (maxrowlen - length row) "" | row <- rows]
	maxrowlen = maximum [length row | row <- rows]
	justify = zipWith justifyCell [0..]
	justifyCell i = Text.justifyLeft (colwidth i) ' '
	colwidth i = 1 + maximum [Text.length (row !! i) | row <- paddedRows]

tagGraph :: [Tag] -> [Ping] -> [Text]
tagGraph ts ps = pingGraph ps [(t, unitWeight $ filter (tagged t) ps) | t <- ts]

printQualityGraph :: [Ping] -> IO ()
printQualityGraph = mapM_ putStrLn . tagGraph labels where
	labels = ["#ADD", "#DIS", "#DRT", "#FLO", "#OUT", "#TRG"]

printLevelGraph :: [Ping] -> IO ()
printLevelGraph pings = do
	let isOff = (||) <$> (tagged "OFF") <*> (Set.null . pingTags)
	let (meta, nonmeta) = partition (tagged "%META") pings
	let (upk, nonupk) = partition (tagged "UPK") nonmeta
	let (off, obj) = partition isOff nonupk
	let content =
		[ ("UPKEEP", unitWeight upk)
		, ("META", unitWeight meta)
		, ("OBJ", unitWeight obj)
		, ("OFF", unitWeight off) ]
	mapM_ putStrLn $ pingGraph pings content

pingFraction :: Ping -> Rational
pingFraction = (1 %) . toInteger . Set.size . Set.filter objtag . pingTags where
	objtag t = "*" `Text.isPrefixOf` t || "@" `Text.isPrefixOf` t

objTags :: [Text]
objTags =
	[ "CSM"
	, "EAT"
	, "EXR"
	, "FAI"
	, "HYG"
	, "LRN"
	, "MHX"
	, "NWK"
	, "OoO"
	, "PRT"
	, "TCH"
	, "TRA"
	, "WRI" ]

projTags :: [Text]
projTags =
	[ "@Alli"
	, "@LoH"
	, "@MIRI"
	, "@MoW"
	, "@Nate" ]

printCounts :: [Text] -> [Ping] -> IO ()
printCounts tags pings = do
	let weighted tag = [(p, pingFraction p) | p <- filter (tagged tag) pings]
	let rows = [(tag, weighted tag) | tag <- tags]
	let mostWeight (_, a) (_, b) = compare (sum $ map snd b) (sum $ map snd a)
	let sortedRows = sortBy mostWeight rows
	mapM_ putStrLn $ pingGraph pings sortedRows
