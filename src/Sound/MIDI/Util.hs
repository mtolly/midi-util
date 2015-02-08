{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.MIDI.Util
( Beats(..), Seconds(..), BPS(..)
, decodeFile, encodeFileBeats, minResolution
, makeTempo, applyTempo, unapplyTempo, applyTempoTrack, unapplyTempoTrack
, TempoMap, makeTempoMap, applyTempoMap, unapplyTempoMap
, MeasureMap, MeasureBeats, MeasureMode(..), measures, makeMeasureMap
, applyMeasureMap, unapplyMeasureMap
, readTempo, showTempo
, readSignature, showSignature
, trackName, readTrackName, showTrackName
, trackJoin, trackSplit, trackTake, trackDrop
) where

import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid (Monoid)
import Data.Ratio (numerator, denominator)

import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NNC

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta

import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB

-- | A hack to look up mappings in a 'Map.Map' using either one of
-- two key types, which increase together.
data DoubleKey a b
  = DoubleKey !a !b
  | LookupA   !a
  | LookupB      !b
  deriving (Show, Read)

instance (Ord a, Ord b) => Eq (DoubleKey a b) where
  dk1 == dk2 = compare dk1 dk2 == EQ

instance (Ord a, Ord b) => Ord (DoubleKey a b) where

  compare (DoubleKey a1 _ ) (DoubleKey a2 _ ) = compare a1 a2 -- A is arbitrary
  compare (DoubleKey a1 _ ) (LookupA   a2   ) = compare a1 a2
  compare (DoubleKey _  b1) (LookupB      b2) = compare b1 b2

  compare (LookupA   a1   ) (DoubleKey a2 _ ) = compare a1 a2
  compare (LookupA   a1   ) (LookupA   a2   ) = compare a1 a2
  compare (LookupA   _    ) (LookupB      _ ) = error
    "compare: internal error! tried to compare LookupA and LookupB"

  compare (LookupB      b1) (DoubleKey _  b2) = compare b1 b2
  compare (LookupB      _ ) (LookupA      _ ) = error
    "compare: internal error! tried to compare LookupB and LookupA"
  compare (LookupB      b1) (LookupB      b2) = compare b1 b2

-- | Musical time, measured in beats a.k.a. quarter notes.
newtype Beats = Beats { fromBeats :: NN.Rational }
  deriving (Eq, Ord, Show, Monoid, NNC.C, Num, Real, Fractional, RealFrac)

-- | Real time, measured in seconds.
newtype Seconds = Seconds { fromSeconds :: NN.Rational }
  deriving (Eq, Ord, Show, Monoid, NNC.C, Num, Real, Fractional, RealFrac)

-- | A ratio between musical time and real time, measured in beats per second.
newtype BPS = BPS { fromBPS :: NN.Rational }
  deriving (Eq, Ord, Show, Monoid, NNC.C, Num, Real, Fractional, RealFrac)

makeTempo :: Beats -> Seconds -> BPS
makeTempo (Beats b) (Seconds s) = BPS $ b / s

applyTempo :: BPS -> Beats -> Seconds
applyTempo (BPS bps) (Beats b) = Seconds $ b / bps

unapplyTempo :: BPS -> Seconds -> Beats
unapplyTempo (BPS bps) (Seconds s) = Beats $ bps * s

decodeFile :: F.T -> Either [RTB.T Beats E.T] [RTB.T Seconds E.T]
decodeFile (F.Cons _typ dvn trks) = case dvn of
  F.Ticks res -> let
    readTime tks = Beats $ fromIntegral tks / fromIntegral res
    in Left $ map (RTB.mapTime readTime) trks
  F.SMPTE fps tksPerFrame -> let
    realFps = case fps of
      29 -> 29.97
      _  -> fromIntegral fps
    readTime tks = Seconds $
      fromIntegral tks / (realFps * fromIntegral tksPerFrame)
    in Right $ map (RTB.mapTime readTime) trks

encodeFileBeats :: F.Type -> Integer -> [RTB.T Beats E.T] -> F.T
encodeFileBeats typ res
  = F.Cons typ (F.Ticks $ fromIntegral res)
  . map (RTB.discretize . RTB.mapTime (* fromIntegral res))

-- | To correctly encode all the given tracks, the resolution must be a
-- multiple of the returned number.
minResolution :: [RTB.T Beats E.T] -> Integer
minResolution
  = foldr lcm 1
  . map (denominator . NN.toNumber . fromBeats)
  . concatMap RTB.getTimes

-- | Extracts the tempo from a tempo change event.
readTempo :: E.T -> Maybe BPS
readTempo (E.MetaEvent (Meta.SetTempo uspqn)) = let
  spqn = fromIntegral uspqn / 1000000
  qnps = recip spqn
  in Just $ BPS qnps
readTempo _ = Nothing

-- | Creates a MIDI event to set the tempo to the given value.
-- Rounds the tempo to the nearest whole \"microseconds per beat\" if necessary.
showTempo :: BPS -> E.T
showTempo (BPS qnps) = let
  spqn = recip qnps
  uspqn = spqn * 1000000
  in E.MetaEvent $ Meta.SetTempo $ round uspqn

-- | Given a MIDI event, if it is a time signature event, returns the length
-- of one measure set by the time signature.
readSignature :: E.T -> Maybe Beats
readSignature (E.MetaEvent (Meta.TimeSig n d _ _)) = Just $ let
  writtenFraction = fromIntegral n / (2 ^ d)
  sigLength = 4 * writtenFraction
  in Beats sigLength
readSignature _ = Nothing

-- | If the given number is @2 ^ n@ where @n@ is a non-negative integer,
-- returns @n@.
logBase2 :: Integer -> Maybe Integer
logBase2 x = go 0 1 where
  go !p !y = case compare x y of
    EQ -> Just p
    GT -> go (p + 1) (y * 2)
    LT -> Nothing

-- | Given a measure length, tries to encode it as a MIDI time signature.
showSignature :: Beats -> Maybe E.T
showSignature (Beats sigLength) = let
  writtenFraction = NN.toNumber $ sigLength / 4
  num = fromIntegral $ numerator writtenFraction
  in do
    denomPow <- logBase2 $ denominator writtenFraction
    Just $ E.MetaEvent $ case denomPow of
      0 -> Meta.TimeSig (num * 4) 2                       24 8
      1 -> Meta.TimeSig (num * 2) 2                       24 8
      _ -> Meta.TimeSig num       (fromIntegral denomPow) 24 8
      -- For prettiness we make the denominator at least /4,
      -- so for example 4/4 is not encoded as 1/1 even though it is simpler.

translationError :: (Show t) => String -> t -> a
translationError f t = error $
  "Sound.MIDI.Util." ++ f ++ ": internal error! couldn't translate position " ++ show t

-- | Converts between positions in musical time and real time.
newtype TempoMap = TempoMap (Map.Map (DoubleKey Beats Seconds) BPS)

makeTempoMap :: RTB.T Beats E.T -> TempoMap
makeTempoMap = TempoMap . Map.fromAscList . go 0 0 2 . RTB.mapMaybe readTempo where
  go :: Beats -> Seconds -> BPS -> RTB.T Beats BPS -> [(DoubleKey Beats Seconds, BPS)]
  go b s bps rtb = (DoubleKey b s, bps) : case RTB.viewL rtb of
    Nothing                 -> []
    Just ((db, bps'), rtb') -> go (b + db) (s + applyTempo bps db) bps' rtb'

applyTempoMap :: TempoMap -> Beats -> Seconds
applyTempoMap (TempoMap tm) bts = case Map.lookupLE (LookupA bts) tm of
  Just (DoubleKey b s, bps) -> s + applyTempo bps (bts - b)
  _ -> translationError "applyTempoMap" bts

unapplyTempoMap :: TempoMap -> Seconds -> Beats
unapplyTempoMap (TempoMap tm) secs = case Map.lookupLE (LookupB secs) tm of
  Just (DoubleKey b s, bps) -> b + unapplyTempo bps (secs - s)
  _ -> translationError "unapplyTempoMap" secs

applyTempoTrack :: TempoMap -> RTB.T Beats a -> RTB.T Seconds a
applyTempoTrack tm
  = RTB.fromAbsoluteEventList
  . ATB.mapTime (applyTempoMap tm)
  . RTB.toAbsoluteEventList 0

unapplyTempoTrack :: TempoMap -> RTB.T Seconds a -> RTB.T Beats a
unapplyTempoTrack tm
  = RTB.fromAbsoluteEventList
  . ATB.mapTime (unapplyTempoMap tm)
  . RTB.toAbsoluteEventList 0

-- | Converts between a simple beat position,
-- and a measure offset plus a beat position.
newtype MeasureMap = MeasureMap (Map.Map (DoubleKey Beats Int) Beats)

type MeasureBeats = (Int, Beats)

-- | What to do when 'makeMeasureMap' finds a misaligned time signature?
data MeasureMode
  = Error    -- ^ Throw an exception.
  | Ignore   -- ^ Ignore it.
  | Truncate -- ^ Truncate the previous measure.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

measures :: Int -> Beats -> Beats
measures m b = fromIntegral m * b

makeMeasureMap :: MeasureMode -> RTB.T Beats E.T -> MeasureMap
makeMeasureMap mm = MeasureMap . Map.fromAscList . go 0 0 4 . RTB.mapMaybe readSignature where
  go :: Beats -> Int -> Beats -> RTB.T Beats Beats -> [(DoubleKey Beats Int, Beats)]
  go b m tsig rtb = (DoubleKey b m, tsig) : case RTB.viewL rtb of
    Nothing                  -> []
    Just ((db, tsig'), rtb') -> case properFraction $ db / tsig of
      (dm, 0           ) -> go (b + db) (m + dm) tsig' rtb'
      (dm, leftoverMsrs) -> case mm of
        Error -> error $ unwords
          [ "makeMeasureMap: misaligned time signature found after"
          , show m
          , "measures and"
          , show $ fromBeats db
          , "beats"
          ]
        Ignore   -> go b m tsig $ RTB.delay db rtb'
        Truncate -> let
          leftoverBeats = leftoverMsrs * tsig
          truncated = (DoubleKey (b + measures dm tsig) (m + dm), leftoverBeats)
          in truncated : go (b + db) (m + dm + 1) tsig' rtb'

applyMeasureMap :: MeasureMap -> Beats -> MeasureBeats
applyMeasureMap (MeasureMap mm) bts = case Map.lookupLE (LookupA bts) mm of
  Just (DoubleKey b msr, tsig) -> let
    msrs = floor $ (bts - b) / tsig
    leftover = (bts - b) - fromIntegral msrs * tsig
    in (msr + msrs, leftover)
  _ -> translationError "applyMeasureMap" bts

unapplyMeasureMap :: MeasureMap -> MeasureBeats -> Beats
unapplyMeasureMap (MeasureMap mm) (msr, bts) = case Map.lookupLE (LookupB msr) mm of
  Just (DoubleKey b m, tsig) -> b + fromIntegral (msr - m) * tsig + bts
  _ -> translationError "unapplyMeasureMap" (msr, bts)

splitZero :: (NNC.C t) => RTB.T t a -> ([a], RTB.T t a)
splitZero rtb = case RTB.viewL rtb of
  Just ((dt, x), rtb') | dt == NNC.zero -> case splitZero rtb' of
    (xs, rtb'') -> (x : xs, rtb'')
  _ -> ([], rtb)

trackName :: (NNC.C t) => RTB.T t E.T -> Maybe String
trackName = listToMaybe . mapMaybe readTrackName . fst . splitZero

readTrackName :: E.T -> Maybe String
readTrackName (E.MetaEvent (Meta.TrackName s)) = Just s
readTrackName _                                = Nothing

showTrackName :: String -> E.T
showTrackName = E.MetaEvent . Meta.TrackName

-- | Equivalent to 'Control.Monad.join', but 'RTB.T' doesn't have a 'Monad'
-- instance (presumably because 'RTB.merge' has an 'Ord' constraint).
trackJoin :: (NNC.C t, Ord a) => RTB.T t (RTB.T t a) -> RTB.T t a
trackJoin rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> RTB.delay dt $ RTB.merge x $ trackJoin rtb'

trackSplit :: (NNC.C t) => t -> RTB.T t a -> (RTB.T t a, RTB.T t a)
trackSplit t rtb = case RTB.viewL rtb of
  Nothing -> (RTB.empty, RTB.empty)
  Just ((dt, x), rtb') -> case NNC.split t dt of
    (_, (True , d)) {- t <= dt -} -> (RTB.empty, RTB.cons d x rtb')
    (_, (False, d)) {- t >  dt -} -> case trackSplit d rtb' of
      (taken, dropped) -> (RTB.cons dt x taken, dropped)

-- | Drops all events at or after the given time from the event list.
trackTake :: (NNC.C t) => t -> RTB.T t a -> RTB.T t a
trackTake t rtb = fst $ trackSplit t rtb

-- | Drops the given amount of time from the start of the event list.
-- Events that are exactly at the time that is dropped will be kept in the list.
trackDrop :: (NNC.C t) => t -> RTB.T t a -> RTB.T t a
trackDrop t rtb = snd $ trackSplit t rtb
