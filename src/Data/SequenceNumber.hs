-- | Manipulation of sequence numbers, such as those often found in network protocols
module Data.SequenceNumber
  ( SequenceNumberField (..)
  , SequenceNumber
  , SeqNo8
  , SeqNo16
  , SeqNo32
  , SeqNo64
  )
where

import           Data.Int
import           Data.Word

-- | Sequence numbers are a modulo number field in which the distance between two numbers
--   can be both positive and negative. Sequence numbers are intended to represent an
--   unbounded sequence of numbers (typically used as identifiers, not for their other
--   numeric properties) in a finite number of bits.
--
--   The expectation is that total number of active identifiers is bounded, and that in
--   comparing two sequence numbers there is concept of "before" and "after" which is
--   unambigious, provided the total number of active identifiers is within that bound.
--
--   @fromEnum sn@ will generate all the possible sequence number (unbounded and wrapped)
--  , starting with @sn@.

class SequenceNumberField n m | n -> m where
  -- | The maximum absolute distance, typically half the repeat interval - 1.
  maxDistance :: n
  -- | Calculate the signed distance, use of the matching Int,
  --   i.e. @SequenceNumberField Word8 Int8@ does the right thing
  toSignedD   :: n -> m
  distance    :: n -> n -> n

--   Sequence numbers have a clear interpretation of `Eq`, for `Num` the arithmetic is
--   modulo arithmetic on the underlying field (i.e. negation is equivalent to adding half
--   the field size).
--
--   Even with the largest possible `maxDistance` for a given underlying representation
--   there are issues with decidability.
--   See <http://en.wikipedia.org/wiki/Serial_number_arithmetic>.
--
--   This leads to an implementation choice. The fact that there are certain values for which
--   ordering is undecidable, means that `compare` of `Eq` should not be a total function.
--   However, implementing `compare` such that it raised an error under such circumstances
--   is likely to cause confusion. To make the function total, a bias has to be chosen.
--
--   The bias implemented here is to make the `GT` outcome of `compare` the negation
--   of the `LT` -- thus making `LT` accurate - with the junk mapped to the `GT` result.
--
--   However, the implmentations of `>=` and `>` to include the `maxDistance` calculation,
--   thus they don't include any junk.
--
--   This means that, although `compare` is total, it will not be equivalent to the union
--   of the results from `<=` and '>'.

newtype SequenceNumber n m = SN {unSN :: n}
  deriving (Eq, Num, Bounded, Integral, Real)

instance (Show n) => Show (SequenceNumber n m) where
  show = show . unSN

instance (Bounded n, Num n, Integral n, Num m) =>
    SequenceNumberField (SequenceNumber n m) m where
     maxDistance  = SN $ (maxBound - minBound) `div` 2
     toSignedD    = fromIntegral . unSN
     distance a b = SN $ (unSN b) - (unSN a)

{- -- specfic instance, useful for checking above
instance SequenceNumberField SeqNo8 Int8 where
  maxDistance  = SN $ (maxBound - minBound) `div` 2
  toSignedD    = fromIntegral . unSN
  distance a b = SN $ unSN a - unSN b
-}

instance (Eq n, Ord m, Bounded n, Integral n, Num m) =>
            Ord (SequenceNumber n m) where
 a <= b
    = (d' <= 0) && (abs d' <= md)
    where
      md = toSignedD $ maxDistance `asTypeOf` a
      d' = toSignedD $ b `distance` a
 a  > b
    = (d'  > 0) && (abs d' <= md)
    where
      md = toSignedD $ maxDistance `asTypeOf` a
      d' = toSignedD $ b `distance` a
 a >= b
    = (d' >= 0) && (abs d' <= md)
    where
      md = toSignedD $ maxDistance `asTypeOf` a
      d' = toSignedD $ b `distance` a

instance  (Integral n) => Enum (SequenceNumber n m) where
  fromEnum    = fromIntegral . unSN
  toEnum      = SN . fromIntegral
  succ (SN n) = SN $ n + 1
  pred (SN n) = SN $ n - 1

type SeqNo8  = SequenceNumber Word8  Int8
type SeqNo16 = SequenceNumber Word16 Int16
type SeqNo32 = SequenceNumber Word32 Int32
type SeqNo64 = SequenceNumber Word64 Int64

