module Day16 (solve) where

-- packet structure:
--  - packet (1x)
--    - header
--      - version (3 bits)
--      - type ID (3 bits)
--    - payload (depends on type id)
--      - operator (optional, 1 bit)
--      - length (optional, 11/15 bits)
--      - sub-packet (1...)
--  - padding
--
-- Type ID | Payload
--      4  | Literal
--      *  | Operator
--
-- Operators & length
--   operator = 0 -> next 15 bits represent the total length (in bits) of the remaining paylad.
--   operator = 1 -> next 11 bits represent the number of sub-packets in this packet
--
-- Literal
--   Sub-divided into chunks of 5 bits. First bit inducates last chunck (0) or data remaining (1).
--   Other 4 bits are part of the value.
--
-- Example:
-- input: 38006F45291200
--
--  3  | 8  | 0  | 0  | 6  | F  | 4  | 5  | 2  | 9  | 1  | 2  | 0  | 0  |
-- 0011|1000|0000|0000|0110|1111|0100|0101|0010|1001|0001|0010|0000|0000
-- VVVT|TTIL|LLLL|LLLL|LLLL|LLAA|AAAA|AAAA|ABBB|BBBB|BBBB|BBBB|B
--
-- A: 11010001010
--    VVVTTTAAAAA -> V: 6, T: 4 (literal), value: 10 (b1010)
--
-- B: 0101001000100100
--    VVVTTTAAAAAAAAAA -> V: 2, T: 4 (literal), value: 20 (b0001 0100)
--


import Data.List
import Data.Maybe

data LengthType = Bits Int
                  | Packets Int
                  deriving (Show)

data Packet = Literal { pVersion :: Int,
                        pValue :: Int }
              | Operator { pVersion :: Int,
                           pOperation :: Int,
                           pSubs :: [Packet] }
              deriving (Show)

-- https://stackoverflow.com/a/14936754/5974196
hexChar :: Char -> Int
hexChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $
    elemIndex ch "0123456789ABCDEF"

int2nibble :: Int -> [Int]
int2nibble i = go i 3
  where go n 0 = if n > 0 then [1] else [0]
        go n c = let
                  value = if n >= 2^c then 1 else 0
                  remainder = if value > 0 then n - 2^c else n
                 in value : go remainder (c - 1)

bin2dec :: [Int] -> Int
bin2dec = foldl (\acc -> (+) (2 * acc)) 0

parseVersion :: [Int] -> (Int, [Int])
parseVersion xs = (bin2dec v, r)
  where v = take 3 xs
        r = drop 3 xs

parseType :: [Int] -> (Int, [Int])
parseType xs = (bin2dec t, r)
  where t = take 3 xs
        r = drop 3 xs

parseLiteral :: [Int] -> (Int, [Int])
parseLiteral xs = go xs []
  where go ys zs
          | end = (bin2dec value, stream)
          | otherwise = go stream value
          where end = head ys == 0
                value = zs ++ take 4 (tail ys)
                stream = drop 5 ys

parseOperator :: [Int] -> (LengthType, [Int])
parseOperator xs = (op, drop bits stream)
  where bits = if head xs == 0 then 15 else 11
        stream = tail xs
        value = bin2dec $ take bits stream
        op = if bits == 15 then Bits value else Packets value

parsePacket :: [Int] -> (Packet, [Int])
parsePacket xs = case t of
                  4 -> (Literal v l, x3)
                  _ -> (Operator v t subs, x5)
  where (v, x1) = parseVersion xs
        (t, x2) = parseType x1
        (l, x3) = parseLiteral x2
        (n, x4) = parseOperator x2
        (subs, x5) = parseSubPackets n x4

parseSubPackets :: LengthType -> [Int] -> ([Packet], [Int])
parseSubPackets (Bits bits) xs = (go (take bits xs) [], drop bits xs)
  where go :: [Int] -> [Packet] -> [Packet]
        go [] output = output
        go input output = go remainder (output ++ [p])
          where (p, remainder) = parsePacket input
parseSubPackets (Packets n) xs = go n xs []
  where go :: Int -> [Int] -> [Packet] -> ([Packet], [Int])
        go 0 input output = (output, input)
        go i input output = go (i - 1) remainder (output ++ [p])
          where (p, remainder) = parsePacket input

parse :: String -> Packet
parse xs = fst (parsePacket stream)
  where stream = concatMap (int2nibble . hexChar) xs

sumVersion :: Packet -> Int
sumVersion (Literal v _) = v
sumVersion (Operator v _ s) = v + sum (0:map sumVersion s)

part1 :: Packet -> Int
part1 = sumVersion

part2 :: Packet -> Int
part2 (Literal _ v) = v
part2 (Operator _ opcode children) = case opcode of
  0 -> sum (0:map part2 children)
  1 -> product (map part2 children)
  2 -> minimum (map part2 children)
  3 -> maximum (map part2 children)
  5 -> let [a,b] = map part2 children in if a > b then 1 else 0
  6 -> let [a,b] = map part2 children in if a < b then 1 else 0
  7 -> let [a,b] = map part2 children in if a == b then 1 else 0
  _ -> error "Invalid operation"

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = parse input
        s1 = show $ part1 entries -- 895
        s2 = show $ part2 entries -- 1148595959144
