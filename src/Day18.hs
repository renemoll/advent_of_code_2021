module Day18 (solve) where

data Tree a = Node (Tree a) (Tree a)
            | Value a
            deriving (Show, Eq)

-- Add int to left/right of a Node and return the Node
data CarrySide = CarryLeft | CarryRight
add :: Int -> CarrySide -> Tree Int -> Tree Int
add x CarryLeft  (Node a b) = Node (add x CarryLeft a) b
add x CarryRight (Node a b) = Node a (add x CarryRight b)
add x _ (Value y) = Value (x + y)


-- look into the current and the children
--  the children can be the bottom (explode)
--    both values go up -> left is added to the LHS and right to the RHS
--    can i do something with (carry, L, R)? -> Carry is of type Tree!
--  the children can be another pair or a plain Value

-- explode only returns a tree is something changed
explode :: Tree Int -> Maybe (Tree Int)
explode x = maybe Nothing (\(y,_,_) -> Just y) $ go 0 x
  where -- go returns a tree and two values (L,R)
        go :: Int -> Tree Int -> Maybe (Tree Int, Int, Int)
        go _ (Value _) = Nothing
        go 4 (Node (Value a) (Value b)) = Just (Value 0, a, b)
        go d (Node a b) = let
                            left = go (d + 1) a -- left = Just (Tree, Int, Int) or Nothing
                            right = go (d + 1) b -- right = Just (Tree, Int, Int) or Nothing
                            carryLeft :: (Tree Int, Int, Int) -> Maybe (Tree Int, Int, Int)
                            carryLeft  (node, carryL, carryR) = Just (Node node (add carryR CarryLeft b), carryL, 0)
                            carryRight :: (Tree Int, Int, Int) -> Maybe (Tree Int, Int, Int)
                            carryRight (node, carryL, carryR) = Just (Node (add carryL CarryRight a) node, 0, carryR)
                          in case maybe Nothing carryLeft left of
                            Just y -> Just y
                            Nothing -> maybe Nothing carryRight right

split :: Tree Int -> Maybe (Tree Int)
split = go
  where go (Value a)
          | a >= 10 = Just (Node (Value (a `div` 2)) (Value ((a + 1) `div` 2)))
          |  otherwise = Nothing
        go (Node a b) = let
                          left = split a -- left = Just Tree Int or Nothing
                          right = split b -- right = Just Tree Int or Nothing
                          splitLeft :: Tree Int -> Maybe (Tree Int)
                          splitLeft x = Just (Node x b)
                          splitRight :: Tree Int -> Maybe (Tree Int)
                          splitRight x = Just (Node a x)
                        in case maybe Nothing splitLeft left of
                            Just x -> Just x
                            Nothing -> maybe Nothing splitRight right

reduce :: Tree Int -> Tree Int
reduce x = maybe (doSplit x) reduce (explode x)
  where doSplit y = maybe x reduce (split y)

parse :: String -> [Tree Int]
parse = map (fst . go) . lines
  where go ('[':xs) = let
                        (lhs, ys) = go xs
                        (rhs, zs) = go ys
                      in (Node lhs rhs, zs)
        go (']':xs) = go xs
        go (x:',':xs) = (Value (read [x] :: Int), xs)
        go (x:']':xs) = (Value (read [x] :: Int), xs)
        go (',':xs) = go xs
        go x = (Value (read x :: Int), "")

magnitude :: Tree Int -> Int
magnitude (Node a b) = 3 * magnitude a + 2 * magnitude b
magnitude (Value a) = a

part1 :: [Tree Int] -> Int
part1 x = magnitude t
  where t = foldl (\a b -> reduce (Node a b)) (head x) (tail x)

part2 :: [Tree Int] -> Int
part2 x = maximum [magnitude $ reduce (Node a b) | a <- x, b <- x, a /= b]

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = parse input
        s1 = show $ part1 entries -- 4132
        s2 = show $ part2 entries -- 4685
