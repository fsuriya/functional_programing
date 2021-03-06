len [] = 0
len (x:xs) = len xs + 1

join :: ([a],[a]) -> [a]
join ([],ys) = ys
join (x:xs,ys) = x : join(xs,ys)

join' :: [a] -> [a] -> [a]
join' [] ys = ys
join' (x:xs) ys = x : join' xs ys

partialJoin = join' [1,2,3]

--------------------lec 03----------------------------
fac 0 = 1
fac n = if n > 0  
        then n * fac (n - 1)
        else error "bad user"

fac' :: Integral p => p -> p
fac' n
  | n == 0  = 1
  | n > 0   = n * fac (n - 1)
  | otherwise = error "negative number"

rev :: [t] -> [t]
rev [] = []
rev (x:xs) = rev xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []                  -- "_" is wildcard use it to ignore unused parameters
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  

-------------------lec 04----------------------------------
zipper' xs ys = rev $ zipper_aux xs ys []
  where zipper_aux [] _ res = res
        zipper_aux _ [] res = res
        zipper_aux (x:xs) (y:ys) res = zipper_aux xs ys ((x,y):res)

        rev l = rev_aux l []
            where rev_aux [] res = res
                  rev_aux (x:xs) res = rev_aux xs (x:res)
------------------------------------------------------------
carteian [] _ = []
carteian (x:xs) ys = pair  ys ++ carteian xs ys
  where pair [] = []
        pair (y:ys) = (x,y) : pair ys

-- filter (\(x,y) -> even $ x+y) (carteian [1,2,3] [4,5,6])
sum l = foldl (+) 0 l

------------------------------------------------------------
--reverse' l = foldr (\x acc -> acc ++ [x]) [] l
reverse' :: [a] -> [a]
reverse' l = foldr (\x acc -> acc ++ [x]) [] l
---------------------lec07----------------------------------
data Month = January | February
    | March | April
    | May | June
    | July | August
    | September | October
    | November | December
  deriving (Show, Eq, Enum)

nextMonth m = case m of
  January -> February
  February -> March
  March -> April
  April -> May
  May -> June
  June -> July
  July -> August
  August -> September
  September -> October
  October -> November
  November -> December
  December -> January

months = [January .. December]
--------------------------------------------------
data Gender = Male | Female

instance Eq Gender where
  Male == Male =  True
  Female == Female = True
  _ == _ = False

data D = C1 Int | C2 Bool | C3 (Int,Bool) deriving (Eq, Ord)

data' x = 3
--------------------------------------------------
-- instance Functor ((->) r) where
--   -- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
--   fmap :: (a -> b) -> (r -> a) -> (r -> b)
--   -- fmap f g = \r -> f (g r)
--   -- famp f g = \r (f.g) r
--   -- fmap f g = f . g
--   -- fmap f g = (.) f g
--   fmap = (.)

-- instance Functor (Either a) where
--   fmap :: (r -> b) -> Either a r -> Either a b
--   fmap f (Left x) = Left x
--   fmap f (Right) = Right (f x)

-- ffmap :: f (a -> b) -> f a -> f b
-- ffmap Nothing _ = Nothing
-- ffmap _ Nothing = Nothing
-- ffmap (Just f) (Just v) = Just (f v)

-- ffmap [] _ = []
-- ffmap _ [] = []
-- ffmap fs vs = [f v | f <- fs , v <- vs]

-------------------------------------------------
--(+) <*> (*5) $ 3

applylist [] _ = []
applylist _ [] = []
applylist (f:fs) (x:xs) = f x : applylist fs xs

newtype ziplist a = 
        ziplist { getziplist :: [a] }

instance Functor ziplist where
  fmap f = ziplist . fmap f . getziplist

instance Applicative ziplist where
  pure x = ziplist (repeat  x)
  ziplist fs <*> ziplist xs = ziplist $ zipWith (\f x -> f x) fs xs