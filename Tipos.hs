id :: a -> a
id x = x

first :: (a, b) -> a
first (x, y) = x

swap :: (a, b) -> (b,a)
swap (x, y) = (y,x)

apply :: (a -> b) -> a -> b
apply f = g
	where g x = f x

--uflip :: ((a, b) -> c) -> (b,a) -> c
uflip f = g
	where g x = f (swap x)

--twice :: (a -> a) -> a -> a
twice f = g
	where g x = f (f x)

doble :: Int -> Int
doble x = x + x 

cuadruple :: Int -> Int
cuadruple x = 4 * x

sumaPar :: (Int, Int) -> Int
sumaPar (x, y) = x + y

maxDelPar :: (Int, Int) -> Int
maxDelPar (x,y) = max x y

--appPar :: (a -> b) -> (c,d) -> (b, b) 
appPar f (x , y) = (f x, f y)

aInt :: (Int -> Int) -> Int
aInt f = f 2

appFork (f , g) = h
	where h x = (f x, g x) 

flip f = g
	where g x = h
		where h y = (f y) x 

appDup f = g
	where g x = f (x, x)

isH :: Char -> Int
isH 'h' = 1
isH 'H' = 1
isH _ = 0
