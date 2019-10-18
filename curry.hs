myid :: a -> a
myid x = x

first :: (a, b) -> a
first (x, y) = x

second :: (a, b) -> b
second (x, y) = y

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


sumaPar :: (Int, Int) -> Int
sumaPar (x, y) = x + y


sumar :: Int -> Int -> Int
sumar x y = x + y

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
		where h y = f y x 

appDup f = g
	where g x = f (x, x)

isH :: Char -> Int
isH 'h' = 1
isH 'H' = 1
isH _ = 0

curry1 :: ((a,b) -> c) -> a -> b -> c
curry1 = \f -> \x -> \y -> f (x,y)

curry2 :: ((a,b) -> c) -> a -> b -> c
curry2 f x y = f (x,y)

curry3 :: ((a,b) -> c) -> a -> b -> c
curry3 f = g
	where g x = h
		where h y = f (x,y)

uncurry1 :: (a -> b -> c) -> (a,b) -> c
uncurry1 = \f -> \(x,y) -> f x y 

uncurry2 :: (a -> b -> c) -> (a,b) -> c
uncurry2 f (x,y) = f x y 

uncurry3 :: (a -> b -> c) -> (a,b) -> c
uncurry3 f = g
	where g (x,y) = f x y

compose = \f -> \g -> \x -> f(g x)
compose' = \f -> \ g -> f.g

twice2 f = f.f

many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x)


many' :: Int -> (a -> a) ->a -> a
many' 0 f = id
many' n f = compose f (many' (n-1) f) 

many2 = \n -> \f -> \x -> compose f (many' (n-1) f) x

many3 :: Int -> (a -> a) ->a -> a
many3 0 f x = x
many3 n f x = many3 (n-1) f (f x) 

timesTwoPlusThree x = sumar (doble x) 3
timesTwoPlusThree' = compose (+3) doble

fourTimes f x = f (f (f (f x)))
fourTimes' f = many 4 f 
fourTimes'' f  = twice twice f

cuadruple :: Int -> Int
cuadruple x = 4 * x
cuadruple' = twice doble
cuadruple'' = compose doble doble


inf = inf + 1
bottom = bottom
