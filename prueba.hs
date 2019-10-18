twice f = g
	where g x = f (f x)

doble :: Int -> Int
doble x = x + x

cuadruple x = 4 * x 	

id f = f

flip f = (\(x,y) -> f(y,x))

--appFork =  (\f -> \(x,y)-> (f x, f y)) doble  (1,2)

first (x,y) = x

fork x = (x, x)

swap (x,y) = (y,x)

uflip f = g
	where g p = f (swap p)

appFork f = g
	where g (x, y) = (f x, f y)

sumarPar (x, y) = x + y

f :: (a -> a) -> (a -> a) -> (a -> a)
f x y = x.y 
