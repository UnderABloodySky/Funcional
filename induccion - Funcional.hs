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

const :: a -> b -> a
const x y = x

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

data Gusto = Chocolate | Sambayon | Frutilla | DulceDeLeche deriving Show

data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto deriving Show

data Shape = Circle Float | Rect Float Float deriving Show

agregarChocolate consH = consH Chocolate

construirSNormal c = c 1.0
		


inf = inf + 1
bottom = bottom

assoc :: (a, (b,c)) -> ((a,b), c)
assoc (x, (y,z)) = ((x,y), z)

appAssoc f p = f (assoc p)

myTwice f = f.f

myCuadruple1 = (4*).id
myCuadruple2 = doble.doble

myDoble1 = (2*).id
myDoble2 = (\x->x+x).id
myDoble3 = sumaPar.dup

dup :: a -> (a, a)
dup x = (x,x)

myMany 0 f = id
myMany n f = myMany (n-1) f . f


data Ingrediente = Salsa | Queso | Aceitunas Int | Anchoas | Cebolla | Jamon deriving Show

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ingrediente pizza) = 1 + cantidadDeCapas pizza

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa ingrediente pizza) = cantSiEsAceituna ingrediente + cantidadDeAceitunas pizza

cantSiEsAceituna :: Ingrediente -> Int
cantSiEsAceituna (Aceitunas n) = n
cantSiEsAceituna _ = 0

dubplicarAceitunas :: Pizza -> Pizza 
dubplicarAceitunas Prepizza = Prepizza
dubplicarAceitunas (Capa ingrediente pizza) = Capa (dupSiEsAceituna ingrediente) (dubplicarAceitunas pizza)

dupSiEsAceituna :: Ingrediente -> Ingrediente
dupSiEsAceituna (Aceitunas n) = Aceitunas (n*2)
dupSiEsAceituna ingrediente = ingrediente

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa ingrediente pizza) = if esQueso ingrediente
												then sinLactosa pizza
												else Capa ingrediente (sinLactosa pizza)


esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

aptaIntoleranteLactosa :: Pizza -> Bool
aptaIntoleranteLactosa Prepizza = True
aptaIntoleranteLactosa (Capa ingrediente pizza) = if esQueso ingrediente
														then False
														else aptaIntoleranteLactosa pizza

agregaAceitunasCorrectamente :: Pizza -> Bool
agregaAceitunasCorrectamente Prepizza = True
agregaAceitunasCorrectamente (Capa ingrediente pizza) = if esIngredienteInvalido ingrediente
															then False
															else agregaAceitunasCorrectamente pizza

esIngredienteInvalido :: Ingrediente -> Bool
esIngredienteInvalido (Aceitunas n) = n < 1
esIngredienteInvalido _ = False


sonIngredientesIguales :: Ingrediente -> Ingrediente -> Bool
sonIngredientesIguales (Aceitunas _) (Aceitunas _) = True
sonIngredientesIguales Cebolla Cebolla = True
sonIngredientesIguales Salsa Salsa = True
sonIngredientesIguales Queso Queso = True
sonIngredientesIguales Anchoas Anchoas = True
sonIngredientesIguales Jamon Jamon = True
sonIngredientesIguales _ _ = False

tiene :: Ingrediente -> Pizza -> Bool
tiene ingrediente Prepizza = False
tiene ingrediente0 (Capa ingrediente1 pizza) = if sonIngredientesIguales ingrediente0 ingrediente1 
														then True
														else tiene ingrediente0 pizza

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa (Aceitunas n) (Capa (Aceitunas m) pizza)) = conDescripcionMejorada (Capa (Aceitunas (n+m)) pizza)
conDescripcionMejorada (Capa ingrediente pizza) = Capa ingrediente (conDescripcionMejorada pizza)


conDescripcionMejorada' :: Pizza -> Pizza
conDescripcionMejorada' pizza = normalizarAceitunas (Capa (Aceitunas (cantidadDeAceitunas pizza)) (sinAceitunas pizza))

normalizarAceitunas :: Pizza -> Pizza
normalizarAceitunas (Capa (Aceitunas 0) pizza) = pizza
normalizarAceitunas pizza = pizza

sinAceitunas :: Pizza -> Pizza
sinAceitunas Prepizza = Prepizza
sinAceitunas (Capa ingrediente pizza) = if esAceituna ingrediente
												then sinAceitunas pizza
												else Capa ingrediente (sinAceitunas pizza)

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _ = False

type Nombre = String
data Planilla = Fin | Registro Nombre Planilla deriving Show
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo deriving Show

largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin = 0
largoDePlanilla (Registro _ planilla) = 1 + largoDePlanilla planilla

esta :: Nombre -> Planilla -> Bool
esta _ Fin = False
esta name0 (Registro name1 planilla) = if esMismoNombre name0 name1
												then True
												else esta name0 planilla

esMismoNombre :: Nombre -> Nombre -> Bool
esMismoNombre name0 name1 = name0 == name1

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas otraPlanilla Fin = otraPlanilla
juntarPlanillas planilla (Registro nombre1 planilla1) = juntarPlanillas (agregarNombreAlFinal nombre1 planilla) planilla1 

agregarNombreAlFinal :: Nombre -> Planilla -> Planilla
agregarNombreAlFinal name Fin = Registro name Fin
agregarNombreAlFinal name0 (Registro name1 planilla) = Registro name1 (agregarNombreAlFinal name0 planilla)

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario _) = 0
nivelesJerarquicos (Investigador _ e0 e1 e2) = 1 + nivelesJerarquicos e0 +  nivelesJerarquicos e1 + nivelesJerarquicos e2  

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario _) = 1
cantidadDeIntegrantes (Investigador _ e0 e1 e2) = 1 + cantidadDeIntegrantes e0 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2

planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario name) = Registro name Fin
planillaDeIntegrantes (Investigador name e0 e1 e2) = Registro name (juntarPlanillas (juntarPlanillas (planillaDeIntegrantes e0) (planillaDeIntegrantes e1)) (planillaDeIntegrantes e2))     

pizza = Capa Queso 
			(Capa (Aceitunas 4) 
				(Capa (Aceitunas 8) 
					(Capa Queso 
						(Capa (Aceitunas 4) 
							(Capa Jamon 
								(Capa (Aceitunas 4) 
									(Capa (Aceitunas 4) 
										(Capa Cebolla Prepizza))))))))
equipo = Investigador "Oswald"
	      (Investigador "Alonzo" (Becario "Alan")
			       (Becario "Alfred")
			       (Becario "Stephen"))
	      (Investigador "John"   (Becario "Brian")
			       (Becario "Graham")
			       (Becario "Ioan"))
	      (Investigador "Robert" (Becario "Gordon")
			       (Becario "John")
			       (Becario "Raymond"))

data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a) deriving Show
data Tesoro = Cofre | Oro | Joyas deriving Show
data VariasCosas a b = Objeto a | Criatura b deriving Show
data Monstruo = Gargola | Dragon | Troll deriving Show

cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion _) = 0
cantidadDeBifurcaciones (Pasaje _ dungeon) = cantidadDeBifurcaciones dungeon
cantidadDeBifurcaciones (Bifurcacion _ dungeon0 dungeon1) = 1 + cantidadDeBifurcaciones dungeon0 + cantidadDeBifurcaciones dungeon1

cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion _) = 0
cantidadDePuntosInteresantes (Pasaje _ dungeon) = 1 + cantidadDeBifurcaciones dungeon
cantidadDePuntosInteresantes (Bifurcacion _ dungeon0 dungeon1) = 1 + cantidadDePuntosInteresantes dungeon0 + cantidadDePuntosInteresantes dungeon1

cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion _) = 0
cantidadDePuntosVacios (Pasaje maybe dungeon) =  isNothing maybe + cantidadDeBifurcaciones dungeon
cantidadDePuntosVacios (Bifurcacion maybe dungeon0 dungeon1) =  isNothing maybe + cantidadDePuntosVacios dungeon0 + cantidadDePuntosVacios dungeon1

isNothing :: Maybe a -> Int
isNothing Nothing = 1
isNothing _ = 0


esLineal :: Dungeon a -> Bool
esLineal (Habitacion _) = True
esLineal (Pasaje _ dungeon) =  True && esLineal dungeon
esLineal (Bifurcacion _ _ _) =  False

llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe x (Habitacion y) = x == y 
llenoDe x (Pasaje maybe dungeon) =  Just x == maybe && llenoDe x dungeon
llenoDe x (Bifurcacion maybe dungeon0 dungeon1) =  Just x == maybe && llenoDe x dungeon0 && llenoDe x dungeon1 


