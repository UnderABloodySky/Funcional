data ListaNoVacia a = Cons a (ListaNoVacia a) | Unit a deriving (Show, Eq)


-- Retorna la cantidad de elementos de la ListaNoVacia
length' :: ListaNoVacia a -> Int
length' (Unit _) = 1
length' (Cons _ lNv) = 1 + length' lNv


-- Dado un elemento y una ListaNoVacia, agrega un elemento x al principio lista lNv. 
cons' :: a -> ListaNoVacia a ->  ListaNoVacia a
cons'  x lNv = Cons x lNv


-- Dado un elemento y una ListaNoVacia, agrega un elemento x al final de la lista.
append' :: a -> ListaNoVacia a ->  ListaNoVacia a
append' x (Unit y) = Cons y (Unit x)
append' x (Cons y lNv) = Cons y (append' x lNv)


-- Dada una una ListaNoVacia retorna su reversa. 
reversePT :: ListaNoVacia a -> ListaNoVacia a
reversePT lNv = listToListaNoVacia(reverse (listaNoVaciaToList lNv))


-- Dada una ListaNoVacia retorna su primer elemento
head' :: ListaNoVacia a -> a
head' (Unit x) = x
head' (Cons x _) = x


-- Dada una ListaNoVacia retorna su ultimo elemento
last' :: ListaNoVacia a -> a
last' (Unit x) = x
last' (Cons x lNv) = last' lNv


-- Dada una ListaNoVacia retorna la misma sin su primer elemento
tail' :: ListaNoVacia a -> ListaNoVacia a
tail' (Unit _) = error "La lista no puede ser vacia"
tail' (Cons x lNv) = lNv


-- Dada una ListaNoVacia retorna la misma sin su ultimo elemento
init' :: ListaNoVacia a -> ListaNoVacia a 
init' (Unit x) = error"La lista no puede ser vacia" 
init' (Cons x (Unit _)) = Unit x
init' (Cons x lNv) = Cons x (init' lNv)

 
-- Dada una una ListaNoVacia retorna su reversa. 
reverse' :: ListaNoVacia a ->  ListaNoVacia a
reverse' (Unit x) = Unit x
reverse' (Cons x lNv) =  append' x (reverse' lNv)


-- Dada una listaNoVacia, retorna una lista con los elementos de la misma. 
listaNoVaciaToList :: ListaNoVacia a -> [a]
listaNoVaciaToList (Unit x) = [x]
listaNoVaciaToList (Cons x lNv) = x : listaNoVaciaToList lNv


-- Dada una lista, retorna una ListaNoVacia con los elementos de la misma.
listToListaNoVacia :: [a] -> ListaNoVacia a
listToListaNoVacia [] = error "La lista no puede ser vacia"
listToListaNoVacia [x] = Unit x 
listToListaNoVacia (x:xs) = Cons x (listToListaNoVacia xs) 


-- Dada dos ListasNoVacia, retorna la ListaNoVacia resultante de entrelasar los elementos  de la primera con la segunda
zip' :: ListaNoVacia a -> ListaNoVacia a -> ListaNoVacia a
zip' (Unit x) (Unit y) = Cons x (Unit y) 
zip' lNv (Unit y) = append' y lNv
zip' (Unit x) lNv = Cons x lNv
zip' (Cons x lNv0) (Cons y lNv1) =  append' x (cons' y (zip' lNv0 lNv1)) 


-- Dada dos ListasNoVacia agrega a la segunda todos los elementos de la primera	
add :: ListaNoVacia a -> ListaNoVacia a -> ListaNoVacia a
add (Unit y) lNv = Cons y lNv 
add lNv (Unit y) = append' y lNv
add lNv0 (Cons y lNv1) = add (append' y  lNv0) lNv1 


-- Dada un numero y una ListaNoVacia denota el elemento en dicha posicion
get :: Int -> ListaNoVacia a -> a
get 0 lNv = head' lNv
get n (Unit _) = error "La lista tiene menos de esa cantidad de elementos"
get n (Cons _ lNv) = get (n-1) lNv


l = Cons 0 (Cons 1(Cons 2(Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 (Cons 11 (Cons 12 (Cons 13 (Cons 14 (Cons 15 (Unit 16))))))))))))))))



