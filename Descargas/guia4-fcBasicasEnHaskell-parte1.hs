import Distribution.Simple.Command (OptDescr(BoolOpt))
dobleme :: Int -> Int
dobleme x = x + x

sumar1 :: Int -> Int
sumar1 x = x + 1


{-Ejercicio1-}
f :: Int -> Int 
f n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16
    | otherwise = 0

fBis :: Int -> Int 
fBis 1 = 8
fBis 4 = 131
fBis 16 = 16

g :: Int -> Int 
g 8 = 16
g 16 = 4
g 131 =1
g _ = 0
 

fog :: Int -> Int 
fog n = f ( g n)

gof :: Int -> Int 
gof n = g ( f n)

{- Ejercicio2-}
{- devuelve el valor absoluto -}
absoluto :: Int -> Int
absoluto n  | n > 0 = n
            | otherwise = (-n)

{- maximo valor absoluto entre 2 enteros NO ANDA !! -}
maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto n m  | absoluto n > absoluto m = n
                    | otherwise = m

{- maximo3 : maximo entre 3 enteros -}
maximo3 :: Int -> Int -> Int -> Int
maximo3 n m l   | n >= m && n >= l = n
                | m >= n && m >= l = m  {- vale solo m >= l = m -}
                | otherwise = l

{- alguno es cero entre 2 racionales -}
algunoEs0 :: Int -> Int -> Bool
algunoEs0 n m   | n == 0 = True
                | m == 0 = True
                | otherwise = False

{- alguno es cero entre 2 racionales -}
algunoEs0_v2:: Int -> Int -> Bool
algunoEs0_v2 n m   | n == 0 || m == 0 = True
                | otherwise = False

{- alguno es cero entre 2 racionales -}
algunoEs0_v3 :: Int -> Int -> Bool
algunoEs0_v3 n m   = n == 0 || m == 0                

{- alguno es cero entre 2 racionales con pattern matching -}
algunoEs0Bis :: Float -> Float -> Bool
algunoEs0Bis _ 0 = True
algunoEs0Bis 0 _ = True
algunoEs0Bis _ _ = False

{- alguno es cero entre 2 racionales con pattern matching -}
algunoEs0Bis_v2 :: Float -> Float -> Bool
algunoEs0Bis_v2 x 0 = True
algunoEs0Bis_v2 0 x = True
algunoEs0Bis_v2 _ _ = False

{- Ambos son cero -}
ambosSonCero :: Float -> Float -> Bool
ambosSonCero x y    | x == 0 && y == 0 = True
                    | otherwise = False

ambosSonCero2 :: Float -> Float -> Bool
ambosSonCero2 0 0 = True
ambosSonCero2 _ _ = False


{- sumaDistintos dados 3 numeros suma sin sumar repetidos (los repetidos no los suma ni una vez)-}
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos n m l     | n /= m && n /= l && m /= l = n + m + l
                        | n == m && n /= l = l
                        | m == l && m /= n = n
                        | n == l && n /= m = m
                        | n == m && m == l = 0

{- digito de las unidades -}
digitoUnidades :: Int -> Int
digitoUnidades n    | n >= 0 = mod n 10
                    | otherwise = mod (-n) 10

{- digito de las decenas -}
digitoDecenas :: Int -> Int
digitoDecenas n     | n >= 0 = div (mod n 100) 10 
                    | otherwise = div (mod (-n) 100) 10 


{- digito de las decenas -}
digitoDecenas2 :: Int -> Int
digitoDecenas2 n     | n >= 0 = digitoUnidades (div n 10) 
                    | otherwise = digitoUnidades (div (-n) 10)  


{- Ejercicio 3-}
{- Implementar fc estanRelacionados que a partir de 2 enteros a, b distintos de 0, devuelve true sii
 existe algun k entero tal que a*a+a*b*k=0 
 dividiendo la ecuacion por a queda: a+b*k=0 y despejando k queda:
 k = (-a)/b Luego, si k es entero, a y b estan relacionados, de lo cotrario no
-}
estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b   | mod (-a) b == 0 = True
                        | otherwise  = False

estanRelacionados2 :: Int -> Int -> Bool
estanRelacionados2 a b  = mod (-a) b == 0


{- Ejercicio 4-}
{- producto interno o escalar -}
prodInt :: (Float, Float, Float) -> (Float, Float, Float) -> Float
prodInt (a,b,c) (x,y,z) = a*x + b*y + c*z


{- Dadas dos duplas determinar si la primera es menor a la segunda, componente a componente -}
{-    ESPECIFICACION-}
{- 
    problema todoMenor (x, y : <R x R >) : Bool {
        requiere : { True }
        asegura : { Res = True sii x0 < y0 && x1 < y1 }
    }
-}
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor x y = fst(x) < fst(y) && snd(x) < snd(y)

todoMenor2 :: (Float, Float) -> (Float, Float) -> Bool
todoMenor2 (a, b) (c, d) = a < c && b < d

{- calcula la distancia entre dos puntos de R x R-}
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (a, b) (x, y) = sqrt ((x-a)^2 + (y-b)^2)

{- dada terna de enteros calcula la suma de los 2 elem-}
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (a, b, c) = a+b+c

{- dada terna de enteros y un natural calcula la suma de los elem de la terna que son ultiplos del antural-}
{- Agrego funcion que dado un elemento de la terna y un natural 
devuelve el valor del elemento si es multiplo del natural o devuelve 0 en caso contrario-}
valorQueEsMultiploDeN :: Int -> Int -> Int 
valorQueEsMultiploDeN x n   | mod x n == 0 = x
                            | otherwise = 0
{- dada terna de enteros y un natural calcula la suma de los elem de la terna que son ultiplos del antural-}
sumaMultiplos :: (Int, Int, Int) -> Int -> Int
sumaMultiplos (i, j, k) n   = valorQueEsMultiploDeN i n + valorQueEsMultiploDeN j n + valorQueEsMultiploDeN k n

{- dada terna de enteros y un natural calcula la suma de los elem de la terna que son ultiplos del antural                        
sumaMultiplos :: (Int, Int, Int) -> Int -> Int
sumaMultiplos (i, j, k) n   | mod i n == 0 && mod j n == 0 && mod k n == 0 = i+j+k
                            | mod i n == 0 && mod j n == 0 && mod k n /= 0 =
-} 


{- dada terna de enteros devuelve la posicion del primer numero par si es que hay alguno y devuelve 4 si son todos impares -}
posicionPrimerNroParEnTerna :: (Int, Int, Int) -> Int
posicionPrimerNroParEnTerna (i, j, k)  | mod i 2 == 0 = 0
posicionPrimerNroParEnTerna (i, j, k)  | mod j 2 == 0 = 1
posicionPrimerNroParEnTerna (i, j, k)  | mod k 2 == 0 = 2
                                    | otherwise = 4

{- crea par a partir de 2 componentes dadas por separado 
  debe funcionar para elementos de cualquier tipo -}
creaPar :: t1 -> t2 -> (t1, t2)
creaPar a b = (a, b)

{- invierte los elem del par pasado como parametro 
  debe funcionar para elementos de cualquier tipo -}
inviertePar :: (t1, t2) -> (t2, t1)
inviertePar ( a, b) = (b, a)

{- EJERCICIO 5 -}
{- Implementar la funcion todos menores que toma una terna de enteros y devuelve Bool-}
{- ESPEIFICACION -}
{- 
    problema todosMenores ((n1, n2, n3) : Z x Z x Z) : Bool {
        requiere: {True}
        asegura:  { res == True sii ( (f(n1) > g(n1)) && (f(n2) > g(n2)) && (f(n3) > g(n3)) ) }
    }
    problema f (n : Z) : Z {
        requiere: {True}
        asegura:  { ( n <= 7 -> res = n * n ) O SI ( n > 7 -> res = 2*n - 1 )}
    }
    problema g (n : Z) : Z {
        requiere: {True}
        asegura:  { Si n es par, entonces res=n/2, en caso contrario res=3*n+1 }
    }
 -}

todosMenores :: (Int, Int, Int) -> Bool
todosMenores (i, j, k)     | (f2 i > g2 i) && (f2 j > g2 j) && (f2 k > g2 k) = True
                           | otherwise = False

f2 :: Int -> Int
f2 n     | n <= 7 = n*n
        | otherwise = 2*n-1

g2 :: Int -> Int
g2 n     | mod n 2 == 0 = div n 2
        | otherwise = 3*n+1

{- EJERCICIO 6 -}
{- Implementar la funcion bisiesto que dado un entero devuelve Bool-}
{- ESPEIFICACION -}
{- 
    problema bisiesto ( aaaa : Z ) : Bool {
        requiere: {True}
        asegura:  { res == Fasle sii aaaa no es multiplo de 4  o aaaa es multiplo d 100 y no de 400 }
    }
 -}
bisiesto :: Int -> Bool
bisiesto aaaa   | (mod aaaa 4 /= 0) || ( (mod aaaa 100 == 0) &&  (mod aaaa 400 /= 0 ) ) = False
                | otherwise = True
                
