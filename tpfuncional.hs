import List
import Test.HUnit -- para correr tests

--Definición de Grafo--
data Grafo a = G {nodos :: [a], adyacencias :: a->[a]}


--Funciones adicionales (pueden agregar otras)--

conjuntosIguales::Eq a =>[a]->[a]->Bool
conjuntosIguales xs ys = (all (flip elem xs) ys) && (all (flip elem ys) xs)

instance Eq a =>Eq (Grafo a) where
  (==) g1 g2 = (conjuntosIguales (nodos g1) (nodos g2)) && (all (\x->conjuntosIguales (adyacencias g1 x) (adyacencias g2 x)) (nodos g1))

-- para mostrar las salidas de las funciones definimos un impresor del grafo
showNodos :: Grafo a -> [a]
showNodos g = (nodos g) 

showAdyacencia :: Grafo a -> a -> [a]
showAdyacencia g n = (adyacencias g n)

showGrafo :: Grafo a -> [(a,[a])]
showGrafo g = [ (x,(adyacencias g x)) | x<-(nodos g)]

-- Como la lista de adyacencias no contiene elementos repetidos, se puede usar break con la primer y unica aparicion del elemento
quitar :: Eq a => a -> [a] -> [a]
quitar e xs = fst (break (==e) xs) ++ tail (snd (break (==e) xs) )

-- vecinos de un nodo n, devuelve los nodos que estan conectados con n sin tener en cuenta las direcciones de los ejes
vecinos :: Eq a => Grafo a -> a -> [a]
vecinos g n = [ vecino | vecino <-(nodos g) , (elem n (adyacencias g vecino)) ||  (elem vecino (adyacencias g n))]

subLista ::  Int -> Int -> [a] -> [a]
subLista i j xs = [ xs !! k | k<-[i..j] ] 

-- devuelve el valor de valores de hacer la conjuncion entre los valores de la lista
conjuncionValores :: [Bool] -> Bool
conjuncionValores  = foldr (\x -> \rec -> x && rec) True

--Funciones pedidas--

------------------------------------- ejercicio 1 ----------------------------------------
-- La función "agregar nodo" construye un nuevo grafo a partir del dado, agregándole un nodo más sin adyacencias.
-- Para implementar esta función, nos dimos cuenta que la construcción del grafo se hace sin el uso de las llaves (las cuales se encuetran en su definicion). Nos valimos como ejemplos los que dió la catedra.


agregarNodo :: Eq a=>Grafo a -> a -> Grafo a
agregarNodo g n | elem n (nodos g) = g
				| otherwise  = G ((nodos g)++[n]) ady1
				where ady1 x |(x == n) = []
					     | not(x == n) = (adyacencias g x)  

			
------------------------------------------Ejercicio2--------------------------------------
 
agregarEje :: Eq a=>Grafo a -> (a,a) -> Grafo a
agregarEje g (n,m) | elem m (adyacencias g n) = g
				   | otherwise = G (nodos g) ady1
				where ady1 x | (x == n) && not(n == m) = (adyacencias g n)++[m]
					     | not (x == n) = (adyacencias g x)
					     | (x == m) = (adyacencias g x) 


-----------------------------------------Ejercicio 3----------------------------------------
{--
El eje a sacar viene dado por (n,m).  Como el grafo es dirigido, el eje que se saca solo afecta a las adyacencias de n y no a las de m. Por tal motivo sólo se tienen en cuenta los casos donde x es igual a n o distintinto de este, donde x representa a un nodo.
Como hay que comparar nodos, para poder sacar el eje, es necesario que el tipo de los elementos contenga la igualdad.
--}		  

sacarEje:: Eq a => Grafo a -> (a,a) -> Grafo a 
sacarEje g (n,m)   | elem m (adyacencias g n) = G (nodos g) ady1
				   | otherwise = g
			where ady1 x | not (x == n) = (adyacencias g x)
						 | (x == n) = quitar m (adyacencias g x)

-----------------------------------------Ejercicio 4--------------------------------------------- 
{--
El nodo a sacar, va a afectar los ejes que llegan a él. Es decir, el hecho de sacar el nodo x va a afectar a las adyacencias de los nodos que tengan a este nodo como adyacencia. Es decir, a los nodos afectados se les quita la adyacencia correspondiente a x.
Por tal motivo, se revisa para cada nodo (n) en el grafo, distinto de x, para ver si x pertenece a las adyacencias de n. De ser así, se elimina dicha adyacencia.
--}


sacarNodo:: Eq a => Grafo a -> a -> Grafo a
sacarNodo g n | elem n (nodos g)= G (quitar n (nodos g)) ady1
			  | otherwise = g
				where ady1 x | not (x == n) = if (elem n (adyacencias g x)) then (quitar n (adyacencias g x)) else (adyacencias g x)

	
-----------------------------------------Ejercicio 5-----------------------------------------------

{--
Para calcular el grado de un nodo se dividió en problema en la suma de dos subproblemas: calcular el grado de entrada y calcular el grado de salida.
Dado un nodo a, se lista todos los nodos que lo tienen en su lista de adyacencias. La longitud de esa lista determina el grado de entrada. A eso se le suma la longitud de la lista de sus adyaencias que es el grado de salida. La suma de ambos nos da el grado total.
--}

gradoDeSalida:: Grafo a -> a -> Int
gradoDeSalida g n = length (adyacencias g n)

gradoDeEntrada :: Eq a => Grafo a -> a -> Int
gradoDeEntrada g n = length ([ x | x<-(nodos g), (not (x == n)) && (elem n (adyacencias g x) )]) 

grado:: Eq a => Grafo a -> a -> Int
grado g n = (gradoDeSalida g n) + (gradoDeEntrada g n)

	
{-- 
maximoGrado devuelve el máximo de los grados de los nodos de un grafo
Nos pareció indispensable usar recursión para obtener el máximo de una lista de números. Para esto definimos dameElMax usando foldr.
Como comentario aparte, también se podria haber hecho  "fold1 max [ (grado g n) | n<- (nodos g) ]"
--}	

maximoGrado:: Eq a => Grafo a -> Int
maximoGrado g = dameElMax [ (grado g n) | n<- (nodos g) ]

dameElMax :: [Int] -> Int
dameElMax  = foldr (\x rec -> (max x rec)) 0

---------------------------------------Ejercicio 6------------------------------------------------ 
{--
	 Diferencia: dada dos listas, xs e ys, devuelve la lista de aquellos elementos en xs que no estan en ys
	 Como está implementada, "diferencia" hace la diferencia de la segunda lista con la primera. O sea a la segunda lista le quita los elementos que también están en la primera.			  
--}

diferencia:: Eq a => [a] -> [a] -> [a]
diferencia = \xs -> foldr (\x rec -> if (elem x xs) then rec else [x]++rec ) []

{--
subgrafo:
Un subgrafo g1 de G es un grafo cuyo conjunto de vértices es un subconjunto del de G, y cuyo conjunto de aristas es un subconjunto del de G, restringido a ese subconjunto de vértices. 
La función dice si g1 es subgrafo de g2. Para esto verifica que la lista de nodos de g1, corresponde a una lista de nodos en el grafo g2. Por otra parte para ver si g1 es subgrafo de g2, hay que verificar que todos los ejes de g1 estén en g2 . 
Para verificar esto, se recorre cada nodo (de g1) y se chequea que las adyacencias que posee, también las posee ese mismo nodo en el otro grafo (osea en g2)
--} 

esSubgrafo:: Eq a => Grafo a -> Grafo a -> Bool
esSubgrafo g1 g2 = (esSubconjunto (nodos g1) (nodos g2)) && (ejesIncluidos g1 g2)

esSubconjunto :: Eq a => [a] -> [a] -> Bool
esSubconjunto nodos1 nodos2 = (diferencia nodos2 nodos1) == []

ejesIncluidos :: Eq a => Grafo a -> Grafo a -> Bool
ejesIncluidos g1 g2 = conjuncionValores [ (diferencia (adyacencias g2 x)  (adyacencias g1 x)) == [] | x<- nodos g1 ] 

---------------------------------------Ejercicio 7------------------------------------------------

{--
subgrafo inducido: dado un subconjunto de nodos, armar el grafo que consiste de estos nodos y todas las aristas entre ellos que se encuentren en el grafo original. 

--}

subgrafoInducido:: Eq a => Grafo a -> [a] -> Grafo a
subgrafoInducido g nodos = G nodos ady1
			where ady1 x = (lasAdyacencias g nodos x)


{--lasAdyacencias toma un grafo (g), una lista de nodos (nodos) y el nodo del cual buscamos sus adyacencias (nodo). Devuelve las adyacencias de ese nodo con los nodos pasados. La función recorre la lista nodos. Si el nodo_i pertenece a las adyacencias de nodo en g, entonces se agrega a la lista resultante. De esta manera se obtienen las adyacencias del nodo n en el subgrafo inducido--}


lasAdyacencias :: Eq a => Grafo a -> [a] -> a -> [a]
lasAdyacencias g nodos nodo = [n | n <- nodos , not (n==nodo) && (elem n (adyacencias g nodo))]


--------------------------------------------------Ejercicio 8 ---------------------------

{-- 
nodosGMayorIgual2 dado un Grafo filtra los nodos que tienen grado >= 2 (nos devuelve una lista de nodos).
--}
nodosGMayorIgual2 :: Eq a => Grafo a -> [a]
nodosGMayorIgual2 g  = filter (\n -> ((grado g n) >= 2)) (nodos g)

-- La función sinGradoMenor2 dado un grafo, devuelve el grafo resultante de eliminarle los nodos de grado < 2. 

sacarGradoMenor2 :: Eq a=> Grafo a -> Grafo a
sacarGradoMenor2 g = G(nodosGMayorIgual2 g) ady1
					where ady1 x = [n | n <- (adyacencias g x), not (elem n nodosEliminados)]
				              nodosEliminados = diferencia (nodosGMayorIgual2 g) (nodos g)


{-- La función hayCiclo revisa si existe un ciclo en el grafo g. Para esto aplica repetidas veces la función sacarGradoMenor2 que devuelve el grafo que resulta de quitarle los nodos de grado menor a 2. Luego de sucesivas aplicaciones si el grafo resultante no es vacío, entonces tiene un ciclo.
Esto lo hacemos usando la función iterate que toma una función f:a->a (en este caso sacarGradoMenor2) y un elemento (en este caso el grafo que queremos revisar) y le aplica la función infinitas veces. Sabemos que tarde o temprano la función va a devolver el mismo grafo que toma (es decir, a partir de un punto todos los elementos restantes de la lista infinita son iguales), pero como no sabemos la posición, no podemos usar la función take. Entonces decidimos crear una lista por comprensión que toma los elementos de la lista de iteraciones que no tenga nodos de grado menor a dos. Como son toods iguales, obtenemos la cabecera de la lista y si no es vacía, es porque es un ciclo perteneciente al grafo original.

--}


hayCiclo :: Eq a => Grafo a -> Bool
hayCiclo g = not (null (head [nodos x | x <- iteraciones, (null [n | n <- (nodos x), (grado x n) < 2])]))
	where iteraciones = (iterate(\k -> sacarGradoMenor2 k) g) 


-- ------------------------------------------------Ejercicio 9
{--
Un grafo g es conexo, si para todo par de nodos del grafo existe un camino que empiece en el extremo izquierdo del par y termine en el extremo derecho del par. Para lograr esto, primero se arman los pares y se obtienen todos los caminos posibles entre nodos, de la misma longitud. Es decir, se calculan las permutaciones de los nodos del grafo. Una vez hecho esto se consulta para cada par si existe algun camino, dentro de cada permutacion. 

--}

-- devuelve todos los pares de nodos, resultantes de las combinaciones de nodos del grafo
losParesDeNodos :: Grafo a -> [(a,a)]
losParesDeNodos g = [ ( (nodos g)!!i, (nodos g) !! j) | i<-[0..length (nodos g)-1], j <-[i+1..length (nodos g)-1] ]

-- se obtienen todos las permutaciones de nodos del grafo
permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = [((xs !! i) : ys) | i <- [0..(length xs-1)], ys <- permutaciones (sinElI xs i)]

sinElI xs i = [xs !! j | j <- [0..i-1]] ++ [xs !! k | k <- [i+1..(length xs -1)]]

-- dado un par devuelve todos los caminos existentes entre esos dos nodos. 
caminoPara :: Eq a => Grafo a -> (a,a) -> [[a]]
caminoPara g par = [ subLista i j xs | xs<- (permutaciones (nodos g)) ,i<- [0..length xs - 1] , j<- [i..length xs - 1] , (cumpleCondiciones g par (subLista i j xs) )]

hayCaminoPara :: Eq a => Grafo a -> (a,a) -> Bool
hayCaminoPara g p =  (length (caminoPara g p)) > 0 


-- se fija, dada una lista, si cumple que es un camino para el par
cumpleCondiciones :: Eq a => Grafo a ->  (a,a) -> [a] -> Bool
cumpleCondiciones g p xs = ((head xs) == (fst p) ) && ( (xs !! ((length xs) - 1) ) == (snd p)) && (esCamino g xs) 

-- se fija si los nodos de la lista forman un camino. (Sin tener en cuenta el sentido de los ejes) 
esCamino:: Eq a => Grafo a -> [a] -> Bool
esCamino g xs  = conjuncionValores [ (elem (xs !! i) (vecinos g (xs !! j))) | i<-[1..(length xs) -1], j<-[i-1] ]

esConexo :: Eq a => Grafo a -> Bool
esConexo g = conjuncionValores  [(hayCaminoPara g p) | p<- (losParesDeNodos g)]


-- ------------------------------------------------Ejercicio 10
{--
Decimos que un árbol es un grafo acíclico. Por lo tanto alcanza con comprobar que el grafo no tenga ciclos. Para esto utilizamos la función del ejercicio 8.
--}


esArbol::Eq a => Grafo a -> Bool
esArbol g = not (hayCiclo g)

------------------------
--Grafos de prueba--

grafo1::Grafo Char
grafo1 = G ['a','b','c','d'] ady1
	  where
		ady1 'a' = "bc"
		ady1 'b' = "c"
		ady1 'c' = "d"
		ady1 'd' = "a"
		ady1 x = error "El nodo "++(x:" no pertenece al grafo")

		
grafo2::Grafo Char
grafo2 = G ['a','c'] ady2
	  where
		ady2 'a' = ['c']
		ady2 'c' = []
		ady2 x = error "El nodo "++(x:" no pertenece al grafo")
		

grafo3::Grafo Int
grafo3 = G [1,2,3,4,5] (\x->filter (<= 5) [2*x,2*x+1])

grafo4::Grafo Int
grafo4 = agregarEje grafo3 (5,1)

grafo5::Grafo Int
grafo5 = agregarEje grafo3 (1,5)

grafo7::Grafo Int
grafo7 = agregarNodo grafo3 0

grafo8::Grafo Int
grafo8 = agregarNodo grafo5 0

--Agreguen sus propios grafos y pruebas.

grafo9::Grafo Char
grafo9 = G ['a','b','c','d'] ady1
	  where
		ady1 'a' = "b"
		ady1 'b' = ""
		ady1 'c' = "d"
		ady1 'd' = ""
		ady1 x = error "El nodo "++(x:" no pertenece al grafo")

grafo10::Grafo Char
grafo10 = G ['a','b'] ady1
	  where
		ady1 'a' = "b"
		ady1 'b' = ""

grafo11::Grafo Char
grafo11 = G ['a','b','c'] ady1
	  where
		ady1 'a' = "bd"
		ady1 'b' = ""
		ady1 'c' = ""

-- grafo12 es el resultante de agregarNodo 'e' al grafo1
grafo12::Grafo Char
grafo12 = G ['a','b','c','d','e'] ady1
	  where
		ady1 'a' = "bc"
		ady1 'b' = "c"
		ady1 'c' = "d"
		ady1 'd' = "a"
		ady1 'e' = ""
		ady1 x = error "El nodo "++(x:" no pertenece al grafo")


-- grafo13 es el resultado de agregarEje ('e','a') al grafo1 
grafo13::Grafo Char
grafo13 = G ['a','b','c','d','e'] ady1
	  where
		ady1 'a' = "bc"
		ady1 'b' = "c"
		ady1 'c' = "d"
		ady1 'd' = "a"
		ady1 'e' = "a"

-- grafo14 es el resultado de sacarEje ('d','a') al grafo1
grafo14::Grafo Char
grafo14 = G ['a','b','c','d'] ady1
	  where
		ady1 'a' = "bc"
		ady1 'b' = "c"
		ady1 'c' = "d"
		ady1 'd' = ""

-- grafo15 es el resultado de sacarEje ('a','b') al grafo1
grafo15::Grafo Char
grafo15 = G ['a','b','c','d'] ady1
	  where
		ady1 'a' = "c"
		ady1 'b' = "c"
		ady1 'c' = "d"
		ady1 'd' = "a"
		ady1 x = error "El nodo "++(x:" no pertenece al grafo")

-- grafo16 es el resultado de sacarNodo 'c' al grafo1
grafo16::Grafo Char
grafo16 = G ['a','b','d'] ady1
	  where
		ady1 'a' = "b"
		ady1 'b' = ""
		ady1 'd' = "a"


-- grafo17  es el subgrafo inducido de grafo1 a partir de los nodos ['a','c']
grafo17::Grafo Char
grafo17 = G ['a','c'] ady1
	  where
		ady1 'a' = "c"
		ady1 'c' = ""

-- grafo18  es el subgrafo inducido de grafo1 a partir de los nodos ['a','b']
grafo18::Grafo Char
grafo18 = G ['a','b'] ady1
	  where
		ady1 'a' = "b"
		ady1 'b' = ""

-- grafo19  es el subgrafo inducido de grafo1 a partir de los nodos ['a','c','d']
grafo19::Grafo Char
grafo19 = G ['a','c','d'] ady1
	  where
		ady1 'a' = "c"
		ady1 'c' = "d"
		ady1 'd' = "a"


grafo20::Grafo Char
grafo20 = G ['a','b'] ady1
	where
		ady1 'a' = "b"
		ady1 'b' = ""

grafo21::Grafo Char
grafo21 = G ['a','b','c'] ady1
	 where
		ady1 'a' = "bd"
		ady1 'b' = ""
		ady1 'c' = ""


grafoVacio:: Grafo Char
grafoVacio = G []  ady
	where ady x = []

grafoUnNodo :: Grafo Char
grafoUnNodo = G ['y'] ady
	where ady x = []

grafo22 :: Grafo Char
grafo22 = G ['a','b','c','d','e','f','g','h'] ady
	where ady 'a' = ""
	      ady 'b' = "ae"
	      ady 'c' = "ad"
	      ady 'd' = "b"
	      ady 'e' = "fg"
	      ady 'f' = "h"
	      ady 'g' = "h"
	      ady 'h' = ""


-- ------------------------------------TESTS------------------------------------------

					---Ejercicio 1--
testAgregarNodo = TestCase (do 
     				assertEqual "Agregar nodo 'e' al grafo1" (showGrafo grafo12) (showGrafo (agregarNodo grafo1 'e'))
                		)
testAgregarNodoExistente = TestCase (do 
     				assertEqual "Agregar nodo 'a' al grafo1" (showGrafo grafo1) (showGrafo (agregarNodo grafo1 'a'))
                		)

					---Ejercicio 2--
testAgregarEje = TestCase (do 
     				assertEqual "Agregar eje ('e','a') al grafo12" (showGrafo grafo13) (showGrafo (agregarEje grafo12 ('e','a')))
                		)
testAgregarEjeExistente = TestCase (do 
     				assertEqual "Agregar eje ('a','b') al grafo1" (showGrafo grafo1) (showGrafo (agregarEje grafo1 ('a','b')))
                		)
                		
					---Ejercicio 3--
testsacarEje_1 = TestCase (do 
     				assertEqual "Sacar eje ('d','a') al grafo1" (showGrafo grafo14) (showGrafo (sacarEje grafo1 ('d','a')))
                		)
testsacarEje_2 = TestCase (do 
     				assertEqual "Sacar eje ('a','b') al grafo1" (showGrafo grafo15) (showGrafo (sacarEje grafo1 ('a','b')))
                		)
testsacarEjeInexistente = TestCase (do 
     				assertEqual "Sacar eje ('b','a') al grafo9" (showGrafo grafo9) (showGrafo (sacarEje grafo9 ('b','a')))
                		)
					---Ejercicio 4--
testsacarNodo = TestCase (do 
      				assertEqual "Sacar nodo 'c' al grafo1" (showGrafo grafo16) (showGrafo (sacarNodo grafo1 'c') )
      					)
testsacarNodoInexistente = TestCase (do 
      				assertEqual "Sacar nodo 'e' al grafo1" (showGrafo grafo1) (showGrafo (sacarNodo grafo1 'e') )      				
                		)

					---Ejercicio 5--
testGrado = TestCase (do 
     				assertEqual "Grado nodo 'b' en el grafo1" 2 (grado grafo1 'b')
                		)

testMaximoGrado = TestCase (do 
     				assertEqual "El grado máximo del grafo1" 3 (maximoGrado grafo1)
                		)

					---Ejercicio 6--
testSubgrafo_1 = TestCase (do 
     				assertEqual "El grafo20 es subgrafo de grafo1" True (esSubgrafo grafo20 grafo1)
                		)

testSubgrafo_2 = TestCase (do 
     				assertEqual "El grafo21 no es subgrafo de grafo1" False (esSubgrafo grafo21 grafo1)

                		)

testSubgrafo_vacio = TestCase (do 
     				assertEqual "El grafoVacio es subgrafo de grafo1" True (esSubgrafo grafoVacio grafo1)

                		)

testSubgrafo_unNodo = TestCase (do 
     				assertEqual "El grafoUnNodo no es subgrafo de grafo1" False (esSubgrafo grafoUnNodo grafo1)

                		)

testDifListas_int = TestCase (do 
     				assertEqual "Diferencia de listas" [4,5] (diferencia [1,2,3,6,7,8,9] [4,2,3,1,5])
                		)

testDifListas_char = TestCase (do 
	  				assertEqual "Diferencia de listas" "c" (diferencia ['a','b'] ['c'])
                		)


					---Ejercicio 7--
testSubgrafoInducido_1 = TestCase (do 
     				assertEqual "Grafo inducido del grafo1, a partir de los nodos 'a' y 'c'" (showGrafo grafo17) (showGrafo (subgrafoInducido grafo1 ['a','c']))
                		)

testSubgrafoInducido_2 = TestCase (do 
     				assertEqual "Grafo inducido del grafo1, a partir de los nodos 'a' y 'b'" (showGrafo grafo18) (showGrafo (subgrafoInducido grafo1 ['a','b']))
                		)
testSubgrafoInducido_3 = TestCase (do 
     				assertEqual "Grafo inducido del grafo1, a partir de los nodos 'a', 'c' y 'd' " (showGrafo grafo19) (showGrafo (subgrafoInducido grafo1 ['a','c','d']))
                		)


					---Ejercicio 8--
testsacarGradoMenor2_1 = TestCase (do 
     				assertEqual "Grafo22 sin nodos de grado < 2" (showGrafo grafo22) (showGrafo (sacarGradoMenor2 grafo22))
                		)

testsacarGradoMenor2_2 = TestCase (do 
     				assertEqual "Grafo9 sin nodos de grado < 2" (showGrafo grafoVacio) (showGrafo (sacarGradoMenor2 grafo9))
                		)



testhayCiclo_true1 = TestCase (do 
     				assertEqual "Hay ciclo en el grafo1" True (hayCiclo grafo1)
                		)

testhayCiclo_true2 = TestCase (do 
     				assertEqual "Hay ciclo en el grafo22" True (hayCiclo grafo22)
                		)



testhayCicloVacio_false = TestCase (do 
     				assertEqual "No hay ciclo en el grafo vacío" False (hayCiclo grafoVacio)
                		)

testhayCicloUnNodo_false = TestCase (do 
     				assertEqual "No hay ciclo en el grafo de un solo nodo" False (hayCiclo grafoUnNodo)
                		)

					---Ejercicio 9--
testEsConexo_true = TestCase (do 
     				assertEqual "El grafo1 es conexo" True (esConexo grafo1)
                		)


testEsConexo_false = TestCase (do 
     				assertEqual "El grafo9 no es conexo" False (esConexo grafo9)
                		)

testEsConexoVacio_true = TestCase (do 
     				assertEqual "El grafoVacio es conexo" True (esConexo grafoVacio)
						) 

testEsConexoUnNodo_true = TestCase (do 
     				assertEqual "El grafoUnNodo es conexo" True (esConexo grafoVacio)
						) 

					---Ejercicio 10--
testesArbol_False1 = TestCase (do 
     				assertEqual "El grafo1 no es arbol" False (esArbol grafo1)
                		)

testesArbol_False2 = TestCase (do 
     				assertEqual "El grafo22 no es arbol" False (esArbol grafo22)
                		)



testesArbolVacio_true = TestCase (do 
     				assertEqual "El grafo vacío es un arbol" True (esArbol grafoVacio)
                		)

testesArbolUnNodo_true = TestCase (do 
     				assertEqual "El grafo de un solo nodo es un arbol" True (esArbol grafoUnNodo)
                		)


tests = TestList [TestLabel "Agregar nodo 'e' al grafo1" testAgregarNodo,
		TestLabel "Agregar nodo 'a' al grafo1" testAgregarNodoExistente,
		 ---Ejercicio 2--
		TestLabel "Agregar eje ('e','a') al grafo1" testAgregarEje,
		TestLabel "Agregar eje ('a','b') al grafo1" testAgregarEjeExistente,
		---Ejercicio 3--
		TestLabel "Sacar eje ('d','a') al grafo1" testsacarEje_1,
		TestLabel "Sacar eje ('a','b') al grafo1" testsacarEje_2,
		TestLabel "Sacar eje ('b','a') al grafo9" testsacarEjeInexistente,
		---Ejercicio 4--
		TestLabel "Sacar nodo 'c' al grafo1" testsacarNodo,
		TestLabel "Sacar nodo 'e' al grafo1" testsacarNodoInexistente,
		---Ejercicio 5--
		TestLabel "Grado del nodo 'b' en el grafo1" testGrado,
		TestLabel "El grado máximo del grafo1" testMaximoGrado,
		---Ejercicio 6-- 
		TestLabel "El grafo20 es subgrafo de grafo1" testSubgrafo_1,
		TestLabel "El grafo21 no es subgrafo de grafo1" testSubgrafo_2,
		TestLabel "El grafoVacio es subgrafo de grafo1" testSubgrafo_vacio,
		TestLabel "El grafoUnNodo no es subgrafo de grafo1" testSubgrafo_unNodo,
		TestLabel "Diferencia de listas de enteros" testDifListas_int,
		TestLabel "Diferencia de listas de chars" testDifListas_char,
		---Ejercicio 7--				
		TestLabel "Grafo inducido del grafo1, a partir de los nodos 'a' y 'c'" testSubgrafoInducido_1,
		TestLabel "Grafo inducido del grafo1, a partir de los nodos 'a' y 'b'" testSubgrafoInducido_2,
		TestLabel "Grafo inducido del grafo1, a partir de los nodos 'a', 'c' y 'd' " testSubgrafoInducido_3,
		---Ejercicio 8--
		TestLabel "sacarGradoMenor2 del grafo 22" testsacarGradoMenor2_1,
		TestLabel "sacarGradoMenor2 al grafo9" testsacarGradoMenor2_2,
		TestLabel "Hay ciclo en el grafo1" testhayCiclo_true1,
		TestLabel "Hay ciclo en el grafo22" testhayCiclo_true2,
		TestLabel "Hay ciclo en el grafo vacío" testhayCicloVacio_false,
		TestLabel "Hay ciclo en el grafo con un nodo" testhayCicloUnNodo_false,
		---Ejercicio 9--
		TestLabel "El grafo1 es conexo" testEsConexo_true,
		TestLabel "El grafo9 no es conexo" testEsConexo_false,
		TestLabel "El grafoVacio es conexo" testEsConexoVacio_true,
		TestLabel "El grafoUnNodo es conexo" testEsConexoUnNodo_true,
		---Ejercicio 10--
		TestLabel "El grafo1 no es arbol" testesArbol_False1,
		TestLabel "El grafo22 no es arbol" testesArbol_False2,
		TestLabel "El grafo vacío es un arbol" testesArbolVacio_true,
		TestLabel "El grafo con un nodo es un arbol" testesArbolUnNodo_true]

main :: IO Counts
main = runTestTT tests

