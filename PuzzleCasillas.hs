-- Jaime Delgado Linares 
--  PUZZLE DE LAS CASILLAS. 

-- Definimos el tablero
type Tablero = [Int]

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
-- Operaciones auxiliares que nos ayudaran a realizar los movimientos
-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-- Las dos operaciones siguientes sirven para intercambiar los elementos de dos posiciones dadas
-- Busca el elemento a y lo cambia por el emento b (La uso solo para ayudar a la funcion "intercambia")
inter:: Int -> Int -> Tablero -> Tablero
inter _ _ [] = []
inter a b (x:xs) = if x == a then [b] ++ inter a b xs else if x == b then [a] ++ inter a b xs else [x] ++ inter a b xs
-- Cambia los elementos de las dos posiciones que le digas
intercambia:: Int -> Int -> Tablero -> Tablero
intercambia _ _ [] = []
intercambia a b (x:xs) = inter (elemento a (x:xs)) (elemento b (x:xs)) (x:xs)
	
	
-- Operacion que devuelve el elemento contenido en la posicion indicada
elemento:: Int -> Tablero -> Int
elemento _ [] = 0 
elemento 1 (x:xs) = x
elemento a (x:xs) = elemento (a-1) xs 
	
	
-- Devuelve la posicion en la que se encuentra el hueco
posHuecoLibre :: Tablero -> Int
posHuecoLibre [] = 0
posHuecoLibre (0:_) = 1
posHuecoLibre (x:xs) = 1 + posHuecoLibre xs

-- Devuelve una lista con la posicion del espacio en blanco actualizada.
-- Mueve el espacio hacia arriba si num=1, abajo si num=2, izq si num=3 y der si num=4. En otro caso devuelve el tablero inicial
mueve:: Int -> Tablero -> Int -> Tablero
mueve num x tam -- Compruebo errores
	| num==1 && (posHuecoLibre x)-tam<1 = x
	| num==2 && (posHuecoLibre x)+tam>tam*tam = x
	| num==3 && (posHuecoLibre x)-1<1 = x
	| num==4 && (posHuecoLibre x)+1>tam*tam = x
	| num==1 = intercambia (posHuecoLibre x) ((posHuecoLibre x)-tam) x --Arriba
	| num==2 = intercambia (posHuecoLibre x) ((posHuecoLibre x)+tam) x --Abajo
	| num==3 = intercambia (posHuecoLibre x) ((posHuecoLibre x)-1) x --Izquierda
	| num==4 = intercambia (posHuecoLibre x) ((posHuecoLibre x)+1) x --Derecha
	| otherwise = x

	
-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
-- Operaciones con tableros
-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
-- Introduce una lista en una lista de listas en la posicion k		
introduce:: [Tablero] -> Tablero -> Int -> [Tablero]
introduce (x:xs) y 1 = y:xs  
introduce (x:xs) y k 
	| k>length (x:xs) = (x:xs)
	| k<1 = (x:xs)
	| otherwise = x:(introduce xs y (k-1)) 

-- Comprueba si una lista esta en la lista de listas	
existe:: [Tablero] -> Tablero -> Bool
existe [[]] _ = False
existe (x:xs) z 
	| compruebaEstado x z = True
	| otherwise = existe xs z
existe _ _ = False

-- Comprueba si dos listas son iguales	
compruebaEstado:: Tablero -> Tablero -> Bool
compruebaEstado [] [] = True
compruebaEstado [] _ = False
compruebaEstado _ [] = False
compruebaEstado	(x:xs) (y:ys) = if x==y then compruebaEstado xs ys else False

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
-- Operaciones para la HEURISTICA
-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-- Vamos a implementar una heurística de fichas descolocadas sin contar el espacio en blanco ya que es admisible y consistente
-- Devuelve el numero de fichas descolocadas de un tablero comparandolo con la solucion
fichasDescolocadas:: Tablero -> Tablero -> Tablero -> Int -> Int
fichasDescolocadas _ [] _ _ = 0
fichasDescolocadas tablero (x:xs) solucion pos =
	let posSol = (posicion solucion x);
		posTab = (posicion tablero x);
	in	if x==0 then (fichasDescolocadas tablero xs solucion (pos+1))
		else if posSol == posTab then (fichasDescolocadas tablero xs solucion (pos+1))
		else 1 + (fichasDescolocadas tablero xs solucion (pos+1))

-- Devuelve la posicion del elemento en una lista
posicion:: Tablero -> Int -> Int
posicion [] _ = 0
posicion (x:xs) a 
	| x==a = 0
	| otherwise = 1 + (posicion xs a)
	


-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
-- Operacion que resuelve el problema del puzzle
-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-- Funcion que resuelve el problema. 
-- Dado un tablero, una lista de estados, la solucion al tablero, y el numero de filas/columnas devolvemos la lista 
-- con los estados hasta llegar a la solucion
resuelve:: Tablero -> [Tablero] -> Tablero -> Int -> [Tablero]
resuelve estado lista solucion tam = 
	if (compruebaEstado estado solucion) then reverse lista  -- Comprobamos si lo hemos resuelto
	else 
		
		let 
			-- Guardamos los posibles siguientes estados
			mov1 = mueve 1 estado tam;
			mov2 = mueve 2 estado tam;
			mov3 = mueve 3 estado tam;
			mov4 = mueve 4 estado tam;
		in  
			-- Si el estado ya estaba visitado volvemos al estado anterior
			if existe lista mov1 && existe lista mov2 && existe lista mov3 && existe lista mov4 then resuelve (lista!!1) lista solucion tam
			else
				
				let 
					-- Guardamos el valor de las heuristicas de cada movimiento
					hMov1 = fichasDescolocadas mov1 mov1 solucion 1;
					hMov2 = fichasDescolocadas mov2 mov2 solucion 1;
					hMov3 = fichasDescolocadas mov3 mov3 solucion 1;
					hMov4 = fichasDescolocadas mov4 mov4 solucion 1;
				in
					-- Si no lo hemos visitado y su heuristica es la mejor, visitamos ese estado
					if not (existe lista mov1) && hMov1<=hMov2 && hMov1<=hMov3 && hMov1<=hMov4 then  resuelve  mov1 ([mov1]++lista) solucion tam
					else if not (existe lista mov2) && hMov2<=hMov1 && hMov2<=hMov3 && hMov2<=hMov4 then resuelve mov2 ([mov2]++lista) solucion tam
					else if not (existe lista mov3) && hMov3<=hMov1 && hMov3<=hMov2 && hMov3<=hMov4 then resuelve mov3 ([mov3]++lista) solucion tam
					else if not (existe lista mov4) && hMov4<=hMov1 && hMov4<=hMov2 && hMov4<=hMov3 then resuelve mov4 ([mov4]++lista) solucion tam
					-- Volvemos al estado anterior
					else resuelve (lista!!1) lista solucion tam
					
					
-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////					
-- Operaciones para el programa principal
-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
-- Lectura de un entero (getInt no es predefinida)
getInt:: IO Int
getInt = do line <- getLine
            return (read line::Int) 

--Pide al usuario el numero de filas/columnas del tablero (3 para el puzzle de 8), y que introduzca los numeros que lo compondran
--(Si no rellenas todo el tablero, del tamaño que sea, saltara un error "Numero incorrecto de numeros")
introduceTablero:: IO [Tablero]
introduceTablero = 
 do putStrLn "Introduce el numero de filas/columnas del tablero(0 para salir)"
    n <- getInt;
	putStrLn "Introduce los numeros que compondran el tablero (0 para ficha blanco)"
    tab <- leeTablero (n*n);
	return (resuelve tab [tab] ([1,2..(n*n)-1]++[0]) n)
	
					
--Va leyendo numeros teniendo en cuenta la dimension del tablero						
leeTablero:: Int -> IO Tablero
leeTablero 0 = return []
leeTablero tamanyo =
	do	
		x <- getInt; 
		xs <- (leeTablero (tamanyo-1))
		return (x:xs)

--Recibe un tablero y devuelve una lista de tableros que son la solucion del problema		
casillas:: Tablero -> [Tablero]
casillas inicial = 
	let	tam = length inicial;
		n = raiz tam;
	in resuelve inicial [inicial] ([1,2..(tam)-1]++[0]) n

--Devuelve la raiz cuadrada de 4,9,16 y 25 para saber cuantas filas tiene el tablero 
--(Solo lo he hecho para los puzzles de 3, 8, 15 y 24 dado que más grande seria muy dificil de solucionar)
--Devuelve un error "Numero incorrecto de numeros" si no es ni 4,9,16 o 25
raiz:: Int -> Int
raiz num 
 | num==4 = 2
 | num==9 = 3
 | num==16 = 4
 | num==25 = 5
 | otherwise = error "Numero incorrecto de numeros"
