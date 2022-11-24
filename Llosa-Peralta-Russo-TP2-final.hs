-- Llosa Fernandez Dante - dantellosafernandez@gmail.com
-- Peralta Santiago - santi.peralta99@gmail.com
-- Russo Agustin - agus.drum12@gmail.com

--Definimos pi con su letra griega para identificarla fácilmente.
π = pi

type Complejo = (Float, Float)

--1.1 Devuelve la parte real de un dado número complejo.
re :: Complejo -> Float
re (r,i) = r

--1.2 Devuelve la parte imaginaria de un dado número complejo.
im :: Complejo -> Float
im (r,i) = i

--1.3 Suma "componente a componente" las partes reales e imaginarias de un dado número complejo.
suma :: Complejo -> Complejo -> Complejo
suma (a,b) (c,d) = ((a+c),(b+d))

--Definimos la función "resta" para usarla mas adelante con la misma logica que suma.
resta :: Complejo -> Complejo -> Complejo
resta (a,b) (c,d) = ((a-c),(b-d))

--1.4 Aplicamos las propiedades del producto entre números complejos.
producto :: Complejo -> Complejo -> Complejo
producto (a,b) (c,d) = ((a*c-b*d),(a*d+b*c))

--1.5 Devuelve el mismo número complejo con su parte imaginaria multiplicada por -1.
conjugado :: Complejo -> Complejo
conjugado (a,b) = (a,(-b))

--1.6 Aplicamos las propiedades para obtener el inverso multiplicativo. 0 + 0i no tiene inverso.
inverso :: Complejo -> Complejo
inverso (0,0) = undefined
inverso (a,b) = ((a/d),(-b/d))
              where d = a^2+b^2

--1.7 Dividir es equivalente a multiplicar por el inverso. No se puede dividir por 0.
cociente :: Complejo -> Complejo -> Complejo
cociente _ (0,0) = undefined
cociente z w = producto z (inverso w)

--1.8 Definida recursivamente como z multiplicado por la potencia anterior.
potencia :: Complejo -> Integer -> Complejo
potencia z 1 = z 
potencia z n = producto z (potencia z (n-1))

--1.9 Separamos el problema en 2 casos y 2 partes. Un caso es con el determinante teniendo valor positivo, mientras que el otro caso es con el determinante negativo. Las partes son la parte que siempre es real del número y la parte que puede ser imaginaria.
raicesCuadratica :: Float -> Float -> Float -> (Complejo,Complejo)
raicesCuadratica a b c |     r < 0 = ((pre , pim),(pre,(-pim)))
                       | otherwise = ((pre+pim,0),(pre-pim, 0))
                       where r = b^2 - 4 * a * c
                             pre = (-b) / (2*a)
                             pim = (sqrt(abs(r))) / (2*a)

--2.1 Aplicamos la definición de módulo.
modulo :: Complejo -> Float
modulo (a, b) = sqrt(a^2 + b^2)

--2.2 Restamos los puntos y calculamos el módulo del resultado.
distancia :: Complejo -> Complejo -> Float
distancia z w = modulo (resta z w)

--2.3 Dividimos el problema en cuadrantes por el funcionamiento de "asin". Llamamos θ al resultado y fuimos sumando lo necesario en cada cuadrante. Entre los 4 cuadrantes se completan todas las posiblidades.
{--argumento :: Complejo -> Float
argumento z | cuadrante1 = θ
            | cuadrante2 = (π/2) - θ 
            | cuadrante3 = θ + π
            |  otherwise = ((3*π/2)) - θ
            where θ = asin (im z / modulo z)
                  θ2 = acos (re z / modulo z)
                  cuadrante1 = (im z >= 0 && re z > 0)
                  cuadrante2 = (im z > 0 && re z <= 0)
                  cuadrante3 = (im z <= 0 && re z < 0)
-- otherwise cubre las variantes pertenecientes al cuarto cuadrante.
--}
argumento :: Complejo -> Float
argumento z | im z >= 0 = θ
            | otherwise = 2*π - θ
             where θ = acos (re z / modulo z)

--2.4 Aplicamos propiedades para definir la parte real y la parte imaginaria del nuevo complejo.
pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r θ = (r*(cos θ),r*(sin θ))

--2.5 Planteamos raíz cuadrada basándonos en las 3 ecucaciones conocidas de un número complejo cualquiera:
--x^2 + y^2 = mod z
--x^2 - y^2 = re z
--2x  *  y  = im z
--Sumando y restando la primera y segunda ecuación podemos determinar los valores absolutos de "x" e "y", y con la tercera ecuación determinamos si los números son positivos o negativos. (esto es lo primero que chequea la funcion, si la parte imaginaria de "z" es mayor o menor a 0. En caso de ser igual a 0, planteamos las raíces de un número real.)
raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada (r,0) | r > 0 = ((sqrt r,0),(-sqrt r,0))
                   | otherwise = ((0,sqrt(abs r)),(0,-sqrt(abs r)))
raizCuadrada z | im z > 0 = ((x,y),(-x,-y))
               | otherwise = ((x,-y),(-x,y))
               where x = sqrt((modulo z + re z)/2)
                     y = sqrt((modulo z - re z)/2)

-- Función auxiliar para "raicesCuadraticaCompleja". Multiplica por un escalar la parte real y la imaginaria de un número complejo.
prodk (r,i) k = (k*r,k*i)

--2.6 Planteamos las limitaciones de la función con la primera linea de pattern matching. Luego aplicamos la fórmula para calcular raíces cuadráticas de un polinomio complejo. Decidimos restar ámbos "w1" y "w2" con "b" ya que es equivalente y queda más prolijo para no tener que negativizar "b". Hicimos uso de muchas funciones anteriores. No solo "raizCuadrada", sino también "cociente", "resta", "producto" y "prodk".
raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja (0,0) _ _ = undefined
raicesCuadraticaCompleja a b c = (cociente (resta w1 b) d, cociente (resta w2 b) d)
                               where w1 = fst (raizCuadrada det)
                                     w2 = snd (raizCuadrada det)
                                     det = resta (potencia b 2)  (prodk (producto a c) 4)
                                     d = prodk a 2

--3.1 Tomamos como caso base la raíz Nesima de todos los "Gn" que existen: (1,0) cuando nuestro "m" vale 0. Como sabemos que el argumento de las raíces se define como 2kπ/n con 0 <= k <= n-1, iteramos con estos valores empezando desde n-1 y terminando en 0 (caso base). Para esta iteración optamos por una función auxiliar.
raicesNEsimasAux :: Integer -> Integer -> [Complejo]
raicesNEsimasAux n 0 = [(1,0)]
raicesNEsimasAux n m = raicesNEsimasAux n (m-1) ++ [pasarACartesianas 1 ((2*fi(m)*π) / fi(n))]
                     where fi = fromIntegral

raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = raicesNEsimasAux n (n-1)

--3.2 Eleva un número complejo (el primero de la lista) a la "n" y le resta 1. Luego utiliza f que cumple función de cota superior para compararlo al 0. Esto es para evitar los errores de precisión que tiene haskell. La recursión pide que el proximo número de la lista sea también raíz Nesima, por lo tanto, devuelve "true" solo cuando TODOS los elementos de la lista son raíces Nesimas de 1.
sonRaicesEnesimas :: Integer -> [Complejo] -> Float -> Bool
sonRaicesEnesimas n []     f = True
sonRaicesEnesimas n [c]    f = modulo (resta (potencia c n) (1,0)) < f
sonRaicesEnesimas n (c:cs) f = modulo (resta (potencia c n) (1,0)) < f && sonRaicesEnesimas n cs f
-- la funcion funciona para todo f >= 0.000001 = 10^(-6)