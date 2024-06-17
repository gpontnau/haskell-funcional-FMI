{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Library where
import PdePreludat

data Pais = Pais {
    ingresoPerCapita  :: Number,
    poblacionActiva   :: PoblacionActiva,
    recursosNaturales :: [RecursosNaturales],
    deudaFMI          :: Number
} deriving (Show)

data PoblacionActiva = PoblacionActiva {
    sectorPublico :: SectorPublico,
    sectorPrivado :: SectorPrivado
} deriving (Show)

type RecursosNaturales = String
type SectorPublico = Number
type SectorPrivado = Number

type Estrategia = Pais -> Pais

modificaIngresoPerCapita :: Number -> Estrategia
modificaIngresoPerCapita porcentaje pais = pais { ingresoPerCapita = ingresoPerCapita pais * porcentaje }


-- prestarle n millones de dólares al país, esto provoca que el país se endeude 
-- en un 150% de lo que el FMI le presta (por los intereses)
prestarDinero :: Number -> Estrategia
prestarDinero cantidad pais = pais { deudaFMI = deudaFMI pais + paisEnDeudado cantidad }

paisEnDeudado :: Number -> Number
paisEnDeudado cantidad = cantidad * 2.5

-- reducir x cantidad de puestos de trabajo del sector público, lo que provoca 
-- que se reduzca la cantidad de activos en el sector público y además que el 
-- ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 
-- 100 ó 15% en caso contrario
reducirPuestosDeTrabajo :: SectorPublico -> Estrategia
reducirPuestosDeTrabajo puestosDeTrabajo pais
    | puestosDeTrabajo > 100 = modificaIngresoPerCapita 0.8 pais
    | otherwise              = modificaIngresoPerCapita 0.85 pais

-- darle a una empresa afín al FMI la explotación de alguno de los recursos 
-- naturales, esto disminuye 2 millones de dólares la deuda que el país mantiene 
-- con el FMI pero también deja momentáneamente sin recurso natural a dicho país. 
-- No considerar qué pasa si el país no tiene dicho recurso.
darEmpresaAfin :: RecursosNaturales -> Estrategia
darEmpresaAfin recurso pais = pais { recursosNaturales = quitarRecurso recurso $ recursosNaturales pais,
                                            deudaFMI = deudaFMI pais - 2000000 }

quitarRecurso :: RecursosNaturales -> [RecursosNaturales] -> [RecursosNaturales]
quitarRecurso recurso = filter (/= recurso)


-- establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de 
-- su Producto Bruto Interno (que se calcula como el ingreso per cápita multiplicado
-- por su población activa, sumando puestos públicos y privados de trabajo) y reducir 
-- 500 puestos de trabajo del sector público.
blindajePais :: Estrategia
blindajePais pais = (prestarDinero (pbi pais / 2) . reducirPuestosDeTrabajo 500) pais

-- Suma el Number del sector público con el del sector privado
pbi :: Pais -> Number
pbi pais = ingresoPerCapita pais * (sectorPublico (poblacionActiva pais) + sectorPrivado (poblacionActiva pais))


-- Se pide implementar en Haskell los siguientes requerimientos explicitando el tipo de cada función:
-- Representar el TAD País.

-- Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita es de 4140 u$s, 
-- la población activa del sector público es de 400.000, la población activa del sector 
-- privado es de 650.000, su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$s) al FMI.
namibia :: Pais
namibia = Pais {
    ingresoPerCapita = 4140,
    poblacionActiva = PoblacionActiva {
        sectorPublico = 400000,
        sectorPrivado = 650000
    },
    recursosNaturales = ["Minería", "Ecoturismo"],
    deudaFMI = 50000000
}


type Receta = [Estrategia] 

estrategias :: Pais -> Receta -> Pais
estrategias = foldr ($)

{- Implementar las estrategias que forman parte de las recetas del FMI. 
> estrategias namibia [blindajePais . darEmpresaAfin "Minería" . reducirPuestosDeTrabajo 500. prestarDinero 1000000]   
Pais
    { ingresoPerCapita = 2649.6
    , poblacionActiva = PoblacionActiva
        { sectorPublico = 400000
        , sectorPrivado = 650000
        }
    , recursosNaturales = []
    , deudaFMI = 1709112500
    }
-}

-- 3
-- a) Modelar una receta que consista en prestar 200 millones, y darle a una empresa X
-- la explotación de la Minería de un país
prestarMillonesYDarEmpresa :: Estrategia
prestarMillonesYDarEmpresa = (prestarDinero 2000000) . (darEmpresaAfin "Minería")

-- b) Aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). 
-- Justificar cómo se logra el efecto colateral.
estrategiasAplicadas :: Receta -> Pais -> Pais
estrategiasAplicadas receta pais = foldr ($) pais receta


-- 4.a) Conocer los países que pueden zafar, que son aquellos que tienen "Petróleo" entre sus riquezas naturales.
puedenZafar:: [Pais] -> [Pais]
puedenZafar = filter $ elem "Petroleo" . recursosNaturales

-- 4.b) Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
totalDeudaPaises :: [Pais] -> Number
totalDeudaPaises = foldr ((+) . deudaFMI) 0 

-- 4.c) Indicar en dónde apareció cada uno de los conceptos (solo una vez) y justificar qué ventaja tuvo para resolver el requerimiento.
-- Reusabilidad: Las funciones de orden superior, la composición y la aplicación parcial permiten escribir 
--             código más modular y reutilizable. Las funciones pequeñas y específicas pueden combinarse 
--             de maneras diferentes para crear funcionalidades complejas sin duplicar código.

-- Claridad: Estas técnicas permiten expresar la lógica de manera más concisa y declarativa. En lugar de 
--         escribir bucles y estructuras condicionales explícitas, podemos utilizar funciones bien definidas 
--         que capturan la esencia de la operación.

-- Facilidad de Razón: La composición y la aplicación parcial ayudan a mantener las funciones puras, lo que 
--                 facilita razonar sobre el código y predecir su comportamiento.





-- Punto 5
-- dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, 
-- en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor
recetasOrdenadas :: Pais -> [Receta] -> Bool
recetasOrdenadas pais [receta] = True
recetasOrdenadas pais (receta1:receta2:recetas) = 
    revisarPBI receta1 pais <= revisarPBI receta2 pais && recetasOrdenadas pais (receta2:recetas)
    where revisarPBI receta = pbi . estrategiasAplicadas receta 

-- Punto 6 
-- Si un país tiene infinitos recursos naturales, modelado con esta función
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

-- --    a) ¿qué sucede con la función 4a? 
-- --    b) ¿y con la 4b?
-- --    Justifique ambos puntos relacionándolos con algún concepto.
-- pruebaInfinita1 = puedenZafar [namibia, Pais 1 1 1 recursosNaturalesInfinitos 1]
-- --              no termina nunca, porque quiere buscar "Mineria" entre los recursos
-- pruebaInfinita2 = totalDeuda [namibia, Pais 1 1 1 recursosNaturalesInfinitos 1]
-- --              se puede porque al no evaluar los recursos solamente suma deuda
-- -- relacionado con evaluacion diferida, solo se evalua lo que se necesita

