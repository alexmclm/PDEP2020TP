-- Monopoly
{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
import Data.Maybe -- por si llegan a usar un metodo de coleccion y devuelva Nothing or justElements
import Text.Show.Functions -- para que las funciones dentro del data se vean <function>
import Test.Hspec -- para usar los test

data Persona = Persona {
    nombre :: Nombre,
    dinero :: Dinero,
    tactica :: Tactica,
    propiedadesCompradas :: [Propiedad],
    acciones :: [Acciones]
} deriving Show

type Nombre = String
type Dinero = Int
type Tactica = String
type Propiedad = (NombrePropiedad,PrecioPropiedad)
type Acciones = Persona -> Persona

type NombrePropiedad = String
type PrecioPropiedad = Int

carolina = Persona "Carolina" 500 "accionista"   [("auto",456)] [pagarAAccionista]
manuel = Persona "Manuel" 500 "oferente singular" []  [enojarse]

--acciones

pasarPorElBanco :: Persona -> Persona
pasarPorElBanco unaPersona = (aumentarDineral 40 . cambiarTactica "Comprador Compulsivo") unaPersona

aumentarDineral :: Int -> Persona -> Persona
aumentarDineral unDinero unaPersona = unaPersona { dinero =dinero unaPersona + unDinero}

cambiarTactica :: String -> Persona -> Persona
cambiarTactica unaNuevaTactica unaPersona = unaPersona {tactica = unaNuevaTactica}

enojarse :: Persona -> Persona
enojarse unaPersona = (aumentarDineral 50 . agregarAccion gritar)unaPersona

agregarAccion :: Acciones -> Persona -> Persona
agregarAccion unaAccion unaPersona = unaPersona {acciones = unaAccion : (acciones unaPersona)   }

gritar :: Persona -> Persona
gritar unaPersona = unaPersona {nombre = nombre unaPersona ++ "AHHHH"}


tieneTactica :: String -> Persona -> Bool
tieneTactica unaTactica unaPersona = ((== unaTactica). tactica) unaPersona


subastar :: Propiedad ->  Persona -> Persona
subastar unaPropiedad unaPersona | tieneTactica "oferente singular" unaPersona || tieneTactica "accionista" unaPersona = (restarValorPropiedad unaPropiedad . agregarAdquisicion unaPropiedad ) unaPersona
                                 | otherwise = id unaPersona


restarValorPropiedad :: Propiedad -> Persona -> Persona
restarValorPropiedad (_,propiedad) unaPersona = restarDineral propiedad unaPersona


agregarAdquisicion :: Propiedad -> Persona -> Persona
agregarAdquisicion unaPropiedad unaPersona = unaPersona {propiedadesCompradas = unaPropiedad : (propiedadesCompradas unaPersona) }

cobrarAlquileres :: Persona -> Persona
cobrarAlquileres unaPersona = aumentarDineral (alquileresAcobrar unaPersona) unaPersona

alquileresAcobrar :: Persona -> Int
alquileresAcobrar unaPersona =  (*10)(cantidadCasasBaratas (propiedadesCompradas unaPersona)) + (*20) (length (propiedadesCompradas unaPersona))


propiedadPrecio :: Propiedad -> Int
propiedadPrecio unaPropiedad = snd unaPropiedad

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = propiedadPrecio unaPropiedad < 150

cantidadCasasBaratas :: [Propiedad] -> Int
cantidadCasasBaratas unaPropiedad = length (filter esPropiedadBarata unaPropiedad)


pagarAAccionista :: Persona -> Persona
pagarAAccionista unaPersona | esAccionista unaPersona = restarDineral 100 unaPersona
                            | otherwise = aumentarDineral 200 unaPersona

restarDineral :: Int -> Persona -> Persona
restarDineral unDinero unaPersona = unaPersona { dinero =dinero unaPersona - unDinero}

esAccionista :: Persona -> Bool
esAccionista unaPersona = (tactica unaPersona) == "accionista"