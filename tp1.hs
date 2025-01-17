-- Monopoly

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




--pasarPorElBanco :: Persona -> Persona
--pasarPorElBanco unaPersona = (aumentarDineral 40 . cambiarTactica "Comprador Compulsivo") unaPersona

--aumentarDineral :: Int -> Persona -> Persona
--aumentarDineral unDinero unaPersona = unaPersona { dinero =dinero unaPersona + unDinero}

--pagarAAccionista :: Persona -> Persona
--pagarAAccionista unaPersona | tieneTactica "accionista" unaPersona = restarDineral 100 unaPersona
--                            | otherwise = aumentarDineral 200 unaPersona

--enojarse :: Persona -> Persona
--enojarse unaPersona = (aumentarDineral 50 . agregarAccion gritar)unaPersona

-- probarValor unDinero unaPersona =  modificarDinero (+ unDinero ) unaPersona

--acciones


modificarDinero :: (Int -> Int) -> Persona -> Persona
modificarDinero unaFuncion unaPersona = unaPersona { dinero = unaFuncion (dinero unaPersona)}


pasarPorElBanco :: Persona -> Persona
pasarPorElBanco unaPersona = (modificarDinero (+40) . cambiarTactica "Comprador Compulsivo") unaPersona


cambiarTactica :: String -> Persona -> Persona
cambiarTactica unaNuevaTactica unaPersona = unaPersona {tactica = unaNuevaTactica}


enojarse :: Persona -> Persona
enojarse unaPersona = (modificarDinero (+50) . agregarAccion gritar)unaPersona

agregarAccion :: Acciones -> Persona -> Persona
agregarAccion unaAccion unaPersona = unaPersona {acciones = unaAccion : (acciones unaPersona)   }

agregarOnomatopeya :: String -> Persona -> Persona
agregarOnomatopeya unOnomatopeya unaPersona = unaPersona { nombre = nombre unaPersona ++ unOnomatopeya}

gritar :: Persona -> Persona
gritar unaPersona = agregarOnomatopeya "AHHHH" unaPersona

tieneTactica :: String -> Persona -> Bool
tieneTactica unaTactica unaPersona = ((== unaTactica). tactica) unaPersona

subastar :: Propiedad ->  Persona -> Persona
subastar unaPropiedad unaPersona
 | esTacticaOferenteOAccionista unaPersona = (restarValorPropiedad unaPropiedad . agregarAdquisicion unaPropiedad ) unaPersona
 | otherwise = id unaPersona

esTacticaOferenteOAccionista  :: Persona -> Bool
esTacticaOferenteOAccionista  unaPersona = tieneTactica "oferente singular" unaPersona || tieneTactica "accionista" unaPersona

restarValorPropiedad :: Propiedad -> Persona -> Persona
restarValorPropiedad (_,valorPropiedad) unaPersona = restarDineral valorPropiedad unaPersona

agregarAdquisicion :: Propiedad -> Persona -> Persona
agregarAdquisicion unaPropiedad unaPersona = unaPersona {propiedadesCompradas = unaPropiedad : (propiedadesCompradas unaPersona) }

--cobrarAlquileres :: Persona -> Persona
--cobrarAlquileres unaPersona = aumentarDineral (alquileresAcobrar unaPersona) unaPersona

--alquileresAcobrar :: Persona -> Int
--alquileresAcobrar unaPersona = sum.map precioAlquiler propiedadesCompradas unaPersona


precioAlquiler unaPropiedad | esPropiedadBarata unaPropiedad = (*10)
                            | otherwise = (*20)

propiedadPrecio :: Propiedad -> Int
propiedadPrecio unaPropiedad = snd unaPropiedad

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = propiedadPrecio unaPropiedad < 150

cantidadCasasBaratas :: [Propiedad] -> Int
cantidadCasasBaratas unaPropiedad = (length.filter esPropiedadBarata) unaPropiedad


pagarAAccionista :: Persona -> Persona
pagarAAccionista unaPersona | tieneTactica "accionista" unaPersona = modificarDinero (+(-100)) unaPersona
                            | otherwise = modificarDinero (+200) unaPersona

restarDineral :: Int -> Persona -> Persona
restarDineral unDinero unaPersona = unaPersona { dinero = dinero unaPersona - unDinero}

hacerBerrinchePor unaPropiedad unaPersona | alcanzaDinero unaPropiedad unaPersona  = comprarPropiedad unaPropiedad unaPersona
                                          | otherwise  = ((hacerBerrinchePor unaPropiedad). modificarDinero (+10)) unaPersona

alcanzaDinero unaPropiedad unaPersona = propiedadPrecio unaPropiedad <= dinero unaPersona

comprarPropiedad unaPropiedad unaPersona = (agregarAdquisicion unaPropiedad . gritar) unaPersona

--dineroTotal :: Persona -> Persona
concatenarAcciones unaPersona =  foldl1 (.) (acciones unaPersona) unaPersona

-- devuelve las acciones de tipo lista de una persona 
--ultimaRumba unaPersona = (acciones.concatenarAcciones) unaPersona

--juegoFinal unaPersona = map (dinero unaPersona) (ultimaRumba unaPersona) unaPersona
dineroTotal unaPersona = (dinero.concatenarAcciones) unaPersona

juegoFinal unaPersona otraPersona | dineroTotal unaPersona < dineroTotal otraPersona  = concatenarAcciones otraPersona
                                  | otherwise =  concatenarAcciones unaPersona
