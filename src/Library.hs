module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Punto 1

type Nombre = String

type CantIngredientes = Number

type TiempoDeCoccion = Number

type Precio = Number

type NombreComensal = String

type Conservador = Bool

type CreeEnPropina = Bool

data Comida = UnaComida
  { nombre :: Nombre,
    cantIngredientes :: CantIngredientes,
    tiempodeCoccion :: TiempoDeCoccion
  }
  deriving (Show)

--data Comensal = UnComensal {
--    apodo:: String,
--    formaDeDejarPropina :: Number -> Number-
--}

type Comensal = Number -> Number

--EL precio de cada comida va a estar definido por el largo de su nombre, por el triple del tiempo de coccion, por la cantidad de ingredientes, mas IVA

pizza :: Comida
pizza = UnaComida "Pizza" 4 2

estofado :: Comida
estofado = UnaComida "Estofado" 6 2

hamburguesa :: Comida
hamburguesa = UnaComida "Hamburguesa" 2 0.25

hamburguesaCompleta :: Comida
hamburguesaCompleta = UnaComida "Hamburguesa Completa" 4 0.35

pebete :: Comida
pebete = UnaComida "Pebete" 3 0.10

precio :: Comida -> Precio
precio (UnaComida nombre cantIngredientes tiempodeCoccion) = (length nombre * (3 * tiempodeCoccion) * cantIngredientes) * 1.21

valoracion :: Number -> Comida -> Number
valoracion anios comida = precio comida * anios

almuerzo :: [Comida]
almuerzo = [pizza, pebete]

cenaFinAnio :: [Comida]
cenaFinAnio = [estofado, pizza, hamburguesaCompleta, pizza]

--Punto 2
{-
pepe :: Comensal
pepe = UnComensal "Pepe" calculoComun

tioCarlos :: Comensal
tioCarlos = UnComensal "Tio Carlos" calculoTioCarlos

aquamarine :: Comensal
aquamarine = UnComensal "Aquamarine" calculoConservador

ruby :: Comensal
ruby = UnComensal "Ruby" calculoConservador

kana :: Comensal
kana = UnComensal "Kana" calculoConservador
 -}
precioTotal :: [Comida] -> Precio
precioTotal almuerzo = sum (map precio almuerzo)

conservador :: Number -> Number
conservador importe
  | importe > 2000 = 150
  | otherwise = 100

noCreyente importe = 0

comun importe = importe * 0.1

tioCarlos importe = comun importe / 2

--calcularPropina comensal importe  =
--    (formaDeDejarPropina comensal) importe

totalAPagar :: Comensal -> [Comida] -> Precio
totalAPagar comensal comidas = precioTotal comidas + comensal (precioTotal comidas)
