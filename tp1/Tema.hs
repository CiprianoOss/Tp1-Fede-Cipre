module Tema (Tema, nuevoT, nombreT,datosT, etiquetasT, agregarT,aplicaT )
    where
import Tipos 
data Tema = Tem Nombre [ Etiqueta ] Datos deriving (Eq, Show, Ord)

nuevoT :: Nombre -> Datos -> Tema
nuevoT nombre datos = Tem nombre [] datos

nombreT :: Tema -> Nombre
nombreT (Tem n ets d) = n

datosT :: Tema -> Datos
datosT (Tem n ets d) = d

etiquetasT :: Tema -> [ Etiqueta ]
etiquetasT (Tem n ets d) = ets

agregarT :: Etiqueta -> Tema -> Tema
agregarT et (Tem nombre ets d) = Tem nombre (et : ets) d

aplicaT :: Etiqueta -> Tema -> Bool
aplicaT et (Tem nombre ets d) = elem et ets
