
module Tipos where
import Data.List

type Datos = String
type Etiqueta = String
type Nombre = String

insertar ::  Ord a => a -> [a] -> [a]
insertar cancion playlist  = sort(playlist ++ [cancion])












