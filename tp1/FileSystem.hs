module FileSystem ( FileSystem, nuevoF, etiquetasF, temasF, agregarF, filtrarF )
where
import Tipos
import Tema
data FileSystem = FS [Etiqueta] [Tema] deriving (Eq, Show)

nuevoF :: FileSystem
nuevoF = FS [] []

etiquetasF :: FileSystem ->[ Etiqueta ]
etiquetasF (FS etiq temas) =   etiq

temasF :: FileSystem ->[ Tema ]
temasF (FS etiq temas) = temas

agregarF :: Tema -> FileSystem -> FileSystem
agregarF tema (FS etiq temas) = FS etiq (temas ++ [tema])

filtrarF :: Etiqueta -> FileSystem -> [Tema]
filtrarF etiqueta (FS etiquetas temas) =
  filter (\tema -> aplicaT etiqueta tema) temas


