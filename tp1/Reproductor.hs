module Reproductor ( Reproductor, nuevoR, archivosR, listaParaR, temasR, playR, actualR, avanzarR, retrocederR,
reiniciarR )
where
import Tipos
import Tema
import Playlist
import FileSystem
data Reproductor = RP FileSystem Playlist deriving (Eq, Show)

nuevoR :: FileSystem ->Reproductor
nuevoR FS = RP FileSystem Playlist

archivosR :: Reproductor ->FileSystem
archivosR (RP filesis playlist) = filesis

listaParaR :: Etiqueta -> Reproductor -> [Tema]
listaParaR etiq (RP filesis playlist) = filtrarF etiq filesis playlist
  where
    filtrarF etiqueta filesystem playlist = filter (\tema -> aplicaT etiqueta tema) (temasF filesystem)

temasR :: Reproductor -> [Tema]
temasR (RP fs playlist) = temasF fs ++ playlistTemas playlist
  where
    playlistTemas :: Playlist -> [Tema]
    playlistTemas (Play _ temas) = temas

playR :: Reproductor -> Etiqueta -> Reproductor
playR (RP fs pl) et = RP fs (nuevaP (filtrarF et fsTemas))
  where
    fsTemas = temasF fs


actualR :: Reproductor -> Tema
actualR (RP fs pl) = actualP pl


avanzarR :: Reproductor -> Reproductor
avanzarR (RP fs (Play i temas))
    | i < length temas - 1 = RP fs (skipP (Play i temas))
    | otherwise = RP fs (Play i temas)

retrocederR :: Reproductor -> Reproductor
retrocederR (RP fs (Play i temas))
    | i > 0 = RP fs (skipP (Play (i-1) temas))
    | otherwise = RP fs (Play i temas)


