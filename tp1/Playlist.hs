module Playlist ( Playlist, nuevaP,actualP,skipP,backP,resetP)
where
import Tipos
import Tema
data Playlist = Play Int [ Tema ] deriving (Eq, Show)
temaprueba = nuevoT "Hola" "chau"
playlistprueba = nuevaP [temaprueba]

nuevaP :: [ Tema ] -> Playlist
--A partir de una lista de temas crea una nueva Playlistt con su ´ındice en cero.
nuevaP temas = Play 0 temas

actualP :: Playlist -> Tema
--Dada una Playlist devuelve el tema en la posici´on indicada por el ´ındice.
actualP (Play indice temas) = temas !! indice

skipP :: Playlist ->Playlist
--Devuelve una Playlist con su ´ındice aumentado en uno.
skipP (Play indice temas) = Play (indice+1) temas
backP :: Playlist ->Playlist
--Idem anterior pero con el ındice decrementado en uno.
backP(Play indice temas) = Play (indice-1) temas

resetP :: Playlist ->Playlist
--Dada una Playlist crea una nueva con la lista de temas de la original.
resetP (Play indice temas) = Play 0 temas

