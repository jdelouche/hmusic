# hmusic
Midi haskell music

Usage en ligne de commande sur MacOsx avec VLC

cabal install && cat europa.txt | dist/build/hmusic/hmusic && /Applications/VLC.app/Contents/MacOS/VLC -I dummy mymusic.mid vlc://quit

input : europa.txt
output : mymusic.mid

En mode interactif:

echo "c1,e1,g1_,q,end" | dist/build/hmusic/hmusic && /Applications/VLC.app/Contents/MacOS/VLC -I dummy mymusic.mid vlc://quit

Exemple de syntaxe: noire do, blanche do, ronde blanche, do bemol, do diese 

c1
c1-
c1_
lc1
hc1


Import du fichier .mid avec https://musescore.org/fr


