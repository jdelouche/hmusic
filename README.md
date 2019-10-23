# hmusic
Midi haskell music

Usage en ligne de commande sur MacOsx avec VLC

cabal build && cat europa.txt | dist/build/hmusic/hmusic && /Applications/VLC.app/Contents/MacOS/VLC -I dummy mymusic.mid vlc://quit

En mode interactif:
echo "c1,e1,g1_,q,end" | dist/build/hmusic/hmusic && /Applications/VLC.app/Contents/MacOS/VLC -I dummy mymusic.mid vlc://quit
