#!/bin/bash

echo "ATTENTION: DESCARTES IN ACTION!"

rm log/times_prop5.csv
echo "Property 5"
echo "File, Mode 0, Mode 4, Mode 6, Mode 7, Mode 8, Mode 9" >> log/times_prop5.csv
for f in *.java
do
	echo "Verifying $f"
  ~/Library/Haskell/bin/descartes -p=5 -m=0 $f +RTS -P -potmp -RTS
  time0=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=5 -m=4 $f +RTS -P -potmp -RTS
  time4=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=5 -m=6 $f +RTS -P -potmp -RTS
  time6=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=5 -m=7 $f +RTS -P -potmp -RTS
  time7=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=5 -m=8 $f +RTS -P -potmp -RTS
  time8=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=5 -m=9 $f +RTS -P -potmp -RTS
  time9=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  echo "$f, $time0, $time4, $time6, $time7, $time8, $time9" >> log/times_prop5.csv
done

rm log/times_prop6.csv
echo "Property 6"
echo "File, Mode 0, Mode 4, Mode 6, Mode 7, Mode 8, Mode 9" >> log/times_prop6.csv
for f in *.java
do
	echo "Verifying $f"
  ~/Library/Haskell/bin/descartes -p=6 -m=0 $f +RTS -P -potmp -RTS
  time0=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=6 -m=4 $f +RTS -P -potmp -RTS
  time4=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=6 -m=6 $f +RTS -P -potmp -RTS
  time6=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=6 -m=7 $f +RTS -P -potmp -RTS
  time7=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=6 -m=8 $f +RTS -P -potmp -RTS
  time8=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=6 -m=9 $f +RTS -P -potmp -RTS
  time9=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  echo "$f, $time0, $time4, $time6, $time7, $time8, $time9" >> log/times_prop6.csv
done
