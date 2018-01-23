#!/bin/bash

echo "ATTENTION: DESCARTES IN ACTION!"

rm log/times_prop5.csv
echo "Property 5"
echo "File, Mode 0, Mode 6, Mode 7, Mode 8, Mode 9" >> log/times_prop5.csv
for f in *.java
do
  echo "Verifying $f"
  ~/Library/Haskell/bin/descartes -p=5 -m=0 $f +RTS -P -RTS
  time0=$(cat descartes.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=5 -m=6 $f +RTS -P -RTS
  time6=$(cat descartes.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=5 -m=7 $f +RTS -P -RTS
  time7=$(cat descartes.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=5 -m=8 $f +RTS -P -RTS
  time8=$(cat descartes.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=5 -m=9 $f +RTS -P -RTS
  time9=$(cat descartes.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  echo "$f, $time0, $time6, $time7, $time8, $time9" >> log/times_prop5.csv
done

rm log/times_prop6.csv
echo "Property 6"
echo "File, Mode 0, Mode 6, Mode 7, Mode 8, Mode 9" >> log/times_prop6.csv
for f in *.java
do
  echo "Verifying $f"
  ~/Library/Haskell/bin/descartes -p=6 -m=0 $f +RTS -P -RTS
  time0=$(cat descartes.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=6 -m=6 $f +RTS -P -RTS
  time6=$(cat descartes.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=6 -m=7 $f +RTS -P -RTS
  time7=$(cat descartes.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=6 -m=8 $f +RTS -P -RTS
  time8=$(cat descartes.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=6 -m=9 $f +RTS -P -RTS
  time9=$(cat descartes.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  echo "$f, $time0, $time6, $time7, $time8, $time9" >> log/times_prop6.csv
done
