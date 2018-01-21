#!/bin/bash

echo "ATTENTION: DESCARTES IN ACTION!"

rm log/times_prop12.csv
echo "Property 12"
echo "File, Mode 0, Mode 6, Mode 7, Mode 8, Mode 9" >> log/times_prop12.csv
for f in *.java
do
  echo "Verifying $f"
  ~/Library/Haskell/bin/descartes -p=12 -m=0 $f +RTS -P -potmp -RTS
  time0=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=12 -m=6 $f +RTS -P -potmp -RTS
  time6=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=12 -m=7 $f +RTS -P -potmp -RTS
  time7=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=12 -m=8 $f +RTS -P -potmp -RTS
  time8=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=12 -m=9 $f +RTS -P -potmp -RTS
  time9=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  echo "$f, $time0, $time6, $time7, $time8, $time9" >> log/times_prop12.csv
done

rm log/times_prop13.csv
echo "Property 13"
echo "File, Mode 0, Mode 6, Mode 7, Mode 8, Mode 9" >> log/times_prop13.csv
for f in *.java
do
  echo "Verifying $f"
  ~/Library/Haskell/bin/descartes -p=13 -m=0 $f +RTS -P -potmp -RTS
  time0=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=13 -m=6 $f +RTS -P -potmp -RTS
  time6=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=13 -m=7 $f +RTS -P -potmp -RTS
  time7=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=13 -m=8 $f +RTS -P -potmp -RTS
  time8=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  ~/Library/Haskell/bin/descartes -p=13 -m=9 $f +RTS -P -potmp -RTS
  time9=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  echo "$f, $time0, $time6, $time7, $time8, $time9" >> log/times_prop13.csv
done

rm log/times_prop14.csv
echo "Property 14"
echo "File, Mode 0, Mode 6, Mode 7, Mode 8, Mode 9" >> log/times_prop14.csv
for f in *.java
do
  echo "Verifying $f"
  gtimeout 60m ~/Library/Haskell/bin/descartes -p=14 -m=0 $f +RTS -P -potmp -RTS
  time0=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  gtimeout 60m ~/Library/Haskell/bin/descartes -p=14 -m=6 $f +RTS -P -potmp -RTS
  time6=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  gtimeout 60m ~/Library/Haskell/bin/descartes -p=14 -m=7 $f +RTS -P -potmp -RTS
  time7=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  gtimeout 60m ~/Library/Haskell/bin/descartes -p=14 -m=8 $f +RTS -P -potmp -RTS
  time8=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  gtimeout 60m ~/Library/Haskell/bin/descartes -p=14 -m=9 $f +RTS -P -potmp -RTS
  time9=$(cat tmp.prof | grep "total time" | grep -o "[0-9]*\.[0-9]*")
  echo "$f, $time0, $time6, $time7, $time8, $time9" >> log/times_prop14.csv
done
