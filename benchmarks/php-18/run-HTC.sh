#!/bin/bash

echo "ATTENTION: DESCARTES IN ACTION!"

rm log/HTC_prop12.csv
echo "Property 12"
echo "File, Mode 0, Mode 4, Mode 6, Mode 7, Mode 8, Mode 9" >> log/HTC_prop12.csv
for f in *.java
do
	echo "Verifying $f"
  { echo "descartes -p=12 -m=0: "; ~/Library/Haskell/bin/descartes -p=12 -m=0 $f; } &> log/$f-tmp.log
  count0=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  { echo "descartes -p=12 -m=4: "; ~/Library/Haskell/bin/descartes -p=12 -m=4 $f; } &> log/$f-tmp.log
  count4=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  { echo "descartes -p=12 -m=6: "; ~/Library/Haskell/bin/descartes -p=12 -m=6 $f; } &> log/$f-tmp.log
  count6=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  { echo "descartes -p=12 -m=7: "; ~/Library/Haskell/bin/descartes -p=12 -m=7 $f; } &> log/$f-tmp.log
  count7=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  { echo "descartes -p=12 -m=8: "; ~/Library/Haskell/bin/descartes -p=12 -m=8 $f; } &> log/$f-tmp.log
  count8=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  { echo "descartes -p=12 -m=9: "; ~/Library/Haskell/bin/descartes -p=12 -m=9 $f; } &> log/$f-tmp.log
  count9=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  echo "$f, $count0, $count4, $count6, $count7, $count8, $count9" >> log/HTC_prop12.csv
done

rm log/HTC_prop13.csv
echo "Property 13"
echo "File, Mode 0, Mode 4, Mode 6, Mode 7, Mode 8, Mode 9" >> log/HTC_prop13.csv
for f in *.java
do
	echo "Verifying $f"
  { echo "descartes -p=13 -m=0: "; ~/Library/Haskell/bin/descartes -p=13 -m=0 $f; } &> log/$f-tmp.log
  count0=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  { echo "descartes -p=13 -m=4: "; ~/Library/Haskell/bin/descartes -p=13 -m=4 $f; } &> log/$f-tmp.log
  count4=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  { echo "descartes -p=13 -m=6: "; ~/Library/Haskell/bin/descartes -p=13 -m=6 $f; } &> log/$f-tmp.log
  count6=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  { echo "descartes -p=13 -m=7: "; ~/Library/Haskell/bin/descartes -p=13 -m=7 $f; } &> log/$f-tmp.log
  count7=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  { echo "descartes -p=13 -m=8: "; ~/Library/Haskell/bin/descartes -p=13 -m=8 $f; } &> log/$f-tmp.log
  count8=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  { echo "descartes -p=13 -m=9: "; ~/Library/Haskell/bin/descartes -p=13 -m=9 $f; } &> log/$f-tmp.log
  count9=$(cat log/$f-tmp.log | grep -o "Analyser State" | wc -l)
  echo "$f, $count0, $count4, $count6, $count7, $count8, $count9" >> log/HTC_prop13.csv
done
