paw -b jpsi-width.kumac -w 0

for file in *.ps
do
  ps2pdf $file
  rm $file
done

cp *.pdf /net/www/chris/plots/jpsi-fit/
