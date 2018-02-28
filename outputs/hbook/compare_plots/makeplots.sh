paw -b compare_sig-bkg.kumac -w 0
for file in *.ps
do
 ps2pdf $file
 rm $file
done
