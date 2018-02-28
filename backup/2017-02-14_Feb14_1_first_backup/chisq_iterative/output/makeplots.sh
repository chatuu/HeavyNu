paw -b chisq.kumac -w 0

subdir[1]=mumu2
subdir[2]=mumu3
subdir[3]=mumu4
subdir[4]=mumu34
subdir[5]=mux2
subdir[6]=mux3
subdir[7]=mux4
subdir[8]=mux34

for idir in 1 2 3 4 5 6 7 8
do
  cd ${subdir[$idir]}
  for file in *.eps
  do
    epstopdf $file
    rm $file
  done
  cd ../
done
