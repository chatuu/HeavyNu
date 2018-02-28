# Copy output plots to ./plots:

###############################
# remove
#
# Remove a single file without
# an error if the file does
# not exist.
function remove {
if [ -e $1 ]; then
    rm $1
fi
}
###########################

function cleantype {
for file in $1
do
 remove $file
done
}
###############################




# Top Plot directory:
topdir=../plots

# Subdirectory naming conventions:
typedir[0]=numucc
typedir[1]=osdimu
typedir[2]=lsdimu
typedir[3]=osmux

detsec[0]=dc
detsec[1]=other
detsec[2]=coil
detsec[3]=upstr

ncand[2]=ncand2
ncand[3]=ncand3
ncand[4]=ncand4
ncand[5]=ncand34

# Bin Tags:
bintag[1]=150mev
bintag[2]=100mev


# Clean the ./plots/ directory:
cleantype "./plots/*.pdf"
cleantype "./plots/*.eps"


# Copy the plots to the ./plots/ directory:

for ibin in 1 2
do
for ityp in 0 1 2 3 
do
  for idet in 0 1 2 3
  do
    for incnd in 2 3 4 5
    do

    subdir=$topdir/${typedir[$ityp]}/${detsec[$idet]}/${ncand[$incnd]}
    substring=${typedir[$ityp]}_${detsec[$idet]}_${ncand[$incnd]}
    sourcefile=data-fit-${bintag[$ibin]}
    copyfile=data-fit-${substring}_${bintag[$ibin]}
    if [ -e $subdir/$sourcefile.eps ]; then
     cp $subdir/$sourcefile.eps ./plots/$copyfile.eps
     cd ./plots/
     epstopdf $copyfile.eps
     rm $copyfile.eps
     cd ../
    elif [ -e $subdir/$sourcefile.pdf ]; then
     cp $subdir/$sourcefile.pdf ./plots/$copyfile.pdf
    fi

    done
  done
done
done

