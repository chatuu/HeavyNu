###########################
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
#############################
function cleandir {
  if [ -d "$1" ]; then
    cd $1
    cleantype "*.pdf"
    cleantype "*.eps"
    cd ..
  else
    echo '  No such directory! ('$1')'
  fi
}
###########################

echo ''
echo '--------------------------------'
echo ' Cleaning plot directories'


typedir[0]=./plots/numucc
typedir[1]=./plots/osdimu
typedir[2]=./plots/lsdimu
typedir[3]=./plots/osmux

detsec[0]=dc
detsec[1]=other
detsec[2]=coil
detsec[3]=upstr

ncand[2]=ncand2
ncand[3]=ncand3
ncand[4]=ncand4
ncand[5]=ncand34


for ityp in 0 1 2 3 
do


cd ${typedir[$ityp]}
    for idet in 0 1 2 3  
    do

    cd ${detsec[$idet]}
        for incnd in 2 3 4 5 
        do

          cleandir ${ncand[$incnd]}

        done
    cd ../
    done
cd ../../
done



echo ' Plot directories are clean!!'
echo '--------------------------------'
echo ' '
