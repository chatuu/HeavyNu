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
###############################
# removetype
#
# (requires "remove")
#
# Remove all files of given
# type with wildcards in $1.
function removetype {
for file in $1
do
 remove $file
done
}
#############################
# refreshline
# 
# Refresh a terminal line and 
# output $1
function refreshline {
    echo -en "\e[1A"
    tput el #Erase to end of line
    echo $1
}
###############################


#------------------------------
# Directory names:
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
#------------------------------

echo ''
echo '|=============================|'
echo '|   Cleaning .tex output.     |'
echo '|=============================|'
echo ''

# Remove the links to the .pdf files:
cd ./links/
removetype "*.pdf"
cd ../


# Loop over top directories:
for ityp in 0 1 2 3 
do
cd ${typedir[$ityp]}
    # Loop over detector sections
    for idet in 0 1 2 3
    do
    cd ${detsec[$idet]}
        # Loop over track multiplicity
        for incnd in 2 3 4 5
        do
        cd ${ncand[$incnd]}


remove cohjpsi.pdf
remove cohjpsi.tex


        cd ../
        done
    cd ../
    done
cd ../
done


echo ' Tex output clean!'
echo '-----------------------'
echo ''

