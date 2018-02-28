
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
#------------------------------

echo ''
echo '|---------------------------|'
echo '|                           |'
echo '|  Converting .eps to .pdf  |'
echo '|                           |'
echo '|---------------------------|'
echo ''


for ityp in 0 1 2 3 
do

dirnumtop=$(($ityp+1))
echo 'Working top directory: '${typedir[$ityp]}' ('$dirnumtop' of 4)'
cd ${typedir[$ityp]}

    for idet in 0 1 2 3
    do

    cd ${detsec[$idet]}

        echo 'Starting folder: ' ${detsec[$idet]}
        for incnd in 2 3 4 5
        do

         cd ${ncand[$incnd]}
         numeps=$(ls -1 *.eps 2> ~/tmp/null | wc -l)
         count=0

         if [ $numeps -ne 0 ]; then
         for file in *.eps
         do
           count=$((count+1))
           dirnum=$(($incnd-1))
           refreshline 'Working file: ('$count' of '$numeps') '$file
           epstopdf $file
           rm $file
         done
         fi

         cd ../


        done

    refreshline "...Folder ${detsec[$idet]} complete! ($numeps files processed)"
    cd ../

    done
echo 'Done with top directory: '${typedir[$ityp]}
echo '----'
cd ../../

done


