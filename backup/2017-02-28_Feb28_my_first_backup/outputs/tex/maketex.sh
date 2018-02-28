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
function compiletex {
pdflatex -interaction=batchmode $1 >> $2
pdflatex -interaction=batchmode $1 >> $2
echo '------------------' >> $2
}
###############################

remove time_makealltex.sh_start.txt
date > time_makealltex.sh_start.txt


echo ''
echo '|===============================================|'
echo '|===============================================|'
echo '|                                               |'
echo '|        Beginning .tex creation.               |'
echo '|                                               |'
echo '|===============================================|'



#------------------------------
# Latex log filename:
texlog=texlog.log

# Template dir:
templatedir=../../../
# Link dir:
linkdir=../../../links

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

# For naming .pdf link files:
typtag[0]=numucc
typtag[1]=mumu
typtag[2]=lsmumu
typtag[3]=mux

dettag[0]=dc
dettag[1]=other
dettag[2]=coil
dettag[3]=upstream

ncndtag[2]=ncnd2
ncndtag[3]=ncnd3
ncndtag[4]=ncnd4
ncndtag[5]=ncnd34
#------------------------------

# First clean the current output:
sh cleanoutput.sh



# Loop over top directories:
for ityp in 0 1 2 3 
do

echo '|-----------------------------------------------|'
dirnumtop=$(($ityp+1))
echo '|  Working top directory: ./'${typedir[$ityp]}'/ ('$dirnumtop' of 4)'
cd ${typedir[$ityp]}

    # Loop over detector sections
    for idet in 0 1 2 3
    do
    echo '|    Beginning folder: ./'${typedir[$ityp]}'/'${detsec[$idet]}'/'
    cd ${detsec[$idet]}

        # Loop over track multiplicity
        for incnd in 2 3 4 5
        do
        cd ${ncand[$incnd]}

# Make .tex compile log file:
remove $texlog
touch $texlog
# Remove old template:
remove cohjpsi.tex
remove mycommands.sty
# Copy current template:
cp $templatedir/cohjpsi.tex .
cp $templatedir/mycommands.sty .
# Compile the tex file
compiletex cohjpsi $texlog
# Remove other .tex output
removetype "*.lof"
removetype "*.lot"
removetype "*.log"
removetype "*.toc"
removetype "*.out"
removetype "*.aux"
# Make a link to the output .pdf file:
if [ -e cohjpsi.pdf ]; then
ln -sf ../${typedir[$ityp]}/${detsec[$idet]}/${ncand[$incnd]}/cohjpsi.pdf $linkdir/cohjpsi_${typtag[$ityp]}_${dettag[$idet]}_${ncndtag[$incnd]}.pdf 
fi


       
         refreshline '|    ...Folder '${ncand[$incnd]}' complete!'
        cd ../
        done

     refreshline "|    ...Folder ${detsec[$idet]} complete!"
    cd ../
    done

echo '|    Done with top directory: ./'${typedir[$ityp]}'/'
cd ../
done

echo '|-----------------------------------------------|'
echo '|===============================================|'
echo '|                                               |'
echo '|          Finished .tex production.            |'
echo '|                                               |'
echo '|===============================================|'
echo '|===============================================|'
echo ''

remove time_makealltex.sh_end.txt
date > time_makealltex.sh_end.txt
