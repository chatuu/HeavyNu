# This script simply prepares the subdirectories before
# copying .tex templates.  It does the following:
#
# 1) Makes the subdirectory structure
# 2) Links the "figs" directory
# 3) Links the "cuts" directory
# 4) Links the zeroth-norm .tex file
# 5) Creates "title.tex"
# 6) Links the signal calculation tables to "sigcalc-*.tex"
# 7) Links the MC Chisq. Calculation tables and Chisq. plots


################################################
# makedir
#
# Make a directory if it doesn't already exist:
#
function makedir {
if [ ! -d "$1" ]; then
 mkdir "$1"
fi
}
################################################
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




#------------------------------
topplotdir=../../../../hbook/plots
topcutdir=../../../../cuts
zerofile=../../../../zeroth_norms.tex
topcalcdir=../../../../hbook/calc_jpsi/tables
topchidir=../../../../../chisq_iterative/output

# Directory names (same structure as plotting sub-directories):
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

# Directory names for cut sub-directories:
classtag[0]=numucc
classtag[1]=osdimu
classtag[2]=lsdimu
classtag[3]=osmupl

dettag[0]=drft
dettag[1]=othr
dettag[2]=coil
dettag[3]=upst

ncnd[2]=ncand2
ncnd[3]=ncand3
ncnd[4]=ncand4
ncnd[5]=ncnd34
#------------------------------
# Titles for each subdirectory:
titleclass[0]='$\boldsymbol{\nu_\mu}$ \textbf{CC}'
titleclass[1]='\textbf{OS}-$\boldsymbol{\mu\mu}$'
titleclass[2]='\textbf{LS}-$\boldsymbol{\mu\mu}$'
titleclass[3]='\textbf{OS}-$\boldsymbol{\mu+}$\textbf{X}'
#-----
titledet[0]='\textbf{DC}'
titledet[1]='\textbf{Other}'
titledet[2]='\textbf{Coil}'
titledet[3]='\textbf{Upstr}'
#-----
titlencnd[2]='\textbf{2-track}'
titlencnd[3]='\textbf{3-track}'
titlencnd[4]='\textbf{4-track}'
titlencnd[5]='\textbf{34-track}'
#------------------------------
# Output directory names for
# MC chisq. normalizations:
# (index 1 is used for the default
#  for other topologies)
chisqdir[1]="mumu2"
chisqdir[2]="mumu3"
chisqdir[3]="mumu4"
chisqdir[4]="mumu34"
chisqdir[5]="mux2"
chisqdir[6]="mux3"
chisqdir[7]="mux4"
chisqdir[8]="mux34"
#------------------------------



# Loop over the top directories (event classification):
for ityp in 0 1 2 3
do

  makedir ${typedir[$ityp]}
  cd ${typedir[$ityp]}
  # Loop over the second level of directories (detector section):
  for idet in 0 1 2 3
  do
    makedir ${detsec[$idet]}
    cd ${detsec[$idet]}
    # Loop over ncand values:
    for incnd in 2 3 4 5
    do
     makedir ${ncand[$incnd]}
     cd ${ncand[$incnd]}

#---------------------------------------------------------------------
# Current subdirectory (for .tex and plots):
currdir="${typedir[$ityp]}/${detsec[$idet]}/${ncand[$incnd]}"
# Current subdirectory for cuts:
cutdir="${classtag[$ityp]}/${dettag[$idet]}/${ncnd[$incnd]}"
# Fit table name:
fitname="sigcalc_${classtag[$ityp]}_${dettag[$idet]}_${ncnd[$incnd]}"

#----------
# Link main plot directory:
     remove figs
     ln -sf $topplotdir/$currdir figs

#----------
# Link normalized cut directory:
     remove cuts
     ln -sf $topcutdir/$cutdir/norm cuts

#----------
# Link zeroth-norm file:
     remove $(basename $zerofile)
     ln -sf $zerofile .

#----------
     #00000000000000000000000000000000000
     # Make title files:
     remove title.tex
     touch title.tex
     echo '\title{CohJ/$\psi$ Analysis '"(${titleclass[$ityp]})(${titledet[$idet]})(${titlencnd[$incnd]})}" >> title.tex
     echo '\author{Chris Kullenberg}' >> title.tex
     echo '\date{\today}' >> title.tex
     #00000000000000000000000000000000000

#----------
# Link signal calculation table:
     remove sigcalc-100mev.tex
     remove sigcalc-150mev.tex
     ln -sf $topcalcdir/${fitname}_100mev.tex sigcalc-100mev.tex 
     ln -sf $topcalcdir/${fitname}_150mev.tex sigcalc-150mev.tex 

#----------
# Link the MC chisq. calculation tables and plots:
     # For MuMu:
     if [ $ityp -eq 1 ]; then
         chisqtab=${chisqdir[$incnd-1]}
     # For Mu-X:
     elif [ $ityp -eq 3 ]; then
         chisqtab=${chisqdir[$incnd+3]}
     # For Others (default is MuMu ncand2):
     else
      chisqtab=${chisqdir[1]}
     fi
     remove chisq_jpsi.tex
     remove chisq_cohpip.tex
     remove chisq_ccdis.tex
     ln -sf $topchidir/$chisqtab/chisq_jpsi.tex .
     ln -sf $topchidir/$chisqtab/chisq_cohpip.tex .
     ln -sf $topchidir/$chisqtab/chisq_ccdis.tex .
     remove chisq_jpsi.pdf
     remove chisq_cohpip.pdf
     remove chisq_ccdis.pdf
     ln -sf $topchidir/$chisqtab/chisq_jpsi.pdf chisq-jpsi.pdf
     ln -sf $topchidir/$chisqtab/chisq_cohpip.pdf chisq-cohpip.pdf
     ln -sf $topchidir/$chisqtab/chisq_ccdis.pdf chisq-ccdis.pdf


 
#---------------------------------------------------------------------
     cd ../
    done
    cd ../
  done
  cd ../
done
