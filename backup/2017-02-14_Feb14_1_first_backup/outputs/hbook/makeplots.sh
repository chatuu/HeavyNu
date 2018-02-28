# This script plots using a main kumac file, and then
# converts the output to .pdf
#
# The script also runs the J/Psi calculation script.
#
# Plots are made if any of the following files
# are missing or modified since last plotting:
#
# makeplots.sh
# makeplots.kumac
#
# *.h
#
# ./plots/*/*/*.pdf
#
# Plots are made if this script is not listed in ./log/lastrun.txt
# 


#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
# BEGIN FUNCTIONS

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
# cleantype
#
# Remove all files of type $1.
# Must give argument in double
# quotes, like "*.txt"
function cleantype {
for file in $1
do
 remove $file
done
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
# alldirs
#
# Perform a command ($1) 
# for the sub-directories "dirpath"
# each having a label "dirlabel"
# with additional arguments ($2, $3, $4...)
#----------------
# These variables define the 
# subdirectory structure for
# the plot .pdf files.
#
# ./plots/<typedir>/<detsec>/<ncand>/

# Event topology:
typedir[0]=numucc
typedir[1]=osdimu
typedir[2]=lsdimu
typedir[3]=osmux

# Detector sections:
detsec[0]=dc
detsec[1]=other
detsec[2]=coil
detsec[3]=upstr

# Ncand value:
ncand[2]=ncand2
ncand[3]=ncand3
ncand[4]=ncand4
ncand[5]=ncand34
#----------------
function alldirs {
for ityp in 0 1 2 3 
do
  for idet in 0 1 2 3
  do
    for incnd in 2 3 4 5
    do
      dirpath="./plots/${typedir[$ityp]}/${detsec[$idet]}/${ncand[$incnd]}"
      dirlabel="${typedir[$ityp]}-${detsec[$idet]}-${ncand[$incnd]}"
      $1 $dirpath $dirlabel $2 $3 $4 $5 $6 $7 $8
    done
  done
done
}
###############################
# checkfile
#
# [Modifies variable $DIFFTOT]
#
# This function finds the difference
# between files $1 and $2.
# Any difference in the files is
# assigned to variable $DIFTOT
function checkfile {
if [ -e $1 ] && [ -e $2 ]; then
    DIFF=$(diff $1 $2)
else 
    DIFF='NOFILE'
fi
if [ "$DIFF" != "" ]; then
DIFFTOT=$DIFF
fi
}
###############################
# chkdiff
#
# (requires "checkfile")
#
# [Modifies variable $DIFFTOT]
#
# <<Format related to "lschk">>
#
# This finds the difference 
# between the old file list
# and the new one:
# ./log/pdflist_$2_$3.txt
# ./log/pdflist_$2_$4.txt
# Any difference is assigned
# to the global variable
# $DIFFTOT
#
# The first argument gives the 
# directory path (from alldirs)
# and is not used.
#
function chkdiff {
logone="./log/pdflist_$2_$3.txt"
logtwo="./log/pdflist_$2_$4.txt"
checkfile $logone $logtwo
}
###############################
# lschk
#
# (requires "remove")
#
# <<Format related to "chkdiff">>
#
# If the files $1/*.pdf exist
# this function lists those 
# files into ./log/pdflist_$2_$3.txt
#
# Any errors from ls are exported 
# to the file ./log/lserr
function lschk {
logfile="./log/pdflist_$2_$3.txt"
remove $logfile
counts=$(ls -l $1/*.pdf 2> ./log/lserr | wc -l)
if [ $counts != 0 ]; then
ls -l $1/*.pdf > $logfile
else
echo "NO .pdf FILES" > $logfile
fi
logfile="./log/epslist_$2_$3.txt"
remove $logfile
counts=$(ls -l $1/*.eps 2> ./log/lserr | wc -l)
if [ $counts != 0 ]; then
ls -l $1/*.eps > $logfile
else
echo "NO .eps FILES" > $logfile
fi
}
###############################
# writechkfiles
#
# This function writes file lists to
# to various files.
# The file naming convention tag
# is given in $1
function writechkfiles {
# Output current histogram file dates (check file)
remove ./log/hfile-dates-$1.txt
ls -l *.h > ./log/hfile-dates-$1.txt
# makeplots.sh file dates (check file)
remove ./log/makeplotssh-dates-$1.txt
ls -l makeplots.sh > ./log/makeplotssh-dates-$1.txt
# makeplots.kumac file dates (check file)
remove ./log/makeplotskumac-dates-$1.txt
ls -l makeplots.kumac > ./log/makeplotskumac-dates-$1.txt
# Output plot file lists:
alldirs lschk $1
# Normalization file list:
normlog=./log/norms-$1.txt
remove $normlog
touch $normlog
for files in normlist*.txt
do
 more $files >> $normlog
done
}
###############################


#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
# BEGIN SCRIPT


remove ./log/time_makeplots.sh_start.txt
date > ./log/time_makeplots.sh_start.txt

echo ''
echo '|================================================================|'
echo '|==      Starting analysis plotting script: makeplots.sh       ==|'
echo '|----------                                            ----------|'
echo ''

#=============================
# Make check files:
writechkfiles "chk"
#=============================

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Check if histogram files have been updated:
DIFFTOT=""
checkfile "log/norms-chk.txt" "log/norms-dir.txt"
checkfile "log/hfile-dates-chk.txt" "log/hfile-dates-dir.txt"
checkfile "log/makeplotssh-dates-chk.txt" "log/makeplotssh-dates-dir.txt"
checkfile "log/makeplotskumac-dates-chk.txt" "log/makeplotskumac-dates-dir.txt"
# Check if all plot files are there:
alldirs chkdiff "chk" "dir"
# Check if this was the last script to run:
if [ -e log/lastrun.txt ]; then
lastscript=$(sed '1q;d' log/lastrun.txt)
else
lastscript='NO FILE!'
fi
if [ "$lastscript" != "makeplots.sh" ]; then
 DIFFTOT="WRONG SCRIPT"
fi
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Remake plots if anything has changed since the last plotting:
if [ "$DIFFTOT" != "" ]; then

# Clear current plots
sh cleanplots.sh

# Make J/Psi signal calculation plots and tables:
cd ./calc_jpsi/
sh makeplots.sh
cd ../

# Make kinematic plots:
echo ''
echo '|------------------------------------------------------|'
echo '|------------------------------------------------------|'
echo ' Running PAW:'
paw -b makeplots.kumac -w 0
echo '|------------------------------------------------------|'
echo '|------------------------------------------------------|'
echo ''

# Convert .eps files to .pdf:
sh makepdfs.sh

#=============================
# Make most recent files:
writechkfiles "dir"
#=============================

# List this script as the last one run:
remove log/lastrun.txt
echo makeplots.sh > log/lastrun.txt

echo ' '
echo '|----------                                            ----------|'
echo '|                    Analysis plots created!                     |'
echo '|================================================================|'
echo ''

# Plots already up-to-date:
else
echo '               Skipping plotting, already up-to-date.             '
echo ''
echo '|----------                                            ----------|'
echo '|                        END makeplots.sh                        |'
echo '|================================================================|'
echo ''
fi

remove ./log/time_makeplots.sh_end.txt
date > ./log/time_makeplots.sh_end.txt
