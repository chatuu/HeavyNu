# This script was made to save time fiddling with the MAKEFILE.
# It simply compiles code with components contained within
# the main directory and sub-directories.  One simply lists
# subdirectories containing code here, and give the number
# of subdirectories.

# List the subdirectories containing code:
codeFolder[1]=code_utilities
# Give the number of subdirectories:
Ndir=1

#################################################################################



#----------------------------------------------------------------------------------
# Function to link (1) or remove (2) all files in a subdirectory to/from the 
# main directory.  The first argument gives the subdirectory name.  The second
# argument gives a choice of copy or remove.
function cpremfolderfiles {
maindir=$PWD
cd ./$1/
for file in *.*
do
  if [ $2 -eq 1 ]; then 
    ln -sf $maindir/$1/$file $maindir/
  elif [ $2 -eq 2 ]; then
    rm -f $maindir/$file
  fi
done
cd $maindir
}
#----------------------------------------------------------------------------------
# Link files, compile, and remove links

rm -f main

for ii in `seq 1 $Ndir`;
do
cpremfolderfiles ${codeFolder[$ii]} 1
done

make
rm -f *.o

for ii in `seq 1 $Ndir`;
do
cpremfolderfiles ${codeFolder[$ii]} 2
done


