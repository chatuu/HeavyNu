date3=$(date +"%b%d_%y%n")
currentdir=$date3'_'$1
maindir=./backup
mkdir $maindir/$currentdir

cp -d ./*.* $maindir/$currentdir
cp ./Makefile $maindir/$currentdir
cp ./main $maindir/$currentdir
cp -rd ./output/ $maindir/$currentdir
cp -rd ./hists/ $maindir/$currentdir

echo 'Backed up to: ' $maindir/$currentdir
