#date3=$(date +"%b%d_%y%n")
date3=$(date +"%F_%b%d")
currentdir=$date3'_'$1
maindir=./backup
mkdir $maindir/$currentdir

cp -d ./*.* $maindir/$currentdir
cp ./Makefile $maindir/$currentdir
cp ./main $maindir/$currentdir
cp -rd ./outputs/ $maindir/$currentdir
cp -rd ./datacard/ $maindir/$currentdir
cp -rd ./nts/ $maindir/$currentdir
cp -rd ./code_utilities/ $maindir/$currentdir
cp -rd ./chisq_iterative/ $maindir/$currentdir
cp -rd ./create_summary_tables/ $maindir/$currentdir
cp -rd ./corr/ $maindir/$currentdir

echo 'Backed up to: ' $maindir/$currentdir

