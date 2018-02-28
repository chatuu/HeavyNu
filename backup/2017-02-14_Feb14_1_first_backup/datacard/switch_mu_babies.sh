# This script switches between 1-muon and 2-muon baby ntuples.
# The 1-mu ntuples have 20 times the statistics, and take much
# longer to run.  But they will give us the Mu+<other> sample.

# The first argument is a 1 or a 2, giving the number of muons
# desired in the baby ntuples.

folder=$1mubabies

rm datacoil
rm *baby*
cp $folder/datacoil .
cp $folder/*baby* .

echo ''
echo 'Switched to '$1'-Muon baby ntuples!'
echo ''
