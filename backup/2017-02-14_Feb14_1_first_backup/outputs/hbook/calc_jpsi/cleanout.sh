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
###########################

function cleantype {
for file in $1
do
 remove $file
done
}
###############################


cleantype "./par/par_*.txt"
cleantype "./tables/*.tex"
cleantype "./norms/*.txt"
cleantype "./plots/*.pdf"
