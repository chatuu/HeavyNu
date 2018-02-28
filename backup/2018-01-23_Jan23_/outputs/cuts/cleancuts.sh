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
# (requires "remove")
#
# Remove all files of given
# type with wildcards in $1.
#
# The argument must be surrounded
# by double quotes! As in:
#  "*.txt"
function cleantype {
for file in $1
do
 remove $file
done
}
#############################

classtag[1]=numucc
classtag[2]=osdimu
classtag[3]=lsdimu
classtag[4]=osmupl

dettag[1]=drft
dettag[2]=othr
dettag[3]=coil
dettag[4]=upst

ncnd[1]=ncand2
ncnd[2]=ncand3
ncnd[3]=ncand4
ncnd[4]=ncnd34

for ii in 1 2 3 4
do
 cd ${classtag[$ii]}
for jj in 1 2 3 4
do
 cd ${dettag[$jj]}
for kk in 1 2 3 4
do
 cd ${ncnd[$kk]}
 cleantype "*.txt"
 cleantype "*.tex"
 cleantype "norm/*.txt"
 cleantype "norm/*.tex"
 cd ..
done
 cd ..
done
 cd ..
done

echo ''
echo 'Cuts Cleaned!'
echo ''
