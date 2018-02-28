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
# removetyp
#
# Remove a file type 
# (*.txt, for example)
# Must give argument in double
# quotes, like "*.txt"
#
# Outputs any ls errors to
# file ~/tmp/null
function removetype {
count=$(ls -l $1 2> ~/tmp/null | wc -l)
if [ $count != 0 ]; then
    rm $1
fi
}
###############################

# Clean cut folder:
cd ./cuts/
sh cleancuts.sh
cd ../

# Clean the plot folder:
cd ./hbook/
sh cleanplots.sh
if [ -d ./log/ ]; then
 cd ./log/
 rm *.*
 cd ../
fi
cd ./calc_jpsi/
sh cleanout.sh
cd ../../

# Clean the .tex folder:
cd ./tex/
sh cleanoutput.sh
cd ../

# Clean the event picture output:
removetype "./evtpic/*.tex"
removetype "./evtpic/*.txt"



