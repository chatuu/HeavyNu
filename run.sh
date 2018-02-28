# This script will perform a complete analysis run.
# First the output will be cleaned.  The code will
# then compile and run.  Then plots will be created
# and summary cut files will be formed.  Finally 
# the .tex files will be made.

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

remove term.txt
touch term.txt

remove run_timestamp_start.txt
date > run_timestamp_start.txt

# Clean the cuts folder:
cd outputs/cuts/
sh cleancuts.sh >> ../../term.txt
cd ../../

# Compile the code:
sh make.sh >> term.txt

# Run the code:
./main

# Run the Chisq. MC fit and make Chisq. plots:
cd chisq_iterative/
sh make.x >> ../term.txt
./main >> ../term.txt
cd output/
sh makeplots.sh >> ../../term.txt
cd ../../

# Make plots (directory will clean itself):
cd outputs/hbook/
sh makeplots.sh >> ../../term.txt
cd ../../

# Make summary cut tables:
cd create_summary_tables/
sh make.x >> ../term.txt
./main >> ../term.txt
cd ../

# Form .tex files (will clean themselves):
cd outputs/tex/
sh maketex.sh >> ../../term.txt
cd ../../


remove run_timestamp_stop.txt
date > run_timestamp_stop.txt
