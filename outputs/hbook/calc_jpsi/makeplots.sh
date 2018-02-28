# This script calculates the J/Psi signal in various
# event toplogies and outputs tables of the signal
# counts (for 3 different ranges) and a single plot
# showing the calculated signal and background fit.


echo ''
echo '|=====================================================|'
echo '|===                                               ===|'
echo '|===  Beginning calculation of the J/Psi Signal    ===|'
echo '|===                                               ===|'
echo '|=====================================================|'
echo ''





# Remove old parameter files, tables and plots:
# (do not want to keep old output for histograms 
#  that may not currently be available)
sh cleanout.sh

# Run fitting .kumac to get parameter files:
paw -b fitjpsi.kumac -w 0

# Calculate the number of signal events and 
# make signal histograms in output.h
./main

# Make single page plots including:
# 1) The full mass
# 2) The background fit
# 3) The calculated signal
paw -b makeplots.kumac -w 0

# Copy the output plots to ./plots/
sh copyplots.sh



echo ''
echo '|------------------------------------|'
echo '|--                                --|'
echo '|-- Finished with J/Psi sig. calc. --|'
echo '|--                                --|'
echo '|------------------------------------|'
echo ''






