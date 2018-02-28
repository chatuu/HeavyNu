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

texfile=eventpics.tex

remove $texfile
more header.template > $texfile

echo '\begin{landscape}' >> $texfile

for file in runinfo*.tex
do
echo ' ' >> $texfile
echo '\clearpage\newpage' >> $texfile
echo "\input{$file}" >> $texfile
echo '%%' >> $texfile
done

echo '\end{landscape}' >> $texfile

echo ' ' >> $texfile
echo '\end{document}' >> $texfile

echo ' '
echo "File $texfile created!"
echo ' '
