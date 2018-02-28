# Refresh broken links when the file still exists:

for file in "$1"
do
 location=$(readlink $file)
 ln -sf $location ./$file
done
