#!/bin/sh

# List of datacard filenames:
card[1]=amccbabygen
card[2]=amccbabyneg
card[3]=amccbabynua
card[4]=ccbabygen
card[5]=ccbabyneg
card[6]=ccbabynua
card[7]=ncbabygen
card[8]=ncbabyneg
card[9]=ncbabynua
card[10]=neccbabygen
card[11]=neccbabyneg
card[12]=neccbabynua
card[13]=qeccbabygen
card[14]=qeccbabyneg
card[15]=qeccbabynua
card[16]=resbabygen
card[17]=resbabyneg
card[18]=resbabynua
card[19]=datacoil
card[20]=datababy

# Associated directory:
dir[1]=nts/babynt_4trk-1mu/genie_mc/anumucc/
dir[2]=nts/babynt_4trk-1mu/neglib_mc/anumucc/
dir[3]=nts/babynt_4trk-1mu/nuage_mc/anumucc/
dir[4]=nts/babynt_4trk-1mu/genie_mc/ccdis/
dir[5]=nts/babynt_4trk-1mu/neglib_mc/ccdis/
dir[6]=nts/babynt_4trk-1mu/nuage_mc/ccdis/
dir[7]=nts/babynt_4trk-1mu/genie_mc/ncdis/
dir[8]=nts/babynt_4trk-1mu/neglib_mc/ncdis/
dir[9]=nts/babynt_4trk-1mu/nuage_mc/ncdis/
dir[10]=nts/babynt_4trk-1mu/genie_mc/nuecc/
dir[11]=nts/babynt_4trk-1mu/neglib_mc/nuecc/
dir[12]=nts/babynt_4trk-1mu/nuage_mc/nuecc/
dir[13]=nts/babynt_4trk-1mu/genie_mc/qecc/
dir[14]=nts/babynt_4trk-1mu/neglib_mc/qecc/
dir[15]=nts/babynt_4trk-1mu/nuage_mc/qecc/
dir[16]=nts/babynt_4trk-1mu/genie_mc/rescc/
dir[17]=nts/babynt_4trk-1mu/neglib_mc/rescc/
dir[18]=nts/babynt_4trk-1mu/nuage_mc/rescc/
dir[19]=nts/babynt_4trk-1mu/data_coil/
dir[20]=nts/babynt_4trk-1mu/data/


cd ../

for ii in {1..20} ; do
 # Remove the card if it already exists:
 rm -f datacard/${card[$ii]}
 # Put the number of ntuples into the file:
 ls -1 ./${dir[$ii]}*.h | wc -l > datacard/${card[$ii]}
 # List the ntuples:
 ls ./${dir[$ii]}*.h >> datacard/${card[$ii]}
 # Remove any link symbols:
 sed -i 's/@//g' datacard/${card[$ii]}
done

cd datacard/

echo ''
echo ''
echo 'DONE'
echo ''
