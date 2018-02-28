# This script opens all raw datacards and concatenates the cards [and changes
# the number of entries] to mix MC generation types.  The general index 
# convention:
#                1 = NUAGE      
#                2 = NEGLIB      
#                3 = GENIE       
#                4 = NUAGE  + NEGLIB  
#                5 = NUAGE  + GENIE   
#                6 = NEGLIB + GENIE   
#                7 = NUAGE  + NEGLIB  + GENIE     

function remove {
if [ -e $1 ]; then
    rm $1
fi
}

# Full NT file tags for MC types that require mixing:
dstr[1]='ccfull'
dstr[2]='ncfull'
dstr[3]='anmcc'
dstr[4]='qecc'
dstr[5]='nuecc'
dstr[6]='anecc'
dstr[7]='anmnc'
dstr[8]='rescc'
# Baby NT file tags for MC types that require mixing:
dstr[9]='ccbaby'
dstr[10]='ncbaby'
dstr[11]='amccbaby'
dstr[12]='qeccbaby'
dstr[13]='neccbaby'
dstr[14]='resbaby'

# Strings giving MC Generator tags:
dtyp[1]='nua' #!--> NUAGE  
dtyp[2]='neg' #!--> NEGLIB 
dtyp[3]='gen' #!--> GENIE  
  #Output:
dtyp[4]='nn'  #!--> NUAGE + NEGLIB  
dtyp[5]='ng'  #!--> NUAGE + GENIE  
dtyp[6]='gn'  #!--> NEGLIB + GENIE  
dtyp[7]='nng' #!--> NUAGE + NEGLIB + GENIE 


# Mix card contents for each MC type
for ii in {1..14} ; do

  # Set input and output card names:
  for jj in {1..7} ; do
    cardname[$jj]=${dstr[$ii]}${dtyp[$jj]}
  done

  # Loop over mix types
  for jj in {4..7} ; do
      
         outfile=${cardname[$ii,$jj]}
         # Remove old file, if it exists:
         remove $outfile
         # Create new file
         echo 0 > $outfile

         # Set combination switches: 
         if [ $jj = 4 ] ; then
           combo[1]=1
           combo[2]=1
           combo[3]=0
         elif [ $jj = 5 ] ; then
           combo[1]=1
           combo[2]=0
           combo[3]=1
         elif [ $jj = 6 ] ; then
           combo[1]=0
           combo[2]=1
           combo[3]=1
         elif [ $jj = 7 ] ; then
           combo[1]=1
           combo[2]=1
           combo[3]=1
         fi
     
     # Loop over included types
     for kk in {1..3} ; do
        if [ "${combo[$kk]}" = "1" ]; then
         infile=${cardname[$ii,$kk]}
         # Get the number of ntuples listed in each file:
         outnum=$(head -n 1 $outfile)
          innum=$(head -n 1 $infile)
         # Add the number of ntuples
         totnt=$(($outnum + $innum))
         # Replace the first line in the output file:
         sed -i "1s/^.*$/$totnt/" $outfile
         # Append the input file list to the output file (omit the first line)
         tail --lines=$innum $infile >> $outfile
        fi
     done
  done
done
