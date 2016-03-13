#!/bin/bash

if [ "$1" == "" ]; then
    echo 'enter the number of external particles'
    read next
else
    next=$1
fi

nfin=`expr $next - 2`
njobs=`expr $next - 1`

sed "s/nfin=.*,/nfin=${nfin},/" cparam.inc > tmp.inc
mv tmp.inc cparam.inc

./mk_matrix.sh $next

make clean 2>/dev/null
make 1>/dev/null
rm data/* 2>/dev/null
rm result.dat 2>/dev/null

job=bases_$RANDOM
njobsp1=`expr $njobs + 1`
i=1
while [ $i -ne $njobsp1 ]; do
    bsub -J $job -q h ./run_bases $i 1>/dev/null
    i=`expr $i + 1`
done

monitor_bsub.sh $job $njobs

make get_sum 1>/dev/null
./get_sum $njobs

TDAY=`date +%m.%d_%I:%M:%S`
logfile=log_gg${nfin}g_$TDAY
i=1
while [ $i -ne $njobsp1 ]; do
    chlogfile=log_bases_ch$i
    cat $chlogfile >> $logfile 
    rm $chlogfile
    i=`expr $i + 1`
done
echo 'Summary' >> $logfile
cat result.dat >> $logfile