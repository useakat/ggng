#!/bin/bash

cp matrix_temp.f matrix.f

sed "/insert_amp/a GG1" matrix.f > tmp.f
sed "s:GG1:     \&    GC_4/DSQRT(2D0),JAMP):" tmp.f > matrix.f
i=$1
while [ $i -ne 4 ]; do
    sed "/insert_amp/a GG1" matrix.f > tmp.f
    sed "s/GG1/     \&    W(1,IP($i)),/" tmp.f > matrix.f
    i=`expr $i - 1`
done
sed "/insert_amp/a GG1" matrix.f > tmp.f
sed "s/GG1/      CALL GLUON$1_0(W(1,IP(1)),W(1,IP(2)),W(1,IP(3)),W(1,IP(4)),/" tmp.f > matrix.f

i=$1
while [ $i -ne 0 ]; do
    if [ $i -eq 1 -o $i -eq 2 ]; then
	sed "/insert_amp/a GG1" matrix.f > tmp.f
	sed "s/GG1/      CALL VXXXXX(P(0,$i),ZERO,NHEL($i),-1*IC($i),W(1,$i))/" tmp.f > matrix.f
    else
	sed "/insert_amp/a GG1" matrix.f > tmp.f
	sed "s/GG1/      CALL VXXXXX(P(0,$i),ZERO,NHEL($i),+1*IC($i),W(1,$i))/" tmp.f > matrix.f
    fi
    i=`expr $i - 1`
done

sed "/insert_amp/d" matrix.f > tmp.f
mv tmp.f matrix.f