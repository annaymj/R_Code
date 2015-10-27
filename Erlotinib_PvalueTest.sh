#!/bin/bash


for file in Erlotinib_aracne_P_*
do
	#get rid of header line
	tail -n +2 $file  > NodeSet_$file
	for i in $(less NodeSet_$file)
		do
		echo $i; done |sort|uniq|grep ^'"' > Genes_$file.txt

	for j in $(less Genes_$file.txt)
		do
		cnt=$(grep $j NodeSet_$file |wc -l)
		echo $j $cnt; done > nodeDegree_$file.txt


done
