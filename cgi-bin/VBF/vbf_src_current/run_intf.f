
make

SCH=0
ICOLL=1
MH=100
CNT=0
PDF=0
imuf=0
imuf=0

OUTNAME="outintf_"

PDFLO="MSTW2008lo68cl.LHgrid"
PDFNLO="MSTW2008nlo68cl.LHgrid"
PDFNNLO="MSTW2008nnlo68cl.LHgrid"

for SCALES in 0 1;
do

for SQRTS in  7000. 10000. 14000.;
do
  for MH in 120. 140. 160. 180. 200. 250. 300. 400. 500.;
  do
 echo " $CNT $SCH $ICOLL $SQRTS $MH $SCALES $imuf $imur "  
       echo "#! /bin/bash -l
echo \" $CNT $SCH $ICOLL $SQRTS $MH $SCALES $imuf $imur $OUTNAME${CNT}.dat $PDFLO $PDFNLO $PDFNNLO $PDF\" | /home/fynu/mzaro/vbf_30/makeruncard
 /home/fynu/mzaro/vbf_30/vbf_intf > log_intf${CNT}.dat
rm run_card.dat
" >run_intf${CNT}.sh
        chmod +x run_intf${CNT}.sh
	sed "s/xyz/run_intf${CNT}.sh/" condor.cmd > run_intf${CNT}.cmd
        condor_submit run_intf${CNT}.cmd
	let CNT=$CNT+1
  done
done
done
