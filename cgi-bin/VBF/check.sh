RUNDIR=/home/madgraph/WWW/htdocs/VBF/VBFdata/NNAME/run
NOW="NNOWW"
FILE=$RUNDIR/$NOW.tmp 


/home/condor/release/bin/condor_q | /bin/grep $NOW  > $FILE
while [ -s $FILE ]; do
/home/condor/release/bin/condor_q | /bin/grep $NOW  > $FILE

sleep 10
done


chmod +x $RUNDIR/complete${NOW}-pl

$RUNDIR/complete${NOW}-pl


