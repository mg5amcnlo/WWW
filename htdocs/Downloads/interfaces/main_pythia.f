C   Example program showing how to use the MadEvent-Pythia interface ME2pythia
C
C   Event analysis should be done on the event record using e.g. hbook
C   Do not forget to remove the dummy subroutines UPINIT and UPEVNT
C      from Pythia before compilation
C
C   Note that the standard choice of pdf in MadEvent needs linking to
C   the LHAPDF library
C
C   Written by Johan Alwall, 21/12/2005
C   Version 1.6 30/11/2006
C   E-mail: johan@alwall.net

      PROGRAM MAPYEX

      IMPLICIT NONE

C...Pythia parameters. 
      INTEGER MSTP,MSTI
      DOUBLE PRECISION PARP,PARI
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
C...Make sure PYDATA is linked
      EXTERNAL PYDATA

C...The event record.
      INTEGER N,NPAD,K
      DOUBLE PRECISION P,V
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)

C...Extra commonblock to transfer run info.
      INTEGER LNHIN,LNHOUT,MSCAL
      COMMON/UPPRIV/LNHIN,LNHOUT,MSCAL

C...Local variables      
      INTEGER NEV,IEV
      CHARACTER*5 CGIVE
      CHARACTER*30 CGIVE0

C...Maximum number of events to generate.
      NEV = -1                  ! -1 means all available events

C   initialize HEP logical units
      OPEN (LNHIN, FILE='unweighted_events.lhe', ERR=90 )

C...Set Pythia output to lnhout
      WRITE(CGIVE,'(I5)') LNHOUT
      CGIVE0='MSTU(11)='//CGIVE
      CALL PYGIVE(CGIVE0)

C...Set pi0 stable to trim event listings.
      CALL PYGIVE('MDCY(C111,1)=0')
      CALL PYGIVE('MDCY(C211,1)=0')

C...Initialize with external process.
      CALL PYINIT('USER',' ',' ',0D0)

      IEV=0
C...Event loop
      DO 100 WHILE(IEV.LT.NEV.OR.NEV.LT.0)
         IEV=IEV+1

C...Get event
        CALL PYEVNT

C...If event generation failed, quit loop
        IF(MSTI(51).EQ.1) THEN
          GOTO 110 
        ENDIF

        IF(IEV.LT.10) THEN
          CALL PYLIST(7)
          CALL PYLIST(2)
        ENDIF

 100  CONTINUE

 110  CALL PYSTAT(1)

      CLOSE (LNHIN)

      RETURN
 90   WRITE(*,*) 'Error: Could not open MadEvent event file'
      WRITE(*,*) 'Quitting...'
      END
      
