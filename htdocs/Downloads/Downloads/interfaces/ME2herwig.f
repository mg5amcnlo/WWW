C-----------------------------------------------------------------------
C     Author: Johan Alwall (based on Pythia 6.405 UPINIT/UPEVNT)
C-----------------------------------------------------------------------
      SUBROUTINE UPINIT
C-----------------------------------------------------------------------
C     the Les Houches initialisation routine
C-----------------------------------------------------------------------
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...User process initialization commonblock.
      INTEGER MAXPUP
      PARAMETER (MAXPUP=100)
      INTEGER IDBMUP,PDFGUP,PDFSUP,IDWTUP,NPRUP,LPRUP
      DOUBLE PRECISION EBMUP,XSECUP,XERRUP,XMAXUP
      COMMON/HEPRUP/IDBMUP(2),EBMUP(2),PDFGUP(2),PDFSUP(2),
     &IDWTUP,NPRUP,XSECUP(MAXPUP),XERRUP(MAXPUP),XMAXUP(MAXPUP),
     &LPRUP(MAXPUP)
      SAVE /HEPRUP/
 
C...Logical unit number
      INTEGER NUNIT
      PARAMETER(NUNIT=10)
 
C...Lines to read in assumed never longer than 200 characters. 
      PARAMETER (MAXLEN=200)
      CHARACTER*(MAXLEN) STRING

C...Format for reading lines.
      CHARACTER*6 STRFMT
      STRFMT='(A000)'
      WRITE(STRFMT(3:5),'(I3)') MAXLEN

C...Open the file
      OPEN(UNIT=NUNIT,STATUS='OLD',FILE='unweighted_events.lhe')

C...Loop until finds line beginning with "<init>" or "<init ". 
  100 READ(NUNIT,STRFMT,END=130,ERR=130) STRING
      IBEG=0
  110 IBEG=IBEG+1
C...Allow indentation.
      IF(STRING(IBEG:IBEG).EQ.' '.AND.IBEG.LT.MAXLEN-5) GOTO 110 
      IF(STRING(IBEG:IBEG+5).NE.'<init>'.AND.
     &STRING(IBEG:IBEG+5).NE.'<init ') GOTO 100

C...Read first line of initialization info.
      READ(NUNIT,*,END=130,ERR=130) IDBMUP(1),IDBMUP(2),EBMUP(1),
     &EBMUP(2),PDFGUP(1),PDFGUP(2),PDFSUP(1),PDFSUP(2),IDWTUP,NPRUP

C...Read NPRUP subsequent lines with information on each process.
      DO 120 IPR=1,NPRUP
        READ(NUNIT,*,END=130,ERR=130) XSECUP(IPR),XERRUP(IPR),
     &  XMAXUP(IPR),LPRUP(IPR)
  120 CONTINUE
      RETURN

C...Error exit: give up if initalization does not work.
  130 WRITE(*,*) ' Failed to read LHEF initialization information.'
      WRITE(*,*) ' Event generation will be stopped.'
      STOP  
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE UPEVNT
C-----------------------------------------------------------------------
C     the Les Houches event reading routine
C-----------------------------------------------------------------------
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...User process event common block.
      INTEGER MAXNUP
      PARAMETER (MAXNUP=500)
      INTEGER NUP,IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
      DOUBLE PRECISION XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP
      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,IDUP(MAXNUP),
     &ISTUP(MAXNUP),MOTHUP(2,MAXNUP),ICOLUP(2,MAXNUP),PUP(5,MAXNUP),
     &VTIMUP(MAXNUP),SPINUP(MAXNUP)
      SAVE /HEPEUP/
 
C...Logical unit number
      INTEGER NUNIT
      PARAMETER(NUNIT=10)

C...Lines to read in assumed never longer than 200 characters. 
      PARAMETER (MAXLEN=200)
      CHARACTER*(MAXLEN) STRING

C...Format for reading lines.
      CHARACTER*6 STRFMT
      STRFMT='(A000)'
      WRITE(STRFMT(3:5),'(I3)') MAXLEN

C...Loop until finds line beginning with "<event>" or "<event ". 
  100 READ(NUNIT,STRFMT,END=130,ERR=130) STRING
      IBEG=0
  110 IBEG=IBEG+1
C...Allow indentation.
      IF(STRING(IBEG:IBEG).EQ.' '.AND.IBEG.LT.MAXLEN-6) GOTO 110 
      IF(STRING(IBEG:IBEG+6).NE.'<event>'.AND.
     &STRING(IBEG:IBEG+6).NE.'<event ') GOTO 100

C...Read first line of event info.
      READ(NUNIT,*,END=130,ERR=130) NUP,IDPRUP,XWGTUP,SCALUP,
     &AQEDUP,AQCDUP

C...Read NUP subsequent lines with information on each particle.
      DO 120 I=1,NUP
        READ(NUNIT,*,END=130,ERR=130) IDUP(I),ISTUP(I),
     &  MOTHUP(1,I),MOTHUP(2,I),ICOLUP(1,I),ICOLUP(2,I),
     &  (PUP(J,I),J=1,5),VTIMUP(I),SPINUP(I)
  120 CONTINUE
      RETURN

C...Special when reached end of file or other error.
  130 WRITE(*,*) ' Failed to read LHEF event information.'
      WRITE(*,*) ' Will assume end of file has been reached.'
      CALL HWUGUP

      RETURN
      END
