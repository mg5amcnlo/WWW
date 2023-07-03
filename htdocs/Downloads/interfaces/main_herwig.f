C-----------------------------------------------------------------------
*CMZ :-        -21/03/03  10.22.32  by  Peter Richardson
*-- Author :    Peter Richardson
C-----------------------------------------------------------------------
      PROGRAM HWIGPR
C---COMMON BLOCKS ARE INCLUDED AS FILE HERWIG65.INC
      INCLUDE 'HERWIG65.INC'
      INTEGER N
      EXTERNAL HWUDAT
C---PROCESS
      PART1 = 'P'
      PART2 = 'PBAR'
      PBEAM1 = 980.
      PBEAM2 = 980.
      IPROC=-100
C--NUMBER OF EVENTS
      MAXEV = 1000
C---INITIALISE OTHER COMMON BLOCKS
      CALL HWIGIN
C---USER CAN RESET PARAMETERS AT
C   THIS POINT, OTHERWISE DEFAULT
C   VALUES IN HWIGIN WILL BE USED.
      PRVTX=.FALSE.
      MAXER=MAXEV
      MAXPR = 10
C---COMPUTE PARAMETER-DEPENDENT CONSTANTS
      CALL HWUINC
C---CALL HWUSTA TO MAKE ANY PARTICLE STABLE
      CALL HWUSTA('PI0     ')
C---USER'S INITIAL CALCULATIONS
      CALL HWABEG
C---INITIALISE ELEMENTARY PROCESS
      CALL HWEINI
C---LOOP OVER EVENTS
      DO 100 N=1,MAXEV
C---INITIALISE EVENT
      CALL HWUINE
C---GENERATE HARD SUBPROCESS
      CALL HWEPRO
C---GENERATE PARTON CASCADES
      CALL HWBGEN
C---DO HEAVY OBJECT DECAYS
      CALL HWDHOB
C---DO CLUSTER FORMATION
      CALL HWCFOR
C---DO CLUSTER DECAYS
      CALL HWCDEC
C---DO UNSTABLE PARTICLE DECAYS
      CALL HWDHAD
C---DO HEAVY FLAVOUR HADRON DECAYS
      CALL HWDHVY
C---ADD SOFT UNDERLYING EVENT IF NEEDED
      CALL HWMEVT
C---FINISH EVENT
      CALL HWUFNE
C---USER'S EVENT ANALYSIS
      CALL HWANAL
  100 CONTINUE
C---TERMINATE ELEMENTARY PROCESS
      CALL HWEFIN
C---USER'S TERMINAL CALCULATIONS
      CALL HWAEND
      END
C----------------------------------------------------------------------
      SUBROUTINE HWABEG
C     USER'S ROUTINE FOR INITIALIZATION
C----------------------------------------------------------------------
      END
C----------------------------------------------------------------------
      SUBROUTINE HWAEND
C     USER'S ROUTINE FOR TERMINAL CALCULATIONS, HISTOGRAM OUTPUT, ETC
C----------------------------------------------------------------------
      END
C----------------------------------------------------------------------
      SUBROUTINE HWANAL
C     USER'S ROUTINE TO ANALYSE DATA FROM EVENT
C----------------------------------------------------------------------
      END
