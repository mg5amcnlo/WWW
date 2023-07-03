c-----this file contains the scale dependent parts of the coefficient
c----- functions, as called from libstructf_xx, xx>15

c------------------------------------------------------------------
c------LO----------------------------------------------------------
c------------------------------------------------------------------
c-----the LO splitting functions as reported in the Ellis, Stirling
c----- and Webber book

      double precision function p0qqa(z)
      implicit none
      double precision z
      p0qqa=-4d0/3d0*(1d0+z)*2d0
      return
      end

      double precision function p0qqb(z)
      implicit none
      double precision z
      p0qqb=4d0/3d0 *2d0/(1d0-z)*2d0
      return
      end

      double precision function p0qqc(z)
      implicit none
      double precision z
      p0qqc=( 4d0/3d0 *3d0/2d0+ 8d0/3d0*dlog(1d0-z))*2d0
      return
      end

      double precision function p0qga(z)
      implicit none
      double precision z
      p0qga=1d0/2d0*(z**2+(1d0-z)**2)*2d0
      return
      end

      double precision function p0gqa(z)
      implicit none
      double precision z
      p0gqa=4d0/3d0*(1d0+(1d0-z)**2)/z*2d0
      return
      end

      double precision function p0gga(z)
      implicit none
      double precision z
      p0gga=2d0*3d0*(-1d0+(1d0-z)/z+z*(1d0-z))*2d0
      return
      end

      double precision function p0ggb(z)
      implicit none
      double precision z
      p0ggb=2d0*3d0/(1-z)*2d0
      return
      end

      double precision function p0ggc(z)
      implicit none
      double precision z
      integer nf
      nf=5
      p0ggc=((11d0*3d0-2d0*nf)/6d0 +2d0/3d0 *dlog(1d0-z))*2d0
      return
      end

c-----compute the hplogs and put them in a common block
      subroutine sethpl(x)
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2)

      common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
 

       CALL HPLOG (X, NW, HC1,HC2,HC3,HC4, HR1,HR2,HR3,HR4,
     ,            HI1,HI2,HI3,HI4, N1, N2) 

      return
      end

c-----same as above, but in a different common block
c      to be used for the C part (which is evaluated in x)
      subroutine sethplc(x)
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2)

      common/hplsc/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
 

       CALL HPLOG (X, NW, HC1,HC2,HC3,HC4, HR1,HR2,HR3,HR4,
     ,            HI1,HI2,HI3,HI4, N1, N2) 

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*
*
* ..The  2-loop MS(bar) non-singlet splitting functions P_NS^(2) 
*    for the evolution of unpolarized partons densities, mu_r = mu_f.
*    The expansion parameter is alpha_s/(4 pi).
* 
* ..The distributions (in the mathematical sense) are given as in eq.
*    (B.26) of Floratos, Kounnas, Lacaze: Nucl. Phys. B192 (1981) 417.
*    The name-endings A, B, and C of the functions below correspond to 
*    the kernel superscripts [2], [3], and [1] in that equation.
*
* ..The code uses the package of Gehrmann and Remiddi for the harmonic
*    polylogarithms published in hep-ph/0107173 = CPC 141 (2001) 296.
*
*
* =====================================================================
*
*
* ..This is the regular 2-loop piece for P_NS^+. 
*
       FUNCTION X1NSPA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
**
* ...The harmonic polylogs up to weight 4 by Gehrmann and Remiddi
*

       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
* ..The soft coefficient for use in X2NSPB and X2NSPC
*
       COMMON / P1SOFT / A2
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)
*
* ...The splitting function in terms of the harmonic polylogs
*    (without the delta(1-x) part, but with the soft contribution)
*
      gqq1 =
     &  + cf*ca * ( 34.D0/9.D0 - 302.D0/9.D0*x + 268.D0/9.D0*dm + 8.D0*
     &    z2*x + 8.D0*z2*dp - 8.D0*z2*dm - 22.D0/3.D0*Hr1(0) - 22.D0/3.D
     &    0*Hr1(0)*x + 44.D0/3.D0*Hr1(0)*dm - 8.D0*Hr2(-1,0) + 8.D0*
     &    Hr2(-1,0)*x + 16.D0*Hr2(-1,0)*dp - 8.D0*Hr2(0,0)*x - 8.D0*
     &    Hr2(0,0)*dp + 8.D0*Hr2(0,0)*dm )
      gqq1 = gqq1 + cf**2 * (  - 4.D0 + 4.D0*x + 8.D0*z2 - 8.D0*z2*x -
     &    16.D0*z2*dp + 8.D0*Hr1(0) - 12.D0*Hr1(0)*dm + 16.D0*Hr2(-1,0)
     &     - 16.D0*Hr2(-1,0)*x - 32.D0*Hr2(-1,0)*dp - 12.D0*Hr2(0,0) +
     &    4.D0*Hr2(0,0)*x + 16.D0*Hr2(0,0)*dp - 8.D0*Hr2(0,1) - 8.D0*
     &    Hr2(0,1)*x + 16.D0*Hr2(0,1)*dm - 8.D0*Hr2(1,0) - 8.D0*Hr2(1,0
     &    )*x + 16.D0*Hr2(1,0)*dm )
      gqq1 = gqq1 + nf*cf * (  - 4.D0/9.D0 + 44.D0/9.D0*x - 40.D0/9.D0*
     &    dm + 4.D0/3.D0*Hr1(0) + 4.D0/3.D0*Hr1(0)*x - 8.D0/3.D0*Hr1(0)
     &    *dm )
*
* ...The soft (`+'-distribution) part of the splitting function
*
       A2 = - 40.D0/9.D0*cf*nf + 268.D0/9.D0*ca*cf - 8.D0*z2*ca*cf
*
       GQQ1L = DM * A2
*
* ...The regular piece of the coefficient function
*
       X1NSPA = GQQ1 - GQQ1L
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the regular 2-loop piece for P_NS^-. 
*
       FUNCTION X1NSMA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
* ..The soft coefficient for use in X2NSPB and X2NSPC
*
       COMMON / P1SOFT / A2
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)
*
* ...The harmonic polylogs up to weight 4 by Gehrmann and Remiddi
*
*
* ...The splitting function in terms of the harmonic polylogs
*    (without the delta(1-x) part, but with the soft contribution)
*
      gqq1 =
     &  + cf*ca * ( 178.D0/9.D0 - 446.D0/9.D0*x + 268.D0/9.D0*dm + 8.D0
     &    *z2 - 8.D0*z2*dp - 8.D0*z2*dm + 2.D0/3.D0*Hr1(0) + 2.D0/3.D0*
     &    Hr1(0)*x + 44.D0/3.D0*Hr1(0)*dm + 8.D0*Hr2(-1,0) - 8.D0*Hr2(
     &    -1,0)*x - 16.D0*Hr2(-1,0)*dp - 8.D0*Hr2(0,0) + 8.D0*Hr2(0,0)*
     &    dp + 8.D0*Hr2(0,0)*dm )
      gqq1 = gqq1 + cf**2 * (  - 36.D0 + 36.D0*x - 8.D0*z2 + 8.D0*z2*x
     &     + 16.D0*z2*dp - 8.D0*Hr1(0) - 16.D0*Hr1(0)*x - 12.D0*Hr1(0)*
     &    dm - 16.D0*Hr2(-1,0) + 16.D0*Hr2(-1,0)*x + 32.D0*Hr2(-1,0)*dp
     &     + 4.D0*Hr2(0,0) - 12.D0*Hr2(0,0)*x - 16.D0*Hr2(0,0)*dp - 8.D0
     &    *Hr2(0,1) - 8.D0*Hr2(0,1)*x + 16.D0*Hr2(0,1)*dm - 8.D0*Hr2(1,
     &    0) - 8.D0*Hr2(1,0)*x + 16.D0*Hr2(1,0)*dm )
      gqq1 = gqq1 + nf*cf * (  - 4.D0/9.D0 + 44.D0/9.D0*x - 40.D0/9.D0*
     &    dm + 4.D0/3.D0*Hr1(0) + 4.D0/3.D0*Hr1(0)*x - 8.D0/3.D0*Hr1(0)
     &    *dm )
*
* ...The soft (`+'-distribution) part of the splitting function
*
       A2 = - 40.D0/9.D0*cf*nf + 268.D0/9.D0*ca*cf - 8.D0*z2*ca*cf
*
       GQQ1L = DM * A2
*
* ...The regular piece of the coefficient function
*
       X1NSMA = GQQ1 - GQQ1L
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the singular (soft) piece.
*
       FUNCTION X1NSB (Y)
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
*
       COMMON / P1SOFT / A2
       nf=5
*
       X1NSB  = A2/(1.D0-Y)
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the 'local' piece.
*
       FUNCTION X1NSC (Y)
*
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF, NF2
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
       COMMON / P1SOFT / A2
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...The coefficient of delta(1-x)
*
       P1DELT = 
     &     - 1.D0/3.D0*cf*nf
     &     + 3.D0/2.D0*cf**2
     &     + 17.D0/6.D0*ca*cf
     &     + 24.D0*z3*cf**2
     &     - 12.D0*z3*ca*cf
     &     - 8.D0/3.D0*z2*cf*nf
     &     - 12.D0*z2*cf**2
     &     + 44.D0/3.D0*z2*ca*cf
*
       X1NSC = LOG (1.D0-Y) * A2 + P1DELT
*
       RETURN
       END

* ..This is the regular 1-loop piece. 
*
       FUNCTION X0NSA (X)
       IMPLICIT REAL*8 (A - Z)
*
       CF = 4./3.D0
       X0NSA = - 2.*CF * (1.+ X)
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the singular (soft) piece.
*
       FUNCTION X0NSB (Y)
       IMPLICIT REAL*8 (A - Z)
*
       CF = 4./3.D0
       X0NSB = 4.*CF/(1.D0-Y)
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the 'local' piece.
*
       FUNCTION X0NSC (Y)
       IMPLICIT REAL*8 (A - Z)
*
       CF = 4./3.D0
       X0NSC = 4.*CF * LOG (1.D0-Y) + 3.*CF
*
       RETURN
       END
*
* =================================================================av==


*
* ..The 1- and 2-loop MS(bar) singlet splitting functions  P_ij^(2) 
*    for the evolution of unpolarized partons densities, mu_r = mu_f.
*    The expansion parameter is alpha_s/(4 pi).
* 
* ..The distributions (in the mathematical sense) are given as in eq.
*    (B.26) of Floratos, Kounnas, Lacaze: Nucl. Phys. B192 (1981) 417.
*    The name-endings A, B, and C of the functions below correspond to 
*    the kernel superscripts [2], [3], and [1] in that equation.
*
* ..The code uses the package of Gehrmann and Remiddi for the harmonic
*    polylogarithms published in hep-ph/0107173 = CPC 141 (2001) 296.
*
*
* =====================================================================
*
*
* ..The 2-loop pure-singlet splitting functions P_ps^(1)
*
       FUNCTION X1PSA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 )
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2),
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2)
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2),
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2)
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2),
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2)
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors and abbreviation
*
       CF = 4./3.D0
       DX = 1.D0/X
*
* ...The harmonic polylogs up to weight 4 by Gehrmann and Remiddi
*
*
* ...The splitting function in terms of the harmonic polylogs
*
       X1PSA =
     &  + cf * (  - 8.D0 + 24.D0*x - 224.D0/9.D0*x**2 + 80.D0/9.D0*
     &    dx + 4.D0*Hr1(0) + 20.D0*Hr1(0)*x + 32.D0/3.D0*Hr1(0)*x**2 -
     &    8.D0*Hr2(0,0) - 8.D0*Hr2(0,0)*x )
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..The 2-loop gluon->quark splitting functions P_qg^(1)
*
       FUNCTION X1QGA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 )
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2),
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2)
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2),
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2)
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2),
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2)
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors and abbreviation
*
       CF = 4./3.D0
       CA = 3.D0
       DX = 1.D0/X
*
* ...The harmonic polylogs up to weight 
*
*
* ...The splitting function in terms of the harmonic polylogs
*
       gqg1 =
     &  + ca * (  - 8.D0 + 100.D0*x - 872.D0/9.D0*x**2 + 80.D0/9.D0*
     &    dx - 16.D0*z2*x + 4.D0*Hr1(0) + 32.D0*Hr1(0)*x + 176.D0/3.D0*
     &    Hr1(0)*x**2 + 16.D0*Hr1(1)*x - 16.D0*Hr1(1)*x**2 - 8.D0*Hr2(
     &    -1,0) - 16.D0*Hr2(-1,0)*x - 16.D0*Hr2(-1,0)*x**2 - 8.D0*Hr2(0
     &    ,0) - 16.D0*Hr2(0,0)*x - 8.D0*Hr2(1,1) + 16.D0*Hr2(1,1)*x -
     &    16.D0*Hr2(1,1)*x**2 )
       gqg1 = gqg1 + cf * ( 28.D0 - 58.D0*x + 40.D0*x**2 - 8.D0*z2 +
     &    16.D0*z2*x - 16.D0*z2*x**2 + 6.D0*Hr1(0) - 8.D0*Hr1(0)*x + 16.
     &    D0*Hr1(0)*x**2 - 16.D0*Hr1(1)*x + 16.D0*Hr1(1)*x**2 + 4.D0*
     &    Hr2(0,0) - 8.D0*Hr2(0,0)*x + 16.D0*Hr2(0,0)*x**2 + 8.D0*Hr2(0
     &    ,1) - 16.D0*Hr2(0,1)*x + 16.D0*Hr2(0,1)*x**2 + 8.D0*Hr2(1,0)
     &     - 16.D0*Hr2(1,0)*x + 16.D0*Hr2(1,0)*x**2 + 8.D0*Hr2(1,1) -
     &    16.D0*Hr2(1,1)*x + 16.D0*Hr2(1,1)*x**2 )
*
       X1QGA = gqg1
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..The 2-loop quark->gluon splitting functions P_gq^(1)
*
       FUNCTION X1GQA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
      nf=5
*
* ...Colour factors and abbreviation
*
       CF = 4./3.D0
       CA = 3.D0
       DX = 1.D0/X
*
* ...The harmonic polylogs up to weight 
*
*
* ...The splitting function in terms of the harmonic polylogs
*
      ggq1 =
     &  + cf*ca * ( 76.D0/9.D0 + 148.D0/9.D0*x + 176.D0/9.D0*x**2 + 4.D0
     &    *dx + 16.D0*z2 - 48.D0*Hr1(0) - 20.D0*Hr1(0)*x - 32.D0/3.D0*
     &    Hr1(0)*x**2 + 88.D0/3.D0*Hr1(1) - 68.D0/3.D0*Hr1(1)*x - 88.D0/
     &    3.D0*Hr1(1)*dx + 16.D0*Hr2(-1,0) + 8.D0*Hr2(-1,0)*x + 16.D0*
     &    Hr2(-1,0)*dx + 16.D0*Hr2(0,0) + 8.D0*Hr2(0,0)*x - 16.D0*Hr2(0
     &    ,1) + 8.D0*Hr2(0,1)*x + 16.D0*Hr2(0,1)*dx - 16.D0*Hr2(1,0) +
     &    8.D0*Hr2(1,0)*x + 16.D0*Hr2(1,0)*dx - 16.D0*Hr2(1,1) + 8.D0*
     &    Hr2(1,1)*x + 16.D0*Hr2(1,1)*dx )
      ggq1 = ggq1 + cf**2 * (  - 10.D0 - 14.D0*x + 8.D0*Hr1(0) + 14.D0*
     &    Hr1(0)*x - 24.D0*Hr1(1) + 20.D0*Hr1(1)*x + 24.D0*Hr1(1)*dx -
     &    8.D0*Hr2(0,0) + 4.D0*Hr2(0,0)*x + 16.D0*Hr2(1,1) - 8.D0*Hr2(1
     &    ,1)*x - 16.D0*Hr2(1,1)*dx )
      ggq1 = ggq1 + nf*cf * ( 80.D0/9.D0 - 64.D0/9.D0*x - 80.D0/9.D0*dx
     &     - 16.D0/3.D0*Hr1(1) + 8.D0/3.D0*Hr1(1)*x + 16.D0/3.D0*Hr1(1)
     &    *dx )
*   
       X1GQA = ggq1       
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..The regular piece of the 2-loop gg splitting function P_gg^(1) 
*
       FUNCTION X1GGA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
* ..The soft coefficient for use in X1GGB and X1GGC
*
       COMMON / P1GSOFT / A2G
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
*
* ...Colour factors
*
       CF  = 4./3.D0
       nf=5
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)
*
* ...The harmonic polylogs up to weight
*
*
* ...The splitting function in terms of the harmonic polylogs
*    (without the delta(1-x) part, but with the soft contribution)
*
      ggg1 =
     &  + ca**2 * (  - 50.D0/9.D0 - 218.D0/9.D0*x + 268.D0/9.D0*dm + 32.
     &    D0*z2 + 16.D0*z2*x**2 - 8.D0*z2*dp - 8.D0*z2*dm - 100.D0/3.D0
     &    *Hr1(0) + 44.D0/3.D0*Hr1(0)*x - 176.D0/3.D0*Hr1(0)*x**2 + 32.D
     &    0*Hr2(-1,0) + 16.D0*Hr2(-1,0)*x + 16.D0*Hr2(-1,0)*x**2 + 16.D0
     &    *Hr2(-1,0)*dx - 16.D0*Hr2(-1,0)*dp + 32.D0*Hr2(0,0)*x - 16.D0
     &    *Hr2(0,0)*x**2 + 8.D0*Hr2(0,0)*dp + 8.D0*Hr2(0,0)*dm - 32.D0*
     &    Hr2(0,1) + 16.D0*Hr2(0,1)*x - 16.D0*Hr2(0,1)*x**2 + 16.D0*
     &    Hr2(0,1)*dx + 16.D0*Hr2(0,1)*dm - 32.D0*Hr2(1,0) + 16.D0*Hr2(
     &    1,0)*x - 16.D0*Hr2(1,0)*x**2 + 16.D0*Hr2(1,0)*dx + 16.D0*Hr2(
     &    1,0)*dm )
      ggg1 = ggg1 + nf*ca * ( 116.D0/9.D0 - 76.D0/9.D0*x + 92.D0/9.D0*
     &    x**2 - 92.D0/9.D0*dx - 40.D0/9.D0*dm - 8.D0/3.D0*Hr1(0) - 8.D0
     &    /3.D0*Hr1(0)*x )
      ggg1 = ggg1 + nf*cf * (  - 32.D0 + 16.D0*x + 40.D0/3.D0*x**2 + 8.D
     &    0/3.D0*dx - 12.D0*Hr1(0) - 20.D0*Hr1(0)*x - 8.D0*Hr2(0,0) - 8.
     &    D0*Hr2(0,0)*x )
*
* ...The soft (`+'-distribution) part of the splitting function
*
       A2G = - 40.D0/9.D0*ca*nf + 268.D0/9.D0*ca**2 - 8.D0*z2*ca**2
*
       GGG1L = DM * A2G
*
* ...The regular piece of the coefficient function
*
       X1GGA = GGG1 - GGG1L
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the singular (soft) piece.
*
       FUNCTION X1GGB (Y)
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
*
       COMMON / P1GSOFT / A2G
*
       nf=5
       X1GGB  = A2G/(1.D0-Y)
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the 'local' piece.
*
       FUNCTION X1GGC (Y)
*
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
       COMMON / P1GSOFT / A2G
*
* ...Colour factors
*
       CF  = 4./3.D0
       nf=5
       CA  = 3.D0
*
* ...The coefficient of delta(1-x)
*
       P1DELT = 
     ,    - 2.D0*cf*nf
     ,    - 8.D0/3.D0*ca*nf
     ,    + 32.D0/3.D0*ca**2
     ,    + 12.D0*z3*ca**2
*
       X1GGC = DLOG (1.D0-Y) * A2G + P1DELT
*
       RETURN
       END
*
* =====================================================================
*
*
* ..The 1-loop gluon->quark splitting functions P_qg^(0)
*
       FUNCTION X0QGA (X)
*
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
       nf=5
*
       X0QGA = 2.*  ( 1. - 2. * X + 2. * X**2 )
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..The 1-loop quark->gluon splitting functions P_gq^(0)
*
       FUNCTION X0GQA (X)
*
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
       nf=5
*
       CF = 4./3.D0
       X0GQA = 4.*CF * ( - 1. + 0.5 * X + 1./X )
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..The regular piece of the 1-loop gg splitting function P_gg^(0)
*
       FUNCTION X0GGA (X)
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
       nf=5
*
       CA = 3.D0
       X0GGA = 4.*CA * ( - 2. + X - X**2 + 1./X )
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the singular (soft) piece.
*
       FUNCTION X0GGB (X)
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
       nf=5
*
       CA = 3.D0
       X0GGB = 4.*CA / (1.D0-X)
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the 'local' piece.
*
       FUNCTION X0GGC (X)
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
       nf=5
*
       CA = 3.D0
       X0GGC = 4.*CA * LOG (1.D0-X) - 2./3.D0 * NF + 11./3.D0 * CA
*
       RETURN
       END
*
* =================================================================av==

*
* ..The exact convolutions of the 1-loop MS(bar) splitting function 
*    and 1-loop MS(bar) DIS coefficient functions
*     P_ij^(0)*C_j^(1).
*    The expansion parameter is alpha_s/(4 pi).
*
* ..The distributions (in the mathematical sense) are given as in eq.
*    (B.26) of Floratos, Kounnas, Lacaze: Nucl. Phys. B192 (1981) 417.
*    The name-endings A, B, and C of the functions below correspond to
*    the kernel superscripts [2], [3], and [1] in that equation.
*
* ..The code uses the package of Gehrmann and Remiddi for the harmonic
*    polylogarithms published in hep-ph/0107173 = CPC 141 (2001) 296.
*
*
* =====================================================================
*
*
* ..The regular piece of P_qq^(0)*c_1,q^(1)
*
       FUNCTION XQ0C1QA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
* ..The soft coefficient for use in XQ0CQB and XQ0CQC
*
       COMMON / PQ0CQSOFT / PQ0CQA0, PQ0CQA1, PQ0CQA2 
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)
       DL1 = LOG (1.D0-X)

*
* ...The harmonic polylogs up to weight
*
*
* ... in terms of the harmonic polylogs
*    (without the delta(1-x) part, but with the soft contribution)
*
      c1qxPqq =
     &  + cf**2 * ( 34.D0 + 20.D0*x - 45.D0*dm + 20.D0*z2 + 20.D0*z2*x
     &     - 32.D0*z2*dm - 8.D0*Hr1(0) - 8.D0*Hr1(0)*x - 16.D0*Hr1(1)
     &     - 8.D0*Hr1(1)*x + 12.D0*Hr1(1)*dm - 12.D0*Hr2(0,0) - 12.D0*
     &    Hr2(0,0)*x + 16.D0*Hr2(0,0)*dm - 20.D0*Hr2(0,1) - 20.D0*Hr2(0
     &    ,1)*x + 32.D0*Hr2(0,1)*dm - 16.D0*Hr2(1,0) - 16.D0*Hr2(1,0)*x
     &     + 32.D0*Hr2(1,0)*dm - 24.D0*Hr2(1,1) - 24.D0*Hr2(1,1)*x + 48.
     &    D0*Hr2(1,1)*dm )

*
* ...The soft (`+'-distribution) part 
*
       PQ0CQA2 = 
     &     + 24.D0*cf**2
       PQ0CQA1 = 
     &     - 12.D0*cf**2
       PQ0CQA0 = 
     &     - 45.d0*cf**2 - 32.d0*z2*cf**2
*
       Q0CQ1L = DM * ( + DL1**2 * PQ0CQA2 + DL1* PQ0CQA1 + PQ0CQA0 )
*
* ...The regular piece
*
       XQ0C1QA = c1qxPqq - Q0CQ1L
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..The regular piece of P_qq^(0)*c_2,q^(1)
*
       FUNCTION XQ0C2QA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
* ..The soft coefficient for use in XQ0CQB and XQ0CQC
*
       COMMON / PQ0CQSOFT / PQ0CQA0, PQ0CQA1, PQ0CQA2 
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)
       DL1 = LOG (1.D0-X)

*
* ...The harmonic polylogs up to weight
*
*
* ... in terms of the harmonic polylogs
*    (without the delta(1-x) part, but with the soft contribution)
*
      c2qxPqq =
     &  + cf**2 * ( 42.D0 + 24.D0*x - 45.D0*dm + 20.D0*z2 + 20.D0*z2*x
     &     - 32.D0*z2*dm - 8.D0*Hr1(0) - 16.D0*Hr1(0)*x - 16.D0*Hr1(1)
     &     - 24.D0*Hr1(1)*x + 12.D0*Hr1(1)*dm - 12.D0*Hr2(0,0) - 12.D0*
     &    Hr2(0,0)*x + 16.D0*Hr2(0,0)*dm - 20.D0*Hr2(0,1) - 20.D0*Hr2(0
     &    ,1)*x + 32.D0*Hr2(0,1)*dm - 16.D0*Hr2(1,0) - 16.D0*Hr2(1,0)*x
     &     + 32.D0*Hr2(1,0)*dm - 24.D0*Hr2(1,1) - 24.D0*Hr2(1,1)*x + 48.
     &    D0*Hr2(1,1)*dm )

*
* ...The soft (`+'-distribution) part 
*
       PQ0CQA2 = 
     &     + 24.D0*cf**2
       PQ0CQA1 = 
     &     - 12.D0*cf**2
       PQ0CQA0 = 
     &     - 45.d0*cf**2 - 32.d0*z2*cf**2
*
       Q0CQ1L = DM * ( + DL1**2 * PQ0CQA2 + DL1* PQ0CQA1 + PQ0CQA0 )
*
* ...The regular piece
*
       XQ0C2QA = c2qxPqq - Q0CQ1L
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..The regular piece of P_qq^(0)*c_3,q^(1)
*
       FUNCTION XQ0C3QA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
* ..The soft coefficient for use in XQ0CQB and XQ0CQC
*
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
      COMMON / PQ0CQSOFT / PQ0CQA0, PQ0CQA1, PQ0CQA2 
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)
       DL1 = LOG (1.D0-X)

*
* ...The harmonic polylogs up to weight
*
*
* ... in terms of the harmonic polylogs
*    (without the delta(1-x) part, but with the soft contribution)
*
      c3qxPqq =
     &  + cf**2 * ( 36.D0 + 18.D0*x - 45.D0*dm + 20.D0*z2 + 20.D0*z2*x
     &     - 32.D0*z2*dm - 4.D0*Hr1(0) - 12.D0*Hr1(0)*x - 8.D0*Hr1(1)
     &     - 16.D0*Hr1(1)*x + 12.D0*Hr1(1)*dm - 12.D0*Hr2(0,0) - 12.D0*
     &    Hr2(0,0)*x + 16.D0*Hr2(0,0)*dm - 20.D0*Hr2(0,1) - 20.D0*Hr2(0
     &    ,1)*x + 32.D0*Hr2(0,1)*dm - 16.D0*Hr2(1,0) - 16.D0*Hr2(1,0)*x
     &     + 32.D0*Hr2(1,0)*dm - 24.D0*Hr2(1,1) - 24.D0*Hr2(1,1)*x + 48.
     &    D0*Hr2(1,1)*dm )

*
* ...The soft (`+'-distribution) part 
*
       PQ0CQA2 = 
     &     + 24.D0*cf**2
       PQ0CQA1 = 
     &     - 12.D0*cf**2
       PQ0CQA0 = 
     &     - 45.d0*cf**2 - 32.d0*z2*cf**2
*
       Q0CQ1L = DM * ( + DL1**2 * PQ0CQA2 + DL1* PQ0CQA1 + PQ0CQA0 )
*
* ...The regular piece
*
       XQ0C3QA = c3qxPqq - Q0CQ1L
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the singular (soft) piece of P_qq^(0)*c_i,q^(1), i=1,2,3
*
       FUNCTION XQ0CQB (Y)
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
*
       COMMON / PQ0CQSOFT / PQ0CQA0, PQ0CQA1, PQ0CQA2 
*
       DL1 = LOG (1.D0-Y)
       nf=5
       DM  = 1.D0/(1.D0-Y)
*
       XQ0CQB = DM * ( + DL1**2 * PQ0CQA2 + DL1* PQ0CQA1 + PQ0CQA0 )
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the 'local' piece of P_qq^(0)*c_i,q^(1), i=1,2,3
*
       FUNCTION XQ0CQC (Y)
*
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
       COMMON / PQ0CQSOFT / PQ0CQA0, PQ0CQA1, PQ0CQA2 
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0

       DL1 = LOG (1.D0-Y)
*
* ...The coefficient of delta(1-x)
*
       PQ0CQDELT =
     &  - 27.d0*cf**2 + 16.d0*z3*cf**2
*
       XQ0CQC = + DL1**3 * PQ0CQA2/6.D0 + DL1**2 * PQ0CQA1/2.D0 
     &      + DL1 * PQ0CQA0 + PQ0CQDELT
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of P_qg^(0)*c_1,q^(1)
*
       FUNCTION XG0C1QA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...in terms of the harmonic polylogs
*
      c1qxPqg =
     &  +cf * (  - 18.D0 + 16.D0*x - 16.D0*x**2 - 4.D0*z2 + 8.D0*z2
     &    *x - 16.D0*z2*x**2 - 2.D0*Hr1(0) - 32.D0*Hr1(0)*x + 24.D0*
     &    Hr1(0)*x**2 + 14.D0*Hr1(1) - 32.D0*Hr1(1)*x + 24.D0*Hr1(1)*
     &    x**2 + 4.D0*Hr2(0,0) - 8.D0*Hr2(0,0)*x + 16.D0*Hr2(0,0)*x**2
     &     + 4.D0*Hr2(0,1) - 8.D0*Hr2(0,1)*x + 16.D0*Hr2(0,1)*x**2 + 8.D
     &    0*Hr2(1,0) - 16.D0*Hr2(1,0)*x + 16.D0*Hr2(1,0)*x**2 + 8.D0*
     &    Hr2(1,1) - 16.D0*Hr2(1,1)*x + 16.D0*Hr2(1,1)*x**2 )

*
* ...The regular piece 
*
       XG0C1QA = c1qxPqg
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of P_qg^(0)*c_2,q^(1)
*
       FUNCTION XG0C2QA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...in terms of the harmonic polylogs
*
      c2qxPqg =
     &  + cf * (  - 10.D0 + 24.D0*x - 32.D0*x**2 - 4.D0*z2 + 8.D0*z2
     &    *x - 16.D0*z2*x**2 - 2.D0*Hr1(0) - 16.D0*Hr1(0)*x + 24.D0*
     &    Hr1(0)*x**2 + 14.D0*Hr1(1) - 32.D0*Hr1(1)*x + 24.D0*Hr1(1)*
     &    x**2 + 4.D0*Hr2(0,0) - 8.D0*Hr2(0,0)*x + 16.D0*Hr2(0,0)*x**2
     &     + 4.D0*Hr2(0,1) - 8.D0*Hr2(0,1)*x + 16.D0*Hr2(0,1)*x**2 + 8.D
     &    0*Hr2(1,0) - 16.D0*Hr2(1,0)*x + 16.D0*Hr2(1,0)*x**2 + 8.D0*
     &    Hr2(1,1) - 16.D0*Hr2(1,1)*x + 16.D0*Hr2(1,1)*x**2 )

*
* ...The regular piece 
*
       XG0C2QA = c2qxPqg
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of P_gq^(0)*c_1,g^(1)
*
       FUNCTION XQ0C1GA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...in terms of the harmonic polylogs
*
      c1gxPgq =
     &  + cf * (  - 32.D0/3.D0 + 8.D0/3.D0*x + 32.D0/3.D0*x**2 - 8.D0
     &    /3.D0*dx + 8.D0*z2 + 8.D0*z2*x - 4.D0*Hr1(0) - 4.D0*Hr1(0)*x
     &     + 16.D0/3.D0*Hr1(0)*x**2 - 4.D0*Hr1(1) + 4.D0*Hr1(1)*x + 16.D
     &    0/3.D0*Hr1(1)*x**2 - 16.D0/3.D0*Hr1(1)*dx - 8.D0*Hr2(0,0) - 8.
     &    D0*Hr2(0,0)*x - 8.D0*Hr2(0,1) - 8.D0*Hr2(0,1)*x )

*
* ...The regular piece 
*
       XQ0C1GA = c1gxPgq
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of P_gq^(0)*c_2,g^(1)
*
       FUNCTION XQ0C2GA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
*
* ...Colour factors
       nf=5
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...in terms of the harmonic polylogs
*
      c2gxPgq =
     &  + cf * (  - 80.D0/3.D0 + 8.D0/3.D0*x + 64.D0/3.D0*x**2 + 8.D0
     &    /3.D0*dx + 8.D0*z2 + 8.D0*z2*x - 4.D0*Hr1(0) - 20.D0*Hr1(0)*x
     &     + 16.D0/3.D0*Hr1(0)*x**2 - 4.D0*Hr1(1) + 4.D0*Hr1(1)*x + 16.D
     &    0/3.D0*Hr1(1)*x**2 - 16.D0/3.D0*Hr1(1)*dx - 8.D0*Hr2(0,0) - 8.
     &    D0*Hr2(0,0)*x - 8.D0*Hr2(0,1) - 8.D0*Hr2(0,1)*x )

*
* ...The regular piece 
*
       XQ0C2GA = c2gxPgq
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of P_gg^(0)*c_1,g^(1)
*
       FUNCTION XG0C1GA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...in terms of the harmonic polylogs
*
      c1gxPgg =
     &  + ca * (  - 20.D0 - 52.D0*x + 202.D0/3.D0*x**2 - 8.D0/3.D0*
     &    dx + 48.D0*z2*x - 16.D0*z2*x**2 - 34.D0/3.D0*Hr1(0) - 148.D0/
     &    3.D0*Hr1(0)*x + 80.D0/3.D0*Hr1(0)*x**2 - 10.D0/3.D0*Hr1(1) - 
     &    148.D0/3.D0*Hr1(1)*x + 176.D0/3.D0*Hr1(1)*x**2 - 16.D0/3.D0*
     &    Hr1(1)*dx - 8.D0*Hr2(0,0) - 32.D0*Hr2(0,0)*x - 48.D0*Hr2(0,1)
     &    *x + 16.D0*Hr2(0,1)*x**2 + 8.D0*Hr2(1,0) - 16.D0*Hr2(1,0)*x
     &     + 16.D0*Hr2(1,0)*x**2 + 16.D0*Hr2(1,1) - 32.D0*Hr2(1,1)*x + 
     &    32.D0*Hr2(1,1)*x**2 )
      c1gxPgg = c1gxPgg + nf * ( 4.D0/3.D0 - 16.D0/3.D0*x + 16.D0/3.D
     &    0*x**2 + 4.D0/3.D0*Hr1(0) - 8.D0/3.D0*Hr1(0)*x + 8.D0/3.D0*
     &    Hr1(0)*x**2 + 4.D0/3.D0*Hr1(1) - 8.D0/3.D0*Hr1(1)*x + 8.D0/3.D
     &    0*Hr1(1)*x**2 )

*
* ...The regular piece 
*
       XG0C1GA = c1gxPgg
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of P_gg^(0)*c_2,g^(1)
*
       FUNCTION XG0C2GA (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...in terms of the harmonic polylogs
*
      c2gxPgg =
     &  + ca * (  - 36.D0 - 308.D0/3.D0*x + 386.D0/3.D0*x**2 + 8.D0/
     &    3.D0*dx + 48.D0*z2*x - 16.D0*z2*x**2 - 34.D0/3.D0*Hr1(0) - 
     &    340.D0/3.D0*Hr1(0)*x + 80.D0/3.D0*Hr1(0)*x**2 - 10.D0/3.D0*
     &    Hr1(1) - 244.D0/3.D0*Hr1(1)*x + 272.D0/3.D0*Hr1(1)*x**2 - 16.D
     &    0/3.D0*Hr1(1)*dx - 8.D0*Hr2(0,0) - 32.D0*Hr2(0,0)*x - 48.D0*
     &    Hr2(0,1)*x + 16.D0*Hr2(0,1)*x**2 + 8.D0*Hr2(1,0) - 16.D0*Hr2(
     &    1,0)*x + 16.D0*Hr2(1,0)*x**2 + 16.D0*Hr2(1,1) - 32.D0*Hr2(1,1
     &    )*x + 32.D0*Hr2(1,1)*x**2 )
      c2gxPgg = c2gxPgg + nf * ( 4.D0/3.D0 - 32.D0/3.D0*x + 32.D0/3.D
     &    0*x**2 + 4.D0/3.D0*Hr1(0) - 8.D0/3.D0*Hr1(0)*x + 8.D0/3.D0*
     &    Hr1(0)*x**2 + 4.D0/3.D0*Hr1(1) - 8.D0/3.D0*Hr1(1)*x + 8.D0/3.D
     &    0*Hr1(1)*x**2 )

*
* ...The regular piece 
*
       XG0C2GA = c2gxPgg
*
       RETURN
       END      

*
* ..The exact convolutions of the 1-loop MS(bar) splitting function 
*     P_ij^(0)*P_jk^(0).
*    The expansion parameter is alpha_s/(4 pi).
*
* ..The distributions (in the mathematical sense) are given as in eq.
*    (B.26) of Floratos, Kounnas, Lacaze: Nucl. Phys. B192 (1981) 417.
*    The name-endings A, B, and C of the functions below correspond to
*    the kernel superscripts [2], [3], and [1] in that equation.
*
* ..The code uses the package of Gehrmann and Remiddi for the harmonic
*    polylogarithms published in hep-ph/0107173 = CPC 141 (2001) 296.
*
*
* =====================================================================
*
*
* ..The regular piece of the 1-loop splitting functions P_qq^(0)*P_qq^(0)
*
       FUNCTION XQ0Q0A (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
*
* ..The soft coefficient for use in XQ0Q0B and XQ0Q0C
*
       COMMON / PQ0SOFT / PQ0A0, PQ0A1
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)
       DL1 = LOG (1.D0-X)

*
* ...The harmonic polylogs up to weight
*
*
* ...The splitting function in terms of the harmonic polylogs
*    (without the delta(1-x) part, but with the soft contribution)
*
      PqqxPqq =
     &  + cf**2 * (  - 20.D0 - 4.D0*x + 24.D0*dm + 12.D0*Hr1(0) + 12.D0
     &    *Hr1(0)*x - 16.D0*Hr1(0)*dm + 16.D0*Hr1(1) + 16.D0*Hr1(1)*x
     &     - 32.D0*Hr1(1)*dm )

*
* ...The soft (`+'-distribution) part of the splitting function
*
       PQ0A1 = 
     &     + 32.D0*cf**2
       PQ0A0 =
     &     + 24.D0*cf**2
*
       Q0Q0L = DM * ( + DL1* PQ0A1 + PQ0A0 )
*
* ...The regular piece
*
       XQ0Q0A = PqqxPqq - Q0Q0L
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the singular (soft) piece.
*
       FUNCTION XQ0Q0B (Y)
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
*
       COMMON / PQ0SOFT / PQ0A0, PQ0A1
*
       DL1 = LOG (1.D0-Y)
       nf=5
       DM  = 1.D0/(1.D0-Y)
*
       XQ0Q0B = DM * ( DL1* PQ0A1 + PQ0A0 )
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the 'local' piece.
*
       FUNCTION XQ0Q0C (Y)
*
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
       COMMON / PQ0SOFT / PQ0A0, PQ0A1
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0

       DL1 = LOG (1.D0-Y)
*
* ...The coefficient of delta(1-x)
*
       PQ0DELT =
     &  + 9.d0*cf**2 - 16.d0*z2*cf**2
*
       XQ0Q0C = + DL1**2 * PQ0A1/2.D0 + DL1 * PQ0A0 + PQ0DELT
*
       RETURN
       END
*
* =====================================================================
*
*
*
* ..The regular piece of the 1-loop splitting functions P_gg^(0)*P_gg^(0)
*
       FUNCTION XG0G0A (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
* ..The soft coefficient for use in XG0G0B and XG0G0C
*
       COMMON / PG0SOFT / PG0A0, PG0A1
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)
       DL1 = LOG (1.D0-X)

*
* ...The harmonic polylogs up to weight
*
*
* ...The splitting function in terms of the harmonic polylogs
*    (without the delta(1-x) part, but with the soft contribution)
*
      PggxPgg =
     &  + ca**2 * (  - 32.D0/3.D0 - 56.D0/3.D0*x + 88.D0/3.D0*x**2 - 88.
     &    D0/3.D0*dx + 88.D0/3.D0*dm - 48.D0*Hr1(0)*x + 16.D0*Hr1(0)*
     &    x**2 - 16.D0*Hr1(0)*dx - 16.D0*Hr1(0)*dm + 64.D0*Hr1(1) - 32.D
     &    0*Hr1(1)*x + 32.D0*Hr1(1)*x**2 - 32.D0*Hr1(1)*dx - 32.D0*Hr1(
     &    1)*dm )
      PggxPgg = PggxPgg + nf*ca * ( 32.D0/3.D0 - 16.D0/3.D0*x + 16.D0/3.
     &    D0*x**2 - 16.D0/3.D0*dx - 16.D0/3.D0*dm )

*
* ...The soft (`+'-distribution) part of the splitting function
*
       PG0A1 = 
     &     + 32.D0*ca**2
       PG0A0 = 
     &      + 88.D0/3.D0*ca**2 - 16.D0/3.D0*nf*ca
*
       G0G0L = DM * ( + DL1* PG0A1 + PG0A0 )
*
* ...The regular piece
*
       XG0G0A = PggxPgg - G0G0L
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the singular (soft) piece.
*
       FUNCTION XG0G0B (Y)
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
*
       COMMON / PG0SOFT / PG0A0, PG0A1
*
       nf=5
       DL1 = LOG (1.D0-Y)
       DM  = 1.D0/(1.D0-Y)
*
       XG0G0B = DM * ( DL1* PG0A1 + PG0A0 )
*
       RETURN
       END
*
* ---------------------------------------------------------------------
*
*
* ..This is the 'local' piece.
*
       FUNCTION XG0G0C (Y)
*
       IMPLICIT REAL*8 (A - Z)
       INTEGER NF
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
*
       COMMON / PG0SOFT / PG0A0, PG0A1
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0

       DL1 = LOG (1.D0-Y)
*
* ...The coefficient of delta(1-x)
*
       PG0DELT = + 121.d0/9.d0*ca**2 
     & - 44.d0/9.d0*nf*ca + 4.d0/9.d0*nf**2 - 16.d0*z2*ca**2
*
       XG0G0C = + DL1**2 * PG0A1/2.D0 + DL1 * PG0A0 + PG0DELT
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of the 1-loop splitting functions P_qq^(0)*P_qg^(0)
*
       FUNCTION XQ0QG0A (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...The splitting function in terms of the harmonic polylogs
*
      PqqxPqg =
     &  + cf * (  - 2.D0 + 8.D0*x - 4.D0*Hr1(0) + 8.D0*Hr1(0)*x - 16.
     &    D0*Hr1(0)*x**2 - 8.D0*Hr1(1) + 16.D0*Hr1(1)*x - 16.D0*Hr1(1)*
     &    x**2 )

*
* ...The regular piece 
*
       XQ0QG0A = PqqxPqg
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of the 1-loop splitting functions P_qq^(0)*P_gq^(0)
*
       FUNCTION XQ0GQ0A (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...The splitting function in terms of the harmonic polylogs
*
      PqqxPgq =
     &  + cf**2 * ( 8.D0 - 2.D0*x + 8.D0*Hr1(0) - 4.D0*Hr1(0)*x + 16.D0
     &    *Hr1(1) - 8.D0*Hr1(1)*x - 16.D0*Hr1(1)*dx )

*
* ...The regular piece
*
       XQ0GQ0A = PqqxPgq
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of the 1-loop splitting functions P_gg^(0)*P_gq^(0)
*
       FUNCTION XG0GQ0A (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...The splitting function in terms of the harmonic polylogs
*
      PggxPgq =
     &  + cf*ca * ( 52.D0/3.D0 + 34.D0/3.D0*x + 16.D0/3.D0*x**2 - 80.D0/
     &    3.D0*dx - 16.D0*Hr1(0) - 16.D0*Hr1(0)*x - 16.D0*Hr1(0)*dx + 
     &    16.D0*Hr1(1) - 8.D0*Hr1(1)*x - 16.D0*Hr1(1)*dx )
      PggxPgq = PggxPgq + nf*cf * ( 8.D0/3.D0 - 4.D0/3.D0*x - 8.D0/3.D0
     &    *dx )

*
* ...The regular piece
*
       XG0GQ0A = PggxPgq
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of the 1-loop splitting functions P_gg^(0)*P_qg^(0)
*
       FUNCTION XG0QG0A (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...The splitting function in terms of the harmonic polylogs
*
      PqgxPgg =
     &  + ca * ( 34.D0/3.D0 + 52.D0/3.D0*x - 80.D0/3.D0*x**2 + 16.D0/
     &    3.D0*dx + 8.D0*Hr1(0) + 32.D0*Hr1(0)*x - 8.D0*Hr1(1) + 16.D0*
     &    Hr1(1)*x - 16.D0*Hr1(1)*x**2 )
      PqgxPgg = PqgxPgg + nf * (  - 4.D0/3.D0 + 8.D0/3.D0*x - 8.D0/3.
     &    D0*x**2 )

*
* ...The regular piece
*
       XG0QG0A = PqgxPgg
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of the 1-loop splitting functions P_qg^(0)*P_gq^(0)
*
       FUNCTION XQG0GQ0A (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...The splitting function in terms of the harmonic polylogs
*
      PqgxPgq =
     &  + cf * ( 4.D0 - 4.D0*x - 16.D0/3.D0*x**2 + 16.D0/3.D0*dx + 8.
     &    D0*Hr1(0) + 8.D0*Hr1(0)*x )

*
* ...The regular piece
*
       XQG0GQ0A = PqgxPgq
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of the 1-loop splitting functions (P_qg^(0))^2
*
       FUNCTION XQG0QG0A (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...The splitting function in terms of the harmonic polylogs
*
      PqgxPqg =
     &  + nf * (  - 8.D0 - 16.D0*x + 24.D0*x**2 - 4.D0*Hr1(0) - 16.D0
     &    *Hr1(0)*x - 16.D0*Hr1(0)*x**2 )
*
* ...The regular piece
*
       XQG0QG0A = PqgxPqg
*
       RETURN
       END
*
* =====================================================================
*
* ..The regular piece of the 1-loop splitting functions (P_gq^(0))^2
*
       FUNCTION XGQ0GQ0A (X)
*
       IMPLICIT REAL*8 (A - Z)
       COMPLEX*16 HC1, HC2, HC3, HC4 
       INTEGER NF, NF2, N1, N2, NW, I1, I2, I3, N
       PARAMETER ( N1 = -1, N2 = 1, NW = 4 ) 
       DIMENSION HC1(N1:N2),HC2(N1:N2,N1:N2),HC3(N1:N2,N1:N2,N1:N2), 
     ,           HC4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HR1(N1:N2),HR2(N1:N2,N1:N2),HR3(N1:N2,N1:N2,N1:N2), 
     ,           HR4(N1:N2,N1:N2,N1:N2,N1:N2) 
       DIMENSION HI1(N1:N2),HI2(N1:N2,N1:N2),HI3(N1:N2,N1:N2,N1:N2), 
     ,           HI4(N1:N2,N1:N2,N1:N2,N1:N2) 
       PARAMETER ( Z2 = 1.6449 34066 84822 64365 D0,
     ,             Z3 = 1.2020 56903 15959 42854 D0 )
       common/hpls/hc1,hc2,hc3,hc4,hr1,hr2,hr3,hr4,hi1,hi2,hi3,hi4
       nf=5
*
* ...Colour factors
*
       CF  = 4./3.D0
       CA  = 3.D0
*
* ...Some abbreviations
*
       DX = 1.D0/X
       DM = 1.D0/(1.D0-X)
       DP = 1.D0/(1.D0+X)

*
* ...The harmonic polylogs up to weight
*
*
* ...The splitting function in terms of the harmonic polylogs
*
      PgqxPgq =
     &  + cf**2 * ( 16.D0 + 8.D0*x - 24.D0*dx - 16.D0*Hr1(0) - 4.D0*
     &    Hr1(0)*x - 16.D0*Hr1(0)*dx )

*
* ...The regular piece
*
       XGQ0GQ0A = PgqxPgq
*
       RETURN
       END
*
* =====================================================================
*
