CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C    WARNING: IN THE NNLO COEFFICIENT FUNCTIONS
C     THERE IS AN OVERALL EXTRA FACTOR OF 2 W.R. TO
C       THE CONVENTIONS USED AT LO AND AT NLO

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

c--------couplings----second argument 1->W-, 2->w+, 3->Z, 4->gamma,
c-----                                  5->gammaZ
c------ the vector goes INTO the proton (=out from the VVH vertex)
      subroutine SetCouplings()
      implicit none
      integer nf,i,j
      double precision couplings(-6:6,5), couplings3(-6:6,5),sthw
      common/coupl/couplings
      common/coupl3/couplings3
      common/nflav/nf
      common/sin_wein/sthw
      do i=-6,6
       do j=1,5
       couplings(i,j)=0d0
       couplings3(i,j)=0d0
       end do
      end do
c---------- W- ------------------      
      do i=1,3
      couplings(2*i,1)=2d0
      couplings3(2*i,1)=2d0
      couplings(-2*i+1,1)=2d0
      couplings3(-2*i+1,1)=2d0
      end do 
c--------- W+ ------------------      
      do i=-6,6
      couplings(i,2)=couplings(-i,1)
      couplings3(i,2)=couplings3(-i,1)
      end do
c---------gamma-----------------
      do i=1,3
      end do
c--------Z0--------------------
      do i=1,3
      couplings(2*i,3)=2d0*(1d0/4d0+(1d0/2d0-4d0/3d0*sthw)**2)
      couplings(-2*i,3)=2d0*(1d0/4d0+(1d0/2d0-4d0/3d0*sthw)**2)
      couplings(2*i-1,3)=2d0*(1d0/4d0+(1d0/2d0-2d0/3d0*sthw)**2)
      couplings(-2*i+1,3)=2d0*(1d0/4d0+(1d0/2d0-2d0/3d0*sthw)**2)

      couplings3(2*i,3)=(1d0-8d0/3d0*sthw)
      couplings3(-2*i,3)=(1d0-8d0/3d0*sthw)
      couplings3(2*i-1,3)=(1d0-4d0/3d0*sthw)
      couplings3(-2*i+1,3)=(1d0-4d0/3d0*sthw)
      end do
      do i=1,nf
       do j=1,5
       couplings(0,j)=couplings(0,j)+couplings(-i,j)+couplings(i,j)
c------the coulplings3 for the gluon is actually not needed--------
       couplings3(0,j)=couplings3(0,j)+couplings3(-i,j)+couplings3(i,j)
       end do
      end do
      return 
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c--------------------------------------------------------------------------
c  PDF gives the value of the parton density, i.e. q(x) instead of x q(x), with the coupling included  

      subroutine PDF(x,q,f)
      implicit none
      integer i
      double precision x,q,f(-6:6)
      call Evolvepdf(x,q,f)
      do i=-6,6
       f(i)=f(i)/x
       end do
      
      return
      end

      double precision function pdfg(f)
c-----the gluon pdf including the sum of the couplings    
      implicit none
      integer v,ic,nf,mellchk,nmell
      double precision x,q,f(-6:6),couplings(-6:6,5)
      common/pdfpar/x,q,ic
      common/vect/v
      common/coupl/couplings
      common/nflav/nf
      common/mell_check/mellchk
      common/mellin/nmell
      call setcouplings()
      pdfg=f(0) *couplings(0,v)
      return
      end

      double precision function Sing(f)
      implicit none
      integer i,nf,v,ic,nmell,mellchk
      common/nflav/nf
      common/vect/v
      double precision x,Q, quark, antiq, f(-6:6)
      double precision couplings(-6:6,5)
      common/coupl/couplings
      common/pdfpar/x,q,ic
      common/mellin/nmell
      common/mell_check/mellchk
      call setcouplings()
      quark=0d0
      antiq=0d0
      do i=1,nf
      quark=quark+f(i*ic)
      antiq=antiq+f(-i*ic)
      end do
      sing=(quark+antiq) *couplings(0,v)
      if(v.eq.3) then
      sing=sing
      endif
      return
      end

c-----the non singlet nsing refers to the non singlet valence
c-----    distribution in hep-ph/9907472
      double precision function NSing(f)
      implicit none
      integer i,nf,v,f3c,ic
      common/nflav/nf
      common/vect/v
      common/f3call/f3c
      double precision x,Q, quark, antiq, f(-6:6)
      double precision couplings(-6:6,5), couplings3(-6:6,5)
      common/coupl/couplings
      common/coupl3/couplings3
      common/pdfpar/x,q,ic
      integer nmell,mellchk
      common/mellin/nmell
      common/mell_check/mellchk
      call setcouplings()
      quark=0d0
      antiq=0d0
      
      if (f3c.eq.0) then
      do i=1,nf
      quark=quark+f(i*ic) *couplings(i,v)
      antiq=antiq+f(-i*ic) *couplings(-i,v)
      end do
      nsing=(quark+antiq)
        else if (f3c.eq.1) then
      do i=1,nf
      quark=quark+f(i*ic) *couplings3(i,v)
      antiq=antiq+f(-i*ic) *couplings3(-i,v)
      end do
      nsing=(quark-antiq)
      endif

      return
      end

c----- the valence distribution      
      double precision function Val(f)
      implicit none
      integer i,nf,v,f3c,ic
      common/nflav/nf
      common/vect/v
      common/f3call/f3c
      double precision x,Q, quark, antiq, f(-6:6)
      double precision couplings(-6:6,5), couplings3(-6:6,5)
      common/coupl/couplings
      common/coupl3/couplings3
      common/pdfpar/x,q,ic
      call setcouplings()
      quark=0d0
      antiq=0d0

      do i=1,nf
      quark=quark+f(i*ic)
      antiq= antiq+f(-i*ic)
      enddo
      if (f3c.eq.0) then
        val=(quark-antiq)*couplings(0,v)
      else
        val=(quark-antiq)*couplings3(0,v)
      endif
        val=val
      return
      end
        
c-----the non singlet plus  nsingp refers to the non singlet plus
c-----    distribution in hep-ph/9907472. analogously for nsingm
      double precision function NSingp(f)
      implicit none
      double precision nsing, sing,f(-6:6)
      integer nmell, mellchk,nf
       common/mellin/nmell
      common/mell_check/mellchk
      common/nflav/nf
 
      nsingp=nsing(f)-sing(f)
      return
      end

      double precision function nsingm(f)
      implicit none
      integer i,nf,v,f3c,ic
      common/nflav/nf
      common/vect/v
      common/f3call/f3c
      double precision x,Q, quark, antiq, f(-6:6),g,val
      double precision couplings(-6:6,5), couplings3(-6:6,5)
      common/coupl/couplings
      common/coupl3/couplings3
      common/pdfpar/x,q,ic
      call setcouplings()
      quark=0d0
      antiq=0d0

      do i=1,nf
      if (f3c.eq.0) then
      quark=quark+f(i*ic)*couplings(i,v)
      antiq= antiq+f(-i*ic)*couplings(-i,v)
      else
      quark=quark+f(i*ic)*couplings3(i,v)
      antiq= antiq+f(-i*ic)*couplings3(-i,v)
      endif
      enddo
      nsingm=quark-antiq-val(f)
      return
      end

      double precision function delta(f)
      implicit none
      integer i,nf,v,f3c,ic
      common/nflav/nf
      common/vect/v
      common/f3call/f3c
      double precision x,Q, up,dn, f(-6:6)
      double precision couplings(-6:6,5), couplings3(-6:6,5)
      common/coupl/couplings
      common/coupl3/couplings3
      common/pdfpar/x,Q,ic
      call setcouplings()
      up=0d0
      dn=0d0

      do i=1,nf
      if (f3c.eq.0) then
        if (mod(i,2).eq.0) then
        up=up+(f(i*ic)-f(-i*ic))*(couplings(0,v))
        else
        dn=dn+(f(i*ic)-f(-i*ic))*(couplings(0,v))
        endif
      else
        if (mod(i,2).eq.0) then
        up=up+(f(i*ic)+f(-i*ic))*(couplings3(0,v))
        else
        dn=dn+(f(i*ic)+f(-i*ic))*(couplings3(0,v))
        endif
      endif
      enddo
      delta=(up-dn)
      return
      end

      double precision function pdfgReg(f,f1,z)
      implicit none
      double precision pdfg, f(-6:6), f1(-6:6),z
      pdfgreg=pdfg(f) - pdfg(f1)*z
      return
      end

      double precision function SingReg(f,f1,z)
      implicit none
      double precision sing, f(-6:6), f1(-6:6),z 
      singreg=sing(f) - sing(f1)*z
      return
      end

      double precision function NSingReg(f,f1,z)
      implicit none
      double precision nsing, f(-6:6), f1(-6:6),z 
      nsingreg=nsing(f) - nsing(f1)*z
      return
      end

      double precision function ValReg(f,f1,z)
      implicit none
      double precision val, f(-6:6), f1(-6:6),z 
      valreg=val(f) - val(f1)*z
      return
      end

      double precision function NSingpReg(f,f1,z)
      implicit none
      double precision nsingp, f(-6:6), f1(-6:6),z 
      nsingpreg=nsingp(f) - nsingp(f1)*z
      return
      end

      double precision function NSingmReg(f,f1,z)
      implicit none
      double precision nsingm, f(-6:6), f1(-6:6),z 
      nsingmreg=nsingm(f) - nsingm(f1)*z
      return
      end

      double precision function deltaReg(f,f1,z)
      implicit none
      double precision delta, f(-6:6), f1(-6:6),z 
      deltareg=delta(f) - delta(f1)*z
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c--------------------------------------------------------------------------
c------------------------   FL   ------------------------------------------

c-----NLO----------------------------------------------------------
  
      double precision function CLNLOa(z)
      implicit none
      double precision z
      clnloa=8d0/3d0 *z*2d0
      return
      end

      double precision function cLNLOga(z)
      implicit none
      double precision z
      clnloga=2d0*z*(1d0-z)*2d0
      return 
      end
      
c-----NNLO---------------------------------------------------------
c------------------------------------------------------------------
c----------SINGLET-------------------------------------------------

      DOUBLE PRECISION FUNCTION CLNNLOSA(Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      DL  = LOG (Y)
      DL1 = LOG (1.D0-Y)
      CLNNLOSA =  ( (15.94D0 - 5.212D0 * Y) * (1.D0-Y)**2 * DL1
     1    + (0.421D0 + 1.520D0 * Y) * DL**2 + 28.09D0 * (1.D0-Y) * DL
     2         - (2.370D0/Y - 19.27D0) * (1.D0-Y)**3 )
      RETURN
      END

      DOUBLE PRECISION FUNCTION CLNNLOGA (Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
       DL  = LOG (Y)
      DL1 = LOG (1.D0-Y)
      CLNNLOGA = ( (94.74D0 - 49.20D0 * Y) * (1.D0-Y) * DL1**2 
     1 + 864.8D0 * (1.D0-Y) * DL1 + 1161.D0* Y * DL * DL1 
     2         + 60.060 * Y * DL**2 + 39.66D0 * (1.D0-Y) * DL 
     3         - 5.333D0 * (1.D0/Y - 1.D0) )
      RETURN
      END

c-------------------------------------------------------------------
c---------NON SINGLET-----------------------------------------------
C-----CHARGED CURRENT--------------      
      DOUBLE PRECISION FUNCTION CLNNLONSA(Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
      nf=5
      DL  = LOG (Y)
      DL1 = LOG (1.D0-Y)
      CLNNLONSA = 
     1   - 52.27D0 + 100.8D0 * Y
     2   + (23.29D0 * Y - 0.043D0) * DL**2 - 22.21D0 * DL 
     3   + 13.30D0 * DL1**2 - 59.12D0 * DL1 - 141.7D0 * DL * DL1 
     4   + NF * 16.D0/27.D0 * 
     5   ( 6.D0* Y*DL1 - 12.D0* Y*DL - 25.D0* Y + 6.D0)
      RETURN
      END

      DOUBLE PRECISION FUNCTION CLNNLONSc(Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      CLNNLONSC = -0.150D0
      return
      END

c--------------neutral current---------------
      DOUBLE PRECISION  FUNCTION CLNNLONSA_nc (Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
      nf=5
      DL  = LOG (Y)
      DL1 = LOG (1.D0-Y)
      CLNNLONSA_NC = 
     1          - 40.41D0 + 97.48D0*Y 
     2          + (26.56D0 * Y - 0.031D0) * DL**2 - 14.85D0 * DL 
     3          + 13.62D0 * DL1**2 - 55.79D0 * DL1 - 150.5D0 * DL * DL1 
     4 + NF * 16.D0/27.D0 * ( 6.D0* Y*DL1 - 12.D0* Y*DL - 25.* Y + 6.D0)
      RETURN
      END


      DOUBLE PRECISION FUNCTION CLNNLONSC_nc (Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      CLNNLONSC_nc=-0.164d0
      RETURN
      END

c-----NLO convoluctions
      double precision function intLnloq(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision z,x,eps,cLnloa,nsing,lf,lr
      common/prec/eps 
      double precision pdf1(-6:6), pdfz(-6:6)

      intLnloq=(1d0-x-eps)/z*cLnloA(z)*nsing(pdfz)
      return
      end

      double precision function intLnlog(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision z,x,eps,cLnloga, pdfg,lf,lr
      common/prec/eps 
      double precision pdf1(-6:6), pdfz(-6:6)

      intLnlog=(1d0-x-eps)/z*cLnloga(z)*pdfg(pdfz)
      return
      end

c-----NNlo convolutions
c-----neutral current
      double precision function IntlnspNNLO_Z(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision nsingp,nsingpreg
      double precision z,x,lf,lr,beta0,eps
      double precision clnnlonsa_nc, clnnlonsc_nc
      double precision clnloa
      double precision xq0c2qa,xq0c1qa
      double precision a,b,c 
      common/prec/eps 
      double precision pdf1(-6:6), pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=clnnlonsa_nc(z) 
     1 +lf*(xq0c2qa(z)-xq0c1qa(z)-beta0*clnloa(z) )
     3 +lr*beta0*(clnloa(z))

      b=0d0

      c=clnnlonsc_nc(x) 

      intlnspNNLO_Z=(1d0-x-eps)/z*(a*nsingp(pdfz)) 
     1                 +c*nsingp(pdf1)
      intlnspnnlo_z=4d0*intlnspnnlo_z
      return
      end
 
      double precision function IntlqNNLO_Z(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision z,x,lf,lr,beta0,eps
      double precision sing, singreg
      integer nf
      common/nflav/nf
      double precision clnnlonsa_nc, clnnlonsc_nc,clnnlosa
      double precision clnloa
      double precision xq0c2qa,xq0c1qa,xq0c1ga,xq0c2ga
      double precision a,b,c 
      common/prec/eps 
      double precision pdf1(-6:6), pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=
     1  clnnlonsa_nc(z) 
     1 + clnnlosa(z)/2d0
     1 +lf*(xq0c2qa(z)-xq0c1qa(z)-beta0*clnloa(z) )
     3 +lr*beta0*(clnloa(z))
     4 +lf*(xq0c2ga(z)-xq0c1ga(z))/2d0

      b=0d0
      c=0d0

      c=clnnlonsc_nc(x) 

      intlqnnlo_Z=(1d0-x-eps)/z*(a*sing(pdfz)) 
     1                 +c*sing(pdf1)
      intlqnnlo_z=intlqnnlo_z*4d0
      return
      end

      double precision function IntlgNNLO(z,x,lf,lr,pdf1,pdfz)
      implicit none
      integer v
      common/vect/v
      double precision z,x,lf,lr,beta0,eps
      double precision clnnloga 
      double precision pdfg
      double precision x1qga, xg0c2qa,xg0c1qa, xg0c2ga,xg0c1ga,
     1  clnloga 
      double precision a,b,c 
      common/prec/eps 
      double precision pdf1(-6:6), pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=clnnloga(z) 
     1 + lf*(xg0c2qa(z)-xg0c1qa(z)+xg0c2ga(z)-xg0c1ga(z)
     1        - beta0*clnloga(z)*2d0)
     3 + lr*beta0*(clnloga(z))*2d0

      c=0d0

      intlgNNLO=(1d0-x-eps)/z*(a*pdfg(pdfz)) 
      intlgnnlo=intlgnnlo/2d0
      if(v.eq.3) then
      intlgnnlo=intlgnnlo*4d0
      endif
      return
      end
 
      double precision function IntlnsmNNLO_W(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision z,x,lf,lr,beta0,eps
      double precision delta, deltareg
      double precision clnnlonsa, clnnlonsc
      double precision clnloa
      double precision xq0c2qa,xq0c1qa
      double precision a,b,c 
      common/prec/eps 
      double precision pdf1(-6:6), pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=clnnlonsa(z) 
     1 +lf*(xq0c2qa(z)-xq0c1qa(z)-beta0*clnloa(z) )
     3 +lr*beta0*(clnloa(z))

      b=0d0

      c=clnnlonsc(x) 
      intlnsmNNLO_W=(1d0-x-eps)/z*(a*delta(pdfz)) 
     1                 +c*delta(pdf1)
      intlnsmnnlo_w=intlnsmnnlo_w/8d0
      return
      end
 
      double precision function IntlqNNLO_W(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision z,x,lf,lr,beta0,eps
      double precision sing, singreg
      integer nf
      common/nflav/nf
      double precision clnnlonsa_nc, clnnlonsc_nc,clnnlosa
      double precision clnloa
      double precision xq0c2qa,xq0c1qa,xq0c1ga,xq0c2ga
      double precision a,b,c 
      double precision pdf1(-6:6), pdfz(-6:6)
      common/prec/eps
      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=
     1  clnnlonsa_nc(z)
     2 + clnnlosa(z)*nf
     1 +lf*(xq0c2qa(z)-xq0c1qa(z)-beta0*clnloa(z) )
     3 +lr*beta0*(clnloa(z))
     4 +lf*(xq0c2ga(z)-xq0c1ga(z))*nf

      b=0d0
      c=0d0
      c=clnnlonsc_nc(x)

      intlqnnlo_w=(1d0-x-eps)/z*(a*sing(pdfz)) 
     1                 +c*sing(pdf1)
       intlqnnlo_w=intlqnnlo_w/8d0

      return
      end

c------------------------------------------------------------- 
      double precision function FL(ic,x,q,muf,mur,ord,v,z,pdfz,pdf1)
      implicit none
      double precision x,q,z, nsing,alphaspdf,pi,eps
      integer ord, nf,v,i,vv,ic,iic
      double precision intlnloq, intlnlog
      double precision intlnspnnlo_z, intlqnnlo_z,intlgnnlo
      double precision intlnsmnnlo_w, intlqnnlo_w
      double precision muf, mur, lf, lr
      common/prec/eps
      double precision xx,qq
      common/pdfpar/xx,qq,iic
      common/nflav/nf
      common/vect/vv
      integer f3c
      common/f3call/f3c
      double precision pdf1(-6:6),pdfz(-6:6)
      lf=2d0*log(q/muf)
      lr=2d0*log(mur/muf)

      if(v.le.2) then
      nf=4
      else
      nf=5
      endif

      f3c=0
      iic=ic
      xx=x
      qq=muf
      vv=v
      if(1d0-x.lt.eps) then
      fl=0d0
      else

      pi=4d0*datan(1d0)

      if (ord.eq.1) then
      FL=0d0

      else if (ord.eq.2) then
      fL=x*alphaspdf(mur)/(4d0*pi)*(
     1 intlnloq(z,x,lf,lr,pdf1,pdfz)
     1 +intlnlog(z,x,lf,lr,pdf1,pdfz)
     1)
     
      else if (ord.eq.3) then
       if (v.eq.1) then
         fl=(alphaspdf(mur)/(4d0*pi))**2 *x*(
     1           intlnsmnnlo_w(z,x,lf,lr,pdf1,pdfz)
     2          +intlqnnlo_w(z,x,lf,lr,pdf1,pdfz)
     3          +intlgnnlo(z,x,lf,lr,pdf1,pdfz)
     4          )       
       else if (v.eq.2) then
         fl=(alphaspdf(mur)/(4d0*pi))**2 *x*(
     1          - intlnsmnnlo_w(z,x,lf,lr,pdf1,pdfz)
     2          +intlqnnlo_w(z,x,lf,lr,pdf1,pdfz)
     3          +intlgnnlo(z,x,lf,lr,pdf1,pdfz)
     4          )
       else if (v.eq.3) then    
        fl=(alphaspdf(mur)/(4d0*pi))**2 *x*(
     1           intlnspnnlo_z(z,x,lf,lr,pdf1,pdfz)
     2          +intlqnnlo_z(z,x,lf,lr,pdf1,pdfz)
     3          +intlgnnlo(z,x,lf,lr,pdf1,pdfz)
     4          )/4d0 
       endif

      endif
      endif
      
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c--------------------------------------------------------------------------
c------------------------   F1   ------------------------------------------


      double precision function F1(ic,x,q,muf,mur,ord,v,z,pdfz,pdf1)
      implicit none
      double precision x,q,muf,mur,z
      double precision pdfz(-6:6), pdf1(-6:6)
      integer ic,ord,v
      double precision f2,fl
      f1=(f2(ic,x,q,muf,mur,ord,v,z,pdfz,pdf1)
     1 -fl(ic,x,q,muf,mur,ord,v,z,pdfz,pdf1))/
     1 (2d0*x)
      return
      end
  

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c--------------------------------------------------------------------------
c------------------------   F2   ------------------------------------------

      double precision function C2NLOa(z)
      implicit none
      double precision z
      C2NLOa= 4d0/3d0*( 3d0+2d0*z-(1d0+z)*log(1-z)
     1   -(1d0+z**2)/(1d0-z)*log(z))*2d0
      return
      end

      double precision function C2NLOb(z)
      implicit none
      double precision z
      C2NLOb= 4d0/3d0*(2d0*log(1d0-z)/(1d0-z)-3d0/2d0 /(1d0-z) )*2d0
      return
      end

      double precision function C2NLOc(z)
      implicit none
      double precision z,pi,l1
      l1=dlog(1d0-z)
      pi=4d0*datan(1d0)
      C2NLOc= (-4d0/3d0*(pi**2/3d0+9d0/2d0 ) - 
     1 2d0/3d0*(3d0-2d0*l1)*l1)*2d0
      return
      end

      double precision function C2NLOg(z)
      implicit none
      double precision z
      C2NLOg=1d0/2d0*( (z**2+(1-z)**2)*log((1-z)/z)-1d0+8*z*(1-z) )*2d0
      return
      end

      double precision FUNCTION C2NNLOSA(Y)
      IMPLICIT double precision (A-Z)
      DL  = LOG (Y)
      DL1 = LOG (1.d0-Y)
      C2NNLOSA = ( 5.290d0 * (1.d0/Y-1.d0) + 4.310d0 * DL**3   
     1   - 2.086d0 * DL**2 + 39.78d0 * DL - 0.101d0 * (1.d0-Y) * DL1**3 
     2   - (24.75d0 - 13.80d0 * Y) * DL**2 * DL1 + 30.23d0 * DL * DL1 )
      RETURN
      end

      double precision FUNCTION C2NNLOGA (Y)
      IMPLICIT double precision (A-Z)
      DL  = LOG (Y)
      DL1 = LOG (1.D0-Y)
      C2NNLOGA =
     1  ( 1.d0/Y * (11.90d0 + 1494.d0* DL1) + 5.319d0 * DL**3  
     1       - 59.48d0 * DL**2 - 284.8d0 * DL + 392.4d0 - 1483.d0* DL1
     2     + (6.445d0 + 209.4d0 * (1.d0-Y)) * DL1**3 - 24.00d0 * DL1**2
     3        - 724.1d0 * DL**2 * DL1 - 871.8d0 * DL * DL1**2 )
      RETURN
      END

      double precision FUNCTION C2NNLOGC (Y)
      IMPLICIT double precision (A-Z)
       C2NNLOGC=-0.28d0
      RETURN
      END
c------------------NON SINGLET----------------------
c---------------charged current-----------------------
      double precision function C2NNLONSA(Y)
      IMPLICIT double precision (A-Z)
      INTEGER NF
      nf=5
      DL  = LOG (Y)
      DL1 = LOG (1.D0-Y)
      C2NNLONSA = - 84.18d0 - 1010.d0* Y
     2 -3.748d0 * DL**3 - 19.56d0 * DL**2 - 1.235d0 * DL 
     3 - 17.19d0 * DL1**3 + 71.08d0 * DL1**2 - 663.0d0 * DL1
     4 - 192.4d0 * DL * DL1**2 + 80.41d0  * DL**2 * DL1
     5 + NF * ( - 5.691d0 - 37.91d0 *Y 
     6 + 2.244d0 * DL**2 + 5.770d0 * DL
     7 - 1.707d0* DL1**2  + 22.95d0 * DL1
     8 + 3.036d0 * DL**2 * DL1 + 17.97d0 * DL * DL1 )     
      RETURN
      END

      DOUBLE PRECISION FUNCTION C2NNLONSB (Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
       nf=5
      DL1 = LOG (1.D0-Y)
      DM  = 1./(1.D0-Y)
      C2NNLONSB = 
     1  + 14.2222d0 * DL1**3 - 61.3333d0 * DL1**2- 31.105d0 * DL1 
     2  + 188.64d0
     3  + NF * ( 1.77778d0 * DL1**2 - 8.5926d0 *  DL1 + 6.3489 ) 
      C2NNLONSB = DM * C2NNLONSB
      RETURN
      END

      DOUBLE PRECISION FUNCTION C2NNLONSC (Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
      nf=5
      DL1 = LOG (1.D0-Y)
      C2NNLONSC = 
     1 + 3.55555D0 * DL1**4 - 20.4444D0 * DL1**3 - 15.5525D0 * DL1**2
     2 + 188.64D0 * DL1 - 338.531D0 + 0.537D0
     3 + NF * (0.592593D0 * DL1**3 - 4.2963D0 * DL1**2 
     4 + 6.3489D0 * DL1 + 46.844D0 - 0.0035D0)
      RETURN
      END

c------------------------ neutral current
      DOUBLE PRECISION  FUNCTION C2NNLONSA_NC (Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
       nf=5
      DL  = LOG (Y)
      DL1 = LOG (1.D0-Y)
      C2NNLONSA_NC = 
     1          - 69.59D0 - 1008.D0* Y
     2          - 2.835D0 * DL**3 - 17.08D0 * DL**2 + 5.986D0 * DL 
     3          - 17.19D0 * DL1**3 + 71.08D0 * DL1**2 - 660.7D0 * DL1
     4          - 174.8D0 * DL * DL1**2 + 95.09D0 * DL**2 * DL1
     5        + NF * ( - 5.691D0 - 37.91D0 * Y 
     6          + 2.244D0 * DL**2 + 5.770D0 * DL 
     7          - 1.707D0 * DL1**2  + 22.95D0 * DL1
     8          + 3.036D0 * DL**2 * DL1 + 17.97D0 * DL * DL1 )     
      RETURN
      END

      DOUBLE PRECISION FUNCTION C2NNLONSB_NC (Y)
c-----the same as c2nnlonsb
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
      nf=5
      DL1 = LOG (1.D0-Y)
      DM  = 1./(1.D0-Y)
      C2NNLONSB_nc = 
     1  + 14.2222d0 * DL1**3 - 61.3333d0 * DL1**2- 31.105d0 * DL1 
     2  + 188.64d0
     3  + NF * ( 1.77778d0 * DL1**2 - 8.5926d0 *  DL1 + 6.3489 ) 
      C2NNLONSB_nc = DM * C2NNLONSB_nc
      RETURN
      END


      DOUBLE PRECISION FUNCTION C2NNLONSC_NC (Y)
      IMPLICIT REAL*8 (A-Z)
      INTEGER NF
       nf=5
      DL1 = LOG (1.D0-Y)
      C2NNLONSC_NC = 
     1 + 3.55555D0 * DL1**4 - 20.4444D0 * DL1**3 - 15.5525D0 * DL1**2
     2 + 188.64D0 * DL1 - 338.531D0 + 0.485D0 
     3 + NF * (0.592593D0 * DL1**3 - 4.2963D0 * DL1**2 
     4 + 6.3489D0 * DL1 + 46.844D0 - 0.0035D0)
      RETURN
      END


c-----NLO convolutions
      double precision function Int2NLOq(z,x,lf,lr,pdf1,pdfz)
      implicit none
      doubLe precision z,x,eps,lf,lr
      common/prec/eps 
      double precision pdf1(-6:6),pdfz(-6:6)

      double precision c2nloa,c2nlob,c2nloc
      double precision p0qqa,p0qqb,p0qqc
      double precision nsing,nsingreg
      integer mchk, nmell
      common/mell_check/mchk
      common/mellin/nmell
     
      double precision a,b,c
      a=c2nloa(z)+p0qqa(z)*lf
      b=c2nlob(z)+p0qqb(z)*lf
      c=c2nloc(x)+p0qqc(x)*lf
      if( mchk.ne.1) then     
      int2nloq=(1d0-x-eps)/z*(a*nsing(pdfz)+b*nsingreg(pdfz,pdf1,z))+
     1 c *nsing(pdf1)
      else
      int2nloq=(1d0-x-eps)*(a*z**(nmell-1)+b*(z**(nmell-1)-1d0))+
     1 c 
      endif
      return
      end

      double precision function Int2NLOg(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision z,x,eps,lf,lr
      common/prec/eps 
      double precision pdf1(-6:6),pdfz(-6:6)

      double precision c2nlog,pdfg, p0qga
      double precision a
      
       integer mchk, nmell,nf
       common/mell_check/mchk
       common/mellin/nmell
       common/nflav/nf
      a=c2nlog(z)+p0qga(z)*lf
      if(mchk.ne.1) then
      int2nlog=(1d0-x-eps)/z*a*pdfg(pdfz)
      else
      int2nlog=(1d0-x-eps)*a*z**(nmell-1)*nf
      endif
      return
      end

c-----NNLO convolutions      
c-----neutral current
      double precision function Int2nspNNLO_Z(z,x,lf,lr,pdf1,pdfz)
      implicit none
        integer mchk, nmell
       common/mell_check/mchk
       common/mellin/nmell
      double precision z,x,lf,lr,beta0,eps
      double precision c2nnlonsa_nc, c2nnlonsc_nc,c2nnlonsb_nc,
     1 nsingp, nsingpreg
      double precision c2nloa, c2nlob,c2nloc
      double precision x1nspa,x1nsb,x1nsc,xq0c2qa,xq0cqb,xq0cqc
      double precision xq0q0a,xq0q0b,xq0q0c,p0qqa,p0qqb,p0qqc
      double precision a,b,c 
      common/prec/eps 
      double precision pdf1(-6:6),pdfz(-6:6)

      double precision nsing, nsingreg
      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=c2nnlonsa_nc(z) 
     1 +lf*(x1nspa(z)+xq0c2qa(z)-beta0*c2nloa(z) )
     2 +lf**2/2d0 *(xq0q0a(z)-beta0*p0qqa(z) )
     3 +lr*beta0*(c2nloa(z)+lf*p0qqa(z))

      b=c2nnlonsb_nc(z)
     1 +lf*(x1nsb(z)+xq0cqb(z)- beta0*c2nlob(z) )
     1 +lf**2/2d0*(xq0q0b(z)-beta0*p0qqb(z) )
     2 +lr*beta0*(c2nlob(z)+lf*p0qqb(z))

      c=c2nnlonsc_nc(x) 
     1 +lf*(x1nsc(x)+xq0cqc(x)-beta0*c2nloc(x)) 
     2 + lf**2/2d0*(xq0q0c(x)-beta0*p0qqc(x) )
     3 +lr*beta0*(c2nloc(x)+lf*p0qqc(x))

      if( mchk.ne.1) then     
      int2nspNNLO_Z=
     1 (1d0-x-eps)/z*(a*nsingp(pdfz)+b*nsingpreg(pdfz,pdf1,z)) 
     1                 +c*nsingp(pdf1)
c       int2nspNNLO_Z=(1d0-x-eps)*(a*nsing(z)+b*nsingreg(z)) 
c     1                 +c*nsing(1d0)
      else
      int2nspnnlo_z=(1d0-x-eps)*(a*z**(nmell-1)+b*(z**(nmell-1)-1d0))+
     1 c
      endif
      int2nspnnlo_z=4d0*int2nspnnlo_z
      return
      end
 
      double precision function Int2qNNLO_Z(z,x,lf,lr,pdf1,pdfz)
      implicit none
      integer nf
       integer mchk, nmell
       common/mell_check/mchk
       common/mellin/nmell
      double precision z,x,lf,lr,beta0,eps
      double precision c2nnlonsa_nc, c2nnlonsc_nc,c2nnlonsb_nc,
     1 c2nnlosa
      double precision sing, singreg
      double precision c2nloa, c2nlob,c2nloc
      double precision x1nspa,x1nsb,x1nsc,xq0c2qa,xq0cqb,xq0cqc
      double precision xq0q0a,xq0q0b,xq0q0c,p0qqa,p0qqb,p0qqc
      double precision xq0c2ga, xqg0gq0a, x1psa
      double precision a,b,c 
      common/nflav/nf
      common/prec/eps 
      double precision pdf1(-6:6),pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=
     1 c2nnlonsa_nc(z) 
     2 +c2nnlosa(z)/2d0
     1 +lf*(x1nspa(z)+xq0c2qa(z)-beta0*c2nloa(z) )
     2 +lf**2/2d0 *(xq0q0a(z)-beta0*p0qqa(z) )
     3 +lr*beta0*(c2nloa(z)+lf*p0qqa(z))
     4 +lf*(x1psa(z)+xq0c2ga(z))/2d0
     5 +lf**2/2d0*(xqg0gq0a(z))/2d0

      b=0d0
      c=0d0
      b=c2nnlonsb_nc(z)
     1 +lf*(x1nsb(z)+xq0cqb(z)- beta0*c2nlob(z) )
     1 +lf**2/2d0*(xq0q0b(z)-beta0*p0qqb(z) )
     2 +lr*beta0*(c2nlob(z)+lf*p0qqb(z))

      c=c2nnlonsc_nc(x) 
     1 +lf*(x1nsc(x)+xq0cqc(x)-beta0*c2nloc(x)) 
     2 + lf**2/2d0*(xq0q0c(x)-beta0*p0qqc(x) )
     3 +lr*beta0*(c2nloc(x)+lf*p0qqc(x))

      if( mchk.ne.1) then     
      int2qnnlo_z=(1d0-x-eps)/z*(a*sing(pdfz)+b*singreg(pdfz,pdf1,z))+
     1 c *sing(pdf1)
      else
      int2qnnlo_z=(1d0-x-eps)*(a*z**(nmell-1)+b*(z**(nmell-1)-1d0))+
     1 c
      endif
      int2qnnlo_z=int2qnnlo_z*4d0

      return
      end

      double precision function Int2gNNLO(z,x,lf,lr,pdf1,pdfz)
      implicit none
      integer nf,v
      common/vect/v
        integer mchk, nmell
       common/mell_check/mchk
       common/mellin/nmell
      double precision z,x,lf,lr,beta0,eps
      double precision c2nnloga, c2nnlogc,pdfg
      double precision x1qga, xg0c2qa, xg0c2ga, c2nlog,xq0qg0a
      double precision xg0qg0a, p0qga
      double precision a,b,c 
      common/nflav/nf
      common/prec/eps 
      double precision pdf1(-6:6),pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=c2nnloga(z) 
     1 + lf*(x1qga(z)+xg0c2qa(z)+xg0c2ga(z) - beta0*2d0*c2nlog(z))
     2 + lf**2/2d0*(xq0qg0a(z)+xg0qg0a(z) - beta0*2d0*p0qga(z))
     3 + lr*beta0*2d0*(c2nlog(z)+lf*p0qga(z))

      c=c2nnlogc(x) 
      if( mchk.ne.1) then     
      int2gNNLO=(1d0-x-eps)/z*(a*pdfg(pdfz)) 
     1                 +c*pdfg(pdf1)
      int2gnnlo=int2gnnlo/2d0
      if (v.eq.3) then
      int2gnnlo=int2gnnlo*4d0
      endif
      else
      int2gnnlo=((1d0-x-eps)*(a*z**(nmell-1))+
     1 c)*nf
      endif

      return
      end
 
      double precision function Int2nsmNNLO_W(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision z,x,lf,lr,beta0,eps
      double precision c2nnlonsa, c2nnlonsc,c2nnlonsb, delta, deltareg
      double precision c2nloa, c2nlob,c2nloc
      double precision x1nsma,x1nsb,x1nsc,xq0c2qa,xq0cqb,xq0cqc
      double precision xq0q0a,xq0q0b,xq0q0c,p0qqa,p0qqb,p0qqc
      double precision a,b,c
      integer mchk, nmell 
      common/mell_check/mchk
      common/mellin/nmell
      common/prec/eps 
      double precision pdf1(-6:6),pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=c2nnlonsa(z) 
     1 +lf*(x1nsma(z)+xq0c2qa(z)-beta0*c2nloa(z) )
     2 +lf**2/2d0 *(xq0q0a(z)-beta0*p0qqa(z) )
     3 +lr*beta0*(c2nloa(z)+lf*p0qqa(z))

      b=c2nnlonsb(z)
     1 +lf*(x1nsb(z)+xq0cqb(z)- beta0*c2nlob(z) )
     1 +lf**2/2d0*(xq0q0b(z)-beta0*p0qqb(z) )
     2 +lr*beta0*(c2nlob(z)+lf*p0qqb(z))

      c=c2nnlonsc(x) 
     1 +lf*(x1nsc(x)+xq0cqc(x)-beta0*c2nloc(x)) 
     2 + lf**2/2d0*(xq0q0c(x)-beta0*p0qqc(x) )
     3 +lr*beta0*(c2nloc(x)+lf*p0qqc(x))
      if( mchk.ne.1) then     
      int2nsmNNLO_W=
     1 (1d0-x-eps)/z*(a*delta(pdfz)+b*deltareg(pdfz,pdf1,z)) 
     1                 +c*delta(pdf1)
      else
      int2nsmnnlo_w=(1d0-x-eps)*(a*z**(nmell-1)+b*(z**(nmell-1)-1d0))+
     1 c
      endif
       int2nsmnnlo_w=int2nsmnnlo_w/8d0
      return
      end
 
      double precision function Int2qNNLO_W(z,x,lf,lr,pdf1,pdfz)
      implicit none
      integer nf
      double precision z,x,lf,lr,beta0,eps
      double precision c2nnlonsa_nc, c2nnlonsc_nc,c2nnlonsb_nc, c2nnlosa
      double precision sing, singreg
      double precision c2nloa, c2nlob,c2nloc
      double precision x1nspa,x1nsb,x1nsc,xq0c2qa,xq0cqb,xq0cqc
      double precision xq0q0a,xq0q0b,xq0q0c,p0qqa,p0qqb,p0qqc
      double precision xq0c2ga, xqg0gq0a,x1psa
      double precision a,b,c 
      integer mchk, nmell
      common/mell_check/mchk
      common/mellin/nmell
      common/nflav/nf
      common/prec/eps 
      double precision pdf1(-6:6),pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=
     1  c2nnlonsa_nc(z) 
     1 +c2nnlosa(z)*nf
     1 +lf*(x1nspa(z)+xq0c2qa(z)-beta0*c2nloa(z) )
     2 +lf**2/2d0 *(xq0q0a(z)-beta0*p0qqa(z) )
     3 +lr*beta0*(c2nloa(z)+lf*p0qqa(z))
     4 +lf*(x1psa(z)+xq0c2ga(z))*nf
     5 +lf**2/2d0*(xqg0gq0a(z))*nf
       b=0d0
      b=c2nnlonsb_nc(z)
     1 +lf*(x1nsb(z)+xq0cqb(z)- beta0*c2nlob(z) )
     1 +lf**2/2d0*(xq0q0b(z)-beta0*p0qqb(z) )
     2 +lr*beta0*(c2nlob(z)+lf*p0qqb(z))
       c=0d0
      c=c2nnlonsc_nc(x) 
     1 +lf*(x1nsc(x)+xq0cqc(x)-beta0*c2nloc(x)) 
     2 + lf**2/2d0*(xq0q0c(x)-beta0*p0qqc(x) )
     3 +lr*beta0*(c2nloc(x)+lf*p0qqc(x))

      if( mchk.ne.1) then     
      int2qnnlo_w=(1d0-x-eps)/z*(a*sing(pdfz)+b*singreg(pdfz,pdf1,z))+
     1 c *sing(pdf1)
      else
      int2qnnlo_w=(1d0-x-eps)*(a*z**(nmell-1)+b*(z**(nmell-1)-1d0))+
     1 c
      endif
      int2qnnlo_w=int2qnnlo_w/8d0
      return
      end

c-------------------------------------------------------------
      double precision function F2(ic,x,q,muf,mur,ord,v,z,pdfz,pdf1)
      implicit none
      double precision x,q,z,nsing,alphaspdf,pi,eps
      integer ord, nf,v,i,vv,ic,iic
      double precision int2nloq,int2nlog
      double precision int2nspnnlo_z,int2qnnlo_z,int2gnnlo,
     1 int2nsmnnlo_w,int2qnnlo_w
      double precision muf,mur,lf,lr
      common/prec/eps
      double precision xx,qq
      common/pdfpar/xx,qq,iic
      common/nflav/nf
      common/vect/vv
      integer f3c
      common/f3call/f3c
      double precision pdf1(-6:6),pdfz(-6:6)
      lf=2d0*log(q/muf)
      lr=2d0*log(mur/muf)

      f3c=0

      if(v.le.2) then
      nf=4
      else
      nf=5
      endif
      iic=ic
      xx=x
      qq=muf
      vv=v
      if(1d0-x.lt.eps) then
      f2=0d0
      else
      pi=4d0*datan(1d0)
      if (ord.eq.1) then
      F2=x*nsing(pdf1)

      else if (ord.eq.2) then
      f2=alphaspdf(mur)/(4d0*pi)*x*(
     1 int2nloq(z,x,lf,lr,pdf1,pdfz)
     1 + int2nlog(z,x,lf,lr,pdf1,pdfz) 
     1 )
     
      else if (ord.eq.3) then
      if (v.eq.1) then
         f2=(alphaspdf(mur)/(4d0*pi))**2 *x*(
     1           int2nsmnnlo_w(z,x,lf,lr,pdf1,pdfz)  !checked
     2          +int2qnnlo_w(z,x,lf,lr,pdf1,pdfz)
     3          +int2gnnlo(z,x,lf,lr,pdf1,pdfz)  !checked
     4          )       
       else if (v.eq.2) then
         f2=(alphaspdf(mur)/(4d0*pi))**2 *x*(
     1          - int2nsmnnlo_w(z,x,lf,lr,pdf1,pdfz)  !checked
     2          +int2qnnlo_w(z,x,lf,lr,pdf1,pdfz)
     3          +int2gnnlo(z,x,lf,lr,pdf1,pdfz)  !checked
     4          )
       else if (v.eq.3) then    
        f2=(alphaspdf(mur)/(4d0*pi))**2 *x*(
     1           int2nspnnlo_z(z,x,lf,lr,pdf1,pdfz)  !checked??
     2          +int2qnnlo_z(z,x,lf,lr,pdf1,pdfz)
     3          +int2gnnlo(z,x,lf,lr,pdf1,pdfz)  !checked
     4          )/4d0 
       endif 
      endif 
      endif
      return
      end
 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c--------------------------------------------------------------------------
c------------------------   F3   ------------------------------------------

      double precision function C3NLOc(z)
      implicit none
      double precision z,c2nloc
      c3nloc=c2nloc(z)
      return
      end   

      double precision function C3NLOb(z)
      implicit none
      double precision z,c2nlob
      c3nlob=c2nlob(z)
      return
      end

      double precision function C3NLOa(z)
      implicit none
      double precision z, c2nloa
      c3nloa=c2nloa(z)-4d0/3d0 *(1d0+z)*2d0
      return
      end

      DOUBLE PRECISION  FUNCTION C3NNLOminA (Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
      nf=5
      DL  = LOG (Y)
      DL1 = LOG (1.D0-Y)
      C3NNLOminA = 
     1          - 206.1D0 - 576.8D0 * Y
     2          - 3.922D0 * DL**3 - 33.31D0 * DL**2 - 67.60D0 * DL 
     3          - 15.20D0 * DL1**3 + 94.61D0 * DL1**2 - 409.6D0 * DL1
     4          - 147.9D0 * DL * DL1**2 
     5          + NF * ( - 6.337D0 - 14.97D0 * Y 
     6          + 2.207D0 * DL**2 + 8.683D0 * DL 
     7          + 0.042D0 * DL1**3 - 0.808D0 * DL1**2 + 25.00D0 * DL1
     8          + 9.684D0 * DL * DL1 )     
      RETURN
      END

      DOUBLE PRECISION FUNCTION C3NNLOplsA (Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
      nf=5
      DL  = LOG (Y)
      DL1 = LOG (1.-Y)
      C3NNLOplsA = 
     1          - 242.9D0 - 467.2D0 * Y
     2          - 3.049D0 * DL**3 - 30.14D0 * DL**2 - 79.14D0 * DL 
     3          - 15.20D0 * DL1**3 + 94.61D0 * DL1**2 - 396.1D0 * DL1
     4          - 92.43D0 * DL * DL1**2 
     5          + NF * ( - 6.337D0 - 14.97D0 * Y 
     6          + 2.207D0 * DL**2 + 8.683D0 * DL 
     7          + 0.042D0 * DL1**3 - 0.808D0 * DL1**2  + 25.00D0 * DL1
     8          + 9.684D0 * DL * DL1 )     
      RETURN
      END

      DOUBLE PRECISION  FUNCTION C3NNLOminC (Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
      nf=5
      DL1 = LOG (1.D0-Y)
      C3NNLOminC = 
     1    + 3.55555D0 * DL1**4 - 20.4444D0 * DL1**3 - 15.5525D0 * DL1**2
     2        + 188.64D0 * DL1 - 338.531D0 - 0.104D0 
     3        + NF * (0.592593D0 * DL1**3 - 4.2963D0 * DL1**2 
     4        + 6.3489D0 * DL1 + 46.844D0 + 0.013D0)
      RETURN
      END

      DOUBLE PRECISION FUNCTION C3NNLOplsC (Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
      nf=5
      DL1  = LOG (1D0-Y)
      C3NNLOplsC = 
     1  + 3.55555D0 * DL1**4 - 20.4444D0 * DL1**3 - 15.5525D0 * DL1**2
     2        + 188.64D0 * DL1 - 338.531D0 - 0.152D0 
     3        + NF * (0.592593D0 * DL1**3 - 4.2963D0 * DL1**2 
     4        + 6.3489D0 * DL1 + 46.844D0 + 0.013D0)
      RETURN
      END

      DOUBLE PRECISION FUNCTION C3NNLOB(Y)
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NF
      nf=5
      DL1 = LOG (1.-Y)
      DM  = 1./(1.-Y) 
      C3NNLOB = 
     1   + 14.2222D0 * DL1**3 - 61.3333D0 * DL1**2 - 31.105D0 * DL1 
     2          + 188.64D0 
     3        + NF * ( 1.77778D0 * DL1**2 - 8.5926D0 * DL1 + 6.3489D0 ) 
      C3NNLOB = DM * C3NNLOB      
      RETURN
      END

c-----NLO convolutions      
      double precision function Int3NLOq(z,x,lf,lr,pdf1,pdfz)
      implicit none
      doubLe precision z,x,eps,lf,lr
      common/prec/eps 
      double precision pdf1(-6:6), pdfz(-6:6)

      double precision c3nloa,c3nlob,c3nloc
      double precision p0qqa,p0qqb,p0qqc
      double precision nsing,nsingreg
      double precision a,b,c
      integer mchk, nmell
      common/mell_check/mchk
      common/mellin/nmell

      a=c3nloa(z)+p0qqa(z)*lf
      b=c3nlob(z)+p0qqb(z)*lf
      c=c3nloc(x)+p0qqc(x)*lf   
      if( mchk.ne.1) then     
      int3nloq=(1d0-x-eps)/z*(a*nsing(pdfz)+b*nsingreg(pdfz,pdf1,z))+
     1 c *nsing(pdf1)
      else
      int3nloq=(1d0-x-eps)*(a*z**(nmell-1)+b*(z**(nmell-1)-1d0))+
     1 c 
      endif

      return
      end
      
c-----NNLO convolutions
c-----neutral current
      double precision function Int3nsNNLO_Z(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision z,x,lf,lr,beta0,eps
      double precision c3nnlomina, c3nnlominc,c3nnlob,
     1 nsing, nsingreg
      double precision c3nloa, c3nlob,c3nloc
      double precision x1nsma,x1nsb,x1nsc,xq0c3qa,xq0cqb,xq0cqc
      double precision xq0q0a,xq0q0b,xq0q0c,p0qqa,p0qqb,p0qqc
      double precision a,b,c 
       integer mchk, nmell
       common/mell_check/mchk
       common/mellin/nmell
      common/prec/eps 
      double precision pdf1(-6:6), pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=c3nnlomina(z) 
     1 +lf*(x1nsma(z)+xq0c3qa(z)-beta0*c3nloa(z) )
     2 +lf**2/2d0 *(xq0q0a(z)-beta0*p0qqa(z) )
     3 +lr*beta0*(c3nloa(z)+lf*p0qqa(z))

      b=c3nnlob(z)
     1 +lf*(x1nsb(z)+xq0cqb(z)- beta0*c3nlob(z) )
     1 +lf**2/2d0*(xq0q0b(z)-beta0*p0qqb(z) )
     2 +lr*beta0*(c3nlob(z)+lf*p0qqb(z))

      c=c3nnlominc(x) 
     1 +lf*(x1nsc(x)+xq0cqc(x)-beta0*c3nloc(x)) 
     2 + lf**2/2d0*(xq0q0c(x)-beta0*p0qqc(x) )
     3 +lr*beta0*(c3nloc(x)+lf*p0qqc(x))
      if( mchk.ne.1) then     
      int3nsNNLO_Z=(1d0-x-eps)/z*(a*nsing(pdfz)+b*nsingreg(pdfz,pdf1,z)) 
     1                 +c*nsing(pdf1)
      else
      int3nsnnlo_z=(1d0-x-eps)*(a*z**(nmell-1)+b*(z**(nmell-1)-1d0))+
     1 c
      endif

      return
      end

c-----charged current
      double precision function Int3nspNNLO_W(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision z,x,lf,lr,beta0,eps
      double precision c3nnloplsa, c3nnloplsc,c3nnlob,
     1 delta, deltareg
      double precision c3nloa, c3nlob,c3nloc
      double precision x1nspa,x1nsb,x1nsc,xq0c3qa,xq0cqb,xq0cqc
      double precision xq0q0a,xq0q0b,xq0q0c,p0qqa,p0qqb,p0qqc
      double precision a,b,c 
       integer mchk, nmell
       common/mell_check/mchk
       common/mellin/nmell
      common/prec/eps 
      double precision pdf1(-6:6), pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=c3nnloplsa(z) 
     1 +lf*(x1nspa(z)+xq0c3qa(z)-beta0*c3nloa(z) )
     2 +lf**2/2d0 *(xq0q0a(z)-beta0*p0qqa(z) )
     3 +lr*beta0*(c3nloa(z)+lf*p0qqa(z))

      b=c3nnlob(z)
     1 +lf*(x1nsb(z)+xq0cqb(z)- beta0*c3nlob(z) )
     1 +lf**2/2d0*(xq0q0b(z)-beta0*p0qqb(z) )
     2 +lr*beta0*(c3nlob(z)+lf*p0qqb(z))

      c=c3nnloplsc(x) 
     1 +lf*(x1nsc(x)+xq0cqc(x)-beta0*c3nloc(x)) 
     2 + lf**2/2d0*(xq0q0c(x)-beta0*p0qqc(x) )
     3 +lr*beta0*(c3nloc(x)+lf*p0qqc(x))
      if( mchk.ne.1) then     
      int3nspNNLO_W=
     1 (1d0-x-eps)/z*(a*delta(pdfz)+b*deltareg(pdfz,pdf1,z)) 
     1                 +c*delta(pdf1)
      else
      int3nspnnlo_w=(1d0-x-eps)*(a*z**(nmell-1)+b*(z**(nmell-1)-1d0))+
     1 c
      endif
      int3nspnnlo_w=int3nspnnlo_w/8d0
      return
      end    


      double precision function Int3nsNNLO_W(z,x,lf,lr,pdf1,pdfz)
      implicit none
      double precision z,x,lf,lr,beta0,eps
      double precision c3nnlomina, c3nnlominc,c3nnlob,
     1 val,valreg
      double precision c3nloa, c3nlob,c3nloc
      double precision x1nsma,x1nsb,x1nsc,xq0c3qa,xq0cqb,xq0cqc
      double precision xq0q0a,xq0q0b,xq0q0c,p0qqa,p0qqb,p0qqc
      double precision a,b,c  
       integer mchk, nmell
       common/mell_check/mchk
       common/mellin/nmell
      common/prec/eps 
      double precision pdf1(-6:6), pdfz(-6:6)

      beta0=11d0-2d0*5d0/3d0
c----- a-> regular, b->singular, c->local
      a=c3nnlomina(z) 
     1 +lf*(x1nsma(z)+xq0c3qa(z)-beta0*c3nloa(z) )
     2 +lf**2/2d0 *(xq0q0a(z)-beta0*p0qqa(z) )
     3 +lr*beta0*(c3nloa(z)+lf*p0qqa(z))

      b=c3nnlob(z)
     1 +lf*(x1nsb(z)+xq0cqb(z)- beta0*c3nlob(z) )
     1 +lf**2/2d0*(xq0q0b(z)-beta0*p0qqb(z) )
     2 +lr*beta0*(c3nlob(z)+lf*p0qqb(z))

      c=c3nnlominc(x) 
     1 +lf*(x1nsc(x)+xq0cqc(x)-beta0*c3nloc(x)) 
     2 + lf**2/2d0*(xq0q0c(x)-beta0*p0qqc(x) )
     3 +lr*beta0*(c3nloc(x)+lf*p0qqc(x))
      if( mchk.ne.1) then     
      int3nsNNLO_W=(1d0-x-eps)/z*(a*val(pdfz)+b*valreg(pdfz,pdf1,z)) 
     1                 +c*val(pdf1)
      else
      int3nsnnlo_w=(1d0-x-eps)*(a*z**(nmell-1)+b*(z**(nmell-1)-1d0))+
     1 c
      endif
      int3nsnnlo_w=int3nsnnlo_w/8d0

      return
      end

c-----------------------------------------------------------
      double precision function F3(ic,x,q,muf,mur,ord,v,z,pdfz,pdf1)
      implicit none
      double precision x,q,z,nsing,nsingp,alphaspdf,pi,eps
      integer ord, nf,v,i,vv,ic,iic
      double precision int3nloq
      double precision int3nsnnlo_z, int3nspnnlo_w,int3nsnnlo_w
      double precision muf,mur,lf,lr
      common/prec/eps
      double precision xx,qq
      common/pdfpar/xx,qq,iic
      common/nflav/nf
      common/vect/vv
      integer f3c
      common/f3call/f3c
      double precision pdf1(-6:6), pdfz(-6:6)
      lf=2d0*log(q/muf)
      lr=2d0*log(mur/muf)
  
      f3c=1

      if(v.le.2) then
      nf=4
      else
      nf=5
      endif
      iic=ic
      xx=x
      qq=muf
      vv=v
      if(1d0-x.lt.eps) then
      f3=0d0
      else

      if (v.eq.4) then
      f3=0d0
      else
      pi=4d0*datan(1d0)

      if (ord.eq.1) then
      F3=nsing(pdf1)

      else if (ord.eq.2) then
       f3=alphaspdf(mur)/(4d0*pi)*(
     1  int3nloq(z,x,lf,lr,pdf1,pdfz)
     4    )

      else if (ord.eq.3) then
      if (v.eq.1) then
          f3=(alphaspdf(mur)/(4d0*pi))**2 *(
     1   int3nsnnlo_w(z,x,lf,lr,pdf1,pdfz)
     2   +int3nspnnlo_w(z,x,lf,lr,pdf1,pdfz)
     3   )     
        else if (v.eq.2) then
         f3=(alphaspdf(mur)/(4d0*pi))**2 *(
     1   int3nsnnlo_w(z,x,lf,lr,pdf1,pdfz)
     2   -int3nspnnlo_w(z,x,lf,lr,pdf1,pdfz)
     3   )
        else if (v.eq.3) then
        f3=(alphaspdf(mur)/(4d0*pi))**2 *(
     1   int3nsnnlo_z(z,x,lf,lr,pdf1,pdfz)
     3   )
       end if

      endif 
      
      endif
      endif
      return
      end

