c-----The s-channel contribute to the elevtroweak higgs production
c-----The computation is basically the Higgs-strahlung process, times the
c-----BR of the vector into hadrons.
c-----QCD corrections implemented are the ones or the DY process
c-----(initial state only). The vector is produced on shell.
c-----see the Djouadi review, hep-ph/0503172 pages 123 and following
c-----NO CKM mixing between quarks, NO top quark
c----------------------------------------------------------------------
      subroutine setcouplingsdy()
c-----creates the couplig matrix, no CKM, no top
      implicit none
      integer i,j,k,nf
      double precision sthw,upz,dnz
      double precision couplingsdy(-6:6,-6:6,3),couplingsdyg(-6:6,3)
      common/coupldy/couplingsdy,couplingsdyg
      common/sin_wein/sthw
      common/nflav/nf
c-----set everything to zero
      do i=-6,6
        do j= -6,6
          do k=1,3
          couplingsdy(i,j,k)=0d0
          enddo
          couplingsdyg(j,k)=0d0
        enddo
      enddo
c-----neutral current
      upz=(1d0-8d0/3d0*sthw)**2+1d0
      dnz=(-1d0+4d0/3d0*sthw)**2+1d0
      do i=-nf,nf
       if (i.ne.0) then
        if (mod(i,2).eq.0)then
         couplingsdy(i,-i,3)=upz
         couplingsdyg(i,3)=upz
         else
         couplingsdy(i,-i,3)=dnz
         couplingsdyg(i,3)=dnz
        endif
       endif
      enddo
c-----charged currents
c-----W-
      do i=1,2
        couplingsdy(2*i-1,-2*i,1)=4d0
        couplingsdy(-2*i,2*i-1,1)=4d0
        couplingsdyg(2*i-1,1)=4d0
        couplingsdyg(-2*i,1)=4d0
      enddo
c-----W+
      do i=1,2
        couplingsdy(2*i,-2*i+1,2)=4d0
        couplingsdy(-2*i+1,2*i,2)=4d0
        couplingsdyg(2*i,2)=4d0
        couplingsdyg(-2*i+1,2)=4d0
      enddo
      return
      end


      double precision function lambda(x,y,z)
c-----the two body phase-space "triangle" function
      implicit none
      double precision x,y,z
      lambda=(1d0-x/z-y/z)**2-4d0*x*y/z**2
      return
      end


      double precision function integrand_s_top(xx)
      implicit none
      double precision xx(3), par(3)
      double precision tauzero,jac
      double precision top, s, sqrts, gevtopb,mv
      integer vec,ord
      double precision mh, mw,mz
      common/masses/mw,mz
      common/hmass/mh
      common/energy/sqrts
      common/conv/gevtopb
      common/pert_ord/ord
      common/tau0/tauzero
c----- par contains tau, -log(x) and z (the argument of the convolutions in the
c       coefficient function beyond the LO)
      s=sqrts**2
      mv=mz

      tauzero=(mv+mh)**2/s
      par(1)=tauzero+xx(1)*(1d0-tauzero)
      par(2)=-dlog(par(1))*xx(2)
      
      jac=-(1d0-tauzero)*dlog(par(1))
      
      integrand_s_top=jac*top(par)*gevtopb
      return
      end

      double precision function top(par)
c-----cheched with paolo bolzoni, it's ok (2010 03 17)      
      implicit none
      integer ic,i
      double precision par(3)
      double precision const,gf, ps,sthw,pi,muf
      double precision shat, sqrtshat,sqrts,s
      double precision mw,mz,wwdth,zwdth,mt,mb,mh
      double precision f1(-6:6), f2(-6:6),pdf,upcp, dncp
      double precision alphaspdf
      double complex c0
      common/qmass/mt,mb
      common/masses/mw,mz
      common/widths/wwdth,zwdth
      common/hmass/mh
      common/energy/sqrts
      common/gfermi/gf
      common/sin_wein/sthw
      common/collider/ic
      double precision tau,x
      tau=par(1)
      x=dexp(-par(2))

      s=sqrts**2
      shat=tau*s
      pi=4d0*datan(1d0)

      muf=mh
      call evolvepdf(x,muf,f1)
      call evolvepdf(tau/x,muf,f2)

c      const=alphaspdf(mh)**2*gf**2/32d0/pi**2
c flux     
c      const=const/2d0/shat
c phase space
c      const=const*sqrt(1d0-(mh+mz)**2/shat)*sqrt(1d0-(mh-mz)**2/shat)
c     1 /8d0/pi
       const=alphaspdf(mh+mz)**2*gf**2/(512d0*pi**3*shat)*
     1   sqrt(1d0-(mh+mz)**2/shat)*sqrt(1d0-(mh-mz)**2/shat)
      top=(mh**4-2d0*(mz**2+shat)*mh**2+(mz**2-shat)**2)
     1  *abs( mb**2 *C0(0d0,0d0,shat,mb**2,mb**2,mb**2)
     4      -mt**2 *C0(0d0,0d0,shat,mt**2,mt**2,mt**2) )**2

      top=top*const*f1(0)*f2(0)/tau

      return
      end


      double precision function integrand_sch(xx)
      implicit none
      double precision xx(3), par(3)
      double precision tauzero,jac
      double precision s_channel, s, sqrts, gevtopb,mv
      integer vec,ord
      double precision mh, mw,mz
      common/masses/mw,mz
      common/hmass/mh
      common/energy/sqrts
      common/conv/gevtopb
      common/pert_ord/ord
      common/tau0/tauzero
c----- par contains tau, -log(x) and z (the argument of the convolutions in the
c       coefficient function beyond the LO)
      s=sqrts**2
      integrand_sch=0d0
      do vec=3,3
      if (vec.lt.3) then
      mv=mw
      else
      mv=mz
      endif

      tauzero=(mv+mh)**2/s
      par(1)=tauzero+xx(1)*(1d0-tauzero)
      par(2)=-dlog(par(1))*xx(2)
      
      jac=-(1d0-tauzero)*dlog(par(1))
      
      integrand_sch=integrand_sch+jac*s_channel(par,ord,vec)
      enddo
      integrand_sch=integrand_sch*gevtopb
      return
      end

      double precision function s_channel(par,ord,vec)
c-----as for libstructf, vec=1,2,3 corresponds respectively to W-,W+,Z0
      implicit none 
      integer vec, ord,nf
      double precision par(3),tau,x,z,muf,mur,mh,pi
      double precision sigmahatlo,qqblum,qglum
      double precision delta1qqb,delta1qg, alphaspdf
      common/nflav/nf
      common/hmass/mh
      common/pi_gr/pi
      tau=par(1)
      x=dexp(-par(2))

      if (vec.lt.3)then
       nf=4
      else
       nf=5
      endif

      if (ord.eq.1)then
        muf=mh
        s_channel=qqblum(tau,x,muf,vec)*sigmahatlo(tau,vec)
c        write(*,*)qqblum(tau,x,muf,vec),sigmahatlo(tau,vec)

      else if (ord.eq.2)then
        muf=mh
        mur=mh
        s_channel=alphaspdf(mur)/4d0/pi * (
     1  qqblum(tau,x,muf,vec)*delta1qqb(z,tau,vec,muf,mur)
     1  +qglum(tau,x,muf,vec)*delta1qg(z,tau,vec,muf,mur)
     2  )

      endif
      return
      end

      double precision function sigmahatlo(tau,vec)
c-----the partonic LO cros section, which is convoluted with the
c-----coefficient function
      implicit none
      integer vec
      double precision tau, mv
      double precision gf, mh,pi,mw,mz
      double precision sqrts,s,shat
      double precision lambda
      common/masses/mw,mz
      common/hmass/mh
      common/gfermi/gf
      common/pi_gr/pi
      common/energy/sqrts
      s=sqrts**2
      shat=tau*s
      if (vec.lt.3)then
       mv=mw
      else
       mv=mz
      endif
      if (lambda(mv**2,mh**2,shat).lt.0d0) then
              write(*,*) "AAA"
      endif
      if (shat.lt.(mv+mh)**2) then
              write(*,*)"BBB"
      endif

      sigmahatlo=gf**2*mv**4/288/pi/shat*sqrt(lambda(mv**2,mh**2,shat))
     1 *(lambda(mv**2,mh**2,shat) + 12d0*mv**2/shat)/(1d0-mv**2/shat)**2
      return
      end


      double precision function delta1qqb(zz,tau,vec,muf,mur)
      implicit none
      integer vec
      double precision sigmahatlo
      double precision zz,tau,tauzero,z,z0,pi
      double precision a, b,b0,c,d
      double precision sqrts,muf, mur, lf, lr
      common/pi_gr/pi
      common/tau0/tauzero
c-----a is the regular part, b the singular part,
c----- b0 the subtraction of the s.p. when the lower bound is not 0,
c----- c is the local part      
      z=tauzero/tau+(1d0-tauzero/tau*zz)
      z0=tauzero/tau*zz
      lf=2d0*dlog(z*tau*sqrts/muf)
      lr=2d0*dlog(z*tau*sqrts/mur)
      
      a=4d0/3d0*(-4d0*(1d0+z)*lf- 8d0*(1+z)*dlog(1d0-z) -
     1         4d0*(1d0+z**2)/(1d0-z) *dlog(z))
      b=4d0/3d0*(8d0*D(0,z)*lf+16d0*D(1,z))
      b0=4d0/3d0*(8d0*D(0,z0)*lf+16d0*D(1,z0))
      c=4d0/3d0*(6d0*lf+8d0*pi**2/6d0-16d0)

      delta1qqb=
     1 (1d0-tauzero/tau)*a*sigmahatlo(z*tau,vec)
     2 +(1d0-tauzero/tau)*b*(sigmahatlo(z*tau,vec)-sigmahatlo(tau,vec))
     3 -tauzero/tau *b0* sigmahatlo(tau,vec)
     4 +c*sigmahatlo(tau,vec)

      return 
      end

      double precision function delta1qg(zz,tau,vec,muf,mur)
      implicit none
      integer vec
      double precision sigmahatlo
      double precision zz,tau,tauzero,z,z0,pi
      double precision a, b,b0,c
      double precision sqrts,muf, mur, lf, lr
      common/pi_gr/pi
      common/tau0/tauzero
c-----a is the regular part, b the singular part,
c----- b0 the subtraction of the s.p. when the lower bound is not 0,
c----- c is the local part      
      z=tauzero/tau+(1d0-tauzero/tau*zz)
      z0=tauzero/tau*zz
      lf=2d0*dlog(z*tau*sqrts/muf)
      lr=2d0*dlog(z*tau*sqrts/mur)
      
      a=1d0/2d0*(2d0*(1d0+2d0*z**2-2d0*z)*(lf+dlog((1d0-z)**2/z))
     1        +1d0-7d0*z**2+6d0*z)
      b=0d0
      b0=0d0
      c=0d0

      delta1qg=
     1 (1d0-tauzero/tau)*a*sigmahatlo(z*tau,vec)
     2 +(1d0-tauzero/tau)*b*(sigmahatlo(z*tau,vec)-sigmahatlo(tau,vec))
     3 -tauzero/tau *b0* sigmahatlo(tau,vec)
     4 +c*sigmahatlo(tau,vec)

      return 
      end

      double precision function D(i,z)
      implicit none
      integer i
      double precision z
      D=(dlog(1d0-z)**i)/(1d0-z)
      return
      end

      double precision function qqblum(tau,x,muf,vec)
c-----quark-antiquark luminosity
      implicit none
      double precision tau, x,muf, f1(-6:6), f2(-6:6)
      integer icoll,i,j,nf,vec
      double precision couplingsdy(-6:6,-6:6,3),couplingsdyg(-6:6,3)
      common/coupldy/couplingsdy,couplingsdyg
      common/collider/icoll
      common/nflav/nf

      if(x.lt.tau) then
      
      write(*,*)x,tau,"convolution error"
      else
      qqblum=0d0
      call setcouplingsdy()

      call evolvepdf(x,muf,f1)
      call evolvepdf(tau/x,muf,f2)
      do i= -nf,nf
      do j= -nf,nf
        if(i*j .ne. 0) then
       qqblum=qqblum+couplingsDY(i,j,vec)*f1(i)*f2(icoll*j)
        endif
      enddo
      enddo
    
      endif
      qqblum=qqblum/tau
      return
      end

      double precision function qglum(tau,x,muf,vec)
c-----(anti)quark-gluon luminosity
      implicit none
      double precision tau, x,muf, f1(-6:6), f2(-6:6)
      integer icoll,i,nf,vec
      double precision couplingsdy(-6:6,-6:6,3),couplingsdyg(-6:6,3)
      common/collider/icoll
      common/nflav/nf
      common/coupldy/couplingsdy,couplingsdyg

      if(x.lt.tau) then
      
      write(*,*)x,tau,"convolution error"
      else
      qglum=0d0
      call setcouplingsdy()

      call evolvepdf(x,muf,f1)
      call evolvepdf(tau/x,muf,f2)
      do i= -nf,nf
        if(i .ne. 0) then
       qglum=qglum+couplingsDYg(i,vec)*
     1 (f1(i)*f2(0)+f1(0)*f2(icoll*i))
        endif
      enddo
    
      endif
      qglum=qglum/tau
      return
      end


