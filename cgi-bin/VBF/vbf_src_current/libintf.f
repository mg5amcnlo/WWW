      double precision function intf(par)
      implicit none
      double precision intf_1, intf_2, intf_3, intf_4, intf_5
      double precision par(6),mv1,mv2,mh,x1,x2,z1,z2,zh,zmh
      double precision cth,sth,cth12,sth12,phi,pi
      double precision props,f1f2, i1i2,f1i1,f1i2,f2i1,f2i2,sqrts,s
      double precision const,gf, ps
      double precision shat, sqrtshat
      double precision mw,mz,wwdth,zwdth
      double precision f1(-6:6), f2(-6:6),pdf,upcp, dncp,sthw
      integer isch,ic,schintf
      common/schannel/isch
      common/pi_gr/pi
      common/masses/mw,mz
      common/widths/wwdth,zwdth
      common/hmass/mh
      common/energy/sqrts
      common/gfermi/gf
      common/sin_wein/sthw
      common/collider/ic
      common/s_interf/schintf
      integer i
      integer iproc
      common/iproc/iproc
      double precision couplvv(3,3)
      common/coupl_vv/couplvv

c-----NOTE: in all the propagators the widhs are set to 0, except for
c     the s-channel propagator (in the process udb->udbh), where the w
c     width is needed. When checking with MG some discrepancies 
c     (up to few %s) can occour on the interference


c-----the phase space parameterization is the one of e+e- ->q qbar g
c-----(see e.g. peskin, schroeder, Final project I)
c----- the z-axis is taken in the direction of f1
c----- th is the angle btw the incoming partons direction and the z-axis
c----- th12 is the angle btw f1,f2, fixed by Carnot's theorem
      intf_1=0d0
      intf_2=0d0
      intf_3=0d0
      intf_4=0d0
      intf_5=0d0
      
      z1=par(1)
      z2=par(2)
      cth=par(3)
      phi=par(4)
      x1=dexp(-par(6))
      x2=par(5)/x1

      s=sqrts**2
      shat=s *x1*x2
      sqrtshat=sqrt(shat)
       
      zmh=mh**2/shat
      zh=2d0-z1-z2     
      sth=sqrt(1d0-cth**2) 
      cth12=-(z1**2+z2**2-zh**2+4d0*zmh)/(2d0*z1*z2)
      sth12=sqrt(1d0-cth12**2)

      i1i2=shat/2d0
      f1f2=z1*z2*shat/4d0*(1d0-cth12)
      f1i1=z1*shat/4d0*(1d0-cth)
      f1i2=z1*shat/4d0*(1d0+cth)
      f2i1=z2*shat/4d0*(1d0-sth*sth12*cos(phi)-cth*cth12)
      f2i2=z2*shat/4d0*(1d0+sth*sth12*cos(phi)+cth*cth12)

      call evolvepdf(x1,mh,f1)
      call evolvepdf(x2,mh,f2)

      ps=shat/x1/x2 /(2d0*pi)**4 / 32d0


c-----INTERFERENCE FOR W+W- FUSION 
c------(proc 2)
      if (iproc.eq.2) then
c------ no intf occours in this case
      intf=1d-30

c-----INTERFERENCE FOR Z0Z0 FUSION 
c------(proc 3)
      elseif (iproc.eq.3) then
c-----(1)-----------------------------------------------------------
c-----interference between Z0Z0 diagram and itself (identical quarks)
c----- active flavours: u,d,c,s,b.
c----- no CKM mixing
c-------------------------------------------------------------------
      mv1=mz
      mv2=mz
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(3,3)*couplvv(3,3))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2
      
      props=(mv2**2+2d0*f1i1)*(mv1**2+2d0*f1i2)*
     1  (mv1**2+2d0*f2i1)*(mv2**2+2d0*f2i2)

      pdf=0d0
      upcp=(1d0/2d0-2d0/3d0*sthw)**4 +(2d0/3d0*sthw)**4
      dncp=(1d0/2d0+1d0/3d0*sthw)**4 +(1d0/3d0*sthw)**4

      do i=1,5
        if(mod(i,2).eq.0) then
         pdf=pdf+upcp*(f1(i)*f2(ic*i)+f1(-i)*f2(-ic*i))
        else
         pdf=pdf+dncp*(f1(i)*f2(ic*i)+f1(-i)*f2(-ic*i))
        endif
      enddo
      intf_1= 8d0 *const* f1f2*i1i2/props *ps/2d0/shat/12d0 *pdf /2d0
c----extra 1/2d0 factor takes into account identical particles

      intf=intf_1

c-----INTERFERENCE FOR A NEUTRAL FINAL STATE 
c------(procs 1,102,103,106,107,110,111,112)
      elseif ((iproc.eq.1).or.(iproc.eq.102).or.(iproc.eq.103).or.
     1 (iproc.eq.106).or.(iproc.eq.107).or.
     1 (iproc.eq.110).or.(iproc.eq.111).or.(iproc.eq.112)) then
      
c-----(1)-----------------------------------------------------------
c-----interference between Z0Z0 and W+W- diagrams (u-type quarks with
c----- d-type quarks). Active flavours u,d,c,s.
c----- no CKM mixing
c----- no b mixing with light flavours
c-------------------------------------------------------------------
      mv1=mw
      mv2=mz
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(1,2)*couplvv(3,3))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2
      
      props=(mv2**2+2d0*f1i1)*(mv1**2+2d0*f1i2)*
     1  (mv1**2+2d0*f2i1)*(mv2**2+2d0*f2i2)

      pdf=0d0

      do i=1,2
      pdf=pdf+ f1(2*i)*f2(ic*(2*i-1))+ f2(ic*(2*i))*f1(2*i-1) 
     1 +f1(-2*i)*f2(ic*(-2*i+1))+ f2(ic*(-2*i))*f1(-2*i+1) 
      end do

      upcp=1d0/sqrt(2d0) * (1d0/2d0 -2d0/3d0*sthw )
      dncp=1d0/sqrt(2d0) * (-1d0/2d0 +1d0/3d0*sthw )
      pdf=upcp*dncp*pdf

      intf_1=  8d0 *const* f1f2*i1i2/props *ps/2d0/shat/12d0 *pdf

      
c-----(2)-----------------------------------------------------------
c-----interference between Z0Z0 diagram and itself (identical quarks)
c----- active flavours: u,d,c,s,b.
c----- no CKM mixing
c-------------------------------------------------------------------
      mv1=mz
      mv2=mz
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(3,3)*couplvv(3,3))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2
      
      props=(mv2**2+2d0*f1i1)*(mv1**2+2d0*f1i2)*
     1  (mv1**2+2d0*f2i1)*(mv2**2+2d0*f2i2)

      pdf=0d0
      upcp=(1d0/2d0-2d0/3d0*sthw)**4 +(2d0/3d0*sthw)**4
      dncp=(1d0/2d0+1d0/3d0*sthw)**4 +(1d0/3d0*sthw)**4

      do i=1,5
        if(mod(i,2).eq.0) then
         pdf=pdf+upcp*(f1(i)*f2(ic*i)+f1(-i)*f2(-ic*i))
        else
         pdf=pdf+dncp*(f1(i)*f2(ic*i)+f1(-i)*f2(-ic*i))
        endif
      enddo

      intf_2= 8d0 *const* f1f2*i1i2/props *ps/2d0/shat/12d0 *pdf /2d0
c----extra 1/2d0 factor takes into account identical particles

c-----(3)-----------------------------------------------------------
c-----interference between Z0Z0 and S-ch W diagrams (u-type quarks with
c----- d~-type quarks). Active flavours u,d,c,s.
c----- no CKM mixing
c----- no b mixing with light flavours
c----- crossing of (1): F2<->-I2
c-------------------------------------------------------------------
      mv1=mw
      mv2=mz
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(1,2)*couplvv(3,3))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2

      props=(2d0*f1i1+mv2**2)*(2d0*f2i2+mv2**2)
     1 *(2d0*i1i2-mv1**2)
     2 *((2d0*f1f2- mv1**2)**2+mv1**2 *wwdth**2)/(2d0*f1f2-mv1**2)

      pdf=0d0

      do i=1,2
      pdf=pdf+ f1(2*i)*f2(ic*(-2*i+1))+ f2(ic*(2*i))*f1(-2*i+1) 
     1 +f1(-2*i)*f2(ic*(2*i-1))+ f2(ic*(-2*i))*f1(2*i-1) 
      end do

      upcp=1d0/sqrt(2d0) * (1d0/2d0 -2d0/3d0*sthw )
      dncp=1d0/sqrt(2d0) * (-1d0/2d0 +1d0/3d0*sthw )
      pdf=upcp*dncp*pdf
 
      intf_3=  8d0 *const* f1i2*f2i1/props *ps/2d0/shat/12d0 *pdf

c-----(4)-----------------------------------------------------------
c-----interference between Z0Z0 and S-ch Z0 diagrams (u-type quarks with
c----- u~-type quarks). Active flavours u,d,c,s,b.
c----- no CKM mixing
c----- crossing of (2): F2<->-I2
c-------------------------------------------------------------------
      mv1=mz
      mv2=mz
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(3,3)*couplvv(3,3))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2
      
      props=(2d0*f1i1+mv2**2)*(2d0*f2i2+mv2**2)
     1 *(2d0*i1i2-mv1**2)
     2 *((2d0*f1f2- mv1**2)**2+mv1**2 *zwdth**2)/(2d0*f1f2-mv1**2)

      pdf=0d0
      upcp=(1d0/2d0-2d0/3d0*sthw)**4 +(2d0/3d0*sthw)**4
      dncp=(1d0/2d0+1d0/3d0*sthw)**4 +(1d0/3d0*sthw)**4

      do i=1,5
        if(mod(i,2).eq.0) then
         pdf=pdf+upcp*(f1(i)*f2(-ic*i)+f1(-i)*f2(ic*i))
        else
         pdf=pdf+dncp*(f1(i)*f2(-ic*i)+f1(-i)*f2(ic*i))
        endif
      enddo

      intf_4= 8d0 *const* f1i2*f2i1/props *ps/2d0/shat/12d0 *pdf 

c-----(5)-----------------------------------------------------------
c-----interference between W+W- and S-ch Z0 diagrams (u-type quarks with
c----- u~-type quarks). Active flavours u,d,c,s.
c----- no CKM mixing
c----- no b mixing with light flavours
c----- crossing of (1): F1<->-I2
c-------------------------------------------------------------------
      mv1=mw
      mv2=mz
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(3,3)*couplvv(3,3))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2

      props=(2d0*f1i1+mv1**2)*(2d0*f2i2+mv1**2)
     1 *(2d0*i1i2-mv2**2)
     2 *((2d0*f1f2- mv2**2)**2+mv2**2 *zwdth**2)/(2d0*f1f2-mv2**2)

      pdf=0d0

      do i=1,4
      pdf=pdf+ f1(i)*f2(-ic*i)+ f1(-i)*f2(ic*i)
      end do

      upcp=1d0/sqrt(2d0) * (1d0/2d0 -2d0/3d0*sthw )
      dncp=1d0/sqrt(2d0) * (-1d0/2d0 +1d0/3d0*sthw )
      pdf=upcp*dncp*pdf

      intf_5=  8d0 *const* f1i2*f2i1/props *ps/2d0/shat/12d0 *pdf


c----------------------------------------------------------------------      
      if(schintf.eq.0) then
      intf= intf_1+intf_2+isch*(intf_3+intf_4+intf_5)
      else
      intf=(intf_3+intf_4+intf_5)
      endif

      elseif ((iproc.eq.6).or.
     1 (iproc.eq.104).or.(iproc.eq.108).or.(iproc.eq.113)) then
c-----INTERFERENCE FOR A + CHARGED FINAL STATE 
c------(procs 6,104,108,113)
c-------------------------------------------------------------------
c-----(1)-----------------------------------------------------------
c-----interference between Z0W+ diagram and itself 
c----- (ud>ddH+) and (d~u~>u~u~H+)
c----- Active flavours u,d,c,s.
c----- no CKM mixing
c----- no b mixing with light flavours
c-------------------------------------------------------------------
      mv1=mz
      mv2=mw
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(1,3)*couplvv(1,3))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2
      
      props=(mv2**2+2d0*f1i1)*(mv1**2+2d0*f1i2)*
     1  (mv2**2+2d0*f2i1)*(mv1**2+2d0*f2i2)

      pdf=0d0
      upcp=1d0/2d0*(-1d0/2d0 +2d0/3d0*sthw )**2
      dncp=1d0/2d0*(-1d0/2d0 +1d0/3d0*sthw )**2

      do i=1,2
         pdf=pdf+
     1 dncp*(f1(2*i)*f2((2*i-1)*ic)+f1(2*i-1)*f2(2*i*ic))+
     1 upcp*(f1(-2*i)*f2(-(2*i-1)*ic)+f1(-(2*i-1))*f2(-2*i*ic))
      enddo

      intf_1= 8d0 *const* f1f2*i1i2/props *ps/2d0/shat/12d0 *pdf /2d0
c----extra 1/2d0 factor takes into account identical particles

c-----(2)-----------------------------------------------------------
c-----interference between Z0W+ and W+Z0 diagram 
c----- (uu>duH+) and (d~d~>d~u~H+)
c----- Active flavours u,d,c,s.
c----- no CKM mixing
c----- no b mixing with light flavours
c-------------------------------------------------------------------
      mv1=mz
      mv2=mw
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(1,3)*couplvv(1,3))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2
      
      props=(mv2**2+2d0*f1i1)*(mv2**2+2d0*f1i2)*
     1  (mv1**2+2d0*f2i1)*(mv1**2+2d0*f2i2)

      pdf=0d0
      upcp=1d0/2d0*(-1d0/2d0 +2d0/3d0*sthw )**2
      dncp=1d0/2d0*(-1d0/2d0 +1d0/3d0*sthw )**2

      do i=1,2
         pdf=pdf+
     1 dncp*f1(-(2*i-1))*f2(-(2*i-1)*ic)+
     1 upcp*f1(2*i)*f2(2*i*ic)
      enddo

      intf_2= 8d0 *const* f1f2*i1i2/props *ps/2d0/shat/12d0 *pdf

      if(schintf.eq.0) then
      intf= intf_1+intf_2+isch*(0d0)
      else
      intf=0d0
      endif


      elseif ((iproc.eq.7).or.
     1 (iproc.eq.105).or.(iproc.eq.109).or.(iproc.eq.114)) then
c-----INTERFERENCE FOR A ++ CHARGED FINAL STATE 
c------(procs 7,105,109,114)
c-----(1)-----------------------------------------------------------
c-----interference between W+W+ diagram and itself
c----- (uu>dd H++) and (d~d~>u~u~H++)
c----- Active flavours u,d,c,s.
c----- no CKM mixing
c----- no b mixing with light flavours
c-------------------------------------------------------------------
      mv1=mw
      mv2=mw
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(1,1)*couplvv(1,1))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2
      
      props=(mv2**2+2d0*f1i1)*(mv1**2+2d0*f1i2)*
     1  (mv2**2+2d0*f2i1)*(mv1**2+2d0*f2i2)

      pdf=0d0
      upcp=1d0/4d0

      do i=1,2
         pdf=pdf +f1(2*i)*f2(2*i*ic)+f1(-(2*i-1))*f2(-(2*i-1)*ic)
      enddo
      pdf=pdf*upcp

      intf_1= 8d0 *const* f1f2*i1i2/props *ps/2d0/shat/12d0 *pdf /2d0
c----extra 1/2d0 factor takes into account identical particles
      if(schintf.eq.0) then
      intf= intf_1+intf_2+isch*(0d0)
      else
      intf=0d0
      endif
      elseif (iproc.eq.5) then
c-----INTERFERENCE FOR A - CHARGED FINAL STATE 
c------(procs 5)
c-------------------------------------------------------------------
c-----(1)-----------------------------------------------------------
c-----interference between Z0W- diagram and itself 
c----- (u~d~>d~d~H-) and (du>uuH-)
c----- Active flavours u,d,c,s.
c----- no CKM mixing
c----- no b mixing with light flavours
c-------------------------------------------------------------------
      mv1=mz
      mv2=mw
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(2,3)*couplvv(2,3))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2
      
      props=(mv2**2+2d0*f1i1)*(mv1**2+2d0*f1i2)*
     1  (mv2**2+2d0*f2i1)*(mv1**2+2d0*f2i2)

      pdf=0d0
      upcp=1d0/2d0*(-1d0/2d0 +2d0/3d0*sthw )**2
      dncp=1d0/2d0*(-1d0/2d0 +1d0/3d0*sthw )**2

      do i=1,2
         pdf=pdf+
     1 dncp*(f1(-2*i)*f2(-(2*i-1)*ic)+f1(-(2*i-1))*f2(-2*i*ic))+
     1 upcp*(f1(2*i)*f2((2*i-1)*ic)+f1((2*i-1))*f2(2*i*ic))
      enddo

      intf_1= 8d0 *const* f1f2*i1i2/props *ps/2d0/shat/12d0 *pdf /2d0
c----extra 1/2d0 factor takes into account identical particles

c-----(2)-----------------------------------------------------------
c-----interference between Z0W- and W-Z0 diagram 
c----- (u~u~>d~u~H-) and (dd>duH-)
c----- Active flavours u,d,c,s.
c----- no CKM mixing
c----- no b mixing with light flavours
c-------------------------------------------------------------------
      mv1=mz
      mv2=mw
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(2,3)*couplvv(2,3))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2
      
      props=(mv2**2+2d0*f1i1)*(mv2**2+2d0*f1i2)*
     1  (mv1**2+2d0*f2i1)*(mv1**2+2d0*f2i2)

      pdf=0d0
      upcp=1d0/2d0*(-1d0/2d0 +2d0/3d0*sthw )**2
      dncp=1d0/2d0*(-1d0/2d0 +1d0/3d0*sthw )**2

      do i=1,2
         pdf=pdf+
     1 dncp*f1((2*i-1))*f2((2*i-1)*ic)+
     1 upcp*f1(-2*i)*f2(-2*i*ic)
      enddo

      intf_2= 8d0 *const* f1f2*i1i2/props *ps/2d0/shat/12d0 *pdf

      if(schintf.eq.0) then
      intf= intf_1+intf_2+isch*(0d0)
      else
      intf=0d0
      endif


      elseif (iproc.eq.4) then
c-----INTERFERENCE FOR A -- CHARGED FINAL STATE 
c------(procs 4)
c-----(1)-----------------------------------------------------------
c-----interference between W-W- diagram and itself
c----- (u~u~>d~d~H--) and (dd>uuH--)
c----- Active flavours u,d,c,s.
c----- no CKM mixing
c----- no b mixing with light flavours
c-------------------------------------------------------------------
      mv1=mw
      mv2=mw
c      const=gf*mv1**2*mv2**2* 16d0*sqrt(2d0) *32d0*gf**2*mv1**2*mv2**2
      const=dsqrt(couplvv(2,2)*couplvv(2,2))
     1 *4d0*32d0*gf**2*mv1**2*mv2**2
      
      props=(mv2**2+2d0*f1i1)*(mv1**2+2d0*f1i2)*
     1  (mv2**2+2d0*f2i1)*(mv1**2+2d0*f2i2)

      pdf=0d0
      upcp=1d0/4d0

      do i=1,2
         pdf=pdf +f1(-2*i)*f2(-2*i*ic)+f1((2*i-1))*f2((2*i-1)*ic)
      enddo
      pdf=pdf*upcp

      intf_1= 8d0 *const* f1f2*i1i2/props *ps/2d0/shat/12d0 *pdf /2d0
c----extra 1/2d0 factor takes into account identical particles
      if(schintf.eq.0) then
      intf= intf_1+intf_2+isch*(0d0)
      else
      intf=0d0
      endif




      endif

      return
      end


      double precision function integrand_intf(xx)
      implicit none
      double precision intf
      double precision xx(6),par(6),tau,tauh,mh,sqrts,pi,xmh,jac
      double precision gevtopb
      common/hmass/mh
      common/energy/sqrts
      common/pi_gr/pi
      common/conv/gevtopb
      integer ilast,ipdf,npdf
      common/pdf_sets/npdf
      common/last_integ/ilast
      double precision integralpdf(60)
      common/to_vegas/integralpdf

c-----par contains respectively z1,z2,cth,phi,x1*x2, log(x1)

      tauh=mh**2/sqrts**2
      par(5)=tauh+(1d0-tauh)*xx(5)
      tau=par(5)
      par(6)=-dlog(tau)*xx(6)

      xmh=mh**2/(tau*sqrts**2)

      par(1)=xx(1)*(1d0-xmh)
      par(2)=(1d0-par(1)-xmh)*(1d0-xx(2)) + (1d0-xmh/(1d0-par(1)))*xx(2)

      par(3)=-1d0+2d0*xx(3)
      par(4)=2d0*pi*xx(4)

      jac=-dlog(tau)*(1d0-tauh)*
     1 (1d0-xmh)*
     1 (1d0-par(1)-xmh)*par(1)/(1d0-par(1))*
     2 4d0*pi
      
      integrand_intf=gevtopb*intf(par)*jac 
      if (ilast.eq.1) then
              do ipdf=1,npdf
              call initpdf(ipdf)
              integralpdf(ipdf)=gevtopb*intf(par)*jac
              enddo
      endif
      return
      end
