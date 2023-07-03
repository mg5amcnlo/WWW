      double precision function loop(par)
      implicit none
      integer ic,i
      double precision par(6)
      double precision const,gf, ps,sthw
      double precision shat, sqrtshat,sqrts,s
      double precision mw,mz,wwdth,zwdth,mt,mb,mh
      double precision x1,x2,z1,z2,zh,zmh,q1,q2
      double precision cth,sth,cth12,sth12,phi,pi
      double precision props,f1f2, i1i2,f1i1,f1i2,f2i1,f2i2
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
      common/pi_gr/pi

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
      upcp=1/4d0+ (1/2d0-4d0/3d0 *sthw)**2
      dncp=1/4d0+ (1/2d0-2d0/3d0 *sthw)**2

      pdf=0d0

      do i=1,5
        if (mod (i,2).eq.0)then
         pdf=pdf+(f1(i)+f1(-i))*f2(0)*upcp
     1          +(f2(i*ic)+f2(-i*ic))*f1(0)*upcp
        else
         pdf=pdf+(f1(i)+f1(-i))*f2(0)*dncp
     1          +(f2(i*ic)+f2(-i*ic))*f1(0)*dncp  
        endif
      end do
c-----checked      
      const=8d0*sqrt(2d0)*alphaspdf(mh)**2*gf**3*mz**4/pi**2
      loop=
     1 (f1f2*(f1i2 - i1i2) + f1i1*f2i2 - f1i2*f2i1 + f2i1*i1i2)
     2 /(mz**2+2d0*f2i2)**2
     3 *abs( mb**2 *C0(0d0,0d0,-2d0*f1i1,mb**2,mb**2,mb**2)
     4      -mt**2 *C0(0d0,0d0,-2d0*f1i1,mt**2,mt**2,mt**2) )**2
      loop=loop*const*ps/2d0/shat*pdf

      return
      end



      double precision function integrand_loop(xx)
      implicit none
      double precision loop
      double precision xx(6),par(6),tau,tauh,mh,sqrts,pi,xmh,jac
      double precision gevtopb
      common/hmass/mh
      common/energy/sqrts
      common/pi_gr/pi
      common/conv/gevtopb

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
      
      integrand_loop=gevtopb*loop(par)*jac 
      return
      end
