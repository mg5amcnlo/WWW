      program vbf_hadr
      implicit none
      double precision pi
      integer i,ii, ncalls,j,jj,iv,k
      common/pi_gr/pi
      double precision integral, prob, error,eps,sigtot(6),tot
      double precision integral_intf,prob_intf, error_intf
      double precision integral_intf_s
      double precision integral_loop,prob_loop, error_loop
      integer  itmx, nprn, ncall
      integer sc,isch,icoll
      double precision results(6)

      common/collider/icoll
      double precision f1,f2,f3,fl
      external integrand, integrand_intf
      double precision reg(16), reg_intf(12),reg_sch(6),reg_loop(12)
      double precision reg_ch(2), res, xx,qq
      external inte_ch
      integer iostatus
      integer vv
      integer ord,nf,icomp,imp,ordmax,impr
      double precision sqrts, gf, mp, mh, mw,mz,sthw,wwdth,zwdth,mt,mb
      double precision gevtopb
      double precision xmuf,xmur
      common/energy/sqrts
      common/hmass/mh
      common/gfermi/gf
      common/pmass/mp
      common/pert_ord/ord
      common/improved/imp
      common/prec/eps
      common/sin_wein/sthw
      common/masses/mw,mz
      common/qmass/mt,mb
      common/widths/wwdth,zwdth
      common/conv/gevtopb
      common/integ/xx,qq,vv
      common/scales/xmuf,xmur,sc
      common/schannel/isch
      integer schintf
      common/s_interf/schintf
      character outname *30, pdflo*30,pdfnlo*30,pdfnnlo*30, pdfname*30
      character pdfshort *4
      double precision ti,tf
      integer ilast
      common/last_integ/ilast
      integer pdf,ipdf
      double precision integralpdf(100),sigtotpdf(100)
      common/from_vegas/integralpdf
      double precision wgt, swgt
      common/vegas_wgt/wgt, swgt
      double precision epdfu(6), epdfd(6),epdfs(6)
      integer npdf
      common/pdf_sets/npdf
      integer iproc
      common/iproc/iproc
      double precision vpov
      common/vev_ratio/vpov
      double precision lhs0,lhs,lhc,lhsp,lhcp,lhf
      common/lh_s0/lhs0,lhs,lhc,lhsp,lhcp,lhf
c------input parameters file
      open(unit=70, status="unknown", file="param_card.dat")
      open(unit=77, status="unknown", file="run_card.dat")     
     
      read(70,100) mw
      read(70,100) mz
      read(70,100) mt
      read(70,100) mb
      read(70,100) mp     
      read(70,100) wwdth
      read(70,100) zwdth    
      read(70,100) gf    
      read(70,100) sthw 
      read(70,100) eps
      read(70,100) gevtopb
c-----BSM parameters
c-----ratio v'/v
      read(70,100) vpov
c-----f,s0,s,c for Littlest Higgs model
      read(70,100) lhf
      read(70,100) lhs0
      read(70,100) lhs
      read(70,100) lhc
      read(70,100) lhsp
      read(70,100) lhcp

c-----IPROC
c-------1->SM H
c-------2->GENERIC (SM-LIKE COUPLINGS) W+W-
c-------3->GENERIC (SM-LIKE COUPLINGS) Z0Z0
c-------4->GENERIC (SM-LIKE COUPLINGS) W-W-
c-------5->GENERIC (SM-LIKE COUPLINGS) W-Z0
c-------6->GENERIC (SM-LIKE COUPLINGS) W+Z0
c-------7->GENERIC (SM-LIKE COUPLINGS) W+W+
c-------102->LITTLEST HIGGS H
c-------103->LITTLEST HIGGS PHI 0
c-------104->LITTLEST HIGGS PHI +
c-------105->LITTLEST HIGGS PHI ++
c-------106->GEORGI-MACHACEK H_1 0
c-------107->GEORGI-MACHACEK H_5 0
c-------108->GEORGI-MACHACEK H_5 +
c-------109->GEORGI-MACHACEK H_5 ++
c-------110>GEORGI-MACHACEK H_1'0
c-------111>L-R SYMM H
c-------112>L-R SYMM DELTA 0
c-------113>L-R SYMM DELTA +
c-------114>L-R SYMM DELTA ++
      read(77,101) iproc
c-----ICOMP SETTINGS
c-------1->LO
c-------2->"pure" NLO
c-------3->"pure" NNLO
c-------4->"improved" NLO, NLO pdf
c-------5->"improved" NLO, NNLO pdf
c-------6->"improved" NNLO
      read(77,101) icomp
c------------isch=0-> no s-channel
c------------isch=1-> s-channel included 
      read(77,101) isch
c------------icoll=1-> pp collider
c------------icoll=-1-> ppbar collider
      read(77,101) icoll
      read(77,100) sqrts  
      read(77,100) mh
c------------sc=0-> fixed (=mh)struct. function scales
c------------sc=1-> running struct. function scales 
      read(77,101) sc    
      read(77,100) xmuf   
      read(77,100) xmur
      read(77,102) outname
      read(77,102) pdflo
      read(77,102) pdfnlo
      read(77,102) pdfnnlo
c-----if pdf=1 the pdf error is computed (only for xmu=1)      
      read(77,101) pdf
      write(*,*) isch, icoll, sc,pdf, xmuf, xmur

      close(70)
      close(77)

c----- set VV couplings for the selected process
       call set_vv_coupl(iproc)

c-----opening output file
      open(unit=80, status="unknown", file=outname, access="append")

      pi=4d0*datan(1d0)


      itmx=12

      do i=1,8
      reg(i)=0d0
      reg(i+8)=1d0
      end do

      do i=1,3
      reg_sch(i)=0d0
      reg_sch(i+3)=1d0
      end do

      do i=1,6
      reg_intf(i)=0d0
      reg_intf(i+6)=1d0
      reg_loop(i)=0d0
      reg_loop(i+6)=1d0     
      end do
      
      do ii=1,6
      sigtot(ii)=0d0
      epdfs(ii)=0d0
      epdfu(ii)=0d0
      epdfd(ii)=0d0
      enddo


      write(*,*)"Now computing (Ecm, Mh): ", sqrts,mh
      do k=1,icomp

      do ii=1,100
      sigtotpdf(ii)=0d0
      enddo

      if (k.eq.1) then
        pdfname=pdflo
        ordmax=1
        impr=0
        write(*,*)"LO x-sect"
      else  if (k.eq.2) then
        pdfname=pdfnlo
        ordmax=2
        impr=0
        write(*,*)"NLO x-sect"
       else  if (k.eq.3) then
        pdfname=pdfnnlo
        ordmax=3
        impr=0
        write(*,*)"NNLO x-sect"
      else  if (k.eq.4) then
        pdfname=pdfnlo
        ordmax=2
        impr=1
        write(*,*)"NLO-imp x-sect (pdf NLO)"
      else  if (k.eq.5) then
        pdfname=pdfnnlo
        ordmax=2
        impr=1
        write(*,*)"NLO-imp x-sect (pdf NNLO)"
      else  if (k.eq.6) then
        pdfname=pdfnnlo
        ordmax=3
        impr=1
        write(*,*)"NNLO-imp x-sect"
      endif
c-----init the pdf set
      call initpdfsetbyname(pdfname)
      pdfshort=pdfname
c-----determine the number of pdf error sets
      call numberpdf(npdf)      
      if (npdf.gt.100) npdf=100

      do ord=1,ordmax
       if ((ord.eq.ordmax).and.(impr.eq.1)) then
        imp=1
       else
       imp=0
      endif

      if (ord.eq.1) then 
       ncall=500000 
      else  
       ncall=500000
      endif
      nprn=0 


      call cpu_time(ti)
c-----VBF integration
c   vegas warmup
      ilast=0
      call vegas(reg,8,integrand,0,ncall/50,
     1        5*itmx,nprn,integral,error,prob)
      call vegas(reg,8,integrand,1,ncall/5,
     1        itmx/2,nprn,integral,error,prob)

c   vegas main call
      if ((xmur.eq.1d0).and.(xmuf.eq.1d0).and.(pdf.eq.1)) then
      ilast=1
        do ipdf=1,npdf
        integralpdf(ipdf)=0d0
        enddo
      endif
      call vegas(reg,8,integrand,1,ncall,
     1         1,nprn,integral,error,prob)
      write(*,*) "alpha_s^", ord-1, "  contribute: "
      write(*,*) "t, u channel squared ",integral, " +- ", error
      sigtot(k)=sigtot(k)+integral
      do ii=1,npdf
      sigtotpdf(ii)=sigtotpdf(ii)+integralpdf(ii)
      enddo
      ilast=0


      enddo


      if (k.eq.1) then 
        write(*,*)"Total LO cross-section: ",sigtot(k), " pb"
      else if (k.eq.2) then 
        write(*,*)"Total NLO cross-section: ",sigtot(k), " pb"
       else if (k.eq.3) then 
        write(*,*)"Total NNLO cross-section: ",sigtot(k), " pb"
      else if (k.eq.4) then 
        write(*,*)"Total NLO improved cross-section: ",sigtot(k), " pb"
        write(*,*) "(NLO pdf)"
      else if (k.eq.5) then 
        write(*,*)"Total NLO improved cross-section: ",sigtot(k), " pb"
        write(*,*) "(NNLO pdf)"
      else if (k.eq.6) then 
        write(*,*)"Total NNLO improved cross-section: ",sigtot(k), " pb"
      endif


      if ((pdf.eq.1).and.(xmuf.eq.1d0).and.(xmur.eq.1d0)) then      
      call 
     1 errpdf(sigtot(k),sigtotpdf,epdfs(k),epdfu(k),epdfd(k),pdfshort)
      write(*,*) "PDF error"
      write(*,*) "symmetric :  +-",epdfs(k)
      write(*,*) "asymmetric:   +",epdfu(k),"     -",epdfd(k)
      endif

      write(*,*)
 
      enddo      

      write(*,*)
      call cpu_time(tf)
      write(*,*)"RUNNING TIME", tf-ti
    
      write(80,120)iproc,icoll,
     1 sqrts, mh,sc,pdf,xmuf,xmur, sigtot(1), sigtot(2),
     1  sigtot(3), epdfs(1), epdfs(2), epdfs(3)
c       write(80,120)sqrts, mh,sc,90,xmuf,xmur, sigtot, integral_intf,
c     1  integral_intf_s
      close(80)
      close(81)
    
100   FORMAT(11X,G30.20)
101   FORMAT(11X,I30)
102   FORMAT(12X,A30)

110   format(f7.1,a2,f5.0,a2,2(e19.7,a2),f19.7,a2)
111   format(2a5, 3a21)
120   format(2(i7),f10.0,f10.0,2(i3),2(f7.4),6(e15.6))
   
      stop
      end

      subroutine errpdf(f0, fpdf,es,eu,ed,short)
*this sub computes pdf errors in the cteq/mstw scheme
      implicit none
      double precision f0, fpdf(100),dfu,dfd
      double precision eu,ed,es
      integer i, iu,id
      integer npdf,ierr
      common/pdf_sets/npdf
      character *4 short
      ierr=0
      if (short .eq. "abkm") ierr=1
      if (short .eq. "a02m") ierr=1
      if (short .eq. "NNPD") ierr=2
      es=0d0
      eu=0d0
      ed=0d0
       if (ierr.eq.0) then
c-----standard HEPDATA pdf error computation      
      do i=1,npdf/2
       iu=2*i-1
       id=2*i
       es=es+(fpdf(iu)-fpdf(id))**2
       dfu=fpdf(iu)-f0
       dfd=fpdf(id)-f0
       eu=eu+max(dfu,dfd,0d0)**2
       ed=ed+max(-dfu,-dfd,0d0)**2
      enddo
      eu=sqrt(eu)
      ed=sqrt(ed)

      es=1d0/2d0 *sqrt(es)
       elseif (ierr.eq.1) then
c-----Alekhin/ABKM error computation      
      do i=1,npdf
       es=es+(f0-fpdf(i))**2
      enddo
      es=sqrt(es)
      eu=es
      ed=es
       elseif (ierr.eq.2) then
c-----MC (NNPDF) error computation      
       av=0d0
       av2=0d0
       do i=1, npdf
       av=av+fpdf(i)
       av2=av2+fpdf(i)**2
       enddo
       av=av/npdf
       av2=av2/npdf
       es=dsqrt(npdf/(npdf-1) *(av2-av**2))
       eu=es
       ed=es
       write(*,*) "MC average ", av
       endif

      return
      end

      subroutine errpdf_abkm(f0, fpdf,es,eu,ed)
*this sub computes pdf errors in the abkm scheme
      implicit none
      double precision f0, fpdf(100),df
      double precision eu,ed,es
      integer i
      integer npdf
      common/pdf_sets/npdf
      es=0d0
      eu=0d0
      ed=0d0
      do i=1,npdf
       es=es+(f0-fpdf(i))**2
      enddo

      es=sqrt(es)
      eu=es
      ed=es

      return
      end

      double precision function inte_ch(xx)
       implicit none
      double precision x,q,z,f1,f2,f3,fl
      double precision xx(1)
      integer v
      common/integ/x,q,v
      z=xx(1)
      inte_ch=fl(1,x,q,q/3d0,q/2d0,3,v,z)

      return
      end

      double precision function me(par,qt2)
      implicit none
      double precision par(8),x1,x2,q1,q2,qt1,phi,qt2,sqrts,EE
      double precision gf,mv1,mv2,mw,mz,s,mp,y1,y2,mh
      double precision const, prop,eps
      double precision q1q2, q12,q22,p1q1,p1q2,p2q1,p2q2,p1p2
      double precision muf1,muf2,mur1,mur2
      double precision f1a,f1b,f2a,f2b,
     1 f3a,f3b

      double precision ff(0:61),f11(0:61),f12(0:61),f21(0:61),f22(0:61),
     1 f33(0:61),f1,f2,f3,z1,z2,z3
      double precision sc1,sc2,xmur,xmuf
      integer ord,i,j,vec,v1,v2,sc,imp,icoll,ic1,ic2
      integer ilast
      common/last_integ/ilast
      common/prec/eps
      common/energy/sqrts
      common/gfermi/gf
      common/masses/mw,mz
      common/pmass/mp
      common/hmass/mh
      common/pert_ord/ord
      common/scales/xmuf,xmur,sc
      common/improved/imp
      common/collider/icoll
      double precision ssc1,ssc2
      double precision pdfz1(-6:6),pdf11(-6:6),pdfz2(-6:6),pdf12(-6:6)
      double precision fa(3,3,3,0:100), fb(3,3,3,0:100)
      integer ipdf,npdf,o,v
      double precision mepdf(100)
      common/me_pdf/mepdf
      integer numpdf
      common/pdf_sets/numpdf
      double precision couplvv(3,3)
      common/coupl_vv/couplvv
      integer iproc
      common/iproc/iproc
      do ipdf=1,100
      mepdf(ipdf)=0d0
      enddo

      s=sqrts**2
      x1=dexp(-par(6))
      x2=par(5)/x1
      q1=par(1)
      q2=par(2)
      qt1=par(3)
      phi=par(4)

      z1=par(7)*(1d0-x1)+x1
      z2=par(8)*(1d0-x2)+x2

      y1=q1**2/((qt1**2-q1)*S*x1)
      y2=q2**2/((qt2**2-q2)*S*x2)
      q1q2=(mh**2+q1+q2)/2d0
      q12=-q1
      q22=-q2
      p1p2=S/2d0

      p1q1=q1/(2d0*x1)
      p2q1=q1/(2d0*y1)
      p2q2=q2/(2d0*x2)
      p1q2=q2/(2d0*y2)

      if (icoll.eq.1) then
       ic1=1
       ic2=1
       else
       ic1=1
       ic2=-1
      endif

      ssc1=sqrt(q1)
      ssc2=sqrt(q2)
      
      if (sc.eq.0) then
       sc1=mh
       sc2=mh
      else if(sc.eq.1) then
       sc1=sqrt(q1)
       sc2=sqrt(q2)
      else if(sc.eq.2) then
       sc1=mw
       sc2=mw
      end if

      muR1=sc1*xmuR
      muF1=sc1*xmuF

      muR2=sc2*xmuR
      muF2=sc2*xmuF

      if(ilast.eq.1) then
              npdf=numpdf
      else
              npdf=0
      endif


      do o=1,ord
      if (o.eq.3) then
              call sethpl(z1)
      endif
      do v=1,3
      do ipdf=0,npdf
c      if ((o.eq.1).and.(v.eq.1))then
      call initpdf(ipdf)
      call PDF(x1,muf1,pdf11)
      call PDF(x1/z1,muf1,pdfz1)
c      endif
      fa(1,v,o,ipdf)=f1(ic1,x1,ssc1,muf1,mur1,o,      v,z1,pdfz1,pdf11)
      fa(2,v,o,ipdf)=f2(ic1,x1,ssc1,muf1,mur1,o,      v,z1,pdfz1,pdf11)
      fa(3,v,o,ipdf)=f3(ic1,x1,ssc1,muf1,mur1,o,      v,z1,pdfz1,pdf11)
      enddo
      enddo

      if (o.eq.3) then
              call sethpl(z2)
      endif
      do v=1,3
      do ipdf=0,npdf
c      if ((o.eq.1).and.(v.eq.1))then
      call initpdf(ipdf)
      call PDF(x2,muf2,pdf12)
      call PDF(x2/z2,muf2,pdfz2)
c      endif
      fb(1,v,o,ipdf)=f1(ic2,x2,ssc2,muf2,mur2,o,      v,z2,pdfz2,pdf12)
      fb(2,v,o,ipdf)=f2(ic2,x2,ssc2,muf2,mur2,o,      v,z2,pdfz2,pdf12)
      fb(3,v,o,ipdf)=f3(ic2,x2,ssc2,muf2,mur2,o,      v,z2,pdfz2,pdf12)
      enddo
      enddo

      enddo

      me=0d0

c      do vec=-1,1
       do v1=1,3
       do v2=1,3
       if(v1.eq.1) mv1=mw
       if(v1.eq.2) mv1=mw
       if(v1.eq.3) mv1=mz
       if(v2.eq.1) mv2=mw
       if(v2.eq.2) mv2=mw
       if(v2.eq.3) mv2=mz

c      if (vec.eq.-1) then
c       v1=1
c       v2=2
c       mv1=mw
c       mv2=mw
c      else if(vec.eq.1) then
c       v1=2
c       v2=1
c       mv1=mw
c       mv2=mw
c      else if(vec.eq.0) then
c       v1=3
c       v2=3
c       mv1=mz
c       mv2=mz
c      end if

      do ipdf=0,numpdf

      ff(ipdf)=0d0
      if (imp.eq.0)then
       do i=1,ord
         j=ord-i+1

        f11(ipdf)=fa(1,v1,i,ipdf)*fb(1,v2,j,ipdf)*couplVV(v1,v2)
        f12(ipdf)=fa(1,v1,i,ipdf)*fb(2,v2,j,ipdf)*couplVV(v1,v2)
        f21(ipdf)=fa(2,v1,i,ipdf)*fb(1,v2,j,ipdf)*couplVV(v1,v2)
        f22(ipdf)=fa(2,v1,i,ipdf)*fb(2,v2,j,ipdf)*couplVV(v1,v2)
        f33(ipdf)=fa(3,v1,i,ipdf)*fb(3,v2,j,ipdf)*couplVV(v1,v2)

c------VBF into a scalar
       if (iproc.lt.200) then
       ff(ipdf)=ff(ipdf)+
     1 f11(ipdf)*(2d0+q1q2**2/(q12*q22))
     1  +f12(ipdf)/p2q2*(p2q2**2/q22-mp**2+(p2q1-p2q2*q1q2/q22)**2/q12)
     2  +f21(ipdf)/p1q1*(p1q1**2/q12-mp**2+(p1q2-p1q1*q1q2/q12)**2/q22)
     3  +f22(ipdf)/(p1q1*p2q2) *(p1p2-p1q1*p2q1/q12-p2q2*p1q2/q22+
     3                     p1q1*p2q2*q1q2/(q12*q22))**2
     4  +f33(ipdf)/(2d0*p1q1*p2q2) *(p1p2*q1q2-p1q2*p2q1)


c       if (ipdf.eq.0) write(*,*) v1,v2,couplvv(v1,v2),ff(ipdf) 
       else
c------VBF into a vector     
       ff(ipdf)=ff(ipdf)+
     -  (((q12 - q22)**2*(q1q2**2 + 2*q12*q22) + 
     -       mz**2*(-14*q12**2*q22 + q1q2**2*(2*q1q2 + 11*q22) + 
     -          q12*(11*q1q2**2 + 4*q1q2*q22 - 14*q22**2)))*F11(ipdf))/
     -   (mz**2*q12*q22) + (((q12 - q22)**2*
     -        (-2*p2q1*p2q2*q1q2*q22 + p2q1**2*q22**2 + 
     -          p2q2**2*(q1q2**2 + q12*q22)) + 
     -       mz**2*(p2q1**2*(11*q12 + 2*q1q2 - q22)*q22**2 + 
     -          2*p2q1*p2q2*q1q2*q22*(-11*q12 - 2*q1q2 + q22) + 
     -          p2q2**2*(-(q12**2*q22) + q1q2**2*(2*q1q2 + 3*q22) + 
     -         q12*(11*q1q2**2 + 2*q1q2*q22 - 5*q22**2))))*F12(ipdf))/
     -   (mz**2*p2q2*q12*q22**2) + 
     -  (((q12 - q22)**2*(p1q2**2*q12**2 - 2*p1q1*p1q2*q12*q1q2 + 
     -          p1q1**2*(q1q2**2 + q12*q22)) + 
     -       mz**2*(2*p1q1*p1q2*q12*q1q2*(q12 - 2*q1q2 - 11*q22) + 
     -          p1q2**2*q12**2*(-q12 + 2*q1q2 + 11*q22) + 
     -          p1q1**2*(-5*q12**2*q22 + q1q2**2*(2*q1q2 + 11*q22) + 
     -             q12*(3*q1q2**2 + 2*q1q2*q22 - q22**2))))*F21(ipdf))/
     -   (mz**2*p1q1*q12**2*q22) + 
     -  (((q12 - q22)**2*(-2*p1q2*p2q2*q12 + 2*p1q1*p2q2*q1q2 - 
     -           2*p1q1*p2q1*q22 + q12*q22*s)**2 + 
     -       mz**2*(-4*p1q2**2*p2q2**2*q12**2*(q12 - 2*q1q2 - 3*q22) + 
     -          4*p1q1**2*(p2q1**2*(3*q12 + 2*q1q2 - q22)*q22**2 + 
     -             2*p2q1*p2q2*q1q2*q22*(-3*q12 - 2*q1q2 + q22) + 
     -             p2q2**2*q1q2**2*(3*q12 + 2*q1q2 + 3*q22)) + 
     -   4*p1q1*q12*q22*(q12 - 2*q1q2 + q22)*(-(p2q2*q1q2) + p2q1*q22)*
     -           s - q12**2*q22**2*(q12 - 2*q1q2 + q22)*s**2 + 
     -          4*p1q2*p2q2*q12*
     -           (2*p1q1*(p2q2*q1q2*(q12 - 2*q1q2 - 3*q22) - 
     -                p2q1*q22*(q12 - 2*q1q2 + q22)) + 
     -             q12*q22*(q12 - 2*q1q2 + q22)*s)))*F22(ipdf))/
     -   (4.*mz**2*p1q1*p2q2*q12**2*q22**2) + 
     -  ((-((q12 - q22)**2*(2*p1q2*p2q1 - q1q2*s)) + 
     -       mz**2*(16*p1q1*(p2q2*q1q2 - p2q1*q22) + 
     -          2*p1q2*(-8*p2q2*q12 + p2q1*(q12 + 6*q1q2 + q22)) - 
     -       (q12*(q1q2 - 8*q22) + q1q2*(6*q1q2 + q22))*s))*F33(ipdf))/
     -   (4.*mz**2*p1q1*p2q2)
       endif

       end do
      else
       do i=1,ord
       do j=1,ord
        if ((i+j).gt.ord) then

        f11(ipdf)=fa(1,v1,i,ipdf)*fb(1,v2,j,ipdf)*couplVV(v1,v2)
        f12(ipdf)=fa(1,v1,i,ipdf)*fb(2,v2,j,ipdf)*couplVV(v1,v2)
        f21(ipdf)=fa(2,v1,i,ipdf)*fb(1,v2,j,ipdf)*couplVV(v1,v2)
        f22(ipdf)=fa(2,v1,i,ipdf)*fb(2,v2,j,ipdf)*couplVV(v1,v2)
        f33(ipdf)=fa(3,v1,i,ipdf)*fb(3,v2,j,ipdf)*couplVV(v1,v2)

c------VBF into a scalar
       if (iproc.lt.200) then
       ff(ipdf)=ff(ipdf)+
     1 f11(ipdf)*(2d0+q1q2**2/(q12*q22))
     1  +f12(ipdf)/p2q2*(p2q2**2/q22-mp**2+(p2q1-p2q2*q1q2/q22)**2/q12)
     2  +f21(ipdf)/p1q1*(p1q1**2/q12-mp**2+(p1q2-p1q1*q1q2/q12)**2/q22)
     3  +f22(ipdf)/(p1q1*p2q2) *(p1p2-p1q1*p2q1/q12-p2q2*p1q2/q22+
     3                     p1q1*p2q2*q1q2/(q12*q22))**2
     4  +f33(ipdf)/(2d0*p1q1*p2q2) *(p1p2*q1q2-p1q2*p2q1)
       else
c------VBF into a vector     
       ff(ipdf)=ff(ipdf)+
     -  (((q12 - q22)**2*(q1q2**2 + 2*q12*q22) + 
     -       mz**2*(-14*q12**2*q22 + q1q2**2*(2*q1q2 + 11*q22) + 
     -          q12*(11*q1q2**2 + 4*q1q2*q22 - 14*q22**2)))*F11(ipdf))/
     -   (mz**2*q12*q22) + (((q12 - q22)**2*
     -        (-2*p2q1*p2q2*q1q2*q22 + p2q1**2*q22**2 + 
     -          p2q2**2*(q1q2**2 + q12*q22)) + 
     -       mz**2*(p2q1**2*(11*q12 + 2*q1q2 - q22)*q22**2 + 
     -          2*p2q1*p2q2*q1q2*q22*(-11*q12 - 2*q1q2 + q22) + 
     -          p2q2**2*(-(q12**2*q22) + q1q2**2*(2*q1q2 + 3*q22) + 
     -          q12*(11*q1q2**2 + 2*q1q2*q22 - 5*q22**2))))*F12(ipdf))/
     -   (mz**2*p2q2*q12*q22**2) + 
     -  (((q12 - q22)**2*(p1q2**2*q12**2 - 2*p1q1*p1q2*q12*q1q2 + 
     -          p1q1**2*(q1q2**2 + q12*q22)) + 
     -       mz**2*(2*p1q1*p1q2*q12*q1q2*(q12 - 2*q1q2 - 11*q22) + 
     -          p1q2**2*q12**2*(-q12 + 2*q1q2 + 11*q22) + 
     -          p1q1**2*(-5*q12**2*q22 + q1q2**2*(2*q1q2 + 11*q22) + 
     -             q12*(3*q1q2**2 + 2*q1q2*q22 - q22**2))))*F21(ipdf))/
     -   (mz**2*p1q1*q12**2*q22) + 
     -  (((q12 - q22)**2*(-2*p1q2*p2q2*q12 + 2*p1q1*p2q2*q1q2 - 
     -           2*p1q1*p2q1*q22 + q12*q22*s)**2 + 
     -       mz**2*(-4*p1q2**2*p2q2**2*q12**2*(q12 - 2*q1q2 - 3*q22) + 
     -          4*p1q1**2*(p2q1**2*(3*q12 + 2*q1q2 - q22)*q22**2 + 
     -             2*p2q1*p2q2*q1q2*q22*(-3*q12 - 2*q1q2 + q22) + 
     -             p2q2**2*q1q2**2*(3*q12 + 2*q1q2 + 3*q22)) + 
     -    4*p1q1*q12*q22*(q12 - 2*q1q2 + q22)*(-(p2q2*q1q2) + p2q1*q22)*
     -           s - q12**2*q22**2*(q12 - 2*q1q2 + q22)*s**2 + 
     -          4*p1q2*p2q2*q12*
     -           (2*p1q1*(p2q2*q1q2*(q12 - 2*q1q2 - 3*q22) - 
     -                p2q1*q22*(q12 - 2*q1q2 + q22)) + 
     -             q12*q22*(q12 - 2*q1q2 + q22)*s)))*F22(ipdf))/
     -   (4.*mz**2*p1q1*p2q2*q12**2*q22**2) + 
     -  ((-((q12 - q22)**2*(2*p1q2*p2q1 - q1q2*s)) + 
     -       mz**2*(16*p1q1*(p2q2*q1q2 - p2q1*q22) + 
     -          2*p1q2*(-8*p2q2*q12 + p2q1*(q12 + 6*q1q2 + q22)) - 
     -       (q12*(q1q2 - 8*q22) + q1q2*(6*q1q2 + q22))*s))*F33(ipdf))/
     -   (4.*mz**2*p1q1*p2q2)
       endif

       endif

       end do
       end do

      endif
       enddo

c      const=mv1**4*mv2**4 *gf**3 *4d0*sqrt(2d0) /s
      const=mv1**2*mv2**2 *gf**2 /s
      prop=(q1+mv1**2)**2 * (q2+mv2**2)**2

      me=me+const*ff(0) /prop

      do ipdf=1,100
      mepdf(ipdf)=mepdf(ipdf)+const*ff(ipdf) /prop
       enddo

      enddo
      enddo
c----end of v-loop

      
      return
      end


      double precision function e2(par,qt2)
      implicit none
      double precision par(8),x1,x2,q1,q2,qt1,phi,qt2,sqrts,EE
      common/energy/sqrts
      x1=dexp(-par(6))
      x2=par(5)/x1
      q1=par(1)
      q2=par(2)
      qt1=par(3)
      phi=par(4)
      EE=sqrts/2d0
      e2=EE*x2*qt2**2/Q2+ EE*(1d0-x2)+Q2/(4d0*EE*x2)
      return
      end

      double precision function eh(par, qt2)
      implicit none
      double precision par(8),x1,x2,q1,q2,qt1,phi,qt2,pi,sqrts,c1,c2
      common/energy/sqrts
      x1=dexp(-par(6))
      x2=par(5)/x1
      q1=par(1)
      q2=par(2)
      qt1=par(3)
      phi=par(4)
      c1=sqrts/2d0*(x1+x2-x1*qt1**2/q1-x2*qt2**2/q2)
      c2=-(q2*x1+q1*x2)/(2d0*sqrts*x1*x2)
      eh=c1+c2
      return
      end

      double precision function integrand(xx)
      implicit none
      double precision xx(8), ff, par(8),sqrts,dsig,pi,
     1 q1up,q2up,num,den,me
      double precision x1,x2,q1,q2,q0,qt1,qt2,phi,gevtopb,tau,tauh,jac
     1 ,eps
      double precision qt2a, qt2b
      double precision eh, e2,s2
      double precision A,B,C
      integer ndim,ncomp
      common/energy/sqrts
      common/pi_gr/pi
      double precision mh,mp
      common/hmass/mh
      common/pmass/mp
      common/prec/eps
      common/conv/gevtopb
      double precision mepdf(100)
      common/me_pdf/mepdf
      double precision dsigpdf(100)
      common/dsig_pdf/dsigpdf
      double precision integralpdf(100)
      common/to_vegas/integralpdf
      double precision wgt, swgt
      common/vegas_wgt/wgt, swgt
      integer i,ipdf
      integer ilast
      common/last_integ/ilast
      double precision tmp
      integer npdf
      common/pdf_sets/npdf

      pi=4d0*datan(1d0)
      par(7)=xx(7)
      par(8)=xx(8)

      q0=4d0
      do i=1,6
      xx(i)=eps+xx(i)*(1d0-2d0*eps)
      end do
      q1up=(sqrts-mh)**2-q0

       par(1)=xx(1)*q1up+q0
      par(3)=xx(3)*sqrt(par(1))
      par(4)=xx(4)*2d0*pi
      q1=par(1)
      qt1=par(3)
      phi=par(4)
      tauh=mh**2/sqrts**2
      par(5)=tauh+(1d0-tauh)*xx(5)
      tau=par(5)
      par(6)=-dlog(tau)*xx(6)
      x1=dexp(-par(6))
      x2=tau/x1
      
      num=2d0*sqrts**2*(q1-qt1**2)*x1*x2*(sqrts**2*x1*x2*(qt1**2-q1)+
     1         q1**2+mh**2 *q1)
      den=q1*(2d0*sqrts**2*x1*x2*(qt1**2-q1)+q1*(2d0*q1+
     1             qt1**2*(cos(2d0*phi)-1d0)))
      q2up=num/den

      q2=xx(2)*(q2up-q0)+q0
      par(2)=q2

      jac=-q1up*2*pi*   (1d0-2*eps)**6
     1 * (q2up-q0) * sqrt(q1)* (1d0-tauh)*dlog(tau)
      
      x1=dexp(-par(6))
      x2=par(5)/x1

      A=sqrts**2*(qt1**2-q1)*x1*x2/(q1*q2)
      B=-2d0*qt1*cos(phi)
      C=-qt1**2 *x1*x2*sqrts**2/q1 +sqrts**2
     1 *x1*x2-mh**2-q2-q1+ q1*q2/(sqrts**2 *x1*x2)
      s2=q2*(1d0/x2-1d0)
      
      dsig=0d0
      do ipdf=1,npdf
      dsigpdf(ipdf)=0d0
      enddo

      if(((B*B-4d0*A*C).ge.0d0).and.(q2.ge.0d0))then

       qt2a=(-B-sqrt(B*B-4d0*A*C))/(2d0*A)
       qt2b=(-B+sqrt(B*B-4d0*A*C))/(2d0*A)

          if ((qt2a.ge.0d0).and.(e2(par, qt2a).ge.sqrt(s2)).and.
     1        eh(par,qt2a).ge.mh) then

                 dsig=dsig+me(par,qt2a) *
     2             (2d0*pi)**(-4) *qt1*qt2a/(4d0*x1*x2*
     1             abs(2d0*A*qt2a+B))

         if (ilast.eq.1) then
       do ipdf=1,npdf
                 dsigpdf(ipdf)=dsigpdf(ipdf)+mepdf(ipdf) *
     2             (2d0*pi)**(-4) *qt1*qt2a/(4d0*x1*x2*
     1             abs(2d0*A*qt2a+B))
         enddo
         endif

         endif

          if ((qt2b.ge.0d0).and.(e2(par, qt2b).ge.sqrt(s2)).and.
     1        eh(par,qt2b).ge.mh) then

                 dsig=dsig+me(par,qt2b) *
     2             (2d0*pi)**(-4) *qt1*qt2b/(4d0*x1*x2*
     1             abs(2d0*A*qt2b+B))

         if (ilast.eq.1) then
       do ipdf=1,npdf
                 dsigpdf(ipdf)=dsigpdf(ipdf)+mepdf(ipdf) *
     2             (2d0*pi)**(-4) *qt1*qt2b/(4d0*x1*x2*
     1             abs(2d0*A*qt2b+B))
         enddo
         endif


         endif
     
       endif

       do ipdf=1,npdf
       integralpdf(ipdf)=dsigpdf(ipdf)*jac*gevtopb
       enddo

      integrand=dsig*jac *gevtopb

      return
      end
       


c-------1->SM H
c-------2->GENERIC (SM-LIKE COUPLINGS) W+W-
c-------3->GENERIC (SM-LIKE COUPLINGS) Z0Z0
c-------4->GENERIC (SM-LIKE COUPLINGS) W-W-
c-------5->GENERIC (SM-LIKE COUPLINGS) W-Z0
c-------6->GENERIC (SM-LIKE COUPLINGS) W+Z0
c-------7->GENERIC (SM-LIKE COUPLINGS) W+W+
c-------102->LITTLEST HIGGS H
c-------103->LITTLEST HIGGS PHI 0
c-------104->LITTLEST HIGGS PHI +
c-------105->LITTLEST HIGGS PHI ++
c-------106->GEORGI-MACHACEK H_1 0
c-------107->GEORGI-MACHACEK H_5 0
c-------108->GEORGI-MACHACEK H_5 +
c-------109->GEORGI-MACHACEK H_5 ++
c-------110>GEORGI-MACHACEK H_1'0
c-------111>L-R SYMM H
c-------112>L-R SYMM DELTA 0
c-------113>L-R SYMM DELTA +
c-------114>L-R SYMM DELTA ++
c-----201->Z-prime
c-----202->W-prime +
c-----203->W-prime -
c-----204->Z-prime via WW
c-----205->Z-prime via ZZ
      subroutine set_vv_coupl(iproc)
      integer iproc,i,j
      double precision couplvv(3,3)
      common/coupl_vv/couplvv
      double precision vpov,vpov2
      common/vev_ratio/vpov
      double precision s0,s,c,sp,cp,f
      common/lh_s0/s0,s,c,sp,cp,f
      double precision mw,mz,gf,aa
      common/gfermi/gf
      common/masses/mw,mz
      double precision vof,vof2
      do i=1,3
      do j=1,3
      couplvv(i,j)=0d0
      enddo
      enddo
      aa=4d0*sqrt(2d0)
      vpov2=vpov*vpov
      vof=2d0/dsqrt(gf*aa) /f
      vof2=vof*vof
      write(*,*) "###process id:", iproc
c     V order : 1->W+ 2->W- 3->Z0 
c-------1->SM H
      if (iproc.eq.1) then
      couplvv(1,2)=aa*gf*mw**4
      couplvv(2,1)=aa*gf*mw**4
      couplvv(3,3)=aa*gf*mz**4
      endif
c-------2->GENERIC (SM-LIKE COUPLINGS) W+W-
      if (iproc.eq.2) then
      couplvv(1,2)=aa*gf*mw**4
      couplvv(2,1)=aa*gf*mw**4
      endif
c-------3->GENERIC (SM-LIKE COUPLINGS) Z0Z0
      if (iproc.eq.3) then
      couplvv(3,3)=aa*gf*mz**4
      endif
c-------4->GENERIC (SM-LIKE COUPLINGS) W-W-
      if (iproc.eq.4) then
      couplvv(2,2)=aa*gf*mw**4
      endif
c-------5->GENERIC (SM-LIKE COUPLINGS) W-Z0
      if (iproc.eq.5) then
      couplvv(3,2)=aa*gf*mw**2*mz**2
      couplvv(2,3)=aa*gf*mw**2*mz**2
      endif
c-------6->GENERIC (SM-LIKE COUPLINGS) W+Z0
      if (iproc.eq.6) then
      couplvv(3,1)=aa*gf*mw**2*mz**2
      couplvv(1,3)=aa*gf*mw**2*mz**2
      endif
c-------7->GENERIC (SM-LIKE COUPLINGS) W+W+
      if (iproc.eq.7) then
      couplvv(1,1)=aa*gf*mw**4
      endif
c-----102->LITTLEST HIGGS H
      if (iproc.eq.102) then
      couplvv(1,2)=aa*gf*mw**4
     1 *(1d0-vof2/3d0-(c**2-s**2)**2/2d0*vof2-
     1 s0**2/2d0-aa/2d0*s0*vpov)**2
      couplvv(2,1)=aa*gf*mw**4
     1 *(1d0-vof2/3d0-(c**2-s**2)**2/2d0*vof2-
     1 s0**2/2d0-aa/2d0*s0*vpov)**2
      couplvv(3,3)=aa*gf*mz**4
     1 *(1d0-vof2/3d0-((c**2-s**2)**2 +5d0*(cp**2-sp**2)**2)/2d0*vof2-
     1 s0**2/2d0-aa/2d0*s0*vpov)**2
      endif
c-----103->LITTLEST HIGGS PHI 0
c-----it's related to 6
      if (iproc.eq.103) then
      couplvv(1,2)=aa*gf*mw**4*(s0-aa/2d0*vpov)**2
      couplvv(2,1)=aa*gf*mw**4*(s0-aa/2d0*vpov)**2
      couplvv(3,3)=aa*gf*mz**4*(s0-aa*vpov)**2
      endif
c-----104->LITTLEST HIGGS PHI +
c------it's the same as 13, bugs may be related!      
      if (iproc.eq.104) then
      couplvv(1,3)=aa*gf*mw**2*mz**2*4d0*vpov2
      couplvv(3,1)=aa*gf*mw**2*mz**2*4d0*vpov2
      endif
c-----105->LITTLEST HIGGS PHI ++
c------it's the same as 14, bugs may be related!      
      if (iproc.eq.105) then
      couplvv(1,1)=aa*gf*mw**4*16d0*vpov2
      endif
c-----106->GEORGI-MACHACEK H_1 0
      if (iproc.eq.106) then
      couplvv(1,2)=aa*gf*mw**4*(1d0-8d0*vpov2)**2
      couplvv(2,1)=aa*gf*mw**4*(1d0-8d0*vpov2)**2
      couplvv(3,3)=aa*gf*mz**4*(1d0-8d0*vpov2)**2
      endif
c-----107->GEORGI-MACHACEK H_5 0
      if (iproc.eq.107) then
c------it's the same as 12 /3d0, bugs may be related!      
      couplvv(1,2)=aa*gf*mw**4*8d0*vpov2/3d0
      couplvv(2,1)=aa*gf*mw**4*8d0*vpov2/3d0
      couplvv(3,3)=aa*gf*mz**4*8d0*vpov2*4d0/3d0
      endif
c-----108->GEORGI-MACHACEK H_5 +
c------it's the same as 13 *2d0, bugs may be related!      
      if (iproc.eq.108) then
      couplvv(1,3)=aa*gf*mw**2*mz**2*4d0*vpov2*2d0
      couplvv(3,1)=aa*gf*mw**2*mz**2*4d0*vpov2*2d0
      endif
c-----109->GEORGI-MACHACEK H_5 ++
c------it's the same as 14, bugs may be related!      
      if (iproc.eq.109) then
      couplvv(1,1)=aa*gf*mw**4*16d0*vpov2
      endif
c-----110->GEORGI-MACHACEK H_1'0
      if (iproc.eq.110) then
      couplvv(1,2)=aa*gf*mw**4*8d0*vpov2*8d0/3d0
      couplvv(2,1)=aa*gf*mw**4*8d0*vpov2*8d0/3d0
      couplvv(3,3)=aa*gf*mz**4*8d0*vpov2*8d0/3d0
      endif
c-----111->L-R SYMM H
      if (iproc.eq.111) then
      couplvv(1,2)=aa*gf*mw**4
      couplvv(2,1)=aa*gf*mw**4
      couplvv(3,3)=aa*gf*mz**4
      endif
c-----112->L-R SYMM DELTA 0
      if (iproc.eq.112) then
      couplvv(1,2)=aa*gf*mw**4*8d0*vpov2
      couplvv(2,1)=aa*gf*mw**4*8d0*vpov2
      couplvv(3,3)=aa*gf*mz**4*8d0*vpov2*4d0
      endif
c-----113->L-R SYMM DELTA +
      if (iproc.eq.113) then
      couplvv(1,3)=aa*gf*mw**2*mz**2*4d0*vpov2
      couplvv(3,1)=aa*gf*mw**2*mz**2*4d0*vpov2
      endif
c-----114->L-R SYMM DELTA ++
      if (iproc.eq.114) then
      couplvv(1,1)=aa*gf*mw**4*16d0*vpov2
      endif
c-----201->Z-prime
      if (iproc.eq.201) then
      couplvv(1,2)=aa*gf*mw**4/mz**2 
      couplvv(2,1)=aa*gf*mw**4/mz**2
      couplvv(3,3)=aa*gf*mz**4/mz**2
      endif
c-----202->W-prime +
      if (iproc.eq.202) then
      couplvv(3,1)=aa*gf*mw**4/mz**2
      couplvv(1,3)=aa*gf*mw**4/mz**2
      endif
c-----203->W-prime -
      if (iproc.eq.203) then
      couplvv(3,2)=aa*gf*mw**4/mz**2
      couplvv(2,3)=aa*gf*mw**4/mz**2
      endif
c-----204->Z-prime via WW
      if (iproc.eq.204) then
      couplvv(1,2)=aa*gf*mw**4/mz**2
      couplvv(2,1)=aa*gf*mw**4/mz**2
      endif
c-----205->Z-prime via ZZ
      if (iproc.eq.205) then
      couplvv(3,3)=aa*gf*mw**4/mz**2
      endif


      return
      end

