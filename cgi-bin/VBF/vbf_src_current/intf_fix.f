      program intf_fix
      implicit none
      double precision sqrtsb,mhb,xmufb,xmurb, sigtotb(6), intfb, intfsb
      double precision sqrtsg,mhg,xmufg,xmurg, sigtotg(6), intfg, intfsg

      integer scb, pdfb, scg, pdfg
      integer i, j,k

      open(unit=70, file="res0226.dat", status="unknown")
      open(unit=80, file="resnnlo.dat", status="unknown")
      open(unit=90, file="resnew.dat", status="unknown")

      do i=1,54

      read(80,120)sqrtsg, mhg,scg,pdfg,xmufg,xmurg, sigtotg,intfg,intfsg
   
       do j=1,25
       read(80,120)sqrtsb,mhb,scb,pdfb,xmufb,xmurb, sigtotb,intfb,intfsb
       
       if((
     1 sqrtsb.eq.sqrtsg).and.(mhb.eq.mhg).and.(scb.eq.scg).and.
     1  (pdfb.eq.pdfg))
     1  then
        do k=1,6
        sigtotb(k)=sigtotb(k)-intfb+intfg
        enddo
        write(90,120) sqrtsb,mhb,scb,pdfb, xmufb, xmurb, sigtotb, intfg,
     1    intfsg
       else
               write(*,*)"ERROR"
       endif

      enddo
      enddo
      close(70)
      close(80)
      close(90)


120   format(f10.0,f10.0,2(i3),2(f7.4),8(e15.6))

      return
      end
