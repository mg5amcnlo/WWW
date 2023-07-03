      program make_run_card
      implicit none
      integer sch,icoll,scales,imuf,imur,cnt,pdf,icomp,npdf,iproc
      double precision sqrts,mh,xmuf,xmur
      character *30 output, pdflo, pdfnlo, pdfnnlo
      
      character(8):: date
      character(10):: time
      character(5):: zone
     
      integer values(8)

      double precision mw, mz, wwdth, zwdth, mt, mb, mp, gf, sthw,gevpb
      double precision vpov,lhs0,lhs,lhc,lhsp,lhcp,lhf
      integer iwdth

      mw=80.398d0
      mz=91.1876d0
      wwdth=2.141d0
      zwdth=2.4952d0
      mt=172.5d0
      mb=4.75d0
      mp=0.938272013d0
      gf=1.16637d-5
      sthw=0.23119d0
      gevpb=389379660d0
      vpov=0.5d0
      lhf=4000.d0
      lhs0=0.2d0
      lhs=0.2d0
      lhc=dsqrt(1d0-lhs**2)
      lhsp=0.2d0
      lhcp=dsqrt(1d0-lhsp**2)


      read(*,*) cnt,iproc,icomp,sch, icoll, sqrts, mh, scales,
     1 imuf, imur,output,
     1 pdflo, pdfnlo, pdfnnlo,pdf,iwdth
      xmuf=2.0**imuf
      xmur=2.0**imur

c-----write output file header on the 1st run      
      if (cnt.eq.0) then
      call date_and_time(date,time,zone,values)
      open(unit=50, status="unknown", file=output)

      write(50,101)"#","  DATE:    ",date
      write(50,101)"#","  AUTHOR:  ","marco zaro"

      write(50,101)"#","  LO pdf   ", pdflo
      write(50,101)"#","  NLO pdf  ", pdfnlo
      write(50,101)"#","  NNLO pdf ", pdfnnlo
      write(50,101)
      write(50,102)"#","Mp         ",mp
      write(50,102)"#","Mw         ",mw
      write(50,102)"#","Mz         ",mz
      write(50,102)"#","Wwidth     ",wwdth
      write(50,102)"#","Zwidth     ",zwdth
      write(50,102)"#","Gf         ",gf
      write(50,102)"#","sin^2(thw) ",sthw
      write(50,102)"#","GeV2pb     ",gevpb

      write(50,100)"#","proc","coll","Hwdth",
     1  "E_cm", "Mh ", "sc","PDF", "xmuf","xmur",
     1 "LO    ","NLO   ","NNLO  ", "pdferrLO ","pdferrNLO",
     2  "pdferrNNLO"
      write(*,*)

      close(50)
100   format(a1,3a7,a9,a10,2a3,2a7,6a15)
101   format(a1,a12,a30)
102   format(a1,a12,e15.10)      
      endif
      
      open(unit=55, status="unknown", file="param_card.dat")
      write(55,*)"wmass    = ",mw
      write(55,*)"zmass    = ",mz
      write(55,*)"topmass  = ",mt
      write(55,*)"botmass  = ",mb
      write(55,*)"pmass    = ",mp
      write(55,*)"wwidth   = ",wwdth
      write(55,*)"zwidth   = ",zwdth
      write(55,*)"gfermi   = ",gf
      write(55,*)"sthw^2   = ",sthw
      write(55,*)"epsilon  = ",1.0d-14
      write(55,*)"gev2pb   = ",gevpb
      write(55,*)"vp/v     = ",vpov
      write(55,*)"LH f     = ",lhf
      write(55,*)"LH s0    = ",lhs0
      write(55,*)"LH s     = ",lhs
      write(55,*)"LH c     = ",lhc
      write(55,*)"LH sp    = ",lhsp
      write(55,*)"LH cp    = ",lhcp

      close(55)


      open(unit=60, status="unknown", file="run_card.dat")
      write(60,*)"procID   = ",iproc
      write(60,*)"order    = ",icomp
      write(60,*)"schann   = ",sch
      write(60,*)"icoll    = ",icoll
      write(60,*)"energy_cm= ",sqrts
      write(60,*)"mass     = ",mh
      write(60,*)"sc0mh1Q^2= ",scales
      write(60,*)"muf/sc   = ",xmuf
      write(60,*)"mur/sc   = ",xmur
      write(60,*)"output   = ",output
      write(60,*)"pdflo    = ",pdflo
      write(60,*)"pdfnlo   = ",pdfnlo
      write(60,*)"pdfnnlo  = ",pdfnnlo
      write(60,*)"PDF err  = ",pdf
      write(60,*)"HWdth    = ",iwdth
      close(60)
      return
      end
