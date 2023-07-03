       program struct_chk
       implicit none
       double precision reg(2),res,err, p
       external integrand
       integer i,j,nf,ord,v
       integer mchk, nmell
       common/mell_check/mchk
       common/mellin/nmell
       double precision xf,qf,sthw,pi,f(-6:6),eps
       common/check/xf,ord,v
       common/sin_wein/sthw
       common/pi_gr/pi
       common/nflav/nf
       common/prec/eps
       double precision ti,tf
c       call initpdfsetbyname("cteq6m.LHpdf")
       call initpdfsetbyname("MSTW2008nnlo68cl.LHgrid")
       call initpdf(0)
       sthw=0.2312d0
       sthw=0.2222d0
       pi=4d0*atan(1d0)
       mchk=0
       reg(1)=0d0
       reg(2)=1d0
       nf=5
       eps=1d-6
       call cpu_time(ti)
       do ord=3,3
       write(*,*) "PERT ORD ", ord
       do v=1,3
       do i=1,5
       xf=4d0**(-i)
       mchk=0
       nmell=i*2
       call vegas(reg,1, integrand,0, 2500, 5,-1, res, err, p)
       write(*,*) ord,v,xf,res

       enddo
       write(*,*)
       enddo
       enddo
       call cpu_time(tf)
       write(*,*) "RUNNING TIME", tf-ti

       stop
       end

       double precision function integrand(xx)
       implicit none
       integer ord,v
       double precision int3nsnnlo_z,int3nloq, int3nsnnlo_w,
     1 int3nspnnlo_w
       double precision int2nloq, int2nlog,Int2nsmNNLO_W,int2qnnlo_w
       double precision xx(1),z,x,lf,lr
       double precision xf, qf, muf,mur,f2,f3,f1,fl
       double precision pdf1(-6:6), pdfz(-6:6)
       common/check/xf,ord,v
    
       qf=91.0d0
       z=xx(1)*(1d0-xf) +xf


       muf=qf*2d0
       mur=qf/2d0
       lf=2d0*log(qf/muf)
       lr=2d0*log(mur/muf)
       call PDF(xf,muf,pdf1)
       call PDF(xf/z,muf,pdfz)

       integrand=f3(1,xf,qf,muf,mur,ord,v,z,pdfz,pdf1)

       return 
       end
