*----------------------------------------------------------------
* H total grids
*----------------------------------------------------------------
*
      SUBROUTINE gridHt(mass,evalue)
* 
      IMPLICIT NONE
*
      INTEGER i,top,gdim
      REAL*8 u,value,evalue,mass
      REAL*8, dimension(103) :: bc,cc,dc
* 
* u value of M_H at which the spline is to be evaluated
*
      gdim= 103
*
      CALL FMMsplineSingleHt(bc,cc,dc,top,gdim)
*
      u= mass
      CALL Seval3SingleHt(u,bc,cc,dc,top,gdim,value)
*
      evalue= value
*
      RETURN
*
*-----------------------------------------------------------------------
*
      CONTAINS
*
      SUBROUTINE FMMsplineSingleHt(b,c,d,top,gdim)
*
*---------------------------------------------------------------------------
*
      INTEGER k,n,i,top,gdim,l
*
      REAL*8, dimension(103) :: xc,yc
      REAL*8, dimension(103) :: x,y
*
      REAL*8, DIMENSION(gdim) :: b 
* linear coeff
*
      REAL*8, DIMENSION(gdim) :: c 
* quadratic coeff.
*
      REAL*8, DIMENSION(gdim) :: d 
* cubic coeff.
*
      REAL*8 :: t
      REAL*8,PARAMETER:: ZERO=0.0, TWO=2.0, THREE=3.0
*
* The grid
*
*
      DATA (xc(i),i=1,103)/
     #   90.d0,95.d0,100.d0,105.d0,110.d0,115.d0,120.d0,
     #   125.d0,130.d0,135.d0,140.d0,145.d0,150.d0,155.d0,160.d0,165.d0,
     #   170.d0,175.d0,180.d0,185.d0,190.d0,195.d0,200.d0,210.d0,220.d0,
     #   230.d0,240.d0,250.d0,260.d0,270.d0,280.d0,290.d0,300.d0,310.d0,
     #   320.d0,330.d0,340.d0,350.d0,360.d0,370.d0,380.d0,390.d0,400.d0,
     #   410.d0,420.d0,430.d0,440.d0,450.d0,460.d0,470.d0,480.d0,490.d0,
     #   500.d0,510.d0,520.d0,530.d0,540.d0,550.d0,560.d0,570.d0,580.d0,
     #   590.d0,600.d0,610.d0,620.d0,630.d0,640.d0,650.d0,660.d0,670.d0,
     #   680.d0,690.d0,700.d0,710.d0,720.d0,730.d0,740.d0,750.d0,760.d0,
     #   770.d0,780.d0,790.d0,800.d0,810.d0,820.d0,830.d0,840.d0,850.d0,
     #   860.d0,870.d0,880.d0,890.d0,900.d0,910.d0,920.d0,930.d0,940.d0,
     #   950.d0,960.d0,970.d0,980.d0,990.d0,1000.d0/
*
      DATA (yc(i),i=1,103)/
     # 2.20d-3,2.32d-3,2.46d-3,2.62d-3,2.82d-3,3.09d-3,3.47d-3,4.03d-3,
     # 4.87d-3,6.14d-3,8.12d-3,1.14d-2,1.73d-2,3.02d-2,8.29d-2,2.46d-1,
     # 3.80d-1,5.00d-1,6.31d-1,8.32d-1,1.04d0,1.24d0,1.43d0,1.85d0,
     # 2.31d0,2.82d0,3.40d0,4.04d0,4.76d0,5.55d0,6.43d0,7.39d0,8.43d0,
     # 9.57d0,10.8d0,12.1d0,13.5d0,15.2d0,17.6d0,20.2d0,23.1d0,26.1d0,
     # 29.2d0,32.5d0,35.9d0,39.4d0,43.1d0,46.9d0,50.8d0,54.9d0,59.1d0,
     # 63.5d0,68.0d0,72.7d0,77.6d0,82.6d0,87.7d0,93.1d0,98.7d0,104.d0,
     # 110.d0,116.d0,123.d0,129.d0,136.d0,143.d0,150.d0,158.d0,166.d0,
     # 174.d0,182.d0,190.d0,199.d0,208.d0,218.d0,227.d0,237.d0,248.d0,
     # 258.d0,269.d0,281.d0,292.d0,304.d0,317.d0,330.d0,343.d0,357.d0,
     # 371.d0,386.d0,401.d0,416.d0,432.d0,449.d0,466.d0,484.d0,502.d0,
     # 521.d0,540.d0,560.d0,581.d0,602.d0,624.d0,647.d0/
*
      n= 103
      FORALL(l=1:103)
       x(l)= xc(l)
       y(l)= yc(l)
      ENDFORALL

*.....Set up tridiagonal system.........................................
*     b=diagonal, d=offdiagonal, c=right-hand side
*
      d(1)= x(2)-x(1)
      c(2)= (y(2)-y(1))/d(1)
      DO k= 2,n-1
       d(k)= x(k+1)-x(k)
       b(k)= TWO*(d(k-1)+d(k))
       c(k+1)= (y(k+1)-y(k))/d(k)
       c(k)= c(k+1)-c(k)
      END DO
*
*.....End conditions.  third derivatives at x(1) and x(n) obtained
*     from divided differences.......................................
*
      b(1)= -d(1)
      b(n)= -d(n-1)
      c(1)= ZERO
      c(n)= ZERO
      IF (n > 3) THEN
       c(1)= c(3)/(x(4)-x(2))-c(2)/(x(3)-x(1))
       c(n)= c(n-1)/(x(n)-x(n-2))-c(n-2)/(x(n-1)-x(n-3))
       c(1)= c(1)*d(1)*d(1)/(x(4)-x(1))
       c(n)= -c(n)*d(n-1)*d(n-1)/(x(n)-x(n-3))
      END IF
*
      DO k=2,n    ! forward elimination
       t= d(k-1)/b(k-1)
       b(k)= b(k)-t*d(k-1)
       c(k)= c(k)-t*c(k-1)
      END DO
*
      c(n)= c(n)/b(n)   
*
* back substitution ( makes c the sigma of text)
*
      DO k=n-1,1,-1
       c(k)= (c(k)-d(k)*c(k+1))/b(k)
      END DO
*
*.....Compute polynomial coefficients...................................
*
      b(n)= (y(n)-y(n-1))/d(n-1)+d(n-1)*(c(n-1)+c(n)+c(n))
      DO k=1,n-1
       b(k)= (y(k+1)-y(k))/d(k)-d(k)*(c(k+1)+c(k)+c(k))
       d(k)= (c(k+1)-c(k))/d(k)
       c(k)= THREE*c(k)
      END DO
      c(n)= THREE*c(n)
      d(n)= d(n-1)
*
      RETURN
*
      END Subroutine FMMsplineSingleHt   
*
*------------------------------------------------------------------------
*
      SUBROUTINE Seval3SingleHt(u,b,c,d,top,gdim,f,fp,fpp,fppp)
*
* ---------------------------------------------------------------------------
*
      REAL*8,INTENT(IN) :: u 
* abscissa at which the spline is to be evaluated
*
      INTEGER j,k,n,l,top,gdim
*
      REAL*8, dimension(103) :: xc,yc
      REAL*8, dimension(103) :: x,y
      REAL*8, DIMENSION(gdim) :: b,c,d 
* linear,quadratic,cubic coeff
*
      REAL*8,INTENT(OUT),OPTIONAL:: f,fp,fpp,fppp 
* function, 1st,2nd,3rd deriv
*
      INTEGER, SAVE :: i=1
      REAL*8    :: dx
      REAL*8,PARAMETER:: TWO=2.0, THREE=3.0, SIX=6.0
*
* The grid
*
      DATA (xc(l),l=1,103)/
     #   90.d0,95.d0,100.d0,105.d0,110.d0,115.d0,120.d0,
     #   125.d0,130.d0,135.d0,140.d0,145.d0,150.d0,155.d0,160.d0,165.d0,
     #   170.d0,175.d0,180.d0,185.d0,190.d0,195.d0,200.d0,210.d0,220.d0,
     #   230.d0,240.d0,250.d0,260.d0,270.d0,280.d0,290.d0,300.d0,310.d0,
     #   320.d0,330.d0,340.d0,350.d0,360.d0,370.d0,380.d0,390.d0,400.d0,
     #   410.d0,420.d0,430.d0,440.d0,450.d0,460.d0,470.d0,480.d0,490.d0,
     #   500.d0,510.d0,520.d0,530.d0,540.d0,550.d0,560.d0,570.d0,580.d0,
     #   590.d0,600.d0,610.d0,620.d0,630.d0,640.d0,650.d0,660.d0,670.d0,
     #   680.d0,690.d0,700.d0,710.d0,720.d0,730.d0,740.d0,750.d0,760.d0,
     #   770.d0,780.d0,790.d0,800.d0,810.d0,820.d0,830.d0,840.d0,850.d0,
     #   860.d0,870.d0,880.d0,890.d0,900.d0,910.d0,920.d0,930.d0,940.d0,
     #   950.d0,960.d0,970.d0,980.d0,990.d0,1000.d0/
*
      DATA (yc(l),l=1,103)/
     # 2.20d-3,2.32d-3,2.46d-3,2.62d-3,2.82d-3,3.09d-3,3.47d-3,4.03d-3,
     # 4.87d-3,6.14d-3,8.12d-3,1.14d-2,1.73d-2,3.02d-2,8.29d-2,2.46d-1,
     # 3.80d-1,5.00d-1,6.31d-1,8.32d-1,1.04d0,1.24d0,1.43d0,1.85d0,
     # 2.31d0,2.82d0,3.40d0,4.04d0,4.76d0,5.55d0,6.43d0,7.39d0,8.43d0,
     # 9.57d0,10.8d0,12.1d0,13.5d0,15.2d0,17.6d0,20.2d0,23.1d0,26.1d0,
     # 29.2d0,32.5d0,35.9d0,39.4d0,43.1d0,46.9d0,50.8d0,54.9d0,59.1d0,
     # 63.5d0,68.0d0,72.7d0,77.6d0,82.6d0,87.7d0,93.1d0,98.7d0,104.d0,
     # 110.d0,116.d0,123.d0,129.d0,136.d0,143.d0,150.d0,158.d0,166.d0,
     # 174.d0,182.d0,190.d0,199.d0,208.d0,218.d0,227.d0,237.d0,248.d0,
     # 258.d0,269.d0,281.d0,292.d0,304.d0,317.d0,330.d0,343.d0,357.d0,
     # 371.d0,386.d0,401.d0,416.d0,432.d0,449.d0,466.d0,484.d0,502.d0,
     # 521.d0,540.d0,560.d0,581.d0,602.d0,624.d0,647.d0/
*
      n= 103
      FORALL(l=1:103)
       x(l)= xc(l)
       y(l)= yc(l)
      ENDFORALL
*
*.....First check if u is in the same interval found on the
*     last call to Seval.............................................
*
      IF (  (i<1) .OR. (i >= n) ) i=1
      IF ( (u < x(i))  .OR.  (u >= x(i+1)) ) THEN
       i=1   
*
* binary search
*
       j= n+1
       DO
        k= (i+j)/2
        IF (u < x(k)) THEN
         j= k
        ELSE
         i= k
        ENDIF
        IF (j <= i+1) EXIT
       ENDDO
      ENDIF
*
      dx= u-x(i)   
*
* evaluate the spline
*
      IF (Present(f))    f= y(i)+dx*(b(i)+dx*(c(i)+dx*d(i)))
      IF (Present(fp))   fp= b(i)+dx*(TWO*c(i) + dx*THREE*d(i))
      IF (Present(fpp))  fpp= TWO*c(i) + dx*SIX*d(i)
      IF (Present(fppp)) fppp= SIX*d(i)
*
      RETURN
*
      END Subroutine Seval3SingleHt  
*
      END SUBROUTINE gridHt
*
