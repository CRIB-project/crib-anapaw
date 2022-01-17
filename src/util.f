c ---------------------------------------------------------------------------
      SUBROUTINE Aoq_calc(brho,beta,aoq)
      IMPLICIT NONE
      INCLUDE 'physdata.fh'
      REAL    brho,beta,aoq

      IF (beta.GT.0.) THEN
         aoq = brho*clight/amu/beta/beta2gamma(beta)
      ELSE
         aoq = -1000.
      ENDIF

      RETURN
      END

c ---------------------------------------------------------------------------
      REAL FUNCTION Beta2gamma(beta)
      IMPLICIT NONE
      REAL beta

      beta2gamma = 1./sqrt(1.0-beta*beta)

      RETURN
      END

c ---------------------------------------------------------------------------
c 2009 12/12 Matsushita
      REAL FUNCTION Beta2brho(a,q,beta)
      IMPLICIT NONE
      REAL a,q
      REAL beta
      REAL amu,clight
      PARAMETER (amu=931.49432,clight=299.792458)


      beta2brho = A/Q * beta / sqrt(1.0-beta*beta) * amu / clight

      RETURN
      END

c ---------------------------------------------------------------------------
      LOGICAL FUNCTION Gate1d(x,xmin,xmax,ctrl)
c
c ctrl = 0 : xmin <  x <  xmax
c ctrl = 1 : xmin <= x <= xmax
c
      IMPLICIT NONE
      REAL    x, xmin, xmax
      INTEGER ctrl

      Gate1d = .FALSE.

      IF (ctrl.EQ.1) THEN
         IF ((x.GE.xmin) .AND. (x.LE.xmax)) THEN
            Gate1d = .TRUE.
         ENDIF
      ELSE
         IF ((x.GT.xmin) .AND. (x.LT.xmax)) THEN
            Gate1d = .TRUE.
         ENDIF
      ENDIF

      RETURN
      END

c ---------------------------------------------------------------------------
      SUBROUTINE Lclr(lflag,nl,flag)
      IMPLICIT NONE
      INTEGER nl,flag,i
      LOGICAL lflag(nl)

      DO i = 1, nl, 1
         IF (flag.EQ.1) THEN
            lflag(i) = .TRUE.
         ELSE
            lflag(i) = .FALSE.
         ENDIF
      ENDDO

      RETURN
      END

c ---------------------------------------------------------------------------
      SUBROUTINE Matinv(n,m,a)
      IMPLICIT NONE
      INTEGER n, m, i, j, k
      DOUBLE PRECISION a(n,m)

      DO k = 1, n
         DO j = k+1, m
            a(k,j) = a(k,j) / a(k,k)
         ENDDO
         DO i = 1, n
            IF (i .NE. k) THEN
               DO j = k+1, m
                  a(i,j) = a(i,j) - a(i,k)*a(k,j)
               ENDDO
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END

c ---------------------------------------------------------------------------
      SUBROUTINE Rayfit(px,pz,np,bit,pfx,pft,res,ierr)
      IMPLICIT NONE

      INTEGER np, bit, ierr, i, j
      REAL    px(np),pz(np)
      REAL    pfx,pft,res,sigma,det,dx
      REAL    a(2,2),b(2),w(2)
      PARAMETER (sigma = 0.5)
      
      res = 0.
      DO i = 1, 2
         b(i) = 0.
         DO j = 1, 2
            a(i,j) = 0.
         ENDDO
      ENDDO

      IF (np .LT. 2) THEN
         ierr = 1
         pfx = px(1)
         pft = -1000.
         RETURN
      ENDIF

      DO i = 1, np
         IF (2**(i-1).EQ.iand(bit,2**(i-1))) THEN
            b(1) = b(1) + px(i) * pz(i)
            b(2) = b(2) + px(i)
            a(1,1) = a(1,1) + pz(i)**2
            a(1,2) = a(1,2) + pz(i)
            a(2,2) = a(2,2) + 1.0
         ENDIF
      ENDDO

      a(2,1) = a(1,2)
      det = a(1,1)*a(2,2) - a(2,1)*a(1,2)

      IF (det.EQ.0.) THEN
         ierr = 1
         RETURN
      ENDIF

      w(1) =  a(2,2)/det * b(1) - a(1,2)*b(2)/det
      w(2) = -a(2,1)/det * b(1) + a(1,1)/det*b(2)
      pfx = w(2)
      pft = w(1)

      j = 0
      DO i = 1, np
         IF (2**(i-1).EQ.iand(bit,2**(i-1))) THEN
            j = j + 1
            dx = abs(px(i) - (pft*pz(i)+pfx))
            res = res + (dx/sigma)**2
         ENDIF
      ENDDO

      res = res/(j-2)
      ierr = 0

      RETURN
      END

c ---------------------------------------------------------------------------
      SUBROUTINE Readmat(fname,matrix,nx,ny,nline,
     &     pm,maxorder,order,ier)

      IMPLICIT NONE
      INTEGER nx,ny,nline,maxorder,order,ier
      REAL    matrix(nx,ny)
      INTEGER pm(0:maxorder, 0:maxorder, 0:maxorder, 
     &           0:maxorder, 0:maxorder, 0:maxorder)
      CHARACTER*132 fname

      REAL    prm(5)
      INTEGER power(6),i,k1,k2,k3,k4,k5,k6

      DO k1 = 0, order, 1
         DO k2 = 0, order, 1
            DO k3 = 0, order, 1
               DO k4 = 0, order, 1
                  DO k5 = 0, order, 1
                     DO k6 = 0, order, 1
                        pm(k1,k2,k3,k4,k5,k6) = 0
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDDO

      ier = 0
      nline = 0

      OPEN (UNIT=90, FILE=fname, STATUS='old', ERR=999)

      DO WHILE (.TRUE.)
         CALL Skip_comment(90)
         READ(90,600,END=998) (prm(i),i=1,5),(power(i),i=1,6)
         nline = nline + 1
         DO i = 1, 5, 1
            matrix(nline,i) = prm(i)
         ENDDO
         pm(power(1),power(2),power(3),
     &      power(4),power(5),power(6)) = nline
      ENDDO

 600  FORMAT(5G14.6, 2X, 6I1)
 998  CONTINUE
      CLOSE(90)
      RETURN
 999  WRITE(*,'(2A)')' ANAPAW-E : [Loadmatrixconf] Cannot open file.',
     &     fname(1:Len_trim(fname))
 1000 ier = -1
      RETURN
      END

c ---------------------------------------------------------------------------
      SUBROUTINE Rec_beta(brho12,brho23,fl12,fl23,tof13,
     &                    beta12,beta23)
      IMPLICIT NONE
      INCLUDE 'physdata.fh'
c input
      REAL brho12,brho23,fl12,fl23,tof13
c output
      REAL beta12,beta23
c local
      REAL alpha,a1

      alpha = brho23/brho12
      IF (brho12 .GT. 0. .AND. brho23 .GT. 0. ) THEN
         a1 = SQRT( alpha**2 * clight**2 * tof13**2 
     c        + (alpha**4 - alpha**2)*fl12**2
     c        + (1 - alpha**2) * fl23**2 )
         beta12 = ( a1*fl12 + fl23*clight*tof13 )
     c        / ( a1*clight*tof13 + (1-alpha**2)*fl12*fl23 )
         beta23 = ( a1*fl12 + fl23*clight*tof13 )
     c        / ( clight**2 * tof13**2 + (alpha**2-1)*fl12**2 )
      ELSE
         beta12 = -1000.
         beta23 = -1000.
      ENDIF

      RETURN
      END

c ---------------------------------------------------------------------------
      SUBROUTINE Rec_delta(fv,tv,delta,rec_angle,residu,
     &                     matrix,nx,ny,mline,pm,maxorder,order,
     &                     flag,ier)
      IMPLICIT NONE
c input  vector 1:x,2:a,3:y,4:b,5:delta
      REAL    fv(5),tv(5)
c output
      REAL    delta,rec_angle,residu
c matrix information
      INTEGER nx,ny
      REAL    matrix(nx,ny)
      INTEGER mline,maxorder,order,flag,ier
      INTEGER pm(0:maxorder,0:maxorder,0:maxorder,
     &           0:maxorder,0:maxorder,0:maxorder)
c local
      REAL    a(2,2),b(2,2)
      REAL    vec1(2),vec2(2)
      INTEGER i,j

c      DO i=1,4
c        IF ((fv(i).LE.-1000.) .OR. (tv(i).LE.-1000.)) THEN
c           delta = -1000.
c           rec_angle = -1000.
c           RETURN
c        ENDIF
c      ENDDO

      IF (order.EQ.1) THEN
c first order rec. using only x information
         a(1,1) = matrix(pm(0,0,0,0,0,1),1) ! (x|d)
         a(2,1) = matrix(pm(0,0,0,0,0,1),2) ! (a|d)
         a(1,2) = matrix(pm(0,1,0,0,0,0),1) ! (x|a)
         a(2,2) = matrix(pm(0,1,0,0,0,0),2) ! (a|a)
         IF (flag.EQ.0) THEN
            CALL Reverse_mat(a,b,ier)
            IF (ier .GT. 0) THEN
               DO i = 1, 2
                  vec2(i) = 0
                  DO j = 1, 2
                     vec1(j) = tv(j)-fv(1)*matrix(pm(1,0,0,0,0,0),j)
                     vec2(i) = vec2(i) + b(i,j)*vec1(j)
                  ENDDO
               ENDDO
               ier = 1
            ELSE
               ier = -1
               RETURN
            ENDIF
         ENDIF
         delta = vec2(1)
         rec_angle = vec2(2)
         residu = vec2(2) - fv(2)
      ELSE
c higher order rec.
      ENDIF

      RETURN
      END

c ---------------------------------------------------------------------------
      SUBROUTINE Rec_delta_1st(fv,tv,id,delta,rec_ang_x,rec_ang_y,
     &                     residu_x,residu_y,ier)
      IMPLICIT NONE
      INCLUDE 'numbers.fh'
      INCLUDE 'invmatrix.fh'
c input  vector 1:x,2:a,3:y,4:b,5:delta
      REAL    fv(5),tv(5)
c output
      REAL    delta,rec_ang_x,rec_ang_y,residu_x,residu_y
c local
      REAL    vec1(2),vec2(2)
      INTEGER i,j,id,ier

      ier = 1

c first order rec.
      IF (lmierr(1,id)) THEN
         DO i = 1, 2
            vec2(i) = 0
            DO j = 1, 2
               vec1(j) = tv(j)-fv(1)*mxx(j,id)
               vec2(i) = vec2(i) + mx(i,j,id)*vec1(j)
            ENDDO
         ENDDO
      ELSE
         ier = -1
         RETURN
      ENDIF
      delta     = vec2(1)
      rec_ang_x = vec2(2)
      residu_x  = vec2(2) - fv(2)

      IF (lmierr(2,id)) THEN
         vec2(1) = my(1,1,id)*tv(3) + my(2,1,id)*tv(4)
         vec2(2) = my(1,2,id)*tv(3) + my(2,2,id)*tv(4)
      ELSE
         ier = -1
         RETURN
      ENDIF
      rec_ang_y = vec2(2)
      residu_y  = vec2(2) - fv(4)


      RETURN
      END

c ---------------------------------------------------------------------------
      SUBROUTINE Reverse_mat(a,b,ier)
      IMPLICIT NONE
      REAL    a(2,2),b(2,2)
      INTEGER ier
c     local
      REAL    det

      det = a(1,1)*a(2,2) - a(1,2)*a(2,1)
      IF (abs(det).LE.1e-4) THEN
         ier = -1
      ELSE
         b(1,1) = a(2,2)/det
         b(2,2) = a(1,1)/det
         b(1,2) = -a(1,2)/det
         b(2,1) = -a(2,1)/det
         ier = 1
      ENDIF

      RETURN
      END

c ---------------------------------------------------------------------------
      SUBROUTINE Z_calc(de,beta,ionpair,z_cof,z)
      IMPLICIT NONE
      REAL    de,beta,ionpair
      REAL    z_cof(2)
      REAL    z, de_v

      IF (beta.GT.0) THEN
         de_v = log(ionpair*beta**2) - log(1.0-beta**2) - beta**2
         z = z_cof(1)*sqrt(de/de_v)*beta + z_cof(2)
      ENDIF

      RETURN
      END

c ---------------------------------------------------------------------------
      SUBROUTINE Sorting(datain,dataout,in,inr,N)
c
c   sorting datain(N)
c   sorted output : dataout(N)
c   sorted original index : inr(N)
c
c  Example
c                   1   2   3   4   5
c    datain(1:5):  10.  3.  5. 13.  2.
c    dataout(1:5): 13. 10.  5.  3.  2.
c    inr(1:5):      4   1   3   2   5
c    in(1:5):       2   4   3   1   5
c    dataout(j) = datain(inr(j))
c    datain(k)  = dataout(in(k))
cccc    min case
cccc    dataout(1:5):  2.  3.  5. 10. 13.
cccc    inr(1:5):      5   2   3   1   4
cccc    in(1:5):       4   2   3   5   1
c
      IMPLICIT NONE
      INTEGER  n, in(n), inr(n)
      REAL     datain(n),dataout(n)
      INTEGER  i,j,itmp
      REAL     datamax
c
      DO i = 1, n
         dataout(i) = datain(i)
         inr(i) = i
      ENDDO
      DO i = 1, n
         datamax = dataout(i)
         DO j = i+1, n
            IF (dataout(j) .GE. datamax) THEN
               datamax = dataout(j)
               dataout(j) = dataout(i)
               dataout(i) = datamax
               itmp = inr(j)
               inr(j) = inr(i)
               inr(i) = itmp
            ENDIF
         ENDDO
      ENDDO
      DO i = 1, n
         in(inr(i)) = i
      ENDDO

      RETURN
      END
     
c ---------------------------------------------------------------------------
