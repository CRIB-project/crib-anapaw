c Analyzer 16 : Enclabr
      SUBROUTINE Enclabr(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata)
c---------------------------------------------------------------------------
c ANALYZER 16 : LaBr
c---------------------------------------------------------------------------
cID = 1:5: LaBr1:5
c   W#:  1   2     3    4    5    6    7    8   9  10
c       ID   A[ch] T
cID = 123  (E2+E3)/2
c   W#:  1   2     3    4    5    6    7    8   9  10
c       ID   A[ch] T
c   
      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'commonprm.fh'
      INCLUDE 'numbers.fh'
c
      INCLUDE 'fpdata.fh'
      INCLUDE 'pid.fh'
c
      INTEGER nx, ny, ndet, ndata, naok
      INTEGER rawdata(ndata,ndet)
      INTEGER nhitdata(ndet)
      INTEGER hitdet(ndet)
      INTEGER nhitdet
      REAL    val(nx,ny)

c local
      INTEGER i,id,j
      real Traw
      real Eraw(5)
      real E23,E14,E1234,E1234b
c begin

      IF (initencflag(16)) THEN
         initencflag(16) = .FALSE.
      ENDIF

      naok = 0


      Traw = rawdata(6,1)

      DO j = 1, 5

         Eraw(j) = rawdata(j,1)

         naok = naok + 1
         val(1,naok) = j
         val(2,naok) = Eraw(j)
         val(3,naok) = Traw
      ENDDO

c            write(*,*)'hit'

      E1234 = (Eraw(1)+Eraw(2)+Eraw(3)+Eraw(4))/4.
      E1234b = (Eraw(1)*.5+Eraw(2)+Eraw(3)+Eraw(4)*.5)/3.

      E23 = (Eraw(2)+Eraw(3))/2.
      E14 = (Eraw(1)+Eraw(4))/2.

      naok = naok + 1
      val(1,naok) = 101
      val(2,naok) = E1234
c      val(4,naok) = E1234-(62360-176.74* tof(7,11,1))
      val(4,naok) = E1234-(55500.-168.98* tof(7,11,1))

      naok = naok + 1
      val(1,naok) = 102
      val(2,naok) = E1234b
      val(4,naok) = E1234b-(64299-183.25* tof(7,11,1))


      naok = naok + 1
      val(1,naok) = 123
      val(2,naok) = E23

      val(3,naok) = tof(7,11,1)
      val(4,naok) = E23-(65361-186.53* tof(7,11,1))

      naok = naok + 1
      val(1,naok) = 114
      val(2,naok) = E14


      RETURN
      END





