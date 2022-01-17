c Analyzer 15 : Encimage
      SUBROUTINE Encimage(val,nx,ny,naok)
c---------------------------------------------------------------------------
c ANALYZER 15 : Image
c---------------------------------------------------------------------------
c
c ZPOS Zposition is defined in 'src/prm/ppac/trackzpos.prm'
c
cID = 1 - 8 (nLayer) 
c     1 : F8PPAC-1
c     2 : F8PPAC-2
c     3 : F8 standard focus
c     4 : Target position
c     5 : F8PPAC-3
c
c
c
cW# : 1      2      3      4      5      6      7      8      9      10
c     ID     X      Y      A      B                           A(mrad)B(mard)
c     11
c     R
c---------------------------------------------------------------------------
c

      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'commonprm.fh'

      INTEGER nx, ny, naok
      REAL    val(nx,ny)

      INCLUDE 'numbers.fh'
      INCLUDE 'ppacfocus.fh'
      INCLUDE 'image.fh'
      INCLUDE 'fpdata.fh'

c local
      INTEGER i,ifcs

      IF (initencflag(15)) THEN
         CALL Loadimageprm
         initencflag(15) = .FALSE.
      ENDIF

      naok = 0

c     Tracking images

      DO i = 1, nlayer
         ifcs = int(trackzpos(1,i))
         IF (ifcs.NE.0 .AND. lfpdata(0,1,ifcs)) THEN
            naok = naok + 1
            val(1,naok) = i
            val(2,naok) = 
     &           fpdata(1,1,ifcs) + fpdata(3,1,ifcs) * trackzpos(2,i)
            val(3,naok) = 
     &           fpdata(2,1,ifcs) + fpdata(4,1,ifcs) * trackzpos(2,i)
            val(4,naok) = fpdata(3,1,ifcs)
            val(5,naok) = fpdata(4,1,ifcs)
            val(9,naok)  = atan(fpdata(3,1,ifcs))*1000.
            val(10,naok) = atan(fpdata(4,1,ifcs))*1000.
            val(11,naok) = sqrt( val(2,naok)**2 + val(3,naok)**2 )
         ENDIF
      ENDDO
      
      RETURN

      END
