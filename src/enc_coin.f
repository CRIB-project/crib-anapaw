c Analyzer 9 : enccoin
      SUBROUTINE ENCCOIN(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata)
c---------------------------------------------------------------------------
c ANALYZER 9 : COIN
c---------------------------------------------------------------------------
c Coin. Reg. Channel
c  1: DALI
c  2: DSB (DSF7)
c  3: Beam x DALI
c  4: Beam x F11
c
c   W#: 1   2         3        4 
c       ID  ibit*100  ID or 0  ibit
c---------------------------------------------------------------------------
c     nDataMin                  : the number of required data (ihit_min0)
c     hitDet(1:nHitDet)         : hit detector id, = id in mapfile
c     nData(1:lenDet)           : the number of data for each id (ihit)
c     nHitDet                   : the number of hit detector
c     lenData                   : the length of array
c     lenDet                    : the length of array

      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'

      INTEGER nx, ny, ndet, ndata, naok
      INTEGER rawdata(ndata,ndet)
      INTEGER nhitdata(ndet)
      INTEGER hitdet(ndet)
      INTEGER nhitdet
      REAL    val(nx,ny)

c local
      INTEGER i,id
      INTEGER ibit, imask, icoin

      IF (initencflag(3)) THEN
         initencflag(3) = .FALSE.
      ENDIF

c     initialize
      naok = 0

c      icoin  = rawdata(1,1)

      DO i = 1, 3
         naok = naok + 1
         id = i

         ibit=0
         if (rawdata(1,id).gt.0) then
            ibit=1
         endif

         val(1,naok) = id
         val(2,naok) = ibit*100.

         IF (ibit.NE.0) THEN
            val(3,naok) = i
         ELSE
            val(3,naok) = 0
         ENDIF

         val(4,naok) = rawdata(1,id)

      ENDDO

      RETURN
      END
