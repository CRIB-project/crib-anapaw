c Analyzer 14 : encpileup
      SUBROUTINE Encpileup(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata)
c---------------------------------------------------------------------------
c ANALYZER 14 : encpileup
c---------------------------------------------------------------------------
c
c Definitions of ID should be same as those in src/conf/pileup.conf.
c
cID =  1:  F3Pl-L
cID =  2:  F3Pl-R
cID =  3:  F7Pl-L
cID =  4:  F7Pl-R
cID =  5:  (F5Pl-L)
cID =  6:  (F5Pl-R)
cID =  7:  F8Pl-L
cID =  8:  F8Pl-R
cID =  9:  F11Pl-L
cID = 10:  F11Pl-R
c
c W#: 1      2      3      4      5      6      7      8      9      10
c     ID     TFraw  TLraw  dTraw
c
c     11     12     13     14     15     16     17     18     19     20
c            TF     TL     dT <- not yet make
c
c---------------------------------------------------------------------------
c     ndata                     : the number of required data (ihit_min0)
c     nhitdet                   : the number of hit detector
c     hitdet(1:nhitdet)         : hit detector id, = id in mapfile
c     nhitdata(1:lenDet)        : the number of data for each id (ihit)
c     ndet                      : the length of array

      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'commonprm.fh'
      INCLUDE 'numbers.fh'
c      INCLUDE 'plasticprm.fh'
c      INCLUDE 'plasticconf.fh'
c      INCLUDE 'fpdata.fh'


      INTEGER nx, ny, ndet, ndata, naok
      INTEGER rawdata(ndata,ndet)
      INTEGER nhitdata(ndet)
      INTEGER hitdet(ndet)
      INTEGER nhitdet
      REAL    val(nx,ny)

c local
      INTEGER j,id
c      REAL    ovf
c      PARAMETER (ovf=4095.)
c      LOGICAL tflag1, tflag2
c      LOGICAL Gate1d
c      EXTERNAL Gate1d


      IF (initencflag(14)) THEN
         initencflag(14) = .FALSE.
      ENDIF

      naok = 0

c      write(*,*) ''
c      write(*,*) 'nHitDet',nHitDet
      DO j = 1, nHitDet
         id = hitDet(j)
c         write(*,*) 'hitDet(j), nData', hitDet(j), nData

         IF (nHitData(id).LT.nData) CYCLE

         naok = naok + 1
         val(1,naok) = id
         val(2,naok)  = rawdata(1,id)
         val(3,naok)  = rawdata(2,id)
         val(4,naok)  = rawdata(2,id)-rawdata(1,id)

c         write(*,*) 'rawdata(1,id),rawdata(2,id)',
c     &               rawdata(1,id),rawdata(2,id)
c         write(*,*) 'rawdata(2,id)-rawdata(1,id)',
c     &               rawdata(2,id)-rawdata(1,id)
c
c         tflag1 = .FALSE.
c         tflag2 = .FALSE.
c         IF (Gate1d(val(2,naok),0.0, ovf, 0)) THEN
c            val(12,naok) = (val(2,naok)+rand()-0.5)*plch2ns(1,id)
c            tflag1 = .TRUE.
c         ELSE
c            val(12,naok) = -1000.
c            tflag1 = .FALSE.
c         ENDIF
c         IF (Gate1d(val(3,naok),0.0, ovf, 0)) THEN
c            val(13,naok) = (val(3,naok)+rand()-0.5)*plch2ns(2,id)
c            tflag2 = .TRUE.
c         ELSE
c            val(13,naok) = -1000.
c            tflag2 = .FALSE.
c         ENDIF
c         
c         IF (tflag1 .AND. tflag2) THEN
c            val(21,naok) = (val(12,naok)+val(13,naok))*0.5
c            val(22,naok) = val(12,naok)-val(13,naok)
c         ELSE
c            val(21,naok) = -1000.
c            val(22,naok) = -1000.
c         ENDIF
 
      ENDDO


      RETURN
      END

