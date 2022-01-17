c Analyzer 2 : encpla
      SUBROUTINE Encpla(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata)
c---------------------------------------------------------------------------
c ANALYZER 2 : Plastic & RF
c---------------------------------------------------------------------------
c
c Definitions of ID should be same as those in src/conf/plastic.conf.
c
cID =  1: F2pl
cID =  2: F3pl
cID =  3: F5pl-1
cID =  4: F5pl-2
cID =  5: F7pl
cID =  6: F8pl
cID =  7: F11pl-1
cID =  8: F11pl-2
c
c W#: 1      2      3      4      5      6      7      8      9      10
c     ID     TLraw  TRraw  QLraw  QRraw  <Qraw>
c
c     11     12     13     14     15     16     17     18     19     20
c            TL     TR     QL     QR    
c
c     21     22            23     24            25
c     <T>    dT(L-R)       <Q>    log(QR/QL)    <Q>(MeV)
c 
c     31     32
c            X from dT (mm)
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
      INCLUDE 'fldata.fh'
      INCLUDE 'plasticprm.fh'
      INCLUDE 'plasticconf.fh'
      INCLUDE 'fpdata.fh'


      INTEGER nx, ny, ndet, ndata, naok
      INTEGER rawdata(ndata,ndet)
      INTEGER nhitdata(ndet)
      INTEGER hitdet(ndet)
      INTEGER nhitdet
      REAL    val(nx,ny)

c local
      INTEGER i,j,id,itemp
      REAL    ovf
      PARAMETER (ovf=4095.)
      LOGICAL tflag1, tflag2
      LOGICAL Gate1d
      EXTERNAL Gate1d

      integer evtn
      save evtn

      IF (initencflag(2)) THEN
          CALL Loadplaconf
          DO i = 1, npla, 1
             flpla(INT(tkind(2,i)),INT(tkind(1,i))) = tkind(3,i)
          ENDDO
          CALL Loadplaprm
         initencflag(2) = .FALSE.
      ENDIF

      naok = 0

      evtn = evtn + 1

      DO j = 1, nHitDet
         id = hitDet(j)

         IF (nHitData(id).LT.nData) CYCLE 

         naok = naok + 1
         tflag1 = .FALSE.
         tflag2 = .FALSE.

         val(1,naok) = id
         DO i = 1,4
            val(i+1,naok)  = rawdata(i,id)
         ENDDO
         IF (Gate1d(val(2,naok),0.0, ovf, 0)) THEN
            val(12,naok) = (val(2,naok)+rand()-0.5)*plch2ns(1,id)
            tflag1 = .TRUE.
         ELSE
            val(12,naok) = -1000.
            tflag1 = .FALSE.
         ENDIF
         IF (Gate1d(val(3,naok),0.0, ovf, 0)) THEN
            val(13,naok) = (val(3,naok)+rand()-0.5)*plch2ns(2,id)
            tflag2 = .TRUE.
         ELSE
            val(13,naok) = -1000.
            tflag2 = .FALSE.
         ENDIF

         if(id.ne.6) then
            if(val(4,naok).eq.0) then
               tflag1 = .FALSE.
            end if
            if(val(5,naok).eq.0) then
               tflag2 = .FALSE.
            end if
         endif


c         val(14,naok) = (val(4,naok)+rand()-0.5)-plped(1,id)
c         val(15,naok) = (val(5,naok)+rand()-0.5)-plped(2,id)
         val(14,naok) = val(4,naok)
         val(15,naok) = val(5,naok)

         IF (val(4,naok).GT.0. .AND. val(5,naok).GT.0.) THEN
            val(6,naok) = sqrt(val(4,naok)*val(5,naok))
         ELSE
            val(6,naok) = -1000.
         ENDIF
         
         IF (tflag1 .AND. tflag2) THEN
            val(21,naok) = (val(12,naok)+val(13,naok))*0.5
            val(22,naok) = val(12,naok)-val(13,naok)

            val(32,naok) = pldt2x(1,id) + pldt2x(2,id) * val(22,naok)
     &           + pldt2x(3,id) * val(22,naok)**2
     &           + pldt2x(4,id) * val(22,naok)**3

            lfpdata(0,2,INT(tkind(1,id))) = .TRUE.
            fpdata(INT(tkind(2,id)),2,INT(tkind(1,id))) = val(21,naok)
            lfpdata(INT(tkind(2,id)),2,INT(tkind(1,id))) = .TRUE.
            itemp = INT(tkind(2,id))+4
            IF (itemp.LE.6) THEN
               fpdata(itemp,1,INT(tkind(1,id))) = val(32,naok)
               lfpdata(itemp,1,INT(tkind(1,id))) = .TRUE.
c               WRITE(*,*)id,itemp,INT(tkind(1,id)),INT(tkind(2,id)),
c     &              lfpdata(itemp,1,INT(tkind(1,id)))
            ELSE
               fpdata(itemp,1,INT(tkind(1,id))) = -1000.
               lfpdata(itemp,1,INT(tkind(1,id))) = .FALSE.
            ENDIF
         ELSE
            val(21,naok) = -1000.
            val(22,naok) = -1000.
            val(32,naok) = -1000.
            lfpdata(0,2,INT(tkind(1,id))) = .FALSE.
            fpdata(INT(tkind(2,id)),2,INT(tkind(1,id))) = -1000.
            lfpdata(INT(tkind(2,id)),2,INT(tkind(1,id))) = .FALSE.
            itemp = INT(tkind(2,id))+4
            IF (itemp.LE.6) THEN
               fpdata(itemp,1,INT(tkind(1,id))) = -1000.
               lfpdata(itemp,1,INT(tkind(1,id))) = .FALSE.
            ENDIF
         ENDIF

         IF (val(14,naok).GT.0. .AND. val(15,naok).GT.0. 
     &        .AND. tflag1 .AND. tflag2) THEN
            val(23,naok) = sqrt(val(14,naok)*val(15,naok))
            val(24,naok) = log(val(14,naok)/val(15,naok))
            val(25,naok) = plprm(1,id)*val(23,naok)**2 ! ch to MeV
     &           + plprm(2,id)*val(23,naok) 
     &           + plprm(3,id)
            fpdata(INT(tkind(2,id))+2,3,INT(tkind(1,id))) = val(25,naok)
            lfpdata(INT(tkind(2,id))+2,3,INT(tkind(1,id))) = .TRUE.
         ELSE
            val(23,naok) = -1000.
            val(24,naok) = -1000.
            val(25,naok) = -1000.
            fpdata(INT(tkind(2,id))+2,3,INT(tkind(1,id))) = -1000.
            lfpdata(INT(tkind(2,id))+2,3,INT(tkind(1,id))) = .FALSE.
         ENDIF


      ENDDO


      RETURN
      END

