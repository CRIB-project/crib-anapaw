      SUBROUTINE Encdali(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata)
c-----------------------------------------------------------------------
c ANALYZER 12 : Encdali
c-----------------------------------------------------------------------
c  ID=1,186 
c
c   W#: 1  2    3     4         5     6     7              8
c       ID Araw Traw  Tcal(ns)  Acal  Adop  Tcal w/offset  Adop(beta_ave)
c       
c      11
c       LayerID
c
c  ID=200 Multiplicity (defined by 0 =< Araw =< 3840)
c   W#: 1  2      3
c       ID Multi  TMulti
c
c       11       12        13
c       Beta in  Beta ave  Beta out
c
c  ID=301-486   : TRUE events
c
c   W#  1      2      3      4      5      
c       ID     Araw   Traw   Acal   Tcal
c
c       6      7      8
c       Adop   Adop2  Theta
c
c       11     12
c       DetID  LayerID
c
c   W# : 21              22
c      Adop(-Gamma1)   Adop(Gamma1)
c
c   W# : 23              24
c      Adop(-Gamma2)   Adop(Gamma2)
c
c   W# : 25              26
c      Adop(-Gamma3)   Adop(Gamma3)
c
c   W# : 27              28
c      Adop(-Gamma4)   Adop(Gamma4)
c
c  ID=601-786   : Sorted events from highest to lowest
c               ID=601 is always highest energy.
c
c   W#  1      2      3      4   
c       ID     Adop   Acal   Tcal
c
c       11     12
c       DetID  LayerID
c
c-----------------------------------------------------------------------

c
c
c
c
c     nDataMin                  : the number of required data (ihit_min0)
c     hitDet(1:nHitDet)         : hit detector id
c     nData(1:lenDet)           : the number of data for each id (ihit)
c     nHitDet                   : the number of hit detector
c     lenData                   : the length of array
c     lenDet                    : the length of array
      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'daliprm.fh'
      INCLUDE 'dalidata.fh' ! for tree
c
      INCLUDE 'numbers.fh' 
      INCLUDE 'pid.fh'      ! need ENCPID
      INCLUDE 'fpdata.fh'   ! need PPAC,PLASTIC,MATRIX,IMAGE, at least PLATIC
      INCLUDE 'fortree.fh'
c

      INTEGER nx, ny, ndet, ndata, naok
      INTEGER rawdata(ndata,ndet)
      INTEGER nhitdata(ndet)
      INTEGER hitdet(ndet)
      INTEGER nhitdet
      REAL    val(nx,ny)

      CHARACTER ihitchara*4
      INTEGER ihit_min0

      INTEGER ihit, id, i,j,k
      INTEGER mult, tmult

      REAL    beta_in,beta_out,beta_ave,gamma_ave
      REAL    tval(50,200)
      REAL    svalin(200),svalout(200)
      INTEGER svinr(200),svin(200),svalid

      REAL    Cosd
      EXTERNAL Cosd

c
c For gamma-gamma
c
      Integer*8 siggbit(4),sggibase,sggisub,siggflag(4)
      Integer   sgginaok,sggfnaok,sggM(4)
      Logical   sggflag(4),ggflag
      Real      sggAsum(4)

c
c For debug
c
      LOGICAL   fdebug/.FALSE./
c

      IF (InitENCflag(12)) THEN
         CALL LOADDALIPRM
         CALL GETENV('IHIT_MIN0',ihitchara)
         READ(ihitchara,*) ihit_min0
         IF (ihit_min0.EQ.1) THEN
            WRITE(*,*) ' ANAPAW-M : [Encdali] IHIT_MIN0 = ', ihit_min0
         ELSE
            ihit_min0 = 2
            WRITE(*,*) ' ANAPAW-M : [Encdali] IHIT_MIN0 -> ', ihit_min0
         ENDIF
         InitENCFlag(12) = .FALSE.
         IF (fdebug) THEN
            DO i=1,186,1
               write(*,*)i,ped(i)+gain(i)*3840.
               write(*,*)i,(100.-ped(i))/gain(i),
     &              int((100.-ped(i))/gain(i)/2.)
               write(*,*)i, int((100.-ped(i))/gain(i)/2.)
            ENDDO
         ENDIF
         DO i = 1, 50, 1
            DO j = 1, 200, 1
               tval(i,j) = -1000.
            ENDDO
         ENDDO
         WRITE(*,*)
     & ' ANAPAW-M : [Encdali] Rejected Detectors : 66,124,126,131,133'
      ENDIF

      mult = 0
      tmult = 0
      naok = 0
      ggflag = .FALSE.

c initialization of veriables for tree
      dalimult = 0

      do i=1,nch_dali
         daliid(i) = 0
         do j=1,2
            daliinfo(i,j) = -1000.
         enddo
         svalin(i) = -1000.
         svalout(i) = -1000.
         svinr(i) = 0
         svin(i) = 0
      enddo




      IF( .NOT.(l_aoq(2,2).AND.l_aoq(2,3)) ) GOTO 999
      beta_in  = aoqdata(2,2)
      beta_out = aoqdata(2,3)
      beta_ave = 0.5*(beta_in+beta_out)
c      beta_ave = 0.5*(beta_in+beta_out)*1.04
c      beta_ave = sqrt(beta_in*beta_out)
c      beta_ave = beta_in
c      beta_ave = beta_out

      gamma_ave = 1./sqrt(1.-beta_ave**2)

      DO ihit = 1,nhitdet
         id = hitdet(ihit)
         IF (id.GT.nch_dali) CYCLE
         IF (id.EQ.66 .OR. id.EQ.124 .OR. id.EQ.126
     &        .OR. id.EQ.131 .OR. id.EQ.133) CYCLE
         IF (rawdata(1,id).GT.3840.) CYCLE
c         IF (id2layer(id).LT. 2 .OR. 
c     &        id2layer(id).GE.12) CYCLE

         naok = naok + 1
         val(1,naok) = id
         val(2,naok) = rawdata(1,id) ! Araw
         val(3,naok) = rawdata(2,id) ! Traw
         val(4,naok) = rawdata(2,id) * ch2ns(id) +1000. ! Tcal
         val(5,naok) = ped(id) + rawdata(1,id) * gain(id) ! Acal

         val(6,naok) = val(5,naok) * gamma *(1.-beta*Cosd(theta(id)))! Adop

         val(8,naok) = val(5,naok) * gamma_ave 
     &        *(1.-beta_ave*Cosd(theta(id))) ! Adop

         val(9,naok) = val(4,naok) - fpdata(1,2,7)
         val(10,naok) = val(9,naok) 
     &        - tof(3,7,1)/fl(3,7,1)*fl(7,8,1) + 100.
         val(7,naok) = val(9,naok) 
     &        - tof(3,7,1)/fl(3,7,1)*fl(7,8,1) + 100. - toff(id)
         
         val(11,naok) = id2layer(id)

         IF (val(2,naok).GT.0. .AND. val(3,naok).GT.-12000. .AND.
     & val(3,naok).LT.1500.) THEN
            mult = mult + 1
         ENDIF

         val(15,naok) = rawdata(3,id) ! Multihit

         IF (val(7,naok).GT.truemin .AND. val(7,naok).LE.truemax .AND. 
     &       val(5,naok).GT.10. .AND. rawdata(3,id).EQ.1) THEN
            tmult = tmult + 1
            tval(1,tmult) = id
            tval(2,tmult) = val(2,naok)
            tval(3,tmult) = val(3,naok)
            tval(4,tmult) = val(5,naok)
            tval(5,tmult) = val(7,naok)
            tval(6,tmult) = val(6,naok)
            tval(7,tmult) = val(8,naok)
c            WRITE(*,*)'0',id,tval(7,tmult),tval(4,tmult),tmult
         ENDIF
         daliinfo(ihit,1) = val(5,naok)
         daliinfo(ihit,2) = val(7,naok)
         daliid(ihit) = id
      ENDDO

      dalimult = mult

c True events

      sgginaok = naok + 1
      DO i = 1, 4
         siggbit(i) = 0
         sggflag(i) = .FALSE.
      ENDDO


      IF(fdebug .AND. tmult.GT.0)THEN 
         WRITE(*,*)' -------------------- '
         WRITE(*,*)' Multi : ',tmult
      ENDIF

      DO i = 1, tmult, 1
         naok = naok + 1

         val(1,naok) = i+300
         val(2,naok) = tval(2,i)
         val(3,naok) = tval(3,i)
         val(4,naok) = tval(4,i)
         val(5,naok) = tval(5,i)
         val(6,naok) = tval(6,i)
         val(7,naok) = tval(7,i) ! Adop
         val(8,naok) = theta(int(tval(1,i)))
         val(11,naok) = tval(1,i)
         val(12,naok) = id2layer(int(tval(1,i)))

         svalin(i) = tval(7,i)

          DO k = 1, 4
             IF ( val(7,naok).GE.ggmin(k) .AND.
     &            val(7,naok).LT.ggmax(k) ) THEN
                siggbit(k) = siggbit(k) + 2**(i-1)
                sggflag(k) = .TRUE.
                IF (fdebug) THEN
                   WRITE(*,*)'---'
                   WRITE(*,*)'i : ',i,' k : ',k, ' Min : ',
     &                  ggmin(k),' Max : ',ggmax(k)
                   WRITE(*,*)' data : ',val(7,naok)
                   WRITE(*,'(A8,1Z16)')' bit : ',siggbit(k)
                ENDIF
                ggflag = .TRUE.
             ENDIF
          ENDDO
      ENDDO
      
      IF(fdebug .AND. tmult.GT.0)THEN
         IF(sggflag(1))WRITE(*,'(A10,1Z16)')' flag1 : ',siggbit(1)
         IF(sggflag(2))WRITE(*,'(A10,1Z16)')' flag2 : ',siggbit(2)
         IF(sggflag(3))WRITE(*,'(A10,1Z16)')' flag3 : ',siggbit(3)
         IF(sggflag(4))WRITE(*,'(A10,1Z16)')' flag4 : ',siggbit(4)
      ENDIF

      sggfnaok = naok

      DO i = 1, 4
         sggM(i) = 0
         sggAsum(i) = 0.
      ENDDO

      j = 0
      sggibase = x'ffffffffffffffff'
      IF (ggflag) THEN
         DO i = sgginaok, sggfnaok, 1
            j = j + 1
            sggisub = sggibase - 2**(j-1)
            IF (fdebug) THEN
               WRITE(*,*)' '
               WRITE(*,*)'gg i : ',j
               WRITE(*,'(A12,1Z16)')' sggisub : ',sggisub
            ENDIF
            DO k = 1, 4
               If(sggflag(k))Then
                  siggflag(k) = iand(sggisub,siggbit(k))
                  IF (fdebug) THEN
                     WRITE(*,*)'---- k : ',k
                     WRITE(*,'(A12,1Z16)')' siggbit : ',siggbit(k)
                     WRITE(*,*)' siggflag : ',siggflag(k)
                  ENDIF
                  sggAsum(k) = sggAsum(k) + val(7,i)
                  If(siggflag(k).GE.1)Then
                     sggM(k) = sggM(k) + 1
                     val(21+(k-1)*2,i) = val(7,i)
                     val(22+(k-1)*2,i) = -1000.
                     IF (fdebug) write(*,*)' ggdata 21: ',val(7,i)
                  Else
                     val(21+(k-1)*2,i) = -1000.
                     val(22+(k-1)*2,i) = val(7,i)
                     IF (fdebug) write(*,*)' ggdata 22: ',val(7,i)
                  EndIf
               Else
                  val(21+(k-1)*2,i) = -1000.
                  val(22+(k-1)*2,i) = -1000.
               EndIf
            EndDo
         ENDDO
      ELSE
         DO i = sgginaok, sggfnaok, 1
            DO k = 1, 4
                  val(21+(k-1)*2,i) = -1000.
                  val(22+(k-1)*2,i) = -1000.
            EndDo
         ENDDO
      ENDIF

ccc Multiplicity
      ID = 200

      naok = naok + 1
      val(1,naok) = id
      val(2,naok) = mult
      val(3,naok) = tmult
      val(11,naok) = beta_in
      val(12,naok) = beta_ave
      val(13,naok) = beta_out
      val(21,naok) = sggM(1)
      val(23,naok) = sggM(2)
      val(25,naok) = sggM(3)
      val(27,naok) = sggM(4)

c Sort in energy order.
      CALL Sorting(svalin,svalout,svin,svinr,tmult)
      svalid = 0

c      write(*,*)'--'

      DO i = 1, tmult, 1
         naok = naok + 1

         svalid = svinr(i)

         val(1,naok) = i + 600
c         val(2,naok) = svalout(i)
         val(2,naok) = tval(7,svalid)
         val(3,naok) = tval(4,svalid)
         val(4,naok) = tval(5,svalid)
         val(11,naok) = tval(1,svalid)
         val(12,naok) = id2layer(int(tval(1,svalid)))

c         WRITE(*,*)'tmult ',tmult,' i ',i,' j ',j,' svalid ',svalid
c         WRITE(*,*)'a',tval(1,svalid),svalout(i),tval(4,svalid)
c         WRITE(*,*)'a',tval(1,svalid),tval(7,svalid),tval(4,svalid)
c         WRITE(*,*)'b',tval(1,i),svalin(i),svalout(svin(i)),tval(4,i)
c         WRITE(*,*)'c', id2layer(int(tval(1,svalid)))
      ENDDO

      DO i = 1, tmult, 1
         DO k = 1, 12, 1
            tval(k,i) = -1000.
         ENDDO
      ENDDO

 999  CONTINUE

      RETURN
      END

      
c ======================================

      REAL FUNCTION Cosd(x)

      REAL x,pi

      pi = ACOS(-1.0)
      Cosd = COS(x/180.*pi)

      RETURN

      END
