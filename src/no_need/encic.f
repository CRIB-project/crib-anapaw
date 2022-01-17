c Analyzer 5 : Encic
      SUBROUTINE Encic(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata)
c---------------------------------------------------------------------------
c ANALYZER 5 : IC
c---------------------------------------------------------------------------
cID = 1: F2IC
cID = 2: F3IC
cID = 3: F7IC
cID = 4: F11IC
c   W#:  1   2   3   4   5   6   7   8   9  10
c       ID ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8(7,8 for aux inp.)
c
c   W#: 11  12       13
c           Sum.Ave. Sqt.Ave.
c
c   W#: 21  22       23
c           Sum.Ave. Sqt.Ave.   : [MeV]
c 
c
      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'commonprm.fh'
      INCLUDE 'numbers.fh'
      INCLUDE 'ic.fh'
c
      INCLUDE 'fpdata.fh'
c
      INTEGER nx, ny, ndet, ndata, naok
      INTEGER rawdata(ndata,ndet)
      INTEGER nhitdata(ndet)
      INTEGER hitdet(ndet)
      INTEGER nhitdet
      REAL    val(nx,ny)

c local
      INTEGER i,id,j
      REAL    avsum(nic),sqsum(nic)
c      REAL    temp,tempsum

      IF (initencflag(5)) THEN
         CALL Loadicprm
         initencflag(5) = .FALSE.
      ENDIF

      naok = 0

      DO i = 1, nhitdet
         id = hitdet(i)
         naok = naok + 1
         sqsum(id) = 1.
         avsum(id) = 0.

         val(1,naok) = id

         DO j = 1, nicch
            val(j+1,naok) = rawdata(j,id)
c            write(*,*)'rawdata(j,id)=',rawdata(j,id)
cccccccccccccccccccccccccccccccccccccccccccc  Matsushita test
c            IF( id.eq.4 ) then
c               write(*,*)'F11IC ch',j,' = ',rawdata(j,id)
c            endif
cccccccccccccccccccccccccccccccccccccccccccc  test
            IF (val(j+1,naok).GT.500.) THEN
               avsum(id) = avsum(id)+(val(j+1,naok)+rand()-0.5)
               sqsum(id) = sqsum(id)*(val(j+1,naok)+rand()-0.5)
c               write(*,*)'sqsum(id)=',aqsum(id)
            ELSE
               avsum(id) = 0.
c              avsum(id) = -1000.
c              To calculate Sqrt Average, cannot take minus value
               sqsum(id) = 1.
            ENDIF
         ENDDO
cc
cc check reaction loss
c         DO j = 1, nicch
c            temp = 0.
c            tempsum = 0.
c            IF (sqsum(id).GT.0.) THEN
c               temp = val(j+1,naok)-sqsum(id)**(1./nicch)
c               tempsum = tempsum + temp**2
c            ELSE
c               tempsum = -1000.
c            ENDIF
c         ENDDO
c         IF (tempsum.GT.0.) THEN
c            val(30,naok) = sqrt(tempsum)
c         ELSE
c            val(30,naok) = -1000.
c         ENDIF
cc
cc
         IF (avsum(id).GT.0.) THEN
            avsum(id) = avsum(id)/nicch
            val(12,naok) = avsum(id)
            val(22,naok) = ch2mev(2,id) + avsum(id)*ch2mev(3,id)
            fpdata(1,3,INT(ch2mev(1,id)))  = val(22,naok)
            lfpdata(1,3,INT(ch2mev(1,id))) = .TRUE.
         ELSE
            val(12,naok) = -1000.
            val(22,naok) = -1000.
            fpdata(1,3,INT(ch2mev(1,id)))  = val(22,naok)
            lfpdata(1,3,INT(ch2mev(1,id))) = .FALSE.
         ENDIF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc Matsushita test
c         if( id.eq.4) then
c            WRITE(*,*)'F11IC.Sum.Ave = ',avsum(id)
c         endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc test end

         IF (sqsum(id).GT.0.) THEN
            sqsum(id) = sqsum(id)**(1./nicch)
            val(13,naok) = sqsum(id)
            val(23,naok) = ch2mev(2,id) + sqsum(id)*ch2mev(3,id)
c            WRITE(*,*)'IC : ',i,id,INT(ch2mev(1,id)),val(23,naok)
         ELSE
            val(13,naok) = -1000.
            val(23,naok) = -1000.
         ENDIF         
      ENDDO


      RETURN

      END
