c Analyzer 6 : Enctotale
      SUBROUTINE Enctotale(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata)
c---------------------------------------------------------------------------
c ANALYZER 6 : TotalE
c---------------------------------------------------------------------------
cID = 1: F11-E (LaBr3)
c
c   W#:  1      2     3
c       ID   Araw  Traw
c
c   W#:        12    13
c            Acal  Tcal
c

      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'commonprm.fh'
      INCLUDE 'numbers.fh'
      INCLUDE 'fldata.fh'
      INCLUDE 'totale.fh'
      INCLUDE 'fpdata.fh'

      INTEGER nx, ny, ndet, ndata, naok
      INTEGER rawdata(ndata,ndet)
      INTEGER nhitdata(ndet)
      INTEGER hitdet(ndet)
      INTEGER nhitdet
      REAL    val(nx,ny)

c local
      INTEGER i,id

      IF (initencflag(6)) THEN
         CALL Loadtotaleprm
         DO i = 1, ntotale, 1
            flpla(6,tefcs(i)) = tepos(i)
         ENDDO
         initencflag(6) = .FALSE.
      ENDIF

      naok = 0

      DO i = 1, nhitdet
         id = hitdet(i)
         naok = naok + 1
         val(1,naok) = id
         val(2,naok) = rawdata(1,id)
         val(3,naok) = rawdata(2,id)
         val(12,naok) = ch2mev(1,id) + ch2mev(2,id) * 
     &        (rawdata(1,id)+rand()-0.5)
         val(13,naok) = ch2ns(id) * 
     &        (rawdata(2,id)+rand()-0.5)

         IF (val(12,naok).GT.0.) THEN
            fpdata(teknd(id),4,tefcs(id)) = val(12,naok)
            lfpdata(teknd(id),4,tefcs(id)) = .TRUE.
         ELSE
            fpdata(teknd(id),4,tefcs(id)) = -1000.
            lfpdata(teknd(id),4,tefcs(id)) = .FALSE.
         ENDIF
         IF (val(13,naok).GT.0.) THEN
            fpdata(6,2,tefcs(id)) = val(13,naok)
            lfpdata(6,2,tefcs(id)) = .TRUE.
         ELSE
            fpdata(6,2,tefcs(id)) = -1000.
            lfpdata(6,2,tefcs(id)) = .FALSE.
         ENDIF
         IF (lfpdata(teknd(id),4,tefcs(id)) .AND.
     &        lfpdata(6,2,tefcs(id))) THEN
            lfpdata(0,4,tefcs(id)) = .TRUE.
         ELSE
            lfpdata(0,4,tefcs(id)) = .FALSE.
         ENDIF
      ENDDO


      RETURN

      END
