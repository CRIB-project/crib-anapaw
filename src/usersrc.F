c =====================================================================
      SUBROUTINE CALCDATA
      IMPLICIT NONE
c
c Do not EDIT!! -->
c
      INTEGER nx/500/,ny/500/,naok(50)
      REAL    val(500,500,50)
      COMMON/ANALYSVALUE/ val,naok

#include "analyslogic.fh"
#include "analysevent.fh"
#include "rawdata.fh"
#include "commonprm.fh"
#include "runstat.fh"

c
c Do not EDIT!! <--
c

c
c EXTERNAL Statements
c
c
      EXTERNAL Enccoin          
      EXTERNAL Encrf           
      EXTERNAL Encppac          
      EXTERNAL Encdssd          
      EXTERNAL Encmssd          
      EXTERNAL Encssd          
c      EXTERNAL Encmatrix        
c      EXTERNAL Encimage         
c      EXTERNAL Encpid           
c User detectors
c      EXTERNAL Encdali
c      EXTERNAL Encgammappac
c      EXTERNAL Encgeneric
c      EXTERNAL Encpileup
c      EXTERNAL Enclabr

c      To get brho and nmr values
c      Character(132) xcat,xname,xtag
c      real xret
c      xcat =  'dipole'
c      xname = 'D4'
c      xtag = 'nmr'
c      xtag = 'brho'
c      CALL get_runstatus_real(xcat,xname,xtag,xret)
c      write(*,*) 'xret=', xret
c
c
c Analyzer
c
c      CALL Callanalyzer(9,9,Enccoin)
c      CALL Callanalyzer(2,2,Encpla)
c      CALL Callanalyzer(5,5,Encic)
c      CALL Callanalyzer(6,6,Enctotale)
      CALL Callanalyzer(1,1,Encrf)
      CALL Callanalyzer(2,2,Encppac)
      CALL Callanalyzer(3,3,Enccoin)
      CALL Callanalyzer(4,4,Encdssd)
      CALL Callanalyzer(5,5,Encmssd)
      CALL Callanalyzer(6,6,Encssd)


c      IF (Analyzerflag(10)) CALL Encmatrix(val(1,1,10),nx,ny,naok(10))
c      IF (Analyzerflag(11)) CALL Encpid(val(1,1,11),nx,ny,naok(11))
c      IF (Analyzerflag(15)) CALL Encimage(val(1,1,15),nx,ny,naok(15))

c for dali
c      CALL Callanalyzer(12,12,Encdali)
c for extrappac
c      CALL Callanalyzer(13,13,Encgammappac)
c for genericc
c      CALL Callanalyzer(20,20,Encgeneric)
c for generic
c      CALL Callanalyzer(14,14,Encpileup)
c for Labr
c      CALL Callanalyzer(16,16,Enclabr)

      RETURN
      END
c =========================================================================
      SUBROUTINE END_RUN
c                                             2010.May.15. S.Takeuchi 
c
c Do not EDIT!! -->
      Integer        nx/500/,ny/500/,naok(50)
      Real           val(500,500,50)
      Common/ANALYSVALUE/ val,naok
#include "analyslogic.fh"
#include "analysevent.fh"
#include "commonprm.fh"
#include "runstat.fh"
c Do not EDIT!! <--
c
c INITENCFLAG(i) : i = analyzer number.
c
      IF (AnalyzerFlag(30)) THEN
c         CALL write_effmon()
         WRITE(*,*)' '
         WRITE(*,*)' ANAPAW-M : [END_RUN] is called.'
         WRITE(*,*)' '
      ENDIF

      RETURN

      END

c =========================================================================
      SUBROUTINE CALLANALYZER(analyzer,catid,enc)
      IMPLICIT NONE

#include "analyslogic.fh"
#include "analysevent.fh"
#include "rawdata.fh" 

      INTEGER nx/500/,ny/500/,naok(50)
      REAL    val(500,500,50)
      COMMON/ANALYSVALUE/ val,naok

      INTEGER analyzer
      INTEGER catid

      IF (.NOT. analyzerflag(analyzer)) GOTO 1000

      IF (rawheadpos(catid).EQ.0) THEN
         WRITE(*,*) ' ANAPAW-E : invalid rawheadpos(catid =',catid,
     & ') in analyzer =',analyzer
         GOTO 1000
      ENDIF

      IF (hitheadpos(catid).EQ.0) THEN
         WRITE(*,*) ' ANAPAW-E : invalid hitheadpos(catid =',catid,
     &') in analyzer =',analyzer
         GOTO 1000
      ENDIF
c      WRITE(*,*)'ANAFLAG : ',analyzerflag(analyzer)

      IF (catid.GE.21 .AND. catid.LE.28) THEN
         CALL enc(val(1,1,analyzer),nx,ny,naok(analyzer),
     &        rawdata(rawheadpos(catid)),nhitdata(hitheadpos(catid)),
     &        hitdet(hitheadpos(catid)),
     &        nhitdet(catid),ndet(catid),ndata(catid),analyzer,catid)
      ELSE
         CALL enc(val(1,1,analyzer),nx,ny,naok(analyzer),
     &        rawdata(rawheadpos(catid)),nhitdata(hitheadpos(catid)),
     &        hitdet(hitheadpos(catid)),
     &        nhitdet(catid),ndet(catid),ndata(catid),analyzer)
      ENDIF

c      write(*,*)'Analyzer : ',analyzer
c      write(*,*)'Catid    : ',catid
c      write(*,*)'nhitdata : ',nhitdata(hitheadpos(catid))
c      write(*,*)'hitdet   : ',hitdet(hitheadpos(catid))
c      write(*,*)'nhitdet  : ',nhitdet(catid)
c      write(*,*)'ndet     : ',ndet(catid)
c      write(*,*)'ndata    : ',ndata(catid)
c      write(*,*)'----'

 1000 CONTINUE

      RETURN
      END
