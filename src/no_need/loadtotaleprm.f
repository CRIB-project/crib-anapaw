      SUBROUTINE Loadtotaleprm
      IMPLICIT NONE
      INCLUDE 'numbers.fh'
      INCLUDE 'totale.fh'
      INTEGER i
      CHARACTER*132 prmfile

      CALL GETENV('TOTALE_PRM',prmfile)
      OPEN(UNIT=80, FILE=prmfile, STATUS='old', ERR=1001)

      DO i = 1, ntotale, 1
         CALL Skip_comment(80)
         READ(80,*,END=1000)tefcs(i),teknd(i),
     &        ch2mev(1,i),ch2mev(2,i),ch2ns(i),tepos(i)
      ENDDO

 1000 WRITE(*,*)' ANAPAW-M : [Loadtotaleprm] Loaded parameters.'
      CLOSE(80)
      RETURN

 1001 WRITE(*,'(2A)') ' ANAPAW-E : [Loadtotaleprm] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      STOP
      RETURN

 1002 WRITE(*,*) 
     &     ' ANAPAW-E : [Loadtotaleprm] Error while reading parameters.'
      CLOSE(80)
      STOP
      RETURN

      END


