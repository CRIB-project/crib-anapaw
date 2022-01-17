      SUBROUTINE Loadicprm
      IMPLICIT NONE
      INCLUDE 'numbers.fh'
      INCLUDE 'ic.fh'
      INTEGER ier
      CHARACTER*132 prmfile

      CALL GETENV('IC_PRM',prmfile)
      OPEN(UNIT=80, FILE=prmfile, STATUS='old', ERR=1001)

      CALL Read_flt(80,ch2mev,3*nic,ier)
      IF (ier.NE.0) GOTO 1002

      WRITE(*,*)' ANAPAW-M : [Loadicprm] Loaded parameters.'
      CLOSE(80)
      RETURN

 1001 WRITE(*,'(2A)') ' ANAPAW-E : [Loadicprm] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      STOP
      RETURN

 1002 WRITE(*,*) 
     &     ' ANAPAW-E : [Loadicprm] Error while reading parameters.'
      CLOSE(80)
      STOP
      RETURN

      END


