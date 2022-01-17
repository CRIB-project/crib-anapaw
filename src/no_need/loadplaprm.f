      SUBROUTINE Loadplaprm
      IMPLICIT NONE
      INCLUDE 'numbers.fh'
      INCLUDE 'plasticprm.fh'
      INTEGER ier
      CHARACTER*132 prmfile

      CALL GETENV('PLA_PRM',prmfile)
      OPEN(UNIT=80, FILE=prmfile, STATUS='old', ERR=1001)

      CALL Read_flt_list(80,plch2ns,2*npla,ier)
      IF (ier.NE.0) GOTO 1002

      CALL Read_flt_list(80,plprm,3*npla,ier)
      IF (ier.NE.0) GOTO 1002

      CALL Read_flt_list(80,pldt2x,4*npla,ier)
      IF (ier.NE.0) GOTO 1002

      WRITE(*,*)' ANAPAW-M : [Loadplaprm] Loaded parameters.'
      CLOSE(80)
      RETURN

 1001 WRITE(*,'(2A)') ' ANAPAW-E : [Loadplaprm] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      STOP
      RETURN

 1002 WRITE(*,*) 
     &     ' ANAPAW-E : [Loadplaprm] Error while reading parameters.'
      CLOSE(80)
      STOP
      RETURN

      END


