      SUBROUTINE Loadplaconf
      IMPLICIT NONE
      INCLUDE 'numbers.fh'
      INCLUDE 'fldata.fh'
      INCLUDE 'plasticconf.fh'
      INTEGER i,ier
      REAL    tmp(2,nfocus)
      CHARACTER*132 prmfile
      
      CALL Getenv('PLASTIC_CONFIG',prmfile)
      OPEN (UNIT=80, FILE=prmfile, STATUS='old',ERR=1001)

c load configuration
      CALL Read_flt(80,tkind,3*npla,ier)
      IF (ier.NE.0) GOTO 1002

c load distance between focus and f0
      CALL Read_flt_list(80,tmp,2*nfocus,ier)
      IF (ier.NE.0) GOTO 1002
      flfocus(0) = 0.
      DO i = 1, nfocus, 1
         flfocus(i) = tmp(1,i)
      ENDDO

      WRITE(*,*)' ANAPAW-M : [Loadplaconf] Loaded parameters.'
      CLOSE(80)
      RETURN

 1001 WRITE(*,'(2A)')' ANAPAW-E : [Loadplaconf] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      STOP
      RETURN

 1002 WRITE(*,*)
     &     ' ANAPAW-E : [Loadplaconf] Error while reading parameters.'
      CLOSE(80)
      STOP
      RETURN

      END


