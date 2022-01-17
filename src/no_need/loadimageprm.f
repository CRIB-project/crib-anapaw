      SUBROUTINE Loadimageprm
      IMPLICIT NONE
      INCLUDE 'numbers.fh'
      INCLUDE 'image.fh'
      INTEGER ier
      CHARACTER*132 prmfile
      
      CALL Getenv('IMAGE_PRM',prmfile)
      OPEN (UNIT=80, FILE=prmfile, STATUS='old',ERR=1001)

c Zpos for tracking images
      CALL Read_flt_list(80,trackzpos,2*nlayer,ier)
      IF (ier.NE.0) GOTO 1002

      WRITE(*,*)' ANAPAW-M : [Loadimageprm] Loaded parameters.'
      CLOSE(80)
      RETURN

 1001 WRITE(*,'(2A)')' ANAPAW-E : [Loadimageprm] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      STOP
      RETURN

 1002 WRITE(*,*)
     &     ' ANAPAW-E : [Loadimageprm] Error while reading parameters.'
      CLOSE(80)
      STOP
      RETURN

      END


