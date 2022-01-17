      SUBROUTINE Loadppacprm
      IMPLICIT NONE

      INCLUDE 'numbers.fh'
      INCLUDE 'fldata.fh'
      INCLUDE 'ppacprm.fh'
      INTEGER i, ier, tmp
      REAL    tmp2(2,nfocus)
      CHARACTER*132 prmfile
      
      CALL Getenv('PPAC_PRM',prmfile)
      OPEN (UNIT=80, FILE=prmfile, STATUS='old',ERR=1001)

c for rawdata
      CALL Read_flt_list(80,ch2ns,5*nppac,ier)
      IF (ier.NE.0) GOTO 1002
      CALL Read_flt_list(80,qped,5*nppac,ier)
      IF (ier.NE.0) GOTO 1002

c Flag for inside parameters (Kumagai-san's parameters)
      CALL Read_int(80,iflag_inside_offset,1,ier)
      IF (ier.NE.0) GOTO 1002

c Distance between focus and f0 and Zpos shift of focus point
c      CALL Read_flt_list(80,dzpos,nfocus,ier)
c      IF (ier.NE.0) GOTO 1002
      CALL Read_flt_list(80,tmp2,2*nfocus,ier)
      IF (ier.NE.0) GOTO 1002
      flfocus(0) = 0.
      DO i = 1, nfocus, 1
         flfocus(i) = tmp2(1,i)
         dzpos(i)   = tmp2(2,i)
      ENDDO

c TXsum gate
      CALL Read_int(80,tmp,1,ier)
      IF (ier.NE.0) GOTO 1002
      IF (tmp.NE.0) THEN
         iflag_tsumgate(ix) = .TRUE.
      ELSE
         iflag_tsumgate(ix) = .FALSE.
      ENDIF
      CALL Read_flt_list(80,tsumgate(1,1,ix),2*nppac,ier)
      IF (ier.NE.0) GOTO 1002

c TYsum gate
      CALL Read_int(80,tmp,1,ier)
      IF (ier.NE.0) GOTO 1002
      IF (tmp.NE.0) THEN
         iflag_tsumgate(iy) = .TRUE.
      ELSE
         iflag_tsumgate(iy) = .FALSE.
      ENDIF
      CALL Read_flt_list(80,tsumgate(1,1,iy),2*nppac,ier)
      IF (ier.NE.0) GOTO 1002

c Line calibration
      CALL Read_int(80,iflag_outside_offset,1,ier)
      IF (ier.NE.0) GOTO 1002
      CALL Read_flt_list(80,outoffset,2*nppac,ier)
      IF (ier.NE.0) GOTO 1002

c Geometry offset
      CALL Read_int(80,iflag_geom_offset,1,ier)
      IF (ier.NE.0) GOTO 1002
      CALL Read_flt_list(80,geomoffset,2*nppac,ier)
      IF (ier.NE.0) GOTO 1002

      WRITE(*,*)' ANAPAW-M : [Loadppacprm] Loaded parameters.'
      CLOSE(80)
      RETURN

 1001 WRITE(*,'(2A)')' ANAPAW-E : [Loadppacprm] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      STOP
      RETURN

 1002 WRITE(*,*)
     &     ' ANAPAW-E : [Loadppacprm] Error while reading parameters.'
      CLOSE(80)
      STOP
      RETURN

      END


