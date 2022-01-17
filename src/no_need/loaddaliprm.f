c
c ----- LOADDALIPRM ------- read the parameters for DALI
c
c

      SUBROUTINE Loaddaliprm
      IMPLICIT NONE
      INCLUDE "daliprm.fh"
      REAL    tmp(2)
      INTEGER ier ! error flag
      CHARACTER*256 filedaliprm


c read parameters for dali2
      
      CALL GETENV('DALI_PRM',filedaliprm)
      OPEN(UNIT=80, FILE=filedaliprm, STATUS='old', ERR=110)  

c pedestal      
      CALL Read_flt_list(80,ped,nch_dali,ier)
      IF (ier.NE.0) GOTO 125

c gain
      CALL Read_flt_list(80,gain,nch_dali,ier)
      IF (ier.NE.0) GOTO 125

c Tcal
      CALL Read_flt_list(80,ch2ns,nch_dali,ier)
      IF (ier.NE.0) GOTO 125

c Toffset
      CALL Read_flt_list(80,toff,nch_dali,ier)
      IF (ier.NE.0) GOTO 125

c Doppler corrction
      CALL Read_flt_list(80,theta,nch_dali,ier)
      IF (ier.NE.0) GOTO 125
      CALL Read_flt(80,tmp,1,ier)
      IF (ier.NE.0) GOTO 125
      beta = tmp(1)
      gamma = 1./sqrt(1.-beta**2)
      CALL Read_flt(80,tmp,2,ier)
      IF(ier.NE.0) GOTO 125
      truemin = tmp(1)
      truemax = tmp(2)

c parameters for conversion from ID to Layer
      CALL Read_int_list(80,id2layer,nch_dali,ier)
      IF (ier.NE.0) GOTO 125

c gamma-gamma limit
      CALL Read_flt(80,tmp,2,ier)
      IF(ier.NE.0) GOTO 125
      ggmin(1) = tmp(1)
      ggmax(1) = tmp(2)
      CALL Read_flt(80,tmp,2,ier)
      IF(ier.NE.0) GOTO 125
      ggmin(2) = tmp(1)
      ggmax(2) = tmp(2)
      CALL Read_flt(80,tmp,2,ier)
      IF(ier.NE.0) GOTO 125
      ggmin(3) = tmp(1)
      ggmax(3) = tmp(2)
      CALL Read_flt(80,tmp,2,ier)
      IF(ier.NE.0) GOTO 125
      ggmin(4) = tmp(1)
      ggmax(4) = tmp(2)

      WRITE(*,*) ' ANAPAW-M : [Loaddaliprm] DALI_PRM read in.'
      WRITE(*,*) ' '
      CLOSE(80)
      GOTO 130

 110  WRITE(*,*) ' ANAPAW-E : [Loaddaliprm] DALI_PRM is not found.'
      GOTO 130
      STOP

 125  WRITE(*,*) ' '
      WRITE(*,*) ' ANAPAW-E : [Loaddaliprm] Error in DALI_PRM.'
      WRITE(*,*) ' '
      CLOSE(80)
      STOP

 130  CONTINUE

      RETURN
      END
