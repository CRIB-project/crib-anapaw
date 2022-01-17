      SUBROUTINE Loadpidprm
      IMPLICIT NONE
      INCLUDE 'numbers.fh'
      INCLUDE 'pid.fh'
      INCLUDE 'pidprm.fh'
      INTEGER ier,i,j
      CHARACTER*132 prmfile,confname

      CALL GETENV('PID_PRM',prmfile)
      OPEN(UNIT=80, FILE=prmfile, STATUS='old', ERR=1001)

      CALL Read_flag(80,lfltof,1,ier)
      IF (ier.NE.0) GOTO 1002

      CALL Read_flag(80,lflcorrect,1,ier)
      IF (ier.NE.0) GOTO 1002

      i = 1
      ier = 0
      CALL Skip_comment(80)
      READ(80,*,END=1000)confname
      OPEN(UNIT=90, FILE=confname, STATUS='old', ERR=1001)
        
      DO WHILE (ier.EQ.0)
         CALL Skip_comment(90)
         READ(90,*,END=998) (tfk(j,i),j=1,5)
         ntof = i
         i = i + 1
      ENDDO
 998  CLOSE(90)

      i = 1
      CALL Skip_comment(80)
      READ(80,*,END=1000)confname
      OPEN(UNIT=90, FILE=confname, STATUS='old', ERR=1001)
      DO WHILE (ier.EQ.0)
         CALL Skip_comment(90)
         READ(90,*,END=999) dek(1,i), dek(2,i), 
     &        zcoeff(1,dek(2,i),dek(1,i)),
     &        zcoeff(2,dek(2,i),dek(1,i)),
     &        ionpair(dek(2,i),dek(1,i))
         nde = i
         i = i + 1
      ENDDO
 999  CLOSE(90)

      i = 1
      CALL Skip_comment(80)
      READ(80,*,END=1000)confname
      OPEN(UNIT=90, FILE=confname, STATUS='old', ERR=1001)
      DO WHILE (ier.EQ.0)
         CALL Skip_comment(90)
         READ(90,*,END=1000) idcnf(i),
     &        brcnf(1,i),brcnf(2,i),brcnf(3,i),brcnf(4,i),
     &        decnf(1,i),decnf(2,i),
     &        btcnf(1,i),btcnf(2,i),btcnf(3,i),btcnf(4,i),
     &        btpcnf(1,i),btpcnf(2,i),btpcnf(3,i),
     &        tofcnf(1,i),tofcnf(2,i),tofcnf(3,i)
         nconfig = i
         i = i + 1
      ENDDO

 1000 WRITE(*,*)' ANAPAW-M : [Loadpidprm] Loaded parameters.'
      CLOSE(90)
      CLOSE(80)
      RETURN

 1001 WRITE(*,'(2A)') ' ANAPAW-E : [Loadpidprm] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      CLOSE(90)
      CLOSE(80)
      STOP
      RETURN

 1002 WRITE(*,*) 
     &     ' ANAPAW-E : [Loadpidprm] Error while reading parameters.'
      CLOSE(90)
      CLOSE(80)
      STOP
      RETURN

      END


