      SUBROUTINE Loadmatrixprm
      IMPLICIT NONE
      INCLUDE 'numbers.fh'
      INCLUDE 'matrixprm.fh'
      INTEGER ier, j
      REAL    rtmp(4,maxmat)
      CHARACTER*132 prmfile,brhofile
      
      CALL Getenv('MATRIX_PRM',prmfile)
      OPEN (UNIT=80, FILE=prmfile, STATUS='old',ERR=1001)

c Brho by NMR
c      CALL Read_flt(80,brhonmr,4,ier)
c      IF (ier.NE.0) GOTO 1002

c Brho by NMR
      CALL Skip_comment(80)
      READ(80,*,ERR=1000)brhofile
      CALL Read_brho(brhofile,ier)
      IF (ier.NE.0) GOTO 1002

c # of dispersive focus
      CALL Read_int(80,ndfcs,1,ier)
      IF (ier.NE.0) GOTO 1002

c Focus and dispersion
      CALL Read_flt(80,rtmp,4*ndfcs,ier)
      IF (ier.NE.0) GOTO 1002
c      write(*,*)'ndfcs : ',ndfcs
      DO j = 1, ndfcs
c         write(*,*)j,rtmp(1,j),rtmp(2,j),rtmp(3,j),rtmp(4,j)
         iddisfcs(j)   = INT(rtmp(1,j))
         dispersion(j) = REAL(rtmp(2,j))
         IF (iddisfcs(j).LE.5) THEN
            dfcsbrho(j) = 1
         ELSEIF (iddisfcs(j).GT.5 .AND. iddisfcs(j).LE.7) THEN
            dfcsbrho(j) = 2
         ELSEIF (iddisfcs(j).GT.7 .AND. iddisfcs(j).LE.9) THEN
            dfcsbrho(j) = 3
         ELSEIF (iddisfcs(j).GT.9 .AND. iddisfcs(j).LE.11) THEN
            dfcsbrho(j) = 4
         ENDIF

         IF (rtmp(3,j).EQ.1) THEN
            fuseppac(iddisfcs(j)) = .TRUE.
         ELSE
            fuseppac(iddisfcs(j)) = .FALSE.
         ENDIF
c         WRITE(*,*)'*',j,iddisfcs(j),rtmp(3,j),fuseppac(iddisfcs(j))
         iorder(j) = INT(rtmp(4,j))
      ENDDO

c # of matrices to be calculated
      CALL Read_int(80,nmtrx,1,ier)
      IF (ier.NE.0) GOTO 1002

c Matrix ID's
      CALL Read_int(80,idmtrx,nmtrx,ier)
      IF (ier.NE.0) GOTO 1002

c Flag for high order calculation
      CALL Read_int(80,iflag_highorder,1,ier)
      IF (ier.NE.0) GOTO 1002

      WRITE(*,*)' ANAPAW-M : [Loadmatrixprm] Loaded parameters.'
      CLOSE(80)
      RETURN

 1001 WRITE(*,'(2A)')' ANAPAW-E : [Loadmatrixprm] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      STOP
      RETURN

 1000 WRITE(*,*)
     &     ' ANAPAW-E : [Loadmatrixprm] Error while reading brho.'
      STOP
      RETURN

 1002 WRITE(*,*)
     &     ' ANAPAW-E : [Loadmatrixprm] Error while reading parameters.'
      CLOSE(80)
      STOP
      RETURN

      END

c--------------------------------------------------------------------
      SUBROUTINE Read_brho(prmfile,ier)
      IMPLICIT NONE
      INTEGER ier,i,j
      INTEGER frun,trun
      REAL    b1,b2,b3,b4
      CHARACTER*132 prmfile

      INCLUDE 'runstat.fh'
      INCLUDE 'brho.fh'

      ier = 0
      i = 0
      
      OPEN (UNIT=90, FILE=prmfile, STATUS='old',ERR=1001)

      DO WHILE (ier.EQ.0)
         CALL Skip_comment(90)
         READ(90,*,END=1000),frun,trun,b1,b2,b3,b4
         DO j = frun, trun, 1
            brholist(1,j) = b1
            brholist(2,j) = b2
            brholist(3,j) = b3
            brholist(4,j) = b4
         ENDDO
         i = i + 1
      ENDDO

 1000 WRITE(*,*)' ANAPAW-M : [Loadmatrixprm] Loaded Brho values.'
      numbrho = i
      CLOSE(90)
      RETURN

 1001 WRITE(*,'(2A)')' ANAPAW-E : [Loadmatrixconf] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      STOP
      RETURN

 1002 WRITE(*,*)
     &   ' ANAPAW-E : [Loadmatrixconf] Error while reading parameters.'
      CLOSE(90)
      STOP
      RETURN

      END



