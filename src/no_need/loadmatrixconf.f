      SUBROUTINE Loadmatrixconf
      IMPLICIT NONE
      INTEGER ier, i, ier1
      CHARACTER*132 prmfile,confname

      INCLUDE 'numbers.fh'
      INCLUDE 'matrixconf.fh'
      INCLUDE 'invmatrix.fh'

      REAL    a(2,2)

      ier = 0
      i   = 1

      CALL Getenv('MATRIX_CONFIG',prmfile)
      OPEN (UNIT=80, FILE=prmfile, STATUS='old',ERR=1001)

      DO WHILE (ier.EQ.0)
         CALL Skip_comment(80)
         READ(80,*,END=1000)matprm(1,i),matprm(2,i),matprm(3,i),
     &        morder(i),iflag_reverse_or_not(i),confname
         IF (matprm(3,i).LE.5) THEN
            matprm(4,i) = 1
         ELSEIF (matprm(3,i).GT.5 .AND. matprm(3,i).LE.8) THEN
            matprm(4,i) = 2
         ELSEIF (matprm(3,i).GT.8 .AND. matprm(3,i).LE.10) THEN
            matprm(4,i) = 3
         ELSEIF (matprm(3,i).GT.10 .AND. matprm(3,i).LE.11) THEN
            matprm(4,i) = 4
         ENDIF

         CALL Readmat(confname,matrix(1,1,i),mlinemax,mrowmax,
     &        mline(i),pmatrix(0,0,0,0,0,0,i),maxorder,morder(i),ier)
         IF (ier.NE.0) GOTO 1002
c
c make inverse matrix for 1st order calculation
c
         IF (iflag_reverse_or_not(i).EQ.0) THEN
            a(1,1) = matrix(pmatrix(0,0,0,0,0,1,i),1,i) ! (x|d)
            a(2,1) = matrix(pmatrix(0,0,0,0,0,1,i),2,i) ! (a|d)
            a(1,2) = matrix(pmatrix(0,1,0,0,0,0,i),1,i) ! (x|a)
            a(2,2) = matrix(pmatrix(0,1,0,0,0,0,i),2,i) ! (a|a)
            CALL Reverse_mat(a,mx(1,1,i),ier1)
            mxx(1,i) = matrix(pmatrix(1,0,0,0,0,0,i),1,i) ! (x|x)
            mxx(2,i) = matrix(pmatrix(1,0,0,0,0,0,i),2,i) ! (a|x)
            lmierr(1,i) = .TRUE.
            IF (ier1.NE.1) THEN
               WRITE(*,*)' ANAPAW-M : [Loadmatrixconf] ',
     &              'Failed making inverse matrix-x for ID = ',i,'.'
               lmierr(1,i) = .FALSE.
            ENDIF
            a(1,1) = matrix(pmatrix(0,0,1,0,0,0,i),3,i) ! (y|y)
            a(2,1) = matrix(pmatrix(0,0,1,0,0,0,i),4,i) ! (b|y)
            a(1,2) = matrix(pmatrix(0,0,0,1,0,0,i),3,i) ! (y|b)
            a(2,2) = matrix(pmatrix(0,0,0,1,0,0,i),4,i) ! (b|b)
            CALL Reverse_mat(a,my(1,1,i),ier1)
            lmierr(2,i) = .TRUE.
            IF (ier1.NE.1) THEN
               WRITE(*,*)' ANAPAW-M : [Loadmatrixconf] ',
     &              'Failed making inverse matrix-y for ID = ',i,'.'
               lmierr(2,i) = .FALSE.
            ENDIF
         ELSE
            lmierr(1,i) = .FALSE.
            lmierr(2,i) = .FALSE.
         ENDIF
c
c
c
         nmatrix = matprm(1,i)
         i = i + 1
      ENDDO
      
 1000 WRITE(*,*)' ANAPAW-M : [Loadmatrixconf] Loaded parameters.'

      CLOSE(80)
      RETURN

 1001 WRITE(*,'(2A)')' ANAPAW-E : [Loadmatrixconf] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      STOP
      RETURN

 1002 WRITE(*,*)
     &   ' ANAPAW-E : [Loadmatrixconf] Error while reading parameters.'
      CLOSE(80)
      STOP
      RETURN

      END


