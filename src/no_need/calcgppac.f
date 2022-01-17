      SUBROUTINE Calcgppac(valin,id,valout,lflag)
c
cW# : 1      2      3      4      5      6      7      8      9      10
c     ID     TX1raw TX2raw TY1raw TY2raw TAxraw 
cW# : 11     12     13     14     15     16     17     18     19     20
c     QAraw  TX1    TX2    TY1    TY2    TA     
cW# : 21     22     23     24     25     26     27     28     29     30
c     QA
cW# : 31     32     33     34     35     36     37     38     39     40
c     X(mm)  Y(mm)  TXdiff TYdiff TXsum  TYsum  
c
      IMPLICIT NONE

      REAL    valin(500),valout(500)
      INTEGER id,i
      LOGICAL lflag(2)
      REAL    tcal(2,2),ta,tdiff(2),tsum(2)
      REAL    pos(2)
      REAL    unf,ovf
      PARAMETER (unf=-262144., ovf=262144.)
      LOGICAL  Gate1d
      EXTERNAL Gate1d

      INCLUDE 'numbers.fh'
      INCLUDE 'gppacprm.fh'

      tcal(1,igx) = (valin(2) + rand() - 0.5) * ch2ns(2,id) ! tx1
      tcal(2,igx) = (valin(3) + rand() - 0.5) * ch2ns(4,id) ! tx2
      tcal(1,igy) = (valin(4) + rand() - 0.5) * ch2ns(3,id) ! ty1
      tcal(2,igy) = (valin(5) + rand() - 0.5) * ch2ns(5,id) ! ty2
      ta          = (valin(6) + rand() - 0.5) * ch2ns(1,id) ! ta

      DO i = igx, igy ! i : igx=1=x, igy=2=y
         tdiff(i) = -10000.
         tsum(i)  = -10000.
         IF ( Gate1d(valin(2*i), unf, ovf, 0) .AND. 
     &        Gate1d(valin(2*i+1), unf, ovf, 0) ) THEN
            tdiff(i) = tcal(1,i) - tcal(2,i)
            IF ( Gate1d(valin(6), unf, ovf, 0) ) THEN
               tsum(i) = tcal(1,i) + tcal(2,i) - 2.0 * ta
            ENDIF
         ENDIF
      ENDDO

      valout(12) = tcal(1,igx)
      valout(13) = tcal(2,igx)
      valout(14) = tcal(1,igy)
      valout(15) = tcal(2,igy)
      valout(16) = ta

      valout(33) = tdiff(igx)
      valout(34) = tdiff(igy)
      valout(35) = tsum(igx)
      valout(36) = tsum(igy)

      IF (iflag_outside_offset.EQ.1) THEN
         tdiff(igx) = tdiff(igx) - outoffset(igx,id)
         tdiff(igy) = tdiff(igy) - outoffset(igy,id)
      ENDIF
c Tsum check
      DO i = igx, igy
         pos(i) = -1000.
         lflag(i) = .FALSE.
         IF ( ( iflag_tsumgate(i).AND.
     &          Gate1d(tsum(i),tsumgate(1,id,i),tsumgate(2,id,i),1) )
     &        .OR.
     &        ( .NOT.iflag_tsumgate(i).AND.tsum(i).GT.-1000.) ) THEN
            pos(i) = tdiff(i) * ns2mm(i,id)
            IF (iflag_inside_offset.EQ.1) THEN
               pos(i) = pos(i) - inoffset(i,id)
            ENDIF
            IF (iflag_geom_offset.EQ.1) THEN
               pos(i) = pos(i) - geomoffset(i,id)
            ENDIF
            IF (i.EQ.igx) pos(i) = -1.0*pos(i)
            lflag(i) = .TRUE.
         ENDIF
      ENDDO

      valout(31) = pos(igx)
      valout(32) = pos(igy)

      RETURN

      END

