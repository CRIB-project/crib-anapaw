      SUBROUTINE Calcppac(valin,id,valout,lflag)
c
cW# : 1      2      3      4      5      6      7      8      9      10
c     ID     TX1raw TX2raw TY1raw TY2raw TAxraw QX1raw QX2raw QY1raw QY2raw
cW# : 11     12     13     14     15     16     17     18     19     20
c     QAraw  TX1    TX2    TY1    TY2    TA     QX1    QX2    QY1    QY2
cW# : 21     22     23     24     25     26     27     28     29     30
c     QA     TX1-TA TX2-TA TY1-TA TY2-TA
cW# : 31     32     33     34     35     36     37     38     39     40
c     X(mm)  Y(mm)  TXdiff TYdiff TXsum  TYsum  QXsum  QYsum
c
      IMPLICIT NONE

      REAL    valin(500),valout(500)
      INTEGER id,i
      LOGICAL lflag(2)
      REAL    tcal(2,2),ta,tdiff(2),tsum(2)
      REAL    qcal(2,2),qa,qsum(2),pos(2)
      REAL    ovf
      PARAMETER (ovf=4094.)
      LOGICAL  Gate1d
      EXTERNAL Gate1d

      INCLUDE 'numbers.fh'
      INCLUDE 'ppacprm.fh'

c      REAL    valin6id21
c      SAVE    valin6id21
cc
cc TA of F8PPAC-2B (ID=22) was missing in DayTwo experiment.
cc
c      IF (id.EQ.21) valin6id21 = valin(6)
c      IF (id.EQ.22) valin(6) = valin6id21
cc

      tcal(1,ix) = (valin(2) + rand() - 0.5) * ch2ns(2,id) ! tx1
      tcal(2,ix) = (valin(3) + rand() - 0.5) * ch2ns(4,id) ! tx2
      tcal(1,iy) = (valin(4) + rand() - 0.5) * ch2ns(3,id) ! ty1
      tcal(2,iy) = (valin(5) + rand() - 0.5) * ch2ns(5,id) ! ty2
      ta         = (valin(6) + rand() - 0.5) * ch2ns(1,id) ! ta
      
      qcal(1,ix) = valin(7)  - qped(2,id) ! qx1
      qcal(2,ix) = valin(8)  - qped(4,id) ! qx2
      qcal(1,iy) = valin(9)  - qped(3,id) ! qy1
      qcal(2,iy) = valin(10) - qped(5,id) ! qy2
      qa         = valin(11) - qped(1,id) ! qa

      DO i = ix, iy ! i : ix=1=x, iy=2=y
         tdiff(i) = -10000.
         tsum(i)  = -10000.
         qsum(i)  = -1000.
         IF ( Gate1d(valin(2*i), 0.0, ovf, 0) .AND. 
     &        Gate1d(valin(2*i+1), 0.0, ovf, 0) ) THEN
            tdiff(i) = tcal(1,i) - tcal(2,i)
            IF ( Gate1d(valin(6), 0.0, ovf, 0) ) THEN
               tsum(i) = tcal(1,i) + tcal(2,i) - 2.0 * ta
               valout(2*i+20) = tcal(1,i) - ta
               valout(2*i+21) = tcal(2,i) - ta
            ELSE
               valout(2*i+20) = -1000.
               valout(2*i+21) = -1000.
            ENDIF
         ENDIF
         IF ( qcal(1,i).GT.0. .AND. qcal(2,i).GT.0. )THEN
            qsum(i) = sqrt( qcal(1,i) * qcal(2,i) )
         ENDIF
      ENDDO

      valout(12) = tcal(1,ix)
      valout(13) = tcal(2,ix)
      valout(14) = tcal(1,iy)
      valout(15) = tcal(2,iy)
      valout(16) = ta
      valout(17) = qcal(1,ix)
      valout(18) = qcal(2,ix)
      valout(19) = qcal(1,iy)
      valout(20) = qcal(2,iy)
      valout(21) = qa
c      WRITE(*,*)'**',valout(12),valout(13)

      valout(33) = tdiff(ix)
      valout(34) = tdiff(iy)
      valout(35) = tsum(ix)
      valout(36) = tsum(iy)
      valout(37) = qsum(ix)
      valout(38) = qsum(iy)

      IF (iflag_outside_offset.EQ.1) THEN
         tdiff(ix) = tdiff(ix) - outoffset(ix,id)
         tdiff(iy) = tdiff(iy) - outoffset(iy,id)
      ENDIF

c Tsum check
      DO i = ix, iy
         pos(i) = -1000.
         lflag(i) = .FALSE.
         IF ( ( iflag_tsumgate(i).AND.
     &          Gate1d(tsum(i),tsumgate(1,id,i),tsumgate(2,id,i),1) )
     &        .OR.
     &        ( .NOT.iflag_tsumgate(i).AND.tsum(i).GT.-10000.) ) THEN
            pos(i) = tdiff(i) * ns2mm(i,id)
            IF (iflag_inside_offset.EQ.1) THEN
               pos(i) = pos(i) - inoffset(i,id)
            ENDIF
            IF (iflag_geom_offset.EQ.1) THEN
               pos(i) = pos(i) - geomoffset(i,id)
            ENDIF
            IF (i.EQ.ix) pos(i) = -1.0*pos(i)
            lflag(i) = .TRUE.
         ENDIF
      ENDDO
      
      valout(31) = pos(ix)
      valout(32) = pos(iy)

      RETURN
      
      END

