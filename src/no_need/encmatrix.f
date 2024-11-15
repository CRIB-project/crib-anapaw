c Analyzer 10 : Encmatrix
      SUBROUTINE Encmatrix(val,nx,ny,naok)
c----------------------------------------------------------------------
c ANALYZER 10 : reconstruction of delta using matrix
c----------------------------------------------------------------------
c
c From position
c
c ID =  1 : BigRIPS 
c       2 : ZDS
c
c W#    1         2         3         4         5         6
c      ID   delta(%)     brho                         Focus
c
c From reconstruction
c
c ID = 11 : F3 - F5       Matrix ID : 1    (see src/conf/matrix.conf)
c      12 : F5 - F7                   2
c      13 : F8 - F9                   3
c      14 : F9 - F11                  4
c
c W#    1         2         3         4         5         6
c      ID  delta(%)      brho      d-fl     rec_a     rec_b
c
c      d-fl : (l|d)*delta
c----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'commonprm.fh'
      INCLUDE 'runstat.fh'
      INCLUDE 'numbers.fh'
      INCLUDE 'matrixconf.fh'
      INCLUDE 'matrixprm.fh'
      INCLUDE 'matrixdata.fh'
      INCLUDE 'brho.fh'
      INCLUDE 'ppacfocus.fh'
      INCLUDE 'fpdata.fh'
      INTEGER nx,ny,naok
      REAL    val(nx,ny)
      
      INTEGER i,ierr,order,itemp
      INTEGER id1st,id2nd,j
      REAL    fvec(5),tvec(5),residux,residuy
      REAL    fpxpos

      LOGICAL fcalcpos

      INTEGER prnumber
      SAVE    prnumber

c     for read Brho Value from Rawdata
      CHARACTER*132 xcat,xname,xtag
      real    temp1

      IF (initencflag(10)) THEN
         CALL Loadmatrixconf
         CALL Loadmatrixprm
         xcat = 'dipole'
         xtag = 'nmr'
         xname = 'D3'
         CALL get_runstatus_real( xcat, xname, xtag, temp1 )
         brhonmr(1) = temp1/1000.*6.
         xcat = 'dipole'
         xtag = 'nmr'
         xname = 'D5'
         CALL get_runstatus_real( xcat, xname, xtag, temp1 )
         brhonmr(2) = temp1/1000.*6.
         xcat = 'dipole'
         xtag = 'nmr'
         xname = 'D7'
         CALL get_runstatus_real( xcat, xname, xtag, temp1 )
         brhonmr(3) = temp1/1000.*6.
         xcat = 'dipole'
         xtag = 'nmr'
         xname = 'D8'
         CALL get_runstatus_real( xcat, xname, xtag, temp1 )
         brhonmr(4) = temp1/1000.*6.
c
         brhon(3,5)   = brhonmr(1)
         brhon(5,7)   = brhonmr(2)
         brhon(8,9)   = brhonmr(3)
         brhon(8,10)  = brhonmr(3)
         brhon(8,11)  = (brhonmr(3)+brhonmr(4))/2.
         brhon(9,11)  = brhonmr(4)
         brhon(10,11) = brhonmr(4)
         write(*,'(A33,F7.4)')'  ANAPAW-M : [Encmatrix] Brho3 = '
     &        ,brhonmr(1) 
         write(*,'(A33,F7.4)')'  ANAPAW-M : [Encmatrix] Brho5 = '
     &        ,brhonmr(2) 
         write(*,'(A33,F7.4)')'  ANAPAW-M : [Encmatrix] Brho7 = '
     &        ,brhonmr(3) 
         write(*,'(A33,F7.4)')'  ANAPAW-M : [Encmatrix] Brho8 = '
     &        ,brhonmr(4) 
         prnumber = rnumber
         initencflag(10) = .FALSE.
      ENDIF

      IF (rnumber.NE.prnumber) THEN
         temp1 = 0.
         xcat = 'dipole'
         xtag = 'nmr'
         xname = 'D3'
         CALL get_runstatus_real( xcat, xname, xtag, temp1 )
         brhonmr(1) = temp1/1000.*6.
         xcat = 'dipole'
         xtag = 'nmr'
         xname = 'D5'
         CALL get_runstatus_real( xcat, xname, xtag, temp1 )
         brhonmr(2) = temp1/1000.*6.
         xcat = 'dipole'
         xtag = 'nmr'
         xname = 'D7'
         CALL get_runstatus_real( xcat, xname, xtag, temp1 )
         brhonmr(3) = temp1/1000.*6.
         xcat = 'dipole'
         xtag = 'nmr'
         xname = 'D8'
         CALL get_runstatus_real( xcat, xname, xtag, temp1 )
         brhonmr(4) = temp1/1000.*6.
c
         brhon(3,5)   = brhonmr(1)
         brhon(5,7)   = brhonmr(2)
         brhon(8,9)   = brhonmr(3)
         brhon(8,10)  = brhonmr(3)
         brhon(8,11)  = (brhonmr(3)+brhonmr(4))/2.
         brhon(9,11)  = brhonmr(4)
         brhon(10,11) = brhonmr(4)
         write(*,'(A33,F7.4)')'  ANAPAW-M : [Encmatrix] Brho3 = '
     &        ,brhonmr(1) 
         write(*,'(A33,F7.4)')'  ANAPAW-M : [Encmatrix] Brho5 = '
     &        ,brhonmr(2) 
         write(*,'(A33,F7.4)')'  ANAPAW-M : [Encmatrix] Brho7 = '
     &        ,brhonmr(3) 
         write(*,'(A33,F7.4)')'  ANAPAW-M : [Encmatrix] Brho8 = '
     &        ,brhonmr(4) 
         prnumber = rnumber
      ENDIF


      fcalcpos = .FALSE.
      naok = 0

      DO i = 1, nmtrx, 1
         DO j = 0, 3, 1
            lmatrix(matprm(2,idmtrx(i)),matprm(3,idmtrx(i)),j) = .FALSE. 
         ENDDO
      ENDDO

      DO i = 1, ndfcs, 1
         deltac(i) = -1000.
         brhoc(i)  = -1000.

c         WRITE(*,*)i,iddisfcs(i),fuseppac(iddisfcs(i))
         IF (fuseppac(iddisfcs(i))) THEN
            IF (lfpdata(0,1,iddisfcs(i))) THEN
               fpxpos = fpdata(1,1,iddisfcs(i))
               fcalcpos = .TRUE.
            ELSE
               fpxpos = -1000.
               fcalcpos = .FALSE.
            ENDIF
         ELSEIF (.NOT.fuseppac(iddisfcs(i))) THEN
            itemp = iorder(i) + 4
            IF (lfpdata(itemp,1,iddisfcs(i))) THEN
               fpxpos = fpdata(itemp,1,iddisfcs(i))
               fcalcpos = .TRUE.
            ELSE
               fpxpos = -1000.
               fcalcpos = .FALSE.
            ENDIF
c            WRITE(*,*)itemp,lfpdata(itemp,1,iddisfcs(i))
c            WRITE(*,*)i,iddisfcs(i),fpxpos,fcalcpos
         ENDIF

         IF (fcalcpos) THEN
            deltac(i) = fpxpos/dispersion(i)
            brhoc(i)  = brhonmr(dfcsbrho(i))*(1.+deltac(i)/100.)

            naok = naok + 1
            val(1,naok) = i
            val(2,naok) = deltac(i)
            val(3,naok) = brhoc(i)
            val(6,naok) = iddisfcs(i)
            IF (iddisfcs(i).LE.8) THEN
               rbrho(3,5,0) = brhon(3,5)*(1.+deltac(i)/100.)
               rbrho(5,7,0) = brhon(5,7)*(1.+deltac(i)/100.)
               lmatrix(3,5,0) = .TRUE.
               lmatrix(5,7,0) = .TRUE.
            ELSEIF (iddisfcs(i).GT.8 .AND. iddisfcs(i).LE.11) THEN
               rbrho(8,9,0) = brhon(8,9)*(1.+deltac(i)/100.)
               rbrho(10,11,0) = brhon(10,11)*(1.+deltac(i)/100.)
               rbrho(9,11,0) = brhon(10,11)*(1.+deltac(i)/100.)
               rbrho(8,11,0) = brhon(8,11)*(1.+deltac(i)/100.)
               lmatrix(8,9,0) = .TRUE.
               lmatrix(10,11,0) = .TRUE.
               lmatrix(9,11,0) = .TRUE.
               lmatrix(8,11,0) = .TRUE.
            ENDIF
            fcalcpos = .FALSE.
         ENDIF
      ENDDO

      j = 1

      DO i = 1, nmtrx, 1
         order = 1
         id1st = matprm(2,idmtrx(i))
         id2nd = matprm(3,idmtrx(i))

         IF (lfpdata(0,1,id1st) .AND. 
     &       lfpdata(0,1,id2nd) ) THEN

            fvec(1) = fpdata(1,1,id1st)
            fvec(2) = atan(fpdata(3,1,id1st))*1000.
            fvec(3) = fpdata(2,1,id1st)
            fvec(4) = atan(fpdata(4,1,id1st))*1000.
            tvec(1) = fpdata(1,1,id2nd)
            tvec(2) = atan(fpdata(3,1,id2nd))*1000.
            tvec(3) = fpdata(2,1,id2nd)
            tvec(4) = atan(fpdata(4,1,id2nd))*1000.

            IF (iflag_highorder.EQ.1) order = morder(idmtrx(i))

            IF (order.EQ.1) THEN
               CALL Rec_delta_1st(fvec,tvec,idmtrx(i),
     &              deltar(i),angx(i), angy(i),residux,residuy,ierr) 
            ELSE
               CALL Rec_delta(fvec,tvec,deltar(i),angx(i), residux, 
     &              matrix(1,1,idmtrx(i)),mlinemax,mrowmax,
     &              mline(idmtrx(i)),pmatrix(0,0,0,0,0,0,idmtrx(i)), 
     &              maxorder,order,iflag_reverse_or_not(idmtrx(i)),ierr)
            ENDIF
               
            IF (ierr.NE.1) CYCLE

            brhor(i) = brhonmr(matprm(4,idmtrx(i)))*(1.+deltar(i)/100.)

            dfl(i)   = deltar(i) * 
     &           matrix(pmatrix(0,0,0,0,1,0,idmtrx(i)),5,idmtrx(i))

            IF (lmatrix(id1st,id2nd,j)) j = j + 1

            lmatrix(id1st,id2nd,j) = .TRUE.
            rdelta(id1st,id2nd,j)  = deltar(i)
            rbrho(id1st,id2nd,j)   = brhor(i)
            rangx(id1st,id2nd,j)  = angx(i)
            rangy(id1st,id2nd,j)  = angy(i)
            rdfl(id1st,id2nd,j)    = dfl(i)

            naok = naok + 1
            val(1,naok) = i + 10
            val(2,naok) = deltar(i)
            val(3,naok) = brhor(i)
            val(4,naok) = dfl(i)
            val(5,naok) = angx(i)
            val(6,naok) = angy(i)

         ENDIF

      ENDDO

      RETURN

      END

