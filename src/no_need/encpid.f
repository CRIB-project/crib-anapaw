c Analyzer 11 : Encpid
      SUBROUTINE Encpid(val,nx,ny,naok)
c---------------------------------------------------------------------------
c ANALYZER 11 : PID
c---------------------------------------------------------------------------
c
c ----------
c ID =   1 : TOF & Beta
c            Word lists are defined in 'src/prm/pid/tof.prm'.
c TOF
c    W#: 1           2           3            4            5
c        ID          F3PL-F5PL2  F3PL-F7PL    F3PL-F8PL    F5PL2-F7PL
c        6           7           8            9            10
c        F7PL-F8PL   F7PL-F11PL  F8PL-F9PPAC  F8PL-F11PL   
c
c Beta
c                    22          23           24           25
c                    F3PL-F5PL2  F3PL-F7PL    F3PL-F8PL    F5PL2-F7PL
c        26          27          28           29           30
c        F7PL-F8PL   F7PL-F11PL  F8PL-F9PPAC  F8PL-F11PL   
c
c ----------
c ID =   2 : dE
c    W#: 1       2       3       4       5       6     7
c        ID      F3Pl    F7IC   F7Pl    F11IC   F11Pl  F11Pl_2 
c
c ----------
c
c ID 11 : Brho57_pos,   dE7(IC),  Beta57
c    12 : Brho57_pos,   dE7(IC),  Beta57_NMR
c    13 : Brho57_pos,   dE7(PL),  Beta57
c    14 : Brho57_pos,   dE7(PL),  Beta57_NMR
c    15 : Brho89_pos,  dE11(IC),  Beta711
c    16 : Brho1011_rec,dE11(IC),  Beta89
c    17 : Brho1011_rec,dE11(IC),  Beta89_NMR
c
c    W#: 1       2       3      4      5     6      7
c        ID      Brho    Beta   Z      AoQ   AoQ*Z  (AoQ-1)*Z
c        10
c        matsu_z
c---------------------------------------------------------------------------
c

      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'commonprm.fh'
      INCLUDE 'numbers.fh'
      INCLUDE 'fldata.fh'
      INCLUDE 'pid.fh'
      INCLUDE 'pidprm.fh'
      INCLUDE 'matrixdata.fh'
      INCLUDE 'fpdata.fh'
      INCLUDE 'physdata.fh'

      INTEGER nx, ny, naok
      REAL    val(nx,ny)

c for tree
      INCLUDE 'fortree.fh'
c

c local
      INTEGER i,j
      INTEGER staf,stak,stof,stok,ncfg(nfocus,nfocus)
      REAL    toffset,temp
c user vals
      REAL    tofval,flval,betaval,deval,brhoval
      REAL    zval,aoqval
      LOGICAL lcalc

c Matsushita 11/14 for Caliration
      REAL matsu_z


c
      IF (initencflag(11)) THEN
         CALL Loadpidprm
         DO i = 1, ntof, 1
            DO j = 1, 3, 1
               ltof(INT(tfk(1,i)),INT(tfk(3,i)),j) = .FALSE.
            ENDDO
         ENDDO
         DO i = 1, nfocus, 1
            DO j = 1, nfocus, 1
               ncfg(i,j) = 1
            ENDDO
         ENDDO

         DO i = 1, ntof, 1
            staf = INT(tfk(1,i))
            stak = INT(tfk(2,i))
            stof = INT(tfk(3,i))
            stok = INT(tfk(4,i))

            IF (ltof(staf,stof,ncfg(staf,stof))) 
     &           ncfg(staf,stof) = ncfg(staf,stof) + 1
            ltof(staf,stof,ncfg(staf,stof)) = .TRUE.
            IF (lfltof) THEN
               fl(staf,stof,ncfg(staf,stof)) = 
     &              ( flfocus(stof) + flpla(stok,stof) ) -
     &              ( flfocus(staf) + flpla(stak,staf) )
            ELSE
               fl(staf,stof,ncfg(staf,stof)) = 
     &              flfocus(stof)- flfocus(staf)
            ENDIF
         ENDDO
         initencflag(11) = .FALSE.
      ENDIF

c initialization for tree =====
      do i=1,20
         l_tof(i) = .FALSE.
         do j=1,2
            l_aoq(j,i) = .FALSE.
         enddo
      enddo
c==============================

c --------------------------------------------------------------------------
c
c for TOF. 
c 
c TOF(1st,2nd,j) are available, which are defined in 'src/prm/pid/tof.prm'.
c j is order (max : 3). If same TOF(1st,2nd) exist, j = j + 1.
c
      DO i = 1, ntof, 1
         DO j = 1, 3, 1
            ltof(INT(tfk(1,i)),INT(tfk(3,i)),j) = .FALSE.
         ENDDO
      ENDDO
      DO i = 1, nfocus, 1
         DO j = 1, nfocus, 1
            ncfg(i,j) = 1
         ENDDO
      ENDDO

      naok = 0

c      j = 1
      naok = naok + 1
      val(1,naok) = 1

      DO i = 1, ntof
         staf = INT(tfk(1,i))
         stak = INT(tfk(2,i))
         stof = INT(tfk(3,i))
         stok = INT(tfk(4,i))
         toffset = tfk(5,i)
         IF (ltof(staf,stof,ncfg(staf,stof))) 
     &        ncfg(staf,stof) = ncfg(staf,stof) + 1
         tof(staf,stof,ncfg(staf,stof)) = -1000.
         betac(staf,stof,ncfg(staf,stof)) = -1000.
         IF (lfpdata(stak,2,staf) .AND. lfpdata(stok,2,stof)) THEN
            ltof(staf,stof,ncfg(staf,stof)) = .TRUE.
            tof(staf,stof,ncfg(staf,stof)) = 
     &           fpdata(stok,2,stof) - fpdata(stak,2,staf) + toffset
            IF (fl(staf,stof,ncfg(staf,stof)).GT.0.) THEN
               betac(staf,stof,ncfg(staf,stof)) = 
     &              fl(staf,stof,ncfg(staf,stof))/
     &              tof(staf,stof,ncfg(staf,stof))/clight
            ENDIF
         ENDIF
         val(i+1,naok) = tof(staf,stof,ncfg(staf,stof))
         val(i+21,naok) = betac(staf,stof,ncfg(staf,stof))

c for tree
         if(tof(staf,stof,ncfg(staf,stof)) .gt. 0.) then
            l_tof(i) = .TRUE.
         endif
         tofdata(i) = tof(staf,stof,ncfg(staf,stof))
c         
      ENDDO
c for tree
      notof = ntof
c

c --------------------------------------------------------------------------
c
c for dE. 
c dek(1,i) : Focus ID
c            1 - nfocus
c dek(2,i) : Detector type
c            1 = I.C. 
c            2 = SSD
c            3 = Others : 1st plastic
c            4 = Others : 2nd plastic 
c
      naok = naok + 1
      val(1,naok) = 2
      DO i = 1, nde
         de(dek(2,i),dek(1,i)) = -1000.
         IF (lfpdata(dek(2,i),3,dek(1,i))) THEN
            de(dek(2,i),dek(1,i)) = fpdata(dek(2,i),3,dek(1,i))
         ENDIF
c         val(dek(1,i)+10,naok) = de(dek(2,i),dek(1,i))
         val(i+1,naok) = de(dek(2,i),dek(1,i))
      ENDDO
c --------------------------------------------------------------------------

      DO i = 1, nconfig, 1
         lcalc = .TRUE.
         tofval = -1000.
         flval  = -1000.
         betaval = -1000.
         brhoval = -1000.
         zval = -1000.
         aoqval = -1000.
         matsu_z = -1000.

         IF (lflcorrect .AND. 
     &        lmatrix(brcnf(1,i),brcnf(2,i),brcnf(3,i))) THEN
            fl(btcnf(1,i),btcnf(2,i),btcnf(3,i)) = 
     &           fl(btcnf(1,i),btcnf(2,i),btcnf(3,i)) + 
     &           rdfl(btcnf(1,i),btcnf(2,i),btcnf(3,i))
         ENDIF
c -----------------------------------
c calculation of dE
c
         IF (de(decnf(2,i),decnf(1,i)).GT.0.) THEN
            deval = de(decnf(2,i),decnf(1,i))
         ELSE
            lcalc = lcalc .AND. .FALSE.
         ENDIF

c -----------------------------------
c calculation of beta
c
         SELECT CASE (btcnf(4,i))
         CASE (1)
            IF (ltof(btcnf(1,i),btcnf(2,i),btcnf(3,i)) ) THEN
               tofval  = tof(btcnf(1,i),btcnf(2,i),btcnf(3,i))
               flval   = fl(btcnf(1,i),btcnf(2,i),btcnf(3,i))
               betaval = flval/tofval/clight
            ELSE
               lcalc = lcalc .AND. .FALSE.
            ENDIF
         CASE (2)
            IF ( ltof(tofcnf(1,i),tofcnf(2,i),tofcnf(3,i)) ) THEN
               CALL Rec_beta(
     &              brhon(btcnf(1,i),btcnf(2,i)),
     &              brhon(btpcnf(1,i),btpcnf(2,i)),
     &              fl(btcnf(1,i),btcnf(2,i),btcnf(3,i)),
     &              fl(btpcnf(1,i),btpcnf(2,i),btpcnf(3,i)),
     &              tof(tofcnf(1,i),tofcnf(2,i),tofcnf(3,i)),
     &              betaval,temp)
            ELSE
               lcalc = lcalc .AND. .FALSE.
            ENDIF
         CASE (3)
            IF ( ltof(tofcnf(1,i),tofcnf(2,i),tofcnf(3,i)) .AND.
     &           lmatrix(btcnf(1,i),btcnf(2,i),btcnf(3,i)) .AND.
     &           lmatrix(btpcnf(1,i),btpcnf(2,i),btpcnf(3,i)) ) THEN
               CALL Rec_beta(
     &              rbrho(btcnf(1,i),btcnf(2,i),btcnf(3,i)),
     &              rbrho(btpcnf(1,i),btpcnf(2,i),btpcnf(3,i)),
     &              fl(btcnf(1,i),btcnf(2,i),btcnf(3,i)),
     &              fl(btpcnf(1,i),btpcnf(2,i),btpcnf(3,i)),
     &              tof(tofcnf(1,i),tofcnf(2,i),tofcnf(3,i)),
     &              betaval,temp)
            ELSE
               lcalc = lcalc .AND. .FALSE.
            ENDIF
         CASE DEFAULT
            lcalc = lcalc .AND. .FALSE.
         END SELECT

c -----------------------------------
c calculation of Z
c
         IF (lcalc) THEN
            CALL Z_calc( deval,betaval,
     &                   ionpair(decnf(2,i),decnf(1,i)),
     &                   zcoeff(1,decnf(2,i),decnf(1,i)),zval )
            matsu_z = sqrt(  deval / 
     &                  ( log(ionpair(decnf(2,i),decnf(1,i))*betaval**2)
     &                    - log(1.0-betaval**2) - betaval**2 )
     &                   ) * betaval
         ENDIF
c -----------------------------------
c calculation of brho
c
         SELECT CASE (brcnf(4,i))
         CASE (1)
            IF (lmatrix(brcnf(1,i),brcnf(2,i),brcnf(3,i))) THEN
               brhoval = rbrho(brcnf(1,i),brcnf(2,i),brcnf(3,i))
            ELSE
               lcalc = lcalc .AND. .FALSE.
            ENDIF
         CASE (2)
            IF (lmatrix(brcnf(1,i),brcnf(2,i),0)) THEN
               brhoval = rbrho(brcnf(1,i),brcnf(2,i),0)
            ELSE
               lcalc = lcalc .AND. .FALSE.
            ENDIF
         CASE (3)
            brhoval = brhon(brcnf(1,i),brcnf(2,i))
         CASE DEFAULT
            lcalc = lcalc .AND. .FALSE.
         END SELECT
c         WRITE(*,*)idcnf(i),brcnf(1,i),brcnf(2,i),brcnf(3,i),brcnf(4,i)
c         WRITE(*,*)lmatrix(brcnf(1,i),brcnf(2,i),0)

c -----------------------------------
c calculation of AoQ
c
         IF (lcalc) CALL Aoq_calc(brhoval,betaval,aoqval)

c -----------------------------------
c Store event data
         naok = naok + 1
         val(1,naok) = idcnf(i)
         val(2,naok) = brhoval
         val(3,naok) = betaval
         val(4,naok) = zval
         val(5,naok) = aoqval
         val(6,naok) = aoqval * zval
         val(7,naok) = ( aoqval - 1.) * zval
         val(10,naok) = matsu_z
c for tree
         if(zval .gt. 0.) then
            l_aoq(1,i) = .TRUE.
         endif
         if(betaval .gt.0.) then
            l_aoq(2,i) = .TRUE.
         endif
         aoqdata(1,i) = brhoval
         aoqdata(2,i) = betaval
         aoqdata(3,i) = zval
         aoqdata(4,i) = aoqval
c
      ENDDO
c for tree
      noaoq = nconfig
c

      RETURN

      END
