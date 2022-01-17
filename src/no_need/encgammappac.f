c Analyzer 13 : Encgammappac
      SUBROUTINE Encgammappac(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata)
c---------------------------------------------------------------------------
c ANALYZER 13 : GammaPPAC
c---------------------------------------------------------------------------
c
c Positions (x,y,z) and timing are stored into 'gppacxyzt' for each PPAC.
c
c gppacxyz(i,j,k) ; i     : seq. number in a PPAC
c                  j = 1 : x position
c                      2 : y position
c                      3 : z position of x cathode
c                      4 : z position of y cathode
c                      5 : anode timing
c                  k     : Focus
c
c  ID  :  Name     : Focus : Order : Type 1: RIBF-Single
c                                         2: RIBF-Double
c                                         3: RARF-Single
c                                         4: RIBF-Tandem DPPAC = 3
c
cID = 1: F8PPAC-3A     8       1       4
cID = 2: F8PPAC-3B     8       2       4
c
cW# : 1      2      3      4      5      6      7      8      9      10
c     ID     TX1raw TX2raw TY1raw TY2raw TAxraw 
cW# : 11     12     13     14     15     16     17     18     19     20
c     QAraw  TX1    TX2    TY1    TY2    TA     
cW# : 21     22     23     24     25     26     27     28     29     30
c     QA
cW# : 31     32     33     34     35     36     37     38     39     40
c     X(mm)  Y(mm)  TXdiff TYdiff TXsum  TYsum  QXsum  QYsum
c
cID = 101 :  Focus
cA = x/z, B = y/z, T = avg(A_T) of upstream PPAC
cW# : 1      2      3      4      5      6      7      8      9      10
c     ID     X      Y      A      B      T      ResX   ResY   A(mrad)B(mard)
c
cID = 201 :  Angle @ F8PPAC-4
cA = x/z, B = y/z, T = avg(A_T) of upstream PPAC
cW# : 1      2      3      4      5      6      
c     ID     A      B             A(mrad)B(mard)
c
cID = 202 :  Scattering angle
cW# : 1      2      3            4
c     ID     Theta  Theta(mrad)  Theta(deg.)
c
cW# :       12      13           14
c            Thetax Thetay(mrad) Thetax(deg.)
c
cW# :       22      23           24
c           Thetay  Thetay(mrad) Thetax(deg.)
c
c
      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'commonprm.fh'
      INCLUDE 'numbers.fh'

      INCLUDE 'gppacconf.fh'
      INCLUDE 'gppacprm.fh'
      INCLUDE 'gppacfocus.fh'
      INCLUDE 'ppacfocus.fh'
      INCLUDE 'fpdata.fh'

      INTEGER nx, ny, ndet, ndata, naok
      INTEGER rawdata(ndata,ndet)
      INTEGER nhitdata(ndet)
      INTEGER hitdet(ndet)
      INTEGER nhitdet
      REAL    val(nx,ny)

c local
      INTEGER i,j,k,id,ierr1,ierr2,ifcs,inp
      INTEGER nvp(2),bvp(2)
      LOGICAL lf(2,20),lid(20),lfcs(20),lstore
      SAVE    lid,lfcs
c for scattering angle
      REAL    pvecx(3),pvecy(3),pveczx(3),pveczy(3)
      REAL    vecin(3),vecout(3),normin,normout,sum
      REAL    temp1,temp2

      IF (initencflag(13)) THEN
         CALL Loadgppacconf
         CALL Loadgppacprm
         DO id = 1, ngppac
            gppacxyzt(ppacfcs(2,id),igzx,ppacfcs(1,id))
     &           = ppaczpos(1,id) - dzpos(ppacfcs(1,id))
            gppacxyzt(ppacfcs(2,id),igzy,ppacfcs(1,id)) 
     &           = ppaczpos(2,id) - dzpos(ppacfcs(1,id))
            lid(id) = .FALSE.
            lfcs(id) = .FALSE.
         ENDDO
         initencflag(13) = .FALSE.
      ENDIF

      DO i = 1, nhitdet, 1
         lid(hitdet(i)) = .TRUE.
         lfcs(ppacfcs(1,hitdet(i))) = .TRUE.
      ENDDO

      naok = 0

      nvp(igx) = 0
      nvp(igy) = 0
      bvp(igx) = 0
      bvp(igy) = 0
      lstore = .TRUE.

      DO i = 1, maxfcs
         
         IF (.NOT.lfcs(i)) CYCLE

         DO j = 1, maxnp(i)
            id = ppacid(j,i)

            IF (.NOT.lid(id)) CYCLE

            gppacxyzt(j,igx,i) = -1000.
            gppacxyzt(j,igy,i) = -1000.
            gppacxyzt(j,igt,i) = -1000.
            
            naok = naok + 1
            val(1,naok) = id
            DO k = 1, 10
               val(k+1,naok) = rawdata(k,id)
            ENDDO
            
            CALL Calcgppac(val(1,naok),id,val(1,naok),lf(1,id))

            pvecx(id+1) = -1000.
            pvecy(id+1) = -1000.
            IF (lf(igx,id) .OR. lf(igy,id)) 
     &                      gppacxyzt(j,igt,i) = val(16,naok) ! TA (ns)
            IF (lf(igx,id)) THEN
               gppacxyzt(j,igx,i) = val(31,naok) ! X (mm)
               pvecx(id+1) = val(31,naok)
               pveczx(id+1) = gppacxyzt(j,igzx,i)
            ENDIF
            IF (lf(igy,id)) THEN
               gppacxyzt(j,igy,i) = val(32,naok) ! Y (mm)
               pvecy(id+1) = val(32,naok)
               pveczy(id+1) = gppacxyzt(j,igzy,i)
            ENDIF

c Merging GammaPPAC data into PPAC data
            ifcs = ppacfcs(1,id)
            inp  = epmaxnp(ifcs)+j
            ppacxyzt(inp,igx,ifcs)  = gppacxyzt(j,igx,i)
            ppacxyzt(inp,igy,ifcs)  = gppacxyzt(j,igy,i)
            ppacxyzt(inp,igzx,ifcs) = gppacxyzt(j,igzx,i)
            ppacxyzt(inp,igzy,ifcs) = gppacxyzt(j,igzy,i)
            ppacxyzt(inp,igt,ifcs)  = gppacxyzt(j,igt,i)

         ENDDO

      ENDDO

      DO i = 1, epmaxnp(8)+1, 1
         IF (ppacxyzt(i,igx,8).GT.-900.) THEN
            nvp(igx) = nvp(igx) + 1
            bvp(igx) = bvp(igx) + 2**(i-1)
         ENDIF
         IF (ppacxyzt(i,igy,8).GT.-900.) THEN
            nvp(igy) = nvp(igy) + 1
            bvp(igy) = bvp(igy) + 2**(i-1)
         ENDIF
      ENDDO

      SELECT CASE (epmaxnp(8)+1)
      CASE (1)
         lstore = .FALSE.
      CASE (2)
         DO j = igx, igy, 1
            IF (nvp(j).LE.1) lstore = lstore .AND. .FALSE.
         ENDDO
      CASE DEFAULT
         DO j = igx, igy, 1
            IF (nvp(j).LE.1) lstore = lstore .AND. .FALSE.
            IF (bvp(j).EQ.3 .OR. bvp(j).EQ.12)
     &              lstore = lstore .AND. .FALSE.
         ENDDO
      END SELECT

      IF (lstore) THEN
c         CALL Rayfit(ppacxyzt(1,igx,8),ppacxyzt(1,igzx,8),epmaxnp(8)+1,
c     &        bvp(igx),fpdata(1,1,8),fpdata(3,1,8),resx(8),ierr1)
c         CALL Rayfit(ppacxyzt(1,igy,8),ppacxyzt(1,igzy,8),epmaxnp(8)+1,
c     &        bvp(igy),fpdata(2,1,8),fpdata(4,1,8),resy(8),ierr2)
         
         IF ( ierr1.EQ.0 .AND. ierr2.EQ.0 ) THEN
            lfpdata(0,1,8) = .TRUE.
            naok = naok + 1
            val(1,naok) = 101
            val(2,naok) = fpdata(1,1,8)
            val(3,naok) = fpdata(2,1,8)
            val(4,naok) = fpdata(3,1,8)
            val(5,naok) = fpdata(4,1,8)
            val(6,naok) = fpdata(5,2,8)
            val(7,naok) = resx(8)
            val(8,naok) = resy(8)
            val(9,naok)  = atan(fpdata(3,1,8))*1000.
            val(10,naok) = atan(fpdata(4,1,8))*1000.
         ENDIF
         
      ENDIF
c
c for scattering angle
c
      nvp(igx) = 0
      nvp(igy) = 0
      bvp(igx) = 0
      bvp(igy) = 0
      lstore = .TRUE.

      pveczx(1) = 16.0          ! target position
      pveczy(1) = 16.0          ! target position
      pvecx(1)  = fpdata(1,1,8) + pveczx(1)*fpdata(3,1,8)
      pvecy(1)  = fpdata(2,1,8) + pveczy(1)*fpdata(4,1,8)
      
      vecin(1) = fpdata(3,1,8)
      vecin(2) = fpdata(4,1,8)
      vecin(3) = 1.

      DO i = 1, 3, 1
         IF (pvecx(i).GT.-900.) THEN
            nvp(igx) = nvp(igx) + 1
            bvp(igx) = bvp(igx) + 2**(i-1)
         ENDIF
         IF (pvecy(i).GT.-900.) THEN
            nvp(igy) = nvp(igy) + 1
            bvp(igy) = bvp(igy) + 2**(i-1)
         ENDIF
      ENDDO

      DO i = igx, igy, 1
         IF (nvp(i).LE.1) lstore = lstore .AND. .FALSE.
         IF (bvp(i).EQ.6) lstore = lstore .AND. .FALSE.
      ENDDO

      vecout(1) = -1000.
      vecout(2) = -1000.
      vecout(3) = 1.
      IF (lstore) THEN
         CALL Rayfit(pvecx,pveczx,3,bvp(igx),
     &        temp1,vecout(1),temp2,ierr1)
         CALL Rayfit(pvecy,pveczy,3,bvp(igy),
     &        temp1,vecout(2),temp2,ierr2)

         IF (ierr1.EQ.0 .AND. ierr2.EQ.0) THEN

            naok = naok + 1
            val(1,naok) = 201
            val(2,naok) = vecout(1)
            val(3,naok) = vecout(2)
            val(5,naok) = ATAN(vecout(1))*1000.
            val(6,naok) = ATAN(vecout(2))*1000.

            normin = SQRT( vecin(1)**2+vecin(2)**2+vecin(3)**2 )
            normout = SQRT( vecout(1)**2+vecout(2)**2+vecout(3)**2 )
            sum = 0.
            DO i = 1, 3, 1
               sum = sum + vecin(i)*vecout(i)
            ENDDO
            naok = naok + 1
            val(1,naok) = 202
            val(2,naok) = ACOS(sum/normin/normout)
            val(3,naok) = ACOS(sum/normin/normout) * 1000.
            val(4,naok) = ACOS(sum/normin/normout) / 3.14159 * 180.
            
            normin = SQRT( vecin(1)**2+vecin(3)**2 )
            normout = SQRT( vecout(1)**2+vecout(3)**2 )
            sum = 0.
            DO i = 1, 3, 2
               sum = sum + vecin(i)*vecout(i)
            ENDDO
            val(12,naok) = ACOS(sum/normin/normout)
            val(13,naok) = ACOS(sum/normin/normout) * 1000.
            val(14,naok) = ACOS(sum/normin/normout) / 3.14159 * 180.
            
            normin = SQRT( vecin(2)**2+vecin(3)**2 )
            normout = SQRT( vecout(2)**2+vecout(3)**2 )
            sum = 0.
            DO i = 2, 3, 1
               sum = sum + vecin(i)*vecout(i)
            ENDDO
            val(22,naok) = ACOS(sum/normin/normout)
            val(23,naok) = ACOS(sum/normin/normout) * 1000.
            val(24,naok) = ACOS(sum/normin/normout) / 3.14159 * 180.
         ENDIF
      ENDIF

      RETURN

      END
