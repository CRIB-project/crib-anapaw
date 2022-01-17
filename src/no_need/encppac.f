c Analyzer 1 : Encppac
      SUBROUTINE Encppac(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata)
c---------------------------------------------------------------------------
c ANALYZER 1 : PPAC
c---------------------------------------------------------------------------
c
c Positions (x,y,z) and timing are stored into 'ppacxyzt' for each PPAC.
c
c ppacxyz(i,j,k) ; i     : seq. number in a PPAC
c                  j = 1 : x position
c                      2 : y position
c                      3 : z position of x cathode
c                      4 : z position of y cathode
c                      5 : anode timing
c                  k     : Focus
c
c
c  ID  :  Name     : Focus : Order : Type 1: RIBF-Single
c                                         2: RIBF-Double
c                                         3: RARF-Single
c
c     ** Order of ID is same as that written in ppac.conf. **
c
cID = 1: F1PPAC-1      1       1       1
cID = 2: F1PPAC-2      1       2       1
cID = 3: F2PPAC-1      2       1       1
cID = 4; F2PPAC-2      2       2       1
cID = 5: F3PPAC-1A     3       1       2
cID = 6: F3PPAC-1B     3       1       2
cID = 7: F3PPAC-2A     3       2       2
cID = 8: F3PPAC-2B     3       2       2
cID = 9: F4PPAC        4       1       1
cID =10: F5PPAC-1A     5       1       2
cID =11: F5PPAC-1B     5       1       2
cID =12: F5PPAC-2A     5       2       2
cID =13: F5PPAC-2B     5       2       2
cID =14: F6PPAC        6       1       1
cID =15: F7PPAC-1A     7       1       2
cID =16: F7PPAC-1B     7       1       2
cID =17: F7PPAC-2A     7       2       2
cID =18: F7PPAC-2B     7       2       2
cID =19: F8PPAC-1A     8       1       2
cID =20: F8PPAC-1B     8       1       2
cID =21: F8PPAC-2A     8       2       2
cID =22: F8PPAC-2B     8       2       2
cID =23: F9PPAC-1A     9       1       2
cID =24: F9PPAC-1B     9       1       2
cID =25: F9PPAC-2A     9       2       2
cID =26: F9PPAC-2B     9       2       2
cID =27: F10PPAC-1A    10      1       2
cID =28: F10PPAC-1B    10      1       2
cID =29: F10PPAC-2A    10      2       2
cID =30: F10PPAC-2B    10      2       2
cID =31: F11PPAC-1A    11      1       2
cID =32: F11PPAC-1B    11      1       2
cID =33: F11PPAC-2A    11      2       2
cID =34: F11PPAC-2B    11      2       2
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
c modified focus position 
c
cID =101: F1
cID =102: F2
cID =103: F3
cID =104: F4
cID =105: F5
cID =106: F6
cID =107: F7
cID =108: F8
cID =109: F9
cID =110: F10
cID =111: F11
cA = x/z, B = y/z, T = avg(A_T) of upstream PPAC
cW# : 1      2      3      4      5      6      7      8      9      10
c     ID     X      Y      A      B      T      ResX   ResY   A(mrad)B(mard)
c---------------------------------------------------------------------------
c

      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'commonprm.fh'

      INTEGER nx, ny, ndet, ndata, naok
      INTEGER rawdata(ndata,ndet)
      INTEGER nhitdata(ndet)
      INTEGER hitdet(ndet)
      INTEGER nhitdet
      REAL    val(nx,ny)

      INCLUDE 'numbers.fh'
      INCLUDE 'ppacprm.fh'
      INCLUDE 'ppacconf.fh'
      INCLUDE 'ppacfocus.fh'
      INCLUDE 'fpdata.fh'
c for tree
      INCLUDE 'fortree.fh'
c

c local
      INTEGER i,j,k,id,ierr1,ierr2
      INTEGER nvp(2),bvp(2)
      LOGICAL lf(2,nppac),lid(nppac),lfcs(nppac),lstore

      SAVE lid,lfcs

      IF (initencflag(1)) THEN
         CALL Loadppacconf
         CALL Loadppacprm
         DO id = 1, nppac, 1
            ppacxyzt(ppacfcs(2,id),izx,ppacfcs(1,id)) 
     &           = ppaczpos(1,id) - dzpos(ppacfcs(1,id))
            ppacxyzt(ppacfcs(2,id),izy,ppacfcs(1,id))
     &           = ppaczpos(2,id) - dzpos(ppacfcs(1,id))
            lid(id) = .FALSE.
            lfcs(id) = .FALSE.
         ENDDO
         initencflag(1) = .FALSE.
      ENDIF

      DO i = 1, nhitdet, 1
         lid(hitdet(i)) = .TRUE.
         lfcs(ppacfcs(1,hitdet(i))) = .TRUE.
      ENDDO

      naok = 0
      
      DO i = 1, maxfcs, 1
         nvp(ix) = 0
         nvp(iy) = 0
         bvp(ix) = 0
         bvp(iy) = 0
         lstore = .TRUE.

         lfpdata(0,1,i) = .FALSE.
         lfpdata(5,2,i) = .FALSE.

         IF (.NOT.lfcs(i)) CYCLE

         DO j = 1, maxnp(i), 1
            id = ppacid(j,i)

            IF (.NOT.lid(id)) CYCLE

            ppacxyzt(j,ix,i) = -1000.
            ppacxyzt(j,iy,i) = -1000.
            ppacxyzt(j,it,i) = -1000.
            
            naok = naok + 1
            val(1,naok) = id
            DO k = 1, 10, 1
               val(k+1,naok) = rawdata(k,id)
            ENDDO

            CALL Calcppac(val(1,naok),id,val(1,naok),lf(1,id))

            IF (lf(ix,id) .OR. lf(iy,id)) 
     &         ppacxyzt(j,it,i) = val(16,naok) ! TA (ns)
            IF (lf(ix,id)) THEN
               ppacxyzt(j,ix,i) = val(31,naok) ! X (mm)
               nvp(ix) = nvp(ix) + 1
               bvp(ix) = bvp(ix) + 2**(j-1)
            ENDIF
            IF (lf(iy,id)) THEN
               ppacxyzt(j,iy,i) = val(32,naok) ! Y (mm)
               nvp(iy) = nvp(iy) + 1
               bvp(iy) = bvp(iy) + 2**(j-1)
            ENDIF
               
c for ppac_focus used in Encgammappac
            epmaxnp(i)  = maxnp(i)  ! maximum number of PPAC in a focus
c
         ENDDO

         SELECT CASE (maxnp(i))
         CASE (1)
            lstore = .FALSE.
         CASE (2)
            DO j = ix, iy, 1
               IF (nvp(j).LE.1) lstore = lstore .AND. .FALSE.
            ENDDO
         CASE DEFAULT
            DO j = ix, iy, 1
               IF (nvp(j).LE.1) lstore = lstore .AND. .FALSE.

               IF (bvp(j).EQ.3 .OR. bvp(j).EQ.12 .OR. bvp(j).EQ.48)  
     &              lstore = lstore .AND. .FALSE.
            ENDDO
         END SELECT

         IF (lstore) THEN
            
            k = 0
            fpdata(5,2,i) = 0.  ! Average PPAC Anode Timing 
            DO j = 1, 2         ! for PPACFT. upstream only
c               IF (ppacxyzt(j,it,i).GT.0.) THEN
                  fpdata(5,2,i) = fpdata(5,2,i) + ppacxyzt(j,it,i)  ! ppacxyzt( seq.number, xyzt, focal plane )
                  k = k + 1
c               ENDIF
            ENDDO
            IF (k.EQ.0) THEN
               fpdata(5,2,i) = -1000.  ! Average PPAC Anode Timing 
            ELSE
               fpdata(5,2,i) = fpdata(5,2,i)/k  ! Average PPAC Anode Timing 
            ENDIF

            CALL Rayfit(ppacxyzt(1,ix,i),ppacxyzt(1,izx,i),maxnp(i),
     &           bvp(ix),fpdata(1,1,i),fpdata(3,1,i),resx(i),ierr1)
            CALL Rayfit(ppacxyzt(1,iy,i),ppacxyzt(1,izy,i),maxnp(i),
     &           bvp(iy),fpdata(2,1,i),fpdata(4,1,i),resy(i),ierr2)
            
            IF ( ierr1.EQ.0 .AND. ierr2.EQ.0 ) THEN
               lfpdata(0,1,i) = .TRUE.
               lfpdata(5,2,i) = .TRUE.
               naok = naok + 1
               val(1,naok) = 100 + i
               val(2,naok) = fpdata(1,1,i)  ! FX
               val(3,naok) = fpdata(2,1,i)  ! FY
               val(4,naok) = fpdata(3,1,i)  ! FA
               val(5,naok) = fpdata(4,1,i)  ! FB
               val(6,naok) = fpdata(5,2,i)
               val(7,naok) = resx(i)
               val(8,naok) = resy(i)
               val(9,naok)  = atan(fpdata(3,1,i))*1000.
               val(10,naok) = atan(fpdata(4,1,i))*1000.
            ENDIF

         ELSE
            DO j = 1, 4
               fpdata(j,1,i) = -1000.
            ENDDO 
            fpdata(5,2,i) = -1000.

            naok = naok + 1
            val(1,naok) = 100 + i
            DO j = 2,10
               val(j,naok) = -1000.
            ENDDO

         ENDIF

      ENDDO

      RETURN

      END
