      SUBROUTINE Loadppacconf
      IMPLICIT NONE
      INTEGER ier, i, j
      CHARACTER*132 prmfile,confname

      INCLUDE 'numbers.fh'
      INCLUDE 'fldata.fh'
      INCLUDE 'ppacprm.fh'
      INCLUDE 'ppacconf.fh'

      INTEGER ifcs,iordr,itype,n,preifcs
      INTEGER id,nlp,nofp
      REAL    zpos,prm(4,2)

      REAL    diffzpos(6,2) ! see ppacconf.prm
      DATA    diffzpos/-3.5,-18.7,18.7,-4.1,-18.7,18.7,  ! dzx
     &                  5.1,-10.1,10.1, 4.1,-10.1,10.1/  ! dzy

      INTEGER tp2n(4),tp2a(4)
      DATA    tp2n/1,2,1,2/,tp2a/1,2,4,5/

      ier = 0

      CALL Getenv('PPAC_CONFIG',prmfile)
      OPEN (UNIT=80, FILE=prmfile, STATUS='old',ERR=1001)

      preifcs = 0
      nofp = 0
      nlp = 0
      id = 0
      maxfcs = 0
      DO i = 1, nfocus
         maxnp(i) = 0
      ENDDO

      DO WHILE (ier.EQ.0)
         CALL Skip_comment(80)
         READ(80,*,END=1000)ifcs,iordr,zpos,itype,confname
c         WRITE(*,*)ifcs,iordr,zpos,itype,confname
         nlp = tp2n(itype)
c         nlp = itype
c         IF (itype.EQ.3) nlp = 1
         n = nlp*4
         OPEN (UNIT=90, FILE=confname, STATUS='old',ERR=1001)
         CALL Read_flt(90,prm,n,ier)
         CLOSE(90)
         IF (preifcs.NE.ifcs) nofp = 0
         IF (maxnp(ifcs).NE.0) nofp = maxnp(ifcs)
         DO i = 1, nlp, 1
            nofp = nofp+1
            id = id + 1
            j = tp2a(itype) + i - 1
c            j = itype + i - 1
c            IF(itype.EQ.1) j = 1
c            IF(itype.EQ.3) j = 4
c            WRITE(*,*)'[Loadppacconf] F,order,id,itype : ',
c     &           ifcs,nofp,id,j
            ppaczpos(izx-2,id) = zpos + diffzpos(j,1)
            ppaczpos(izy-2,id) = zpos + diffzpos(j,2)
            ns2mm(ix,id) = 0.5 * prm(ix,i)
            ns2mm(iy,id) = 0.5 * prm(iy,i)
            inoffset(ix,id) = prm(ix+2,i)
            inoffset(iy,id) = prm(iy+2,i)
            ppacfcs(1,id) = ifcs
            ppacfcs(2,id) = nofp
            ppacid(nofp,ifcs) = id
            IF (nofp.EQ.1) flpla(5,ifcs) = zpos
         ENDDO
         maxnp(ifcs) = nofp
         preifcs = ifcs
         IF (preifcs.GT.maxfcs) maxfcs = preifcs
      ENDDO
      
 1000  WRITE(*,*)' ANAPAW-M : [Loadppacconf] Loaded parameters.'

      CLOSE(80)
      RETURN

 1001  WRITE(*,'(2A)')' ANAPAW-E : [Loadppacconf] Cannot open file.', 
     &     prmfile(1:Len_trim(prmfile))
      STOP
      RETURN

 1002  WRITE(*,*)
     &     ' ANAPAW-E : [Loadppacconf] Error while reading parameters.'
      CLOSE(80)
      STOP
      RETURN

      END



