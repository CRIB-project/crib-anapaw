c  2021.11 Made by HY from encgeneric

      subroutine encrf(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata,analyzer)

      implicit none
      integer nx, ny, ndet, ndata, naok
      integer rawdata(ndata,ndet)
      integer nhitdata(ndet)
      integer hitdet(ndet)
      integer nhitdet
      integer analyzer
      real val(nx,ny)
      real calib_fac
      integer i,id

      include 'analyslogic.fh'
      include 'rf.inc'

      if (initencflag(analyzer)) then
         write(*,*) ' ANAPAW-M : enc_ssd analys =',analyzer
         write(*,*) ' ANAPAW-M : ndet  =',ndet
         write(*,*) ' ANAPAW-M : ndata =',ndata
         initencflag(analyzer) = .false. 
      endif

c      write(*,*) ' ANAPAW-M : analyzer =',analyzer, ' nhitdet =',nhitdet

      naok = 0

      calib_fac=0.122
      
      do i=1,nhitdet
         id = hitdet(i)
c         write(*,*) 'id =',hitdet(i),' nhit =',nhitdata(id)
         naok = naok + 1
         val(1,naok) = id
         val(2,naok) = rawdata(1,id)*calib_fac ! RF1
         val(3,naok) = rawdata(2,id)*calib_fac ! RF2
         val(4,naok) = rawdata(1,id) ! RF1raw
         val(5,naok) = rawdata(2,id) ! RF2raw

         trf1 = val(2,naok)
         trf2 = val(3,naok)
      enddo

      

      return
      end

