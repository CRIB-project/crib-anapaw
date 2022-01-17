c  2021.11 Made by HY from encgeneric

      subroutine encssd(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata,analyzer)

      implicit none
      integer nx, ny, ndet, ndata, naok
      integer rawdata(ndata,ndet)
      integer nhitdata(ndet)
      integer hitdet(ndet)
      integer nhitdet
      integer analyzer
      real val(nx,ny)
      integer i,j,id

      include 'analyslogic.fh'
      include 'ssd.inc'

      if (initencflag(analyzer)) then
         write(*,*) ' ANAPAW-M : enc_ssd analys =',analyzer
         write(*,*) ' ANAPAW-M : ndet  =',ndet
         write(*,*) ' ANAPAW-M : ndata =',ndata

         Call SSD_PRM

         initencflag(analyzer) = .false. 
      endif

c      write(*,*) ' ANAPAW-M : analyzer =',analyzer, ' nhitdet =',nhitdet

      naok = 0

      do i=1,nhitdet
         id = hitdet(i)
c         write(*,*) 'id =',hitdet(i),' nhit =',nhitdata(id)
         naok = naok + 1
         val(1,naok) = id
         val(2,naok) = rawdata(1,id) ! Araw
         val(3,naok) = rawdata(2,id) ! Traw
         val(4,naok) = (rawdata(1,id)-Offset_a_dssd(ID)) 
     &        * Gain_a_dssd(ID) ! Acal
         val(5,naok) = (rawdata(2,id)-Offset_t_dssd(ID))  
     &        * Gain_t_dssd(ID) !Tcal

c         do j=1,ndata
c            val(10+j,naok) = rawdata(j,id)
c            write(*,*) id, ' : ',j,' , ',rawdata(j,id)
c         enddo
      enddo

      return
      end

