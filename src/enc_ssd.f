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
      integer i,id

      include 'analyslogic.fh'
      include 'ssd.inc'
      include 'tel.inc'

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
c         write(*,*) 'id =',hitdet(i),' nhitdet =',nhitdet
         naok = id          
c         naok = naok + 1
         val(1,naok) = id
         val(2,naok) = rawdata(1,id) ! Araw
         val(3,naok) = rawdata(2,id) ! Traw
         val(4,naok) = (rawdata(1,id)-Offset_a_ssd(ID)) 
     &        * Gain_a_ssd(ID) ! Acal
         val(5,naok) = (rawdata(2,id)-Offset_t_ssd(ID))  
     &        * Gain_t_ssd(ID) !Tcal

c         write(*,*) 'naok, val(4,naok)=',naok,val(4,naok)

      enddo
      naok = nhitdet

c      write(*,*) 'val(4,1),val(4,2)=',val(4,1),val(4,2)

c telescope; assuming naok = id


         etel(1,3) =  val(4,1) ! means tel 1 3rd layer is Acal for id=1 ssd
         etel(1,4) =  val(4,2)
         etel(2,3) =  val(4,3)
         etel(2,4) =  val(4,4)
         etel(3,3) =  val(4,5)
         etel(3,4) =  val(4,6)
         etel(4,3) =  val(4,7)
         etel(4,4) =  val(4,8)
         etel(5,3) =  val(4,9)
         etel(5,4) =  val(4,10)

c timings
         ttel(1,3) =  val(5,1) 
         ttel(1,4) =  val(5,2)
         ttel(2,3) =  val(5,3)
         ttel(2,4) =  val(5,4)
         ttel(3,3) =  val(5,5)
         ttel(3,4) =  val(5,6)
         ttel(4,3) =  val(5,7)
         ttel(4,4) =  val(5,8)
         ttel(5,3) =  val(5,9)
         ttel(5,4) =  val(5,10)


      return
      end

