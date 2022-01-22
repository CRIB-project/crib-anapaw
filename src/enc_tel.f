c  2021.11 Made by HY from encgeneric

      subroutine enctel(val,nx,ny,naok)

      implicit none
      integer nx, ny,naok
      real val(nx,ny)
      integer id,layer
      real esum

      include 'analyslogic.fh'
      include 'tel.inc'

c      if (initencflag(analyzer)) then
c         write(*,*) ' ANAPAW-M : enc_tel analys =',analyzer
c         initencflag(analyzer) = .false. 
c      endif

c      write(*,*) ' ANAPAW-M : analyzer =',analyzer, ' nhitdet =',nhitdet

      naok = 0

c 1...id, 2...esum
c 11....1x energy for each layer 
c 21....2x timing for each layer 

      do id=1,n_tel
         naok = naok + 1
         val(1,naok) = id
         esum = 0
         
         do layer=1,n_layer
c            write(*,*) "id,layer=",id,layer
c            write(*,*) "etel(id,layer)=",etel(id,layer)
            val(layer+10,naok) = etel(id,layer) ! energy (MeV)
            val(layer+20,naok) = etel(id,layer) ! timing (ns)
c            if (etel(id,layer).gt.0.050)   ! such code may be needed to supress pedestal
            if (etel(id,layer).gt.0.001) then  
               esum = esum + etel(id,layer)
            endif

         enddo

c         write(*,*) "esum=",esum
         val(2,naok) = esum ! total energy

      enddo


      return
      end

