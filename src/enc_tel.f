c  2021.11 Made by HY from encgeneric

      subroutine enctel(val,nx,ny,naok)

      implicit none
      integer nx, ny,naok
      real val(nx,ny)
      integer id,layer
      real esum, de12, e12

      include 'analyslogic.fh'
      include 'tel.inc'

c      if (initencflag(analyzer)) then
c         write(*,*) ' ANAPAW-M : enc_tel analys =',analyzer
c         initencflag(analyzer) = .false. 
c      endif

c      write(*,*) ' ANAPAW-M : analyzer =',analyzer, ' nhitdet =',nhitdet

      naok = 0

c 1...id, 2...esum, 3...de1+de2, 4...e1+e2
c 11....1x energy for each layer 
c 21....2x timing for each layer 

c For Tel1-3 
      do id=1,3 !n_tel
         naok = naok + 1
         val(1,naok) = id
         de12 = 0
         e12 = 0
         esum = 0
         
         do layer=1,2 ! dE1 + dE2
c            write(*,*) "id,layer=",id,layer
c            write(*,*) "etel(id,layer)=",etel(id,layer)
            val(layer+10,naok) = etel(id,layer) ! energy (MeV)
            val(layer+20,naok) = ttel(id,layer) ! timing (ns)
c            if (etel(id,layer).gt.0.050)   ! such code may be needed to supress pedestal
            if (etel(id,layer).gt.0.001) then  
               de12 = de12 + etel(id,layer)
            endif
         enddo
         do layer=3,n_layer ! E1 + E2
c            write(*,*) "id,layer=",id,layer
c            write(*,*) "etel(id,layer)=",etel(id,layer)
            val(layer+10,naok) = etel(id,layer) ! energy (MeV)
            val(layer+20,naok) = ttel(id,layer) ! timing (ns)
c            if (etel(id,layer).gt.0.050)   ! such code may be needed to supress pedestal
            if (etel(id,layer).gt.0.001) then  
               e12 = e12 + etel(id,layer)
            endif
         enddo
         esum = de12 + e12

c         write(*,*) "esum=",esum
         val(2,naok) = esum ! total energy
         val(3,naok) = de12 ! dE1+dE2
         val(4,naok) = e12 ! E1+E2

      enddo

cc
c For Tel4-5 (Without dE1 layer)
      do id=4,n_tel
         naok = naok + 1
         val(1,naok) = id
         de12 = 0
         e12 = 0
         esum = 0
         
         val(11,naok) = 0. ! energy (MeV)
         val(21,naok) = ttel(id,1) ! timing (ns) (temporarily dE2 X timing)

         layer=2 ! dY2
         val(layer+10,naok) = etel(id,layer) ! energy (MeV)
         val(layer+20,naok) = ttel(id,layer) ! timing (ns)
         if (etel(id,layer).gt.0.001) then  
            de12 = de12 + etel(id,layer)
         endif
         do layer=3,n_layer ! (E1 + E2)
c            write(*,*) "id,layer=",id,layer
c            write(*,*) "etel(id,layer)=",etel(id,layer)
            val(layer+10,naok) = etel(id,layer) ! energy (MeV)
            val(layer+20,naok) = ttel(id,layer) ! timing (ns)
c            if (etel(id,layer).gt.0.050)   ! such code may be needed to supress pedestal
            if (etel(id,layer).gt.0.001) then  
               e12 = e12 + etel(id,layer)
            endif
         enddo
         esum = de12 + e12

c         write(*,*) "esum=",esum
         val(2,naok) = esum ! total energy
         val(3,naok) = de12 ! dE2
         val(4,naok) = e12 ! E1+E2
      enddo



      return
      end

