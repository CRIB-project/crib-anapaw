c  2021.11 Made by HY from encgeneric

      subroutine encmssd(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata,analyzer)

      implicit none
      integer nx, ny, ndet, ndata, naok
      integer rawdata(ndata,ndet)
      integer nhitdata(ndet)
      integer hitdet(ndet)
      integer nhitdet
      integer analyzer
      real val(nx,ny)
      integer i,j,id,pos1,pos2

      real tmp,tmp2, coffset, civ, chigh
      real pos_cal1(18)
c    The last number "1000" is to detect overflow
      data pos_cal1/0.0,0.053,0.11,0.169,0.239,0.30,0.359,0.421,0.49,
     &            0.56,0.617,0.675,0.742,0.807,0.871,0.935,1.0,1000/  !For position signal calibration
      real pos_cal2(18)
      data pos_cal2/0.0,0.063,0.125,0.195,0.263,0.326,0.392,0.464,
     &            0.532,0.595,0.659,0.729,0.799,0.866,0.935,1.0,
     &            1.065,1000/  !For position signal calibration


      include 'analyslogic.fh'
      include 'ssd.inc'

      if (initencflag(analyzer)) then
         write(*,*) ' ANAPAW-M : enc_ssd analys =',analyzer
         write(*,*) ' ANAPAW-M : ndet  =',ndet
         write(*,*) ' ANAPAW-M : ndata =',ndata


         Call MSSD_PRM

         initencflag(analyzer) = .false. 
      endif

c      write(*,*) ' ANAPAW-M : analyzer =',analyzer, ' nhitdet =',nhitdet

      naok = 0

      do i=1,nhitdet
         id = hitdet(i)
c         write(*,*) 'id =',hitdet(i),' nhit =',nhitdata(id)
         naok = naok + 1
         val(1,naok) = id
         val(2,naok) = rawdata(1,id) ! Araw1
         val(3,naok) = rawdata(2,id) ! Araw2
         val(4,naok) = rawdata(3,id) ! P1
         val(5,naok) = rawdata(4,id) ! P2
         val(6,naok) = rawdata(5,id) ! Traw

c     Need a position spectrum-> ch number

         coffset = pos_offset_mssd(2*id-1)
         civ = pos_interval_mssd(2*id-1) 
         chigh = pos_highpeak_mssd(2*id-1)

         tmp = (rawdata(3,id) - coffset + 0.5*civ) /
     &           (chigh - coffset + civ)

         pos1 = 0
         do j=1, 17
            if(tmp.gt.pos_cal1(j) .and. tmp.lt.pos_cal1(j+1)) then
c               write(*, *) "tmp, tmp2, j, pos_cal1(j), pos_cal1(j+1)=",
c     &                      tmp, tmp2, j, pos_cal1(j), pos_cal1(j+1)
              pos1 = j
            endif
         end do



c   write for test
c        write(*, *)"tmp, rawdata, pos1=",tmp,rawdata(3,id),pos1
c         write(*, *)"rawdata, offset, interval, highpeak=",
c     &       rawdata(3,id),pos_offset_mssd(id),pos_interval_mssd(id),
c     &       pos_highpeak_mssd(id)

         coffset = pos_offset_mssd(2*id)
         civ = pos_interval_mssd(2*id) 
         chigh = pos_highpeak_mssd(2*id)

         tmp = (rawdata(4,id) - coffset + 0.5*civ) /
     &           (chigh - coffset + civ)


         pos2 = 0
         do j=1, 17
            if(tmp.gt.pos_cal2(j) .and. tmp.lt.pos_cal2(j+1)) then
              pos2 = j
            endif
         end do

c   write for test
c        write(*, *)"tmp, rawdata, pos2=",tmp,rawdata(4,id),pos2
c         write(*, *)"rawdata, offset, interval, highpeak=",
c     &       rawdata(4,id),pos_offset_mssd(id),pos_interval_mssd(id),
c     &       pos_highpeak_mssd(id)


         if ((pos1.ge.1).and.(pos1.le.16)) then 
         val(12,naok) = (rawdata(1,id)-Offset_a_mssd((ID-1)*16+pos1)) 
     &        * Gain_a_mssd((ID-1)*16+pos1) ! Acal
         else 
            val(12,naok) = 0
         endif

         if ((pos2.ge.1).and.(pos2.le.16)) then 
         val(13,naok) = (rawdata(2,id)-Offset_a_mssd((ID-1)*16+pos2)) 
     &        * Gain_a_mssd((ID-1)*16+pos2) ! Acal
         else 
            val(13,naok) = 0
         endif

         val(14,naok) = pos1 ! P1
         val(15,naok) = pos2 ! P2

c        write (*,*) 'pos1=',val(14,naok)

         if ((pos1.ge.1).and.(pos1.le.16)) then 
         val(16,naok) = (rawdata(5,id)-Offset_t_mssd((ID-1)*16+pos1))  
     &        * Gain_t_mssd((ID-1)*16+pos1) !Tcal pos1 only?
         else 
            val(16,naok) = 0
         endif

c         do j=1,ndata
c            val(10+j,naok) = rawdata(j,id)
c            write(*,*) id, ' : ',j,' , ',rawdata(j,id)
c         enddo
      enddo


c       val(*,101....196)        

      do i=101,196

c         write(*,*) 'id =',hitdet(i),' nhit =',nhitdata(id)
         naok = naok + 1
         val(1,naok) = i

c get postion 
         id=(i-101)/16+1
         pos1=int(val(14,id))
         pos2=int(val(15,id))

         if (((ID-1)*16+pos1).eq.i) then
            val(2,naok) = rawdata(1,id) ! Araw1
         endif
         if (((ID-1)*16+pos2).eq.i) then
            val(2,naok) = rawdata(2,id) ! Araw1
         endif
         val(6,naok) = rawdata(5,id) ! Traw


      enddo

      return
      end
