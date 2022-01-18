c  2021.11 Made by HY from encgeneric

      subroutine encdssd(val,nx,ny,naok,
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
      include 'tel.inc'

      if (initencflag(analyzer)) then
         write(*,*) ' ANAPAW-M : enc_ssd analys =',analyzer
         write(*,*) ' ANAPAW-M : ndet  =',ndet
         write(*,*) ' ANAPAW-M : ndata =',ndata

         Call DSSD_PRM

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


c         Write(*,*) rawdata(2,id)
c         Write(*,*) val(5,naok)

c         do j=1,ndata
c            val(10+j,naok) = rawdata(j,id)
c            write(*,*) id, ' : ',j,' , ',rawdata(j,id)
c         enddo
      enddo

      call getmaxstrip(val,nx,ny, 1,16, 101) ! Tel1-1
      call getmaxstrip(val,nx,ny,17,32, 102) ! Tel1-2
      call getmaxstrip(val,nx,ny,33,48, 103) ! Tel2-2
      call getmaxstrip(val,nx,ny,49,64, 104) ! Tel3-2

      naok=104

      etel(1,1) =  val(5,101)     ! means tel 1 1rd layer is Emax for the first dssd
      etel(1,2) =  val(5,102)
      etel(2,2) =  val(5,103)
      etel(3,2) =  val(5,104)


      return
      end



      subroutine getmaxstrip(val,nx,ny,I1,I2, IDOUT)
      implicit none
c      include 'psd.inc'
c      include 'rf.inc'
c  Do not EDIT!!
c      Logical   AnalyserFlag(50),INITENCFLAG(50)
c      Logical   USERFLAG(10),EVTERR
c      Common/ANALYSLOGIC/ AnalyserFlag,INITENCFLAG,USERFLAG,EVTERR 
c  Do not EDIT!! 
c
      integer nx,ny
      integer I1  ! ID start for ADC
      integer I2  ! ID end   for ADC
      integer IDOUT ! ID for output (101--105)
      real val(nx,ny)
c
      real Amax, Emax, Tmax, dummy
      real Arawmax, Trawmax
      integer i, IDmax
      parameter (dummy=-10000.)
c
      Amax = dummy
      Emax = dummy
      Tmax = dummy
      IDmax = 0
c
      do i=I1,I2
        if (val(4,i).gt.Amax) then
           Amax = val(4,i)
           Emax = val(4,i) !* gain_common(IDOUT-150)
           IDmax = i
           Tmax = val(5,i)
           Arawmax = val(2,i)
           Trawmax = val(3,i)
        endif  
      enddo
c
      val(1,IDOUT) = IDOUT
      val(2,IDOUT) = IDmax
      val(3,IDOUT) = Amax
      val(4,IDOUT) = Tmax
      val(5,IDOUT) = Emax
c      val(6,IDOUT) = 1000.
      val(6,IDOUT) = Trawmax
      val(7,IDOUT) = Arawmax
c      if (AnalyserFlag(1)) then
c        if (trf1.gt.0.) val(6,IDOUT) = Tmax - trf1
c        if (trf2.gt.0.) val(7,IDOUT) = Tmax - trf2
c      endif
c
      return
      end
