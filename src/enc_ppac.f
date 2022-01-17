c  2021.12 Made by HY from encgeneric & old enc_ppac.f

      subroutine encppac(val,nx,ny,naok,
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

      Real     TXsum(5),TYsum(5),dTX(5),dTY(5)
      Real     X(5),Y(5),dx(5),dy(5),xx,yy
c
      integer sid(4) 
      Data sid/3,2,2,1/  !reorder id F3A, F3B, F2, F1 


      real     dummy
      parameter (dummy = -8192.)

      integer count
      real ax, ax2, sx, adx, adx2, sdx, axdx, zx
      real ay, ay2, sy, ady, ady2, sdy, aydy, zy
      common /waistcommon/ count, ax, ax2, adx, adx2, axdx,
     1                            ay, ay2, ady, ady2, aydy
c
      integer piflag
      common /picommon/ piflag



      include 'analyslogic.fh'
      include 'fortree.fh'
      include 'rf.inc'
      Include 'ppac.inc'

      if (initencflag(analyzer)) then
         write(*,*) ' ANAPAW-M : enc_ssd analys =',analyzer
         write(*,*) ' ANAPAW-M : ndet  =',ndet
         write(*,*) ' ANAPAW-M : ndata =',ndata

         Call PPAC_PRM

         initencflag(analyzer) = .false. 

         write(6,*) '**** Gain PPAC'
         write(6,*) Gain_PPAC
         write(6,*) '**** Pos gain PPAC'
         write(6,*) Posgain
         write(6,*) '**** Offset PPAC'
         write(6,*) Off_PPAC
         write(6,*) '**** Offset geom'
         write(6,*) offset_geom
         write(6,*) '**** Zpos'
         write(6,*) Zpos
         write(6,*) '**** pcubx'
         write(6,*) pcubx
         write(6,*) '**** pcuby'
         write(6,*) pcuby
c         write(6,*) !Ytmp reflected!
c
         count = 0
         ax = 0.
         ax2 = 0.
         adx = 0.
         adx2 = 0.
         axdx = 0.
         ay = 0.
         ay2 = 0.
         ady = 0.
         ady2 = 0.
         aydy = 0.



      endif

c      write(*,*) ' ANAPAW-M : analyzer =',analyzer, ' nhitdet =',nhitdet

      naok = 0


c      Subroutine Enc_PPAC(Raw,nw,val,nx,ny,naok)
c
c                                             2000.Sep.25 S.Takeuchi
c                                             2002.May    T.Teranishi
c                                             2002.Dec.13 H.Baba
c                                             2007.Jul    H.Yamaguchi for MCP 
c ===========================================================================
c
c Input:
c               id=1-3
c               1: F1 PPAC
c               2: F2 PPAC/F3 PPACb
c               3: F3 PPACa
c               rawdata(1,id)    PPAC X1
c               rawdata(2,id)    PPAC X2
c               rawdata(3,id)    PPAC Y1
c               rawdata(4,id)    PPAC Y2
c               rawdata(5,id)    PPAC A  (except for F1 PPAC)
c
c
c Output:
c
c   ID=1        PPAC_A@F3
c      2        PPAC_B@F3
c      3        PPAC  @F2
c      4        MCP   (not yet) 
c      5        MCP(VME) (not yet)   
c
c   W#: 1      2      3      4      5      6      7      8      9     10
c       id     Traw   Eraw          X1raw  X2raw  Y1raw  Y2raw  
c
c   W#:11     12     13     14     15     16     17     18     19     20
c              Tcal   Ecal          X1cal  X2cal  Y1cal  Y2cal
c
c   W#:21     22     23     24     25     26     27     28     29     30
c              Xcal   Ycal   X(mm)  Y(mm)  TXsum  TYsum  <Tx>   <Ty>
c
c   W#:31     32     33     34     35     36     37     38     39     40
c            <Tx>   <Ty>
c             -Tcal  -Tcal 
c
c
c  ID=10,20 : Position on Target 10...PPAC A/B  20...PPACA/MCP
c
c     W#: 1      2      3      4        5        6      7      8
c         id     X(mm)  Y(mm)  X'(mrad) Y'(mrad) R(mm)  XX'    YY'
c
c  ID=11 : Waist information
c     W#: 1      2      3
c         id     Zx(mm) Zy(mm)
c
c  ID=100, 110 : TOF(PPACa-b), TOF(PPACa-MCP) 
c     W#: 1      2                    3      (Sep, 2002)  4 (Sep, 2002)
c         id     TB-TA                RF1-Tx (Sep, 2002)  RF1-Ty (Sep, 2002)
c
c      implicit none
c
c      integer*2 Raw(15)  ! can be used upto raw(10) when nw=10
c
c      integer   nw,nx,ny,ID,naok
c      Real      val(nx,ny)

c
c  Do not EDIT!!
c      Logical   AnalyserFlag(50),INITENCFLAG(50)
c      Logical   USERFLAG(10),EVTERR
c      Common/ANALYSLOGIC/ AnalyserFlag,INITENCFLAG,USERFLAG,EVTERR 
c  Do not EDIT!!
c

c
c Position of PPAC_A,PPAC_B,Target,SSD,MCP,MCP in meter.
c      Real     ZPOS(-1:4)
c      Data     ZPOS/0.0,0.861,0.0,0.4305,0.861,0.861/
c
c  for waist analysis
c
c ID = 1,2,3,4 : PPAC_a,PPAC_b,PPAC_F2, F1 PPAC

cccccccccc
      do ID = 1,4
         val(1,ID) = ID

         val(2,ID) = rawdata(5,sid(id)) ! T  raw
         val(3,ID) = dummy      ! QDC is not used
         
         val(5,ID) = rawdata(1,sid(id)) ! X1 raw
         val(6,ID) = rawdata(2,sid(id)) ! X2 raw
         val(7,ID) = rawdata(3,sid(id)) ! Y1 raw
         val(8,ID) = rawdata(4,sid(id)) ! Y2 raw

c         write(*,*) "ID,sid,X1",ID, sid(ID),val(5,ID)
c         write(*,*) "ID,sid,Y2",ID, sid(ID),val(8,ID)
c     
ccccccccccccc
         If(val(2,ID).gt.0. .and. val(2,ID).lt.10000.) Then
            val(12,ID) = val(2,ID) * TGain_PPAC(ID) + TOff_PPAC(ID) ! T  cal
         Else
            val(12,ID) = dummy
         EndIf

         val(13,ID) = val(3,ID)                                  ! A  cal

         If(val(5,ID).gt.0. .and. val(5,ID).lt.10000.) Then
            val(15,ID) = val(5,ID) * Gain_PPAC(1,ID)
     &           + Off_PPAC(1,ID)                                ! X1 cal
         Else
            val(15,ID) = dummy
         EndIf
         If(val(6,ID).gt.0. .and. val(6,ID).lt.10000.) Then
            val(16,ID) = val(6,ID) * Gain_PPAC(2,ID)
     &           + Off_PPAC(2,ID)                                ! X2 cal
         Else
            val(16,ID) = dummy
         EndIf
         If(val(7,ID).gt.0. .and. val(7,ID).lt.10000.) Then
            val(17,ID) = val(7,ID) * Gain_PPAC(3,ID)
     &           + Off_PPAC(3,ID)                                ! Y1 cal
         Else
            val(17,ID) = dummy
         EndIf
         If(val(8,ID).gt.0. .and. val(8,ID).lt.10000.) Then
            val(18,ID) = val(8,ID) * Gain_PPAC(4,ID)
     &           + Off_PPAC(4,ID)                                ! Y2 cal
         Else
            val(18,ID) = dummy
         EndIf

         
         If(val(15,ID).gt.0. .and. val(16,ID).gt.0.) Then
            TXsum(ID)  = val(15,ID) + val(16,ID)
            dTX(ID)    = val(15,ID) - val(16,ID)
c         write(6,*) TXsum(4)
         Else
            TXsum(ID)  = dummy
            dTX(ID)    = dummy
         EndIf

         If(val(17,ID).gt.0. .and. val(18,ID).gt.0.) Then
            TYsum(ID)  = val(17,ID) + val(18,ID)
            dTY(ID)    = val(17,ID) - val(18,ID)
         Else
            TYsum(ID)  = dummy
            dTY(ID)    = dummy
         EndIf

         if(iflag_position_offset.eq.1) then
            if (dTx(id).ne.dummy)
     +        dTX(id) = dTX(id) - offset_x(id)
            if (dTy(id).ne.dummy)
     +        dTY(id) = dTY(id) - offset_y(id) 
         endif

         if(iflag_position_offset_line.eq.1) then
            if (dTx(id).ne.dummy)
     +        dTX(id) = dTX(id) - offset_line_x(id)
            if (dTy(id).ne.dummy)
     +        dTY(id) = dTY(id) - offset_line_y(id)
         endif

         val(22,ID) = dTX(ID)             ! Xcal
         val(23,ID) = dTY(ID)             ! Ycal

         if(  val(5,ID).gt.0.and.val(5,ID).lt.10000.and.
     &        val(6,ID).gt.0.and.val(6,ID).lt.10000.)then
            X(ID) = PosGain(1,ID) * val(22,ID) ! X(mm)
            if(iflag_geom_position_offset.eq.1)
     &           X(ID)=X(ID)-offset_geom(1,ID) ! X(mm)
            if(iflag_cath_position_offset.eq.1)
     &           X(ID)=X(ID)-offset_cath(1,ID) ! X(mm)
         else
            X(ID) = dummy
         endif

         if(  val(7,ID).gt.0.and.val(7,ID).lt.10000.and.
     &        val(8,ID).gt.0.and.val(8,ID).lt.10000.) then
            Y(ID) = PosGain(2,ID) * val(23,ID) ! Y(mm)
            if(iflag_geom_position_offset.eq.1)
     &           Y(ID)=Y(ID)-offset_geom(2,ID) ! Y(mm)
            if(iflag_cath_position_offset.eq.1)
     &           Y(ID)=Y(ID)-offset_cath(2,ID) ! Y(mm)
         else
            Y(ID) = dummy
         endif
c
         if(iflag_reverse_axis.eq.1) then
            X(ID) = X(ID)*reverse_axis(1,ID)
            Y(ID) = Y(ID)*reverse_axis(2,ID)
         endif
         
         val(24,ID) = X(ID)
         val(25,ID) = Y(ID)
c
         val(26,ID) = TXsum(ID)
         val(27,ID) = TYsum(ID)
         if (TXsum(ID).ne.dummy) then
           val(28,ID) = 0.5*TXsum(ID)
           val(39,ID) = val(28,ID) - val(12,ID)
         else
           val(28,ID) = dummy
           val(39,ID) = dummy
         endif
         if (TYsum(ID).ne.dummy) then
           val(29,ID) = 0.5*TYsum(ID)
           val(40,ID) = val(29,ID) - val(12,ID)
         else
           val(29,ID) = dummy
           val(40,ID) = dummy
         endif
         if(val(28,ID).ne.dummy .and. val(29,ID).ne.dummy) then
            val(30,ID) = 0.5*(val(28,ID)+val(29,ID))
         else
            val(30,ID) = dummy
         endif

         if (val(28,ID).ne.dummy .and. val(12,ID).ne.dummy) then
           val(32,ID) = val(28,ID)-val(12,ID)   ! <Tx>-Tcal
         else
           val(32,ID) = dummy
         endif
         if (val(29,ID).ne.dummy .and. val(12,ID).ne.dummy) then
           val(33,ID) = val(29,ID)-val(12,ID)   ! <Ty>-Tcal
         else
           val(33,ID) = dummy
         endif
c
         if(iflag_Pileup_T0_PPAC.eq.1) then
           if (val(32,ID).ne.dummy)
     +       val(32,ID) = val(32,ID) - Pileup_T0_PPAC(1,ID)
           if (val(33,ID).ne.dummy)
     +       val(33,ID) = val(33,ID) - Pileup_T0_PPAC(2,ID)
         endif
c
      enddo

cc For MCP calibration with cubic functions
c$$$      ID = 4
c$$$      if(X(ID).ne.dummy .and. Y(ID).ne.dummy) then
c$$$         val(34,ID) = pcubx(1) 
c$$$     &    + pcubx(2)*X(ID) + pcubx(3)*Y(ID) 
c$$$     &    + pcubx(4)*X(ID)**2 + pcubx(5)*Y(ID)**2 + pcubx(6)*X(ID)*Y(ID)
c$$$     &    + pcubx(7)*X(ID)**3 + pcubx(8)*Y(ID)**3
c$$$     &    + pcubx(9)*X(ID)**2*Y(ID) + pcubx(10)*X(ID)*Y(ID)**2 ! X(mm)
c$$$         val(35,ID) = pcuby(1) 
c$$$     &    + pcuby(2)*X(ID) + pcuby(3)*Y(ID) 
c$$$     &    + pcuby(4)*X(ID)**2 + pcuby(5)*Y(ID)**2 + pcuby(6)*X(ID)*Y(ID)
c$$$     &    + pcuby(7)*X(ID)**3 + pcuby(8)*Y(ID)**3
c$$$     &    + pcuby(9)*X(ID)**2*Y(ID) + pcuby(10)*X(ID)*Y(ID)**2 ! X(mm)
c$$$      else
c$$$         val(34,ID) = dummy
c$$$         val(35,ID) = dummy
c$$$      endif

      naok=5
c TOF
     
      val(1,100) = 100
      if(val(30,2).ne.dummy .and.val(30,1).ne.dummy) then
         val(2,100) = val(30,2)-val(30,1)
      else
         val(2,100) = dummy
      endif
      
      val(1,110) = 110
c      if(val(2,4).ne.dummy .and.val(2,1).ne.dummy) then
c         val(2,110) = val(2,4)-val(2,1)
      if(val(30,4).ne.dummy .and.val(30,1).ne.dummy) then
         val(2,110) = val(30,4)-val(30,1)
c         write(6,*) val(2,110)
      else
         val(2,110) = dummy
      endif

c     PPACa-b extrapolation 
      if (X(1).ne.dummy .and. X(2).ne.dummy) then
        dX(1) = (X(2) - X(1))/(ZPOS(-2) - ZPOS(-3))
      else
        dX(1) = dummy
      endif
      if (Y(1).ne.dummy .and. Y(2).ne.dummy) then
        dY(1) = (Y(2) - Y(1))/(ZPOS(-2) - ZPOS(-3))
      else
        dY(1) = dummy
      endif

c     PPACa-MCP extrapolation   
      if (X(1).ne.dummy .and. X(4).ne.dummy) then
        dX(2) = (X(4) - X(1))/(ZPOS(0) - ZPOS(-3))
      else
        dX(2) = dummy
      endif
      if (Y(1).ne.dummy .and. Y(4).ne.dummy) then
        dY(2) = (Y(4) - Y(1))/(ZPOS(0) - ZPOS(-3))
      else
        dY(2) = dummy
      endif
c     PPACa-MCP_VME extrapolation   
      if (X(1).ne.dummy .and. X(5).ne.dummy) then
        dX(3) = (X(5) - X(1))/(ZPOS(0) - ZPOS(-3))
      else
        dX(3) = dummy
      endif
      if (Y(1).ne.dummy .and. Y(4).ne.dummy) then
        dY(3) = (Y(5) - Y(1))/(ZPOS(0) - ZPOS(-3))
      else
        dY(3) = dummy
      endif
c
      if ((trf1.ne.dummy).and.(val(28,1).ne.dummy)) then
        val(3,100) = trf1-val(28,1) + 62.0
      else
        val(3,100) = dummy
      endif
c
      if ((trf1.ne.dummy).and.(val(29,1).ne.dummy)) then
        val(4,100) = trf1-val(29,1) + 62.0
      else
        val(4,100) = dummy
      endif
c
c
c      Do i=1, n_layer-n_ppac
c    i...PPACb or MCP    
       Do i=1, 3   
         ID = i*10
         if (dX(i).ne.dummy) then
           xx = X(1) + dX(i) * (ZPOS(1) - ZPOS(-3))
c         xx = X(1) + dX(i) * (ZPOS(1) - ZPOS(-3))-6.25 ! j.Y.MOON
         else
           xx = dummy
         endif
c         if (i.eq.1) write(6,*) xx, x(1), dx(1), zpos(1), zpos(-3)
         if (dY(i).ne.dummy) then
c           yy = Y(1) + dY(i) * (ZPOS(1) - ZPOS(-3))
         yy = Y(1) + dY(i) * (ZPOS(1) - ZPOS(-3)) 
         else
           yy = dummy
         endif
         val(1,ID) = ID
         val(2,ID) = XX
         val(3,ID) = YY
         val(4,ID) = dX(i)
         val(5,ID) = dY(i)
         if (xx.ne.dummy .and. yy.ne.dummy) then
           val(6,ID) = sqrt(XX**2+YY**2)
         else
           val(6,ID) = dummy
         endif
         if (dx(i).ne.dummy) then
           val(7,ID) = XX * dX(i)
         else
           val(7,ID) = dummy
         endif
         if (dy(i).ne.dummy) then
           val(8,ID) = YY * dY(i)
         else
           val(8,ID) = dummy
         endif
      EndDo
c
c 2010/6/20 S.M
      ID=40
c extrapolation to the beam stopper
      xx = x(1) + dx(1) * 1.
      yy = y(1) + dy(1) * 1.

      val(1,ID) = ID
      val(2,ID) = xx
      val(3,ID) = yy

c      write(*,*) 'F3a',X(1),Y(1),ZPOS(-3)
c      write(*,*) 'F3b',X(2),Y(2),ZPOS(-2)

c  waist position
      if ((dx(1).ne.dummy.and.dy(1).ne.dummy).and.
     1    (piflag.ne.0)) then
c     1    (abs(dx).le.20).and.(abs(x(1)).le.10)) then
        count = count + 1
c        write(6,*) count, '10C!'
c        write(6,*) count
c        write(6,*) '------'
c        write(6,*) x(1),x(2),dx
c        write(6,*) dx, dy
c
        ax = ax + x(2)
        ax2 = ax2 + x(2)**2
        adx = adx + dx(1)
        adx2 = adx2 + dx(1)**2
        axdx = axdx + x(2)*dx(1)
        ay = ay + y(2)
        ay2 = ay2 + y(2)**2
        ady = ady + dy(1)
        ady2 = ady2 + dy(1)**2
        aydy = aydy + y(2)*dy(1)
        if (count.eq.1000) then
          ax = ax / 1000.
          ax2 = ax2 / 1000.
          sx = sqrt(ax2-ax**2)
          adx = adx / 1000.
          adx2 = adx2 / 1000.
          sdx = sqrt(adx2-adx**2)
          axdx = axdx / 1000.
          zx = -(axdx-ax*adx)/sdx**2
c         write(6,*) zx, ax, sdx, adx, axdx

          ay = ay / 1000.
          ay2 = ay2 / 1000.
          sy = sqrt(ay2-ay**2)
          ady = ady / 1000.
          ady2 = ady2 / 1000.
          sdy = sqrt(ady2-ady**2)
          aydy = aydy / 1000.
          zy = -(aydy-ay*ady)/sdy**2
c
          count = 0
          ax = 0.
          ax2 = 0.
          adx = 0.
          adx2 = 0.
          axdx = 0.
          ay = 0.
          ay2 = 0.
          ady = 0.
          ady2 = 0.
          aydy = 0.
c
        else
          zx = dummy
          zy = dummy
        endif
      else
        zx = dummy
        zy = dummy
      endif
      ID = 11
      val(1,ID) = ID
      val(2,ID) = zx
      val(3,ID) = zy
c

      naok = 110
      return
c
      end
