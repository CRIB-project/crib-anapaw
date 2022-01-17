c =========================================================================
      Subroutine NTBOOK

c -- from ANALYS Source !! Do Not Edit !! ------------------------------- c
      Logical  AnalyzerFlag(50),INITENCFLAG(50)
      Logical  USERFLAG(10),EVTERR,analyslist(50)
      Common/ANALYSLOGIC/  AnalyzerFlag,INITENCFLAG,
     &                     USERFLAG,EVTERR,analyslist
c ----------------------------------------------------------------------- c

c -- for ntuple defined by user ----------------------------------------- c

c -- Analyzer 1
c      Integer   RunNum
c      Integer   Coin
c      Common/CoinReg/ RunNum,Coin
c -- Analyzer 1
      Integer   AnaRun2
      Integer nppac
      parameter (nppac=35)
      Real TX1(nppac),TX2(nppac),TY1(nppac),TY2(nppac)
      Real AX1(nppac),AX2(nppac),AY1(nppac),AY2(nppac)
      Real TA(nppac),AA(nppac), X(nppac),Y(nppac)
      Common/PPAC/TX1,TX2,TY1,TY2,AX1,AX2,AY1,AY2,
     &     TA,AA,X,Y
      Integer nfpl
      parameter (nfpl=11)
      Real fx(nfpl),fa(nfpl),fy(nfpl),fb(nfpl)
      common/FPL/fx,fa,fy,fb
      

c -- Analyzer 2
c      Integer   AnaRun1
c      Real      F2Tl,F2Tr,F3Tl,F3Tr,RF1,RF2,
c     &          F2Al,F2Ar,F3Al,F3Ar,SSDA
c      Common/BeamLine/ AnaRun1,F2TL,F2TR,F3TL,F3TR,
c     &                 RF1,RF2,F2AL,F2AR,F3AL,F3AR,SSDA
      Real PLTL(8),PLTR(8),PLAL(8),PLAR(8)
      common/PLASTIC/PLTL,PLTR,PLAL,PLAR
      Real F7IC(6),FEIC(6)
      common/IC/F7IC,FEIC

      Real NAIE
      common/NAI/NAIE
      
c == Booking Part ==

c      If(AnalyzerFlag(1)) Then
c         Call HBNAME(10, 'CoinReg', RunNum,'RunNum:I,Coin:I')
c      EndIf
      Call HBNAME(10, 'PPAC', TX1(1),
     &     'TX1(35):R,TX2(35):R,TY1(35):R,TY2(35):R,' //
     &     'AX1(35):R,AX2(35):R,AY1(35):R,AY2(35):R,' //
     &     'TA(35):R,AA(35):R,X(35):R,Y(35):R')
      Call HBNAME(10, 'FPL', FX(1),
     &     'FX(11):R,FA(11):R,FY(11):R,FB(11):R')
cc      If(AnalyzerFlag(2)) Then
c      Call HBNAME(10, 'PLASTIC',PLTL(1),
c     &        'PLTL(8):R,PLTR(8):R, PLAL(8):R,PLAR(8):R')
c      Call HBNAME(10, 'IC',F7IC(1),'F7IC(6):R,FEIC(6):R')
c      Call HBNAME(10, 'NAI', NAIE, 'NAIE:R')
c      EndIf
c      If(AnalyzerFlag(3)) Then
c         Call HBNAME(10, 'PPAC', AnaRun2,
c     &        'AnaRun2:I,P1X1,P1X2,P1Y1,P1Y2,P2X1,P2X2,P2Y1,P2Y2, '//
c     &        'PAT, PBT, PAA, PBA')
c      EndIf

      Return

      End

c =========================================================================
      Subroutine Add_Ntuple(IOFLAG)

c -- for Local ---------------------------------------------------------- c
      Logical  IOFLAG   ! ( True : Next File )

c -- from ANALYS Source !! Do Not Edit !! ------------------------------- c
      Real     val(500,500,50)
      Integer  naok(50)
      Integer  RunNumber
      Logical  AnalyzerFlag(50),INITENCFLAG(50)
      Logical  USERFLAG(10),EVTERR,analyslist(50)
      Common/ANALYSVALUE/  val,naok
      Common/AnalysNtuple/ NTUPLE_FIRST,RunNumber
      Common/ANALYSLOGIC/  AnalyzerFlag,INITENCFLAG,
     &                     USERFLAG,EVTERR,analyslist
c ----------------------------------------------------------------------- c

c -- for ntuple defined by user ----------------------------------------- c

c -- Analyzer 1
c      Integer   RunNum
c      Integer   Coin
c      Common/CoinReg/ RunNum,Coin
      Integer ipc,idet
      Integer nppac
      parameter (nppac=35)
      Real TX1(nppac),TX2(nppac),TY1(nppac),TY2(nppac)
      Real AX1(nppac),AX2(nppac),AY1(nppac),AY2(nppac)
      Real TA(nppac),AA(nppac), X(nppac),Y(nppac)
      Common/PPAC/TX1,TX2,TY1,TY2,AX1,AX2,AY1,AY2,
     &     TA,AA,X,Y
      Integer nfpl
      parameter (nfpl=11)
      Real fx(nfpl),fa(nfpl),fy(nfpl),fb(nfpl)
      common/FPL/fx,fa,fy,fb
c
cc -- Analyzer 2
cc      Integer   AnaRun1
cc      Real      F2Tl,F2Tr,F3Tl,F3Tr,RF1,RF2,
cc     &          F2Al,F2Ar,F3Al,F3Ar,SSDA
cc      Common/BeamLine/ AnaRun1,F2TL,F2TR,F3TL,F3TR,
cc     &                 RF1,RF2,F2AL,F2AR,F3AL,F3AR,SSDA
c      Real PLTL(8),PLTR(8),PLAL(8),PLAR(8)
c      common/PLASTIC/PLTL,PLTR,PLAL,PLAR
c      Real F7IC(6),FEIC(6)
c      common/IC/F7IC,FEIC
c      Real NAIE
c      common/NAI/NAIE

c -- Analyzer 3

c ----------------------------------------------------------------------- c

      If(IOFLAG)Then                  !! Do Not Edit !!
         Call Ntuple_io(0)            !! Do Not Edit !!
         Call Ntuple_io(1)            !! Do Not Edit !!
      EndIf                           !! Do Not Edit !!

c ----------------------------------------------------------------------- c
c               Definition of val(Word,ID,Analyzer)                       c
c ----------------------------------------------------------------------- c

c -- Analyzer 1
c      If(AnalyzerFlag(1)) Then

      do i=1,naok(1)
         do ipc=1,nppac
            if (val(1,i,1) .eq. ipc) then
               TX1(ipc) = val(12,i,1)
               TX2(ipc) = val(13,i,1)
               TY1(ipc) = val(14,i,1)
               TY2(ipc) = val(15,i,1)
               TA(ipc)  = val(16,i,1)
               AX1(ipc) = val(17,i,1)
               AX2(ipc) = val(18,i,1)
               AY1(ipc) = val(19,i,1)
               AY2(ipc) = val(20,i,1)
               AA(ipc)  = val(21,i,1)
               X(ipc)   = val(31,i,1)
               Y(ipc)   = val(32,i,1)
            endif
         enddo
         do ipc=1,nfpl
            if (val(1,i,1) .eq. (100+ipc)) then
               fx(ipc) = val(2,i,1)
               fy(ipc) = val(3,i,1)
               fa(ipc) = val(9,i,1)
               fb(ipc) = val(10,i,1)
            endif
         enddo
      enddo
cc      EndIf
cc -- Analyzer 2
cc      If(AnalyzerFlag(2)) Then
cc         AnaRun1 = RunNum
c
c      do i=1,naok(2)
c         if (val(1,i,2) .ge. 1 .and. val(1,i,2) .le. 8) then
c            PLTL(val(1,i,2)) = val(12,i,2)
c            PLTR(val(1,i,2)) = val(13,i,2)
c            PLAL(val(1,i,2)) = val(14,i,2)
c            PLAR(val(1,i,2)) = val(15,i,2)
c         endif
c      enddo
c      do i=1,naok(5)
c         if (val(1,i,5) .eq. 3) then
c            do idet=1,6
c               F7IC(idet) = val(1+idet,i,5)
c            enddo
c         endif
c         if (val(1,i,5) .eq. 4) then
c            do idet=1,6
c               FEIC(idet) = val(1+idet,i,5)
c            enddo
c         endif
c      enddo
c      do i=1,naok(6)
c         if (val(1,i,6) .eq. 2) then
c            NAIE = val(2,i,6)
c         endif
c      enddo
cc      EndIf
c
cc -- Analyzer 3
cc      If(AnalyzerFlag(3)) Then
cc         AnaRun2 = RunNum
cc      EndIf

      Return
      
      End




