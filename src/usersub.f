c
      Subroutine USERSUB(GateFlag)
c
c                                             2000.Oct.20. S.Takeuchi
c                                             2003.Nov.09. S.Takeuchi modified
c
c Do not EDIT!! -->
      Integer        nx/500/,ny/500/,naok(50)
      Real           val(500,500,50)
      Common/ANALYSVALUE/ val,naok
      INCLUDE 'analyslogic.fh'
      INCLUDE 'analysevent.fh'
      INCLUDE 'commonprm.fh'
      INCLUDE 'runstat.fh'
      Logical        GateFlag(0:2000)
      Integer        USERFGATE(10)
      Common/ANALYSUSER/  USERFGATE
c Do not EDIT!! <--
c
c

c
c UFLAG : 1  --  Hogehoge  --
c

      If( (GateFlag(USERFGATE(1))) ) Then
c
c         Call USER_HogeHoge(val(1,1,3),nx,ny,naok(3))
c
      EndIf

      Return

      End

