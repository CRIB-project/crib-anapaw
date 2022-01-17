      Subroutine PPAC_PRM
      Implicit none

      Include 'ppac.inc'
      Integer ier
      Character*132 ppacprm

      Call getenv('PPAC_PRM',ppacprm)

      Open(unit=80,file=ppacprm,status='old',err=10)
      Goto 20
 10   Write(*,*)' ENC_PPAC-E : PPAC_PRM not found.'
      stop
 20   Continue
      Write(*,*)' ENC_PPAC-M : PPAC_PRM read in.'
      Write(*,*)' '
      Call read_int(80,IFlag_PPAC,1,ier)
      if(ier.ne.0) Goto 30
c      print *, 'IFlag_PPAC:',IFlag_PPAC 
      Call read_flt(80,Off_PPAC,4*n_ppac,ier)
      if(ier.ne.0) Goto 30 
c      print *, 'Off_PPAC',Off_PPAC
      Call read_flt(80,Gain_PPAC,4*n_ppac,ier)
      if(ier.ne.0) Goto 30
c      print *, 'Gain_PPAC',Gain_PPAC
      
      Call read_int(80,IFlag_Tcalib,1,ier)
      if(ier.ne.0) Goto 30
c      print *, 'IFlag_Tcalib',IFlag_Tcalib
      Call read_flt(80,TGain_PPAC,n_ppac,ier)
      if(ier.ne.0) Goto 30
c      print *, 'TGain_PPAC',TGain_PPAC
      Call read_flt(80,TOff_PPAC,n_ppac,ier)
      if(ier.ne.0) Goto 30
c      print *, 'TOff_PPAC',TOff_PPAC
      
      Call read_flt(80,PosGain,2*n_ppac,ier)
      if(ier.ne.0) Goto 30
c      print *, 'PosGain',PosGain
      Call read_flt(80,ZPOS,n_layer,ier)
      if(ier.ne.0) Goto 30
c      print *, 'ZPOS',zpos
      
      Call read_int(80, iflag_position_offset, 1, ier)
      if(ier.ne.0) Goto 30
c      print *, 'iflag_position_offset',iflag_position_offset
      Call read_flt(80, offset_x, n_PPAC, ier)
      if(ier.ne.0) Goto 30
c      print *, 'offset_x',offset_x
      Call read_flt(80, offset_y, n_PPAC, ier)
      if(ier.ne.0) Goto 30
c      print *, 'offset_y',offset_y
      
      Call read_int(80, iflag_position_offset_line, 1, ier)
      if(ier.ne.0) Goto 30
c      print *, 'iflag_position_offset_line',iflag_position_offset_line
      Call read_flt(80, offset_line_x, n_PPAC, ier)
      if(ier.ne.0) Goto 30
c      print *, 'offset_line_x',offset_line_x
      Call read_flt(80, offset_line_y, n_PPAC, ier)
      if(ier.ne.0) Goto 30
c      print *, 'offset_line_y',offset_line_y

      Call read_int(80, iflag_geom_position_offset, 1, ier)
      if(ier.ne.0) Goto 30
c      print *, 'iflag_geom_position_offset',iflag_geom_position_offset
      Call read_flt(80, offset_geom, 2*n_PPAC, ier)
      if(ier.ne.0) Goto 30
c      print *, 'offset_geom 3*(x,y)',offset_geom

      Call read_int(80, iflag_cath_position_offset, 1, ier)
      if(ier.ne.0) Goto 30
c      print *, 'iflag_cath_position_offset',iflag_cath_position_offset
      Call read_flt(80, offset_cath, 2*n_PPAC, ier)
      if(ier.ne.0) Goto 30
c      print *, 'offset_cath 3*(x,y)',offset_cath
      Call read_int(80, iflag_reverse_axis, 1, ier) 
      if(ier.ne.0) Goto 30
      Call read_flt(80, reverse_axis, 2*n_PPAC, ier)
      if(ier.ne.0) Goto 30
c     
      Call read_int(80, iflag_Pileup_T0_PPAC, 1, ier)
      if(ier.ne.0) Goto 30
c      print *, 'iflag_Pileup_T0_PPAC',iflag_Pileup_T0_PPAC
      Call read_flt(80,Pileup_T0_PPAC,2*n_ppac,ier)
      if(ier.ne.0) Goto 30
c      print *, 'Pileup_T0_PPAC',Pileup_T0_PPAC

cc For MCP calibration with cubic functions
      Call read_flt(80,pcubx,10,ier)
      if(ier.ne.0) Goto 30
      Call read_flt(80,pcuby,10,ier)
      if(ier.ne.0) Goto 30
      
      Close(80)
      Goto 40

 30   Continue
      Write(*,*)' ENC_PPAC-E : Error in PPAC_PRM.'
      stop

 40   Continue

c      close(unit=80)
      Return
      End

      
