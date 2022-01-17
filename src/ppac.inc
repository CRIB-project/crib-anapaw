c
c       ID
c
        Integer n_ppac,n_layer
        Parameter(n_ppac=5)     ! PPAC_A,B,F2,MCP,MCP_VME
        Parameter(n_layer=6)    ! PPAC_A,B, plane 1, plane 2, MCP,MCP_VME
c
c
c       Parameters for PPACs
c
        Integer IFlag_PPAC,IFlag_Tcalib
        Real TOff_PPAC(n_ppac),TGain_PPAC(n_ppac)
        Real Off_PPAC(4,n_ppac),Gain_PPAC(4,n_ppac)
        Real PosGain(2,n_ppac)
        Real ZPOS(1-n_ppac:n_layer-n_ppac)
        integer iflag_position_offset,iflag_position_offset_line
        real offset_x(n_ppac),offset_y(n_ppac)
        real offset_line_x(n_ppac),offset_line_y(n_ppac)

        integer iflag_geom_position_offset
        integer iflag_cath_position_offset
        real offset_geom(2,n_ppac),offset_cath(2,n_ppac)
        integer iflag_reverse_axis
	real reverse_axis(2,n_ppac)
        integer iflag_Pileup_T0_PPAC
        real Pileup_T0_PPAC(2,n_ppac)


	integer mcp_raw(10)

cc For MCP calibration with cubic functions
        real pcubx(10), pcuby(10)

cccc  for BGO
c
c       Integer IDF2L,IDF2R,IDF3L,IDF3R,IDRF1,IDRF2,IDSSD,
c     & IDPPAC
c       Parameter (IDF2L=1,IDF2R=2)
c       Real ADC_Ped2(4)
c       Data ADC_Ped2/30.,30.,30.,30./
c       Real ADC_Gain2(4)
c       Data ADC_Gain2/1.,1.,1.,1./
c
c       Common/BeamParam2/ TDC_LIM2,nSecPerCh2,ADC_Ped2,ADC_Gain2,
c
c       Parameters for Beam Line Scintillators
c
c
c       Real TDC_LIM2(2)
c       Data TDC_LIM2/0.,4000./
c       Real nSecPerCh2(2)
c       Data nSecPerCh2/0.1,0.1/
cccccc


        Common/PPACParam/ TOff_PPAC,TGain_PPAC,Off_PPAC,Gain_PPAC,
     &                    PosGain,ZPOS,IFlag_PPAC,IFlag_Tcalib,
     &                    iflag_position_offset,
     &                    iflag_position_offset_line,
     &                    offset_x,offset_y,
     &                    offset_line_x,offset_line_y,
     &                    iflag_geom_position_offset,
     &                    iflag_cath_position_offset,
     &                    offset_geom,offset_cath,
     &                    iflag_Pileup_T0_PPAC,
     &                    Pileup_T0_PPAC,
     &			  iflag_reverse_axis,
     &			  reverse_axis,
     &			  pcubx, pcuby    

        Common/MCPraw/ mcp_raw
