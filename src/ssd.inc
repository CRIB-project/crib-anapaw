c
c------ SSD.INC ------ common area for SSD calib
c
	Integer nch_dssd, nch_mssd,nch_ssd, nmux
	parameter (nch_dssd = 64) 
	parameter (nch_mssd = 6) ! MUX-32
c	parameter (nch_mssd = 12) ! MUX-32 for pos1 pos2
	parameter (nmux = 16) ! 16 ch-> MUXed
	parameter (nch_ssd = 17) ! single channel

	real data_t_dssd(nch_dssd)
	real data_a_dssd(nch_dssd)

	real offset_a_dssd(nch_dssd), gain_a_dssd(nch_dssd)
	real offset_t_dssd(nch_dssd), gain_t_dssd(nch_dssd)

	real data_t_mssd(nch_mssd)
	real data_a_mssd(nch_mssd)

	real offset_a_mssd(nch_mssd*nmux)
	real gain_a_mssd(nch_mssd*nmux)
	real offset_t_mssd(nch_mssd*nmux)
	real gain_t_mssd(nch_mssd*nmux)

	real pos_offset_mssd(nch_mssd*2)
	real pos_interval_mssd(nch_mssd*2)

c add okawa (12/28)
	real pos_highpeak_mssd(nch_mssd*2)

	real offset_a_ssd(nch_ssd), gain_a_ssd(nch_ssd)
	real offset_t_ssd(nch_ssd), gain_t_ssd(nch_ssd)


c	real ped_lowgain(16), gain_lowgain(16)
		
        integer ypattern(16) 
        Data ypattern/8,7,6,5,4,3,2,1,9,10,11,12,13,14,15,16/  !reorder ADC channel-> strip channel by geometrical order



c
        common/COM_SSD/
     &  data_t_dssd,  ! T (ch) 
     &  data_a_dssd,  ! A (ch) 
     &  data_t_mssd,  ! T (ch) 
     &  data_a_mssd,  ! A (ch) 
     &  offset_a_dssd, gain_a_dssd,
     &  offset_t_dssd, gain_t_dssd,
     &  offset_a_mssd, gain_a_mssd,
     &  offset_t_mssd, gain_t_mssd,
     &  pos_offset_mssd,pos_interval_mssd,pos_highpeak_mssd,
     &  offset_a_ssd, gain_a_ssd,
     &  offset_t_ssd, gain_t_ssd
     &  ypattern