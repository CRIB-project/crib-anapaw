c
c------ TEL.INC ------ common area for Telescope layer analysis
c
	Integer n_tel, n_layer
	parameter (n_tel = 5)  ! number of telescopes 
	parameter (n_layer = 4) ! Number of layers in a telescope (assumed <10)

	real etel(n_tel,n_layer)
	real ttel(n_tel,n_layer)
c
        common/COM_TEL/
     &  etel, ttel
