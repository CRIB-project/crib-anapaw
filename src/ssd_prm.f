c
c------ SSD_PRM ------ read psd parameters
c
c                                              Oct 23, 2001  H.Baba
c                                              modified by T. Teranishi
c
        SUBROUTINE SSD_PRM
        implicit none
c
        include 'ssd.inc'
        integer itmp(9)
        Character*132 ssdprm
c
        integer ier


c       Read parameters

        Call getenv('SSD_PRM',ssdprm)
c	write(6,*) psdprm

        open(unit=80,file=ssdprm,status='old',err=50)

c ======================================================

        call icler(itmp,9) 



c	print *,'PSD TDC',itdc_low,'-',itdc_high
c	write(*,*) 'nch_ssd',nch_ssd
c
        call read_flt(80,Offset_a_ssd,nch_ssd,ier)
        if (ier.ne.0) goto 70
        print *,'Offset_a_ssd:', Offset_a_ssd

        call read_flt(80,Gain_a_ssd,nch_ssd,ier)
        if (ier.ne.0) goto 70
        print *,'Gain_ssd:', Gain_a_ssd

        call read_flt(80,Offset_t_ssd,nch_ssd,ier)
        if (ier.ne.0) goto 70
        print *,'Offset_t_ssd:', Offset_t_ssd

        call read_flt(80,Gain_t_ssd,nch_ssd,ier)
        if (ier.ne.0) goto 70
        print *,'Gain_t_ssd:', Gain_t_ssd

c
c=============================================================

        write(*,*)' ENC_SSD : SSD_PRM read in.'
        close(unit=80)
        go to 60
 70     write(*,*)' ENC_SSD : Error in SSD_PRM.'
        stop
 50     write(*,*)' ENC_SSD : SSD_PRM not found, skip.'
 60     continue

        write(*,*)' '

        return
        end
