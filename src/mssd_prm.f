c
c------ SSD_PRM ------ read psd parameters
c
c                                              Oct 23, 2001  H.Baba
c                                              modified by T. Teranishi
c
        SUBROUTINE MSSD_PRM
        implicit none
c
        include 'ssd.inc'
        integer itmp(9)
        Character*132 mssdprm
c
        integer ier


c       Read parameters

        Call getenv('MSSD_PRM',mssdprm)
c	write(6,*) psdprm

        open(unit=80,file=mssdprm,status='old',err=50)

c ======================================================

        call icler(itmp,9) 

c	print *,'PSD TDC',itdc_low,'-',itdc_high
c
        call read_flt(80,Offset_a_mssd,nch_mssd*nmux,ier)
        if (ier.ne.0) goto 70
        print *,'Offset_a_mssd:', Offset_a_mssd

       call read_flt(80,Gain_a_mssd,nch_mssd*nmux,ier)
       if (ier.ne.0) goto 70
       print *,'Gain_mssd:', Gain_a_mssd

       call read_flt(80,Offset_t_mssd,nch_mssd*nmux,ier)
       if (ier.ne.0) goto 70
       print *,'Offset_t_mssd:', Offset_t_mssd

       call read_flt(80,Gain_t_mssd,nch_mssd*nmux,ier)
       if (ier.ne.0) goto 70
       print *,'Gain_t_mssd:', Gain_t_mssd

       call read_flt(80,pos_offset_mssd,nch_mssd*2,ier)
       if (ier.ne.0) goto 70
       print *,'Pos_offset_mssd:', pos_offset_mssd

       call read_flt(80,pos_interval_mssd,nch_mssd*2,ier)
       if (ier.ne.0) goto 70
c       print *,'Pos_interval_mssd:', pos_offset_mssd
       print *,'Pos_interval_mssd:', pos_interval_mssd
c     add okawa (12/28)
       call read_flt(80,pos_highpeak_mssd,nch_mssd*2,ier)
       if (ier.ne.0) goto 70
       print *,'Pos_highpeak_mssd:', pos_highpeak_mssd
c
c=============================================================

        write(*,*)' ENC_MSSD : MSSD_PRM read in.'
        close(unit=80)
        go to 60
 70     write(*,*)' ENC_MSSD : Error in MSSD_PRM.'
        stop
 50     write(*,*)' ENC_MSSD : MSSD_PRM not found, skip.'
 60     continue

        write(*,*)' '

        return
        end
