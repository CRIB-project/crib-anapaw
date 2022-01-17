c
c------ SSD_PRM ------ read psd parameters
c
c                                              Oct 23, 2001  H.Baba
c                                              modified by T. Teranishi
c
        SUBROUTINE DSSD_PRM
        implicit none
c
        include 'ssd.inc'
        integer itmp(9)
        Character*132 dssdprm
c
        integer ier


c       Read parameters

        Call getenv('DSSD_PRM',dssdprm)
c	write(6,*) psdprm

        open(unit=80,file=dssdprm,status='old',err=50)

c ======================================================

        call icler(itmp,9) 

c	print *,'PSD TDC',itdc_low,'-',itdc_high
c
        call read_flt(80,Offset_a_dssd,nch_dssd,ier)
        if (ier.ne.0) goto 70
        print *,'Offset_a_dssd:', Offset_a_dssd

       call read_flt(80,Gain_a_dssd,nch_dssd,ier)
       if (ier.ne.0) goto 70
       print *,'Gain_dssd:', Gain_a_dssd

       call read_flt(80,Offset_t_dssd,nch_dssd,ier)
       if (ier.ne.0) goto 70
       print *,'Offset_t_dssd:', Offset_t_dssd

       call read_flt(80,Gain_t_dssd,nch_dssd,ier)
       if (ier.ne.0) goto 70
       print *,'Gain_t_dssd:', Gain_t_dssd

c
c=============================================================

        write(*,*)' ENC_DSSD : DSSD_PRM read in.'
        close(unit=80)
        go to 60
 70     write(*,*)' ENC_DSSD : Error in DSSD_PRM.'
        stop
 50     write(*,*)' ENC_DSSD : DSSD_PRM not found, skip.'
 60     continue

        write(*,*)' '

        return
        end
