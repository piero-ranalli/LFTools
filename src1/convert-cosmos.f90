program catalogue_read_cosmos

  ! This program reads the XMM-COSMOS catalogue, removes objects without redshifts,
  ! and writes it in the format required by lf-catcorrect.

  
use dataread

    type(catalogue) :: cat
    integer u,u2,i
    character(3) :: foo
    real flux052,zphot

    call cat%new(2000)
    
    open (newunit=u,  file='xmmcosmos-xmm53fields_table_112011.dat', status='old')
    open (newunit=u2, file='xmmcosmos-formatted-cat.dat', status='new')

    write (u2,*) ' #        id       flux         zspecflag    redshift       detectprob  soft/hard_ratio  optical_id'
    
    ! skip comment rows
    read (u,*) foo
    read (u,*) foo

    ! add objects to catalogue instead of starting from number 1
    i=cat%last+1
    do
       read (u,*,end=342) foo,cat%id(i),foo,foo,flux052,cat%flux(i),foo,foo,foo,foo, &
            foo,foo,foo,foo,foo,foo,foo,foo,foo,foo,foo,cat%zp(i),foo,foo,zphot

       ! skip objects not detected in 2-10 keV band
       if (cat%flux(i) < 0) cycle

       ! photo-z ID is the XID
       cat%opticalid(i) = cat%id(i)


       ! check redshifts
       if (cat%zp(i) <= 0)  then

          if (zphot <= 0)   cycle  ! skip objects without z

          ! otherwise it's a photoz
          cat%zp(i) = zphot
          cat%zspecflag(i) = 0

       else

          cat%zspecflag(i) = 1  ! a spec-z

       end if

       !write (*,*) cat%id(i), cat%flux(i), cat%zp(i)

       if (flux052 < 0)   flux052=0
       cat%fratio(i) = flux052 / cat%flux(i)

       cat%weight(i) = 1.

       write (u2,*) cat%id(i),cat%flux(i),cat%zspecflag(i),cat%zp(i),1,cat%fratio(i), &
                    cat%opticalid(i)

       i = i+1
    end do

342 close(unit=u)
    cat%last = i-1
    close(unit=u2)




  end program catalogue_read_cosmos
  
