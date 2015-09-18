PROGRAM CATCORRECT

 ! corrections for photo-z and absorption,
 ! preparation of catalogue for downstream tools

! (c) Piero Ranalli 2014-2015
! Released under the terms of the GPL v3 (see file COPYING)


! TODO: allow fluxcorr, for both catalogue and area

use catcorrectoptions
use photoz
use cosmology
use curves
use lumfunc   ! gives also llmin,llmax,zmin,zmax
use gehrels
use dataread
use areas

implicit none

integer,parameter :: binsize = 1500  ! max number of objects in bin

real pimms,loglum,intel
real passol,ll0
external elle
real elle
real llf,fi
real tempo(150)
real zp(binsize),flux(binsize),lumx(binsize),rmag(binsize),fxott
real fluxerr(binsize)
integer idp(binsize),clp(binsize)
integer i,noggetti
character*3 foo
real bar

real ::  nbefore,nafter

! photo-z corrections
type(photoz_pdf) :: optical


type(catalogue) :: cat,catcorr

real corr,ncoggetti ! numero corretto di oggetti

real prob
real problimit
!      problimit = .05

! allocate space for the catalogues
call cat%new(3000000)
call catcorr%new(3000000)

! parse options and write summary
call parseopt
call optsummary


call setcosmology(myH0,myOM,myOL)


if (do_range) then
   write (*,*) 'Please insert: LogL_min LogL_max z_min z_max'
   read (*,*) llmin,llmax,zmin,zmax
endif


call cat%read(infile)

nbefore = cat%wcount()

if (do_compl) then
   write (*,*) 'doing completeness correction'

   call cat%correct_w_fluxes(completenessfile)
endif

if (do_photozpdf) then
   write (*,*) 'doing photoz pdf corr'

   call optical%choose(pdfstart,pdfstop,pdfpath)
   call catcorr%reset
   call cat%correct_w_photozpdf(catcorr,optical)
   cat = catcorr
endif


if (do_nhcorr) then
   write(*,*) 'correcting for absorption'

   call catcorr%reset
   call cat%read_probmatrix
   call cat%correct_w_hr_nh_prob(catcorr)
   
   !write (*,*) 'somma dei flussi ',sum(cat%flux),sum(catcorr%flux)
   cat = catcorr
end if



nafter = cat%wcount()

! consistency check: sum of weights (i.e. number of objects) must be the same
! before and after corrections
! (but NB the sum does change if we are correcting for completeness)
if ( abs(nbefore-nafter) > .5 .and. .not. do_compl) then
   write (*,*) 'Internal error: number of objects after correction is not the same as it was before.'
   write (*,*) 'before:', nbefore, '    after:', nafter
end if


! compute luminosities
call cat%setkcorr(gamma)
write (*,*) 'K-corrections using gamma=',gamma
call cat%flux2lum
      
if (do_range) then
   call cat%selectlum(llmin,llmax)
   call cat%selectz(zmin,zmax)
end if


! write catalogue
call cat%writelnls(outfile)



if (cat%wcount() < .9) then  ! not even 1 object in the bin
   write (*,*) 'Warning: not even 1 object in the output catalogue.'
end if

end PROGRAM CATCORRECT


subroutine optsummary
  use catcorrectoptions

  implicit none

  write (*,*) 'lf-catcorrect    ---   apply corrections and/or format catalogues'
  write (*,*) ''
  if (do_nhcorr .or. do_photozpdf .or. do_range) then
     write (*,*) 'Will apply the following corrections and selections:'
  else
     write (*,*) 'No correction or selection will be applied.'
  end if

  ! if (do_complcorr)    write (*,*) '* incompleteness'  ! NYI
  if (do_nhcorr)       write (*,*) '* absorption'
  if (do_photozpdf)    write (*,*) '* photo-z probability distributions'
  if (do_range)        write (*,*) '* luminosity and redshift ranges'
  write (*,*) 'Will use the following cosmology:'
  write (*,*) 'H0=',myH0,' OmegaLambda=',myOL,' OmegaMatter=',myOM
  write (*,*) ''

end subroutine optsummary
