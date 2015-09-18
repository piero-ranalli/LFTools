PROGRAM BINNED_LUMINOSITY_FUNCTION ! binned luminosity functions

! (c) Piero Ranalli 2004-2015
! Released under the terms of the GPL v3 (see file COPYING)

use m_option_parser

use lfbinnedconfig
use fileutils
use cosmology
use curves
use lumfunc   ! gives also llmin,llmax,zmin,zmax
use gehrels
use dataread
use areas

implicit none


real pimms,loglum,gamma,intel
real passol,ll0
real llf,fi
integer i,j,noggetti,catrecno
character*3 foo
real bar


! end of command line pars

type(curve) :: correction
type(catalogue) :: cat,cat2
type(coverage), target :: area
integer, allocatable :: catsize(:)
! area is passed to lumfunc via areapointer

real ncoggetti ! numero corretto di oggetti


! read catalogue/area lists and cosmology
call configure
call setcosmology(myH0,myOM,myOL)  ! defaults are not working!

! find total number of records
catrecno=0
allocate( catsize(size(catlist)) )
do i=1, size(catlist)
   catsize(i) = lines_in_file(catlist(i))
   catrecno = catrecno + catsize(i)
end do

write (*,*) 'allocating space for ',catrecno,' objects'
call cat%new(catrecno)

! read first catalogue
write (*,*) 'reading catalogue ',catlist(1)
call cat%readprocessed( catlist(1) )

! append other catalogues
do i=2, size(catlist)
   call cat2%new(catsize(i))
   write (*,*) 'reading catalogue ',catlist(i)
   call cat2%readprocessed( catlist(i) )

   do j=1, catsize(i)
      call cat2%copycat( j, cat, 1., 0. )
   end do

   call cat2%destroy
end do


write (*,*) cat%wcount()


! --------------------------------------------------


call cat%saveweights

do


   write (*,*) 'Please insert: LogL_min LogL_max z_min z_max'
   read (5,*,end=9999) llmin,llmax,zmin,zmax



   call cat%selectlum(llmin,llmax)
   call cat%selectz(zmin,zmax)


   call lumfunc_setup


   if (cat%wcount() < .9) then  ! not even 1 object in the bin
      print *,'not even 1 object in the bin'
      call cat%recoverweights
      cycle
   end if



   ! calc N (number of object)

   ncoggetti = cat%wcount()

   noggetti = int(ncoggetti)

   write (*,*) noggetti, ' objects in the bin'
   if (noggetti.le.50) then
      write (*,*) '1sigma interval: ',gehrelsl(noggetti), gehrelsh(noggetti)
   endif


   ! calc Volume, as the sum of the volumes from all area files

   intel= 0

   do i=1, size(arealist)

      call area%read( arealist(i) )
      areapointer => area

      passol = (llmax-llmin)/300.d0
      ll0 = llmin
      do while (ll0.le.llmax)

         intel = intel + zinte(ll0)*passol
         ll0 = ll0 + passol

      end do

   end do

   if (intel<=0) then
      write (*,*) 'Warning: volume integral = ',intel
      write (*,*) 'though there are ',ncoggetti,' objects in the bin.'
      cycle
   end if


   write (*,*) 'volume integral = ',intel

   write (*,*) 'llmin llmax zmin zmax phi phi_low phi_up tot_weight'

3434 format (1X,F4.1,1X,F4.1,1X,F5.3,1X,F5.3,1X,F7.3,1X,F7.3,1X,F7.3,1X,F6.1,1X,1A)

   write (*,3434) llmin,llmax,zmin,zmax,     &
        log10(real(noggetti)/intel),         &
        log10((gehrelsl(noggetti))/intel),   &
        log10((gehrelsh(noggetti))/intel),   &
        ncoggetti, '%'

   !   write (*,*) 'final dimension of catalogue arrays: ',cat%last

   call cat%recoverweights

end do

9999 continue

end PROGRAM

