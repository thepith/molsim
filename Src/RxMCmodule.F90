!************************************************************************
!> \page sso sso.F90
!! **RxMCModule**
!! *Module to perorm reaction ensamble Monte Carlo simulations
!************************************************************************

module RxMCModule

function RxMCInitReaction(iStage, GammaReaction, rpt)

   implicit none

   if(.not. lmonoatom) then
      ! WARN
   endif

   if(.not. latweakcharge(rpt)) then
      ! WARN
   endif

   reaction%nprod = 0
   reaction%nedu = 0
   reaction%ipedu(:) = 0
   reaction%ipprod(:) = 0
   do ip = ipnpt(rpt), ipnpt(rpt) + nppt(rpt) - 1
      if (laz(ip)) then
         nprod = nprod + 1
         reaction%ipprod(nprod) = ip
      else
         nedu = nedu + 1
         reaction%ipedu(nedu) = ip
      end if
   end do

   reaction%GammaReaction = GammaReaction

end function RxMCOneWay

subroutine RxMCOneWay(iStage, ipedu, nedu, nprod, GammaReaction)

   use MCModule
   implicit none

   integer(4), intent(in) :: iStage

   character(40), parameter :: txroutine ='RxMCOneWay'
   logical    :: lboxoverlap, lhsoverlap, lhepoverlap
   real(8) ::  random,  weight, xsign
   integer(4) :: ip, ipt, ia, ialoc, ianmove, iatmove

   if (ltrace) call WriteTrace(3, txroutine, iStage)

   if (ltime) call CpuAdd('start', txroutine, 1, uout)

   imovetype = irxmcmove

   lptmdutwob = .true.   ! engage energy evaluation "inside" the moving group

! .............. define charge(s) to be changed ..............

   nptm = 1
   iedu = Random(iseed) * nedu + 1
   ipmove = ipedu(iedu)
   ipnptm(1) = ipmove
   lptm(ipmove)  =.true.

   ia = ipmove

   ianatm(1) = ia               ! update ianatm
   if (jatweakcharge(iatan(ia)) > 0) then  ! get id of atom and particle carrying counter charge
      nptm = 2
      ia = iananweakcharge(ianatm(1))  ! id of atom
      ianatm(2) = ia
      ip = ipnan(ia)                   ! id of particle
      ipnptm(2) = ip
      lptm(ip) = .true.
   end if
   natm = nptm

   rotm(1:3,1:nptm)      = ro(1:3,ipnptm(1:nptm))
   oritm(1:3,1:3,1:nptm) = ori(1:3,1:3,ipnptm(1:nptm))
   rtm(1:3,1:natm) = r(1:3,ianatm(1:natm))
   call CheckPartBCTM(nptm, rotm, lboxoverlap)

   if (lfixedori) then           ! lfixedori atains its value in coordinate.F90
       call AddNeighbours
       call UpdateOri
   end if

   laztm(1:natm) =.not.laz(ianatm(1:natm))  ! get trial charge states

   where (laztm(1:natm)) ! set trial charge according to trial charge state
      aztm(1:natm) = zat(iatan(ianatm(1:natm)))
   elsewhere
      aztm(1:natm) = Zero
   end where

   if (itest == 90) then
      call writehead(3, txroutine, uout)
      write(uout,'(a,2i5  )') 'ipnapm(iploc)', (ipnptm(1:nptm))
      write(uout,'(a,2i5  )') 'ianatm(ialoc)', (ianatm(1:natm))
      write(uout,'(a,2l5  )') 'laz(ialoc)', (laz(ianatm(1:natm)))
      write(uout,'(a,2l5  )') 'laztm(ialoc)', (laztm(1:natm))
      write(uout,'(a,2f5.1)') 'az(ialoc)', (az(ianatm(1:natm)))
      write(uout,'(a,2f5.1)') 'aztm(ialoc)', (aztm(1:natm))
   end if

   if (ltime) call CpuAdd('stop', txroutine, 1, uout)

   if (lboxoverlap) goto 200

! ............. evaluate energy difference ...............

   call DUTotal(lhsoverlap, lhepoverlap)

! ............. calculate nonenergetic weights .............

   ianmove = ianatm(1)                                ! id of titrating atom
   iatmove = iatan(ianmove)                           ! atome type of titrating atom
   xsign = sign(one,zat(iatmove))                     ! sign of charge of the charged state
   if (.not.laz(ianmove)) xsign = -xsign              ! sign to be used in weight
   weight = GammaReaction * (float(nedu)**nptm/float(nprod + 1)**nptm)

!  ............. decide new configuration .............

200 continue
   if (itestmc == 3) call TestChargeChange1(uout)
   call Metropolis(lboxoverlap, lhsoverlap, lhepoverlap, weight, du%tot*beta)

! .............. update .............

   if (ievent == imcaccept) then
      call MCUpdate       ! update energies and coordinates
      do ialoc = 1, natm
         laz(ianatm(ialoc)) = laztm(ialoc) ! update charge status
         az(ianatm(ialoc))  = aztm(ialoc)  ! update atom charge
      end do
      ipedu(iedu:nedu) = cshift(ipedu(iedu:nedu), 1)
      ipedu(nedu) = 0
      nedu = nedu - 1
      nprod = nprod + 1
      iprod(nprod) = ipmove
   end if

   if (itest == 90) then
      call writehead(3, txroutine, uout)
      write(uout,'(a,e12.5)')  'weight', weight                          !cc
      write(uout,'(a,i5)')     'ievent', ievent                          !cc
      write(uout,'(a,10l5)')   'laztm', laztm(1:2)                       !cc
      write(uout,'(a,10l5)')   'laz', laz(ianatm(1:2))                   !cc
      write(uout,'(a,10f5.1)') 'aztm', aztm(1:2)                         !cc
      write(uout,'(a,10f5.1)') 'az', az(ianatm(1:2))                     !cc
      write(uout,'(a,10f12.5)')'u%twob(0:nptpt)', real(u%twob(0:nptpt))  !cc
   end if

   lptmdutwob =.false. ! restore lptmdutwob

   if (itestmc == 3) call TestChargeChange2(uout)

contains

!........................................................................

subroutine TestChargeChange1(unit)
   integer(4),   intent(in) :: unit
   call WriteHead(3, 'Test'//trim(txroutine)//'1', unit)
   write(unit,'(a,t20,i4    )') 'ipmove              ', ipmove
   write(unit,'(a,t20,i4    )') 'iptmove             ', iptmove
   write(unit,'(a,t20,2l    )') 'laz(ipnptm(1:nptm)) ', laz(ipnptm(1:nptm))
   write(unit,'(a,t20,2l    )') 'laztm(1:nptm)       ', laztm(1:nptm)
   write(unit,'(a,t20,3f15.5)') 'weight              ', weight
   write(unit,'(a,t20, l    )') 'lboxoverlap         ' ,lboxoverlap
   write(unit,'(a,t20, l    )') 'lhsoverlap          ', lhsoverlap
   write(unit,'(a,t20, l    )') 'lhepoverlap         ', lhepoverlap
   write(unit,'(a,t20,1f15.5)') 'du%tot              ', du%tot
   write(unit,'(a,t20,7f15.5)') 'du%twob(0:nptpt)    ', du%twob(0:nptpt)
   write(unit,'(a,t20,1f15.5)') 'u%tot       (old)   ', u%tot
   write(unit,'(a,t20,100l  )') 'laz(1:na) (old)     ' ,laz(1:na)
end subroutine TestChargeChange1

!........................................................................

subroutine TestChargeChange2(unit)
   integer(4),   intent(in) :: unit
   call WriteHead(3, 'Test'//trim(txroutine)//'2', unit)
   write(unit,'(a,t20, i5   )') 'ievent              ', ievent
   write(unit,'(a,t20,1f15.5)') 'u%tot     (new)     ', u%tot
   write(unit,'(a,t20,100l  )') 'laz(1:na) (new)     ', laz(1:na)
end subroutine TestChargeChange2

!........................................................................

end subroutine ChargeChange
end module RxMCModule
