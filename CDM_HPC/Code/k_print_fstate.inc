! $Id: k_print_fstate.inc 1055 2012-09-11 00:40:51Z boman $

!> \ingroup grptools
!> Print a k_idm::s_fstate structure to the standard output

subroutine K_PRINT_FSTATE(fs)
   use K_IDM
   implicit none
   
   type(s_fstate), intent(in) :: fs     !< input structure to be printed
   integer :: i
   
   print "('Failure state:')"
   print "('  failure   =',L)", fs%failure
   print "('  theta_max =',F7.4, ' (=',F8.4,' deg.)')", fs%tm, fs%tm*180.0_dp/pi
   print "('  f_max     =',F7.4)", fs%fm
   print "('  nb of max =',I2)", fs%n
   do i=1,fs%n
      print "('   ',I1,': t=',F7.4, ' (=',F8.4,' deg.) f=',F7.4)", i, fs%tmax(i), fs%tmax(i)*180.0_dp/pi, fs%fmax(i) 
   end do
   
end subroutine K_PRINT_FSTATE
