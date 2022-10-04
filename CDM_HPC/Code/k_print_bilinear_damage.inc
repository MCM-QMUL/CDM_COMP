! $Id: k_print_bilinear_damage.inc 1056 2012-09-13 05:54:58Z boman $

!> \ingroup grptools
!> Print a k_idm::s_bilinear_damage structure to the standard output

subroutine K_PRINT_BILINEAR_DAMAGE(law)
   use K_IDM
   implicit none
   
   type(s_bilinear_damage), intent(in) :: law !< input structure to be printed
   
   print "('Bilinear damage:')"
   print "('  length =',G20.10)", law%L
   print "('  epsO   =',G20.10)", law%e0
   print "('  epsF   =',G20.10)", law%eF
   print "('  X      =',G20.10)", law%X
   print "('  G      =',G20.10)", law%G
   print "('  bad    =',G20.10)", law%bad
   
end subroutine K_PRINT_BILINEAR_DAMAGE
