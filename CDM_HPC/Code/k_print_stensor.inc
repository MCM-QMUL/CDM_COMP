! $Id: k_print_stensor.inc 1055 2012-09-11 00:40:51Z boman $

!> \ingroup grptools
!> Print a k_idm::s_stensor structure to the standard output

subroutine K_PRINT_STENSOR(s)
   use K_IDM
   implicit none
   
   type(s_stensor), intent(in) :: s    !< input structure to be printed
   
   print "('Bilinear damage:')"
   print "('  s11/s11 =',G20.10)", s%s11
   print "('  s22/snn =',G20.10)", s%s22
   print "('  s33/stt =',G20.10)", s%s33
   print "('  s12/s1n =',G20.10)", s%s12
   print "('  s23/snt =',G20.10)", s%s23
   print "('  s13/s1t =',G20.10)", s%s13
   
end subroutine K_PRINT_STENSOR
