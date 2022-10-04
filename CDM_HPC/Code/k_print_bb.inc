! $Id: k_print_bb.inc 1055 2012-09-11 00:40:51Z boman $

!> \ingroup grptools
!> Print a k_idm::s_bbox structure to the standard output

subroutine K_PRINT_BB(bb)
   use K_IDM
   implicit none
   
   type(s_bbox), intent(in) :: bb !< input structure to be printed
   
   print "('Bounding box:')"
   print "('  snn (min) = ',F8.2)", bb%snn%min
   print "('  snn (max) = ',F8.2)", bb%snn%max
   print "('  snt (min) = ',F8.2)", bb%snt%min
   print "('  snt (max) = ',F8.2)", bb%snt%max
   print "('  s1n (min) = ',F8.2)", bb%s1n%min
   print "('  s1n (max) = ',F8.2)", bb%s1n%max
   
end subroutine K_PRINT_BB
