! $Id: k_cubic_law_error.inc 1045 2012-08-17 01:54:27Z boman $
!> \ingroup grpshear
!> Display an error message in the .log/.sta 
!> if the mesh is not sufficiently fine

subroutine K_CUBIC_LAW_ERROR(text, m, length, gamma_F, area)
   use K_IDM
   implicit none
   character(len=*), intent(in)       :: text      !< text to be displayed
   type(s_material_shear), intent(in) :: m         !< material data
   real(DP), intent(in)               :: length    !< characteristic length
   real(DP), intent(in)               :: gamma_F   !< shear strain at failure
   real(DP), intent(in)               :: area      !< area below the curve
   integer, save :: counter1 = 0

   if (counter1<1) then
      print "('ERROR: mesh is not sufficiently fine! (',A,')')", text
      print *, "   length     =", length 
      print *, "   gamma_F    =", gamma_F 
      print *, "   gamma_max  =", m%gamma_max
      print *, "   max length =", m%Gshear/area
   
      call XPLB_ABQERR(-2, "mesh is not sufficiently fine! (%S)", 0, 0.0_dp, text)
      call XPLB_ABQERR(-2, "  length     = %R", 0, length, ' ')
      call XPLB_ABQERR(-2, "  gamma_F    = %R", 0, gamma_F, ' ')
      call XPLB_ABQERR(-2, "  gamma_max  = %R", 0, m%gamma_max, ' ')   
      call XPLB_ABQERR(-3, "  max length = %R", 0, m%Gshear/area, ' ')   
      counter1 = counter1+1
   end if    

end subroutine K_CUBIC_LAW_ERROR


