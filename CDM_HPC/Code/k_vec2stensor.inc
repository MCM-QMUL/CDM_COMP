! $Id: k_vec2stensor.inc 1055 2012-09-11 00:40:51Z boman $

!> \ingroup grptools
!> Copy a 6-component vector to a k_idm::s_stensor structure

subroutine K_VEC2STENSOR(nblock, k, vec, st)
   use K_IDM
   implicit none
   
   integer, intent(in)                       :: nblock  !< number of material points
   integer, intent(in)                       :: k       !< material point index
   real(DP), dimension(nblock,*), intent(in) :: vec     !< input vector to be converted
   type(s_stensor), intent(out)              :: st      !< target structure
   
   st%s11 = vec(k,1)
   st%s22 = vec(k,2)
   st%s33 = vec(k,3)
   st%s12 = vec(k,4)
   st%s23 = vec(k,5)
   st%s13 = vec(k,6)
   
end subroutine K_VEC2STENSOR
