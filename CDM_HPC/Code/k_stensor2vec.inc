! $Id: k_stensor2vec.inc 1055 2012-09-11 00:40:51Z boman $

!> \ingroup grptools
!> Copy a k_idm::s_stensor structure to a 6-component vector

subroutine K_STENSOR2VEC(nblock, k, st, vec)
   use K_IDM
   implicit none
   
   integer, intent(in)                        :: nblock  !< number of material points
   integer, intent(in)                        :: k       !< material point index
   type(s_stensor), intent(in)                :: st      !< input structure
   real(DP), dimension(nblock,*), intent(out) :: vec     !< target vector
   
   vec(k,1) = st%s11
   vec(k,2) = st%s22
   vec(k,3) = st%s33
   vec(k,4) = st%s12
   vec(k,5) = st%s23
   vec(k,6) = st%s13 
    
end subroutine K_STENSOR2VEC
