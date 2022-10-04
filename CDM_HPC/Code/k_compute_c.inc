! $Id: k_compute_c.inc 1051 2012-09-04 06:41:05Z boman $
!> \ingroup grptools
!> Computation of the stiffness matrix \f$ C \f$
!>

subroutine K_COMPUTE_C(m, C)
   use K_IDM
   implicit none
   type(s_material_elastic), intent(in) :: m
   real(DP), intent(out)                :: C(3,3)
   real(DP) :: den

   den = one - m%nu12*m%nu21 - m%nu23*m%nu32 - m%nu31*m%nu13 - two*m%nu21*m%nu32*m%nu13

   C(1,1) = m%E1*(one-m%nu23*m%nu32)/den
   C(1,2) = m%E1*(m%nu21+m%nu31*m%nu23)/den
   C(1,3) = m%E1*(m%nu31+m%nu21*m%nu32)/den

   C(2,1) = C(1,2) 
   C(2,2) = m%E2*(one-m%nu13*m%nu31)/den
   C(2,3) = m%E2*(m%nu32+m%nu12*m%nu31)/den

   C(3,1) = C(1,3)
   C(3,2) = C(2,3)
   C(3,3) = m%E3*(one-m%nu12*m%nu21)/den

end subroutine K_COMPUTE_C
