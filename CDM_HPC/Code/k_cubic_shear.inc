! $Id: k_cubic_shear.inc 1049 2012-08-28 23:15:44Z boman $
!> \ingroup grpshear
!> Shear exponential function
!> 
!>\f$	\tau(\gamma) = c_1\,\gamma^3 - c_2\,\gamma^2\mbox{sign}\gamma + c_3\gamma \f$
!>

function K_CUBIC_SHEAR(x, m)
   use K_IDM
   implicit none
   real(DP), intent(in) :: x
   type(s_material_shear), intent(in) :: m
   real(DP) :: K_CUBIC_SHEAR
   K_CUBIC_SHEAR = m%c1*(sign(one,x)*exp(sign(one,x)*m%c2*x)-sign(one,x)*exp(sign(one,x)*m%c3*x))
end function K_CUBIC_SHEAR
