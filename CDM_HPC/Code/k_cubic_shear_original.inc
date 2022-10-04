! $Id: k_cubic_shear.inc 1049 2012-08-28 23:15:44Z boman $
!> \ingroup grpshear
!> Shear cubic function
!>
!>\f$	\tau(\gamma) = c_1\,\gamma^3 - c_2\,\gamma^2\mbox{sign}\gamma + c_3\gamma \f$
!>

function K_CUBIC_SHEAR(x, m)
   use K_IDM
   implicit none
   real(DP), intent(in) :: x
   type(s_material_shear), intent(in) :: m
   real(DP) :: K_CUBIC_SHEAR
   K_CUBIC_SHEAR = m%c1*x**3 - m%c2*sign(one,x)*x**2 + m%c3*x
end function K_CUBIC_SHEAR
