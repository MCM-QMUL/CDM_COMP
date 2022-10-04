! $Id: k_bilinear_law.inc 1051 2012-09-04 06:41:05Z boman $
!> \ingroup grpfibre
!> Bilinear law - Computation of the current damage variable
!!
!! This routine is used for modelling the damage progression 
!!   - in the fibre direction 
!!   - on the fracture plane computed by the Puck's criterion
!!
!! The initiation strain is computed by
!! \f[
!!  \varepsilon^{*}_0 = \frac{\varepsilon_0\varepsilon_F}{(1-d^{\text{old}})\varepsilon_F+\varepsilon_0d^{\text{\old}}}
!! \f]
!! And the damage is then updated using
!! \f[
!!       d^{\text{new}} = \displaystyle\frac{\varepsilon_F}{\varepsilon_F-\varepsilon_0} 
!!       \left( 1-\frac{1}{|\varepsilon|} \right)
!! \f]

subroutine K_BILINEAR_LAW(law, eps, epsI, d0, d1)
   use K_IDM
   implicit none  
   type(s_bilinear_damage), intent(in)  :: law   !< input data
   real(DP), intent(in)                 :: eps   !< current strain
   real(DP), intent(in)                 :: epsI  !< current inelastic strain
   real(DP), intent(in)                 :: d0    !< previous damage
   real(DP), intent(out)                :: d1    !< current damage (output)
   real(DP) :: dnew, eps0

   ! default output
   d1 = d0
   ! compute the initiation strain from e0 and d0
   eps0 = (law%e0*(law%eF-epsI)+d0*epsI*(law%e0-law%eF)) / &
          ((law%eF-epsI)+d0*(law%e0-law%eF)) 
        
   ! check criterion
   if (abs(eps) >= eps0) then
      dnew = one-(law%e0-epsI)/(abs(eps)-epsI)*(law%eF-abs(eps))/(law%eF-law%e0)
      dnew = min(dnew, one)
      if (dnew>d0) then
         d1 = dnew
      end if
   endif
  
end subroutine K_BILINEAR_LAW
