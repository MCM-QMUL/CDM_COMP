! $Id: k_nl_shear_1d.inc 1055 2012-09-11 00:40:51Z boman $
!> \ingroup grptools
!> Compute the effective nonlinear shear and the inelastic shear strain

subroutine K_NL_SHEAR_1D(G, ms, d0, eps0, deps, tau0, eps_inel0, tau, eps_inel1)

   use K_IDM
   implicit none

   real(DP), intent(in)               :: G         !< (undamaged) shear modulus
   type(s_material_shear), intent(in) :: ms        !< material data (shear behaviour)
   real(DP), intent(in)               :: d0        !< damage (previous time increment)
   real(DP), intent(in)               :: eps0      !< shear strain (previous time increment)
   real(DP), intent(in)               :: deps      !< shear increment
   real(DP), intent(in)               :: tau0      !< effective shear stress (previous time increment) 
   real(DP), intent(in)               :: eps_inel0 !< inelastic shear strain (previous time increment)  
   real(DP), intent(out)              :: tau       !< effective shear stress (output)
   real(DP), intent(out)              :: eps_inel1 !< inelastic shear strain (output)
 
   real(DP)  :: gamma0, gamma, dgamma, gammaE0, gamma_y, gammaE
   real(DP)  :: tauE, tau_y
   
   real(DP) :: K_CUBIC_SHEAR
   
   gamma = two*(eps0+deps)
   if(.false.) then   ! enable this test for an elastic behaviour
      tau       = G*gamma   
      eps_inel1 = zero
      return
   end if
   
   gamma0 = two*eps0
   dgamma = two*deps  
   
   ! elastic predictor
   tauE = tau0 + G*dgamma
   ! max allowable effective shear stress => tau_y
   gammaE0 = tau0/G
   gamma_y = two*eps_inel0 + abs(gammaE0+dgamma)
   if (d0==zero) then
      tau_y = K_CUBIC_SHEAR(gamma_y, ms)
   else
      tau_y = abs(tauE)
   end if
   
   if(abs(tauE)>abs(tau_y)) then
      tau       = sign(tau_y, tauE)
      gammaE    = tau/G
      eps_inel1 = (gamma_y - abs(gammaE))/two 
   else
      tau       = tauE  
      eps_inel1 = eps_inel0
   end if
   
end subroutine K_NL_SHEAR_1D
