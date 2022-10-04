! $Id: k_effective_stress.inc 1055 2012-09-11 00:40:51Z boman $
!> \ingroup grptools
!> Compute the current strains and the effective stresses
!>
!> \f[
!>   \boldsymbol{\varepsilon}(t^{n+1})=\boldsymbol{\varepsilon}^{n}(t^{n})+\Delta\boldsymbol{\varepsilon}
!> \f]
!> \f[
!>   \boldsymbol{\sigma}^{\text{eff}}=\mathbf{C}\boldsymbol{\varepsilon}
!> \f]

subroutine K_EFFECTIVE_STRESS(nblock, me, ms12, ms23, ms13, &
                              stateOld, strainInc, stressNew, stateNew)
   use K_IDM
   implicit none
   integer, intent(in)                        :: nblock     !< number of material points
   type(s_material_elastic), intent(in)       :: me         !< material data (elasticity)
   type(s_material_shear), intent(in)         :: ms12       !< material data (shear 12)
   type(s_material_shear), intent(in)         :: ms23       !< material data (shear 23)
   type(s_material_shear), intent(in)         :: ms13       !< material data (shear 13)
   real(DP), dimension(nblock,*), intent(in)  :: stateOld   !< state array of the previous increment
   real(DP), dimension(nblock,*), intent(in)  :: strainInc  !< strain increment
   real(DP), dimension(nblock,*), intent(out) :: stressNew  !< output stress
   real(DP), dimension(nblock,*), intent(out) :: stateNew   !< state array of the current increment
   
   real(DP), dimension(3,3) :: C                            !< C constitutive matrix
   integer :: k
   
   call K_COMPUTE_C(me, C)
   
   do k=1, nblock
      ! current strain
      stateNew(k,1:6) = stateOld(k,1:6)+strainInc(k,1:6)
      ! elastic law for sig11, sig22, sig33
      stressNew(k,1:3) = MATMUL(C, stateNew(k,1:3))
      ! non linear shear
      call K_NL_SHEAR_1D(me%G12, ms12, stateOld(k,ST_MC_D), stateOld(k,S_12), strainInc(k,S_12), &
               stateOld(k,ST_SIGE_12), stateOld(k,ST_EPS_INEL_12), &
               stateNew(k,ST_SIGE_12), stateNew(k,ST_EPS_INEL_12))
      call K_NL_SHEAR_1D(me%G23, ms23, stateOld(k,ST_MC_D), stateOld(k,S_23), strainInc(k,S_23), &
               stateOld(k,ST_SIGE_23), stateOld(k,ST_EPS_INEL_23), &
               stateNew(k,ST_SIGE_23), stateNew(k,ST_EPS_INEL_23))
      call K_NL_SHEAR_1D(me%G13, ms13, stateOld(k,ST_MC_D), stateOld(k,S_13), strainInc(k,S_13), &
               stateOld(k,ST_SIGE_13), stateOld(k,ST_EPS_INEL_13), &
               stateNew(k,ST_SIGE_13), stateNew(k,ST_EPS_INEL_13))
               
      stressNew(k,S_12) = stateNew(k,ST_SIGE_12)
      stressNew(k,S_23) = stateNew(k,ST_SIGE_23)
      stressNew(k,S_13) = stateNew(k,ST_SIGE_13)
   end do

end subroutine K_EFFECTIVE_STRESS
