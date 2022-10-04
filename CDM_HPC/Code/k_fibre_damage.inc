! $Id: k_fibre_damage.inc 1056 2012-09-13 05:54:58Z boman $
!> \ingroup grpfibre
!> Fibre damage model - computation of the stress in the fibre direction

subroutine K_FIBRE_DAMAGE(nblock, m, stateOld, stateNew, stressNew)
   use K_IDM
   implicit none  
   integer, intent(in)                          :: nblock      !< number of material points
   type(s_material_fibre), intent(inout)        :: m           !< material data
   real(DP), dimension(nblock,*), intent(in)    :: stateOld    !< old material state from Abaqus
   real(DP), dimension(nblock,*), intent(out)   :: stateNew    !< new material state from Abaqus (updated by his routine)
   real(DP), dimension(nblock,*), intent(inout) :: stressNew   !< material state (current increment)
   real(DP) :: eps11          !< current strain
   real(DP) :: d11            !< combined damage
   real(DP) :: d11T           !< damage in tension
   real(DP) :: d11C           !< damage in compression
   integer :: k
   
   do k=1, nblock    
    
      d11T  = stateOld(k,ST_D11T)
      d11C  = stateOld(k,ST_D11C)
      eps11 = stateNew(k,S_11)
      
      ! call the appropriate bilinear law depending on the strain sign
      if(HCP_ENABLE_FIBRE) then
         if(eps11>=zero) then
            call K_UPDATE_BILINEAR_LAW(nblock, "fibre tension", m%T, stateNew, k)
            call K_BILINEAR_LAW(m%T, eps11, zero, stateOld(k,ST_D11T), d11T)
         else   
            call K_UPDATE_BILINEAR_LAW(nblock, "fibre compression", m%C, stateNew, k)
            call K_BILINEAR_LAW(m%C, eps11, zero, stateOld(k,ST_D11C), d11C)
         endif 
      endif
      
      ! store the updated damage values
      stateNew(k,ST_D11T) = d11T
      stateNew(k,ST_D11C) = d11C

      ! compute the resulting combined damage in the fibre direction (d11)
      if(eps11>=zero) then  
         d11 = max(d11T,d11C)        ! stiffness recovery - Appruzese
      else      
         d11 = d11C
      endif
      !d11 = d11T + d11C - d11T*d11C ! no stiffness recovery - Faggiani

      ! update the fibre stress
      stressNew(k,S_11) = (one-d11)*stressNew(k,S_11)
      
   end do
   
end subroutine K_FIBRE_DAMAGE
