! $Id: k_matrix_damage.inc 1056 2012-09-13 05:54:58Z boman $
!> \ingroup grpmatrix
!> Compute the matrix damage and related stresses 

subroutine K_MATRIX_DAMAGE( nblock, mm, me, ms12, ms23, ms13, stateOld, &
                            stateNew, stressNew)
   use K_IDM
   implicit none

   integer, intent(in) :: nblock                        !< number of material points
   type(s_material_matrix), intent(in)   :: mm          !< material data related to the matrix
   type(s_material_elastic), intent(in)  :: me          !< material data (elasticity)
   type(s_material_shear), intent(in)    :: ms12        !< material data (shear 12)
   type(s_material_shear), intent(in)    :: ms23        !< material data (shear 23)
   type(s_material_shear), intent(in)    :: ms13        !< material data (shear 13)
   real(DP), dimension(nblock,*), intent(in)    :: stateOld    !< material state (previous increment)
   real(DP), dimension(nblock,*), intent(inout) :: stateNew    !< material state (current increment)
   real(DP), dimension(nblock,*), intent(inout) :: stressNew   !< material state (current increment)
   
   ! local vars
   type(s_stensor)         :: s       !< stress (1,2,3)
   type(s_stensor)         :: sr      !< stress (1,N,T)
   type(s_bbox)            :: bb      !< bounding box
   type(s_zeros)           :: z       !< zeros of each term of Puck's criterion
   type(s_fstate)          :: fs      !< failure state
   type(s_matrix_state)    :: st0     !< matrix state (previous increment)
   type(s_matrix_state)    :: st1     !< matrix state (current increment)
   type(s_stensor)         :: eE      !< elastic strain tensor (1,2,3)  
   type(s_stensor)         :: eEr     !< elastic strain tensor (1,N,T) 
   type(s_stensor)         :: eI      !< inelastic strain tensor (1,2,3)  
   type(s_stensor)         :: eIr     !< inelastic strain tensor (1,N,T)  
   real(DP)                :: epsER   !< resulting elastic strain on the fracture plane
   real(DP)                :: epsIR   !< resulting inelastic strain on the fracture plane
   real(DP)                :: area, A1, A2, A3, gam
   integer                 :: k
    
   do k=1, nblock
   
      call K_STATE2SMATRIX(nblock, k, stateOld, st0)
      call K_STATE2SMATRIX(nblock, k, stateNew, st1)
      
      st0%C%L  = stateNew(k, ST_LENGTH)
      st1%C%L  = stateNew(k, ST_LENGTH)
      
      ! effective stress state 
      call K_VEC2STENSOR(nblock, k, stressNew, s)
      
      ! check failure criterion
      if(st0%d==zero) then
         call K_COMPUTE_BBOX(s, bb) 
         call K_FIND_ZEROS(s, z) 
         call K_CHECK_PUCK_CRITERION(s, mm, z, bb, fs)
         if(fs%failure) then
            st1%thetaC = fs%tm    ! set the fracture angle
         else
            call K_SMATRIX2STATE(nblock, k, st1, stateNew)  ! is this useful?
            cycle ! criterion not reached - exit
         end if
      else
         ! use the previous angle
         st1%thetaC = st0%thetaC
      end if
      
      ! current elastic strain
      eE%s11 = stateNew(k,S_11)
      eE%s22 = stateNew(k,S_22)
      eE%s33 = stateNew(k,S_33)
      eE%s12 = stressNew(k,S_12)/two/me%G12
      eE%s23 = stressNew(k,S_23)/two/me%G23
      eE%s13 = stressNew(k,S_13)/two/me%G13
      
      ! current inelastic strain
      eI%s11 = zero
      eI%s22 = zero
      eI%s33 = zero
      eI%s12 = stateNew(k,ST_EPS_INEL_12)
      eI%s23 = stateNew(k,ST_EPS_INEL_23)
      eI%s13 = stateNew(k,ST_EPS_INEL_13)
      
      ! rotation of tensors 
      call K_ROTTENSOR(eE, st1%thetaC, eEr)       ! strains: elastic part
      call K_ROTTENSOR(eI, st1%thetaC, eIr)       ! strains: inelastic part
      call K_ROTTENSOR(s, st1%thetaC, sr)         ! stresses

      ! combined strains (always>0) 
      epsER  = sqrt(max(zero,eEr%s22)**2+(two*eEr%s23)**2+(two*eEr%s12)**2)
      epsIR  = sqrt(max(zero,eIr%s22)**2+(two*eIr%s23)**2+(two*eIr%s12)**2)
      
      ! damage initiation
      if(st1%C%eF==zero) then
         st1%C%e0 = epsIR+epsER
         st1%C%X  = sqrt(max(zero,sr%s22)**2+(sr%s23)**2+(sr%s12)**2) ! resulting stress, always >0
         ! combination of fracture energies (see Raimondo)
         if(sr%s22>=0) then
            st1%C%G = mm%G2T
         else
            st1%C%G = mm%G2C
         end if
         st1%C%G = st1%C%G *(sr%s22/st1%C%X)**2 &
                 + mm%G23   *(sr%s23/st1%C%X)**2 &
                 + mm%G12   *(sr%s12/st1%C%X)**2
         ! strain at complete failure (eF)       
         gam = abs(eEr%s22)
         A1 = (gam*st1%C%X)/two
         gam = two*(eIr%s23+abs(eEr%s23))
         A2 =  ms23%c1/four *gam**4  - ms23%c2/three*gam**3  + ms23%c3/two*gam**2
         gam = two*(eIr%s12+abs(eEr%s12))
         A3 =  ms12%c1/four *gam**4  - ms12%c2/three*gam**3  + ms12%c3/two*gam**2
         area = A1 *(sr%s22/st1%C%X)**2 &
              + A2 *(sr%s23/st1%C%X)**2 &
              + A3 *(sr%s12/st1%C%X)**2
         st1%C%eF = two/st1%C%X * (st1%C%G/st1%C%L-area) + st1%C%e0
         
         if(st1%C%eF<=st1%C%e0) then
            call K_BILINEAR_LAW_ERROR("matrix", st1%C)
            ! the cleanest way would be to stop the computation and refine the mesh
            ! for practical reason, we mark the element as bad and we continue the simulation
            st1%C%eF = st1%C%e0+tiny(one)
            st1%C%bad = one
         end if 
      end if
        
      call K_BILINEAR_LAW(st1%C, epsIR+epsER, epsIR, st0%d, st1%d) ! update st1%d
      if(st1%d==zero) st1%d=tiny(one) ! otherwise, it does NOT increase during initiation

      ! apply matrix damage to the rotated effective stress tensor
      sr%s12 = (one-st1%d)*sr%s12
      sr%s23 = (one-st1%d)*sr%s23
      if(sr%s22>zero) then
         sr%s22 = (one-st1%d)*sr%s22
      endif
      
      ! stress rotation to the (1,2,3) system
      call K_ROTTENSOR(sr, -st1%thetaC, s)
    
      ! save the current state/stresses to abaqus arrays
      call K_STENSOR2VEC(nblock, k, s, stressNew)
      call K_SMATRIX2STATE(nblock, k, st1, stateNew)
   
   end do
   
end subroutine K_MATRIX_DAMAGE
