! $Id: k_smatrix2state.inc 1056 2012-09-13 05:54:58Z boman $

!> \ingroup grptools
!> Copy a k_idm::s_matrix_state stucture to the Abaqus state array

subroutine K_SMATRIX2STATE(nblock, k, st, state)
   use K_IDM
   implicit none
   
   integer, intent(in)                        :: nblock  !< number of material points
   integer, intent(in)                        :: k       !< material point index
   type(s_matrix_state), intent(in)           :: st      !< state structure related to matrix damage
   real(DP), dimension(nblock,*), intent(out) :: state   !< Abaqus state array
   
   state(k,ST_THETAC)      = st%thetaC 
   state(k,ST_MC_D)        = st%d  
   state(k,ST_MC_EPS0)     = st%C%e0
   state(k,ST_MC_X)        = st%C%X
   state(k,ST_MC_EPSF)     = st%C%eF
   state(k,ST_BAD)         = st%C%bad
   
end subroutine K_SMATRIX2STATE

