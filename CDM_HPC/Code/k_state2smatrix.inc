! $Id: k_state2smatrix.inc 1056 2012-09-13 05:54:58Z boman $

!> \ingroup grptools
!> Copy the state parameters related to matrix damage from Abaqus to 
!> a k_idm::s_matrix_state structure

subroutine K_STATE2SMATRIX(nblock, k, state, st)
   use K_IDM
   implicit none
   
   integer, intent(in)                       :: nblock  !< number of material points
   integer, intent(in)                       :: k       !< material point index
   real(DP), dimension(nblock,*), intent(in) :: state   !< Abaqus state array
   type(s_matrix_state), intent(out)         :: st      !< target data structure
   
   st%thetaC  = state(k,ST_THETAC)
   st%d       = state(k,ST_MC_D)
   st%C%e0    = state(k,ST_MC_EPS0)
   st%C%X     = state(k,ST_MC_X)   
   st%C%eF    = state(k,ST_MC_EPSF)
   st%C%bad   = state(k,ST_BAD)
   
end subroutine K_STATE2SMATRIX
