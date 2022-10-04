! $Id: k_element_deletion.inc 1051 2012-09-04 06:41:05Z boman $
!> \ingroup grptools
!> Checks if the current element should be deleted or not.      
!>
!> The current element is deleted if any damage variable 
!> (\f$ d_{11}^C \f$, \f$ d_{11}^T \f$ or \f$ d_{2} \f$)
!> is greater than the hard-coded parameter \ref k_idm::hcp_dmax

subroutine K_ELEMENT_DELETION(nblock, stateNew)
   use K_IDM
   implicit none 
   integer, intent(in) :: nblock                                !< number of material points
   real(DP), dimension(nblock, *), intent(inout) :: stateNew    !< SDVs (current increment)
   integer :: k
   
   do k=1, nblock
      if(stateNew(k,ST_D11T)>HCP_DMAX .or. &
         stateNew(k,ST_D11C)>HCP_DMAX ) then
         stateNew(k,ST_ACTIVE) = zero
      end if
   end do

end subroutine K_ELEMENT_DELETION
