! $Id: k_char_length.inc 1051 2012-09-04 06:41:05Z boman $
!> \ingroup grptools
!> Compute and store the characteristic length       

subroutine K_CHAR_LENGTH(nblock, nstatev, stateOld, stateNew, charLength)
   use K_IDM
   implicit none 
   integer, intent(in)                                :: nblock     !< number of material points
   integer, intent(in)                                :: nstatev    !< number of SDVs                 
   real(DP), dimension(nblock,nstatev), intent(in)    :: stateOld   !< SDVs (previous increment)
   real(DP), dimension(nblock,nstatev), intent(inout) :: stateNew   !< SDVs (current increment)
   real(DP), dimension(nblock), intent(in)            :: charLength !< Characteristic lengths from Abaqus
   real(DP) :: length
   integer :: k
   
   do k=1, nblock
      length = stateOld(k,ST_LENGTH)
      if (length==zero) then
         length = charLength(k)
      end if
      stateNew(k,ST_LENGTH) = length   ! is always the INITIAL length 
   end do
   
   
end subroutine K_CHAR_LENGTH
 