! $Id: k_update_bilinear_law.inc 1056 2012-09-13 05:54:58Z boman $
!> \ingroup grpfibre
!> Update the element-dependent material data

subroutine K_UPDATE_BILINEAR_LAW(nblock, text, law, stateNew, k)   
   use K_IDM
   implicit none  
   
   integer, intent(in)                           :: nblock
   character(len=*), intent(in)                  :: text
   type(s_bilinear_damage), intent(inout)        :: law
   real(DP), dimension(nblock, *), intent(inout) :: stateNew
   integer, intent(in) :: k
   
   law%L  = stateNew(k, ST_LENGTH)
   law%eF = (two*law%G)/(law%X*law%L) ! recomputed since it is not stored in the material state
   if (law%eF <= law%e0) then
      call K_BILINEAR_LAW_ERROR(text, law)
      law%eF  = law%e0+tiny(one)
      law%bad = one
      stateNew(k,ST_BAD) = one
   end if
end subroutine K_UPDATE_BILINEAR_LAW

