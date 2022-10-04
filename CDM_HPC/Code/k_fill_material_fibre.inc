! $Id: k_fill_material_fibre.inc 1055 2012-09-11 00:40:51Z boman $

!> \ingroup grptools
!> Copy the fibre damage parameters from Abaqus to 
!> a k_idm::s_material_fibre structure

subroutine K_FILL_MATERIAL_FIBRE(props, m)
   use K_IDM
   implicit none
   
   real(DP), dimension(*), intent(in)  :: props  !< Abaqus array of properties
   type(s_material_fibre), intent(out) :: m      !< target data structure
   
   ! tension
   m%T%X     = props(PR_XT)   
   m%T%G     = props(PR_G1T)  
   m%T%e0    = props(PR_XT)/props(PR_E1)
   m%T%eF    = zero 
   
   ! compression
   m%C%X     = props(PR_XC)
   m%C%G     = props(PR_G1C)
   m%C%e0    = props(PR_XC)/props(PR_E1)
   m%C%eF    = zero 
   
end subroutine K_FILL_MATERIAL_FIBRE
