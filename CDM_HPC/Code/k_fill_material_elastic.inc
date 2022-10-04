! $Id: k_fill_material_elastic.inc 1055 2012-09-11 00:40:51Z boman $

!> \ingroup grptools
!> Copy the elastic parameters from Abaqus to
!> a k_idm::s_material_elastic structure

subroutine K_FILL_MATERIAL_ELASTIC(props, m)
   use K_IDM
   implicit none
   
   real(DP), dimension(*), intent(in)    :: props  !< Abaqus array of properties
   type(s_material_elastic), intent(out) :: m      !< target data structure
   
   m%E1     = props(PR_E1)
   m%E2     = props(PR_E2)
   m%E3     = props(PR_E3)  
   m%nu12   = props(PR_NU12)
   m%nu23   = props(PR_NU23) 
   m%nu13   = props(PR_NU13)
   m%G12    = props(PR_G12)
   m%G23    = props(PR_G23)
   m%G13    = props(PR_G13)
   
   ! computed values
   m%nu21 = m%nu12*(m%E2/m%E1)
   m%nu31 = m%nu13*(m%E3/m%E1)
   m%nu32 = m%nu23*(m%E3/m%E2)
   
end subroutine K_FILL_MATERIAL_ELASTIC
