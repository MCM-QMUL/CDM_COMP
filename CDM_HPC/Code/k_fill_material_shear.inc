! $Id: k_fill_material_shear.inc 1055 2012-09-11 00:40:51Z boman $

!> \ingroup grptools
!> Copy the shear damage parameters from Abaqus to 
!> k_idm::s_material_shear structures

subroutine K_FILL_MATERIAL_SHEAR(props, m12, m23, m13)
   use K_IDM
   implicit none
   real(DP), dimension(*), intent(in)  :: props !< Abaqus array of properties
   type(s_material_shear), intent(out) :: m12   !< target data structure (shear 12)
   type(s_material_shear), intent(out) :: m23   !< target data structure (shear 23)
   type(s_material_shear), intent(out) :: m13   !< target data structure (shear 13)

   m12%gamma_0    = props(PR_GAMMA_0_12)
   m12%gamma_max  = props(PR_GAMMA_MAX_12)
   m12%Gshear     = props(PR_GSHEAR_12)
   m12%a          = props(PR_A_12)
   m12%c1         = props(PR_C1_12)
   m12%c2         = props(PR_C2_12)
   m12%c3         = props(PR_C3_12)

   m23%gamma_0    = props(PR_GAMMA_0_23)
   m23%gamma_max  = props(PR_GAMMA_MAX_23)
   m23%Gshear     = props(PR_GSHEAR_23)
   m23%a          = props(PR_A_23)
   m23%c1         = props(PR_C1_23)
   m23%c2         = props(PR_C2_23)
   m23%c3         = props(PR_C3_23)

   m13%gamma_0    = props(PR_GAMMA_0_13)
   m13%gamma_max  = props(PR_GAMMA_MAX_13)
   m13%Gshear     = props(PR_GSHEAR_13)
   m13%a          = props(PR_A_13)
   m13%c1         = props(PR_C1_13)
   m13%c2         = props(PR_C2_13)
   m13%c3         = props(PR_C3_13)
   
end subroutine K_FILL_MATERIAL_SHEAR
