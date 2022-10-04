! $Id: k_fill_material_matrix.inc 1055 2012-09-11 00:40:51Z boman $

!> \ingroup grptools
!> Copy the matrix damage parameters from Abaqus to
!> a k_idm::s_material_matrix structure

subroutine K_FILL_MATERIAL_MATRIX(props, m)
   use K_IDM
   implicit none
   
   real(DP), dimension(*), intent(in)   :: props  !< Abaqus array of properties
   type(s_material_matrix), intent(out) :: m      !< target data structure
   
   m%Yt  = props(PR_YT)
   m%Yc  = props(PR_YC)
   m%S12 = props(PR_S12)
   m%tf  = HCP_THETAF*pi/180.0_dp
   m%G2T = props(PR_G2T)
   m%G2C = props(PR_G2C)
   m%G23 = props(PR_GSHEAR_23)
   m%G12 = props(PR_GSHEAR_12)
   
   ! computed values
   m%S23  = m%Yc/(two*tan(m%tf))
   m%munt = -one/tan(two*m%tf)
   m%mu1n = m%S12/m%S23*m%munt 
   
end subroutine K_FILL_MATERIAL_MATRIX

