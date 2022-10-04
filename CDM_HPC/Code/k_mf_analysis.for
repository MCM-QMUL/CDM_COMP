! $Id: k_mf_analysis.for 1045 2012-08-17 01:54:27Z boman $
!> \ingroup grpprogs
!> Test routine.
!> Calls K_CHECK_PUCK_CRITERION() for a given stress state

include 'k_prototypes.inc'

subroutine K_MF_ANALYSIS(sig22, sig33, sig12, sig13, sig23, S12, Yc, Yt)
   !DEC$ ATTRIBUTES DLLEXPORT::K_MF_ANALYSIS
   use K_IDM
   use K_PROTOTYPES, only : K_MIN_BRENT
   implicit none
   real(DP), intent(in) :: sig22, sig33, sig12, sig13, sig23, S12, Yc, Yt

   ! local vars
   type(s_material_matrix) :: m
   type(s_stensor)          :: s
   type(s_bbox)            :: bb
   type(s_zeros)           :: z
   type(s_fstate)          :: fs

   ! fill the material struct
   m%Yt  = Yt;
   m%Yc  = Yc;
   m%S12 = S12;
   m%tf  = HCP_THETAF*pi/180.0_dp

   m%S23  = m%Yc/(two*tan(m%tf))
   m%munt = -one/tan(two*m%tf)
   m%mu1n = m%S12/m%S23*m%munt;

   ! fill the stress state
   s%s11 = 0.   ! useless
   s%s22 = sig22
   s%s33 = sig33
   s%s12 = sig12
   s%s13 = sig13
   s%s23 = sig23

   print "('Input values')"
   print "('  s22 =',F8.2)", s%s22
   print "('  s33 =',F8.2)", s%s33
   print "('  s12 =',F8.2)", s%s12
   print "('  s13 =',F8.2)", s%s13
   print "('  s23 =',F8.2)", s%s23
   print "('  S12 =',F8.2)", m%S12
   print "('  Yc  =',F8.2)", m%Yc
       
   print "('Computed values')"
   print "('  S23  =',F8.2)", m%S23
   print "('  munt =',F8.2)", m%munt
   print "('  mu1n =',F8.2)", m%mu1n
   
   call K_COMPUTE_BBOX(s, bb) 
   call K_FIND_ZEROS(s, z) 
   call K_CHECK_PUCK_CRITERION(s, m, z, bb, fs)
   
end subroutine K_MF_ANALYSIS
