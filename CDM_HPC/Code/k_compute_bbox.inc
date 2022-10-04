! $Id: k_compute_bbox.inc 1045 2012-08-17 01:54:27Z boman $
!> \ingroup grpmatrix
!> Compute the Bounding box of the stress in the (snn, snt, s1n) space

subroutine K_COMPUTE_BBOX(s, bb)
   use K_IDM
   implicit none
   type(s_stensor), intent(in) :: s
   type(s_bbox), intent(out)   :: bb
   real(DP) :: rho 
   
   rho = sqrt( ((s%s22-s%s33)/two)**2+s%s23**2 )

   bb%snn%min = (s%s22+s%s33)/two-rho
   bb%snn%max = (s%s22+s%s33)/two+rho
   bb%snt%min = -rho
   bb%snt%max = rho
   bb%s1n%min = -sqrt(s%s12**2+s%s13**2)
   bb%s1n%max = -bb%s1n%min

   !call K_PRINT_BB(bb)

end subroutine K_COMPUTE_BBOX
