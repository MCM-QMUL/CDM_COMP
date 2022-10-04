! $Id: k_rottensor.inc 1051 2012-09-04 06:41:05Z boman $
!> \ingroup grpmatrix
!> Rotate a symetric tensor around 1-axis
!!
!! \f$ 
!! \left(
!!   \begin{array}{ccc}
!!      r_{11} & r_{12} & r_{13}  \\
!!      r_{12} & r_{22} & r_{23}  \\
!!      r_{13} & r_{23} & r_{33}  
!!   \end{array}
!! \right)
!! =
!! \left(
!!   \begin{array}{ccc}
!!      1 & 0 & 0  \\
!!      0 & \cos t & \sin t  \\
!!      0 & -\sin t & \cos t  \\
!!   \end{array}
!! \right)
!! \left(
!!   \begin{array}{ccc}
!!      s_{11} & s_{12} & s_{13}  \\
!!      s_{12} & s_{22} & s_{23}  \\
!!      s_{13} & s_{23} & s_{33}
!!   \end{array}
!! \right)
!! \left(
!!   \begin{array}{ccc}
!!      1 & 0 & 0  \\
!!      0 & \cos t & -\sin t  \\
!!      0 & \sin t & \cos t  \\
!!   \end{array}
!! \right)
!! \f$
!!
!> @todo is the compiler smart enough to create temp variables for sin(t) and cos(t)??

subroutine K_ROTTENSOR(s, t, r)
   use K_IDM
   implicit none

   type(s_stensor), intent(in)  :: s  !< source tensor
   real(DP), intent(in)         :: t  !< angle
   type(s_stensor), intent(out) :: r  !< destination tensor
   real(DP) :: cost, sint
   real(DP) :: cost2, sint2
   real(DP) :: cos2t, sin2t

   cost = cos(t)
   sint = sin(t)
   cost2 = cost*cost
   sint2 = sint*sint
   cos2t = cost2-sint2
   sin2t = two*cost*sint
      
   r%s11 = s%s11
   r%s12 =  s%s12*cost+s%s13*sint
   r%s13 = -s%s12*sint+s%s13*cost 
   r%s22 = s%s22*cost2+s%s33*sint2+s%s23*sin2t
   r%s33 = s%s22*sint2+s%s33*cost2-s%s23*sin2t
   r%s23 = (s%s33-s%s22)/two*sin2t+s%s23*cos2t

end subroutine K_ROTTENSOR 
