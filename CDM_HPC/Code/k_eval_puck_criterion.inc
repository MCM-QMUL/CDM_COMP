! $Id: k_eval_puck_criterion.inc 1051 2012-09-04 06:41:05Z boman $
!> \ingroup grpmatrix
!> Evaluate Puck's criterion for a given stress state and an angle

!> @todo could be improved by pre-computing cos and sin of t
!> @todo could use ROTTENSOR

subroutine K_EVAL_PUCK_CRITERION(t, s, m, r)
   use K_IDM
   implicit none
   
   real(DP)                            :: t   !< angle
   type(s_stensor), intent(in)         :: s   !< stress state
   type(s_material_matrix), intent(in) :: m   !< material data
   type(s_rstress), intent(out)        :: r   !< rotated stress

   r%s1n =  s%s12*cos(t)+s%s13*sin(t)
   !r%s1t = -s%s12*sin(t)+s%s13*cos(t)  ! useless
   r%snn = s%s22*cos(t)*cos(t)+s%s33*sin(t)*sin(t)+s%s23*sin(2*t)
   !r%stt = s%s22*sin(t)*sin(t)+s%s33*cos(t)*cos(t)-s%s23*sin(2*t) ! useless
   r%snt = (s%s33-s%s22)/two*sin(2*t)+s%s23*cos(2*t);

   r%F1 = (r%snt/(m%S23-m%munt*min(r%snn,zero)))**2
   r%F2 = (r%s1n/(m%S12-m%mu1n*min(r%snn,zero)))**2
   r%F3 = (max(r%snn,zero)/m%Yt)**2
   r%F = r%F1+r%F2+r%F3

end subroutine K_EVAL_PUCK_CRITERION
