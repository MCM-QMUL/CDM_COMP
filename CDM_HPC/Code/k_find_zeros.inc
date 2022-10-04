! $Id: k_find_zeros.inc 1051 2012-09-04 06:41:05Z boman $
!> \ingroup grpmatrix
!> Calculate the zeros of each term of Puck's failure criterion 

subroutine K_FIND_ZEROS(s, z)
   use K_IDM
   implicit none
   
   type(s_stensor), intent(in) :: s
   type(s_zeros), intent(out)  :: z
   
   real(DP) :: rho, ex1, ex2 
   
    ! -- first term (snt) - 2 roots
    z%F1z1 = 0.5_dp*atan2(two*s%s23,s%s22-s%s33) ! in [-pi/2,pi/2]
    if(z%F1z1<zero) then
        z%F1z1 = z%F1z1+pi/two ! in [0,pi/2]
    end if
    z%F1z2 = z%F1z1+pi/two ! in [pi/2,pi]
 
    ! -- second term (s1n) - 1 root
    z%F2z1 = atan2(-s%s12,s%s13) ! in [-pi,pi]
    if(z%F2z1<zero) then
        z%F2z1 = z%F2z1+pi ! in [0,pi]
    end if

    ! -- third term (snn) - 0, 1 or 2 roots
    if(s%s33/=zero) then
        rho = s%s23*s%s23-s%s22*s%s33
        if(rho>=zero) then ! 2 roots
            z%F3z1 = atan2((-s%s23-sqrt(rho)),s%s33) ! in [-pi,pi]
            z%F3z2 = atan2((-s%s23+sqrt(rho)),s%s33) ! in [-pi,pi]
        else ! 0 roots
            ! extrema
            ex1 = 0.5*atan2(two*s%s23,s%s22-s%s33)      ! in [-pi/2,pi/2]               
            ex2 = ex1+pi/two                       ! in [0,pi]
            z%F3z1 = ex1+pi/four                    ! in [-pi/4,pi/4]
            z%F3z2 = ex2+pi/four                    ! in [pi/4,3*pi/4]
            if(z%F3z1<zero) then
                z%F3z1 = z%F3z1+pi/four             ! in [0,pi/2]
                z%F3z2 = z%F3z2+pi/four             ! in [pi/2,pi]
            end if  
        end if
    else if(s%s23/=zero) then ! 2 roots
        z%F3z1 = atan2(-s%s22,(2*s%s23))      ! in [-pi,pi]
        z%F3z2 = pi/two
    else ! 1 root
        z%F3z1 = pi/two
        z%F3z2 = pi/two
    end if
    
    if(z%F3z1<zero) then
        z%F3z1 = z%F3z1+pi
    end if
    if(z%F3z2<zero) then
        z%F3z2 = z%F3z2+pi
    end if 
    
    ! sort the roots
    if(z%F3z1>z%F3z2) then
        call K_SWAP(z%F3z1, z%F3z2)
    end if

   !call K_PRINT_ZEROS(z)

contains

   subroutine K_SWAP(a, b)
      real(DP), intent(inout) :: a,b
      real(DP) :: tmp
      tmp = a
      a   = b
      b   = tmp
   end subroutine K_SWAP

   subroutine K_PRINT_ZEROS(z)
      use K_IDM
      implicit none
      type(s_zeros), intent(in) :: z
      print "('Zeros of each term')"
      print "('  F1z1 =',F8.4, ' (=',F8.4,' deg.)')", z%F1z1, z%F1z1*180.0_dp/pi
      print "('  F1z2 =',F8.4, ' (=',F8.4,' deg.)')", z%F1z2, z%F1z2*180.0_dp/pi
      print "('  F2z1 =',F8.4, ' (=',F8.4,' deg.)')", z%F2z1, z%F2z1*180.0_dp/pi
      print "('  F3z1 =',F8.4, ' (=',F8.4,' deg.)')", z%F3z1, z%F3z1*180.0_dp/pi
      print "('  F3z2 =',F8.4, ' (=',F8.4,' deg.)')", z%F3z2, z%F3z2*180.0_dp/pi

   end subroutine K_PRINT_ZEROS

end subroutine K_FIND_ZEROS
