! $Id: k_prototypes.inc 1045 2012-08-17 01:54:27Z boman $
!> \ingroup grpmain
!> Module K_PROTOTYPES - contains function prototypes
!!
!! Ideally, all routines should be declared like this
!! (allows the compiler to check the calls during compilation)

module K_PROTOTYPES
   use K_IDM
   implicit none

   interface
      subroutine K_MIN_BRENT(ax, bx, cx, FUNC, tol, verb, xmin, fmin, success)
         use K_IDM
         implicit none
         real(DP), intent(in)  :: ax, bx, cx, tol
         real(DP), intent(out) :: xmin, fmin
         logical, intent(out)  :: success
         logical, intent(in)   :: verb
         interface
            function FUNC(x)
            use K_IDM
            implicit none
            real(DP), intent(in) :: x
            real(DP) :: FUNC
            end function FUNC
         end interface
      end subroutine K_MIN_BRENT
   end interface   
     
end module K_PROTOTYPES
