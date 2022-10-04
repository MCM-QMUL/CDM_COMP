! $Id: k_min_brent.inc 1055 2012-09-11 00:40:51Z boman $
!> \ingroup grpmatrix
!> Brent minimisation algorithm
!! (adapted by RB from "numerical recipes")
!!
!! Given a function func, and given a bracketing triplet of abscissas ax, bx, cx (such that bx
!! is between ax and cx, and func(bx) is less than both func(ax) and func(cx)), this
!! routine isolates the minimum to a fractional precision of about tol using Brent's method.
!! The abscissa of the minimum is returned as xmin, and the minimum function value is
!! returned as brent, the returned function value.
!! Parameters: Maximum allowed number of iterations; golden ratio; and a small number that
!! protects against trying to achieve fractional accuracy for a minimum that happens to be
!! exactly zero.
!!
!> @todo if it fails near a boundary, check the value of boundary

subroutine K_MIN_BRENT(ax, bx, cx, FUNC, opts, xmin, fmin, success)
   use K_IDM
   implicit none
   real(DP), intent(in)           :: ax       !< initial bracket (point #1)
   real(DP), intent(in)           :: bx       !< initial bracket (point #2)
   real(DP), intent(in)           :: cx       !< initial bracket (point #3)
   type(s_brent_opts), intent(in) :: opts     !< options
   real(DP), intent(out)          :: xmin     !< x value of the minimum
   real(DP), intent(out)          :: fmin     !< value of FUNC at the minimum
   logical, intent(out)           :: success  !< success flag
   interface
      !> function to be minimised
      function FUNC(x)
      use K_IDM
      implicit none
      real(DP), intent(in) :: x
      real(DP) :: FUNC
      end function FUNC
   end interface
   
   real(DP), parameter     :: cgold = 0.3819660_dp
   real(DP), parameter     :: zeps  = 1.0e-3_dp*epsilon(ax)
   integer                 :: iter
   real(DP)                :: a, b, d, e, etemp, fu, fv, fw, fx
   real(DP)                :: p, q, r, tol1, tol2, u, v, w, x, xm
   character(len=20)       :: methtxt
   
   if (opts%verb) print "('Brent minimisation')"
   a = min(ax,cx)
   b = max(ax,cx)
   v = bx
   w = v
   x = v
   e = 0.0
   fx = func(x)
   fv = fx
   fw = fx
   do iter=1,opts%itmax
      xm = 0.5_dp*(a+b)
      tol1 = opts%tol*abs(x)+zeps
      tol2 = two*tol1
      if (abs(x-xm) <= (tol2-0.5_dp*(b-a))) then ! Construct a trial parabolic fit.
         xmin = x
         fmin = fx
         success = .true.
         return
      end if
      if (abs(e) > tol1) then
         r = (x-w)*(fx-fv)
         q = (x-v)*(fx-fw)
         p = (x-v)*q-(x-w)*r
         q = two*(q-r)
         if (q > zero) p = -p
         q = abs(q)
         etemp = e
         e = d
         if (abs(p) >= abs(0.5_dp*q*etemp) .or. &
            p <= q*(a-x) .or. p >= q*(b-x)) then
            ! The above conditions determine the acceptability of the parabolic fit. Here it is
            ! not o.k., so we take the golden section step into the larger of the two segments.
            e = merge(a-x,b-x, x >= xm )
            d = cgold*e
            if (opts%verb) methtxt = "golden"
         else  ! Take the parabolic step.
            d = p/q
            u = x+d
            if (u-a < tol2 .or. b-u < tol2) d=sign(tol1,xm-x)
            if (opts%verb) methtxt = "parabolic"
         end if
      else  ! Take the golden section step into the larger of the two segments.
         e = merge(a-x,b-x, x >= xm )
         d = cgold*e
         if (opts%verb) methtxt = "golden"
      end if
      u = merge(x+d,x+sign(tol1,d), abs(d) >= tol1 )
      ! Arrive here with d computed either from parabolic fit, or else from golden section
      fu = FUNC(u) ! This is the one function evaluation per iteration.
      if (opts%verb) print  "('  ',I3,': x=',G12.6,' f(x)=',G12.6,' e=',G12.6,' (',A,')')", iter, u, fu, e, trim(methtxt)
      if (fu <= fx) then ! Now we have to decide what to do with our function evaluation.
         if (u >= x) then
            a = x
         else
            b = x
         end if
         call K_SHFT(v, w, x, u)
         call K_SHFT(fv, fw, fx, fu)
      else
         if (u < x) then
            a = u
         else
            b = u
         end if
         if (fu <= fw .or. w == x) then
            v  = w
            fv = fw
            w  = u
            fw = fu
         else if (fu <= fv .or. v == x .or. v == w) then
            v  = u
            fv = fu
         end if
      end if
   end do
   
   !xmin = zero ! orig
   !fmin = zero ! orig

   xmin    = x  ! mod. RB
   fmin    = fx ! mod. RB
   success = .false.
   
contains
   !> (a,b,c) = (b,c,d)
   subroutine K_SHFT(a,b,c,d)
      real(DP), intent(out)   :: a 
      real(DP), intent(inout) :: b
      real(DP), intent(inout) :: c
      real(DP), intent(in)    :: d
      a = b
      b = c
      c = d
   end subroutine K_SHFT
   
end subroutine K_MIN_BRENT
