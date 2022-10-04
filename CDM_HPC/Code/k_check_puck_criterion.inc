! $Id: k_check_puck_criterion.inc 1051 2012-09-04 06:41:05Z boman $
!> \ingroup grpmatrix
!> Search for the maxima of the matrix failure criterion. 
!> Then, check if failure has occurred
!> (the results are stored in a s_fstate structure)
!!
!! \pre the zeros of the failure criterion and the bounding box must be computed first
!!      using k_find_zeros() and k_compute_bbox()
!!
!! \see k_find_zeros k_compute_bbox

subroutine K_CHECK_PUCK_CRITERION(s, m, z, bb, fs)
   use K_IDM
   implicit none
   
   type(s_stensor), intent(in)         :: s    !< stress state
   type(s_material_matrix), intent(in) :: m    !< material data
   type(s_zeros), intent(in)           :: z    !< zeros of each terms
   type(s_bbox), intent(in)            :: bb   !< bounding box
   type(s_fstate), intent(out)         :: fs   !< failure state
   
   
   real(DP) :: F1bb, F2bb, F3bb, Fbb
   real(DP) :: b1, b2, b3
   type(s_rstress) :: r
   logical  :: success
   integer  :: i
   real(DP) :: mxf,mxt
   type(s_brent_opts) :: opts

   ! Evaluate the criterion at bbox corner    
   F1bb = (bb%snt%max/(m%S23-m%munt*min(bb%snn%max,zero)))**2
   F2bb = (bb%s1n%max/(m%S12-m%mu1n*min(bb%snn%max,zero)))**2
   F3bb = (max(bb%snn%max,zero)/m%Yt)**2   
   Fbb = F1bb+F2bb+F3bb

   ! initialise the structure
   ! todo: fill fmax,tmax with 0
   fs%failure = .false.
   fs%fm = 0
   fs%tm = 0
   fs%n  = 0

   ! bounding box test
   if(Fbb<one .and. .false.) then
      ! no failure for any angle (for sure)
      return
   else 
      ! enable this in order to see the shape of the bb zone
      if(.false.) then
         fs%failure = .true.
         fs%fm      = one
         fs%tm      = zero
         fs%fmax(1) = fs%fm
         fs%tmax(1) = fs%tm
         fs%n       = 1
         return
      end if
   end if
   
   ! setup Brent's algorithm
   opts%tol   = HCP_BRENT_TOL  
   opts%itmax = HCP_BRENT_ITMAX 
   opts%verb  = .false.

   ! search for the two maxima produced by the snt term    
   b1 = z%F1z2-pi
   b2 = z%F1z1
   b3 = z%F1z2
   call K_MIN_BRENT(b1, (b1+b2)/2, b2, MFAILUREF, opts, fs%tmax(1), fs%fmax(1), success)
   fs%n = fs%n+1
    
   if(fs%tmax(1)<zero) then
      fs%tmax(1) = fs%tmax(1)+pi
   end if
   call K_MIN_BRENT(b2, (b2+b3)/2, b3, MFAILUREF, opts, fs%tmax(2), fs%fmax(2), success)
   fs%n = fs%n+1

   ! if there is some tension - snn term may be dominant 
   if(F3bb>zero) then
      b1 = z%F3z2-pi
      b2 = z%F3z1
      b3 = z%F3z2
      call K_EVAL_PUCK_CRITERION((b1+b2)/2,s,m,r)
      if(r%F3>zero) then  ! check where the snn term is not 0
         call K_MIN_BRENT(b1, (b1+b2)/2, b2, MFAILUREF, opts, fs%tmax(3), fs%fmax(3), success)
         fs%n = fs%n+1
         if (fs%tmax(3)<zero) then
            fs%tmax(3) = fs%tmax(3)+pi
         end if
      else
         call K_MIN_BRENT(b2, (b2+b3)/2, b3, MFAILUREF, opts, fs%tmax(3), fs%fmax(3), success)
         fs%n = fs%n+1
      end if
   end if 
    
   ! we have found the minima of -myf
   do i=1,fs%n
      fs%fmax(i) = -fs%fmax(i)
   end do
    
   ! Extract the max (only one of them if double or triple)
   mxf = fs%fmax(1)
   mxt = fs%tmax(1)
   do i=2,fs%n
      if (fs%fmax(i)>mxf) then
         mxf = fs%fmax(i)
         mxt = fs%tmax(i)
      end if
   end do
   !print *,"mxf=", mxf, "mxt=",mxt  
    
   ! Extract the max with smallest t
   fs%fm = mxf
   fs%tm = mxt
   do i=1,fs%n
      if (abs(fs%fmax(i)-mxf)<0.01_dp) then   ! (mxf, not fs%fm!)
         if (fs%tmax(i)<fs%tm) then
            fs%fm = fs%fmax(i)
            fs%tm = fs%tmax(i)
         end if
      end if
   end do

   ! Check failure
   if(fs%fm>=one) then
      fs%failure = .true.
   else
      fs%failure = .false.
   end if

   !call K_PRINT_FSTATE(fs)

contains

   function MFAILUREF(t)
      implicit none
      real(DP)             :: MFAILUREF
      real(DP), intent(in) :: t
      call K_EVAL_PUCK_CRITERION(t,s,m,r)
      MFAILUREF = -r%F
   end function MFAILUREF    

end subroutine K_CHECK_PUCK_CRITERION
