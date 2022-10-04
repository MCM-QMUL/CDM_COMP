! $Id: k_biaxial.for 1045 2012-08-17 01:54:27Z boman $
!> \ingroup grpprogs
!> Biaxial test

subroutine K_BIAXIAL(sig22, sig33, sig12, sig13, sig23, S12, Yc, Yt)
   !DEC$ ATTRIBUTES DLLEXPORT::K_BIAXIAL
   use K_IDM
   implicit none
   real(DP), intent(in) :: sig22, sig33, sig12, sig13, sig23, S12, Yc, Yt

   ! local vars
   type(s_material_matrix) :: m
   type(s_stensor)          :: s

   integer, parameter            :: ntheta = 10000
   real(DP), parameter           :: range  = 2.0_dp
   real(DP), parameter           :: tol    = 1.0e-4_dp
   real(DP)  :: t
   real(DP)  :: d1, x1, y1, t1
   real(DP)  :: d2, x2, y2, t2
   real(DP)  :: d3, x3, y3, t3
   logical   :: f1, f2, f3
   integer   :: n
   integer   :: ios
   real(DP)  :: cpu1, cpu2
   

   ! fill the material struct
   m%Yt  = Yt;
   m%Yc  = Yc;
   m%S12 = S12;
   m%tf  = HCP_THETAF*pi/180.0_dp;

   m%S23  = m%Yc/(two*tan(m%tf));
   m%munt = -one/tan(two*m%tf);
   m%mu1n = m%S12/m%S23*m%munt;

   ! fill the stress state
   s%s11 = 0.;    ! useless
   s%s22 = sig22;
   s%s33 = sig33;
   s%s12 = sig12;
   s%s13 = sig13;
   s%s23 = sig23;

   ! open result file
   open (unit=1, &
         file="biax.txt", &
         action="write", &
         iostat=ios)
   if ( ios /= 0 ) then
      print "('Cannot open result file')"
      return
   end if
   
   call CPU_TIME(cpu1)
   
   do n=1,ntheta
      t = (n-1)*two*pi/ntheta
      ! bissection
      d1=zero
      x1=range*m%Yc*cos(t)*d1
      y1=range*m%Yc*sin(t)*d1
      !print "('  (s22=',F8.2,', s33=',F8.2,')')", x1,y1
      call K_FCRITERION(x1, y1, s, m, f1, t1);
      !print "('  -> d1=',F8.2,', f1=',L)", d1,f1

      d2=one
      x2=range*m%Yc*cos(t)*d2
      y2=range*m%Yc*sin(t)*d2
      !print "('  (s22=',F10.6,', s33=',F8.2,')')", x2,y2
      call K_FCRITERION(x2, y2, s, m, f2, t2);
      !print "('  -> d2=',F10.6,', f2=',L)", d2, f2

      if(f2==.false.) then
         x3 = zero
         y3 = zero
         t3 = zero 
         f3 = .false.
         write(unit=1,fmt="('NaN NaN NaN 0')")
         
      else
         do while((d2-d1)/two>tol)
            d3=(d1+d2)/two
            x3=range*m%Yc*cos(t)*d3
            y3=range*m%Yc*sin(t)*d3
            call K_FCRITERION(x3, y3, s, m, f3, t3)
            !print "('  -> d3=',F10.6,', f3=',L)", d3, f3

            if(f3) then       
                 d2=d3
                 x2=x3
                 y2=y3
                 f2=f3
                 t2=t3
            else
                 d1=d3
                 x1=x3
                 y1=y3
                 f1=f3 
                 t1=t3
            end if
         end do         
         write(unit=1,fmt="(G20.10,' ',G20.10,' ',G20.10,' ',I2)") x3, y3, t3, merge(1,0,f3)
   
      end if
      print "('t =',F8.2,' s22=',F8.2,' s33=',F8.2,' f=',L,' t=',F8.2,' dd=',F8.2)", t, x3, y3, f3, t3, (d2-d1)/two
      
      if ( ios /= 0 ) then
         print "('Cannot write to result file')"
         return
      end if      

      
   end do
   call CPU_TIME(cpu2)
   print "('CPU Time =', F5.1)", cpu2-cpu1
   
   close(unit=1)
   
   
contains

   subroutine K_FCRITERION(x, y, s, m, f, theta)
      implicit none
      real(DP), intent(in)                 :: x, y
      type(s_stensor), intent(inout)        :: s
      type(s_material_matrix), intent(in)  :: m
      logical, intent(out)                 :: f
      real(DP), intent(out)                :: theta
      
      ! local
      type(s_bbox)            :: bb
      type(s_zeros)           :: z
      type(s_fstate)          :: fs
   
      s%s22 = x
      s%s33 = y
      call K_COMPUTE_BBOX(s, bb) 
      call K_FIND_ZEROS(s, z) 
      call K_CHECK_PUCK_CRITERION(s, m, z, bb, fs)
      f     = fs%failure
      theta = fs%tm
   end subroutine K_FCRITERION

end subroutine K_BIAXIAL

