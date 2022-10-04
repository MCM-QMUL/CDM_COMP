! $Id: k_bilinear_law_error.inc 1056 2012-09-13 05:54:58Z boman $
!> \ingroup grpfibre
!> Display an error message in the .log/.sta 
!> if the mesh is not sufficiently fine

subroutine K_BILINEAR_LAW_ERROR(text, law)
   use K_IDM
   implicit none
   character(len=*), intent(in)         :: text
   type(s_bilinear_damage), intent(in)  :: law
   integer, save :: counter1 = 0
   integer :: stopcode
   
   if (counter1<1) then
      ! print msg in the .log
      print "('ERROR: mesh is not sufficiently fine! (',A,')')", text
      print "('length =',G20.10)", law%L
      print "('eps0   =',G20.10)", law%e0
      print "('epsF   =',G20.10)", law%eF
      print "('X      =',G20.10)", law%X
      print "('G      =',G20.10)", law%G
      
      if(HCP_IGNORE_GERROR) then
         stopcode = -2
      else
         stopcode = -3
      end if
      ! print msg in the .sta
      call XPLB_ABQERR(-2, "mesh is not sufficiently fine! (%S)", 0, 0.0_dp, text)
      call XPLB_ABQERR(-2, "  length = %R", 0, law%L, ' ')
      call XPLB_ABQERR(-2, "  eps0   = %R", 0, law%e0, ' ')
      call XPLB_ABQERR(stopcode, "  epsF   = %R", 0, law%eF, ' ')   
      counter1 = counter1+1
   end if
   
end subroutine K_BILINEAR_LAW_ERROR
