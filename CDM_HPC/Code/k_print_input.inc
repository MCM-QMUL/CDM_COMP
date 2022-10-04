! $Id: k_print_input.inc 1051 2012-09-04 06:41:05Z boman $
!> \ingroup grptools
!> Print the input parameters & perform some checks

subroutine K_PRINT_INPUT( &
      nblock, ndir, nshr, nstatev, nfieldv, nprops, lanneal, &
      stepTime, totalTime, dt, cmname, coordMp, charLength, &
      props, density, strainInc, relSpinInc,  &
      tempOld, stretchOld, defgradOld, fieldOld, &
      stressOld, stateOld, enerInternOld, enerInelasOld, &
      tempNew, stretchNew, defgradNew, fieldNew, &
      stressNew, stateNew, enerInternNew, enerInelasNew)

   use K_IDM
   implicit none
   include 'k_abq_vars.inc'
   
   ! local variables
   character(len=20) :: prectext
   integer, save :: counter1 = 0
   
   character(len=256) :: jobname, joboutdir
   integer            :: ljobname, ljoboutdir
   
   
   if(counter1 == 0) then
      print "('======================================')"
      !print "('INFO: subroutine ""',A,'"" (',A,')')", trim(cmname), version_tag ! stack overflow in debug mode
      print "('INFO: VUMAT subroutine  (',A,')')", version_tag
      print "('======================================')"
      
      call VGETJOBNAME(jobname, ljobname)
      call VGETOUTDIR(joboutdir, ljoboutdir)
      print "('Jobname = ""', A,'""')", jobname(1:ljobname)
      print "('Outdir  = ""', A,'""')", joboutdir(1:ljoboutdir)
   
      if (precision(dt)>10) then
         prectext = "DOUBLE PRECISION"
      else
         prectext = "SINGLE PRECISION"
      end if
      print "('  precision(dt) = ', I3,' (',A,')')", precision(dt), trim(prectext)
      print "('  nprops        = ', I3)", nprops
      print "('  nshr          = ', I3)", nshr
      print "('  nblock        = ', I3)", nblock
      print "('  ndir          = ', I3)", ndir
      print "('  nfieldv       = ', I3)", nfieldv
      print "('  nstatev       = ', I3)", nstatev
      if (nstatev<ST_MAX) then 
         print *, "ERROR: Depvar (nstatev) should be at least", ST_MAX
         call XPLB_ABQERR(-3,'Depvar (nstatev) should be at least %I', ST_MAX, zero, ' ')
      end if
      if (ndir /= 3) then
         print *, "ERROR: This VUMAT only works for 3D problems"
         call XPLB_ABQERR(-3,'This VUMAT only works for 3D problems ', 0, zero, ' ')
      end if

      print "(/,'* Material properties:')"
      print "('  --------------------')"
      print "('   E1           = ',G20.10)", props(PR_E1)
      print "('   E2           = ',G20.10)", props(PR_E2)
      print "('   E3           = ',G20.10)", props(PR_E3)
      print "('   nu12         = ',G20.10)", props(PR_NU12)
      print "('   nu13         = ',G20.10)", props(PR_NU13)
      print "('   nu23         = ',G20.10)", props(PR_NU23)
      print "('   G12          = ',G20.10)", props(PR_G12)
      print "('   G23          = ',G20.10)", props(PR_G23)
      print "('   G13          = ',G20.10)", props(PR_G13)
      print "('   XT           = ',G20.10)", props(PR_XT)
      print "('   XC           = ',G20.10)", props(PR_XC)
      print "('   YT           = ',G20.10)", props(PR_YT)
      print "('   YC           = ',G20.10)", props(PR_YC)
      print "('   gamma_0_12   = ',G20.10)", props(PR_GAMMA_0_12)
      print "('   gamma_max_12 = ',G20.10)", props(PR_GAMMA_MAX_12)
      print "('   gamma_0_23   = ',G20.10)", props(PR_GAMMA_0_23)
      print "('   gamma_max_23 = ',G20.10)", props(PR_GAMMA_MAX_23)
      print "('   gamma_0_13   = ',G20.10)", props(PR_GAMMA_0_13)
      print "('   gamma_max_13 = ',G20.10)", props(PR_GAMMA_MAX_13)
      print "('   S12          = ',G20.10)", props(PR_S12)
      print "('   G1T          = ',G20.10)", props(PR_G1T)
      print "('   G1C          = ',G20.10)", props(PR_G1C)
      print "('   G2T          = ',G20.10)", props(PR_G2T)
      print "('   G2C          = ',G20.10)", props(PR_G2C)
      print "('   Gshear_12    = ',G20.10)", props(PR_GSHEAR_12)
      print "('   Gshear_23    = ',G20.10)", props(PR_GSHEAR_23)
      print "('   Gshear_13    = ',G20.10)", props(PR_GSHEAR_13)
      print "('   a_12         = ',G20.10)", props(PR_A_12)
      print "('   a_23         = ',G20.10)", props(PR_A_23)
      print "('   a_13         = ',G20.10)", props(PR_A_13)
      print "('   c1_12        = ',G20.10)", props(PR_C1_12)
      print "('   c2_12        = ',G20.10)", props(PR_C2_12)
      print "('   c3_12        = ',G20.10)", props(PR_C3_12)
      print "('   c1_23        = ',G20.10)", props(PR_C1_23)
      print "('   c2_23        = ',G20.10)", props(PR_C2_23)
      print "('   c3_23        = ',G20.10)", props(PR_C3_23)
      print "('   c1_13        = ',G20.10)", props(PR_C1_13)
      print "('   c2_13        = ',G20.10)", props(PR_C2_13)
      print "('   c3_13        = ',G20.10)", props(PR_C3_13)
      print "('======================================')"
      counter1=counter1+1
   end if
   
end subroutine K_PRINT_INPUT
