! $Id: k_debug_matlab.inc 1054 2012-09-11 00:05:19Z boman $
!> \ingroup grptools
!> Dump everything to matlab

subroutine K_DEBUG_MATLAB( &
      nblock, ndir, nshr, nstatev, nfieldv, nprops, lanneal, &
      stepTime, totalTime, dt, cmname, coordMp, charLength, &
      props, density, strainInc, relSpinInc,  &
      tempOld, stretchOld, defgradOld, fieldOld, &
      stressOld, stateOld, enerInternOld, enerInelasOld, &
      tempNew, stretchNew, defgradNew, fieldNew, &
      stressNew, stateNew, enerInternNew, enerInelasNew)

   use K_IDM
   implicit none

   ! input parameters
   integer :: nblock     !< Number of material points to be processed in this call to VUMAT
   integer :: ndir       !< Number of direct components in a symmetric tensor
   integer :: nshr       !< Number of indirect components in a symmetric tensor
   integer :: nstatev    !< Number of user-defined state variables
   integer :: nfieldv    !< Number of user-defined external field variables [RB:????]
   integer :: nprops     !< User-specified number of user-defined material properties
   integer :: lanneal    !< [unused] Flag indicating whether the routine is being called during an annealing process. 

   real(DP) :: stepTime   !< Value of time since the step began
   real(DP) :: totalTime  !< [unused] Value of total time. 
   real(DP) :: dt         !< [unused] Time increment size.
   character(len=*) :: cmname     !< User-specified material name
   real(DP), dimension (nblock,1)              :: coordMp         !< [unused] Material point coordinates. 
   real(DP), dimension (nblock)                :: charLength      !< Characteristic element length.
   real(DP), dimension (nprops)                :: props           !< User-supplied material properties.
   real(DP), dimension (nblock)                :: density         !< [unused] Current density at the material points in the midstep configuration
   real(DP), dimension (nblock,ndir+nshr)      :: strainInc       !< Strain increment tensor at each material point. (11-22-33-12-23-31)
   real(DP), dimension (nblock,nshr)           :: relSpinInc      !< [unused] Incremental relative rotation vector at each material point defined in the corotational system. 
   real(DP), dimension (nblock)                :: tempOld         !< [unused] Temperatures at each material point
   real(DP), dimension (nblock,ndir+nshr)      :: stretchOld      !< [unused] Stretch tensor U at the beginning of the increment
   real(DP), dimension (nblock,ndir+nshr+nshr) :: defgradOld      !< [unused] Deformation gradient tensor F at the beginning of the increment. (11-22-33-12-23-31-21-32-13)
   real(DP), dimension (nblock,nfieldv)        :: fieldOld        !< [unused] Values of the user-defined field variables
   real(DP), dimension (nblock,ndir+nshr)      :: stressOld       !< Stress tensor at each material point at the beginning of the increment.
   real(DP), dimension (nblock,nstatev)        :: stateOld        !< State variables at each material point at the beginning of the increment.
   real(DP), dimension (nblock)                :: enerInternOld   !< [unused] Internal energy per unit mass at each material point at the beginning of the increment.
   real(DP), dimension (nblock)                :: enerInelasOld   !< [unused] Dissipated inelastic energy per unit mass at each material point at the beginning of the increment.
   real(DP), dimension (nblock)                :: tempNew         !< [unused] Temperatures at each material point at the end of the increment. 
   real(DP), dimension (nblock,ndir+nshr)      :: stretchNew      !< [unused] Stretch tensor U at each material point at the end of the increment 
   real(DP), dimension (nblock,ndir+nshr+nshr) :: defgradNew      !< [unused] Deformation gradient tensor F at each material point at the end of the increment. 
   real(DP), dimension (nblock,nfieldv)        :: fieldNew        !< [unused] Values of the user-defined field variables at each material point at the end of the increment.
   ! output variables
   real(DP), dimension (nblock,ndir+nshr)      :: stressNew       !< Stress tensor at each material point at the end of the increment.
   real(DP), dimension (nblock,nstatev)        :: stateNew        !< State variables at each material point at the end of the increment. 
   real(DP), dimension (nblock)                :: enerInternNew   !< [unused] Internal energy per unit mass at each material point at the end of the increment.
   real(DP), dimension (nblock)                :: enerInelasNew   !< [unused] Dissipated inelastic energy per unit mass at each material point at the end of the increment.
   ! local variables
   real(DP), save :: lasttime = -1.0  ! last time
   integer, save :: nelem     = 0             !< current element (first of the block)
   integer, save :: increment = 0             !< current increment
   integer, save :: matlab_i  = 0             !< current matlab index
   integer       :: ios
   character(len=256) :: jobname, joboutdir, mlabfile
   integer            :: ljobname, ljoboutdir
   integer            :: ie                   !< nb of the element in the block
   
   ! -- hard-coded parameters -----------------------------------------------
   integer :: matlab_freq    = 10000   !< save everything every "matlab_freq" increments
   integer :: element_number = -1      !< element to be processed (set to any negative value to disable output)
   ! ------------------------------------------------------------------------
   
   if(lasttime /= totalTime) then ! new increment
      lasttime = totalTime 
      nelem  = 1 ! elements "nelem" to "nelem+nblock-1" are processed
   end if
   
   if(element_number<nelem .or. element_number>nelem+nblock-1) then
      nelem = nelem+nblock
      return ! skip all element except the wanted one
   else
      ie = element_number-nelem+1
   endif
   
   ! open the file
   if(increment == 0) then 
      call VGETJOBNAME(jobname, ljobname)
      call VGETOUTDIR(joboutdir, ljoboutdir)  
      mlabfile = joboutdir(1:ljoboutdir)//'/'//jobname(1:ljobname)//'DEBUG.m'
      call XPLB_ABQERR(-1,'Opening MATLAB file "%S" ', 0, zero, mlabfile)

      open (unit = 16, &   !this number is recommended by the ABAQUS manual
            file   = mlabfile, &
            form   = "formatted", &
            access = "sequential", &
            status = "unknown", &
            iostat = ios)      
      if (ios /= 0) then
         call XPLB_ABQERR(-3,'Cannot create MATLAB file!', 0, zero, ' ')         
      end if
   end if
   
   ! increase the increment counter
   increment = increment +1
   
   if(mod(increment, matlab_freq) == 0) then
      matlab_i = matlab_i +1
      write(unit=16, fmt="('dt(',I8,')=',G20.10,';')")        matlab_i, dt
      !write(unit=16, fmt="('stepTime(',I8,')=',G20.10,';')")  matlab_i, stepTime
      write(unit=16, fmt="('totalTime(',I8,')=',G20.10,';')") matlab_i, totalTime

      call K_WRITE_VECTOR6(nblock, ie, 16, stressNew, matlab_i, 'sig')
      call K_WRITE_VECTOR6(nblock, ie, 16, strainInc, matlab_i, 'deps')
      call K_WRITE_VECTOR6(nblock, ie, 16, stateNew,  matlab_i, 'eps')
      !call K_WRITE_VECTOR6(nblock, ie, 16, stretchOld, matlab_i, 'Uold')
      !call K_WRITE_VECTOR6(nblock, ie, 16, stretchNew, matlab_i, 'U')
      !call K_WRITE_VECTOR9(nblock, ie, 16, defgradOld, matlab_i, 'Fold') 
      !call K_WRITE_VECTOR9(nblock, ie, 16, defgradNew, matlab_i, 'F')    
      
      write(unit=16, fmt="('d11T(',I8,')=',G20.10,';')")        matlab_i, stateNew(ie,ST_D11T)
      write(unit=16, fmt="('d11C(',I8,')=',G20.10,';')")        matlab_i, stateNew(ie,ST_D11C)
      !write(unit=16, fmt="('d22T(',I8,')=',G20.10,';')")        matlab_i, stateNew(ie,ST_MT_D)
      !write(unit=16, fmt="('d22C(',I8,')=',G20.10,';')")        matlab_i, stateNew(ie,ST_MC_D)
      !write(unit=16, fmt="('flag(',I8,')=',G20.10,';')")        matlab_i, stateNew(ie,ST_ACTIVE)
      !write(unit=16, fmt="('thetaC(',I8,')=',G20.10,';')")      matlab_i, stateNew(ie,ST_THETAC)
      !write(unit=16, fmt="('gamma_R_O(',I8,')=',G20.10,';')")   matlab_i, stateNew(ie,ST_MC_EPS0)
      !write(unit=16, fmt="('sigma_R_O(',I8,')=',G20.10,';')")   matlab_i, stateNew(ie,ST_MC_X)
      !write(unit=16, fmt="('gamma_R_F(',I8,')=',G20.10,';')")   matlab_i, stateNew(ie,ST_MC_EPSF)
      !write(unit=16, fmt="('length(',I8,')=',G20.10,';')")      matlab_i, stateNew(ie,ST_LENGTH)
      !write(unit=16, fmt="('eps_inel_12(',I8,')=',G20.10,';')") matlab_i, stateNew(ie,ST_EPS_INEL_12)
      !write(unit=16, fmt="('eps_inel_23(',I8,')=',G20.10,';')") matlab_i, stateNew(ie,ST_EPS_INEL_23)
      !write(unit=16, fmt="('eps_inel_13(',I8,')=',G20.10,';')") matlab_i, stateNew(ie,ST_EPS_INEL_13)
   end if
   
   nelem = nelem+nblock
   
end subroutine K_DEBUG_MATLAB

! print a 3x3 symmetric matrix (such as sigma)
! (11-22-33-12-23-31)

subroutine K_WRITE_VECTOR6(nblock, ie, u, v, i, name)
   use K_IDM
   implicit none
   integer, intent(in)                        :: u, i, nblock, ie
   real(DP), intent(in), dimension(nblock, *) :: v
   character(len=*), intent(in)               :: name
   write(unit=u, fmt="(A,'11(',I8,')=',G20.10,';')") name, i, v(ie,1)
   write(unit=u, fmt="(A,'22(',I8,')=',G20.10,';')") name, i, v(ie,2)
   write(unit=u, fmt="(A,'33(',I8,')=',G20.10,';')") name, i, v(ie,3)
   write(unit=u, fmt="(A,'12(',I8,')=',G20.10,';')") name, i, v(ie,4)
   write(unit=u, fmt="(A,'23(',I8,')=',G20.10,';')") name, i, v(ie,5)
   write(unit=u, fmt="(A,'31(',I8,')=',G20.10,';')") name, i, v(ie,6)
end subroutine K_WRITE_VECTOR6

! print a 3x3 unsymmetric matrix (such as F)
! (11-22-33-12-23-31-21-32-13)

subroutine K_WRITE_VECTOR9(nblock, ie, u, v, i, name)
   use K_IDM
   implicit none
   integer, intent(in)                        :: u, i, nblock, ie
   real(DP), intent(in), dimension(nblock, *) :: v
   character(len=*), intent(in)               :: name
   call K_WRITE_VECTOR6(nblock, ie, u, v, i, name)
   write(unit=u, fmt="(A,'21(',I8,')=',G20.10,';')") name, i, v(ie,7)
   write(unit=u, fmt="(A,'32(',I8,')=',G20.10,';')") name, i, v(ie,8)
   write(unit=u, fmt="(A,'13(',I8,')=',G20.10,';')") name, i, v(ie,9)
end subroutine K_WRITE_VECTOR9
