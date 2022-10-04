! $Id: vumatxargs.inc 1054 2012-09-11 00:05:19Z boman $

!> \ingroup grpmain
!! VUMAT subroutine with additionnal arguments (see http://imechanica.org/node/2540).
!! 
!! Each call of this subroutine is done for several (`nblock`) material points. The related element numbers
!! are given in the array `nElement` (size `nblock`). All of the material points have the same 
!! Gauss point number (`nMatPoint`), layer number (`nLayer`) and section number (`nSecPoint`).

subroutine VUMATXARGS( nblock, ndir, nshr, nstatev, nfieldv, nprops, lanneal, &
                       stepTime, totalTime, dt, cmname, coordMp, charLength, &
                       props, density, strainInc, relSpinInc,  &
                       tempOld, stretchOld, defgradOld, fieldOld, &
                       stressOld, stateOld, enerInternOld, enerInelasOld, &
                       tempNew, stretchNew, defgradNew, fieldNew, &
                       stressNew, stateNew, enerInternNew, enerInelasNew,  &
                       nElement, nMatPoint, nLayer, nSecPoint)
   !DEC$ ATTRIBUTES DLLEXPORT::VUMAT

   use K_IDM
   implicit none

   integer, intent(in) :: nblock                 !< Number of material points to be processed in this call to VUMAT
   integer, intent(in) :: ndir                   !< Number of direct components in a symmetric tensor
   integer, intent(in) :: nshr                   !< Number of indirect components in a symmetric tensor
   integer, intent(in) :: nstatev                !< Number of user-defined state variables
   integer, intent(in) :: nfieldv                !< Number of user-defined external field variables
   integer, intent(in) :: nprops                 !< User-specified number of user-defined material properties
   integer, intent(in) :: lanneal                !< [unused] Flag indicating whether the routine is being called during an annealing process. 
   real(DP), intent(in) :: stepTime              !< Value of time since the step began
   real(DP), intent(in) :: totalTime             !< [unused] Value of total time. 
   real(DP), intent(in) :: dt                    !< [unused] Time increment size.
   character(len=*), intent(in) :: cmname        !< User-specified material name
   real(DP), intent(in), dimension(nblock,*)              :: coordMp         !< [unused] Material point coordinates. 
   real(DP), intent(in), dimension(nblock)                :: charLength      !< Characteristic element length.
   real(DP), intent(in), dimension(nprops)                :: props           !< User-supplied material properties.
   real(DP), intent(in), dimension(nblock)                :: density         !< [unused] Current density at the material points in the midstep configuration
   real(DP), intent(in), dimension(nblock,ndir+nshr)      :: strainInc       !< Strain increment tensor at each material point. (11-22-33-12-23-31)
   real(DP), intent(in), dimension(nblock,nshr)           :: relSpinInc      !< [unused] Incremental relative rotation vector at each material point defined in the corotational system. 
   real(DP), intent(in), dimension(nblock)                :: tempOld         !< [unused] Temperatures at each material point
   real(DP), intent(in), dimension(nblock,ndir+nshr)      :: stretchOld      !< [unused] Stretch tensor U at the beginning of the increment
   real(DP), intent(in), dimension(nblock,ndir+2*nshr)    :: defgradOld      !< [unused] Deformation gradient tensor F at the beginning of the increment. (11-22-33-12-23-31-21-32-13)
   real(DP), intent(in), dimension(nblock,nfieldv)        :: fieldOld        !< [unused] Values of the user-defined field variables
   real(DP), intent(in), dimension(nblock,ndir+nshr)      :: stressOld       !< Stress tensor at each material point at the beginning of the increment.
   real(DP), intent(in), dimension(nblock,nstatev)        :: stateOld        !< State variables at each material point at the beginning of the increment.
   real(DP), intent(in), dimension(nblock)                :: enerInternOld   !< [unused] Internal energy per unit mass at each material point at the beginning of the increment.
   real(DP), intent(in), dimension(nblock)                :: enerInelasOld   !< [unused] Dissipated inelastic energy per unit mass at each material point at the beginning of the increment.
   real(DP), intent(in), dimension(nblock)                :: tempNew         !< [unused] Temperatures at each material point at the end of the increment. 
   real(DP), intent(in), dimension(nblock,ndir+nshr)      :: stretchNew      !< Stretch tensor U at each material point at the end of the increment 
   real(DP), intent(in), dimension(nblock,ndir+2*nshr)    :: defgradNew      !< [unused] Deformation gradient tensor F at each material point at the end of the increment. 
   real(DP), intent(in), dimension(nblock,nfieldv)        :: fieldNew        !< [unused] Values of the user-defined field variables at each material point at the end of the increment.
   ! output variables
   real(DP), dimension(nblock,ndir+nshr)      :: stressNew       !< Stress tensor at each material point at the end of the increment.
   real(DP), dimension(nblock,nstatev)        :: stateNew        !< State variables at each material point at the end of the increment. 
   real(DP), dimension(nblock)                :: enerInternNew   !< [unused] Internal energy per unit mass at each material point at the end of the increment.
   real(DP), dimension(nblock)                :: enerInelasNew   !< [unused] Dissipated inelastic energy per unit mass at each material point at the end of the increment.

   ! Additionnal arguments
   integer, dimension(nblock), intent(in) :: nElement !< element numbers  
   integer, intent(in) :: nMatPoint                   !< Gauss point number  
   integer, intent(in) :: nLayer                      !< Layer number 
   integer, intent(in) :: nSecPoint                   !< Section number  

   ! local variables 
   type(s_material_elastic) :: me                !< material properties (elastic part)
   type(s_material_fibre)   :: mf                !< material properties (fibres)
   type(s_material_shear)   :: ms12, ms23, ms13  !< material properties (shear)
   type(s_material_matrix)  :: mm                !< material properties (matrix)

   call K_PRINT_INPUT(nblock, ndir, nshr, nstatev, nfieldv, nprops, lanneal, &
      stepTime, totalTime, dt, cmname, coordMp, charLength, &
      props, density, strainInc, relSpinInc,  &
      tempOld, stretchOld, defgradOld, fieldOld, &
      stressOld, stateOld, enerInternOld, enerInelasOld, &
      tempNew, stretchNew, defgradNew, fieldNew, &
      stressNew, stateNew, enerInternNew, enerInelasNew)

   ! initialise stateNew to stateOld (the following code relies on that)
   ! (maybe already done by abaqus? => check)
   stateNew = stateOld
   
   ! retrieve the material properties & store the effective stresses in "stressNew"
   call K_FILL_MATERIAL_ELASTIC(props, me)
   call K_FILL_MATERIAL_SHEAR(props, ms12, ms23, ms13)
   call K_EFFECTIVE_STRESS(nblock, me, ms12, ms23, ms13, &
                           stateOld, strainInc, stressNew, stateNew)
   !return ! if enabled => no damage
   
   ! the packager step should exit here
   if (stepTime == zero) return
   
   ! retrieve damage-related properties (fibre and matrix)
   call K_FILL_MATERIAL_FIBRE(props, mf)
   call K_FILL_MATERIAL_MATRIX(props, mm)
   
   ! compute the characteristic lengths
   call K_CHAR_LENGTH(nblock, nstatev, stateOld, stateNew, charLength)
   ! fibre damage
   call K_FIBRE_DAMAGE(nblock, mf, stateOld, stateNew, stressNew)
   ! matrix damage  
   if (HCP_ENABLE_MATRIX) then
      call K_MATRIX_DAMAGE(nblock, mm, me, ms12, ms23, ms13, stateOld, &
                           stateNew, stressNew)
   endif   
   
   ! element deletion 
   call K_ELEMENT_DELETION(nblock, stateNew)

   if(HCP_DEBUG_MATLAB) then
      call K_DEBUG_MATLAB( &
         nblock, ndir, nshr, nstatev, nfieldv, nprops, lanneal, &
         stepTime, totalTime, dt, cmname, coordMp, charLength, &
         props, density, strainInc, relSpinInc,  &
         tempOld, stretchOld, defgradOld, fieldOld, &
         stressOld, stateOld, enerInternOld, enerInelasOld, &
         tempNew, stretchNew, defgradNew, fieldNew, &
         stressNew, stateNew, enerInternNew, enerInelasNew)
   end if  
       
end subroutine VUMATXARGS
