! $Id: k_abq_vars.inc 1048 2012-08-23 06:26:15Z boman $
! Input parameters of abaqus

integer, intent(in) :: nblock     !< Number of material points to be processed in this call to VUMAT
integer, intent(in) :: ndir       !< Number of direct components in a symmetric tensor
integer, intent(in) :: nshr       !< Number of indirect components in a symmetric tensor
integer, intent(in) :: nstatev    !< Number of user-defined state variables
integer, intent(in) :: nfieldv    !< Number of user-defined external field variables [RB:????]
integer, intent(in) :: nprops     !< User-specified number of user-defined material properties
integer, intent(in) :: lanneal    !< [unused] Flag indicating whether the routine is being called during an annealing process. 

real(DP), intent(in) :: stepTime   !< Value of time since the step began
real(DP), intent(in) :: totalTime  !< [unused] Value of total time. 
real(DP), intent(in) :: dt         !< [unused] Time increment size.
character(len=*), intent(in) :: cmname     !< User-specified material name
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
