! $Id: vumat.for 1054 2012-09-11 00:05:19Z boman $

include 'k_idm.inc'

!> \ingroup grpmain
!! Main VUMAT subroutine called by Abaqus
!!
!! This subroutine retrieves the number of the currently processed element by reading
!! beyond the `jblock` variable. \n
!! (see http://imechanica.org/node/2540)
!! \warning if double precision is wanted, the packager \b MUST be run 
!! in double precision... (`double=both`)

subroutine VUMAT( jblock, ndir, nshr, nstatev, nfieldv, nprops, lanneal, &
      stepTime, totalTime, dt, cmname, coordMp, charLength, &
      props, density, strainInc, relSpinInc,  &
      tempOld, stretchOld, defgradOld, fieldOld, &
      stressOld, stateOld, enerInternOld, enerInelasOld, &
      tempNew, stretchNew, defgradNew, fieldNew, &
      stressNew, stateNew, enerInternNew, enerInelasNew)
   !DEC$ ATTRIBUTES DLLEXPORT::VUMAT

   use K_IDM

   integer, dimension(*), intent(in)  :: jblock          !< Number of material points to be processed in this call to VUMAT
   integer, intent(in)                :: ndir            !< Number of direct components in a symmetric tensor
   integer, intent(in)                :: nshr            !< Number of indirect components in a symmetric tensor
   integer, intent(in)                :: nstatev         !< Number of user-defined state variables
   integer, intent(in)                :: nfieldv         !< Number of user-defined external field variables [RB:????]
   integer, intent(in)                :: nprops          !< User-specified number of user-defined material properties
   integer, intent(in)                :: lanneal         !< [unused] Flag indicating whether the routine is being called during an annealing process. 
   real(DP), intent(in)               :: stepTime        !< Value of time since the step began
   real(DP), intent(in)               :: totalTime       !< [unused] Value of total time. 
   real(DP), intent(in)               :: dt              !< [unused] Time increment size.
   character(len=*), intent(in)       :: cmname          !< User-specified material name
   real(DP), intent(in), dimension(*) :: coordMp         !< [unused] Material point coordinates. 
   real(DP), intent(in), dimension(*) :: charLength      !< Characteristic element length.
   real(DP), intent(in), dimension(*) :: props           !< User-supplied material properties.
   real(DP), intent(in), dimension(*) :: density         !< [unused] Current density at the material points in the midstep configuration
   real(DP), intent(in), dimension(*) :: strainInc       !< Strain increment tensor at each material point. (11-22-33-12-23-31)
   real(DP), intent(in), dimension(*) :: relSpinInc      !< [unused] Incremental relative rotation vector at each material point defined in the corotational system. 
   real(DP), intent(in), dimension(*) :: tempOld         !< [unused] Temperatures at each material point
   real(DP), intent(in), dimension(*) :: stretchOld      !< [unused] Stretch tensor U at the beginning of the increment
   real(DP), intent(in), dimension(*) :: defgradOld      !< [unused] Deformation gradient tensor F at the beginning of the increment. (11-22-33-12-23-31-21-32-13)
   real(DP), intent(in), dimension(*) :: fieldOld        !< [unused] Values of the user-defined field variables
   real(DP), intent(in), dimension(*) :: stressOld       !< Stress tensor at each material point at the beginning of the increment.
   real(DP), intent(in), dimension(*) :: stateOld        !< State variables at each material point at the beginning of the increment.
   real(DP), intent(in), dimension(*) :: enerInternOld   !< [unused] Internal energy per unit mass at each material point at the beginning of the increment.
   real(DP), intent(in), dimension(*) :: enerInelasOld   !< [unused] Dissipated inelastic energy per unit mass at each material point at the beginning of the increment.
   real(DP), intent(in), dimension(*) :: tempNew         !< [unused] Temperatures at each material point at the end of the increment. 
   real(DP), intent(in), dimension(*) :: stretchNew      !< Stretch tensor U at each material point at the end of the increment 
   real(DP), intent(in), dimension(*) :: defgradNew      !< [unused] Deformation gradient tensor F at each material point at the end of the increment. 
   real(DP), intent(in), dimension(*) :: fieldNew        !< [unused] Values of the user-defined field variables at each material point at the end of the increment.
   ! output variables
   real(DP), dimension(*) :: stressNew       !< Stress tensor at each material point at the end of the increment.
   real(DP), dimension(*) :: stateNew        !< State variables at each material point at the end of the increment. 
   real(DP), dimension(*) :: enerInternNew   !< [unused] Internal energy per unit mass at each material point at the end of the increment.
   real(DP), dimension(*) :: enerInelasNew   !< [unused] Dissipated inelastic energy per unit mass at each material point at the end of the increment.

   integer, parameter :: i_umt_nblock = 1
   integer, parameter :: i_umt_npt    = 2
   integer, parameter :: i_umt_layer  = 3
   integer, parameter :: i_umt_kspt   = 4
   integer, parameter :: i_umt_noel   = 5
  
   call VUMATXARGS(jblock(i_umt_nblock),  &
         ndir, nshr, nstatev, nfieldv, nprops, lanneal,  &
         stepTime, totalTime, dt, cmname, coordMp, charLength,  &
         props, density, strainInc, relSpinInc,  &
         tempOld, stretchOld, defgradOld, fieldOld,  &
         stressOld, stateOld, enerInternOld, enerInelasOld,  &
         tempNew, stretchNew, defgradNew, fieldNew,  &
         stressNew, stateNew, enerInternNew, enerInelasNew,  &
         jblock(i_umt_noel), jblock(i_umt_npt),  &
         jblock(i_umt_layer), jblock(i_umt_kspt))

end subroutine VUMAT

! ======= Additionnal subroutines =========

!> \file vumat.for
!! All the subroutines are included in this file in order to build the entire 
!! code by compiling this single file

include 'vumatxargs.inc'

! fibre damage
include 'k_fibre_damage.inc'
include 'k_bilinear_law.inc'
include 'k_bilinear_law_error.inc'
include 'k_update_bilinear_law.inc'

! matrix damage
include 'k_matrix_damage.inc'
include 'k_min_brent.inc'
include 'k_compute_bbox.inc'
include 'k_find_zeros.inc'
include 'k_check_puck_criterion.inc'
include 'k_eval_puck_criterion.inc'
include 'k_rottensor.inc'
include 'k_cubic_law_error.inc'
include 'k_cubic_shear.inc'

! tools
include 'k_effective_stress.inc'
include 'k_nl_shear_1d.inc'
include 'k_compute_c.inc'
include 'k_print_input.inc'
include 'k_debug_matlab.inc'
include 'k_char_length.inc'
include 'k_element_deletion.inc'

! structures
include 'k_fill_material_elastic.inc'
include 'k_fill_material_fibre.inc'
include 'k_fill_material_matrix.inc'
include 'k_fill_material_shear.inc'
include 'k_print_bb.inc'
include 'k_print_bilinear_damage.inc'
include 'k_print_stensor.inc'
include 'k_smatrix2state.inc'
include 'k_state2smatrix.inc'
include 'k_stensor2vec.inc'
include 'k_vec2stensor.inc'
