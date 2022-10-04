! $Id: k_idm.inc 1056 2012-09-13 05:54:58Z boman $
!> \ingroup grpmain
!> Module K_IDM (Intralaminar Damage Model)
!!   - precision of the code (double or single - see variable DP)
!!   - constants (zero, one, pi, ...)
!!   - hard-coded parameters
!!   - enumerations (props & state codes)
!!   - data structures
!!
!! NOTE: use "double=both" if double precision!

module K_IDM

   !> a string for version identification
   character(len=*), parameter :: version_tag = "v3.2"

   !> precision of the code
   !> \details Change the value of DP to `KIND(1.0)` in order to use 
   !> single precision floating point numbers in the entire code.
   integer, parameter :: I4B = SELECTED_INT_KIND(9)
   integer, parameter :: DP  = KIND(1.0D0)            !< double precision
   !integer, parameter :: DP  = KIND(1.0)             !< single precision

   !> \defgroup grpcsts Constants
   !> \{
   real(DP), parameter :: zero  = 0.0_dp                                         !< real zero
   real(DP), parameter :: one   = 1.0_dp                                         !< real one
   real(DP), parameter :: two   = 2.0_dp                                         !< real two
   real(DP), parameter :: three = 3.0_dp                                         !< real three
   real(DP), parameter :: four  = 4.0_dp                                         !< real four
   real(DP), parameter :: pi    = 3.141592653589793238462643383279502884197_dp   !< \f$ \pi \f$
   !> \}
   
   !> \defgroup grphcp Hard-coded parameters
   !> These parameters change the behaviour of the VUMAT. 
   !> Some of them could be put into the `props` array of Abaqus.
   !> \{
   logical, parameter :: HCP_DEBUG_MATLAB  =  .false.   !< produces a huge output file for matlab
   logical, parameter :: HCP_ENABLE_FIBRE  =  .true.    !< enable fibre damage
   logical, parameter :: HCP_ENABLE_MATRIX =  .true.    !< enable matrix damage
   logical, parameter :: HCP_IGNORE_GERROR =  .true.    !< ignore errors related to G
   
   real(DP), parameter :: HCP_DMAX        =  0.95_dp   !< maximum damage before element deletion
   real(DP), parameter :: HCP_BRENT_TOL   =  1.0e-3_dp !< Brent algorithm: tolerance
   integer, parameter  :: HCP_BRENT_ITMAX =  50        !< Brent algorithm: maximum number of iterations
   real(DP), parameter :: HCP_THETAF      =  53.0_dp   !< facture angle for pure transverse compression
   !> \}
   
   !> \defgroup grpprops PROPS codes
   !> Indexes of the Abaqus array `props` (simulate a C enumeration in fortran). 
   !> \details For example, you should use `props(k,PR_E1)` instead of `props(k,1)`. Advantages:
   !>   * it is more readable. 
   !>   * the internal ordering of the parameters may be changed without changing the source code.
   !> \{
   integer, parameter :: PR_E1           =  1  !< Young's modulus in direction 1 (fibre) \f$ E_1 \f$
   integer, parameter :: PR_E2           =  2  !< Young's modulus in direction 2 (transverse) \f$ E_2 \f$
   integer, parameter :: PR_E3           =  3  !< Young's modulus in direction 3 (thickness) \f$ E_3 \f$
   integer, parameter :: PR_NU12         =  4  !< Poisson's ratio \f$ \nu_{12} \f$
   integer, parameter :: PR_NU23         =  5  !< Poisson's ratio \f$ \nu_{23} \f$
   integer, parameter :: PR_NU13         =  6  !< Poisson's ratio \f$ \nu_{13} \f$
   integer, parameter :: PR_G12          =  7  !< Shear modulus in plane 12 \f$ G_{12} \f$
   integer, parameter :: PR_G23          =  8  !< Shear modulus in plane 23 \f$ G_{23} \f$
   integer, parameter :: PR_G13          =  9  !< Shear modulus in plane 13 \f$ G_{13} \f$
   integer, parameter :: PR_XT           = 10  !< Tensile strength (fibre direction) \f$ X_T \f$
   integer, parameter :: PR_XC           = 11  !< Compressive strength (fibre direction) \f$ X_C \f$ **[should be positive]**
   integer, parameter :: PR_YT           = 12  !< Tensile strength (transverse direction) \f$ Y_T \f$
   integer, parameter :: PR_YC           = 13  !< Compressive strength (transverse direction) \f$ Y_C \f$ **[should be positive]**  
   integer, parameter :: PR_GAMMA_0_12   = 14  !< initiation strain (nonlinear shear 12) \f$ \gamma_{12}^0 \f$
   integer, parameter :: PR_GAMMA_MAX_12 = 15  !< strain corresponding to max shear (nonlinear shear 12) \f$ \gamma_{12}^{\max} \f$
   integer, parameter :: PR_GAMMA_0_23   = 16  !< initiation strain (nonlinear shear 23) \f$ \gamma_{23}^0 \f$
   integer, parameter :: PR_GAMMA_MAX_23 = 17  !< strain corresponding to max shear (nl shear 23) \f$ \gamma_{23}^{\max} \f$
   integer, parameter :: PR_GAMMA_0_13   = 18  !< initiation strain (nonlinear shear 13) \f$ \gamma_{13}^0 \f$
   integer, parameter :: PR_GAMMA_MAX_13 = 19  !< strain corresponding to max shear (nonlinear shear 13) \f$ \gamma_{13}^{\max} \f$
   integer, parameter :: PR_S12          = 20  !< Shear strength in plane 12  \f$ S_{12} \f$ 
   integer, parameter :: PR_G1T          = 21  !< fracture energy (fibre tension) \f$ G_1^T \f$ 
   integer, parameter :: PR_G1C          = 22  !< fracture energy (fibre compression) \f$ G_1^C \f$
   integer, parameter :: PR_G2T          = 23  !< fracture energy (transverse tension) \f$ G_2^T \f$
   integer, parameter :: PR_G2C          = 24  !< fracture energy (transverse compression) \f$ G_2^C \f$
   integer, parameter :: PR_GSHEAR_12    = 25  !< fracture energy (nonlinear shear 12) \f$ G_{12}^s \f$
   integer, parameter :: PR_GSHEAR_23    = 26  !< fracture energy (nonlinear shear 23) \f$ G_{23}^s \f$
   integer, parameter :: PR_GSHEAR_13    = 27  !< fracture energy (nonlinear shear 13) \f$ G_{13}^s \f$
   integer, parameter :: PR_A_12         = 28  !< shear modulus reduction coefficient (nonlinear shear 12) \f$ a^{(12)} \f$
   integer, parameter :: PR_A_23         = 29  !< shear modulus reduction coefficient (nonlinear shear 23) \f$ a^{(23)} \f$
   integer, parameter :: PR_A_13         = 30  !< shear modulus reduction coefficient (nonlinear shear 13) \f$ a^{(13)} \f$
   integer, parameter :: PR_C1_12        = 31  !< coefficient of the cubic curve (nonlinear shear 12) \f$ c_1^{(12)} \f$
   integer, parameter :: PR_C2_12        = 32  !< coefficient of the cubic curve (nonlinear shear 12) \f$ c_2^{(12)} \f$
   integer, parameter :: PR_C3_12        = 33  !< coefficient of the cubic curve (nonlinear shear 12) \f$ c_3^{(12)} \f$
   integer, parameter :: PR_C1_23        = 34  !< coefficient of the cubic curve (nonlinear shear 23) \f$ c_1^{(23)} \f$
   integer, parameter :: PR_C2_23        = 35  !< coefficient of the cubic curve (nonlinear shear 23) \f$ c_2^{(23)} \f$
   integer, parameter :: PR_C3_23        = 36  !< coefficient of the cubic curve (nonlinear shear 23) \f$ c_3^{(23)} \f$
   integer, parameter :: PR_C1_13        = 37  !< coefficient of the cubic curve (nonlinear shear 13) \f$ c_1^{(13)} \f$
   integer, parameter :: PR_C2_13        = 38  !< coefficient of the cubic curve (nonlinear shear 13) \f$ c_2^{(13)} \f$
   integer, parameter :: PR_C3_13        = 39  !< coefficient of the cubic curve (nonlinear shear 13) \f$ c_3^{(13)} \f$
   integer, parameter :: PR_MAX          = 39  !< max index (be sure this number equals the highest `PR_*` number)
   !> \}
   
   !> \defgroup grpstate STATE codes (SDV)
   !> Indexes of the Abaqus `stateNew` and `stateOld`  arrays(simulate a C enumeration in fortran). 
   !> \details For example, you should use `stateNew(k,ST_THETAC)` instead of `stateNew(k,10)`. Advantages:
   !>   * it is more readable. 
   !>   * the internal ordering of the parameters may be changed without changing the source code.
   !> \{
   integer, parameter :: ST_EPS11         =  1   !< current strain 11 \f$ \varepsilon_{11} \f$
   integer, parameter :: ST_EPS22         =  2   !< current strain 22 \f$ \varepsilon_{22} \f$
   integer, parameter :: ST_EPS33         =  3   !< current strain 33 \f$ \varepsilon_{33} \f$
   integer, parameter :: ST_EPS12         =  4   !< current strain 12 \f$ \varepsilon_{12} \f$
   integer, parameter :: ST_EPS23         =  5   !< current strain 23 \f$ \varepsilon_{23} \f$
   integer, parameter :: ST_EPS13         =  6   !< current strain 13 \f$ \varepsilon_{13} \f$
   integer, parameter :: ST_ACTIVE        =  7   !< status (element deletion)
   integer, parameter :: ST_D11T          =  8   !< fibre damage variable (tension) \f$ d_{11}^T \in [0,1] \f$
   integer, parameter :: ST_D11C          =  9   !< fibre damage variable (compression) \f$ d_{11}^C \in [0,1] \f$
   integer, parameter :: ST_THETAC        = 10   !< fracture plane angle \f$ \theta_f \in [0,\pi] \f$
   integer, parameter :: ST_MC_D          = 11   !< matrix damage variable (compression)  \f$ d_{2}^C  \in [0,1]\f$  
   integer, parameter :: ST_MC_EPS0       = 12   !< initiation strain (matrix compression) \f$ \gamma^0_R  > 0\f$
   integer, parameter :: ST_MC_X          = 13   !< initiation stress (matrix compression) \f$ \tau^0_R > 0\f$
   integer, parameter :: ST_MC_EPSF       = 14   !< strain at failure (matrix compression) \f$ \gamma^f_R > \gamma^0_R\f$ 
   integer, parameter :: ST_EPS_INEL_12   = 15   !< inelastic shear strain in plane 12 \f$ \varepsilon^{\text{in}}_{12} > 0\f$
   integer, parameter :: ST_EPS_INEL_23   = 16   !< inelastic shear strain in plane 23 \f$ \varepsilon^{\text{in}}_{23} > 0\f$
   integer, parameter :: ST_EPS_INEL_13   = 17   !< inelastic shear strain in plane 13 \f$ \varepsilon^{\text{in}}_{13} > 0\f$
   integer, parameter :: ST_LENGTH        = 18   !< characteristic length (from Abaqus - on the initial configuration) \f$ l_c \f$
   integer, parameter :: ST_SIGE_12       = 19   !< effective shear stress 12
   integer, parameter :: ST_SIGE_23       = 20   !< effective shear stress 23
   integer, parameter :: ST_SIGE_13       = 21   !< effective shear stress 13
   integer, parameter :: ST_BAD           = 22   !< if non zero, the element produced an error
   integer, parameter :: ST_MAX           = 22   !< max index (be sure this number equals the highest `ST_` number)
   !> \}
   
   !> \defgroup grpidx ABAQUS indexes. 
   !> ABAQUS indexes for 3x3 symetric tensors (for readability).
   !> \details For example, you should use `stressNew(k,S_12)` instead of `stressNew(k,4)`.
   !> \{
   integer, parameter :: S_11 =  1     !< component 11
   integer, parameter :: S_22 =  2     !< component 22
   integer, parameter :: S_33 =  3     !< component 33
   integer, parameter :: S_12 =  4     !< component 12
   integer, parameter :: S_23 =  5     !< component 23
   integer, parameter :: S_13 =  6     !< component 13
   !> \}

   !> structure containing material data related to orthotropic elasticity
   type s_material_elastic
      real(DP) :: E1          !< Young's modulus \f$ E_1 \f$
      real(DP) :: E2          !< Young's modulus \f$ E_2 \f$
      real(DP) :: E3          !< Young's modulus \f$ E_3 \f$
      real(DP) :: nu12        !< Poisson's ratio \f$ \nu_{12} \f$
      real(DP) :: nu23        !< Poisson's ratio \f$ \nu_{23} \f$
      real(DP) :: nu13        !< Poisson's ratio \f$ \nu_{13} \f$
      real(DP) :: nu21        !< Poisson's ratio \f$ \nu_{21} \f$  (computed from above values)
      real(DP) :: nu32        !< Poisson's ratio \f$ \nu_{32} \f$  (computed from above values)
      real(DP) :: nu31        !< Poisson's ratio \f$ \nu_{31} \f$  (computed from above values)
      real(DP) :: G12         !< shear modulus in plane 12  \f$ G_{12} \f$ 
      real(DP) :: G23         !< shear modulus in plane 23  \f$ G_{23} \f$ 
      real(DP) :: G13         !< shear modulus in plane 13  \f$ G_{13} \f$ 
   end type s_material_elastic

   !> structure containing material data related to matrix failure
   type s_material_matrix
      real(DP) :: Yt          !< tensile strength (transverse direction)
      real(DP) :: Yc          !< compressive strength (transverse direction)
      real(DP) :: tf          !< fracture angle for pure transverse compression  \f$ \theta_f \f$ ( = k_idm::HCP_THETAF )
      real(DP) :: S12         !< shear strength in plane 12  \f$ S_{12} \f$
      real(DP) :: S23         !< shear strength in plane 23  \f$ S_{23}^A \f$
      real(DP) :: munt        !< friction coefficient \f$ \mu_{NT} \f$
      real(DP) :: mu1n        !< friction coefficient \f$ \mu_{1N} \f$
      real(DP) :: G2T         !< fracture energy (tension) \f$ G_2^T \f$
      real(DP) :: G2C         !< fracture energy (compression) \f$ G_2^C \f$
      real(DP) :: G23         !< fracture energy (shear 23) \f$ G_{23}^s \f$
      real(DP) :: G12         !< fracture energy (shear 12/13) \f$ G_{12}^s \f$
   end type s_material_matrix   
 
   !> structure for a bilinear damage law (inputs)
   type s_bilinear_damage
      real(DP) :: X           !< initialion stress
      real(DP) :: e0          !< initialion strain
      real(DP) :: G           !< fracture energy
      real(DP) :: eF          !< failure strain
      real(DP) :: L           !< characteristic length
      real(DP) :: bad         !< error state
   end type s_bilinear_damage 

   !> structure for a bilinear damage law (outputs)
   type s_bilinear_state
      real(DP) :: dT          !< damage in tension
      real(DP) :: dC          !< damage in compression
      real(DP) :: e           !< current strain
   end type s_bilinear_state
   
   !> structure containing material data related to fibre damage
   type s_material_fibre
      type(s_bilinear_damage) :: T   !< tension
      type(s_bilinear_damage) :: C   !< compression
   end type s_material_fibre 
 
   !> structure for material data related to nonlinear shear
   type s_material_shear
      real(DP) :: gamma_0     !< initiation strain \f$ \gamma_{ij}^0 \f$
      real(DP) :: gamma_max   !< strain corresponding to maximum shear \f$ \gamma_{ij}^{\max} \f$
      real(DP) :: Gshear      !< facture energy
      real(DP) :: a           !< shear modulus reduction coefficient \f$ a^{(ij)} \f$
      real(DP) :: c1          !< coefficient of the cubic curve \f$ c_1^{(ij)} \f$
      real(DP) :: c2          !< coefficient of the cubic curve \f$ c_2^{(ij)} \f$
      real(DP) :: c3          !< coefficient of the cubic curve \f$ c_3^{(ij)} \f$
   end type s_material_shear 
      
   !> structure for a symetric tensor (order2)
   !! @todo rename s11 to c11 (for "component 11")
   type s_stensor
      real(DP) :: s11         !< abaqus comp. 1 
      real(DP) :: s22         !< abaqus comp. 2
      real(DP) :: s33         !< abaqus comp. 3
      real(DP) :: s12         !< abaqus comp. 4
      real(DP) :: s23         !< abaqus comp. 5
      real(DP) :: s13         !< abaqus comp. 6
   end type s_stensor  
    
   !> structure for a (min, max) pair
   type s_minmax
      real(DP) :: min         !< minimum value
      real(DP) :: max         !< maximum value
   end type s_minmax 
    
   !> structure for stress bounding box
   type s_bbox
      type(s_minmax) :: snn   !< \f$ \sigma_{NN} \f$ component
      type(s_minmax) :: snt   !< \f$ \sigma_{NT} \f$ component
      type(s_minmax) :: s1n   !< \f$ \sigma_{1N} \f$ component
   end type s_bbox  
   
   !> structure for the zeros of each term of the matrix failure criterion
   type s_zeros
      real(DP) :: F1z1        !< first zero of term 1
      real(DP) :: F1z2        !< second zero of term 1
      real(DP) :: F2z1        !< first zero of term 2
      real(DP) :: F3z1        !< first zero of term 3
      real(DP) :: F3z2        !< second zero of term 3
   end type s_zeros 
    
   !> structure for the matrix failure state
   type s_fstate
      logical :: failure               !< failure flag (true is the criterion is met)
      real(DP) :: fm                   !< global maximum \f$ F^{\max} = F(\theta_f^{\max}) \f$
      real(DP) :: tm                   !< global maximum \f$ \theta_f^{\max} \f$
      real(DP), dimension(3) :: fmax   !< array of local maxima \f$ F_i \f$
      real(DP), dimension(3) :: tmax   !< array of local maxima \f$ \theta_i \f$
      integer  :: n                    !< size of fmax/tmax arrays
   end type s_fstate

   !> structure for the stresses acting on the fracture plane
   !> \todo s1n,snn,snt should be changed to a s_stensor
   type s_rstress
      real(DP) :: s1n           !< \f$ \sigma_{1N} \f$
      !real(DP) :: s1t !useless
      real(DP) :: snn           !< \f$ \sigma_{NN} \f$
      !real(DP) :: stt !useless
      real(DP) :: snt           !< \f$ \sigma_{NT} \f$
      real(DP) :: F1            !< first term of Puck's criterion
      real(DP) :: F2            !< second term of Puck's criterion
      real(DP) :: F3            !< third term of Puck's criterion
      real(DP) :: F             !< sum of the three terms
   end type s_rstress
   
   !> structure for the state of the material related to matrix damage
   type s_matrix_state
      real(DP) :: d                  !< resulting combined damage
      real(DP) :: thetaC             !< fracture plane angle
      type(s_bilinear_damage) :: C   !< 
   end type s_matrix_state

   !> options for the Brent algorithms
   type s_brent_opts
      real(DP)  :: tol        !< tolerance
      integer   :: itmax      !< maximum number of iterations
      logical   :: verb       !< print each iteration if `.true.`
   end type s_brent_opts

end module K_IDM

