! $Id: k_utilities.for 1045 2012-08-17 01:54:27Z boman $
!> \file k_utilities.for
!> Replacement of the utilities from Abaqus.
!>
!> These routines allow us to build the subroutine without Abaqus.
!> In particular, they are called by the python interface.

!> \ingroup grptools
!> Get the name of the Abaqus Job.
subroutine VGETJOBNAME(jobname, ljobname)
   use K_IDM
   implicit none
   character(len=*), intent(out) :: jobname   !< output string
   integer, intent(out)          :: ljobname  !< length of the output string
   jobname  = "PythonJOB"
   ljobname = len_trim(jobname)
end subroutine VGETJOBNAME

!> \ingroup grptools
!> Get the name of the output directory.
subroutine VGETOUTDIR(joboutdir, ljoboutdir)
   use K_IDM
   implicit none
   character(len=*), intent(out) :: joboutdir   !< output string
   integer, intent(out)          :: ljoboutdir  !< length of the output string
   joboutdir  = "."
   ljoboutdir = len_trim(joboutdir)
end subroutine VGETOUTDIR

!> \ingroup grptools
!> Quit Abaqus nicely.
!!
!! In Abaqus, this function also closes the `.odb`. \n
!! It must be used in the VUMAT instead of ``stop``. 
subroutine XPLB_EXIT()
   implicit none
   stop
end subroutine XPLB_EXIT

!> \ingroup grptools
!> Display an error or a warning or an info message.
!!
!! This replacement routine does nothing.
!! \todo print the message (not easy - the format must be converted).
subroutine XPLB_ABQERR(lop, string, intv, realv, charv)
   use K_IDM
   implicit none
   !> level of warning.
   !>  * Set `lop = 1` if an informational message is to be issued.
   !>  * Set `lop = -1` if a warning message is to be issued.
   !>  * Set `lop = -2` if an error message is to be issued and the analysis is to be continued.
   !>  * Set `lop = -3` if an error message is to be issued and the analysis is to be stopped immediately.
   integer :: lop
   character(len=*) :: string  !< string to be displayed
   !integer, dimension(*)          :: intv   ! (error on intel with "/warn:all" if scalar instead of array)
   !real(DP), dimension(*)         :: realv  ! (error on intel with "/warn:all" if scalar instead of array)
   !character(len=*), dimension(8) :: charv  ! (error on intel with "/warn:all" if len(charv)!=8)
   integer           :: intv   !< list of integers appearing in the string
   real(DP)          :: realv  !< list of real numbers appearing in the string
   character(len=*)  :: charv  !< list of characters appearing in the string
   
   ! TODO: print the string
   
   ! stop if needed
   if(lop==-3) then
      call XPLB_EXIT()
   end if
end subroutine XPLB_ABQERR

