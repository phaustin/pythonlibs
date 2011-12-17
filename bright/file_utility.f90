!------------------------------------------------------------------------------
!M+
! NAME:
!       file_utility
!
! PURPOSE:
!       Module containing generic file utility routines
!
! CATEGORY:
!       Utility
!
! CALLING SEQUENCE:
!       USE file_utility
!
! OUTPUTS:
!       None
!
! MODULES:
!       None.
!
! CONTAINS:
!       get_lun:     PUBLIC function to return a free logical unit number for
!                    file access.
!
!       file_exists: PUBLIC function to determine if a named file exists.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jul-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

MODULE file_utility


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE


  ! ----------------------------------
  ! Explicit visibility of subprograms
  ! ----------------------------------

  PUBLIC :: get_lun, &
            file_exists


CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       get_lun
!
! PURPOSE:
!       PUBLIC function to obtain a free logical unit number for file access
!
! CALLING SEQUENCE:
!       result = get_lun()
!
! INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns a default integer that can be used as a logical unit
!       number to open and access a file.
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       On some older compilers (mostly f77 so it may not be an issue for f90
!       code) the act of inquiring about a unit number, whether it is connected
!       or not, can cause it to become defined as connected, that is, it may be
!       seen as being in use when it is not. This should be tested for before
!       use.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The search for a free logical unit number begins at 10. The logical
!       unit number if tested to see if it is connected to an open file. If
!       so, it is incremented by 1. This is repeated until a free logical
!       unit number is found.
!S-
!------------------------------------------------------------------------------

  FUNCTION get_lun() RESULT( lun )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    INTEGER :: lun
    LOGICAL :: file_open


    ! --------------------------------------------
    ! Initialise logical unit number and file_open
    ! --------------------------------------------

    lun = 9
    file_open = .TRUE.


    ! ------------------------------
    ! Start open loop for lun search
    ! ------------------------------

    lun_search: DO

      ! -- Increment logical unit number
      lun = lun + 1

      ! -- Check if file is open
      INQUIRE( UNIT = lun, OPENED = file_open )

      ! -- Is this lun available?
      IF ( .NOT. file_open ) EXIT lun_search

    END DO lun_search

  END FUNCTION get_lun



!------------------------------------------------------------------------------
!S+
! NAME:
!       file_exists
!
! PURPOSE:
!       PUBLIC function to determine if a file exists.
!
! CALLING SEQUENCE:
!       result = file_exists( file_name )
!
! INPUT ARGUMENTS:
!       file_name:  Name of the file the existence of which is to be determined.
!                   UNITS:      None
!                   TYPE:       Character
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns a logical result.
!
!       result = .TRUE.  => file exists
!              = .FALSE. => file does not exist
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The file name is INQUIREd by FILE keyword. The result of the inquiry
!       is the function result.
!S-
!------------------------------------------------------------------------------

  FUNCTION file_exists( file_name ) RESULT ( existence )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    CHARACTER( * ), INTENT( IN ) :: file_name
    LOGICAL :: existence


    ! ---------------
    ! Inquire by name
    ! ---------------

    INQUIRE( FILE = file_name, EXIST = existence )

  END FUNCTION file_exists

END MODULE file_utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: file_utility.f90,v 1.1 2003/06/07 16:39:35 phil Exp $
!
! $Date: 2003/06/07 16:39:35 $
!
! $Revision: 1.1 $
!
! $State: Exp $
!
! $Log: file_utility.f90,v $
! Revision 1.1  2003/06/07 16:39:35  phil
! new diagnostics
!
! Revision 1.2  2000/12/04 13:16:46  paulv
! - Added HTML documentation delimiters.
! - Added UNIT= keyword to INQUIRE in GET_LUN()
! - Updated GET_LUN() header documentation.
!
! Revision 1.1  2000/11/20 20:51:50  paulv
! Initial checkin.
!
!
!
!

