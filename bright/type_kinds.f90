!------------------------------------------------------------------------------
!M+
! NAME:
!       type_kinds
!
! PURPOSE:
!       Module to hold specification kinds for variable declaration.
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       USE type_kinds
!
! OUTPUTS:
!       Byte     Specification kind for byte (1-byte) integer variable
!       Short    Specification kind for short (2-byte) integer variable
!       Long     Specification kind for long (4-byte) integer variable
!       LLong    Specification kind for double long (8-byte) integer variable
!       Single   Specification kind for single precision (4-byte) real variable
!       Double   Specification kind for double precision (8-byte) real variable
!       Quad     Specification kind for quad precision (16-byte) real variable
!
!       ip_kind: Generic specification kind for integer
!       fp_kind: Generic specification kind for floating point
!
! MODULES:
!       None
!
! CONTAINS:
!       type_size:  PUBLIC function to return the number of bytes used to
!                   represent the data type.
!
! SIDE EFFECTS:
!       If the LLong or Quad type kinds are not available they default to the
!       Long and Double kind specifications.
!
! RESTRICTIONS:
!       None
!
! EXAMPLE:
!       USE type_kinds
!       INTEGER( Long ) :: i, j
!       REAL( Single ) :: x, y
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Jan-1996
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 1996,2000 Paul van Delst
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


MODULE type_kinds


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC ABS,                &
            KIND,               &
            SELECTED_INT_KIND,  &
            SELECTED_REAL_KIND, &
            SIZE,               &
            TRANSFER,           &
            TRIM


  ! ------------------------------------
  ! Kind definitions for variable typing
  ! ------------------------------------

  ! -- Integer types
  INTEGER, PARAMETER, PUBLIC  :: Byte    = SELECTED_INT_KIND(1)   ! Byte  integer
  INTEGER, PARAMETER, PUBLIC  :: Short   = SELECTED_INT_KIND(4)   ! Short integer
  INTEGER, PARAMETER, PUBLIC  :: Long    = SELECTED_INT_KIND(8)   ! Long  integer

  ! -- Set extra long integer type if available
  INTEGER, PARAMETER, PRIVATE :: LLong_t = SELECTED_INT_KIND(16)  ! LLong integer
  INTEGER, PARAMETER, PUBLIC  :: LLong   = ( ( ABS( LLong_t ) + LLong_t ) * LLong_t + &
                                             ( ABS( LLong_t ) - LLong_t ) * Long ) / &
                                           ( 2 * ABS( LLong_t ) )

  ! -- Floating point types
  INTEGER, PARAMETER, PUBLIC  :: Single = SELECTED_REAL_KIND(6)  ! Single precision
  INTEGER, PARAMETER, PUBLIC  :: Double = SELECTED_REAL_KIND(15) ! Double precision

  ! -- Set quad precision if it is available
  INTEGER, PARAMETER, PRIVATE :: Quad_t = SELECTED_REAL_KIND(20) ! Quad precision
  INTEGER, PARAMETER, PUBLIC  :: Quad   = ( ( ABS( Quad_t ) + Quad_t ) * Quad_t + &
                                            ( ABS( Quad_t ) - Quad_t ) * Double ) / &
                                          ( 2 * ABS( Quad_t ) )


  ! --------------------------------
  ! Expected byte-sizes of the kinds
  ! --------------------------------

  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Byte_kind  = 1
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Short_kind = 2
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Long_kind  = 4
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_LLong_kind = 8

  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Single_kind = 4
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Double_kind = 8
  INTEGER, PARAMETER, PUBLIC :: n_bytes_for_Quad_kind   = 16


  ! ----------------------------------------------------
  ! Default generic floating point and integer precision
  ! ----------------------------------------------------

  INTEGER, PARAMETER, PUBLIC :: ip_kind = Long
  INTEGER, PARAMETER, PUBLIC :: fp_kind = Single
!  INTEGER, PARAMETER, PUBLIC :: fp_kind = Double


  ! ------------------------------
  ! Explicit subprogram visibility
  ! ------------------------------

  PUBLIC :: type_size


CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       type_size
!
! PURPOSE:
!       PUBLIC function to determine the size (in bytes) of a particular data type.
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       result = type_size( type_kind )
!
! INPUT ARGUMENTS:
!       type_kind:   String describing the definition of the data type. Valid
!                    values are (CASE SENSITIVE):
!                      "Byte"
!                      "Short"
!                      "Long"
!                      "LLong"
!                      "Single"
!                      "Double"
!                      "Quad"
!                      "ip_kind"
!                      "fp_kind"
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The returned value is the number of bytes used to represent the data
!       type on the platform the source code was compiled.
!
!       If an invalid/unrecognised type_kind string was passed, the returned
!       result is -1.
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
!S-
!--------------------------------------------------------------------------------

  FUNCTION type_size( type_kind )

    CHARACTER( LEN = * ), INTENT( IN ) :: type_kind
    INTEGER                            :: type_size

    CHARACTER( LEN = 1 ), DIMENSION( 1 ) :: x

    SELECT CASE ( TRIM( type_kind ) )

      CASE ( 'Byte' )
        type_size = SIZE( TRANSFER( 0_Byte, x ) )

      CASE ( 'Short' )
        type_size = SIZE( TRANSFER( 0_Short, x ) )

      CASE ( 'Long' )
        type_size = SIZE( TRANSFER( 0_Long, x ) )

      CASE ( 'LLong' )
        type_size = SIZE( TRANSFER( 0_LLong, x ) )

      CASE ( 'ip_kind' )
        type_size = SIZE( TRANSFER( 0_ip_kind, x ) )

      CASE ( 'Single' )
        type_size = SIZE( TRANSFER( 0.0_Single, x ) )

      CASE ( 'Double' )
        type_size = SIZE( TRANSFER( 0.0_Double, x ) )

      CASE ( 'Quad' )
        type_size = SIZE( TRANSFER( 0.0_Quad, x ) )

      CASE ( 'fp_kind' )
        type_size = SIZE( TRANSFER( 0.0_fp_kind, x ) )

      CASE DEFAULT
        WRITE( *, '( /5x, "Invalid type kind: ", a )' ) TRIM( type_kind )
        type_size = -1

    END SELECT

  END FUNCTION type_size

END MODULE type_kinds


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: type_kinds.f90,v 1.1 2003/06/07 16:39:36 phil Exp $
!
! $Date: 2003/06/07 16:39:36 $
!
! $Revision: 1.1 $
!
! $State: Exp $
!
! $Log: type_kinds.f90,v $
! Revision 1.1  2003/06/07 16:39:36  phil
! new diagnostics
!
! Revision 3.4  2001/05/09 19:08:12  paulv
! - Changed definitions of and references to INT_PRECISION and FP_PRECISION
!   to IP_KIND and FP_KIND.
! - IP_KIND and FP_KIND definitions changed from
!     INTEGER, PARAMETER, PUBLIC :: ip_kind = KIND( 0 )
!     INTEGER, PARAMETER, PUBLIC :: fp_kind = KIND( 0.0 )
!   to
!     INTEGER, PARAMETER, PUBLIC :: ip_kind = Long
!     INTEGER, PARAMETER, PUBLIC :: fp_kind = Single
!   using the already defined specific kind types.
! - Added a commented line containing the definition of FP_KIND for double
!   precision:
!   !  INTEGER, PARAMETER, PUBLIC :: fp_kind = Double
!
! Revision 3.3  2000/12/04 13:14:43  paulv
! - Added HTML documentation delimiters.
!
! Revision 3.2  2000/11/20 20:48:09  paulv
! - Removed CIMSS copyright text due to GNU license.
!
! Revision 3.1  2000/11/20 20:46:21  paulv
! - New version. Brought up to date with NCEP_RTM type_kinds module.
!
! Revision 2.2  2000/04/06 16:56:04  paulv
! Removed 8-byte integer type kind. The Absoft F90 compiler did not like it
! and set the kind value to -1.
!
! Revision 2.1  2000/04/03 18:11:57  paulv
! - Added 8-byte integer variable kind
! - Added quad-precision floating point variable kind
! - Added definitions for the expected number of bytes each variable type
!   will occupy in memory.
!
! Revision 1.3  1998/11/20 21:02:27  paulv
! Added RCS Id keyword as a PRIVATE character parameter.
! Added PUBLIC attribute to integer and real type declarations.
!
! Revision 1.2  1998/04/23 14:28:54  paulv
! Documentation update.
!
! Revision 1.1  1998/03/27 22:43:51  paulv
! Initial revision
!
!
!==============================================================================
