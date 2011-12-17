!------------------------------------------------------------------------------
!M+
! NAME:
!       planck_functions
!
! PURPOSE:
!       Module containing Planck function radiance, temperature, dB/dT, and 
!       dT/dB routines for scalar and rank-1 array inputs.
!
!       All calculations are done in double-precision.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE planck_functions
!
! MODULES:
!       type_kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       fundamental_constants:  Module containing definitions of the
!                               constants required for Planck radiance
!                               calculations.
!
!       error_handler:          Module to define simple error codes and
!                               handle error conditions
!
! CONTAINS:
!       planck_radiance:        Function to calculate the Planck radiance
!                               given the wavenumber or wavelength, and
!                               temperature.
!
!       planck_temperature:     Function to calculate the Planck temperature
!                               given the wavenumber or wavelength, and radiance.
!
!       planck_dbdt:            Function to calculate the derivative of
!                               the Planck radiance with respect to temperature
!                               given the wavenumber or wavelength, and
!                               temperature.
!
!       planck_dtdb:            Function to calculate the Planck temperature
!                               derivative with respect to radiance given the
!                               wavenumber or wavelength, and radiance.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None known
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 14-Oct-1999
!                     paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 1999, 2000 Paul van Delst
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

MODULE planck_functions



  ! ------------
  ! Modules used
  ! ------------
 
  USE type_kinds
  USE fundamental_constants, ONLY: C_1, C_2
  USE error_handler


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------------------
  ! Expose/hide members explicitly
  ! ------------------------------

  PRIVATE
  PUBLIC :: planck_radiance
  PUBLIC :: planck_temperature
  PUBLIC :: planck_dbdt
  PUBLIC :: planck_dtdb


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE planck_radiance
    MODULE PROCEDURE scalar_planck_radiance       ! Main function
    MODULE PROCEDURE R1_x_S_t_planck_radiance     ! I/P Rank1  x,  Scalar temperature
    MODULE PROCEDURE S_x_R1_t_planck_radiance     ! I/P Scalar x,  Rank1  temperature
    MODULE PROCEDURE R1_xt_planck_radiance        ! I/P Rank1  x,  Rank1  temperature
    MODULE PROCEDURE R1_x_R2_t_planck_radiance    ! I/P Rank1  x,  Rank2  temperature
  END INTERFACE ! planck_radiance

  INTERFACE planck_temperature
    MODULE PROCEDURE scalar_planck_temperature    ! Main function
    MODULE PROCEDURE R1_x_S_r_planck_temperature  ! I/P Rank1  x,  Scalar radiance
    MODULE PROCEDURE S_x_R1_r_planck_temperature  ! I/P Scalar x,  Rank1  radiance
    MODULE PROCEDURE R1_xr_planck_temperature     ! I/P Rank1  x,  Rank1  radiance
    MODULE PROCEDURE R1_x_R2_r_planck_temperature ! I/P Rank1  x,  Rank2  radiance
  END INTERFACE ! planck_temperature

  INTERFACE planck_dbdt
    MODULE PROCEDURE scalar_planck_dbdt           ! Main function
    MODULE PROCEDURE R1_x_S_t_planck_dbdt         ! I/P Rank1  x,  Scalar temperature
    MODULE PROCEDURE S_x_R1_t_planck_dbdt         ! I/P Scalar x,  Rank1  temperature
    MODULE PROCEDURE R1_xt_planck_dbdt            ! I/P Rank1  x,  Rank1  temperature
    MODULE PROCEDURE R1_x_R2_t_planck_dbdt        ! I/P Rank1  x,  Rank2  temperature
  END INTERFACE ! planck_dbdt

  INTERFACE planck_dtdb
    MODULE PROCEDURE scalar_planck_dtdb           ! Main function
    MODULE PROCEDURE R1_x_S_r_planck_dtdb         ! I/P Rank1  x,  Scalar radiance
    MODULE PROCEDURE S_x_R1_r_planck_dtdb         ! I/P Scalar x,  Rank1  radiance
    MODULE PROCEDURE R1_xr_planck_dtdb            ! I/P Rank1  x,  Rank1  radiance
    MODULE PROCEDURE R1_x_R2_r_planck_dtdb        ! I/P Rank1  x,  Rank2  radiance
  END INTERFACE ! planck_dtdb



  ! -----------------
  ! Module aarameters
  ! -----------------

  ! -- Public parameters
  INTEGER, PUBLIC, PARAMETER :: WAVENUMBER_UNIT = 1
  INTEGER, PUBLIC, PARAMETER :: WAVELENGTH_UNIT = 2

  ! -- Floating point precision
  REAL( Double ), PARAMETER :: TOLERANCE = EPSILON( 1.0_Double )

  ! -- Scale factors
  INTEGER,         PARAMETER                               :: N_SCALE_FACTORS = 2
  REAL( Double ),  PARAMETER, DIMENSION( N_SCALE_FACTORS ) :: RADIANCE_SCALE_FACTOR = &
                                                              (/ 1000.0d0, 1.0d0 /) 
  REAL( Double ),  PARAMETER, DIMENSION( N_SCALE_FACTORS ) :: C_1_SCALE_FACTOR = &
                                                              (/ 1.0d+08, 1.0d+24 /)
  REAL( Double ),  PARAMETER, DIMENSION( N_SCALE_FACTORS ) :: C_2_SCALE_FACTOR = &
                                                              (/ 100.0d0, 1.0d+06 /)


CONTAINS



!------------------------------------------------------------------------------
!S+
! NAME:
!       planck_radiance
!
! PURPOSE:
!       Function to calculate the Planck radiance given the wavenumber or
!       wavelength, and temperature.  Calculation is done in double precision.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       error_status = planck_radiance( unit_key,                 & ! Input
!                                       x,                        & ! Input
!                                       temperature,              & ! Input
!                                       radiance,                 & ! Output
!                                       message_log = message_log ) ! Optional input
!
! INPUT ARGUMENTS:
!       unit_key:     Flag specifying spectral units. Valid values are:
!                       WAVENUMBER_UNIT: x in cm-1, radiance in mW/(m2.sr.cm-1)
!                       WAVELENGTH_UNIT: x in micron, radiance in W/(m2.sr.micron)
!                     UNITS:      None.
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       x:            Spectral ordinate. Can be a SCALAR or VECTOR
!                     UNITS:      Inverse centimetres (cm^-1) or microns (um).
!                     TYPE:       Double precision float
!                     DIMENSION:  Scalar or Nx1 vector
!                     ATTRIBUTES: INTENT( IN )
!
!       temperature:  Temperature(s) for which the Planck radiance(s)
!                     is(are) required. Can be a SCALAR or VECTOR. 
!                     See radiance output description for allowed
!                     dimensionality.
!                     UNITS:      Kelvin
!                     TYPE:       Double precision float
!                     DIMENSION:  Scalar or Nx1 vector
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       radiance:     The Planck radiance(s) for the supplied temperature(s).
!                     The output dimensions are determined by the input.
!                     In the following chart N == frequencies, K == temperatures or
!                     spectra (i.e. separate temperatures or temperature spectra):
!
!                         Input X     Input TEMPERATURE    Output RADIANCE
!                        dimension       dimension            dimension
!                     ------------------------------------------------------
!                         scalar          scalar                scalar
!                           N             scalar                  N
!                         scalar            K                     K
!                           N               N                     N
!                           N               K                not allowed
!                           N             N x K                 N x K
!
!                     UNITS:      mW/(m2.sr.cm-1) OR W/(m2.sr.micron)
!                     TYPE:       Double precision float
!                     DIMENSION:  See chart above.
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defingin the error status.
!
!       If result = SUCCESS the Planck calculation was successful
!                 = FAILURE an error was found with the input.
!
! CALLS:
!      display_message:    Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       For wavenumber input, the Planck radiance is calculated using:
!
!                   c1 * wavenumber^3
!         B =  ----------------------------
!                  ( c2 * wavenumber )
!               EXP( --------------- ) - 1
!                  (        T        )
!
!       For wavelength input:
!
!                                    c1
!         B = --------------------------------------------------
!                             [    (        c2      )     ]
!              wavelength^5 * [ EXP( -------------- ) - 1 ]
!                             [    ( wavelength * T )     ]
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck radiance results to return
!       radiances in the units of mW/(m2.sr.cm-1) for wavenumber input
!       or W/(m2.sr.micron) for wavelength input.
!
!       WAVENUMBER
!       ----------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!       All calculations are done in double precision.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Jul-1996
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION scalar_planck_radiance( units,        &  ! Input
                                   x,            &  ! Input
                                   temperature,  &  ! Input
                                   radiance,     &  ! Output
                                   message_log ) &  ! Optional input
                                 RESULT ( error_status )


    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------
 
    INTEGER,         INTENT( IN )            :: units
    REAL( Double ),  INTENT( IN )            :: x
    REAL( Double ),  INTENT( IN )            :: temperature

    REAL( Double ),  INTENT( OUT )           :: radiance
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_RADIANCE'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( Double ) :: frequency, wavelength
    REAL( Double ) :: x_c_1, x_c_2
    REAL( Double ) :: exponential



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Units input
    IF ( ( units /= WAVENUMBER_UNIT ) .AND. &
         ( units /= WAVELENGTH_UNIT )       ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid UNITS argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Frequency/wavelength input
    IF ( x < TOLERANCE ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid X (frequency/wavelength) argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
 
    ! -- Temperature input
    IF ( temperature < TOLERANCE ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid TEMPERATURE argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


 
    !-------------------------------------------------------------------------------
    !                       -- Calculate spectral parameters --
    !-------------------------------------------------------------------------------

    IF ( units == WAVENUMBER_UNIT ) THEN


      ! -----------------------
      ! Spectral units in cm^-1
      ! -----------------------

      frequency = x

      x_c_1 = C_1_SCALE_FACTOR( units ) * C_1 * ( frequency**3 )
      x_c_2 = C_2_SCALE_FACTOR( units ) * C_2 * frequency

    ELSE


      ! -------------------------
      ! Spectral units in microns
      ! -------------------------

      wavelength = x

      x_c_1 = C_1_SCALE_FACTOR( units ) * C_1 / ( wavelength**5 )
      x_c_2 = C_2_SCALE_FACTOR( units ) * C_2 / wavelength

    END IF



    !-----------------------------------------------------------------------
    !                     -- Calculate radiance --
    !-----------------------------------------------------------------------

    exponential = EXP( x_c_2 / temperature )
    radiance    = RADIANCE_SCALE_FACTOR( units ) * x_c_1 / ( exponential - 1.0d0 )

    RETURN

  END FUNCTION scalar_planck_radiance



  FUNCTION R1_x_S_t_planck_radiance( units,        &  ! Input
                                     x,            &  ! Input
                                     temperature,  &  ! Input
                                     radiance,     &  ! Output
                                     message_log ) &  ! Optional input
                                   RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------
 
    INTEGER,                         INTENT( IN )  :: units
    REAL( Double ),  DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ),                  INTENT( IN )  :: temperature

    REAL( Double ),  DIMENSION( : ), INTENT( OUT ) :: radiance
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log



    ! ------
    ! Result
    ! ------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_RADIANCE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Output array has incorrect length
    n = SIZE( x )
    IF ( SIZE( radiance ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of RADIANCE output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, n

      error_status = scalar_planck_radiance( units,                    &  ! Input
                                             x(i),                     &  ! Input
                                             temperature,              &  ! Input
                                             radiance(i),              &  ! Output
                                             message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION R1_x_S_t_planck_radiance




  FUNCTION S_x_R1_t_planck_radiance( units,        &  ! Input
                                     x,            &  ! Input
                                     temperature,  &  ! Input
                                     radiance,     &  ! Output
                                     message_log ) &  ! Optional input
                                   RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------
 
    INTEGER,                         INTENT( IN )  :: units
    REAL( Double ),                  INTENT( IN )  :: x
    REAL( Double ),  DIMENSION( : ), INTENT( IN )  :: temperature

    REAL( Double ),  DIMENSION( : ), INTENT( OUT ) :: radiance
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log



    ! ------
    ! Result
    ! ------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_RADIANCE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Output array has incorrect length
    k = SIZE( temperature )
    IF ( SIZE( radiance ) /= k ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of TEMPERATURE input and RADIANCE output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, k

      error_status = scalar_planck_radiance( units,                    &  ! Input
                                             x,                        &  ! Input
                                             temperature(i),           &  ! Input
                                             radiance(i),              &  ! Output
                                             message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION S_x_R1_t_planck_radiance




  FUNCTION R1_xt_planck_radiance( units,        &  ! Input
                                  x,            &  ! Input
                                  temperature,  &  ! Input
                                  radiance,     &  ! Output
                                  message_log ) &  ! Optional input
                                RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------
 
    INTEGER,                         INTENT( IN )  :: units
    REAL( Double ),  DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ),  DIMENSION( : ), INTENT( IN )  :: temperature

    REAL( Double ),  DIMENSION( : ), INTENT( OUT ) :: radiance
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log



    ! ------
    ! Result
    ! ------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_RADIANCE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Arrays have same lengths
    n = SIZE( x )
    IF ( SIZE( temperature ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of X and TEMPERATURE inputs', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( radiance ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of RADIANCE output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, n

      error_status = scalar_planck_radiance( units,                    &  ! Input
                                             x(i),                     &  ! Input
                                             temperature(i),           &  ! Input
                                             radiance(i),              &  ! Output
                                             message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION R1_xt_planck_radiance




  FUNCTION R1_x_R2_t_planck_radiance( units,        &  ! Input
                                      x,            &  ! Input
                                      temperature,  &  ! Input
                                      radiance,     &  ! Output
                                      message_log ) &  ! Optional input
                                    RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------
 
    INTEGER,                            INTENT( IN )  :: units
    REAL( Double ),  DIMENSION( : ),    INTENT( IN )  :: x
    REAL( Double ),  DIMENSION( :, : ), INTENT( IN )  :: temperature

    REAL( Double ),  DIMENSION( :, : ), INTENT( OUT ) :: radiance
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log



    ! ------
    ! Result
    ! ------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_RADIANCE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, n, k



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Arrays have same lengths
    n = SIZE( temperature, DIM = 1 )
    k = SIZE( temperature, DIM = 2 )

    IF ( SIZE( x ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of X[Nx1] and TEMPERATURE[NxK] inputs.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( radiance, DIM = 1 ) /= n .AND. &
         SIZE( radiance, DIM = 2 ) /= k ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of input TEMPERATURE and output RADIANCE.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    spectrum_loop: DO j = 1, k

      frequency_loop: DO i = 1, n

        error_status = scalar_planck_radiance( units,                    &  ! Input
                                               x(i),                     &  ! Input
                                               temperature(i,j),         &  ! Input
                                               radiance(i,j),            &  ! Output
                                               message_log = message_log )  ! Optional input
        IF ( error_status /= SUCCESS ) EXIT spectrum_loop

      END DO frequency_loop

    END DO spectrum_loop

    RETURN

  END FUNCTION R1_x_R2_t_planck_radiance




!------------------------------------------------------------------------------
!S+
! NAME:
!       planck_temperature
!
! PURPOSE:
!       Function to calculate the Planck temperature given the wavenumber or
!       wavelength, and radiance.  Calculation is done in double precision.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       error_status = planck_temperature( unit_key,                 & ! Input
!                                          x,                        & ! Input
!                                          radiance,                 & ! Input
!                                          temperature,              & ! Output
!                                          message_log = message_log ) ! Optional input
!
! INPUT ARGUMENTS:
!       unit_key:     Flag specifying spectral units. Valid values are:
!                       WAVENUMBER_UNIT: x in cm-1, radiance in mW/(m2.sr.cm-1)
!                       WAVELENGTH_UNIT: x in micron, radiance in W/(m2.sr.micron)
!                     UNITS:      None.
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       x:            Spectral ordinate. Can be a SCALAR or VECTOR
!                     UNITS:      Inverse centimetres (cm^-1) or microns (um).
!                     TYPE:       Double precision float
!                     DIMENSION:  Scalar or Nx1 vector
!                     ATTRIBUTES: INTENT( IN )
!
!       radiance:     Planck radiance(s) for which temperature(s) are required.
!                     Can be a SCALAR, VECTOR, or 2-D ARRAY. See temperature 
!                     output for allowed dimensionality.
!                     UNITS:      mW/(m2.sr.cm-1) OR W/(m2.sr.micron)
!                     TYPE:       Double precision float
!                     DIMENSION:  See output temperature dimensionality chart
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       temperature:  The temperature(s) corresponding to the supplied radiance(s).
!                     The output dimensions are determined by the input.
!                     In the following chart N == frequencies, K == temperatures or
!                     spectra (i.e. separate temperatures or temperature spectra):
!
!                         Input X     Input RADIANCE     Output TEMPERATURE
!                        dimension       dimension           dimension
!                     ------------------------------------------------------
!                         scalar          scalar               scalar
!                           N             scalar                 N
!                         scalar            K                    K
!                           N               N                    N
!                           N               K               not allowed
!                           N             N x K                N x K
!
!                     UNITS:      Kelvin
!                     TYPE:       Double precision float
!                     DIMENSION:  See chart above.
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defingin the error status.
!
!       If result = SUCCESS the Planck calculation was successful
!                 = FAILURE an error was found with the input.
!
! CALLS:
!      display_message:    Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       For wavenumber input, the Planck temperature is calculated using:
!
!                    c2 * wavenumber
!         T = -------------------------------
!                ( c1 * wavenumber^3      )
!              LN( ----------------- +  1 )
!                (          B             )
!
!       For wavelength input :
!
!                                c2
!          T = -----------------------------------------
!                              (        c1            )
!               wavelength * LN( ---------------- + 1 )
!                              ( wavelength^5 * B     )
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck radiance results to return
!       radiances in the units of mW/(m2.sr.cm-1) for wavenumber input
!       or W/(m2.sr.micron) for wavelength input.
!
!       WAVENUMBER
!       ----------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!       All calculations are done in double precision.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Jul-1996
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------


  FUNCTION scalar_planck_temperature( units,        &  ! Input
                                      x,            &  ! Input
                                      radiance,     &  ! Input
                                      temperature,  &  ! output
                                      message_log ) &  ! Optional input
                                    RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,         INTENT( IN )  :: units
    REAL( Double ),  INTENT( IN )  :: x
    REAL( Double ),  INTENT( IN )  :: radiance

    REAL( Double ),  INTENT( OUT ) :: temperature
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_TEMPERATURE'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( Double ) :: frequency, wavelength
    REAL( Double ) :: x_c_1, x_c_2
    REAL( Double ) :: logarithm



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Units input
    IF ( ( units /= WAVENUMBER_UNIT ) .AND. &
         ( units /= WAVELENGTH_UNIT )       ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid UNITS argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Frequency/wavelength input
    IF ( x < TOLERANCE ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid X (frequency/wavelength) argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
 
    ! -- Radiance input
    IF ( radiance < TOLERANCE ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid RADIANCE argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


 
    !-------------------------------------------------------------------------------
    !                       -- Calculate spectral parameters --
    !-------------------------------------------------------------------------------

    IF ( units == WAVENUMBER_UNIT ) THEN


      ! -----------------------
      ! Spectral units in cm^-1
      ! -----------------------

      frequency = x

      x_c_1 = C_1_SCALE_FACTOR( units ) * C_1 * ( frequency**3 )
      x_c_2 = C_2_SCALE_FACTOR( units ) * C_2 * frequency

    ELSE


      ! -------------------------
      ! Spectral units in microns
      ! -------------------------

      wavelength = x

      x_c_1 = C_1_SCALE_FACTOR( units ) * C_1 / ( wavelength**5 )
      x_c_2 = C_2_SCALE_FACTOR( units ) * C_2 / wavelength

    END IF



    !-----------------------------------------------------------------------
    !                     -- Calculate radiance --
    !-----------------------------------------------------------------------

    logarithm   = LOG( ( RADIANCE_SCALE_FACTOR( units ) * x_c_1 / radiance ) + 1.0d0 )
    temperature = x_c_2 / logarithm


    RETURN

  END FUNCTION scalar_planck_temperature



  FUNCTION R1_x_S_r_planck_temperature( units,        &  ! Input
                                        x,            &  ! Input
                                        radiance,     &  ! Input
                                        temperature,  &  ! output
                                        message_log ) &  ! Optional input
                                      RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                        INTENT( IN )  :: units
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ),                 INTENT( IN )  :: radiance

    REAL( Double ), DIMENSION( : ), INTENT( OUT ) :: temperature
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_TEMPERATURE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Output array has incorrect length
    n = SIZE( x )
    IF ( SIZE( temperature ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of TEMPERATURE output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

 

    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, n

      error_status = scalar_planck_temperature( units,                    &  ! Input
                                                x(i),                     &  ! Input
                                                radiance,                 &  ! Input
                                                temperature(i),           &  ! Output
                                                message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION R1_x_S_r_planck_temperature



  FUNCTION S_x_R1_r_planck_temperature( units,        &  ! Input
                                        x,            &  ! Input
                                        radiance,     &  ! Input
                                        temperature,  &  ! output
                                        message_log ) &  ! Optional input
                                      RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                        INTENT( IN )  :: units
    REAL( Double ),                 INTENT( IN )  :: x
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: radiance

    REAL( Double ), DIMENSION( : ), INTENT( OUT ) :: temperature
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_TEMPERATURE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Output array has incorrect length
    n = SIZE( radiance )
    IF ( SIZE( temperature ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of TEMPERATURE output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

 

    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, n

      error_status = scalar_planck_temperature( units,                    &  ! Input
                                                x,                        &  ! Input
                                                radiance(i),              &  ! Input
                                                temperature(i),           &  ! Output
                                                message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION S_x_R1_r_planck_temperature



  FUNCTION R1_xr_planck_temperature( units,        &  ! Input
                                     x,            &  ! Input
                                     radiance,     &  ! Input
                                     temperature,  &  ! output
                                     message_log ) &  ! Optional input
                                   RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                        INTENT( IN )  :: units
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: radiance

    REAL( Double ), DIMENSION( : ), INTENT( OUT ) :: temperature
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_TEMPERATURE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Arrays have same lengths
    n = SIZE( x )
    IF ( SIZE( radiance ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of X and RADIANCE inputs', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( temperature ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of TEMPERATURE output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

 

    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, n

      error_status = scalar_planck_temperature( units,                    &  ! Input
                                                x(i),                     &  ! Input
                                                radiance(i),              &  ! Input
                                                temperature(i),           &  ! Output
                                                message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION R1_xr_planck_temperature




  FUNCTION R1_x_R2_r_planck_temperature( units,        &  ! Input
                                         x,            &  ! Input
                                         radiance,     &  ! Input
                                         temperature,  &  ! output
                                         message_log ) &  ! Optional input
                                       RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                           INTENT( IN )  :: units
    REAL( Double ), DIMENSION( : ),    INTENT( IN )  :: x
    REAL( Double ), DIMENSION( :, : ), INTENT( IN )  :: radiance

    REAL( Double ), DIMENSION( :, : ), INTENT( OUT ) :: temperature
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_TEMPERATURE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, n, k



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Arrays have same lengths
    n = SIZE( radiance, DIM = 1 )
    k = SIZE( radiance, DIM = 2 )

    IF ( SIZE( x ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of X[Nx1] and RADIANCE[NxK] inputs.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( temperature, DIM = 1 ) /= n .AND. &
         SIZE( temperature, DIM = 2 ) /= k ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of input RADIANCE and output TEMPERATURE.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    spectrum_loop: DO j = 1, k

      frequency_loop: DO i = 1, n

        error_status = scalar_planck_temperature( units,                    &  ! Input
                                                  x(i),                     &  ! Input
                                                  radiance(i,j),            &  ! Input
                                                  temperature(i,j),         &  ! Output
                                                  message_log = message_log )  ! Optional input
        IF ( error_status /= SUCCESS ) EXIT spectrum_loop

      END DO frequency_loop

    END DO spectrum_loop

    RETURN

  END FUNCTION R1_x_R2_r_planck_temperature




!------------------------------------------------------------------------------
!S+
! NAME:
!       planck_dbdt
!
! PURPOSE:
!       Function to calculate the derivative of the Planck radiance with
!       respect to temperature given the wavenumber or wavelength, and
!       temperature.  Calculation is done in double precision.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       error_status = planck_dbdt( unit_key,                 & ! Input
!                                   x,                        & ! Input
!                                   temperature,              & ! Input
!                                   dbdt,                     & ! Output
!                                   message_log = message_log ) ! Optional input
!
! INPUT ARGUMENTS:
!       unit_key:     Flag specifying spectral units. Valid values are:
!                       WAVENUMBER_UNIT: x in cm-1, radiance in mW/(m2.sr.cm-1)
!                       WAVELENGTH_UNIT: x in micron, radiance in W/(m2.sr.micron)
!                     UNITS:      None.
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       x:            Spectral ordinate. Can be a SCALAR or VECTOR
!                     UNITS:      Inverse centimetres (cm^-1) or microns (um).
!                     TYPE:       Double precision float
!                     DIMENSION:  Scalar or Nx1 vector
!                     ATTRIBUTES: INTENT( IN )
!
!       temperature:  Temperature(s) for which the Planck radiance
!                     derivative(s) is(are) required. Can be a SCALAR
!                     or VECTOR. See dbdt output description for allowed
!                     dimensionality.
!                     UNITS:      Kelvin
!                     TYPE:       Double precision float
!                     DIMENSION:  Scalar or Nx1 vector
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       dbdt:         The derivative(s) of Planck radiance with respect to
!                     temperature for the supplied temperature(s). The 
!                     output dimensions are determined by the input. In the 
!                     following chart N == frequencies, K == temperatures or
!                     spectra (i.e. separate temperatures or temperature spectra):
!
!                         Input X     Input TEMPERATURE      Output DBDT
!                        dimension       dimension            dimension
!                     ------------------------------------------------------
!                         scalar          scalar                scalar
!                           N             scalar                  N
!                         scalar            K                     K
!                           N               N                     N
!                           N               K                not allowed
!                           N             N x K                 N x K
!
!                     UNITS:      mW/(m2.sr.cm-1.K) OR  W/(m2.sr.um.K)
!                     TYPE:       Double precision float
!                     DIMENSION:  See chart above.
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defingin the error status.
!
!       If result = SUCCESS the Planck derivative calculation was successful
!                 = FAILURE an error was found with the input.
!
! CALLS:
!      display_message:    Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       For wavenumber input, the Planck radiance derivative with respect
!       to temperature is calculated using :
!
!                                               ( c2 * wavenumber )
!                   c1 * c2 * wavenumber^4 * EXP( --------------- )
!                                               (        T        )             
!          dB/dT = -------------------------------------------------
!                      {     [    ( c2 * wavenumber )     ] }^2
!                      { T * [ EXP( --------------- ) - 1 ] }
!                      {     [    (        T        )     ] }
!
!
!       For wavelength input :
!                                            (       c2       )
!                               c1 * c2 * EXP( -------------- )
!                                            ( wavelength * T )
!          dB/dT = --------------------------------------------------------
!                                  {     [    (       c2       )     ] }^2
!                   wavelength^6 * { T * [ EXP( -------------- ) - 1 ] }
!                                  {     [    ( wavelength * T )     ] }
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck radiance results to return
!       radiances in the units of mW/(m2.sr.cm-1) for wavenumber input
!       or W/(m2.sr.micron) for wavelength input.
!
!       WAVENUMBER
!       ----------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!       All calculations are done in double precision.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Jul-1996
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------


  FUNCTION scalar_planck_dbdt( units,        &   ! Input
                               x,            &   ! Input
                               temperature,  &   ! Input
                               dbdt,         &   ! Output
                               message_log ) &   ! Optional input
                             RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,         INTENT( IN )  :: units
    REAL( Double ),  INTENT( IN )  :: x
    REAL( Double ),  INTENT( IN )  :: temperature

    REAL( Double ),  INTENT( OUT ) :: dbdt
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_DBDT'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( Double ) :: frequency, wavelength
    REAL( Double ) :: x_c_1, x_c_2
    REAL( Double ) :: exponential



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Units input
    IF ( ( units /= WAVENUMBER_UNIT ) .AND. &
         ( units /= WAVELENGTH_UNIT )       ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid UNITS argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Frequency/wavelength input
    IF ( x < TOLERANCE ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid X (frequency/wavelength) argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
 
    ! -- Temperature input
    IF ( temperature < TOLERANCE ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid TEMPERATURE argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


 
    !-------------------------------------------------------------------------------
    !                       -- Calculate spectral parameters --
    !-------------------------------------------------------------------------------

    IF ( units == WAVENUMBER_UNIT ) THEN


      ! -----------------------
      ! Spectral units in cm^-1
      ! -----------------------

      frequency = x

      x_c_1 = C_1_SCALE_FACTOR( units ) * C_1 * &
              C_2_SCALE_FACTOR( units ) * C_2 * &
              ( frequency**4 )
      x_c_2 = C_2_SCALE_FACTOR( units ) * C_2 * frequency

    ELSE


      ! -------------------------
      ! Spectral units in microns
      ! -------------------------

      wavelength = x

      x_c_1 = C_1_SCALE_FACTOR( units ) * C_1 * &
              C_2_SCALE_FACTOR( units ) * C_2 / &
              ( wavelength**6 )
      x_c_2 = C_2_SCALE_FACTOR( units ) * C_2 / wavelength

    END IF



    !-----------------------------------------------------------------------
    !                     -- Calculate dbdt --
    !-----------------------------------------------------------------------

    exponential = EXP( x_c_2 / temperature )
    dbdt        = RADIANCE_SCALE_FACTOR( units ) * x_c_1 * exponential / &
                       ( temperature * ( exponential - 1.0d0 ) )**2

    RETURN

  END FUNCTION scalar_planck_dbdt



  FUNCTION R1_x_S_t_planck_dbdt( units,        &   ! Input
                                 x,            &   ! Input
                                 temperature,  &   ! Input
                                 dbdt,         &   ! Output
                                 message_log ) &   ! Optional input
                               RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                         INTENT( IN )  :: units
    REAL( Double ),  DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ),                  INTENT( IN )  :: temperature

    REAL( Double ),  DIMENSION( : ), INTENT( OUT ) :: dbdt
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_DBDT'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Output array has incorrect length
    n = SIZE( x )
    IF ( SIZE( dbdt ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of DBDT output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, n

      error_status = scalar_planck_dbdt( units,                    &  ! Input
                                         x(i),                     &  ! Input
                                         temperature,              &  ! Input
                                         dbdt(i),                  &  ! Output
                                         message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION R1_x_S_t_planck_dbdt



  FUNCTION S_x_R1_t_planck_dbdt( units,        &   ! Input
                                 x,            &   ! Input
                                 temperature,  &   ! Input
                                 dbdt,         &   ! Output
                                 message_log ) &   ! Optional input
                               RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                         INTENT( IN )  :: units
    REAL( Double ),                  INTENT( IN )  :: x
    REAL( Double ),  DIMENSION( : ), INTENT( IN )  :: temperature

    REAL( Double ),  DIMENSION( : ), INTENT( OUT ) :: dbdt
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_DBDT'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Output array has incorrect length
    k = SIZE( temperature )
    IF ( SIZE( dbdt ) /= k ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of TEMPERATURE input and DBDT output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF




    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, k

      error_status = scalar_planck_dbdt( units,                    &  ! Input
                                         x,                        &  ! Input
                                         temperature(i),           &  ! Input
                                         dbdt(i),                  &  ! Output
                                         message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION S_x_R1_t_planck_dbdt



  FUNCTION R1_xt_planck_dbdt( units,        &   ! Input
                              x,            &   ! Input
                              temperature,  &   ! Input
                              dbdt,         &   ! Output
                              message_log ) &   ! Optional input
                            RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                         INTENT( IN )  :: units
    REAL( Double ),  DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ),  DIMENSION( : ), INTENT( IN )  :: temperature

    REAL( Double ),  DIMENSION( : ), INTENT( OUT ) :: dbdt
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_DBDT'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Arrays have same lengths
    n = SIZE( x )
    IF ( SIZE( temperature ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of X and TEMPERATURE inputs', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( dbdt ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of DBDT output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, n

      error_status = scalar_planck_dbdt( units,                    &  ! Input
                                         x(i),                     &  ! Input
                                         temperature(i),           &  ! Input
                                         dbdt(i),                  &  ! Output
                                         message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION R1_xt_planck_dbdt



  FUNCTION R1_x_R2_t_planck_dbdt ( units,        &   ! Input
                                   x,            &   ! Input
                                   temperature,  &   ! Input
                                   dbdt,         &   ! Output
                                   message_log ) &   ! Optional input
                                 RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                           INTENT( IN )  :: units
    REAL( Double ),  DIMENSION( : ),    INTENT( IN )  :: x
    REAL( Double ),  DIMENSION( :, : ), INTENT( IN )  :: temperature

    REAL( Double ),  DIMENSION( :, : ), INTENT( OUT ) :: dbdt
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_DBDT'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, n, k



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Arrays have same lengths
    n = SIZE( temperature, DIM = 1 )
    k = SIZE( temperature, DIM = 2 )

    IF ( SIZE( x ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of X[Nx1] and TEMPERATURE[NxK] inputs.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( dbdt, DIM = 1 ) /= n .AND. &
         SIZE( dbdt, DIM = 2 ) /= k ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of input TEMPERATURE and output DBDT.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    spectrum_loop: DO j = 1, k

      frequency_loop: DO i = 1, n

        error_status = scalar_planck_dbdt( units,                    &  ! Input
                                           x(i),                     &  ! Input
                                           temperature(i,j),         &  ! Input
                                           dbdt(i,j),                &  ! Output
                                           message_log = message_log )  ! Optional input
        IF ( error_status /= SUCCESS ) EXIT spectrum_loop

      END DO frequency_loop

    END DO spectrum_loop

    RETURN

  END FUNCTION R1_x_R2_t_planck_dbdt



!------------------------------------------------------------------------------
!S+
! NAME:
!       planck_dtdb
!
! PURPOSE:
!       Function to calculate the Planck temperature derivative with respect
!       to radiance given the wavenumber or wavelength, and radiance.
!       Calculation is done in double precision.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       error_status = planck_dtdb( unit_key,                 & ! Input
!                                   x,                        & ! Input
!                                   radiance,                 & ! Input
!                                   dtdb,                     & ! Output
!                                   message_log = message_log ) ! Optional input
!
! INPUT ARGUMENTS:
!       unit_key:     Flag specifying spectral units. Valid values are:
!                       WAVENUMBER_UNIT: x in cm-1, radiance in mW/(m2.sr.cm-1)
!                       WAVELENGTH_UNIT: x in micron, radiance in W/(m2.sr.micron)
!                     UNITS:      None.
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       x:            Spectral ordinate. Can be a SCALAR or VECTOR
!                     UNITS:      Inverse centimetres (cm^-1) or microns (um).
!                     TYPE:       Double precision float
!                     DIMENSION:  Scalar or Nx1 vector
!                     ATTRIBUTES: INTENT( IN )
!
!       radiance:     Radiance(s) for which the Planck temperature
!                     derivative(s) is(are) required. Can be a SCALAR,
!                     VECTOR, or 2-D ARRAY. See dtdb output for allowed
!                     dimensionality.
!                     UNITS:      mW/(m2.sr.cm-1) OR W/(m2.sr.micron)
!                     TYPE:       Double precision float
!                     DIMENSION:  See output temperature dimensionality chart
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       dtdb:         The derivative(s) of Planck temperature with respect to
!                     radiance. The output dimensions are determined by the input.
!                     In the following chart N == frequencies, K == radiances or
!                     spectra (i.e. separate radiances or radiance spectra):
!
!                         Input X     Input RADIANCE        Output DTDB
!                        dimension       dimension           dimension
!                     ------------------------------------------------------
!                         scalar          scalar               scalar
!                           N             scalar                 N
!                         scalar            K                    K
!                           N               N                    N
!                           N               K               not allowed
!                           N             N x K                N x K
!
!                     UNITS:      (K.m2.sr.cm-1)/mW OR (K.m2.sr.um)/W
!                     TYPE:       Double precision float
!                     DIMENSION:  See chart above.
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the Planck derivative calculation was successful
!                 = FAILURE an error was found with the input.
!
! CALLS:
!      display_message:    Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       For wavenumber input, the Planck temperature differential with respect
!       to radiance is calculated using:
!
!                               c1 * c2 * wavenumber^4
!  dT/dB = --------------------------------------------------------------------
!           { c1 * wavenumber^3     }   {       ( c1 * wavenumber^3      ) }^2
!           { ----------------- + 1 } * { B * LN( ----------------- +  1 ) }
!           {         B             }   {       (          B             ) }
!
!
!       For wavelength input:
!
!                                           c1 * c2
!  dT/dB = --------------------------------------------------------------------------------
!                          {        c1            }   {       (        c1            ) }^2
!           wavelength^6 * { ---------------- + 1 } * { B * LN( ---------------- + 1 ) }
!                          { wavelength^5 * B     }   {       ( wavelength^5 * B     ) }
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck radiances such that radiances
!       are in the units of mW/(m2.sr.cm-1) for wavenumber input or
!       W/(m2.sr.micron) for wavelength input.
!
!       WAVENUMBER
!       ----------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!       All calculations are done in double precision.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Nov-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------


  FUNCTION scalar_planck_dtdb( units,        &  ! Input
                               x,            &  ! Input
                               radiance,     &  ! Input
                               dtdb,         &  ! Output
                               message_log ) &  ! Optional input
                               RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,         INTENT( IN )  :: units
    REAL( Double ),  INTENT( IN )  :: x
    REAL( Double ),  INTENT( IN )  :: radiance

    REAL( Double ),  INTENT( OUT ) :: dtdb
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_DTDB'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( Double ) :: frequency, wavelength
    REAL( Double ) :: x_c_1, x_c_2
    REAL( Double ) :: scaled_radiance
    REAL( Double ) :: argument



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Units input
    IF ( ( units /= WAVENUMBER_UNIT ) .AND. &
         ( units /= WAVELENGTH_UNIT )       ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid UNITS argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Frequency/wavelength input
    IF ( x < TOLERANCE ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid X (frequency/wavelength) argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
 
    ! -- Radiance input
    IF ( radiance < TOLERANCE ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid RADIANCE argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


 
    !-------------------------------------------------------------------------------
    !                       -- Calculate spectral parameters --
    !-------------------------------------------------------------------------------

    IF ( units == WAVENUMBER_UNIT ) THEN


      ! -----------------------
      ! Spectral units in cm^-1
      ! -----------------------

      frequency = x

      x_c_1 = C_1_SCALE_FACTOR( units ) * C_1 * ( frequency**3 )
      x_c_2 = C_2_SCALE_FACTOR( units ) * C_2 * frequency

    ELSE


      ! -------------------------
      ! Spectral units in microns
      ! -------------------------

      wavelength = x

      x_c_1 = C_1_SCALE_FACTOR( units ) * C_1 / ( wavelength**5 )
      x_c_2 = C_2_SCALE_FACTOR( units ) * C_2 / wavelength

    END IF



    !-----------------------------------------------------------------------
    !                     -- Calculate dT/dB --
    !-----------------------------------------------------------------------

    ! -- Radiance in terms of W
    scaled_radiance = radiance / RADIANCE_SCALE_FACTOR( units )

    ! -- Common term in dT/dB formulation
    argument = ( x_c_1 / scaled_radiance ) + 1.0d0

    ! -- Calculate dT/dB in (K.....)/W
    dtdb =                    x_c_1 * x_c_2 / &
    !      ---------------------------------------------------------
            ( argument * ( scaled_radiance * LOG( argument ) )**2 )

    ! -- Convert dT/dB units to (K...cm-1)/mW if required.
    dtdb = dtdb / RADIANCE_SCALE_FACTOR( units )

    RETURN

  END FUNCTION scalar_planck_dtdb



  FUNCTION R1_x_S_r_planck_dtdb( units,        &  ! Input
                                 x,            &  ! Input
                                 radiance,     &  ! Input
                                 dtdb,         &  ! Output
                                 message_log ) &  ! Optional input
                               RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                        INTENT( IN )  :: units
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ),                 INTENT( IN )  :: radiance

    REAL( Double ), DIMENSION( : ), INTENT( OUT ) :: dtdb
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_DTDB'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Output array has incorrect length
    n = SIZE( x )
    IF ( SIZE( dtdb ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of DTDB output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, n

      error_status = scalar_planck_dtdb( units,                    &  ! Input
                                         x(i),                     &  ! Input
                                         radiance,                 &  ! Input
                                         dtdb(i),                  &  ! Output
                                         message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION R1_x_S_r_planck_dtdb



  FUNCTION S_x_R1_r_planck_dtdb( units,        &  ! Input
                                 x,            &  ! Input
                                 radiance,     &  ! Input
                                 dtdb,         &  ! Output
                                 message_log ) &  ! Optional input
                               RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                        INTENT( IN )  :: units
    REAL( Double ),                 INTENT( IN )  :: x
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: radiance

    REAL( Double ), DIMENSION( : ), INTENT( OUT ) :: dtdb
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_DTDB'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Output array has incorrect length
    n = SIZE( radiance )
    IF ( SIZE( dtdb ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of DTDB output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, n

      error_status = scalar_planck_dtdb( units,                    &  ! Input
                                         x,                        &  ! Input
                                         radiance(i),              &  ! Input
                                         dtdb(i),                  &  ! Output
                                         message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION S_x_R1_r_planck_dtdb



  FUNCTION R1_xr_planck_dtdb( units,        &  ! Input
                               x,            &  ! Input
                               radiance,     &  ! Input
                               dtdb,         &  ! Output
                               message_log ) &  ! Optional input
                               RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                        INTENT( IN )  :: units
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: radiance

    REAL( Double ), DIMENSION( : ), INTENT( OUT ) :: dtdb
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_DTDB'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Arrays have same lengths
    n = SIZE( x )
    IF ( SIZE( radiance ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of X and RADIANCE inputs', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( dtdb ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of DTDB output', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    DO i = 1, n

      error_status = scalar_planck_dtdb( units,                    &  ! Input
                                         x(i),                     &  ! Input
                                         radiance(i),              &  ! Input
                                         dtdb(i),                  &  ! Output
                                         message_log = message_log )  ! Optional input
      IF ( error_status /= SUCCESS ) EXIT

    END DO

    RETURN

  END FUNCTION R1_xr_planck_dtdb



  FUNCTION R1_x_R2_r_planck_dtdb( units,        &  ! Input
                                  x,            &  ! Input
                                  radiance,     &  ! Input
                                  dtdb,         &  ! output
                                  message_log ) &  ! Optional input
                                RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                           INTENT( IN )  :: units
    REAL( Double ), DIMENSION( : ),    INTENT( IN )  :: x
    REAL( Double ), DIMENSION( :, : ), INTENT( IN )  :: radiance

    REAL( Double ), DIMENSION( :, : ), INTENT( OUT ) :: dtdb
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PLANCK_DTDB'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, n, k



    !-------------------------------------------------------------------------------
    !                            -- Check array input --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

    ! -- Arrays have same lengths
    n = SIZE( radiance, DIM = 1 )
    k = SIZE( radiance, DIM = 2 )

    IF ( SIZE( x ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of X[Nx1] and RADIANCE[NxK] inputs.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( dtdb, DIM = 1 ) /= n .AND. &
         SIZE( dtdb, DIM = 2 ) /= k ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent size of input RADIANCE and output DTDB.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                        -- Loop over spectral points --
    !
    ! Believe it or not, doing it this way rather than using F90's ability to
    ! perform arithmetic on arrays is *faster*! At least on the systems this code
    ! was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI Origin).
    !
    ! How 'bout that?
    !-------------------------------------------------------------------------------

    spectrum_loop: DO j = 1, k

      frequency_loop: DO i = 1, n

        error_status = scalar_planck_dtdb( units,                    &  ! Input
                                           x(i),                     &  ! Input
                                           radiance(i,j),            &  ! Input
                                           dtdb(i,j),                &  ! Output
                                           message_log = message_log )  ! Optional input
        IF ( error_status /= SUCCESS ) EXIT spectrum_loop

      END DO frequency_loop

    END DO spectrum_loop

    RETURN

  END FUNCTION R1_x_R2_r_planck_dtdb

END MODULE planck_functions



!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: planck_functions.f90,v 1.1 2003/06/07 16:39:35 phil Exp $
!
! $Date: 2003/06/07 16:39:35 $
!
! $Revision: 1.1 $
!
! $State: Exp $
!
! $Log: planck_functions.f90,v $
! Revision 1.1  2003/06/07 16:39:35  phil
! new diagnostics
!
! Revision 2.8  2001/05/09 17:30:36  paulv
! - Added PUBLIC parameter definitions for units.
! - Replaced all references to explicit number units key to the defined
!   parameter, WAVENUMBER_UNIT or WAVELENGTH_UNIT.
!
! Revision 2.7  2001/05/08 21:37:33  paulv
! - Changed all references to derived constants D_C_1 and D_C_2 to C_1
!   and C_2 to reflect changes in the FUNDAMENTAL_CONSTANTS module.
!
! Revision 2.6  2000/12/13 16:08:19  paulv
! - More doc. corrections! Dammit.
!
! Revision 2.5  2000/12/13 16:04:48  paulv
! - Updated header documentation.
!
! Revision 2.3  2000/12/11 20:43:00  paulv
! - Updated and corrected header documentation.
!
! Revision 2.2  2000/12/11 20:32:34  paulv
! - Modified to reflect changes in FUNDAMENTAL_CONSTANTS module.
! - All explicit typing of integers as ( Long ) removed.
! - Cosmetic changes.
!
! Revision 2.1  2000/12/11 18:35:33  paulv
! - Added multiple dimension input options.
!
! Revision 1.8  2000/12/01 22:40:27  paulv
! - Adding more specific functions to allow more freedom in specifying
!   input argument dimensions.
! - Adding header for each generic function.
! - INCOMPLETE....IN PROGRESS
!
! Revision 1.7  2000/11/16 21:11:33  paulv
! - Added planck_dtdb() functions.
! - Updated header documentation.
! - Converted all declared parameter names to uppercase.
!
! Revision 1.6  2000/11/03 20:09:05  paulv
! - Added ONLY clause to the USE statement for the fundamental_constants
!   module. Only the derived constants c_1 and c_2 are required for the
!   Planck function calculations.
!
! Revision 1.5  2000/05/03 18:32:18  paulv
! - Fundamental constants are now supplied via a module rather than defined
!   in line. This is true for the derived radiation constants c_1 and c_2
! - Return error codes are defined as parameters:
!     SUCCESS =  1
!     FAILURE = -1
! - Extraneous array definitions removed.
! - Updated header documentation.
!
! Revision 1.4  1999/10/18 18:54:22  paulv
! Corrected error in header documentation
!
! Revision 1.3  1999/10/18 17:10:21  paulv
! Added RCS Id keyword
!
! Revision 1.2  1999/10/18 15:23:26  paulv
! - Replaced rank-1 functions with calls-in-a-loop to scalar functions.
!   This eliminated the replicated code and actually turned out to be
!   faster than using vector arithmetic.
! - Updated header documentation.
!
