PROGRAM sensor_planck_functions_test

  ! $Id: sensor_planck_functions_test.f90,v 1.1 2003/06/07 16:39:35 phil Exp $

  USE type_kinds
  USE error_handler
  USE sensor_planck_functions

  IMPLICIT NONE

  INTEGER, PARAMETER :: MAX_N_SENSORS = 49

  CHARACTER( 14 ), PARAMETER, DIMENSION( MAX_N_SENSORS ) :: &
    sensor_description = (/ 'GOES-8        ', 'GOES-9        ', 'GOES-10       ', &
                            'NOAA-5 HIRS/2 ', 'NOAA-6 HIRS/2 ', 'NOAA-7 HIRS/2 ', &
                            'NOAA-8 HIRS/2 ', 'NOAA-9 HIRS/2 ', 'NOAA-10 HIRS/2', &
                            'NOAA-11 HIRS/2', 'NOAA-12 HIRS/2', 'NOAA-14 HIRS/2', &
                            'NOAA-15 HIRS/3', 'MODIS PFM     ', 'MODIS Spec.   ', &
                            'NOAA-5 AVHRR  ', 'NOAA-6 AVHRR  ', 'NOAA-7 AVHRR  ', &
                            'NOAA-8 AVHRR  ', 'NOAA-9 AVHRR  ', 'NOAA-10 AVHRR ', &
                            'NOAA-11 AVHRR ', 'NOAA-12 AVHRR ', 'NOAA-14 AVHRR ', &
                            'NOAA-15 AVHRR ', 'GOES-4 VAS    ', 'GOES-5 VAS    ', &
                            'GOES-6 VAS    ', 'GOES-7 VAS    ', 'MeteoSat 3    ', &
                            'MeteoSat 4    ', 'MeteoSat 5    ', 'MeteoSat 6    ', &
                            'MeteoSat 7    ', 'GMS-5 Det.A   ', 'GMS-5 Det.B   ', &
                            'MAS Jan95     ', 'MAS Jun96     ', 'MAS Feb97     ', &
                            'Fengyun2      ', 'GOES-11       ', 'GOES-12       ', &
                            'NOAA-16 HIRS/3', 'MODIS AM1     ', 'MeteoSat SG1  ', &
                            'GMS-5 Det.A2  ', 'MAS Mar99     ', 'MAS Jan00     ', &
                            'MAS Oct00     '/)

  INTEGER, PARAMETER :: MAX_N_CHANNELS = 50

  INTEGER, DIMENSION( MAX_N_CHANNELS ) :: channel_number

  INTEGER :: n, l, n_channels, units, r_units
  INTEGER :: error_status

  REAL( Double ) :: temperature, t, radiance, r, frequency, dbdt, dtdb


  ! -- Unit strings for output
  INTEGER, PARAMETER :: n_units = 2
  CHARACTER( 10 ), DIMENSION( n_units ) :: V_unit_string  = (/ 'wavenumber', &
                                                               'wavelength' /)
  CHARACTER( 15 ), DIMENSION( n_units ) :: R_unit_string  = (/ 'mW/(m2.sr.cm-1)', &
                                                               'W/(m2.sr.um)   ' /)
  CHARACTER( 17 ), DIMENSION( n_units ) :: DBDT_unit_string = (/ 'mW/(m2.sr.cm-1.K)', &
                                                                 'W/(m2.sr.um.K)   ' /)
  CHARACTER( 17 ), DIMENSION( n_units ) :: DTDB_unit_string = (/ '(K.m2.sr.cm-1)/mW', &
                                                                 '(K.m2.sr.um)/W   ' /)

  ! -- Output format strings
  CHARACTER( 6 )   :: dtdb_fmt
  CHARACTER( 200 ) :: output_fmt


  ! -------------------------
  ! Some up-front definitions
  ! -------------------------

  ! -- Set default units: W/(m2.sr.um)
  units   = WAVELENGTH_UNIT

  ! -- Set conversion units (for accessing R_unit_string() only)
  r_units = WAVENUMBER_UNIT

  ! -- Define default temperature
  temperature = 300.0_Double


  ! -----------------
  ! Loop over sensors
  ! -----------------

  sensor_loop: DO n = 1, MAX_N_SENSORS

    WRITE( *, '( 5x, "Sensor : ", a )' ) sensor_description( n )


    ! ---------------------------------
    ! Get the available channel numbers
    ! ---------------------------------

    error_status = sensor_channels( n, n_channels, channel_number )
    IF ( error_status == FAILURE ) STOP


    ! ------------------
    ! Loop over channels
    ! ------------------

    channel_loop: DO l = 1, n_channels


      ! -------------------------------------------------
      ! Calculate radiances, temperatures, derivates etc.
      ! -------------------------------------------------

      error_status = sensor_radiance( units, &
                                      n, &
                                      channel_number( l ), &
                                      temperature, &
                                      radiance )
      IF ( error_status == FAILURE ) STOP

      error_status = sensor_convert_radiance( units, &
                                              n, &
                                             channel_number( l ), &
                                             radiance, &
                                             r )
      IF ( error_status == FAILURE ) STOP

      error_status = sensor_temperature( units, &
                                         n, &
                                         channel_number( l ), &
                                         radiance, &
                                         t )
      IF ( error_status == FAILURE ) STOP

      error_status = sensor_dbdt( units, &
                                  n, &
                                  channel_number( l ), &
                                  temperature, &
                                  dbdt )
      IF ( error_status == FAILURE ) STOP

      error_status = sensor_dtdb( units, &
                                  n, &
                                  channel_number( l ), &
                                  radiance, &
                                  dtdb )
      IF ( error_status == FAILURE ) STOP


      ! --------------
      ! Output results
      ! --------------

      ! -- Futz around with the output formats
      IF ( ABS( dtdb ) > 999.0_Double ) THEN
        dtdb_fmt = 'es10.3'
      ELSE
        dtdb_fmt = 'f10.6 '
      END IF

      output_fmt = '( 2x, "Channel ", i2, '// &
                         '" R = ", f10.6, a, '// &
                         '" R = ", f10.6, a, '// &
                         '" T = ", f10.6, "K   ", '// &
                         '" dB/dT = ", f10.6, a, '// &
                         '" dT/dB = ", '//dtdb_fmt//', a )'

      ! -- Write results to screen
      WRITE( *, FMT = TRIM( output_fmt ) ) channel_number( l ), &
                                           radiance, R_unit_string( units   ), &
                                           r,        R_unit_string( r_units ), &
                                           t, &
                                           dbdt, DBDT_unit_string( units ), &
                                           dtdb, DTDB_unit_string( units )

    END DO channel_loop

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )
              
  END DO sensor_loop

END PROGRAM sensor_planck_functions_test
