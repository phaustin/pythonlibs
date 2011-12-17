!------------------------------------------------------------------------------
!M+
! NAME:
!       sensor_planck_functions
!
! PURPOSE:
!       Module containing Planck function radiance and temperature routines
!       for various broadband sensors for scalar and rank-1 array inputs
!
!       All calculations are done in double precision.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE sensor_planck_functions
!
! MODULES:
!       type_kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       error_handler:              Module to define simple error codes and
!                                   handle error conditions
!
!       planck_functions:           Module containing monochromatic Planck
!                                   function radiance, temperature, dB/dT, and
!                                   dT/dB routines.
!
! CONTAINS:
!       sensor_channels:            Function to return the number and list of
!                                   channels given a sensor ID.
!
!       sensor_convert_radiance:    Function to convert radiances from units of
!                                   mW/(m2.sr.cm-1) to W/(m2.sr.micron) or vice
!                                   versa.
!
!       sensor_planck_radiance:     Function to calculate the Planck radiance
!                                   given a sensor ID and channel, and
!                                   temperature.
!
!       sensor_planck_temperature:  Function to calculate the Planck temperature
!                                   given a sensor ID and channel, and radiance.
!
!       sensor_planck_dbdt:         Function to calculate the derivative of
!                                   the Planck radiance with respect to temperature
!                                   given a sensor ID and channel, and temperature.
!
!       sensor_planck_dtdb:         Function to calculate the Planck temperature
!                                   derivative with respect to radiance given a
!                                   sensor ID and channel, and radiance.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-May-2001
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


MODULE sensor_planck_functions


  ! ------------
  ! Modules used
  ! ------------

  ! -- Data type kind definitions. I rename the double precision
  ! -- defintion "Double" to "DP" so the data statement definition
  ! -- lines are not too long.
  USE type_kinds, DP => Double

  ! -- Error code definitions and code
  USE error_handler

  ! -- Monochromatic Planck functions
  USE planck_functions


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: sensor_channels
  PUBLIC :: sensor_convert_radiance
  PUBLIC :: sensor_radiance
  PUBLIC :: sensor_temperature
  PUBLIC :: sensor_dbdt
  PUBLIC :: sensor_dtdb

  ! -- From planck_functions module
  PUBLIC :: WAVENUMBER_UNIT
  PUBLIC :: WAVELENGTH_UNIT


  ! -------------------------
  ! Local parameters and data
  ! -------------------------

  ! -- Precision related values
  REAL( DP ), PARAMETER :: ZERO = 0.0_DP
  REAL( DP ), PARAMETER :: ONE  = 1.0_DP
  REAL( DP ), PARAMETER :: TOLERANCE = EPSILON( ONE )

  ! -- Parameters
  INTEGER, PARAMETER :: MAX_N_CHANNELS = 808
  INTEGER, PARAMETER :: MAX_N_SENSORS  = 49

  ! -- Local variables for DATA initialisations
  INTEGER :: l, n

  ! -- Data variables
  CHARACTER( 14 ), DIMENSION( MAX_N_SENSORS ) :: &
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

  INTEGER,    DIMENSION( 0:MAX_N_SENSORS  ) :: n_sensor_channels

  INTEGER,    DIMENSION( MAX_N_CHANNELS ) :: channel
  REAL( DP ), DIMENSION( MAX_N_CHANNELS ) :: central_frequency, &
                                             temperature_intercept, &
                                             temperature_slope


  ! -------------------------------
  ! No. of channels for each sensor
  ! -------------------------------

  DATA ( n_sensor_channels( n ), n = 0, MAX_N_SENSORS )/ &
     0,24,24,24,20,20,20,20,20,20,20,20,20,20,17,17, 3, 3, 3, &
     3, 3, 3, 3, 3, 3, 3,12,12,12,12, 4, 4, 4, 4, 4, 3, 3,50, &
    50,50, 2,24,24,20,17, 8, 3,50,50,50/


  ! ---------------------------
  ! Fill the sensor data arrays
  ! ---------------------------

  ! -- GOES-8        
  DATA ( channel( l ), l = 1, 24 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    21,22,23,24,25/
  DATA ( central_frequency( l ), l = 1, 24 )/ &
      679.8404_DP,  694.1847_DP,  710.2506_DP,  731.6406_DP,  746.6873_DP,  789.0519_DP, &
      828.0692_DP,  905.9832_DP, 1028.2504_DP, 1339.1760_DP, 1422.5356_DP, 1534.5789_DP, &
     2182.9321_DP, 2205.1672_DP, 2242.7339_DP, 2419.0442_DP, 2506.7144_DP, 2664.4590_DP, &
    15384.6104_DP,15384.6104_DP, 2560.6792_DP, 1482.3042_DP,  935.0697_DP,  835.4203_DP/
  DATA ( temperature_intercept( l ), l = 1, 24 )/ &
     0.01210_DP, 0.01195_DP, 0.01504_DP, 0.01253_DP, 0.01548_DP, 0.04750_DP, 0.13364_DP, &
     0.11477_DP, 0.03905_DP, 0.14488_DP, 0.28229_DP, 0.14245_DP, 0.02175_DP, 0.02152_DP, &
     0.02097_DP, 0.05667_DP, 0.06168_DP, 0.36222_DP, 0.00000_DP, 0.00000_DP, 0.63568_DP, &
     0.60603_DP, 0.37351_DP, 0.22171_DP/
  DATA ( temperature_slope( l ), l = 1, 24 )/ &
     0.99994_DP, 0.99995_DP, 0.99993_DP, 0.99995_DP, 0.99993_DP, 0.99981_DP, 0.99949_DP, &
     0.99960_DP, 0.99988_DP, 0.99963_DP, 0.99932_DP, 0.99967_DP, 0.99996_DP, 0.99996_DP, &
     0.99997_DP, 0.99991_DP, 0.99991_DP, 0.99949_DP, 1.00000_DP, 1.00000_DP, 0.99911_DP, &
     0.99858_DP, 0.99873_DP, 0.99916_DP/

  ! -- GOES-9        
  DATA ( channel( l ), l = 25, 48 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    21,22,23,24,25/
  DATA ( central_frequency( l ), l = 25, 48 )/ &
      681.2105_DP,  693.9980_DP,  711.0496_DP,  732.3804_DP,  746.8923_DP,  789.4624_DP, &
      828.3709_DP,  909.4391_DP, 1028.7023_DP, 1335.9365_DP, 1421.3561_DP, 1528.4003_DP, &
     2183.8833_DP, 2206.8425_DP, 2245.4590_DP, 2415.2649_DP, 2512.0220_DP, 2666.6499_DP, &
    15384.6104_DP,15384.6104_DP, 2555.6399_DP, 1481.5399_DP,  934.4062_DP,  833.6053_DP/
  DATA ( temperature_intercept( l ), l = 25, 48 )/ &
     0.00987_DP, 0.01186_DP, 0.01218_DP, 0.01190_DP, 0.01351_DP, 0.04399_DP, 0.13449_DP, &
     0.11929_DP, 0.04072_DP, 0.14380_DP, 0.27616_DP, 0.13702_DP, 0.01889_DP, 0.01983_DP, &
     0.01908_DP, 0.05305_DP, 0.06117_DP, 0.30417_DP, 0.00000_DP, 0.00000_DP, 0.58637_DP, &
     0.48409_DP, 0.36225_DP, 0.20144_DP/
  DATA ( temperature_slope( l ), l = 25, 48 )/ &
     0.99995_DP, 0.99995_DP, 0.99995_DP, 0.99995_DP, 0.99994_DP, 0.99982_DP, 0.99949_DP, &
     0.99958_DP, 0.99987_DP, 0.99964_DP, 0.99933_DP, 0.99969_DP, 0.99997_DP, 0.99997_DP, &
     0.99997_DP, 0.99992_DP, 0.99991_DP, 0.99958_DP, 1.00000_DP, 1.00000_DP, 0.99917_DP, &
     0.99887_DP, 0.99876_DP, 0.99923_DP/

  ! -- GOES-10       
  DATA ( channel( l ), l = 49, 72 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    21,22,23,24,25/
  DATA ( central_frequency( l ), l = 49, 72 )/ &
      679.1133_DP,  695.9221_DP,  713.4692_DP,  732.0781_DP,  748.2764_DP,  790.6541_DP, &
      831.4687_DP,  909.6537_DP, 1030.0992_DP, 1342.2073_DP, 1425.7466_DP, 1536.5338_DP, &
     2189.1223_DP, 2213.0227_DP, 2249.4861_DP, 2421.0337_DP, 2513.8530_DP, 2665.3032_DP, &
    15384.6104_DP,15384.6104_DP, 2553.9211_DP, 1486.0392_DP,  936.2370_DP,  830.7422_DP/
  DATA ( temperature_intercept( l ), l = 49, 72 )/ &
     0.00988_DP, 0.01195_DP, 0.01236_DP, 0.01248_DP, 0.01366_DP, 0.04329_DP, 0.13886_DP, &
     0.11732_DP, 0.03969_DP, 0.14968_DP, 0.27547_DP, 0.12940_DP, 0.02001_DP, 0.01842_DP, &
     0.02019_DP, 0.05304_DP, 0.05330_DP, 0.28741_DP, 0.00000_DP, 0.00000_DP, 0.62223_DP, &
     0.61437_DP, 0.27790_DP, 0.21145_DP/
  DATA ( temperature_slope( l ), l = 49, 72 )/ &
     0.99995_DP, 0.99995_DP, 0.99994_DP, 0.99995_DP, 0.99994_DP, 0.99983_DP, 0.99947_DP, &
     0.99959_DP, 0.99988_DP, 0.99962_DP, 0.99934_DP, 0.99971_DP, 0.99997_DP, 0.99997_DP, &
     0.99997_DP, 0.99992_DP, 0.99992_DP, 0.99961_DP, 1.00000_DP, 1.00000_DP, 0.99912_DP, &
     0.99857_DP, 0.99905_DP, 0.99919_DP/

  ! -- NOAA-5 HIRS/2 
  DATA ( channel( l ), l = 73, 92 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 73, 92 )/ &
      668.4830_DP,  679.0150_DP,  690.6880_DP,  703.6100_DP,  716.1060_DP,  732.0810_DP, &
      748.9010_DP,  899.7120_DP, 1027.4260_DP, 1220.7889_DP, 1363.2030_DP, 1482.8101_DP, &
     2191.6370_DP, 2212.0239_DP, 2238.5081_DP, 2273.3081_DP, 2359.3760_DP, 2510.4980_DP, &
     2659.8611_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 73, 92 )/ &
     0.00921_DP, 0.01445_DP, 0.02308_DP, 0.02520_DP, 0.02189_DP, 0.02550_DP, 0.02730_DP, &
     0.07988_DP, 0.06409_DP, 0.19063_DP, 0.09764_DP, 0.33010_DP, 0.02056_DP, 0.02076_DP, &
     0.02525_DP, 0.05835_DP, 0.02410_DP, 0.06819_DP, 0.36111_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 73, 92 )/ &
     0.99996_DP, 0.99993_DP, 0.99989_DP, 0.99989_DP, 0.99990_DP, 0.99989_DP, 0.99988_DP, &
     0.99972_DP, 0.99980_DP, 0.99949_DP, 0.99976_DP, 0.99925_DP, 0.99997_DP, 0.99997_DP, &
     0.99996_DP, 0.99991_DP, 0.99996_DP, 0.99990_DP, 0.99950_DP, 1.00000_DP/

  ! -- NOAA-6 HIRS/2 
  DATA ( channel( l ), l = 93, 112 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 93, 112 )/ &
      667.8870_DP,  679.6090_DP,  690.0510_DP,  704.6300_DP,  718.0030_DP,  732.2820_DP, &
      749.1560_DP,  900.0360_DP, 1028.7400_DP, 1222.6370_DP, 1368.6240_DP, 1481.5630_DP, &
     2190.4561_DP, 2210.7251_DP, 2237.7180_DP, 2269.7151_DP, 2360.7910_DP, 2515.1780_DP, &
     2650.2100_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 93, 112 )/ &
     0.00667_DP, 0.01803_DP, 0.02346_DP, 0.02578_DP, 0.02588_DP, 0.02437_DP, 0.02663_DP, &
     0.06377_DP, 0.05756_DP, 0.18981_DP, 0.09298_DP, 0.34443_DP, 0.01744_DP, 0.01915_DP, &
     0.02229_DP, 0.01847_DP, 0.02028_DP, 0.05713_DP, 0.35949_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 93, 112 )/ &
     0.99997_DP, 0.99992_DP, 0.99989_DP, 0.99988_DP, 0.99988_DP, 0.99989_DP, 0.99989_DP, &
     0.99978_DP, 0.99982_DP, 0.99949_DP, 0.99977_DP, 0.99921_DP, 0.99997_DP, 0.99997_DP, &
     0.99996_DP, 0.99997_DP, 0.99997_DP, 0.99992_DP, 0.99950_DP, 1.00000_DP/

  ! -- NOAA-7 HIRS/2 
  DATA ( channel( l ), l = 113, 132 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 113, 132 )/ &
      667.3860_DP,  679.2480_DP,  691.2470_DP,  704.7280_DP,  717.8730_DP,  733.7960_DP, &
      749.9950_DP,  899.0130_DP, 1026.0210_DP, 1221.4189_DP, 1361.7560_DP, 1481.7910_DP, &
     2182.3450_DP, 2208.3569_DP, 2238.4070_DP, 2260.1631_DP, 2359.8821_DP, 2513.5049_DP, &
     2667.6631_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 113, 132 )/ &
     0.00822_DP, 0.01698_DP, 0.02183_DP, 0.02330_DP, 0.02106_DP, 0.02599_DP, 0.02819_DP, &
     0.06444_DP, 0.06152_DP, 0.18350_DP, 0.08879_DP, 0.34610_DP, 0.02546_DP, 0.02323_DP, &
     0.02016_DP, 0.02009_DP, 0.01955_DP, 0.05242_DP, 0.35581_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 113, 132 )/ &
     0.99996_DP, 0.99992_DP, 0.99990_DP, 0.99989_DP, 0.99991_DP, 0.99989_DP, 0.99988_DP, &
     0.99977_DP, 0.99980_DP, 0.99950_DP, 0.99978_DP, 0.99920_DP, 0.99996_DP, 0.99996_DP, &
     0.99997_DP, 0.99997_DP, 0.99997_DP, 0.99992_DP, 0.99951_DP, 1.00000_DP/

  ! -- NOAA-8 HIRS/2 
  DATA ( channel( l ), l = 133, 152 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 133, 152 )/ &
      666.9260_DP,  678.8690_DP,  690.3400_DP,  716.1330_DP,  717.6550_DP,  732.1450_DP, &
      748.5940_DP,  898.8980_DP, 1026.3860_DP, 1220.9900_DP, 1366.0310_DP, 1483.2061_DP, &
     2188.2209_DP, 2210.3279_DP, 2238.2849_DP, 2271.1040_DP, 2357.2290_DP, 2514.7520_DP, &
     2656.1809_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 133, 152 )/ &
     0.01281_DP, 0.01424_DP, 0.02561_DP, 0.05023_DP, 0.02140_DP, 0.02560_DP, 0.02694_DP, &
     0.06432_DP, 0.06093_DP, 0.19128_DP, 0.09354_DP, 0.33509_DP, 0.01965_DP, 0.02505_DP, &
     0.01851_DP, 0.02174_DP, 0.02058_DP, 0.05951_DP, 0.34891_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 133, 152 )/ &
     0.99994_DP, 0.99993_DP, 0.99988_DP, 0.99978_DP, 0.99990_DP, 0.99989_DP, 0.99989_DP, &
     0.99977_DP, 0.99981_DP, 0.99948_DP, 0.99977_DP, 0.99923_DP, 0.99997_DP, 0.99996_DP, &
     0.99997_DP, 0.99997_DP, 0.99997_DP, 0.99991_DP, 0.99952_DP, 1.00000_DP/

  ! -- NOAA-9 HIRS/2 
  DATA ( channel( l ), l = 153, 172 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 153, 172 )/ &
      667.6180_DP,  679.8430_DP,  691.1780_DP,  703.5490_DP,  717.2690_DP,  732.9690_DP, &
      749.6940_DP,  898.2290_DP, 1030.4730_DP, 1220.1930_DP, 1366.0500_DP, 1483.4470_DP, &
     2190.7429_DP, 2209.7380_DP, 2242.9319_DP, 2274.6060_DP, 2359.9021_DP, 2517.2009_DP, &
     2663.5220_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 153, 172 )/ &
     0.00769_DP, 0.01477_DP, 0.02329_DP, 0.02410_DP, 0.02163_DP, 0.02725_DP, 0.02446_DP, &
     0.06497_DP, 0.06036_DP, 0.18418_DP, 0.08865_DP, 0.34660_DP, 0.01965_DP, 0.01971_DP, &
     0.01876_DP, 0.05709_DP, 0.02209_DP, 0.05658_DP, 0.33670_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 153, 172 )/ &
     0.99996_DP, 0.99993_DP, 0.99989_DP, 0.99989_DP, 0.99990_DP, 0.99988_DP, 0.99990_DP, &
     0.99977_DP, 0.99981_DP, 0.99950_DP, 0.99978_DP, 0.99921_DP, 0.99997_DP, 0.99997_DP, &
     0.99997_DP, 0.99991_DP, 0.99997_DP, 0.99992_DP, 0.99953_DP, 1.00000_DP/

  ! -- NOAA-10 HIRS/2
  DATA ( channel( l ), l = 173, 192 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 173, 192 )/ &
      667.6530_DP,  680.2580_DP,  691.3670_DP,  704.5120_DP,  716.7910_DP,  733.0010_DP, &
      750.5630_DP,  898.6430_DP, 1027.7910_DP, 1223.6560_DP, 1364.0990_DP, 1483.4041_DP, &
     2189.7000_DP, 2206.7380_DP, 2239.5691_DP, 2268.2371_DP, 2359.0850_DP, 2513.0381_DP, &
     2659.8950_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 173, 192 )/ &
     0.00712_DP, 0.01482_DP, 0.02448_DP, 0.02077_DP, 0.02049_DP, 0.02609_DP, 0.02931_DP, &
     0.06336_DP, 0.06047_DP, 0.19605_DP, 0.09103_DP, 0.33532_DP, 0.02206_DP, 0.02328_DP, &
     0.02289_DP, 0.02091_DP, 0.01818_DP, 0.05452_DP, 0.31858_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 173, 192 )/ &
     0.99997_DP, 0.99993_DP, 0.99989_DP, 0.99991_DP, 0.99991_DP, 0.99989_DP, 0.99988_DP, &
     0.99978_DP, 0.99981_DP, 0.99947_DP, 0.99978_DP, 0.99923_DP, 0.99996_DP, 0.99996_DP, &
     0.99996_DP, 0.99997_DP, 0.99997_DP, 0.99992_DP, 0.99956_DP, 1.00000_DP/

  ! -- NOAA-11 HIRS/2
  DATA ( channel( l ), l = 193, 212 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 193, 212 )/ &
      669.0510_DP,  678.9570_DP,  689.8320_DP,  703.4250_DP,  716.9370_DP,  732.2010_DP, &
      749.4970_DP,  900.5130_DP, 1031.2240_DP,  795.9020_DP, 1361.4301_DP, 1480.0200_DP, &
     2190.0901_DP, 2209.7729_DP, 2239.3740_DP, 2268.0000_DP, 2416.5891_DP, 2512.0420_DP, &
     2664.3760_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 193, 212 )/ &
     0.01008_DP, 0.01439_DP, 0.02264_DP, 0.02106_DP, 0.02222_DP, 0.02340_DP, 0.02235_DP, &
     0.06517_DP, 0.06581_DP, 0.02418_DP, 0.08864_DP, 0.28313_DP, 0.02387_DP, 0.01892_DP, &
     0.02076_DP, 0.01860_DP, 0.02847_DP, 0.04958_DP, 0.33222_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 193, 212 )/ &
     0.99995_DP, 0.99993_DP, 0.99990_DP, 0.99990_DP, 0.99990_DP, 0.99990_DP, 0.99991_DP, &
     0.99977_DP, 0.99979_DP, 0.99990_DP, 0.99978_DP, 0.99935_DP, 0.99996_DP, 0.99997_DP, &
     0.99997_DP, 0.99997_DP, 0.99996_DP, 0.99993_DP, 0.99954_DP, 1.00000_DP/

  ! -- NOAA-12 HIRS/2
  DATA ( channel( l ), l = 213, 232 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 213, 232 )/ &
      667.6460_DP,  680.2650_DP,  690.0980_DP,  704.2640_DP,  716.3890_DP,  732.3350_DP, &
      751.8370_DP,  898.7670_DP, 1025.6219_DP, 1221.3230_DP, 1367.7061_DP, 1480.6830_DP, &
     2188.4771_DP, 2210.6521_DP, 2238.5591_DP, 2267.7410_DP, 2361.6721_DP, 2514.7161_DP, &
     2653.6460_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 213, 232 )/ &
     0.00903_DP, 0.01547_DP, 0.02429_DP, 0.02309_DP, 0.02312_DP, 0.02966_DP, 0.02771_DP, &
     0.06374_DP, 0.06194_DP, 0.19111_DP, 0.10342_DP, 0.34888_DP, 0.02123_DP, 0.01808_DP, &
     0.02304_DP, 0.02278_DP, 0.02049_DP, 0.05880_DP, 0.34786_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 213, 232 )/ &
     0.99996_DP, 0.99993_DP, 0.99989_DP, 0.99990_DP, 0.99990_DP, 0.99987_DP, 0.99988_DP, &
     0.99977_DP, 0.99980_DP, 0.99948_DP, 0.99975_DP, 0.99920_DP, 0.99997_DP, 0.99997_DP, &
     0.99996_DP, 0.99996_DP, 0.99997_DP, 0.99992_DP, 0.99952_DP, 1.00000_DP/

  ! -- NOAA-14 HIRS/2
  DATA ( channel( l ), l = 233, 252 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 233, 252 )/ &
      668.9550_DP,  679.4720_DP,  689.6920_DP,  703.7250_DP,  714.7360_DP,  732.3260_DP, &
      749.7260_DP,  898.7080_DP, 1028.4720_DP,  796.1320_DP, 1361.3080_DP, 1481.0790_DP, &
     2191.3640_DP, 2207.3960_DP, 2236.3560_DP, 2268.2090_DP, 2420.4309_DP, 2512.3220_DP, &
     2648.3931_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 233, 252 )/ &
     0.00332_DP, 0.01393_DP, 0.01343_DP, 0.02091_DP, 0.02011_DP, 0.02217_DP, 0.02313_DP, &
     0.06247_DP, 0.05960_DP, 0.02590_DP, 0.08963_DP, 0.27515_DP, 0.02072_DP, 0.01904_DP, &
     0.02087_DP, 0.01842_DP, 0.02871_DP, 0.04426_DP, 0.32515_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 233, 252 )/ &
     0.99998_DP, 0.99993_DP, 0.99994_DP, 0.99990_DP, 0.99991_DP, 0.99990_DP, 0.99990_DP, &
     0.99978_DP, 0.99981_DP, 0.99990_DP, 0.99978_DP, 0.99937_DP, 0.99997_DP, 0.99997_DP, &
     0.99997_DP, 0.99997_DP, 0.99996_DP, 0.99994_DP, 0.99955_DP, 1.00000_DP/

  ! -- NOAA-15 HIRS/3
  DATA ( channel( l ), l = 253, 272 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 253, 272 )/ &
      669.1800_DP,  678.8120_DP,  690.4790_DP,  703.1960_DP,  715.9820_DP,  731.7930_DP, &
      747.7120_DP,  897.4150_DP, 1032.1880_DP,  801.1550_DP, 1362.4890_DP, 1529.8750_DP, &
     2188.2400_DP, 2209.9880_DP, 2235.3311_DP, 2242.0400_DP, 2419.0271_DP, 2518.8469_DP, &
     2657.3159_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 253, 272 )/ &
     0.00234_DP, 0.01202_DP, 0.01886_DP, 0.01689_DP, 0.02195_DP, 0.01762_DP, 0.02343_DP, &
     0.08723_DP, 0.04646_DP, 0.01471_DP, 0.07389_DP, 0.10292_DP, 0.02453_DP, 0.01917_DP, &
     0.01924_DP, 0.02263_DP, 0.03348_DP, 0.04675_DP, 0.29790_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 253, 272 )/ &
     0.99999_DP, 0.99994_DP, 0.99991_DP, 0.99992_DP, 0.99990_DP, 0.99992_DP, 0.99990_DP, &
     0.99969_DP, 0.99985_DP, 0.99994_DP, 0.99982_DP, 0.99977_DP, 0.99996_DP, 0.99997_DP, &
     0.99997_DP, 0.99996_DP, 0.99995_DP, 0.99993_DP, 0.99959_DP, 1.00000_DP/

  ! -- MODIS PFM     
  DATA ( channel( l ), l = 273, 289 )/ &
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36/
  DATA ( central_frequency( l ), l = 273, 289 )/ &
     2641.9514_DP, 2505.4053_DP, 2518.1399_DP, 2465.5430_DP, 2235.9211_DP, 2200.4043_DP, &
     7272.8174_DP, 1479.2521_DP, 1364.0463_DP, 1174.0076_DP, 1028.2155_DP,  908.6765_DP, &
      831.8903_DP,  748.6353_DP,  731.2197_DP,  719.0625_DP,  704.6822_DP/
  DATA ( temperature_intercept( l ), l = 273, 289 )/ &
     0.48765_DP, 0.09512_DP, 0.10022_DP, 0.08995_DP, 0.07378_DP, 0.07147_DP, 0.25450_DP, &
     0.23272_DP, 0.21236_DP, 0.16415_DP, 0.08260_DP, 0.12294_DP, 0.07218_DP, 0.02175_DP, &
     0.02145_DP, 0.02047_DP, 0.01928_DP/
  DATA ( temperature_slope( l ), l = 273, 289 )/ &
     0.99933_DP, 0.99986_DP, 0.99986_DP, 0.99987_DP, 0.99988_DP, 0.99988_DP, 0.99987_DP, &
     0.99946_DP, 0.99947_DP, 0.99954_DP, 0.99974_DP, 0.99957_DP, 0.99972_DP, 0.99991_DP, &
     0.99991_DP, 0.99991_DP, 0.99991_DP/

  ! -- MODIS Spec.   
  DATA ( channel( l ), l = 290, 306 )/ &
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36/
  DATA ( central_frequency( l ), l = 290, 306 )/ &
     2668.6384_DP, 2525.7512_DP, 2525.7512_DP, 2468.9536_DP, 2240.1279_DP, 2215.8657_DP, &
     7272.8174_DP, 1489.4172_DP, 1361.6765_DP, 1172.3431_DP, 1025.0695_DP,  907.9979_DP, &
      830.5705_DP,  748.3636_DP,  731.4962_DP,  719.0044_DP,  704.4731_DP/
  DATA ( temperature_intercept( l ), l = 290, 306 )/ &
     0.37808_DP, 0.04936_DP, 0.04936_DP, 0.04789_DP, 0.04054_DP, 0.04156_DP, 0.25451_DP, &
     0.34642_DP, 0.19666_DP, 0.15135_DP, 0.07990_DP, 0.08238_DP, 0.06503_DP, 0.01864_DP, &
     0.01973_DP, 0.01742_DP, 0.01545_DP/
  DATA ( temperature_slope( l ), l = 290, 306 )/ &
     0.99948_DP, 0.99993_DP, 0.99993_DP, 0.99993_DP, 0.99993_DP, 0.99993_DP, 0.99987_DP, &
     0.99920_DP, 0.99951_DP, 0.99957_DP, 0.99975_DP, 0.99971_DP, 0.99975_DP, 0.99992_DP, &
     0.99991_DP, 0.99992_DP, 0.99993_DP/

  ! -- NOAA-5 AVHRR  
  DATA ( channel( l ), l = 307, 309 )/ &
     3, 4, 5/
  DATA ( central_frequency( l ), l = 307, 309 )/ &
     2651.1050_DP,  920.6150_DP,  920.6150_DP/
  DATA ( temperature_intercept( l ), l = 307, 309 )/ &
     1.81578_DP, 0.46051_DP, 0.46051_DP/
  DATA ( temperature_slope( l ), l = 307, 309 )/ &
     0.99757_DP, 0.99841_DP, 0.99841_DP/

  ! -- NOAA-6 AVHRR  
  DATA ( channel( l ), l = 310, 312 )/ &
     3, 4, 5/
  DATA ( central_frequency( l ), l = 310, 312 )/ &
     2671.7671_DP,  912.4040_DP,  912.4040_DP/
  DATA ( temperature_intercept( l ), l = 310, 312 )/ &
     1.76683_DP, 0.32518_DP, 0.32518_DP/
  DATA ( temperature_slope( l ), l = 310, 312 )/ &
     0.99761_DP, 0.99887_DP, 0.99887_DP/

  ! -- NOAA-7 AVHRR  
  DATA ( channel( l ), l = 313, 315 )/ &
     3, 4, 5/
  DATA ( central_frequency( l ), l = 313, 315 )/ &
     2688.7271_DP,  927.1360_DP,  840.5460_DP/
  DATA ( temperature_intercept( l ), l = 313, 315 )/ &
     2.08086_DP, 0.34485_DP, 0.23495_DP/
  DATA ( temperature_slope( l ), l = 313, 315 )/ &
     0.99727_DP, 0.99881_DP, 0.99911_DP/

  ! -- NOAA-8 AVHRR  
  DATA ( channel( l ), l = 316, 318 )/ &
     3, 4, 5/
  DATA ( central_frequency( l ), l = 316, 318 )/ &
     2651.3469_DP,  914.2300_DP,  914.2300_DP/
  DATA ( temperature_intercept( l ), l = 316, 318 )/ &
     1.77024_DP, 0.32281_DP, 0.32281_DP/
  DATA ( temperature_slope( l ), l = 316, 318 )/ &
     0.99758_DP, 0.99888_DP, 0.99888_DP/

  ! -- NOAA-9 AVHRR  
  DATA ( channel( l ), l = 319, 321 )/ &
     3, 4, 5/
  DATA ( central_frequency( l ), l = 319, 321 )/ &
     2691.6541_DP,  929.4340_DP,  844.8980_DP/
  DATA ( temperature_intercept( l ), l = 319, 321 )/ &
     1.91250_DP, 0.33589_DP, 0.22604_DP/
  DATA ( temperature_slope( l ), l = 319, 321 )/ &
     0.99745_DP, 0.99885_DP, 0.99915_DP/

  ! -- NOAA-10 AVHRR 
  DATA ( channel( l ), l = 322, 324 )/ &
     3, 4, 5/
  DATA ( central_frequency( l ), l = 322, 324 )/ &
     2672.4871_DP,  909.4960_DP,  909.4960_DP/
  DATA ( temperature_intercept( l ), l = 322, 324 )/ &
     1.78711_DP, 0.28999_DP, 0.28999_DP/
  DATA ( temperature_slope( l ), l = 322, 324 )/ &
     0.99759_DP, 0.99898_DP, 0.99898_DP/

  ! -- NOAA-11 AVHRR 
  DATA ( channel( l ), l = 325, 327 )/ &
     3, 4, 5/
  DATA ( central_frequency( l ), l = 325, 327 )/ &
     2684.2549_DP,  927.7430_DP,  841.9690_DP/
  DATA ( temperature_intercept( l ), l = 325, 327 )/ &
     1.81837_DP, 0.34391_DP, 0.24602_DP/
  DATA ( temperature_slope( l ), l = 325, 327 )/ &
     0.99758_DP, 0.99882_DP, 0.99907_DP/

  ! -- NOAA-12 AVHRR 
  DATA ( channel( l ), l = 328, 330 )/ &
     3, 4, 5/
  DATA ( central_frequency( l ), l = 328, 330 )/ &
     2652.9351_DP,  921.1090_DP,  837.1570_DP/
  DATA ( temperature_intercept( l ), l = 328, 330 )/ &
     1.90344_DP, 0.40766_DP, 0.23351_DP/
  DATA ( temperature_slope( l ), l = 328, 330 )/ &
     0.99743_DP, 0.99859_DP, 0.99912_DP/

  ! -- NOAA-14 AVHRR 
  DATA ( channel( l ), l = 331, 333 )/ &
     3, 4, 5/
  DATA ( central_frequency( l ), l = 331, 333 )/ &
     2659.5149_DP,  929.3830_DP,  834.6060_DP/
  DATA ( temperature_intercept( l ), l = 331, 333 )/ &
     1.98132_DP, 0.43272_DP, 0.24104_DP/
  DATA ( temperature_slope( l ), l = 331, 333 )/ &
     0.99734_DP, 0.99852_DP, 0.99909_DP/

  ! -- NOAA-15 AVHRR 
  DATA ( channel( l ), l = 334, 336 )/ &
     3, 4, 5/
  DATA ( central_frequency( l ), l = 334, 336 )/ &
     2694.8530_DP,  925.7150_DP,  839.5020_DP/
  DATA ( temperature_intercept( l ), l = 334, 336 )/ &
     1.58348_DP, 0.36698_DP, 0.21465_DP/
  DATA ( temperature_slope( l ), l = 334, 336 )/ &
     0.99781_DP, 0.99874_DP, 0.99919_DP/

  ! -- GOES-4 VAS    
  DATA ( channel( l ), l = 337, 348 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12/
  DATA ( central_frequency( l ), l = 337, 348 )/ &
      679.8340_DP,  690.2910_DP,  700.2200_DP,  714.5010_DP,  750.3990_DP, 2208.1079_DP, &
      789.2890_DP,  897.4540_DP, 1374.9110_DP, 1486.1630_DP, 2252.6211_DP, 2541.1089_DP/
  DATA ( temperature_intercept( l ), l = 337, 348 )/ &
     0.01090_DP, 0.01577_DP, 0.01527_DP, 0.02252_DP, 0.02479_DP, 0.07110_DP, 0.03563_DP, &
     1.00261_DP, 0.08623_DP, 0.88194_DP, 0.06108_DP, 0.47665_DP/
  DATA ( temperature_slope( l ), l = 337, 348 )/ &
     0.99995_DP, 0.99993_DP, 0.99993_DP, 0.99990_DP, 0.99989_DP, 0.99988_DP, 0.99986_DP, &
     0.99645_DP, 0.99979_DP, 0.99798_DP, 0.99990_DP, 0.99931_DP/

  ! -- GOES-5 VAS    
  DATA ( channel( l ), l = 349, 360 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12/
  DATA ( central_frequency( l ), l = 349, 360 )/ &
      678.7610_DP,  689.9270_DP,  700.7840_DP,  715.0560_DP,  751.3660_DP, 2207.7930_DP, &
      790.2100_DP,  897.9220_DP, 1375.0000_DP, 1486.1550_DP, 2252.0391_DP, 2544.3999_DP/
  DATA ( temperature_intercept( l ), l = 349, 360 )/ &
     0.01038_DP, 0.01466_DP, 0.01399_DP, 0.02247_DP, 0.02541_DP, 0.07191_DP, 0.02440_DP, &
     0.93448_DP, 0.08474_DP, 0.85066_DP, 0.05993_DP, 0.45914_DP/
  DATA ( temperature_slope( l ), l = 349, 360 )/ &
     0.99995_DP, 0.99993_DP, 0.99994_DP, 0.99990_DP, 0.99989_DP, 0.99988_DP, 0.99990_DP, &
     0.99669_DP, 0.99979_DP, 0.99806_DP, 0.99990_DP, 0.99934_DP/

  ! -- GOES-6 VAS    
  DATA ( channel( l ), l = 361, 372 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12/
  DATA ( central_frequency( l ), l = 361, 372 )/ &
      681.1000_DP,  691.4670_DP,  699.0960_DP,  715.3390_DP,  748.7080_DP, 2208.6790_DP, &
      792.0840_DP,  889.3200_DP, 1375.6219_DP, 1466.2040_DP, 2254.9309_DP, 2526.9060_DP/
  DATA ( temperature_intercept( l ), l = 361, 372 )/ &
     0.01072_DP, 0.01375_DP, 0.01438_DP, 0.02209_DP, 0.02559_DP, 0.07305_DP, 0.02359_DP, &
     0.92825_DP, 0.08318_DP, 0.79211_DP, 0.06056_DP, 0.44756_DP/
  DATA ( temperature_slope( l ), l = 361, 372 )/ &
     0.99995_DP, 0.99994_DP, 0.99993_DP, 0.99990_DP, 0.99989_DP, 0.99988_DP, 0.99991_DP, &
     0.99669_DP, 0.99979_DP, 0.99816_DP, 0.99990_DP, 0.99936_DP/

  ! -- GOES-7 VAS    
  DATA ( channel( l ), l = 373, 384 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12/
  DATA ( central_frequency( l ), l = 373, 384 )/ &
      681.4480_DP,  691.7170_DP,  704.7170_DP,  715.7310_DP,  750.2200_DP, 2213.5520_DP, &
      789.1790_DP,  898.5840_DP, 1376.9139_DP, 1489.8700_DP, 2232.3140_DP, 2532.7041_DP/
  DATA ( temperature_intercept( l ), l = 373, 384 )/ &
     0.01165_DP, 0.01827_DP, 0.01333_DP, 0.02165_DP, 0.03148_DP, 0.07815_DP, 0.02496_DP, &
     0.98634_DP, 0.07835_DP, 0.72862_DP, 0.06044_DP, 0.44971_DP/
  DATA ( temperature_slope( l ), l = 373, 384 )/ &
     0.99995_DP, 0.99992_DP, 0.99994_DP, 0.99990_DP, 0.99987_DP, 0.99987_DP, 0.99990_DP, &
     0.99652_DP, 0.99980_DP, 0.99834_DP, 0.99990_DP, 0.99935_DP/

  ! -- MeteoSat 3    
  DATA ( channel( l ), l = 385, 388 )/ &
     1, 2, 3, 4/
  DATA ( central_frequency( l ), l = 385, 388 )/ &
      876.0223_DP,  876.3080_DP, 1549.4990_DP, 1549.4990_DP/
  DATA ( temperature_intercept( l ), l = 385, 388 )/ &
     0.91491_DP, 0.91033_DP, 4.31718_DP, 4.31718_DP/
  DATA ( temperature_slope( l ), l = 385, 388 )/ &
     0.99668_DP, 0.99670_DP, 0.99042_DP, 0.99042_DP/

  ! -- MeteoSat 4    
  DATA ( channel( l ), l = 389, 392 )/ &
     1, 2, 3, 4/
  DATA ( central_frequency( l ), l = 389, 392 )/ &
      882.2479_DP,  882.0363_DP, 1601.4169_DP, 1601.4169_DP/
  DATA ( temperature_intercept( l ), l = 389, 392 )/ &
     0.80889_DP, 0.81324_DP, 3.22542_DP, 3.22542_DP/
  DATA ( temperature_slope( l ), l = 389, 392 )/ &
     0.99708_DP, 0.99706_DP, 0.99290_DP, 0.99290_DP/

  ! -- MeteoSat 5    
  DATA ( channel( l ), l = 393, 396 )/ &
     1, 2, 3, 4/
  DATA ( central_frequency( l ), l = 393, 396 )/ &
      882.9320_DP,  886.0599_DP, 1612.5583_DP, 1612.5583_DP/
  DATA ( temperature_intercept( l ), l = 393, 396 )/ &
     0.94080_DP, 0.82813_DP, 3.53674_DP, 3.53674_DP/
  DATA ( temperature_slope( l ), l = 393, 396 )/ &
     0.99660_DP, 0.99702_DP, 0.99219_DP, 0.99219_DP/

  ! -- MeteoSat 6    
  DATA ( channel( l ), l = 397, 400 )/ &
     1, 2, 3, 4/
  DATA ( central_frequency( l ), l = 397, 400 )/ &
      878.7573_DP,  878.8658_DP, 1605.8839_DP, 1607.1190_DP/
  DATA ( temperature_intercept( l ), l = 397, 400 )/ &
     0.82243_DP, 0.82563_DP, 3.19673_DP, 3.19777_DP/
  DATA ( temperature_slope( l ), l = 397, 400 )/ &
     0.99702_DP, 0.99701_DP, 0.99294_DP, 0.99293_DP/

  ! -- MeteoSat 7    
  DATA ( channel( l ), l = 401, 404 )/ &
     1, 2, 3, 4/
  DATA ( central_frequency( l ), l = 401, 404 )/ &
      871.6087_DP,  871.5997_DP, 1587.0670_DP, 1589.0453_DP/
  DATA ( temperature_intercept( l ), l = 401, 404 )/ &
     1.04092_DP, 1.03991_DP, 4.00057_DP, 3.95370_DP/
  DATA ( temperature_slope( l ), l = 401, 404 )/ &
     0.99620_DP, 0.99621_DP, 0.99121_DP, 0.99130_DP/

  ! -- GMS-5 Det.A   
  DATA ( channel( l ), l = 405, 407 )/ &
     1, 2, 3/
  DATA ( central_frequency( l ), l = 405, 407 )/ &
      925.3738_DP,  869.6129_DP, 1443.5082_DP/
  DATA ( temperature_intercept( l ), l = 405, 407 )/ &
     0.51474_DP, 0.44856_DP, 0.47288_DP/
  DATA ( temperature_slope( l ), l = 405, 407 )/ &
     0.99823_DP, 0.99836_DP, 0.99888_DP/

  ! -- GMS-5 Det.B   
  DATA ( channel( l ), l = 408, 410 )/ &
     1, 2, 3/
  DATA ( central_frequency( l ), l = 408, 410 )/ &
      929.4388_DP,  869.2247_DP, 1444.7681_DP/
  DATA ( temperature_intercept( l ), l = 408, 410 )/ &
     0.53054_DP, 0.50912_DP, 0.50511_DP/
  DATA ( temperature_slope( l ), l = 408, 410 )/ &
     0.99818_DP, 0.99814_DP, 0.99880_DP/

  ! -- MAS Jan95     
  DATA ( channel( l ), l = 411, 460 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38, &
    39,40,41,42,43,44,45,46,47,48,49,50/
  DATA ( central_frequency( l ), l = 411, 460 )/ &
    18181.8184_DP,15080.6816_DP,14202.5273_DP,13448.0898_DP,12763.2412_DP,12099.2139_DP, &
    11461.3184_DP,11002.3105_DP,10604.4541_DP, 6183.1445_DP, 5980.8608_DP, 5823.7725_DP, &
     5646.5273_DP, 5473.1543_DP, 5288.4868_DP, 5167.9590_DP, 5053.0571_DP, 4933.3994_DP, &
     4805.1514_DP, 4705.8823_DP, 4606.1724_DP, 4506.5342_DP, 4407.2275_DP, 4293.8726_DP, &
     4221.1904_DP, 3426.0654_DP, 3213.5742_DP, 3067.4846_DP, 2929.2012_DP, 2789.7876_DP, &
     2673.8767_DP, 2565.5491_DP, 2468.4673_DP, 2378.0024_DP, 2293.4170_DP, 2215.6265_DP, &
     2143.7437_DP, 2074.3992_DP, 2011.4529_DP, 1952.4651_DP, 1894.2908_DP, 1164.3699_DP, &
     1023.3982_DP,  949.2604_DP,  908.1920_DP,  835.7213_DP,  778.1517_DP,  755.5633_DP, &
      729.2820_DP,  704.9653_DP/
  DATA ( temperature_intercept( l ), l = 411, 460 )/ &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.56864_DP, 0.48582_DP, 0.45785_DP, 0.38229_DP, 0.35976_DP, 0.28202_DP, &
     0.27218_DP, 0.25545_DP, 0.22048_DP, 0.19598_DP, 0.18610_DP, 0.15081_DP, 0.30575_DP, &
     0.28283_DP, 0.17353_DP, 0.15770_DP, 0.10460_DP, 0.07089_DP, 0.07580_DP, 0.08705_DP, &
     0.05843_DP/
  DATA ( temperature_slope( l ), l = 411, 460 )/ &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 0.99926_DP, 0.99934_DP, 0.99935_DP, 0.99944_DP, 0.99945_DP, 0.99956_DP, &
     0.99956_DP, 0.99957_DP, 0.99962_DP, 0.99965_DP, 0.99966_DP, 0.99972_DP, 0.99913_DP, &
     0.99909_DP, 0.99941_DP, 0.99945_DP, 0.99960_DP, 0.99971_DP, 0.99968_DP, 0.99962_DP, &
     0.99973_DP/

  ! -- MAS Jun96     
  DATA ( channel( l ), l = 461, 510 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38, &
    39,40,41,42,43,44,45,46,47,48,49,50/
  DATA ( central_frequency( l ), l = 461, 510 )/ &
    18181.8184_DP,15080.6816_DP,14202.5273_DP,13448.0898_DP,12763.2412_DP,12099.2139_DP, &
    11461.3184_DP,11002.3105_DP,10604.4541_DP, 6183.1445_DP, 5980.8608_DP, 5823.7725_DP, &
     5646.5273_DP, 5473.1543_DP, 5288.4868_DP, 5167.9590_DP, 5053.0571_DP, 4933.3994_DP, &
     4805.1514_DP, 4705.8823_DP, 4606.1724_DP, 4506.5342_DP, 4407.2275_DP, 4293.8726_DP, &
     4221.1904_DP, 3426.0654_DP, 3213.5742_DP, 3067.4846_DP, 2929.2012_DP, 2854.5042_DP, &
     2736.1296_DP, 2621.9785_DP, 2516.2522_DP, 2421.3152_DP, 2336.4961_DP, 2250.8394_DP, &
     2174.8164_DP, 2102.1458_DP, 2036.5098_DP, 1976.6462_DP, 1918.5337_DP, 1172.3962_DP, &
     1030.5238_DP,  951.9429_DP,  907.9117_DP,  834.0051_DP,  775.6671_DP,  752.2060_DP, &
      723.1222_DP,  699.8414_DP/
  DATA ( temperature_intercept( l ), l = 461, 510 )/ &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.52120_DP, 0.51181_DP, 0.45677_DP, 0.39472_DP, 0.35928_DP, 0.32266_DP, &
     0.28751_DP, 0.24534_DP, 0.22019_DP, 0.19173_DP, 0.17809_DP, 0.15160_DP, 0.22324_DP, &
     0.22915_DP, 0.14889_DP, 0.12535_DP, 0.08537_DP, 0.05520_DP, 0.06596_DP, 0.07014_DP, &
     0.04987_DP/
  DATA ( temperature_slope( l ), l = 461, 510 )/ &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 0.99932_DP, 0.99932_DP, 0.99936_DP, 0.99943_DP, 0.99946_DP, 0.99950_DP, &
     0.99954_DP, 0.99959_DP, 0.99963_DP, 0.99966_DP, 0.99968_DP, 0.99972_DP, 0.99937_DP, &
     0.99927_DP, 0.99950_DP, 0.99956_DP, 0.99967_DP, 0.99977_DP, 0.99972_DP, 0.99969_DP, &
     0.99977_DP/

  ! -- MAS Feb97     
  DATA ( channel( l ), l = 511, 560 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38, &
    39,40,41,42,43,44,45,46,47,48,49,50/
  DATA ( central_frequency( l ), l = 511, 560 )/ &
    18181.8184_DP,15080.6816_DP,14202.5273_DP,13448.0898_DP,12763.2412_DP,12099.2139_DP, &
    11461.3184_DP,11002.3105_DP,10604.4541_DP, 6183.1445_DP, 5980.8608_DP, 5823.7725_DP, &
     5646.5273_DP, 5473.1543_DP, 5288.4868_DP, 5167.9590_DP, 5053.0571_DP, 4933.3994_DP, &
     4805.1514_DP, 4705.8823_DP, 4606.1724_DP, 4506.5342_DP, 4407.2275_DP, 4293.8726_DP, &
     4221.1904_DP, 3426.0654_DP, 3213.5742_DP, 3067.4846_DP, 2802.7197_DP, 2685.4392_DP, &
     2577.5027_DP, 2473.6265_DP, 2382.1135_DP, 2294.0471_DP, 2215.2556_DP, 2141.0715_DP, &
     2072.1555_DP, 2008.4579_DP, 1948.3807_DP, 1893.0568_DP, 1853.4858_DP, 1174.6525_DP, &
     1031.6848_DP,  952.9512_DP,  909.4891_DP,  834.6580_DP,  776.6336_DP,  753.6943_DP, &
      724.6671_DP,  701.4915_DP/
  DATA ( temperature_intercept( l ), l = 511, 560 )/ &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.60154_DP, 0.50829_DP, 0.48717_DP, 0.40622_DP, 0.36113_DP, 0.30714_DP, 0.27533_DP, &
     0.26612_DP, 0.22394_DP, 0.19839_DP, 0.18822_DP, 0.14626_DP, 0.16140_DP, 0.24022_DP, &
     0.24101_DP, 0.15216_DP, 0.13486_DP, 0.09010_DP, 0.05564_DP, 0.06730_DP, 0.07240_DP, &
     0.05086_DP/
  DATA ( temperature_slope( l ), l = 511, 560 )/ &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     0.99922_DP, 0.99931_DP, 0.99931_DP, 0.99941_DP, 0.99945_DP, 0.99952_DP, 0.99955_DP, &
     0.99956_DP, 0.99962_DP, 0.99965_DP, 0.99966_DP, 0.99973_DP, 0.99969_DP, 0.99932_DP, &
     0.99923_DP, 0.99949_DP, 0.99953_DP, 0.99966_DP, 0.99977_DP, 0.99972_DP, 0.99968_DP, &
     0.99977_DP/

  ! -- Fengyun2      
  DATA ( channel( l ), l = 561, 562 )/ &
     1, 2/
  DATA ( central_frequency( l ), l = 561, 562 )/ &
      884.1068_DP, 1452.1847_DP/
  DATA ( temperature_intercept( l ), l = 561, 562 )/ &
     1.13260_DP, 3.30686_DP/
  DATA ( temperature_slope( l ), l = 561, 562 )/ &
     0.99591_DP, 0.99218_DP/

  ! -- GOES-11       
  DATA ( channel( l ), l = 563, 586 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    21,22,23,24,25/
  DATA ( central_frequency( l ), l = 563, 586 )/ &
      681.2400_DP,  694.6743_DP,  712.2640_DP,  732.4712_DP,  746.5780_DP,  789.3919_DP, &
      832.2855_DP,  908.7804_DP, 1030.3114_DP, 1339.2693_DP, 1425.8530_DP, 1536.8776_DP, &
     2189.3298_DP, 2211.6252_DP, 2243.4470_DP, 2423.3372_DP, 2511.6924_DP, 2666.2615_DP, &
    15384.6104_DP,15384.6104_DP, 2561.4983_DP, 1482.2567_DP,  932.3574_DP,  832.7332_DP/
  DATA ( temperature_intercept( l ), l = 563, 586 )/ &
     0.00955_DP, 0.01206_DP, 0.01230_DP, 0.01189_DP, 0.01317_DP, 0.04492_DP, 0.12990_DP, &
     0.11800_DP, 0.03855_DP, 0.15092_DP, 0.27383_DP, 0.13448_DP, 0.01900_DP, 0.01939_DP, &
     0.02005_DP, 0.04879_DP, 0.05423_DP, 0.29165_DP, 0.00000_DP, 0.00000_DP, 0.62864_DP, &
     0.59339_DP, 0.38284_DP, 0.20258_DP/
  DATA ( temperature_slope( l ), l = 563, 586 )/ &
     0.99996_DP, 0.99994_DP, 0.99994_DP, 0.99995_DP, 0.99994_DP, 0.99982_DP, 0.99951_DP, &
     0.99959_DP, 0.99988_DP, 0.99962_DP, 0.99934_DP, 0.99969_DP, 0.99997_DP, 0.99997_DP, &
     0.99997_DP, 0.99993_DP, 0.99992_DP, 0.99960_DP, 1.00000_DP, 1.00000_DP, 0.99911_DP, &
     0.99861_DP, 0.99869_DP, 0.99923_DP/

  ! -- GOES-12       
  DATA ( channel( l ), l = 587, 610 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    21,22,23,24,26/
  DATA ( central_frequency( l ), l = 587, 610 )/ &
      681.4457_DP,  694.8673_DP,  711.6454_DP,  733.0012_DP,  749.4852_DP,  789.6354_DP, &
      832.9395_DP,  911.1657_DP, 1027.6647_DP, 1342.6996_DP, 1424.1841_DP, 1536.6445_DP, &
     2186.9526_DP, 2208.0701_DP, 2246.9890_DP, 2422.1497_DP, 2511.7500_DP, 2661.9778_DP, &
    15384.6104_DP,15384.6104_DP, 2564.8223_DP, 1542.3776_DP,  933.4076_DP,  751.1144_DP/
  DATA ( temperature_intercept( l ), l = 587, 610 )/ &
     0.01031_DP, 0.01296_DP, 0.01241_DP, 0.01191_DP, 0.01705_DP, 0.04485_DP, 0.13553_DP, &
     0.12334_DP, 0.03855_DP, 0.15895_DP, 0.27473_DP, 0.13826_DP, 0.02217_DP, 0.01779_DP, &
     0.02034_DP, 0.04941_DP, 0.05390_DP, 0.28781_DP, 0.00000_DP, 0.00000_DP, 0.69711_DP, &
     5.08328_DP, 0.37555_DP, 0.09535_DP/
  DATA ( temperature_slope( l ), l = 587, 610 )/ &
     0.99995_DP, 0.99994_DP, 0.99994_DP, 0.99995_DP, 0.99993_DP, 0.99982_DP, 0.99948_DP, &
     0.99957_DP, 0.99988_DP, 0.99960_DP, 0.99934_DP, 0.99969_DP, 0.99996_DP, 0.99997_DP, &
     0.99997_DP, 0.99993_DP, 0.99992_DP, 0.99961_DP, 1.00000_DP, 1.00000_DP, 0.99902_DP, &
     0.98872_DP, 0.99872_DP, 0.99960_DP/

  ! -- NOAA-16 HIRS/3
  DATA ( channel( l ), l = 611, 630 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20/
  DATA ( central_frequency( l ), l = 611, 630 )/ &
      669.7900_DP,  679.8890_DP,  691.2570_DP,  701.9150_DP,  716.6280_DP,  731.3900_DP, &
      749.8310_DP,  897.8800_DP, 1032.4189_DP,  803.5770_DP, 1364.3220_DP, 1526.9370_DP, &
     2187.0100_DP, 2206.5339_DP, 2233.3440_DP, 2242.8999_DP, 2417.9951_DP, 2517.8750_DP, &
     2666.3201_DP,14500.0000_DP/
  DATA ( temperature_intercept( l ), l = 611, 630 )/ &
     0.00242_DP, 0.01310_DP, 0.01923_DP, 0.01668_DP, 0.02232_DP, 0.01739_DP, 0.02388_DP, &
     0.08920_DP, 0.04635_DP, 0.01466_DP, 0.07154_DP, 0.10743_DP, 0.02458_DP, 0.01879_DP, &
     0.01994_DP, 0.02201_DP, 0.03558_DP, 0.04754_DP, 0.30393_DP, 0.00000_DP/
  DATA ( temperature_slope( l ), l = 611, 630 )/ &
     0.99999_DP, 0.99994_DP, 0.99991_DP, 0.99992_DP, 0.99990_DP, 0.99992_DP, 0.99990_DP, &
     0.99968_DP, 0.99985_DP, 0.99994_DP, 0.99982_DP, 0.99976_DP, 0.99996_DP, 0.99997_DP, &
     0.99997_DP, 0.99996_DP, 0.99995_DP, 0.99993_DP, 0.99958_DP, 1.00000_DP/

  ! -- MODIS AM1     
  DATA ( channel( l ), l = 631, 647 )/ &
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36/
  DATA ( central_frequency( l ), l = 631, 647 )/ &
     2641.7747_DP, 2505.2771_DP, 2518.0286_DP, 2465.4277_DP, 2235.8152_DP, 2200.3462_DP, &
     7272.8174_DP, 1477.9669_DP, 1362.7373_DP, 1173.1901_DP, 1027.7155_DP,  908.0376_DP, &
      831.5399_DP,  748.3394_DP,  730.8963_DP,  718.8681_DP,  704.5367_DP/
  DATA ( temperature_intercept( l ), l = 631, 647 )/ &
     0.47533_DP, 0.09216_DP, 0.09713_DP, 0.08884_DP, 0.07322_DP, 0.07077_DP, 0.25447_DP, &
     0.22178_DP, 0.20597_DP, 0.15991_DP, 0.08139_DP, 0.12001_DP, 0.07021_DP, 0.02083_DP, &
     0.02025_DP, 0.01932_DP, 0.01683_DP/
  DATA ( temperature_slope( l ), l = 631, 647 )/ &
     0.99935_DP, 0.99987_DP, 0.99986_DP, 0.99987_DP, 0.99988_DP, 0.99988_DP, 0.99987_DP, &
     0.99948_DP, 0.99949_DP, 0.99955_DP, 0.99974_DP, 0.99958_DP, 0.99973_DP, 0.99991_DP, &
     0.99991_DP, 0.99991_DP, 0.99992_DP/

  ! -- MeteoSat SG1  
  DATA ( channel( l ), l = 648, 655 )/ &
     1, 2, 3, 4, 5, 6, 7, 8/
  DATA ( central_frequency( l ), l = 648, 655 )/ &
      747.7863_DP,  840.2908_DP,  931.0472_DP, 1035.1588_DP, 1149.1992_DP, 1362.2972_DP, &
     1609.2186_DP, 2580.0864_DP/
  DATA ( temperature_intercept( l ), l = 648, 655 )/ &
     0.27813_DP, 0.26969_DP, 0.43518_DP, 0.04372_DP, 0.15027_DP, 0.43101_DP, 2.06751_DP, &
     3.38113_DP/
  DATA ( temperature_slope( l ), l = 648, 655 )/ &
     0.99882_DP, 0.99898_DP, 0.99851_DP, 0.99986_DP, 0.99957_DP, 0.99892_DP, 0.99553_DP, &
     0.99534_DP/

  ! -- GMS-5 Det.A2  
  DATA ( channel( l ), l = 656, 658 )/ &
     1, 2, 3/
  DATA ( central_frequency( l ), l = 656, 658 )/ &
      925.3739_DP,  869.6128_DP, 1453.2585_DP/
  DATA ( temperature_intercept( l ), l = 656, 658 )/ &
     0.51472_DP, 0.44857_DP, 0.44805_DP/
  DATA ( temperature_slope( l ), l = 656, 658 )/ &
     0.99823_DP, 0.99836_DP, 0.99893_DP/

  ! -- MAS Mar99     
  DATA ( channel( l ), l = 659, 708 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38, &
    39,40,41,42,43,44,45,46,47,48,49,50/
  DATA ( central_frequency( l ), l = 659, 708 )/ &
    18181.8184_DP,15080.6816_DP,14202.5273_DP,13448.0898_DP,12763.2412_DP,12099.2139_DP, &
    11461.3184_DP,11002.3105_DP,10604.4541_DP, 6183.1445_DP, 5980.8608_DP, 5823.7725_DP, &
     5646.5273_DP, 5473.1543_DP, 5288.4868_DP, 5167.9590_DP, 5053.0571_DP, 4933.3994_DP, &
     4805.1514_DP, 4705.8823_DP, 4606.1724_DP, 4506.5342_DP, 4407.2275_DP, 4293.8726_DP, &
     4221.1904_DP, 3426.0654_DP, 3213.5742_DP, 3067.4846_DP, 2779.1758_DP, 2666.0457_DP, &
     2556.8425_DP, 2457.5610_DP, 2364.1255_DP, 2283.1172_DP, 2202.5024_DP, 2129.9458_DP, &
     2061.1589_DP, 1999.0144_DP, 1939.7693_DP, 1885.6880_DP, 1846.0917_DP, 1181.1044_DP, &
     1035.3038_DP,  958.1932_DP,  913.3672_DP,  837.3330_DP,  781.3520_DP,  755.7639_DP, &
      726.7311_DP,  703.0262_DP/
  DATA ( temperature_intercept( l ), l = 659, 708 )/ &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.61662_DP, 0.52822_DP, 0.46598_DP, 0.45265_DP, 0.33163_DP, 0.29796_DP, 0.27657_DP, &
     0.26368_DP, 0.22343_DP, 0.18590_DP, 0.17652_DP, 0.18208_DP, 0.20497_DP, 0.24776_DP, &
     0.19995_DP, 0.16299_DP, 0.13278_DP, 0.09993_DP, 0.07446_DP, 0.07083_DP, 0.08299_DP, &
     0.05685_DP/
  DATA ( temperature_slope( l ), l = 659, 708 )/ &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     0.99921_DP, 0.99928_DP, 0.99935_DP, 0.99937_DP, 0.99949_DP, 0.99953_DP, 0.99955_DP, &
     0.99956_DP, 0.99962_DP, 0.99967_DP, 0.99968_DP, 0.99965_DP, 0.99960_DP, 0.99930_DP, &
     0.99936_DP, 0.99945_DP, 0.99953_DP, 0.99962_DP, 0.99970_DP, 0.99970_DP, 0.99963_DP, &
     0.99974_DP/

  ! -- MAS Jan00     
  DATA ( channel( l ), l = 709, 758 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38, &
    39,40,41,42,43,44,45,46,47,48,49,50/
  DATA ( central_frequency( l ), l = 709, 758 )/ &
    18181.8184_DP,15080.6816_DP,14202.5273_DP,13448.0898_DP,12763.2412_DP,12099.2139_DP, &
    11461.3184_DP,11002.3105_DP,10604.4541_DP, 6183.1445_DP, 5980.8608_DP, 5823.7725_DP, &
     5646.5273_DP, 5473.1543_DP, 5288.4868_DP, 5167.9590_DP, 5053.0571_DP, 4933.3994_DP, &
     4805.1514_DP, 4705.8823_DP, 4606.1724_DP, 4506.5342_DP, 4407.2275_DP, 4293.8726_DP, &
     4221.1904_DP, 3426.0654_DP, 3213.5742_DP, 3067.4846_DP, 2780.6328_DP, 2670.8247_DP, &
     2564.5740_DP, 2463.8884_DP, 2373.1108_DP, 2291.4961_DP, 2213.6111_DP, 2141.0110_DP, &
     2074.2073_DP, 2011.9910_DP, 1954.0674_DP, 1899.4778_DP, 1860.4587_DP, 1171.3835_DP, &
     1035.3677_DP,  957.2316_DP,  914.1708_DP,  838.5929_DP,  783.9796_DP,  758.5498_DP, &
      730.2234_DP,  706.0392_DP/
  DATA ( temperature_intercept( l ), l = 709, 758 )/ &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.51807_DP, 0.42762_DP, 0.42469_DP, 0.38133_DP, 0.33558_DP, 0.30138_DP, 0.28409_DP, &
     0.26005_DP, 0.24769_DP, 0.20502_DP, 0.17790_DP, 0.15627_DP, 0.12197_DP, 0.27388_DP, &
     0.25089_DP, 0.18677_DP, 0.15631_DP, 0.11874_DP, 0.09129_DP, 0.08759_DP, 0.09588_DP, &
     0.06503_DP/
  DATA ( temperature_slope( l ), l = 709, 758 )/ &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     0.99932_DP, 0.99941_DP, 0.99941_DP, 0.99945_DP, 0.99949_DP, 0.99953_DP, 0.99954_DP, &
     0.99956_DP, 0.99957_DP, 0.99964_DP, 0.99968_DP, 0.99971_DP, 0.99977_DP, 0.99922_DP, &
     0.99920_DP, 0.99937_DP, 0.99945_DP, 0.99955_DP, 0.99963_DP, 0.99963_DP, 0.99958_DP, &
     0.99970_DP/

  ! -- MAS Oct00     
  DATA ( channel( l ), l = 759, 808 )/ &
     1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19, &
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38, &
    39,40,41,42,43,44,45,46,47,48,49,50/
  DATA ( central_frequency( l ), l = 759, 808 )/ &
    18181.8184_DP,15080.6816_DP,14202.5273_DP,13448.0898_DP,12763.2412_DP,12099.2139_DP, &
    11461.3184_DP,11002.3105_DP,10604.4541_DP, 6183.1445_DP, 5980.8608_DP, 5823.7725_DP, &
     5646.5273_DP, 5473.1543_DP, 5288.4868_DP, 5167.9590_DP, 5053.0571_DP, 4933.3994_DP, &
     4805.1514_DP, 4705.8823_DP, 4606.1724_DP, 4506.5342_DP, 4407.2275_DP, 4293.8726_DP, &
     4221.1904_DP, 3426.0654_DP, 3213.5742_DP, 3067.4846_DP, 2752.7686_DP, 2647.9761_DP, &
     2540.3821_DP, 2442.2498_DP, 2374.2346_DP, 2292.5437_DP, 2198.7036_DP, 2127.2539_DP, &
     2061.0830_DP, 2000.7931_DP, 1944.6562_DP, 1891.4918_DP, 1854.9449_DP, 1171.8671_DP, &
     1034.1681_DP,  956.2694_DP,  913.3351_DP,  837.3818_DP,  781.7614_DP,  758.1290_DP, &
      729.1593_DP,  705.4514_DP/
  DATA ( temperature_intercept( l ), l = 759, 808 )/ &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, 0.00000_DP, &
     0.56512_DP, 0.49226_DP, 0.46961_DP, 0.41826_DP, 0.33599_DP, 0.30172_DP, 0.32300_DP, &
     0.28315_DP, 0.28140_DP, 0.23663_DP, 0.24245_DP, 0.18091_DP, 0.15971_DP, 0.27536_DP, &
     0.24629_DP, 0.17959_DP, 0.15564_DP, 0.11657_DP, 0.07754_DP, 0.08346_DP, 0.09690_DP, &
     0.06050_DP/
  DATA ( temperature_slope( l ), l = 759, 808 )/ &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, 1.00000_DP, &
     0.99925_DP, 0.99931_DP, 0.99933_DP, 0.99938_DP, 0.99949_DP, 0.99953_DP, 0.99948_DP, &
     0.99952_DP, 0.99951_DP, 0.99958_DP, 0.99956_DP, 0.99967_DP, 0.99969_DP, 0.99922_DP, &
     0.99921_DP, 0.99940_DP, 0.99945_DP, 0.99956_DP, 0.99969_DP, 0.99965_DP, 0.99957_DP, &
     0.99972_DP/


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC ABS, ANY, EPSILON, MINLOC, SUM




CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       sensor_channels
!
! PURPOSE:
!       Function to return the number and list of channels for a given sensor
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       error_status = sensor_channels( sensor_id,   &  ! Input
!                                       n_channels,  &  ! Output
!                                       channels,    &  ! Output
!                                       message_log = message_log ) ! Optional input
!
! INPUT ARGUMENTS:
!       sensor_id:    Number uniquely identifying a given sensor/platform.
!                     Valid values are:
!                           1) GOES-8             26) GOES-4 VAS
!                           2) GOES-9             27) GOES-5 VAS
!                           3) GOES-10            28) GOES-6 VAS
!                           4) NOAA-5 HIRS/2      29) GOES-7 VAS
!                           5) NOAA-6 HIRS/2      30) MeteoSat 3
!                           6) NOAA-7 HIRS/2      31) MeteoSat 4
!                           7) NOAA-8 HIRS/2      32) Meteosat 5
!                           8) NOAA-9 HIRS/2      33) Meteosat 6
!                           9) NOAA-10 HIRS/2     34) Meteosat 7
!                          10) NOAA-11 HIRS/2     35) GMS-5 Det.A
!                          11) NOAA-12 HIRS/2     36) GMS-5 Det.B
!                          12) NOAA-14 HIRS/2     37) MAS Jan95
!                          13) NOAA-15 HIRS/3     38) MAS Jun96
!                          14) MODIS PFM          39) MAS Feb97
!                          15) MODIS Spec.        40) Fengyun2
!                          16) NOAA-5 AVHRR       41) GOES-11
!                          17) NOAA-6 AVHRR       42) GOES-12
!                          18) NOAA-7 AVHRR       43) NOAA-16 HIRS/3
!                          19) NOAA-8 AVHRR       44) MODIS AM1
!                          20) NOAA-9 AVHRR       45) MeteoSat SG1
!                          21) NOAA-10 AVHRR      46) GMS-5 Det.A (new ch3)
!                          22) NOAA-11 AVHRR      47) MAS Mar99
!                          23) NOAA-12 AVHRR      48) MAS Jan00
!                          24) NOAA-14 AVHRR      49) MAS Oct00
!                          25) NOAA-15 AVHRR
!                     UNITS:      None.
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
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
!       n_channels:   The number of valid sensor channel numbers returned in
!                     the CHANNELS output argument.
!                     UNITS:      None.
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
!       channels:     An array containing the list of channel numbers. The
!                     starting channel number may not be 1. If the number
!                     of sensor channels is greater than the dimension of
!                     the supplied argument, an informational message is
!                     output and as many channels as can be accomodated by
!                     the array are returned.
!                     UNITS:      None.
!                     TYPE:       Integer
!                     DIMENSION:  Rank-1, assumed-shape
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the function call was successful
!                 = FAILURE an error occured processing the input
!
! CALLS:
!      display_message:    Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!      None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! EXAMPLE:
!       INTEGER, PARAMETER :: MAX_N_CHANNELS = 50
!       INTEGER, DIMENSION( MAX_N_CHANNELS ) :: channel_number
!       INTEGER :: n, n_channels
!       INTEGER :: error_status
!
!       error_status = sensor_channels( n, n_channels, channel_number )
!       IF ( error_status /= SUCCESS ) STOP
!S-
!------------------------------------------------------------------------------

  FUNCTION sensor_channels( sensor_id,    &  ! Input
                            n_channels,   &  ! Output
                            channels,     &  ! Output
                            message_log ) &  ! Optional input
                          RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                 INTENT( IN )  :: sensor_id
    INTEGER,                 INTENT( OUT ) :: n_channels
    INTEGER, DIMENSION( : ), INTENT( OUT ) :: channels

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SENSOR_CHANNELS'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n, l1, l2



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

    ! -- Sensor id input
    IF ( sensor_id < 1 .OR. &
         sensor_id > MAX_N_SENSORS ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_ID argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Sensor channel array
    n = SIZE( channels )

    IF ( n_sensor_channels( sensor_id ) > n ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Input CHANNELS array not large enough to hold '//&
                            'all channel numbers for sensor '//&
                            sensor_description( sensor_id ), &
                            INFORMATION, &
                            message_log = message_log )
    ENDIF

 

    !-------------------------------------------------------------------------------
    !                      -- Get channel number data --
    !-------------------------------------------------------------------------------

    n_channels = MIN( n_sensor_channels( sensor_id ), n )

    l1 = SUM( n_sensor_channels( 0:sensor_id-1 ) ) + 1
    l2 = l1 + n_channels - 1

    channels( 1:n_channels ) = channel( l1:l2 )

    error_status = SUCCESS

  END FUNCTION sensor_channels




!------------------------------------------------------------------------------
!S+
! NAME:
!       sensor_convert_radiance
!
! PURPOSE:
!       Function to convert radiances from units of mW/(m2.sr.cm-1) to
!       W/(m2.sr.micron) or vice versa.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       error_status = sensor_convert_radiance( unit_key,        &  ! Input
!                                               sensor_id,       &  ! Input
!                                               sensor_channel,  &  ! Input
!                                               input_radiance,  &  ! Input
!                                               output_radiance, &  ! Output
!                                               message_log = message_log ) ! Optional input
!
! INPUT ARGUMENTS:
!       unit_key:        Flag specifying the INPUT radiance units.
!                        Valid values are:
!                          WAVENUMBER_UNIT: x in cm-1, radiance in mW/(m2.sr.cm-1)
!                          WAVELENGTH_UNIT: x in micron, radiance in W/(m2.sr.micron)
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       sensor_id:       Number uniquely identifying a given sensor/platform.
!                        Valid values are:
!                              1) GOES-8             26) GOES-4 VAS
!                              2) GOES-9             27) GOES-5 VAS
!                              3) GOES-10            28) GOES-6 VAS
!                              4) NOAA-5 HIRS/2      29) GOES-7 VAS
!                              5) NOAA-6 HIRS/2      30) MeteoSat 3
!                              6) NOAA-7 HIRS/2      31) MeteoSat 4
!                              7) NOAA-8 HIRS/2      32) Meteosat 5
!                              8) NOAA-9 HIRS/2      33) Meteosat 6
!                              9) NOAA-10 HIRS/2     34) Meteosat 7
!                             10) NOAA-11 HIRS/2     35) GMS-5 Det.A
!                             11) NOAA-12 HIRS/2     36) GMS-5 Det.B
!                             12) NOAA-14 HIRS/2     37) MAS Jan95
!                             13) NOAA-15 HIRS/3     38) MAS Jun96
!                             14) MODIS PFM          39) MAS Feb97
!                             15) MODIS Spec.        40) Fengyun2
!                             16) NOAA-5 AVHRR       41) GOES-11
!                             17) NOAA-6 AVHRR       42) GOES-12
!                             18) NOAA-7 AVHRR       43) NOAA-16 HIRS/3
!                             19) NOAA-8 AVHRR       44) MODIS AM1
!                             20) NOAA-9 AVHRR       45) MeteoSat SG1
!                             21) NOAA-10 AVHRR      46) GMS-5 Det.A (new ch3)
!                             22) NOAA-11 AVHRR      47) MAS Mar99
!                             23) NOAA-12 AVHRR      48) MAS Jan00
!                             24) NOAA-14 AVHRR      49) MAS Oct00
!                             25) NOAA-15 AVHRR
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       sensor_channel:  Channel number of the specified sensor for which
!                        the radiance is required.
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       input_radiance:  The Planck radiance for the sensor channel
!                        that is to be converted to different units.
!                        UNITS:      mW/(m2.sr.cm-1) OR W/(m2.sr.micron)
!                        TYPE:       Double precision float
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       Character
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       output_radiance: The Planck radiance for the sensor channel
!                        in the requested units.
!                        UNITS:      W/(m2.sr.micron) OR mW/(m2.sr.cm-1)
!                        TYPE:       Double precision float
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the function call was successful
!                 = WARNING input radiance was less than the
!                           defined numerical precision and the
!                           returned radiance was set to 0.0.
!                 = FAILURE an error occured processing the input
!                           or performing the calculation.
!
! CALLS:
!      display_message:    Subroutine to output messages
!                          SOURCE: error_handler module
!
!      sensor_temperature: Function to calculate the sensor Planck
!                          temperature.
!
!      sensor_radiance:    Function to calculate the sensor Planck
!                          radiance.
!
! CONTAINS:
!      None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       If the input radiance is less than machine precision
!       (determined using the EPSILON intrinsic function) then an
!       output radiance value of 0.0 is returned and the WARNING
!       status set.
!
! EXAMPLE:
!       USE type_kinds
!
!       INTEGER, PARAMETER :: MAX_N_CHANNELS = 50
!       INTEGER, DIMENSION( MAX_N_CHANNELS ) :: channel_number
!       INTEGER :: n, l
!       INTEGER :: error_status
!
!       REAL( Double ) :: in_radiance, out_radiance
!         :
!         :
!       error_status = sensor_convert_radiance( units, &
!                                               n, &
!                                              channel_number( l ), &
!                                              in_radiance, &
!                                              out_radiance )
!       IF ( error_status /= SUCCESS ) STOP
!
! PROCEDURE:
!       The input radiance is converted to a temperature and this temperature
!       is converted back to a radiance in the appropriate units.
!S-
!------------------------------------------------------------------------------

  FUNCTION sensor_convert_radiance( units,           &  ! Input
                                    sensor_id,       &  ! Input
                                    sensor_channel,  &  ! Input
                                    input_radiance,  &  ! Input
                                    output_radiance, &  ! Output
                                    message_log )    &  ! Optional input
                                  RESULT ( error_status )


    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,    INTENT( IN )  :: units
    INTEGER,    INTENT( IN )  :: sensor_id
    INTEGER,    INTENT( IN )  :: sensor_channel
    REAL( DP ), INTENT( IN )  :: input_radiance

    REAL( DP ), INTENT( OUT ) :: output_radiance

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SENSOR_CONVERT_RADIANCE'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 80 ) :: message

    INTEGER :: l1, l2

    INTEGER :: output_units

    REAL( DP ) :: temperature



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

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

    ! -- Sensor id input
    IF ( sensor_id < 1 .OR. &
         sensor_id > MAX_N_SENSORS ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_ID argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Sensor channel input
    l1 = SUM( n_sensor_channels( 0:sensor_id-1 ) ) + 1
    l2 = l1 + n_sensor_channels( sensor_id ) - 1

    IF ( .NOT. ( ANY( channel( l1:l2 ) == sensor_channel ) ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_CHANNEL for '//sensor_description( sensor_id ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
 
    ! -- Radiance input.
    ! -- If less than zero == return error
    ! -- If less than precision == return zero.
    IF ( input_radiance < ZERO ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid INPUT_RADIANCE argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    IF ( input_radiance < TOLERANCE ) THEN
      error_status = WARNING
      WRITE( message, '( "INPUT_RADIANCE < numerical precision (", es8.1, &
                        &"). Output radiance set to 0.0" )' ) &
                      TOLERANCE
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      output_radiance = ZERO
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                         -- Convert the radiance units --
    !-------------------------------------------------------------------------------

    ! -------------------------------------------
    ! Convert the input radiance to a temperature
    ! -------------------------------------------

    error_status = sensor_temperature( units,          &  ! Input
                                       sensor_id,      &  ! Input
                                       sensor_channel, &  ! Input
                                       input_radiance, &  ! Input
                                       temperature,    &  ! Output
                                       message_log = message_log ) ! Optional input

    IF ( error_status /= SUCCESS ) THEN
      WRITE( message, '( "Call to SENSOR_TEMPERATURE failed for ", a, " channel ", i2 )' ) &
                      sensor_description( sensor_id ), sensor_channel
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ---------------------------------------------
    ! Determine the required output units
    ! I chose to do it this way as I didn't want 
    ! to rely on the actual VALUES of the unit keys
    ! ---------------------------------------------
 
    IF ( units == WAVENUMBER_UNIT ) THEN
      output_units = WAVELENGTH_UNIT
    ELSE
      output_units = WAVENUMBER_UNIT
    END IF


    ! ------------------------------------------------------
    ! Convert the temperature to the required radiance units
    ! ------------------------------------------------------

    error_status = sensor_radiance( output_units,    &  ! Input
                                    sensor_id,       &  ! Input
                                    sensor_channel,  &  ! Input
                                    temperature,     &  ! Input
                                    output_radiance, &  ! Output
                                    message_log = message_log ) ! Optional input

    IF ( error_status /= SUCCESS ) THEN
      WRITE( message, '( "Call to SENSOR_RADIANCE failed for ", a, " channel ", i2 )' ) &
                      sensor_description( sensor_id ), sensor_channel
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

  END FUNCTION sensor_convert_radiance 





!------------------------------------------------------------------------------
!S+
! NAME:
!       sensor_radiance
!
! PURPOSE:
!       Function to calculate the Planck radiance for various broadband 
!       infrared sensors given the temperature.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       error_status = sensor_radiance( unit_key,       &  ! Input
!                                       sensor_id,      &  ! Input
!                                       sensor_channel, &  ! Input
!                                       temperature,    &  ! Input
!                                       radiance,       &  ! Output
!                                       message_log = message_log ) ! Optional input
!
! INPUT ARGUMENTS:
!       unit_key:        Flag specifying spectral units. Valid values are:
!                          WAVENUMBER_UNIT: x in cm-1, radiance in mW/(m2.sr.cm-1)
!                          WAVELENGTH_UNIT: x in micron, radiance in W/(m2.sr.micron)
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       sensor_id:       Number uniquely identifying a given sensor/platform.
!                        Valid values are:
!                              1) GOES-8             26) GOES-4 VAS
!                              2) GOES-9             27) GOES-5 VAS
!                              3) GOES-10            28) GOES-6 VAS
!                              4) NOAA-5 HIRS/2      29) GOES-7 VAS
!                              5) NOAA-6 HIRS/2      30) MeteoSat 3
!                              6) NOAA-7 HIRS/2      31) MeteoSat 4
!                              7) NOAA-8 HIRS/2      32) Meteosat 5
!                              8) NOAA-9 HIRS/2      33) Meteosat 6
!                              9) NOAA-10 HIRS/2     34) Meteosat 7
!                             10) NOAA-11 HIRS/2     35) GMS-5 Det.A
!                             11) NOAA-12 HIRS/2     36) GMS-5 Det.B
!                             12) NOAA-14 HIRS/2     37) MAS Jan95
!                             13) NOAA-15 HIRS/3     38) MAS Jun96
!                             14) MODIS PFM          39) MAS Feb97
!                             15) MODIS Spec.        40) Fengyun2
!                             16) NOAA-5 AVHRR       41) GOES-11
!                             17) NOAA-6 AVHRR       42) GOES-12
!                             18) NOAA-7 AVHRR       43) NOAA-16 HIRS/3
!                             19) NOAA-8 AVHRR       44) MODIS AM1
!                             20) NOAA-9 AVHRR       45) MeteoSat SG1
!                             21) NOAA-10 AVHRR      46) GMS-5 Det.A (new ch3)
!                             22) NOAA-11 AVHRR      47) MAS Mar99
!                             23) NOAA-12 AVHRR      48) MAS Jan00
!                             24) NOAA-14 AVHRR      49) MAS Oct00
!                             25) NOAA-15 AVHRR
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       sensor_channel:  Channel number of the specified sensor for which
!                        the radiance is required.
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       temperature:     Temperature for which the Planck radiance is
!                        required.
!                        UNITS:      Kelvin
!                        TYPE:       Double precision float
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       Character
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       radiance:        The Planck radiance for the requested sensor channel
!                        at the specified temperature.
!                        UNITS:      mW/(m2.sr.cm-1) OR W/(m2.sr.micron)
!                        TYPE:       Double precision float
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the function call was successful
!                 = WARNING input temperature was less than the
!                           defined numerical precision and the
!                           returned radiance was set to 0.0.
!                 = FAILURE an error occured processing the input
!                           or performing the calculation.
!
! CALLS:
!      display_message:    Subroutine to output messages
!                          SOURCE: error_handler module
!
!      planck_radiance:    Function to calculate monochromatic Planck
!                          radiances.
!                          SOURCE: planck_functions module
!
! CONTAINS:
!      None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       If the input temperature is less than machine precision
!       (determined using the EPSILON intrinsic function) then a
!       radiance value of 0.0 is returned and the WARNING status
!       set..
!
! EXAMPLE:
!       USE type_kinds
!
!       INTEGER, PARAMETER :: MAX_N_CHANNELS = 50
!       INTEGER, DIMENSION( MAX_N_CHANNELS ) :: channel_number
!       INTEGER :: n, l
!       INTEGER :: error_status
!
!       REAL( Double ) :: temperature, radiance
!         :
!         :
!       error_status = sensor_radiance( units, &
!                                       n, &
!                                       channel_number( l ), &
!                                       temperature, &
!                                       radiance )
!       IF ( error_status /= SUCCESS ) STOP
!
! PROCEDURE:
!       A polychromatic correction is applied to the input temperature, T,
!
!         Tc = b0 + ( b1 * T )
!
!       where b0 and b1 are sensor channel dependent constants.
!
!       The corrected temperature, Tc, is then used in the monochromatic
!       Planck radiance calculation.
!S-
!------------------------------------------------------------------------------

  FUNCTION sensor_radiance( units,          &  ! Input
                            sensor_id,      &  ! Input
                            sensor_channel, &  ! Input
                            temperature,    &  ! Input
                            radiance,       &  ! Output
                            message_log )   &  ! Optional input
                          RESULT ( error_status )


    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,    INTENT( IN )  :: units
    INTEGER,    INTENT( IN )  :: sensor_id
    INTEGER,    INTENT( IN )  :: sensor_channel
    REAL( DP ), INTENT( IN )  :: temperature

    REAL( DP ), INTENT( OUT ) :: radiance

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SENSOR_RADIANCE'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 80 ) :: message

    INTEGER :: l, l1, l2
    INTEGER, DIMENSION( 1 ) :: channel_index

    REAL( DP ) :: frequency
    REAL( DP ) :: polychromatic_temperature



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

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

    ! -- Sensor id input
    IF ( sensor_id < 1 .OR. &
         sensor_id > MAX_N_SENSORS ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_ID argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Sensor channel input
    l1 = SUM( n_sensor_channels( 0:sensor_id-1 ) ) + 1
    l2 = l1 + n_sensor_channels( sensor_id ) - 1

    IF ( .NOT. ( ANY( channel( l1:l2 ) == sensor_channel ) ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_CHANNEL for '//sensor_description( sensor_id ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
 
    ! -- Temperature input.
    ! -- If less than zero == return error
    ! -- If less than precision == return zero.
    IF ( temperature < ZERO ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid TEMPERATURE argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    IF ( temperature < TOLERANCE ) THEN
      error_status = WARNING
      WRITE( message, '( "Input TEMPERATURE < numerical precision (", es8.1, &
                        &"). Radiance set to 0.0" )' ) &
                      TOLERANCE
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      radiance = ZERO
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                           -- Calculate radiance --
    !-------------------------------------------------------------------------------

    ! ------------------------------
    ! Get the required channel index
    ! ------------------------------

    channel_index = MINLOC( ABS( channel( l1:l2 ) - sensor_channel ) )
    l = l1 + channel_index( 1 ) - 1


    ! -----------------------------------
    ! Calculate the temperature corrected
    ! for polychromaticity
    ! -----------------------------------

    polychromatic_temperature = temperature_intercept( l ) + ( temperature_slope( l ) * temperature )


    ! ---------------------------------
    ! Convert the frequency if required
    ! ---------------------------------

    IF ( units == WAVENUMBER_UNIT ) THEN
      frequency = central_frequency( l )
    ELSE
      frequency = 10000.0_DP / central_frequency( l )
    END IF


    ! -------------------------------------------
    ! Calculate the monochromatic Planck radiance
    ! with the corrected temperature
    ! -------------------------------------------

    error_status = planck_radiance( units,                     &
                                    frequency,                 &
                                    polychromatic_temperature, &
                                    radiance,                  &
                                    message_log = message_log  )

    IF ( error_status /= SUCCESS ) THEN
      WRITE( message, '( "Call to PLANCK_RADIANCE failed for ", a, " channel ", i2 )' ) &
                      sensor_description( sensor_id ), sensor_channel
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

  END FUNCTION sensor_radiance 





!------------------------------------------------------------------------------
!S+
! NAME:
!       sensor_temperature
!
! PURPOSE:
!       Function to calculate the Planck temperature for various broadband 
!       infrared sensors given teh radiance.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       error_status = sensor_temperature( unit_key,       &  ! Input
!                                          sensor_id,      &  ! Input
!                                          sensor_channel, &  ! Input
!                                          radiance,       &  ! Input
!                                          temperature,    &  ! Output
!                                          message_log = message_log ) ! Optional input
!
! INPUT ARGUMENTS:
!       unit_key:        Flag specifying spectral units. Valid values are:
!                          WAVENUMBER_UNIT: x in cm-1, radiance in mW/(m2.sr.cm-1)
!                          WAVELENGTH_UNIT: x in micron, radiance in W/(m2.sr.micron)
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       sensor_id:       Number uniquely identifying a given sensor/platform.
!                        Valid values are:
!                              1) GOES-8             26) GOES-4 VAS
!                              2) GOES-9             27) GOES-5 VAS
!                              3) GOES-10            28) GOES-6 VAS
!                              4) NOAA-5 HIRS/2      29) GOES-7 VAS
!                              5) NOAA-6 HIRS/2      30) MeteoSat 3
!                              6) NOAA-7 HIRS/2      31) MeteoSat 4
!                              7) NOAA-8 HIRS/2      32) Meteosat 5
!                              8) NOAA-9 HIRS/2      33) Meteosat 6
!                              9) NOAA-10 HIRS/2     34) Meteosat 7
!                             10) NOAA-11 HIRS/2     35) GMS-5 Det.A
!                             11) NOAA-12 HIRS/2     36) GMS-5 Det.B
!                             12) NOAA-14 HIRS/2     37) MAS Jan95
!                             13) NOAA-15 HIRS/3     38) MAS Jun96
!                             14) MODIS PFM          39) MAS Feb97
!                             15) MODIS Spec.        40) Fengyun2
!                             16) NOAA-5 AVHRR       41) GOES-11
!                             17) NOAA-6 AVHRR       42) GOES-12
!                             18) NOAA-7 AVHRR       43) NOAA-16 HIRS/3
!                             19) NOAA-8 AVHRR       44) MODIS AM1
!                             20) NOAA-9 AVHRR       45) MeteoSat SG1
!                             21) NOAA-10 AVHRR      46) GMS-5 Det.A (new ch3)
!                             22) NOAA-11 AVHRR      47) MAS Mar99
!                             23) NOAA-12 AVHRR      48) MAS Jan00
!                             24) NOAA-14 AVHRR      49) MAS Oct00
!                             25) NOAA-15 AVHRR
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       sensor_channel:  Channel number of the specified sensor for which
!                        the radiance is required.
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       radiance:        The Planck radiance for which the sensor channel
!                        temperature is required.
!                        UNITS:      mW/(m2.sr.cm-1) OR W/(m2.sr.micron)
!                        TYPE:       Double precision float
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       Character
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       temperature:     The Planck temperature for the requested sensor channel
!                        and radiance.
!                        UNITS:      Kelvin
!                        TYPE:       Double precision float
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the function call was successful
!                 = WARNING input radiance was less than the
!                           defined numerical precision and the
!                           returned temperature was set to 0.0.
!                 = FAILURE an error occured processing the input
!                           or performing the calculation.
!
! CALLS:
!      display_message:    Subroutine to output messages
!                          SOURCE: error_handler module
!
!      planck_temperature: Function to calculate monochromatic Planck
!                          temperature.
!                          SOURCE: planck_functions module
!
! CONTAINS:
!      None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       If the input radiance is less than machine precision
!       (determined using the EPSILON intrinsic function) then a
!       temperature value of 0.0 is returned and the WARNING status
!       set..
!
! EXAMPLE:
!       USE type_kinds
!
!       INTEGER, PARAMETER :: MAX_N_CHANNELS = 50
!       INTEGER, DIMENSION( MAX_N_CHANNELS ) :: channel_number
!       INTEGER :: n, l
!       INTEGER :: error_status
!
!       REAL( Double ) :: temperature, radiance
!         :
!         :
!       error_status = sensor_temperature( units, &
!                                          n, &
!                                          channel_number( l ), &
!                                          radiance, &
!                                          temperature )
!       IF ( error_status /= SUCCESS ) STOP
!
! PROCEDURE:
!       The Planck temperature is calculated using the input polychromatic
!       radiance assuming monochromaticity. This result yields a "polychromatic
!       temperature", Tc, which is then corrected for polychromaticity to yield
!       the returned temperature T,
!
!         T = ( Tc - b0 ) / b1
!
!       where b0 and b1 are sensor channel dependent constants.
!S-
!------------------------------------------------------------------------------

  FUNCTION sensor_temperature( units,          &  ! Input
                               sensor_id,      &  ! Input
                               sensor_channel, &  ! Input
                               radiance,       &  ! Input
                               temperature,    &  ! Output
                               message_log )   &  ! Optional input
                             RESULT ( error_status )


    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,    INTENT( IN )  :: units
    INTEGER,    INTENT( IN )  :: sensor_id
    INTEGER,    INTENT( IN )  :: sensor_channel
    REAL( DP ), INTENT( IN )  :: radiance

    REAL( DP ), INTENT( OUT ) :: temperature

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SENSOR_TEMPERATURE'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 80 ) :: message

    INTEGER :: l, l1, l2
    INTEGER, DIMENSION( 1 ) :: channel_index

    REAL( DP ) :: frequency
    REAL( DP ) :: polychromatic_temperature



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

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

    ! -- Sensor id input
    IF ( sensor_id < 1 .OR. &
         sensor_id > MAX_N_SENSORS ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_ID argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Sensor channel input
    l1 = SUM( n_sensor_channels( 0:sensor_id-1 ) ) + 1
    l2 = l1 + n_sensor_channels( sensor_id ) - 1

    IF ( .NOT. ( ANY( channel( l1:l2 ) == sensor_channel ) ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_CHANNEL for '//sensor_description( sensor_id ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
 
    ! -- Radiance input.
    ! -- If less than zero == return error
    ! -- If less than precision == return zero.
!!$      Pass through a missing value
!!$    IF ( radiance < ZERO ) THEN
!!$      error_status = FAILURE
!!$      CALL display_message( ROUTINE_NAME, &
!!$                            'Invalid RADIANCE argument', &
!!$                            error_status, &
!!$                            message_log = message_log )
!!$      RETURN
!!$    ENDIF

    IF ( radiance < TOLERANCE ) THEN
      error_status = WARNING
!!$      WRITE( message, '( "Input RADIANCE < numerical precision (", es8.1, &
!!$                        &"). Temperature set to 0.0" )' ) &
!!$                      TOLERANCE
!!$      CALL display_message( ROUTINE_NAME, &
!!$                            message, &
!!$                            error_status, &
!!$                            message_log = message_log )
      temperature = radiance
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                           -- Calculate temperature --
    !-------------------------------------------------------------------------------

    ! ------------------------------
    ! Get the required channel index
    ! ------------------------------

    channel_index = MINLOC( ABS( channel( l1:l2 ) - sensor_channel ) )
    l = l1 + channel_index( 1 ) - 1


    ! ---------------------------------
    ! Convert the frequency if required
    ! ---------------------------------

    IF ( units == WAVENUMBER_UNIT ) THEN
      frequency = central_frequency( l )
    ELSE
      frequency = 10000.0_DP / central_frequency( l )
    END IF


    ! ---------------------------------
    ! Calculate the temperature for the
    ! monochromatic Planck function
    ! ---------------------------------

    error_status = planck_temperature( units,                     &
                                       frequency,                 &
                                       radiance,                  &
                                       polychromatic_temperature, &
                                       message_log = message_log  )

    IF ( error_status /= SUCCESS ) THEN
      WRITE( message, '( "Call to PLANCK_TEMPERATURE failed for ", a, " channel ", i2 )' ) &
                      sensor_description( sensor_id ), sensor_channel
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! -----------------------
    ! Correct the temperature
    ! for polychromaticity
    ! -----------------------

    temperature = ( polychromatic_temperature - temperature_intercept( l ) ) / &
    !             ----------------------------------------------------------
                                    temperature_slope( l )


  END FUNCTION sensor_temperature





!------------------------------------------------------------------------------
!S+
! NAME:
!       sensor_dbdt
!
! PURPOSE:
!       Function to calculate the derivative of the Planck radiance with
!       respect to temperature for various broadband infrared sensors given
!       the temperature.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       error_status = sensor_dbdt( unit_key,       &  ! Input
!                                   sensor_id,      &  ! Input
!                                   sensor_channel, &  ! Input
!                                   temperature,    &  ! Input
!                                   dbdt,           &  ! Output
!                                   message_log = message_log ) ! Optional input
!
! INPUT ARGUMENTS:
!       unit_key:        Flag specifying spectral units. Valid values are:
!                          WAVENUMBER_UNIT: x in cm-1, radiance in mW/(m2.sr.cm-1)
!                          WAVELENGTH_UNIT: x in micron, radiance in W/(m2.sr.micron)
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       sensor_id:       Number uniquely identifying a given sensor/platform.
!                        Valid values are:
!                              1) GOES-8             26) GOES-4 VAS
!                              2) GOES-9             27) GOES-5 VAS
!                              3) GOES-10            28) GOES-6 VAS
!                              4) NOAA-5 HIRS/2      29) GOES-7 VAS
!                              5) NOAA-6 HIRS/2      30) MeteoSat 3
!                              6) NOAA-7 HIRS/2      31) MeteoSat 4
!                              7) NOAA-8 HIRS/2      32) Meteosat 5
!                              8) NOAA-9 HIRS/2      33) Meteosat 6
!                              9) NOAA-10 HIRS/2     34) Meteosat 7
!                             10) NOAA-11 HIRS/2     35) GMS-5 Det.A
!                             11) NOAA-12 HIRS/2     36) GMS-5 Det.B
!                             12) NOAA-14 HIRS/2     37) MAS Jan95
!                             13) NOAA-15 HIRS/3     38) MAS Jun96
!                             14) MODIS PFM          39) MAS Feb97
!                             15) MODIS Spec.        40) Fengyun2
!                             16) NOAA-5 AVHRR       41) GOES-11
!                             17) NOAA-6 AVHRR       42) GOES-12
!                             18) NOAA-7 AVHRR       43) NOAA-16 HIRS/3
!                             19) NOAA-8 AVHRR       44) MODIS AM1
!                             20) NOAA-9 AVHRR       45) MeteoSat SG1
!                             21) NOAA-10 AVHRR      46) GMS-5 Det.A (new ch3)
!                             22) NOAA-11 AVHRR      47) MAS Mar99
!                             23) NOAA-12 AVHRR      48) MAS Jan00
!                             24) NOAA-14 AVHRR      49) MAS Oct00
!                             25) NOAA-15 AVHRR
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       sensor_channel:  Channel number of the specified sensor for which
!                        the radiance is required.
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       temperature:     Temperature at which the Planck radiance derivate is
!                        required.
!                        UNITS:      Kelvin
!                        TYPE:       Double precision float
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       Character
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       dbdt:            The Planck radiance derivate dB/dT for the requested
!                        sensor channel at the specified temperature.
!                        UNITS:      mW/(m2.sr.cm-1.K) OR W/(m2.sr.micron.K)
!                        TYPE:       Double precision float
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the function call was successful
!                 = WARNING input temperature was less than the
!                           defined numerical precision and the
!                           returned dB/dT was set to 0.0.
!                 = FAILURE an error occured processing the input
!                           or performing the calculation.
!
! CALLS:
!      display_message:    Subroutine to output messages
!                          SOURCE: error_handler module
!
!      planck_dbdt:        Function to calculate monochromatic Planck
!                          radiance derivative.
!                          SOURCE: planck_functions module
!
! CONTAINS:
!      None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       If the input temperature is less than machine precision
!       (determined using the EPSILON intrinsic function) then a
!       dB/dT value of 0.0 is returned and the WARNING status
!       set..
!
! EXAMPLE:
!       USE type_kinds
!
!       INTEGER, PARAMETER :: MAX_N_CHANNELS = 50
!       INTEGER, DIMENSION( MAX_N_CHANNELS ) :: channel_number
!       INTEGER :: n, l
!       INTEGER :: error_status
!
!       REAL( Double ) :: temperature, dbdt
!         :
!         :
!       error_status = sensor_dbdt( units, &
!                                   n, &
!                                   channel_number( l ), &
!                                   temperature, &
!                                   dbdt )
!       IF ( error_status /= SUCCESS ) STOP
!
! PROCEDURE:
!       A polychromatic correction is applied to the input temperature, T,
!
!         Tc = b0 + ( b1 * T )
!
!       where b0 and b1 are sensor channel dependent constants.
!
!       The corrected temperature, Tc, is then used in the monochromatic
!       Planck radiance derivative calculation.
!S-
!------------------------------------------------------------------------------

  FUNCTION sensor_dbdt( units,          &  ! Input
                        sensor_id,      &  ! Input
                        sensor_channel, &  ! Input
                        temperature,    &  ! Input
                        dbdt,           &  ! Output
                        message_log )   &  ! Optional input
                      RESULT ( error_status )


    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,    INTENT( IN )  :: units
    INTEGER,    INTENT( IN )  :: sensor_id
    INTEGER,    INTENT( IN )  :: sensor_channel
    REAL( DP ), INTENT( IN )  :: temperature

    REAL( DP ), INTENT( OUT ) :: dbdt

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SENSOR_DBDT'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 80 ) :: message

    INTEGER :: l, l1, l2
    INTEGER, DIMENSION( 1 ) :: channel_index

    REAL( DP ) :: frequency
    REAL( DP ) :: polychromatic_temperature



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

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

    ! -- Sensor id input
    IF ( sensor_id < 1 .OR. &
         sensor_id > MAX_N_SENSORS ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_ID argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Sensor channel input
    l1 = SUM( n_sensor_channels( 0:sensor_id-1 ) ) + 1
    l2 = l1 + n_sensor_channels( sensor_id ) - 1

    IF ( .NOT. ( ANY( channel( l1:l2 ) == sensor_channel ) ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_CHANNEL for '//sensor_description( sensor_id ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
 
    ! -- Temperature input.
    ! -- If less than zero == return error
    ! -- If less than precision == return zero.
    IF ( temperature < ZERO ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid TEMPERATURE argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    IF ( temperature < TOLERANCE ) THEN
      error_status = WARNING
      WRITE( message, '( "Input TEMPERATURE < numerical precision (", es8.1, &
                        &"). dB/dT set to 0.0" )' ) &
                      TOLERANCE
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      dbdt = ZERO
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                           -- Calculate radiance --
    !-------------------------------------------------------------------------------

    ! ------------------------------
    ! Get the required channel index
    ! ------------------------------

    channel_index = MINLOC( ABS( channel( l1:l2 ) - sensor_channel ) )
    l = l1 + channel_index( 1 ) - 1


    ! -----------------------------------
    ! Calculate the temperature corrected
    ! for polychromaticity
    ! -----------------------------------

    polychromatic_temperature = temperature_intercept( l ) + ( temperature_slope( l ) * temperature )


    ! ---------------------------------
    ! Convert the frequency if required
    ! ---------------------------------

    IF ( units == WAVENUMBER_UNIT ) THEN
      frequency = central_frequency( l )
    ELSE
      frequency = 10000.0_DP / central_frequency( l )
    END IF


    ! ----------------------------------------
    ! Calculate the monochromatic Planck dB/dT
    ! with the corrected temperature
    ! ----------------------------------------

    error_status = planck_dbdt( units,                     &
                                frequency,                 &
                                polychromatic_temperature, &
                                dbdt,                      &
                                message_log = message_log  )

    IF ( error_status /= SUCCESS ) THEN
      WRITE( message, '( "Call to PLANCK_DBDT failed for ", a, " channel ", i2 )' ) &
                      sensor_description( sensor_id ), sensor_channel
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

  END FUNCTION sensor_dbdt





!------------------------------------------------------------------------------
!S+
! NAME:
!       sensor_dtdb
!
! PURPOSE:
!       Function to calculate the Planck temperature derivative with respect
!       to radiance for various broadband infrared sensors given the radiance.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       error_status = sensor_dtdb( unit_key,       &  ! Input
!                                   sensor_id,      &  ! Input
!                                   sensor_channel, &  ! Input
!                                   radiance,       &  ! Input
!                                   dtdb,           &  ! Output
!                                   message_log = message_log ) ! Optional input
!
! INPUT ARGUMENTS:
!       unit_key:        Flag specifying spectral units. Valid values are:
!                          WAVENUMBER_UNIT: x in cm-1, radiance in mW/(m2.sr.cm-1)
!                          WAVELENGTH_UNIT: x in micron, radiance in W/(m2.sr.micron)
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       sensor_id:       Number uniquely identifying a given sensor/platform.
!                        Valid values are:
!                              1) GOES-8             26) GOES-4 VAS
!                              2) GOES-9             27) GOES-5 VAS
!                              3) GOES-10            28) GOES-6 VAS
!                              4) NOAA-5 HIRS/2      29) GOES-7 VAS
!                              5) NOAA-6 HIRS/2      30) MeteoSat 3
!                              6) NOAA-7 HIRS/2      31) MeteoSat 4
!                              7) NOAA-8 HIRS/2      32) Meteosat 5
!                              8) NOAA-9 HIRS/2      33) Meteosat 6
!                              9) NOAA-10 HIRS/2     34) Meteosat 7
!                             10) NOAA-11 HIRS/2     35) GMS-5 Det.A
!                             11) NOAA-12 HIRS/2     36) GMS-5 Det.B
!                             12) NOAA-14 HIRS/2     37) MAS Jan95
!                             13) NOAA-15 HIRS/3     38) MAS Jun96
!                             14) MODIS PFM          39) MAS Feb97
!                             15) MODIS Spec.        40) Fengyun2
!                             16) NOAA-5 AVHRR       41) GOES-11
!                             17) NOAA-6 AVHRR       42) GOES-12
!                             18) NOAA-7 AVHRR       43) NOAA-16 HIRS/3
!                             19) NOAA-8 AVHRR       44) MODIS AM1
!                             20) NOAA-9 AVHRR       45) MeteoSat SG1
!                             21) NOAA-10 AVHRR      46) GMS-5 Det.A (new ch3)
!                             22) NOAA-11 AVHRR      47) MAS Mar99
!                             23) NOAA-12 AVHRR      48) MAS Jan00
!                             24) NOAA-14 AVHRR      49) MAS Oct00
!                             25) NOAA-15 AVHRR
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       sensor_channel:  Channel number of the specified sensor for which
!                        the radiance is required.
!                        UNITS:      None.
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       radiance:        The Planck radiance for which the sensor channel
!                        temperature derivate is required.
!                        UNITS:      mW/(m2.sr.cm-1) OR W/(m2.sr.micron)
!                        TYPE:       Double precision float
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       Character
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       dtdb:            The Planck temperature derivative dT/dB for the
!                        requested sensor channel and radiance.
!                        UNITS:      (K.m2.sr.cm-1)/mW OR (K.m2.sr.um)/W
!                        TYPE:       Double precision float
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the function call was successful
!                 = WARNING input radiance was less than the
!                           defined numerical precision and the
!                           returned dT/dB was set to 0.0.
!                 = FAILURE an error occured processing the input
!                           or performing the calculation.
!
! CALLS:
!      display_message:    Subroutine to output messages
!                          SOURCE: error_handler module
!
!      planck_dtdb:        Function to calculate monochromatic Planck
!                          temperature derivative.
!                          SOURCE: planck_functions module
!
! CONTAINS:
!      None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       If the input radiance is less than machine precision
!       (determined using the EPSILON intrinsic function) then a
!       dT/dB value of 0.0 is returned and the WARNING status
!       set..
!
! EXAMPLE:
!       USE type_kinds
!
!       INTEGER, PARAMETER :: MAX_N_CHANNELS = 50
!       INTEGER, DIMENSION( MAX_N_CHANNELS ) :: channel_number
!       INTEGER :: n, l
!       INTEGER :: error_status
!
!       REAL( Double ) :: dtdb, radiance
!         :
!         :
!       error_status = sensor_dtdb( units, &
!                                   n, &
!                                   channel_number( l ), &
!                                   radiance, &
!                                   dtdb )
!       IF ( error_status /= SUCCESS ) STOP
!
! PROCEDURE:
!       The Planck temperature derivative is calculated using the input
!       polychromatic radiance assuming monochromaticity. This result yields a
!       polychromatic derivative, (dTc/dB), which is then corrected for
!       polychromaticity to yield the returned derivative, dT/dB,
!
!         dT/dB = ( dTc/dB ) / b1
!
!       where b1 is a sensor channel dependent constant.
!S-
!------------------------------------------------------------------------------

  FUNCTION sensor_dtdb( units,          &  ! Input
                        sensor_id,      &  ! Input
                        sensor_channel, &  ! Input
                        radiance,       &  ! Input
                        dtdb,           &  ! Output
                        message_log )   &  ! Optional input
                      RESULT ( error_status )


    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    INTEGER,    INTENT( IN )  :: units
    INTEGER,    INTENT( IN )  :: sensor_id
    INTEGER,    INTENT( IN )  :: sensor_channel
    REAL( DP ), INTENT( IN )  :: radiance

    REAL( DP ), INTENT( OUT ) :: dtdb

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SENSOR_DTDB'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 80 ) :: message

    INTEGER :: l, l1, l2
    INTEGER, DIMENSION( 1 ) :: channel_index

    REAL( DP ) :: frequency
    REAL( DP ) :: polychromatic_dtdb



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

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

    ! -- Sensor id input
    IF ( sensor_id < 1 .OR. &
         sensor_id > MAX_N_SENSORS ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_ID argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Sensor channel input
    l1 = SUM( n_sensor_channels( 0:sensor_id-1 ) ) + 1
    l2 = l1 + n_sensor_channels( sensor_id ) - 1

    IF ( .NOT. ( ANY( channel( l1:l2 ) == sensor_channel ) ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid SENSOR_CHANNEL for '//sensor_description( sensor_id ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
 
    ! -- Radiance input.
    ! -- If less than zero == return error
    ! -- If less than precision == return zero.
    IF ( radiance < ZERO ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid RADIANCE argument', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    IF ( radiance < TOLERANCE ) THEN
      error_status = WARNING
      WRITE( message, '( "Input RADIANCE < numerical precision (", es8.1, &
                        &"). dT/dB set to 0.0" )' ) &
                      TOLERANCE
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      dtdb = ZERO
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !                           -- Calculate temperature --
    !-------------------------------------------------------------------------------

    ! ------------------------------
    ! Get the required channel index
    ! ------------------------------

    channel_index = MINLOC( ABS( channel( l1:l2 ) - sensor_channel ) )
    l = l1 + channel_index( 1 ) - 1


    ! ---------------------------------
    ! Convert the frequency if required
    ! ---------------------------------

    IF ( units == WAVENUMBER_UNIT ) THEN
      frequency = central_frequency( l )
    ELSE
      frequency = 10000.0_DP / central_frequency( l )
    END IF


    ! -----------------------------
    ! Calculate the dT/dB for the
    ! monochromatic Planck function
    ! -----------------------------

    error_status = planck_dtdb( units,                     &
                                frequency,                 &
                                radiance,                  &
                                polychromatic_dtdb,        &
                                message_log = message_log  )

    IF ( error_status /= SUCCESS ) THEN
      WRITE( message, '( "Call to PLANCK_DTDB failed for ", a, " channel ", i2 )' ) &
                      sensor_description( sensor_id ), sensor_channel
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ---------------------
    ! Correct the dT/dB for
    ! polychromaticity
    ! ---------------------

    dtdb = polychromatic_dtdb / temperature_slope( l )


  END FUNCTION sensor_dtdb


END MODULE sensor_planck_functions


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: sensor_planck_functions.f90,v 1.1 2003/06/07 16:39:35 phil Exp $
!
! $Date: 2003/06/07 16:39:35 $
!
! $Revision: 1.1 $
!
! $State: Exp $
!
! $Log: sensor_planck_functions.f90,v $
! Revision 1.1  2003/06/07 16:39:35  phil
! new diagnostics
!
! Revision 1.3  2001/05/09 18:19:58  paulv
! - Updated header documentation.
!
! Revision 1.2  2001/05/09 17:37:50  paulv
! - Added SENSOR_CONVERT_RADIANCE() function.
! - Removed definition of WAVENUMBER_UNIT and WAVELENGTH_UNIT. Values are
!   inherited from the PLANCK_FUNCTIONS module and made PUBLIC.
! - Corrected bug with channel list assignment in SENSOR_CHANNELS() function.
!   List assignment was by:
!     channels = channel( l1:l2 )
!   where CHANNELS is an assumed shape dummy argument array that may not
!   correspond to the same length as l2-l1+1. This was changed to:
!     channels( 1:n_channels ) = channel( l1:l2 )
!   where N_CHANNELS is previously assigned as the maximum number of channels
!   that can be returned.
! - All functions except SENSOR_CHANNELS now return a WARNING status if any
!   input radiances or temperatures are less than numberical precision
!   defined using the EPSILON() instrinsic. If this occurs, output variables
!   are set to 0.0.
!   Output messages corresponding to this scenario were made more descriptive.
! - All references to CHANNEL( l ) in output error messages were not needed
!   and replaced with SENSOR_CHANNEL.
!
! Revision 1.1  2001/05/08 21:38:08  paulv
! Initial checkin
!
!
!
