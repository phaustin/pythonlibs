!------------------------------------------------------------------------------
!M+
! NAME:
!       fundamental_constants
!
! PURPOSE:
!       Module containing various fundamental physical constants.
!
! CATEGORY:
!       Constants
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE fundamental_constants
!
! MODULES:
!       type_kinds:      Module containing definitions for kinds of variable
!                        types
!
! CONTAINS:
!       None.
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
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The fundamental constants used are taken from the NIST Reference on
!       Constants, Units, and Uncertainty website:
!
!         http://physics.nist.gov/cuu/Constants/
!
!       See also:
!
!         Mohr, P.J. and B.N. Taylor, "CODATA recommended values of the
!           fundamental physical constants: 1998", Reviews of Modern Physics, 
!           Vol.72, No.2, 2000.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-May-2000
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

MODULE fundamental_constants


  ! ------------
  ! Modules used
  ! ------------

  USE type_kinds


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE

 

  !#----------------------------------------------------------------------------#
  !#                                    -- PI! --                               #
  !#----------------------------------------------------------------------------#

  REAL( Double ), PARAMETER, PUBLIC :: PI = 3.14159265358979_Double



  !#----------------------------------------------------------------------------#
  !#                             -- Univeral constants --                       #
  !#----------------------------------------------------------------------------#


  ! ----------------------------------------------
  ! Speed of light
  ! Symbol:c,  Units:m/s,  Rel.Uncert.(ppm): exact
  ! ----------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: SPEED_OF_LIGHT = 2.99792458e+08_Double


  ! --------------------------------------------------
  ! Permeability of vacuum
  ! Symbol:mu0,  Units:N/A^2,  Rel.Uncert.(ppm): exact
  ! --------------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: PERMEABILITY = PI * 4.0e-07_Double


  ! -----------------------------------------------------
  ! Permittivity of vacuum
  ! Symbol:epsilon0,  Units:F/m,  Rel.Uncert.(ppm): exact
  ! -----------------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: PERMITTIVITY =            1.0_Double / &
                                       !              ------------------------------------
                                                      ( PERMEABILITY * SPEED_OF_LIGHT**2 )


  ! ---------------------------------------------
  ! Planck constant
  ! Symbol:h,  Units:Js,  Rel.Uncert.(ppm): 0.078
  ! ---------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: PLANCK_CONSTANT = 6.62606876e-34_Double


  ! ----------------------------------------------------
  ! Gravitational constant
  ! Symbol:G,  Units:m^3/kg/s^2,  Rel.Uncert.(ppm): 1500
  ! ----------------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: GRAVITATIONAL_CONSTANT = 6.673e-11_Double



  !#----------------------------------------------------------------------------#
  !#                        -- Physicochemical constants --                     #
  !#----------------------------------------------------------------------------#

  ! -----------------------------------------------------
  ! Avogadro constant
  ! Symbol:N(A),  Units:mole^-1,  Rel.Uncert.(ppm): 0.079
  ! -----------------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: AVOGADRO_CONSTANT = 6.02214199e+23_Double


  ! --------------------------------------------
  ! Boltzmann constant
  ! Symbol:k,  Units:J/K,  Rel.Uncert.(ppm): 1.7
  ! --------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: BOLTZMANN_CONSTANT = 1.3806503e-23_Double


  ! -------------------------------------------------
  ! Molar gas constant
  ! Symbol:R,  Units:J/mole/K,  Rel.Uncert.(ppm): 1.7
  ! -------------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: MOLAR_GAS_CONSTANT = 8.314472_Double


  ! ------------------------------------------------------
  ! Stefan-Boltzmann constant
  ! Symbol:sigma,  Units:W/m^2/K^4,  Rel.Uncert.(ppm): 7.0
  ! ------------------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: STEFAN_BOLTZMANN_CONSTANT = 5.670400e-08_Double



  !#----------------------------------------------------------------------------#
  !#                          -- Conversion factors --                          #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------------
  ! Electron volt
  ! Symbol:eV,  Units:J,  Rel.Uncert.(ppm): 0.039
  ! ---------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: ELECTRON_VOLT = 1.602176462e-19_Double


  ! ---------------------------------------------
  ! Unified atomic mass unit
  ! Symbol:u,  Units:kg,  Rel.Uncert.(ppm): 0.079
  ! ---------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: UNIFIED_ATOMIC_MASS_UNIT = 1.66053873e-27_Double


  ! -----------------------------------------------
  ! Standard atmosphere
  ! Symbol:atm,  Units:Pa,  Rel.Uncert.(ppm): exact
  ! -----------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: STANDARD_ATMOSPHERE = 101325.0_Double


  ! ------------------------------------------------
  ! Standard gravity
  ! Symbol:g,  Units:m/s^2,  Rel.Uncert.(ppm): exact
  ! ------------------------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: STANDARD_GRAVITY = 9.80665_Double



  !#----------------------------------------------------------------------------#
  !#                          -- Derived constants --                           #
  !#----------------------------------------------------------------------------#


  ! ------------------------------
  ! First Planck function constant
  ! Symbol:c1,  Units:W.m^2
  ! ------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: C_1 = 2.0_Double * PLANCK_CONSTANT * SPEED_OF_LIGHT**2


  ! -------------------------------
  ! Second Planck function constant
  ! Symbol:c2,  Units:K.m
  ! -------------------------------

  REAL( Double ), PARAMETER, PUBLIC :: C_2 = PLANCK_CONSTANT * SPEED_OF_LIGHT / &
                                       !     ----------------------------------
                                                      BOLTZMANN_CONSTANT


END MODULE fundamental_constants


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: fundamental_constants.f90,v 1.1 2003/06/07 16:39:35 phil Exp $
!
! $Date: 2003/06/07 16:39:35 $
!
! $Revision: 1.1 $
!
! $State: Exp $
!
! $Log: fundamental_constants.f90,v $
! Revision 1.1  2003/06/07 16:39:35  phil
! new diagnostics
!
! Revision 1.4  2001/05/09 17:43:27  paulv
! - Corrected error in documentation.
!
! Revision 1.3  2001/05/09 17:39:42  paulv
! - All constant definitions are now double precision only.
!
! Revision 1.2  2000/12/18 21:15:47  paulv
! - Added single and double precision definitions.
!
! Revision 1.1  2000/05/03 18:36:27  paulv
! Initial checked-in version
!
!
