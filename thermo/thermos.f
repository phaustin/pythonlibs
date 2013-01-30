c ***********************************************************
c  AtmosThermoPack, put together by Dr. Piotr Flatau
c  (CalSpace, Scripps Institute of Oceanography,
c  La Jolla, CA) and including his own original code and
c  some code developed by Eric Smith and Warren Wiscombe.

c  Flatau:  pflatau@macao.ucsd.edu
c  Wiscombe:  wiscombe@climate.gsfc.nasa.gov
c  Eric Smith:  esmith@metsat.met.fsu.edu

c  Some References:
c http://sio209.wikispaces.com/file/view/thermos.f

c    Flatau, P., T. Lee and W. Wiscombe, 1991:  THERMOS, an 
c       interactive package for thermodynamic analysis and Òskew-TÓ
c       diagram, Dept. of Atmospheric Science, Colorado State
c       University, Ft. Collins, CO 80523.

c    Iribarne, J. and W. Godson, 1981: Atmospheric Thermodynamics, 
c       2nd ed., Reidel Publishing

c    Pruppacher, H. and J. Klett, 1980: Microphysics of Clouds and
c       Precipitation, Reidel Publishing (Chaps. 4 & 6)

c    List, R., ed., Smithsonian Meteorological Tables

c    Bolton, Mon. Wea. Rev. 108, July 1980

c ***********************************************************
c NOTES: In the interest of economy, these routines perform
c        no argument error-checking, therefore caveat emptor.

c        Watch out for units, e.g. mixing ratios in g/g not g/kg.

c        In an effort not to have these routines be failure
c        points, they return zero when the input leads to
c        illogical values.  Keep an eye out for zeros.
c ***********************************************************

c  Calling trees for the routines with dependencies:


c    DESWAT--ESWAT

c    DESICE--ESICE


c    ENYMOI-+-ENYDRY--ENTHAL--CPMOI
c           |
c           +-HEATL see 4
c           |
c           +-SPHUM


c    ENTROP-+-CPMOI
c           |
c           +-HEATL see 4


c    RELHUM--RRMIXV see 2


c    THETA--THMOI


c    TTHEQV--YBRENT see 3


c    TCONEP-+-THEQV (1)-+-RRMIXV (2)--ESWAT
c           |           |
c           |           +-THMOI
c           |           |
c           |           +-TLCL--EWAT
c           |
c           +-ERRMSG


c    TTHVIR--YBRENT (3)-+-@FUNC
c                       |
c                       +-ERRMSG


c    TMLAPS-+-HEATL (4)--ERRMSG
c           |
c           +-CPMOI
c           |
c           +-ESWAT


c    TFROST-+-TDEW-+-EWAT
c           |      |
c           |      +-ESWAT
c           |      |
c           |      +-HEATL see 4
c           |
c           +-HEATL see 4


c    VISKIN-+-VISDYN
c           |
c           +-RHOMOI--TVIR



      REAL FUNCTION CONDT( T )

c        Thermal conductivity of air [J m^{-1} sec^{-1} K^{-1}]

c        T=temperature [K]
c        REF.: Champion/ eq. 14.14

c     .. Scalar Arguments ..

      REAL      T
c     ..
c     .. Local Scalars ..

      REAL      AL, BE, GA
c     ..
      DATA      AL, BE, GA / 2.65019E-3, 245.4, 12. /

      CONDT  = AL * SQRT(T)**3 / ( T + BE * 10**( - GA/T ) )

      END


      REAL FUNCTION CPICE( T )

c        Specific heat of ice [J/kg/K]

c        T = temperature [K]
c        Ref.: Pruppacher/Klett, EQS. 4-84

c     .. Scalar Arguments ..

      REAL      T
c     ..

      CPICE  = 2106. + 7.327*( T - 273.15 )

      END


      REAL FUNCTION CPMOI( RMIXV )

c        Specific heat of moist air at const. press. [J/kg/K]

c        RMIXV = mixing ratio [g/g]
c        Ref.: Iribarne/Godson, Eq. IV.87

c     .. Scalar Arguments ..

      REAL      RMIXV
c     ..

      CPMOI  = 1005.7*( 1. + 0.87*RMIXV )

      END


      REAL FUNCTION CPWAT( T )

c        Specific heat of water [J/kg/K]

c        T = temperature [K]
c        Ref.: Pruppacher/Klett, EQS. 4-84

c     .. Scalar Arguments ..

      REAL      T
c     ..

      IF( T.LE.273.15 ) THEN

         CPWAT  = 4218. + 0.3471*( T - 273.15 )**2

      ELSE

         CPWAT  = 4178. + ( T - 308.15 )**2 *
     &            ( 0.01298 + 1.59E-5*( T - 308.15 )**2 )

      END IF

      END


      REAL FUNCTION DESWAT( T )

c        Temperature derivative of saturation vapor pressure
c        over water [mb], Goff-Gratch formulation

c        T=temperature  [K]
c        Ref.:  List, p. 372 (HAS SLIGHT MISPRINT)

c     .. Scalar Arguments ..

      REAL      T
c     ..
c     .. Local Scalars ..

      REAL      C1, C2, C3, C4, C5, C6, X
c     ..
c     .. External Functions ..

      REAL      ESWAT
      EXTERNAL  ESWAT
c     ..
      DATA      C1 / 6790.50 / , C2 / 5.02808 / , C3 / 4916.81 / ,
     &          C4 / 0.0303998 /
      DATA      C5 / 174209. / , C6 / 1302.884 /

      X      = C1 - C2*T + C3 * T**2 *10.**( - C4*T ) +
     &         C5 * 10**( - C6 / T )

      DESWAT = ESWAT( T ) / T**2 * X

      END


      REAL FUNCTION DESICE( T )

c        Temperature derivative of saturation vapor pressure
c        over ice [mb], Goff-Gratch formulation

c        T=temperature  [K]
c        Ref.:  List, p. 372

c     .. Scalar Arguments ..

      REAL      T
c     ..
c     .. Local Scalars ..

      REAL      C1, C2, C3, X
c     ..
c     .. External Functions ..

      REAL      ESICE
      EXTERNAL  ESICE
c     ..
      DATA      C1 / 5721.9 / , C2 / 3.56654 / , C3 / 0.0073908 /

      X      = C1 + C2*T - C3 * T**2
      DESICE = ESICE( T ) / T**2 * X

      END


      REAL FUNCTION ENTROP( P, T, RMIXV )

c      Entropy referred to triple pt. and 1013 mb ( J/kg/K )

c      P=pressure (mb)
c      T=temperature (K)
c      RMIXV=mixing ratio (g/g)
c      ( Iribarne/Godson, Eq. IV.121 )

c     .. Scalar Arguments ..

      REAL      P, RMIXV, T
c     ..
c     .. Local Scalars ..

      REAL      PS, RDCNST, TTP
c     ..
c     .. External Functions ..

      REAL      CPMOI, HEATL
      EXTERNAL  CPMOI, HEATL
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC LOG
c     ..
      DATA      RDCNST / 287.054 / , TTP / 273.15 / , PS / 1013.15 /

      ENTROP = CPMOI( RMIXV )*LOG( T ) - CPMOI( 0. )*LOG( TTP ) -
     &         RDCNST*LOG( P / PS ) + HEATL( T, 1 ) *RMIXV / T

      END


      REAL FUNCTION ENTHAL( T, RMIXV )

c        Enthalpy ( J/kg )

c        T=temperature (K)  
c        RMIXV=mixing ratio (g/g)

c     .. Scalar Arguments ..

      REAL      RMIXV, T
c     ..
c     .. External Functions ..

      REAL      CPMOI
      EXTERNAL  CPMOI
c     ..

      ENTHAL = CPMOI( RMIXV ) * T

      END


      REAL FUNCTION ENYDRY( T, RMIXV, Z )

c        Dry static energy ( J/kg )

c        T=temperature (K)
c        RMIXV=mixing ratio (g/g)  
c        z = altitude (km)

c     .. Scalar Arguments ..

      REAL      RMIXV, T, Z
c     ..
c     .. Local Scalars ..

      REAL      G
c     ..
c     .. External Functions ..

      REAL      ENTHAL
      EXTERNAL  ENTHAL
c     ..
      DATA      G / 9.8066 /

      ENYDRY = ENTHAL( T, RMIXV ) + G*( 1000.*Z )

      END


      REAL FUNCTION ENYMOI( T, RMIXV, Z )

c        Moist static energy ( J/kg )

c        T=temperature (K)
c        RMIXV=mixing ratio (g/g)  
c        z = altitude (km)

c     .. Scalar Arguments ..

      REAL      RMIXV, T, Z
c     ..
c     .. External Functions ..

      REAL      ENYDRY, HEATL, SPHUM
      EXTERNAL  ENYDRY, HEATL, SPHUM
c     ..

      ENYMOI = ENYDRY( T, RMIXV, Z ) + HEATL( T, 1 ) * SPHUM( RMIXV )

      END


      REAL FUNCTION EWAT( P, RMIXV )

c        Water vapor pressure (mb)

c        P=pressure (mb)
c        RMIXV=mixing ratio (g/g)

c     .. Scalar Arguments ..

      REAL      P, RMIXV
c     ..
c     .. Local Scalars ..

      REAL      EPS
c     ..
      DATA      EPS / 0.62198 /

      EWAT  = RMIXV*P / ( EPS + RMIXV )

      END


      subroutine eswatsub(T,es)
      real T, es
      es=eswat(T)
      return
      end

      REAL FUNCTION ESWAT( T )

c       Saturation vapor pressure over water (mb)
c       (Goff-Gratch formulation, based on exact integration of
C       Clausius-Clapeyron equation;  see List)

c       T = temperature (K)

c     .. Scalar Arguments ..

      REAL      T
c     ..
c     .. Local Scalars ..

      REAL      C1, C2, C3, C4, C5, C6, ES, RMIXV
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC LOG10
c     ..
      DATA      C1 / 7.90298 / , C2 / 5.02808 / , C3 / 1.3816E-7 / ,
     &          C4 / 11.344 /, C5 / 8.1328E-3 / , C6 / 3.49149 /


      RMIXV  = 373.16 / T

      ES     = - C1*( RMIXV - 1. ) + C2*LOG10( RMIXV ) -
     &         C3*( 10.**( C4*(1. - 1./RMIXV)) - 1. ) +
     &         C5*( 10.**( - C6*(RMIXV - 1.)) - 1. )

      ESWAT  = 1013.246 * 10.**ES

      END


      REAL FUNCTION ESICE( T )

c       Saturation vapor pressure over ice (mb)   
c       (Goff-Gratch formulation, based on exact integration of
C       Clausius-Clapeyron equation;  see List)

c       T = temperature (K)

c     .. Scalar Arguments ..

      REAL      T
c     ..
c     .. Local Scalars ..

      REAL      C1, C2, C3, TTP
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC LOG10
c     ..
      DATA      TTP / 273.15 / , C1 / 9.09718 / , C2 / 3.56654 / ,
     &          C3 / 0.876793 /

      ESICE  = 6.1071 * 10.**( ( C3 - C1/(T/TTP))*( 1.- (T/TTP)) +
     &                           C2*LOG10( T/TTP) )

      END


      REAL FUNCTION HEATL( T, ITYP )

c        Latent heats of water from empirical fits ( J/kg )
c        ( Pruppacher/Klett, Eqs. 4-85 )   

C        T = temperature (K)
c        ITYP = 1, vaporization  
C               2, fusion  
C               3, sublimation

c     .. Scalar Arguments ..

      INTEGER   ITYP
      REAL      T
c     ..

      IF( ITYP.EQ.1 ) THEN

         HEATL  = 2.5008E+6*( 273.15 / T )**( 0.167 + 3.67E-4*T )

      ELSE IF( ITYP.EQ.2 ) THEN

         HEATL  = 3.337E+5 + T*( 2031. - 10.47*T )

      ELSE IF( ITYP.EQ.3 ) THEN

         HEATL  = 2.6332E+6 + T*( 1727. - 3.625*T )

      ELSE

         CALL ERRMSG( 'HEATL--ITYP arg out of bounds',.True.)

      END IF

      END


      SUBROUTINE HYDROS( NZ, T, PZ0, IPZ, P, Z )

c      Calculates altitudes or pressures from (dry) hydrosatic equation

c     Input :   NZ  : number of levels
c              T(l) : temperature array ( l = 1 to nz ) (K)
c              IPZ  : = 0, then furnish :
c                           PZ0  : surface altitude (km)
c                           P(l) : pressure array (mb)
c                     otherwise, furnish :
c                           PZ0  : surface pressure (mb)
c                           Z(l) : altitude array (km)

c   Output :   Z(l) = altitude array (km) if  IPZ = 0
c               or
c              P(l) = pressure array (mb) if  IPZ.ne.0

c     (all arrays start at the top of the atmosphere)
c ----------------------------------------------------------------------

c     .. Scalar Arguments ..

      INTEGER   IPZ, NZ
      REAL      PZ0
c     ..
c     .. Array Arguments ..

      REAL      P( 1 ), T( 1 ), Z( 1 )
c     ..
c     .. Local Scalars ..

      INTEGER   L
      REAL      G, RDCNST, TAV
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC LOG, EXP
c     ..
c       RDCNST = gas constant for dry air
c       G = acceler. of gravity

      DATA      RDCNST / 287.054 / , G / 9.8066 /
c     ..

      IF( IPZ.NE.0 ) THEN

         P( NZ ) = PZ0

         DO 10 L = NZ - 1, 1, - 1

            TAV    = 0.5*( T(L) + T(L + 1) )
            P( L ) = P( L + 1 )*EXP( ( 1000.*G/RDCNST )*
     &               ( Z(L + 1) - Z(L)) / TAV )

   10    CONTINUE


      ELSE

         Z( NZ ) = PZ0

         DO 20 L = NZ - 1, 1, - 1

            TAV    = 0.5*( T(L) + T(L + 1) )
            Z( L ) = Z( L + 1 ) + ( 1.E-3*RDCNST / G )*TAV*
     &                              LOG( P(L + 1) / P(L) )

   20    CONTINUE

      END IF


      END


      REAL FUNCTION QSATPF( P, T )

c     Saturation mixing ratio

c     T   temperature (deg K)
c     P   total pressure (mb)

c     (uses Bolton's formula, Eqn.10 of MWR, July 1980)
c ******************************************************

c     .. Scalar Arguments ..

      REAL      P, T
c     ..
c     .. Local Scalars ..

      REAL      EST, TDC
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC EXP
c     ..

      TDC   = T - 273.15
      EST   = 6.112*EXP( 17.67*TDC / ( TDC + 243.5) )
      QSATPF  = 0.622*EST / ( P - EST )

      END


      REAL FUNCTION RHOMOI( P, T, RMIXV )

c        Atmospheric density of moist air [kg/cu m]

c        P=pressure [mb]  
c        T=temperature [K]
c        RMIXV = mixing ratio [g/g]

c     .. Scalar Arguments ..

      REAL      P, RMIXV, T
c     ..
c     .. Local Scalars ..

      REAL      RDCNST
c     ..
c     .. External Functions ..

      REAL      TVIR
      EXTERNAL  TVIR
c     ..
c           *** RDCNST = gas constant for dry air (MKS)

      DATA      RDCNST / 287.054 /

         RHOMOI = 100.*P / ( RDCNST * TVIR( T, RMIXV ) )

      END


      REAL FUNCTION RELHUM( P, T, RMIXV )

c        Relative humidity ( range: 0.0 - 1.0 )

c        P=pressure (mb)  
c        T=temperature (K)
c        RMIXV=mixing ratio (g/g)

c     .. Scalar Arguments ..

      REAL      P, RMIXV, T
c     ..
c     .. Local Scalars ..

      REAL      RSAT
c     ..
c     .. External Functions ..

      REAL      RRMIXV
      EXTERNAL  RRMIXV
c     ..

      RSAT  = RRMIXV( P, T, 1.0 )

      IF( RSAT.EQ.0.0 ) THEN

         RELHUM = 0.0

      ELSE

         RELHUM = RMIXV / RSAT

      END IF


      END


      REAL FUNCTION RRMIXV( P, T, RELH )

c        Mixing ratio [g/g]

c        P=pressure [mb]  
c        T=temperature [K]
c        RELH = relative humidity (range 0.0-1.0)

c        Note: RELH=p_v/p_{vsat} and mix rat = eps*p_v/(p-p_v)

c     .. Scalar Arguments ..

      REAL      P, RELH, T
c     ..
c     .. Local Scalars ..

      REAL      EPS, ESAT
c     ..
c     .. External Functions ..

      REAL      ESWAT
      EXTERNAL  ESWAT
c     ..
      DATA      EPS / 0.62198 /


      ESAT  = ESWAT( T )

      IF( ESAT.GE.0.616*P ) THEN

         RRMIXV = 0.0

      ELSE

         RRMIXV = RELH*EPS*ESAT / ( P - RELH*ESAT )

      END IF

      END


      REAL FUNCTION SPHUM( RMIXV )

c        Specific humidity

c        RMIXV=mixing ratio (g/g)  

c     .. Scalar Arguments ..

      REAL      RMIXV
c     ..

      SPHUM  = RMIXV / ( 1. + RMIXV )

      END


      REAL FUNCTION TDEW( P, T, RMIXV )

c        Dew-point temperature [K]

c        P=pressure [mb]  
c        T=temperature [K]
c        RMIXV=mixing ratio [g/g]
c        Ref.: Iribarne/Godson, EQ. VII.5

c     .. Scalar Arguments ..

      REAL      P, RMIXV, T
c     ..
c     .. Local Scalars ..

      REAL      RVCNST, SRAT, SRLOG
c     ..
c     .. External Functions ..

      REAL      ESWAT, EWAT, HEATL
      EXTERNAL  ESWAT, EWAT, HEATL
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC LOG
c     ..
c           *** RVCNST = gas constant for pure water vapor

      DATA      RVCNST / 461.515 /


      SRAT  = EWAT( P, RMIXV ) / ESWAT( T )

      IF( SRAT.EQ.0. ) THEN

         TDEW  = 0.0

      ELSE

         SRLOG  = LOG( SRAT )

         TDEW   = T / ( 1.- T*RVCNST / HEATL( T, 1) * SRLOG )

c                 *** iterate once to improve estimate

         TDEW   = T / ( 1.- T*RVCNST / HEATL( 0.5*(T+TDEW), 1) * SRLOG )

      END IF

      END


      REAL FUNCTION TFROST( P, T, RMIXV )

c        Frost-point temperature [K]

c        P=pressure (mb)  
c        T=temperature [K]
c        RMIXV = mixing ratio [g/g]
c        Ref.: Iribarne/Godson EQ. VII.8

c     .. Scalar Arguments ..

      REAL      P, RMIXV, T
c     ..
c     .. Local Scalars ..

      REAL      TAV, TD, TTP
c     ..
c     .. External Functions ..

      REAL      HEATL, TDEW
      EXTERNAL  HEATL, TDEW
c     ..
c         *** TTP = triple point of water [K]

      DATA      TTP / 273.15 /
c     ..

      TD  = TDEW( P, T, RMIXV )

      IF( TD.EQ.0.0 ) THEN

         TFROST  = 0.0

      ELSE

         TAV  = 0.5*( TD + TTP )
         TFROST  = TTP / ( 1.+ HEATL( TAV, 1) / HEATL( TAV, 3 ) *
     &            ( TTP/TD - 1.) )

      END IF

      END


      REAL FUNCTION TLCL( P, T, RMIXV )

c        Lifting condensation level temperature [K]

c        P=pressure [mb]  
c        T=temperature [K]
c        RMIXV=mixing ratio [g/g]
c        Ref.: Bolton, Eq. 21  (good to 0.1 K)

c     .. Scalar Arguments ..

      REAL      P, RMIXV, T
c     ..
c     .. Local Scalars ..

      REAL      E
c     ..
c     .. External Functions ..

      REAL      EWAT
      EXTERNAL  EWAT
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC LOG
c     ..

      E  = EWAT( P, RMIXV )

      IF( E.LT.1.E-30 ) THEN

         TLCL  = 1.0

      ELSE

         TLCL  = 55. + 2840. / ( 3.5*LOG( T) - LOG( E) - 4.805 )

      END IF


      END


      REAL FUNCTION TMLAPS( P, T )

c          Moist adiabatic lapse rate (deg K/km)

c          P=pressure (mb)  
c          T=temperature (K)
c          (Iribarne/Godson Eq. 28, chap. 9)

c     .. Scalar Arguments ..

      REAL      P, T
c     ..
c     .. Local Scalars ..

      REAL      EPS, ESAT, F1, G, LV, RD, CPDRY
c     ..
c     .. External Functions ..

      REAL      CPMOI, ESWAT, HEATL
      EXTERNAL  CPMOI, ESWAT, HEATL
c     ..
c           *** RD = gas constant for dry air (MKS)
c           *** G  = gravitational acceleration (MKS)

      DATA      RD / 287.05 / , G / 9.81 / , EPS / 0.622 /


      LV     = HEATL( T, 1 )
      CPDRY  = CPMOI( 0.0 )
      ESAT   = ESWAT( T )
      F1     = EPS*LV / RD*ESAT / ( T*( P - ESAT) )
      TMLAPS = 1000.*G / CPDRY*( 1.+ F1 ) /
     &         ( 1.+ EPS*LV / CPDRY*F1 / T )

      END


      REAL FUNCTION TVIR( T, RMIXV )

c        Virtual temperature [K]

c        T=temperature [K]   
c        RMIXV=mixing ratio [g/g]

c     .. Scalar Arguments ..

      REAL      RMIXV, T
c     ..
c     .. Local Scalars ..

      REAL      EPS
c     ..
      DATA      EPS / 0.62198 /

      TVIR  = T * ( 1.+ RMIXV / EPS ) / ( 1.+ RMIXV )

      END


      REAL FUNCTION TVIRL( T, RMIXV, RMIXL )

c        Virtual temperature including water content loading [K]

c        T=temperature [K]   
c        RMIXV=vapor mixing ratio [g/g]
c        RMIXL=liquid water mixing ratio [g/g]

c     .. Scalar Arguments ..

      REAL      RMIXL, RMIXV, T
c     ..
c     .. External Functions ..

      REAL      TVIR
      EXTERNAL  TVIR
c     ..

      TVIRL  = TVIR( T, RMIXV ) * (1.- RMIXL / ( 1.+ RMIXL + RMIXV ) )

      END


      REAL FUNCTION THETA( P, T )

c        Dry potential temperature [K]

c        P=pressure [mb]  
c        T=temperature [K]

c     .. Scalar Arguments ..

      REAL      P, T
c     ..
c     .. External Functions ..

      REAL      THMOI
      EXTERNAL  THMOI
c     ..

      THETA  = THMOI( P, T, 0. )

      END


      REAL FUNCTION THMOI( P, T, RMIXV )

c        Moist potential temperature [K]

c        P=pressure [mb]  
c        T=temperature [K]
c        RMIXV = vapor mixing ratio [g/g]
c        Ref.: Bolton, EQ. 7  (error up to 0.2K from neglecting
c              variation of specific heat of dry air)

c     .. Scalar Arguments ..

      REAL      P, RMIXV, T
c     ..

      THMOI  = T * ( 1000. / P )**( 0.2854*( 1.- 0.28*RMIXV ) )
      END


      REAL FUNCTION THVIR( P, T, RELH, RMIXL )

c        Virtual potential temperature [K]

c        P=pressure [mb]  
c        T=temperature [K]
c        RELH=relative humidity (range 0.-1.)
c        RMIXL=liquid mixing ratio [g/g]

c     .. Scalar Arguments ..

      REAL      P, RELH, RMIXL, T
c     ..
c     .. Local Scalars ..

      REAL      RMIXV, TV
c     ..
c     .. External Functions ..

      REAL      RRMIXV, THMOI, TVIRL
      EXTERNAL  RRMIXV, THMOI, TVIRL
c     ..

      RMIXV  = RRMIXV( P, T, RELH )
      TV     = TVIRL( T, RMIXV, RMIXL )
      THVIR  = THMOI( P, TV, 0. )

      END


      REAL FUNCTION THEQV( P, T, RELH )

c        Equivalent potential temperature [K]
c        (set RELH=1.0 if saturation e.p.t. wanted)

c        P=pressure [mb]  
c        T=temperature [K]
c        RELH=relative humidity 
c        Ref.: Bolton, Eq. 43 (accuracy: 0.3K)

c     .. Scalar Arguments ..

      REAL      P, RELH, T
c     ..
c     .. Local Scalars ..

      REAL      ARG, RMIXV
c     ..
c     .. External Functions ..

      REAL      RRMIXV, THMOI, TLCL
      EXTERNAL  RRMIXV, THMOI, TLCL
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC EXP
c     ..

      RMIXV  = RRMIXV( P, T, RELH )

      IF( RMIXV.EQ.0.0 .OR. RELH.EQ.0.0 ) THEN

         THEQV  = THMOI( P, T, 0.0 )

      ELSE

         ARG    = ( 3376./ TLCL( P, T, RMIXV) - 2.54 ) *RMIXV *
     &            ( 1.+ 0.81*RMIXV )

         THEQV  = THMOI( P, T, RMIXV ) * EXP( ARG )

      END IF

      END


      REAL FUNCTION TCONEP( P0, T0, RH0, P, RH )

c        Temperature at a desired pressure level, assuming
c        same equivalent potential temperature as at
c        a specified reference level.  Temperature a parcel
c        of air would have if moved from the reference level
c        without changing its equivalent potential temperature.

c        P0 = reference level pressure (mb)
c        T0 = reference level temperature (K)
c        RH0 = reference level relative humidity (range: 0.-1.)
c        P = pressure at desired level (mb)
c        RH = relative humidity at desired level (0.-1.)

c     .. Scalar Arguments ..

      REAL      P, P0, RH, RH0, T0
c     ..
c     .. Local Scalars ..

      INTEGER   KTR
      REAL      CONVRG, EPT0, F, F1, F2, T1, T2
c     ..
c     .. External Functions ..

      REAL      THEQV
      EXTERNAL  THEQV
c     ..
c     .. External Subroutines ..

      EXTERNAL  ERRMSG
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC ABS
c     ..
c                               ** max absolute error in result
      DATA    CONVRG / 0.01 /


      EPT0  = THEQV( P0, T0, RH0 )

c                   *** Assume solution lies between 100 and 500 K
      T1  = 100.
      T2  = 500.
      F1  = THEQV( P, T1, RH ) - EPT0
      F2  = THEQV( P, T2, RH ) - EPT0
      IF( F1*F2 .GT. 0.0 ) 
     &    CALL ERRMSG('TCONEP---cannot bracket solution',.TRUE.)

      KTR  = 0
c                    ** Regula Falsi iteration
   10 CONTINUE
      KTR  = KTR + 1

      IF( KTR.GT.2000 ) 
     &    CALL ERRMSG('TCONEP---iteration doesnt converge',.TRUE.)

      TCONEP = T1 - F1*( T2 - T1 ) / ( F2 - F1 )
      F  = THEQV( P, TCONEP, RH ) - EPT0

      IF( ABS(F).GT.CONVRG ) THEN

         IF( F.LT.0.) THEN

            T1  = TCONEP
            F1  = F

         ELSE

            T2  = TCONEP
            F2  = F

         END IF

         GO TO 10

      END IF


      END


      REAL FUNCTION TTHEQV( P, THEVAL, RELH )

c        Temperature from equivalent potential temperature (K)

c        P      - pressure [mb] 
c        THEVAL - equivalent potential temperature [K] 
c        RELH   - relative humidity (range 0.-1.)

c      Usage comments:

c        Set RELH=1. for saturation equivalent potential temperature
c        Iterative method is used (non-linear root finder)
c        Ref.: Bolton, EQ. 43  accuracy: 0.3K
c        WARNING:  It may converge but to the wrong answer !!!
c                  Change T1, T2 in that case. Seems like
c                  theta_es about 360 causes problems
c                  for T2 = TABS+200.

c     .. Parameters ..

      REAL      TABS, T1, T2, TACC
      PARAMETER ( TABS = 273.16, T1 = TABS - 200., T2 = TABS + 60.,
     &            TACC = 0.001 )
      INTEGER   MXPAR
      PARAMETER ( MXPAR = 3 )
c     ..
c     .. Scalar Arguments ..

      REAL      P, RELH, THEVAL
c     ..
c     .. Local Arrays ..

      REAL      PAR( MXPAR )
c     ..
c     .. External Functions ..

      REAL      YBRENT
      EXTERNAL  YBRENT
c     ..
c     .. External Subroutines ..

      EXTERNAL  XTHEQV
c     ..

      PAR( 1 ) = P
      PAR( 2 ) = THEVAL
      PAR( 3 ) = RELH
      TTHEQV = YBRENT( XTHEQV, PAR, MXPAR, T1, T2, TACC )

      END


      REAL FUNCTION TTHVIR( P, THVPAR, RELH, RMIXL )

c        Temperature from virtual potential temperature with
c        liquid condensed phase

c        P=pressure [mb] 
c        RELH=relative humidity (range 0.-1.)
c        RMIXL=liquid water content [g/kg]

c        Iterative method is used (non-linear root finder)
c        WARNING:  It may converge but to the wrong answer !!!

c     .. Parameters ..

      REAL      TABS, T1, T2, TACC
      PARAMETER ( TABS = 273.16, T1 = TABS - 200., T2 = TABS + 200.,
     &          TACC = 0.001 )
      INTEGER   MXPAR
      PARAMETER ( MXPAR = 4 )
c     ..
c     .. Scalar Arguments ..

      REAL      P, RELH, RMIXL, THVPAR
c     ..
c     .. Local Arrays ..

      REAL      PAR( MXPAR )
c     ..
c     .. External Functions ..

      REAL      YBRENT
      EXTERNAL  YBRENT
c     ..
c     .. External Subroutines ..

      EXTERNAL  XTHVIR
c     ..

      PAR( 1 ) = P
      PAR( 2 ) = THVPAR
      PAR( 3 ) = RMIXL
      PAR( 4 ) = RELH
      TTHVIR = YBRENT( XTHVIR, PAR, MXPAR, T1, T2, TACC )

      END


      REAL FUNCTION VISDYN( T )

c        Dynamic viscosity of air [Pa sec]=[N sec / m^2]

c        T = TEMPERATURE [K]
c        REF.: Champion/ eq. 14.12

c     .. Scalar Arguments ..

      REAL      T
c     ..
c     .. Local Scalars ..

      REAL      BETA, S
c     ..
      DATA      BETA / 1.458e-6 / , S / 110.4 /

      VISDYN = BETA * SQRT(T)**3 / ( T + S )

      END


      REAL FUNCTION VISKIN( P, T, RMIXV )

c        Kinematic viscosity of air [m^2 sec^{-1]

c        P=pressure [mb]  
c        T=temperature [K]
c        RMIXV = mixing ratio [g/g]
c        REF.: Champion/ eq. 14.13

c     .. Scalar Arguments ..

      REAL      P, RMIXV, T
c     ..
c     .. External Functions ..

      REAL      RHOMOI, VISDYN
      EXTERNAL  RHOMOI, VISDYN
c     ..

      VISKIN = VISDYN( T ) / RHOMOI( P, T, RMIXV )

      END


      REAL FUNCTION XTHVIR( T, PAR, MXPAR )

c        This function shouldn't be called by the user.
c        It is used only in solving nonlinear equations.

c     .. Scalar Arguments ..

      INTEGER   MXPAR
      REAL      T
c     ..
c     .. Array Arguments ..

      REAL      PAR( MXPAR )
c     ..
c     .. Local Scalars ..

      REAL      P, RELH, RMIXL, THVPAR
c     ..
c     .. External Functions ..

      REAL      THVIR
      EXTERNAL  THVIR
c     ..

      P      = PAR( 1 )
      THVPAR = PAR( 2 )
      RMIXL  = PAR( 3 )
      RELH   = PAR( 4 )
      XTHVIR = THVPAR - THVIR( P, T, RELH, RMIXL )

      END


      REAL FUNCTION XTHEQV( T, PAR, MXPAR )

c        This function shouldn't be called by the user.
c        It is used only in solving nonlinear equations.

c     .. Scalar Arguments ..

      INTEGER   MXPAR
      REAL      T
c     ..
c     .. Array Arguments ..

      REAL      PAR( MXPAR )
c     ..
c     .. Local Scalars ..

      REAL      P, RELH, THEVAL
c     ..
c     .. External Functions ..

      REAL      THEQV
      EXTERNAL  THEQV
c     ..

      P      = PAR( 1 )
      THEVAL = PAR( 2 )
      RELH   = PAR( 3 )
      XTHEQV = THEVAL - THEQV( P, T, RELH )

      END


      REAL FUNCTION YBRENT( FUNC, PAR, MXPAR, X1, X2, TOL )

c        (This routine shouldn't be called directly by the user.)
c        Using Brent's method, finds the zero of a function
c        FUNC(X,PAR,MXPAR) when the zero is known to lie between 
c        X1 and X2.  The root, returned as the function value,
c        is refined until its accuracy is less than TOL.

c        PAR(1:MXPAR)   array of parameters to be used by FUNC

c        This is slightly modified version of ZBRENT from
c        "Numerical recipes, 2nd edition", by W. Press et al.,  
c        Cambridge, 1992.  The modifications are:
c        (1) allowing a function FUNC with parameters in
c            addition to its argument
c        (2) replace two complex < and > tests with multiplies
c        (3) pass the code through the NAG Fortran Tools

c     Parameters are: maximum number of iterations, and machine
c     floating-point precision.  The latter is used to correct the
c     convergence criterion in case the user specifies
c     a value of TOL that is unreachable on the machine.

c     .. Parameters ..

      INTEGER   ITMAX
      REAL      EPS
      PARAMETER ( ITMAX = 100, EPS = 3.E-8 )
c     ..
c     .. Scalar Arguments ..

      INTEGER   MXPAR
      REAL      TOL, X1, X2
c     ..
c     .. Array Arguments ..

      REAL      PAR( MXPAR )
c     ..
c     .. Function Arguments ..

      REAL      FUNC
      EXTERNAL  FUNC
c     ..
c     .. Local Scalars ..

      INTEGER   ITER
      REAL      A, B, C, D, E, FA, FB, FC, P, Q, R, S, TOL1, XM
c     ..

      A = X1
      B = X2
      FA  = FUNC( A, PAR, MXPAR )
      FB  = FUNC( B, PAR, MXPAR )
      IF( FB*FA.GT.0.) 
     &    CALL ERRMSG('YBRENT--root must be bracketed',.True.)

      C = B
      FC = FB

      DO 10 ITER = 1, ITMAX

         IF( FB*FC.GT.0. ) THEN
          C = A
          FC = FA
          D = B-A
          E = D
        ENDIF

        IF( ABS(FC).LT.ABS(FB) ) THEN
          A = B
          B = C
          C = A
          FA = FB
          FB = FC
          FC = FA
        ENDIF
c                       ** Convergence check

        TOL1 = 2.*EPS*ABS(B) + 0.5*TOL
        XM = 0.5*(C-B)
        IF(ABS(XM).LE.TOL1 .OR. FB.EQ.0.)THEN
          YBRENT = B
          RETURN
        ENDIF

        IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN

c                     ** Attempt inverse quadratic interpolation
          S = FB/FA
          IF(A.EQ.C) THEN
            P = 2.*XM*S
            Q = 1.-S
          ELSE
            Q = FA/FC
            R = FB/FC
            P = S * (2.*XM*Q*(Q-R) - (B-A)*(R-1.))
            Q = (Q-1.)*(R-1.)*(S-1.)
          ENDIF

c                             ** Check whether in bounds
          IF(P.GT.0.) Q = - Q
          P = ABS(P)
          IF(2.*P .LT. MIN(3.*XM*Q-ABS(TOL1*Q),ABS(E*Q)) ) THEN

c                    ** Accept interpolation
            E = D
            D = P/Q
          ELSE
c                    ** Interpolation failed; use bisection
            D = XM
            E = D
          ENDIF

        ELSE
c                    ** Bounds decreasing too slowly; use bisection
          D = XM
          E = D
        ENDIF
c                    ** Move last best guess to A
        A = B
        FA = FB
c                    ** Evaluate new trial root

        IF(ABS(D) .GT. TOL1) THEN
          B = B+D
        ELSE
          B = B + SIGN(TOL1,XM)
        ENDIF

        FB = FUNC( B, PAR, MXPAR )

   10 CONTINUE

      CALL ERRMSG('YBRENT--exceeding maximum iterations',.FALSE.)
      YBRENT = B
      RETURN
      END


      SUBROUTINE  ERRMSG( MESSAG, FATAL )

c        Print out a warning or error message;  abort if error

      LOGICAL       FATAL, MsgLim
      CHARACTER*(*) MESSAG
      INTEGER       MaxMsg, NumMsg
      SAVE          MaxMsg, NumMsg, MsgLim
      DATA NumMsg / 0 /,  MaxMsg / 100 /,  MsgLim / .FALSE. /


      IF ( FATAL )  THEN
         WRITE ( *, '(//,2A,//)' )  ' ******* ERROR >>>>>>  ', MESSAG
         STOP
      END IF

      NumMsg = NumMsg + 1
      IF( MsgLim )  RETURN

      IF ( NumMsg.LE.MaxMsg )  THEN
         WRITE ( *, '(/,2A,/)' )  ' ******* WARNING >>>>>>  ', MESSAG
      ELSE
         WRITE ( *,99 )
         MsgLim = .True.
      ENDIF

      RETURN

   99 FORMAT( //,' >>>>>>  TOO MANY WARNING MESSAGES --  ',
     $   'They will no longer be printed  <<<<<<<', // )
      END


      SUBROUTINE MCCLA( CTYPE, Z, P, T, RVDEN, O3DEN, DEN )

c         Return McClatchey model atmospheres

c         Input: CTYPE: 'tropics', 'midsummer', 'midwinter'
c                       'subsummer', 'subwinter' (character variable)
c                       (be sure to use lowercase)

c        Z=height [m]; P=pressure [Pa]; T=temperature [K]
c        RVDEN=mixing ratio [kg/cu m]; O3DEN=ozone[kg/cu m]
c        DEN=air density [kg/cu m]

c        Ref.: R.A. McClatchey et al, 1972, AFCRL-72-0497

c     .. Parameters ..

      INTEGER   NP
      PARAMETER ( NP = 33 )
c     ..
c     .. Scalar Arguments ..

      CHARACTER CTYPE*( * )
c     ..
c     .. Array Arguments ..

      REAL      DEN( NP ), O3DEN( NP ), P( NP ), RVDEN( NP ), T( NP ),
     &          Z( NP )
c     ..
c     .. Local Scalars ..

      INTEGER   I, ITYPE
c     ..
c     .. Local Arrays ..

      REAL      ADEN( NP, 5 ), AO3DEN( NP, 5 ), AP( NP, 5 ),
     &          ARVDEN( NP, 5 ), AT( NP, 5 ), AZ( NP, 5 )
c     ..

C          TROPICAL ATMOSPHERE

      DATA ( az(i,1), ap(i,1), at(i,1), arvden(i,1), ao3den(i,1), 
     & aden(i,1), i=1,17)/
     &100.0,   0.001,  210.000,  1.630E-09,  4.300E-11,  5.000E-04,
     & 70.0,   0.058,  219.000,  2.990E-07,  8.600E-08,  9.210E-02,
     & 50.0,   0.854,  270.000,  3.580E-06,  4.300E-06,  1.101E+00,
     & 45.0,   1.590,  265.000,  6.820E-06,  1.300E-05,  2.097E+00,
     & 40.0,   3.050,  254.000,  1.360E-05,  4.100E-05,  4.181E+00,
     & 35.0,   6.000,  243.000,  2.800E-05,  9.200E-05,  8.600E+00,
     & 30.0,  12.200,  232.000,  5.950E-05,  2.400E-04,  1.831E+01,
     & 25.0,  25.700,  221.000,  1.310E-04,  3.400E-04,  4.045E+01,
     & 24.0,  30.000,  219.000,  1.550E-04,  3.400E-04,  4.763E+01,
     & 23.0,  35.000,  217.000,  1.830E-04,  3.200E-04,  5.618E+01,
     & 22.0,  40.900,  215.000,  2.160E-04,  2.800E-04,  6.645E+01,
     & 21.0,  48.000,  211.000,  2.580E-04,  2.400E-04,  7.938E+01,
     & 20.0,  56.500,  207.000,  3.090E-04,  1.900E-04,  9.515E+01,
     & 19.0,  66.600,  203.000,  3.720E-04,  1.400E-04,  1.145E+02,
     & 18.0,  78.900,  199.000,  4.490E-04,  9.000E-05,  1.382E+02,
     & 17.0,  93.700,  195.000,  5.450E-04,  6.900E-05,  1.676E+02,
     & 16.0, 111.000,  197.000,  6.370E-04,  4.700E-05,  1.972E+02 /

      DATA ( az(i,1), ap(i,1), at(i,1), arvden(i,1), ao3den(i,1), 
     & aden(i,1), i=18,33)/
     & 15.0, 132.000,  204.000,  7.570E-04,  4.700E-05,  2.260E+02,
     & 14.0, 156.000,  210.000,  9.860E-04,  4.500E-05,  2.578E+02,
     & 13.0, 182.000,  217.000,  1.790E-03,  4.500E-05,  2.929E+02,
     & 12.0, 213.000,  224.000,  6.080E-03,  4.300E-05,  3.316E+02,
     & 11.0, 247.000,  230.000,  1.790E-02,  4.100E-05,  3.740E+02,
     & 10.0, 286.000,  237.000,  4.900E-02,  3.900E-05,  4.202E+02,
     &  9.0, 329.000,  244.000,  1.210E-01,  3.900E-05,  4.708E+02,
     &  8.0, 378.000,  250.000,  2.500E-01,  3.900E-05,  5.258E+02,
     &  7.0, 432.000,  257.000,  4.710E-01,  4.100E-05,  5.855E+02,
     &  6.0, 492.000,  264.000,  8.600E-01,  4.300E-05,  6.501E+02,
     &  5.0, 559.000,  270.000,  1.530E+00,  4.500E-05,  7.199E+02,
     &  4.0, 633.000,  277.000,  2.660E+00,  4.700E-05,  7.951E+02,
     &  3.0, 715.000,  284.000,  4.700E+00,  5.100E-05,  8.756E+02,
     &  2.0, 805.000,  288.000,  9.290E+00,  5.400E-05,  9.689E+02,
     &  1.0, 904.000,  294.000,  1.300E+01,  5.600E-05,  1.064E+03,
     &  0.0,1013.000,  300.000,  1.900E+01,  5.600E-05,  1.167E+03 /

C            MIDLATITUDE SUMMER

      DATA ( az(i,2), ap(i,2), at(i,2), arvden(i,2), ao3den(i,2), 
     & aden(i,2), i=1,17)/
     &100.0,   0.001,  210.000,  2.000E-09,  4.300E-11,  5.000E-04,
     & 70.0,   0.067,  218.000,  2.680E-07,  8.600E-08,  6.706E-02,
     & 50.0,   0.951,  276.000,  3.800E-06,  4.300E-06,  9.512E-01,
     & 45.0,   1.760,  270.000,  7.030E-06,  1.300E-05,  1.757E+00,
     & 40.0,   3.330,  258.000,  1.330E-05,  4.100E-05,  3.330E+00,
     & 35.0,   6.520,  245.000,  2.610E-05,  9.200E-05,  6.519E+00,
     & 30.0,  13.200,  234.000,  5.290E-05,  2.000E-04,  1.322E+01,
     & 25.0,  27.700,  224.000,  1.720E-04,  3.000E-04,  4.288E+01,
     & 24.0,  32.200,  223.000,  2.010E-04,  3.200E-04,  5.014E+01,
     & 23.0,  37.600,  222.000,  2.350E-04,  3.400E-04,  5.867E+01,
     & 22.0,  43.700,  220.000,  2.750E-04,  3.600E-04,  6.872E+01,
     & 21.0,  51.000,  219.000,  3.220E-04,  3.600E-04,  8.056E+01,
     & 20.0,  59.500,  218.000,  3.780E-04,  3.400E-04,  9.453E+01,
     & 19.0,  69.500,  217.000,  4.440E-04,  3.200E-04,  1.110E+02,
     & 18.0,  81.200,  216.000,  5.220E-04,  2.800E-04,  1.305E+02,
     & 17.0,  95.000,  216.000,  6.140E-04,  2.400E-04,  1.535E+02,
     & 16.0, 111.000,  216.000,  7.190E-04,  2.100E-04,  1.797E+02 /

      DATA ( az(i,2), ap(i,2), at(i,2), arvden(i,2), ao3den(i,2), 
     & aden(i,2), i=18,33)/
     & 15.0, 130.000,  216.000,  8.420E-04,  1.900E-04,  2.104E+02,
     & 14.0, 153.000,  216.000,  9.950E-04,  1.800E-04,  2.464E+02,
     & 13.0, 179.000,  216.000,  1.660E-03,  1.500E-04,  2.882E+02,
     & 12.0, 209.000,  222.000,  6.460E-03,  1.200E-04,  3.269E+02,
     & 11.0, 243.000,  229.000,  2.190E-02,  1.100E-04,  3.693E+02,
     & 10.0, 281.000,  235.000,  6.430E-02,  9.000E-05,  4.159E+02,
     &  9.0, 324.000,  242.000,  1.180E-01,  8.600E-05,  4.669E+02,
     &  8.0, 372.000,  248.000,  2.100E-01,  7.900E-05,  5.225E+02,
     &  7.0, 426.000,  255.000,  3.710E-01,  7.500E-05,  5.830E+02,
     &  6.0, 487.000,  261.000,  6.090E-01,  6.900E-05,  6.487E+02,
     &  5.0, 554.000,  267.000,  1.000E+00,  6.600E-05,  7.211E+02,
     &  4.0, 628.000,  273.000,  1.890E+00,  6.400E-05,  7.998E+02,
     &  3.0, 710.000,  279.000,  3.430E+00,  6.200E-05,  8.846E+02,
     &  2.0, 802.000,  285.000,  5.850E+00,  6.000E-05,  9.757E+02,
     &  1.0, 902.000,  290.000,  9.300E+00,  6.000E-05,  1.080E+03,
     &  0.0,1013.000,  294.000,  1.400E+01,  6.000E-05,  1.191E+03 /

C            MIDLATITUDE WINTER

      DATA ( az(i,3), ap(i,3), at(i,3), arvden(i,3), ao3den(i,3), 
     & aden(i,3), i=1,17)/
     &100.0,   0.001,  210.200,  2.000E-09,  4.300E-11,  5.000E-04,
     & 70.0,   0.047,  230.700,  2.820E-07,  8.600E-08,  7.051E-02,
     & 50.0,   0.682,  265.700,  3.580E-06,  4.300E-06,  8.954E-01,
     & 45.0,   1.290,  258.500,  6.960E-06,  1.300E-05,  1.741E+00,
     & 40.0,   2.530,  243.200,  1.450E-05,  4.100E-05,  3.625E+00,
     & 35.0,   5.180,  227.800,  3.170E-05,  9.200E-05,  7.924E+00,
     & 30.0,  11.100,  217.400,  7.130E-05,  1.900E-04,  1.783E+01,
     & 25.0,  24.300,  215.200,  1.580E-04,  3.400E-04,  3.950E+01,
     & 24.0,  28.600,  215.200,  1.850E-04,  3.600E-04,  4.624E+01,
     & 23.0,  33.400,  215.200,  2.170E-04,  3.900E-04,  5.415E+01,
     & 22.0,  39.100,  215.200,  2.540E-04,  4.300E-04,  6.338E+01,
     & 21.0,  45.800,  215.200,  2.970E-04,  4.300E-04,  7.421E+01,
     & 20.0,  53.700,  215.200,  3.480E-04,  4.500E-04,  8.690E+01,
     & 19.0,  62.800,  215.200,  4.070E-04,  4.300E-04,  1.017E+02,
     & 18.0,  73.500,  215.700,  4.750E-04,  4.100E-04,  1.188E+02,
     & 17.0,  86.100,  216.200,  5.550E-04,  3.900E-04,  1.388E+02,
     & 16.0, 100.700,  216.700,  6.480E-04,  3.600E-04,  1.620E+02 /

      DATA ( az(i,3), ap(i,3), at(i,3), arvden(i,3), ao3den(i,3), 
     & aden(i,3), i=18,33)/
     & 15.0, 117.800,  217.200,  7.640E-04,  3.400E-04,  1.890E+02,
     & 14.0, 137.800,  217.700,  1.130E-03,  3.200E-04,  2.206E+02,
     & 13.0, 161.000,  218.200,  1.720E-03,  3.000E-04,  2.572E+02,
     & 12.0, 188.200,  218.700,  2.720E-03,  2.600E-04,  2.999E+02,
     & 11.0, 219.900,  219.200,  4.440E-03,  2.100E-04,  3.496E+02,
     & 10.0, 256.800,  219.700,  7.500E-03,  1.600E-04,  4.072E+02,
     &  9.0, 299.200,  225.700,  1.600E-02,  1.200E-04,  4.619E+02,
     &  8.0, 347.300,  231.700,  3.500E-02,  9.000E-05,  5.222E+02,
     &  7.0, 401.600,  237.700,  8.570E-02,  7.700E-05,  5.886E+02,
     &  6.0, 462.700,  243.700,  1.890E-01,  6.400E-05,  6.614E+02,
     &  5.0, 531.300,  249.700,  3.780E-01,  5.800E-05,  7.411E+02,
     &  4.0, 608.100,  255.700,  6.900E-01,  4.900E-05,  8.282E+02,
     &  3.0, 693.800,  261.700,  1.160E+00,  4.900E-05,  9.230E+02,
     &  2.0, 789.700,  265.200,  1.800E+00,  4.900E-05,  1.037E+03,
     &  1.0, 897.300,  268.700,  2.500E+00,  5.400E-05,  1.162E+03,
     &  0.0,1018.000,  272.200,  3.500E+00,  6.000E-05,  1.301E+03 /

C            SUBARCTIC SUMMER

      DATA ( az(i,4), ap(i,4), at(i,4), arvden(i,4), ao3den(i,4), 
     & aden(i,4), i=1,17)/
     &100.0,   0.001,  210.000,  2.000E-09,  4.300E-11,  5.000E-04,
     & 70.0,   0.071,  216.000,  2.830E-07,  8.600E-08,  7.071E-02,
     & 50.0,   0.987,  277.000,  3.950E-06,  4.300E-06,  9.868E-01,
     & 45.0,   1.810,  274.000,  7.270E-06,  1.300E-05,  1.817E+00,
     & 40.0,   3.400,  262.000,  1.360E-05,  4.100E-05,  3.404E+00,
     & 35.0,   6.610,  247.000,  2.650E-05,  9.200E-05,  6.614E+00,
     & 30.0,  13.400,  235.000,  5.350E-05,  1.400E-04,  1.338E+01,
     & 25.0,  27.800,  228.000,  1.700E-04,  2.600E-04,  4.247E+01,
     & 24.0,  32.270,  226.000,  1.990E-04,  2.800E-04,  4.963E+01,
     & 23.0,  37.500,  225.000,  2.320E-04,  3.000E-04,  5.805E+01,
     & 22.0,  43.600,  225.000,  2.700E-04,  3.200E-04,  6.750E+01,
     & 21.0,  50.700,  225.000,  3.140E-04,  3.600E-04,  7.849E+01,
     & 20.0,  58.900,  225.000,  3.650E-04,  3.900E-04,  9.128E+01,
     & 19.0,  68.600,  225.000,  4.330E-04,  4.100E-04,  1.062E+02,
     & 18.0,  79.800,  225.000,  4.940E-04,  4.100E-04,  1.235E+02,
     & 17.0,  92.800,  225.000,  5.740E-04,  3.900E-04,  1.436E+02,
     & 16.0, 108.010,  225.000,  6.680E-04,  3.400E-04,  1.671E+02 /

      DATA ( az(i,4), ap(i,4), at(i,4), arvden(i,4), ao3den(i,4), 
     & aden(i,4), i=18,33)/
     & 15.0, 125.000,  225.000,  7.770E-04,  3.200E-04,  1.943E+02,
     & 14.0, 146.000,  225.000,  1.020E-03,  2.800E-04,  2.260E+02,
     & 13.0, 170.000,  225.000,  2.060E-03,  2.600E-04,  2.630E+02,
     & 12.0, 197.700,  225.000,  4.200E-03,  2.100E-04,  3.059E+02,
     & 11.0, 230.000,  225.000,  8.560E-03,  1.800E-04,  3.559E+02,
     & 10.0, 267.700,  225.000,  1.750E-02,  1.300E-04,  4.142E+02,
     &  9.0, 310.700,  232.000,  4.180E-02,  1.100E-04,  4.663E+02,
     &  8.0, 359.000,  239.000,  1.300E-01,  7.900E-05,  5.231E+02,
     &  7.0, 413.000,  246.000,  2.900E-01,  7.500E-05,  5.849E+02,
     &  6.0, 473.000,  253.000,  5.430E-01,  7.100E-05,  6.519E+02,
     &  5.0, 541.000,  260.000,  9.730E-01,  6.400E-05,  7.244E+02,
     &  4.0, 616.000,  266.000,  1.650E+00,  6.000E-05,  8.077E+02,
     &  3.0, 700.000,  271.000,  2.690E+00,  5.800E-05,  8.985E+02,
     &  2.0, 792.900,  276.000,  4.200E+00,  5.600E-05,  9.971E+02,
     &  1.0, 896.000,  282.000,  6.000E+00,  5.400E-05,  1.110E+03,
     &  0.0,1010.000,  287.000,  9.100E+00,  4.900E-05,  1.220E+03 /

C            SUBARCTIC WINTER

      DATA ( az(i,5), ap(i,5), at(i,5), arvden(i,5), ao3den(i,5), 
     & aden(i,5), i=1,17)/
     &100.0,   0.001,  210.000,  2.000E-09,  4.300E-11,  5.000E-04,
     & 70.0,   0.040,  245.700,  2.280E-07,  8.600E-08,  5.695E-02,
     & 50.0,   0.572,  259.300,  3.070E-06,  4.300E-06,  7.682E-01,
     & 45.0,   1.113,  247.000,  6.280E-06,  1.300E-05,  1.569E+00,
     & 40.0,   2.243,  234.700,  1.330E-05,  4.100E-05,  3.330E+00,
     & 35.0,   4.701,  222.200,  2.950E-05,  9.200E-05,  7.368E+00,
     & 30.0,  10.200,  216.000,  6.580E-05,  1.500E-04,  1.645E+01,
     & 25.0,  22.560,  211.200,  1.490E-04,  3.200E-04,  3.722E+01,
     & 24.0,  26.490,  211.800,  1.740E-04,  3.600E-04,  4.358E+01,
     & 23.0,  31.090,  212.400,  2.040E-04,  4.300E-04,  5.100E+01,
     & 22.0,  36.470,  213.000,  2.390E-04,  4.700E-04,  5.966E+01,
     & 21.0,  42.770,  213.600,  2.790E-04,  5.100E-04,  6.976E+01,
     & 20.0,  50.140,  214.100,  3.260E-04,  5.600E-04,  8.155E+01,
     & 19.0,  58.750,  214.800,  3.810E-04,  6.000E-04,  9.529E+01,
     & 18.0,  68.820,  215.400,  4.450E-04,  6.200E-04,  1.113E+02,
     & 17.0,  80.580,  216.000,  5.200E-04,  6.200E-04,  1.300E+02,
     & 16.0,  94.310,  216.600,  6.070E-04,  6.200E-04,  1.517E+02 /

      DATA ( az(i,5), ap(i,5), at(i,5), arvden(i,5), ao3den(i,5), 
     & aden(i,5), i=18,33)/
     & 15.0, 110.300,  217.200,  7.330E-04,  5.600E-04,  1.770E+02,
     & 14.0, 129.100,  217.200,  1.100E-03,  4.900E-04,  2.071E+02,
     & 13.0, 151.000,  217.200,  1.670E-03,  4.700E-04,  2.422E+02,
     & 12.0, 176.600,  217.200,  2.580E-03,  4.300E-04,  2.834E+02,
     & 11.0, 206.700,  217.200,  3.790E-03,  3.200E-04,  3.315E+02,
     & 10.0, 241.800,  217.200,  5.510E-03,  2.400E-04,  3.879E+02,
     &  9.0, 282.900,  217.200,  8.370E-03,  1.600E-04,  4.538E+02,
     &  8.0, 330.800,  220.600,  1.320E-02,  9.000E-05,  5.226E+02,
     &  7.0, 385.300,  227.300,  3.290E-02,  7.100E-05,  5.904E+02,
     &  6.0, 446.700,  234.100,  9.780E-02,  4.900E-05,  6.646E+02,
     &  5.0, 515.800,  240.900,  2.340E-01,  4.700E-05,  7.457E+02,
     &  4.0, 593.200,  247.700,  4.590E-01,  4.500E-05,  8.339E+02,
     &  3.0, 679.800,  252.700,  7.470E-01,  4.300E-05,  9.366E+02,
     &  2.0, 777.500,  255.900,  1.030E+00,  4.100E-05,  1.058E+03,
     &  1.0, 887.800,  259.100,  1.200E+00,  4.100E-05,  1.193E+03,
     &  0.0,1013.000,  257.100,  1.200E+00,  4.100E-05,  1.372E+03 /


      ITYPE  = 0
      IF( CTYPE(1:4).EQ.'trop' ) ITYPE  = 1
      IF( CTYPE(1:4).EQ.'mids' ) ITYPE  = 2
      IF( CTYPE(1:4).EQ.'midw' ) ITYPE  = 3
      IF( CTYPE(1:4).EQ.'subs' ) ITYPE  = 4
      IF( CTYPE(1:4).EQ.'subw' ) ITYPE  = 5
      IF( ITYPE .EQ. 0 )
     &    CALL ERRMSG( 'MCCLA--bad input arg CTYPE',.TRUE.)

      DO 10 I = NP, 1, - 1

         Z( NP - I + 1 )     = 1000.*AZ( I, ITYPE )
         P( NP - I + 1 )     = 100.*AP( I, ITYPE )
         T( NP - I + 1 )     = AT( I, ITYPE )
         RVDEN( NP - I + 1 ) = 1.E-3 * ARVDEN( I, ITYPE )
         O3DEN( NP - I + 1 ) = 1.E-3 * AO3DEN( I, ITYPE )
         DEN( NP - I + 1 )   = 1.E-3 * ADEN( I, ITYPE )

   10 CONTINUE

      END
