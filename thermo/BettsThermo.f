c *********************************************************************
c
c            Alan Betts thermodynamic subroutines
c            (with further work by Bill Ridgway)
c
c     With these routines in hand, you need no longer be
c     intimidated by atmospheric thermodynamics a la Betts!
c
c     routine      variable  >>>>   (i=input, o=output, i/o=both)
c     -------      --------
c                  p   t   q   psp  tsp  qsp  thsp  thesp thpt thes
c    --------------------------------------------------------------
c
c      ptqv        i   i   i    o    o    o     o     o
c
c      ptpsp       i   i   o    i    o    o     o     o     o    o
c
c      thinv       i   o   -    -    -    -     i     -
c
c      theinv      i  i/o  -    -    -    o     o     i
c
c      qsat        -   -   -    i    i    o     -     -
c
c      satpnt      i   i   i    o    o    -     -     -
c
c    --------------------------------------------------------------
c
c            p      = pressure
c            t      = temperature
c            q      = specific humidity
c
c            psp    = saturation level pressure
c            tsp    = saturation level temperature
c            qsp    = saturation specific humidity
c
c            thsp   = potential temperature at s.p.
c            thesp  = equivalent potential temperature at s.p.
c
c *******************************************************************


      SUBROUTINE PTQV( P, T, QV, PSP, TSP, QSP, THSP, THESP )

c        Computes all of the thermodynamic quantities
c        for the saturation point defined by (p,t,qv).
c
c        Input
c                  P   = pressure
c                  T   = temperature
c                  QV  = spec. humidity
c
c        output
c                  PSP   = sat. point pressure
c                  TSP   = sat. point temperature
c                  THSP  = potential temp. (theta)
c                  THESP = equiv. potential temp.(theta-e)

c *********************************************************************

c     .. Scalar Arguments ..

      REAL      P, PSP, QSP, QV, T, THESP, THSP, TSP
c     ..
c     .. Local Scalars ..

      REAL      ROCP
c     ..
c     .. External Subroutines ..

      EXTERNAL  SATPNT
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC EXP
c     ..
c                       ratio RGAS / CP where gas constant RGAS=287.04
c                       and specific heat of air CP=1005.7

      DATA      ROCP / 0.2854 /

      write(6, *) " ptqv: ",p,T,qv 

      CALL SATPNT( P, T, QV, PSP, TSP )

      QSP    = QV
      THSP   = TSP * ( 1000. / PSP )**ROCP
      THESP  = THSP * EXP( 2670.*QSP / TSP )

      END


      SUBROUTINE PTPSP( P, T, QV, PSP, TSP, QSP, THPT, THES, THSP,
     &                  THESP )

c        Computes all of the thermodynamic quantities
c        for the saturation point defined by (p,t,psp).

c        Input
c                P   = pressure
c                T   = temperature
c                PSP = sat. point pressure
c
c        Output
c                QV  = spec. humidity
c                TSP = sat. point temperature
c                QSP = sat. pt. spec. humidity (total water)
c                THPT= potential temp. (theta(p,t)
c                THES= potential temp. (theta sub es)
c                THSP= potential temp. (theta(psl,tsl)
c                THESP=equiv. potential temp.(theta-e at sl)
c
c *********************************************************************

c     .. Scalar Arguments ..

      REAL      P, PSP, QSP, QV, T, THES, THESP, THPT, THSP, TSP
c     ..
c     .. Local Scalars ..

      REAL      ROCP
c     ..
c     .. External Functions ..

      REAL      QSAT
      EXTERNAL  QSAT
c     ..
c     .. External Subroutines ..

      EXTERNAL  THEINV
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC EXP
c     ..
c                       ratio RGAS / CP where gas constant RGAS=287.04
c                       and specific heat of air CP=1005.7

      DATA      ROCP / 0.2854 /


      IF( PSP.LT.0. ) STOP 'ptpsp'

      IF( P.GE.PSP ) THEN
c                            unsaturated conditions, p.gt.psp

         THPT   = T*( 1000. / P )**ROCP
         THSP   = THPT
         TSP    = THSP*( PSP / 1000. )**ROCP
         QSP    = QSAT( PSP, TSP )
         QV     = QSP
         THES   = THPT*EXP( 2670.*QSAT( P, T) / T )
         THESP  = THSP*EXP( 2670.*QSP / TSP )


      ELSE
c                            saturated, cloud conditions, p.lt.psp

         QV     = QSAT( P, T )
         THPT   = T*( 1000. / P )**ROCP
         THES   = THPT*EXP( 2670.*QV / T )
         THESP  = THES
         TSP    = T

         CALL THEINV( PSP, TSP, QSP, THSP, THESP )

         THSP  = TSP*( 1000. / PSP )**ROCP

      END IF


      END


      SUBROUTINE THINV( P, T, TH )

c        Returns the temperature at pressure
c        level P, based on the potential temperature TH.
c
c        Input   : P  = pressure
c                : TH  = potential temperature (theta)
c
c        Output  : T  = temperature
c
c ********************************************************************

c     .. Scalar Arguments ..

      REAL      P, T, TH
c     ..
c     .. Local Scalars ..

      REAL      ROCP
c     ..
c                       ratio RGAS / CP where gas constant RGAS=287.04
c                       and specific heat of air CP=1005.7

      DATA      ROCP / 0.2854 /


      T  = TH*( P / 1000. )**ROCP

      END


      SUBROUTINE THEINV( P, T, Q, TH, THE )

c        Input   : P    = pressure
c                : T    = temperature (first guess)
c                : THE  = equiv. pot. temp. (theta-e)
c
c        Output  : T    = temperature
c                : Q    = saturation specific humidity
c                : TH   = potential temp. (theta)
c
c ********************************************************************

c     .. Scalar Arguments ..

      REAL      P, Q, T, TH, THE
c     ..
c     .. Local Scalars ..

      INTEGER   KOUNT
      REAL      PFCTR, Q2, ROCP, TGESS1, TGESS2, TH2, THE1, THE2
c     ..
c     .. External Functions ..

      REAL      QSAT
      EXTERNAL  QSAT
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC ABS, EXP
c     ..
c                       ratio RGAS / CP where gas constant RGAS=287.04
c                       and specific heat of air CP=1005.7

      DATA      ROCP / 0.2854 /


      PFCTR  = ( 1000. / P )**ROCP
      KOUNT  = 0

   10 CONTINUE

c  use initial T as first guess for the solution.

      TGESS1 = T
      TH     = TGESS1*PFCTR
      Q      = QSAT( P, TGESS1 )
      THE1   = TH*EXP( 2670.*Q / TGESS1 )

      IF( ABS( THE1 - THE).LT.0.01 ) RETURN

c  also use T-1 to get sensitivity.

      TGESS2 = T - 1.
      TH2    = TGESS2*PFCTR
      Q2     = QSAT( P, TGESS2 )
      THE2   = TH2*EXP( 2670.*Q2 / TGESS2 )

c  make linear adjustment to select the next guess for T.

      T      = T + ( THE1 - THE ) / ( THE2 - THE1 )
      KOUNT  = KOUNT + 1

      IF( KOUNT.LE.20 ) GO TO 10

      WRITE( 6, '(//,A)' ) ' NOTE *** iteration failed in theinv'

      END

      subroutine qsatsub(p, T, qsatval)
      real p, T, qsatval
      qsatval=qsat(p, T)
      return
      end

      REAL FUNCTION QSAT( P, T )

c     T        temperature (deg k)
c     P        pressure (mb)
c     QSAT     saturation mixing ratio (assuming p = p(total))

c     (now uses Bolton's formula, Eqn.10 of MWR, July 1980)
c ********************************************************************

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
      QSAT  = 0.622*EST / ( P - EST )

      END


      SUBROUTINE SATPNT( ZPK, ZTK, ZQK, ZPSL, ZTSL )

c     computes a saturation point by iteration along a dry adiabat

c        Input   : ZPK = pressure
c                : ZTK = temperature
c                : ZQK = specific humidity
c
c        Output  : ZPSL = pressure    at saturation level
c                : ZTSL = temperature at saturation level
c
c        functions needed

c            QSAT: saturation specific humidity

c *********************************************************************

c     .. Scalar Arguments ..

      REAL      ZPK, ZPSL, ZQK, ZTK, ZTSL
c     ..
c     .. Local Scalars ..

      INTEGER   KOUNT
      REAL      ROCP, ZDELPL, ZITH, ZPRAT, ZPSM, ZPTEST,
     &          ZQL, ZQM, ZRT, ZTSM
c     ..
c     .. External Functions ..

      REAL      QSAT
      EXTERNAL  QSAT
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC ABS
c     ..
c                       ratio RGAS / CP where gas constant RGAS=287.04
c                       and specific heat of air CP=1005.7

      DATA      ROCP / 0.2854 /


      KOUNT  = 0

      IF( ZQK.LT.0.000002 .AND. ZPK.GT.100.0 ) ZQK  = 0.000002

      ZPSL   = ZPK*0.98
      ZPTEST = ZPK*0.2
      ZPRAT  = 0.998
      ZRT    = 0.9994283
      ZITH   = ZTK*( 1. / ZPK )**ROCP

   10 CONTINUE

      ZTSL  = ZITH*ZPSL**ROCP
      ZQL  = QSAT( ZPSL, ZTSL )
      ZTSM  = ZTSL*ZRT
      ZPSM  = ZPSL*ZPRAT
      ZQM  = QSAT( ZPSM, ZTSM )

      ZDELPL = ( ZPSL - ZPSM )*( ZQL - ZQK ) / ( ZQL - ZQM )

      IF( ABS( ZDELPL).LT.0.1 ) RETURN

      ZPSL  = ZPSL - ZDELPL

      IF( ZPSL.LT.ZPTEST ) GO TO 30

      KOUNT  = KOUNT + 1

      IF( KOUNT.GT.20 ) GO TO 20

      GO TO 10


   20 CONTINUE
      WRITE( *, '(//,A)' ) ' Iteration failed in satpnt'

   30 CONTINUE
      ZPSL  = ZPSL + ZDELPL

      END


      REAL FUNCTION ATMDEN( P, T, R )

c     ATMDEN   :   atmospheric density (g/cu m)
c     P        :   total pressure (mb)
c     T        :   temperature (K)
c     R        :   dimensionless water vapor mass mixing ratio (g/g)
c ********************************************************************

c     .. Scalar Arguments ..

      REAL      P, R, T
c     ..
c     .. Local Scalars ..

      REAL      EPS, RD, VIRTEM
c     ..
      DATA      RD / 287.05 / , EPS / 0.622 /


      VIRTEM = T*( 1. + R / EPS ) / ( 1. + R )
      ATMDEN = 1.e+5*P / ( RD*VIRTEM )

      END


      SUBROUTINE MOIST( THE, P1, P2, P, T, TH, N )

c       Compute temperatures along a moist adiabat

c ...Input
c ......THE   : value of theta-e which defines moist adiabat
c ......P1,P2 : pressure limits (mb) for computing T,TH
c ......P(N)  : pressure array
c ......N     : number of values in arrays P,T,TH

c ...output
c ......T(N)  : temperatures corresponding to moist adiabat
c ......TH(N) : potential temperature (theta)
c ********************************************************************

c     .. Scalar Arguments ..

      INTEGER   N
      REAL      P1, P2, THE
c     ..
c     .. Array Arguments ..

      REAL      P( N ), T( N ), TH( N )
c     ..
c     .. Local Scalars ..

      INTEGER   J
      REAL      QDUM, TGUESS
c     ..
c     .. External Subroutines ..

      EXTERNAL  THEINV
c     ..


c ...use iterative procedure based on theinv routine

      TGUESS = 290.

      DO 10 J = N, 1, - 1

         IF( P1.GE.P( J) .AND. P( J).GE.P2 ) THEN

            CALL THEINV( P( J), TGUESS, QDUM, TH( J), THE )

            T( J ) = TGUESS

         END IF

   10 CONTINUE


      END
