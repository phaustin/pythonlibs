cpd=1004.5     #heat capacity of dry air (j/kg/k)
cpv=1870.0     #heat capacity of water vapor (j/kg/k)
cl=4190.0      #heat capacity of liquid water (j/kg/k)
rv=461.50      #gas constant for water vapor (j/kg/k)
rd=287.04      #gas constant for dry air (j/kg/k)
lv0=2.501e6    #latent heat of vaporization at t=273.15(j/kg)
g=9.80616      #gravity acceleration (m/s2)
t0=273.16      #water triple point temperature kelvin (k)
e0=0.6112      #water triple point pressure (kpa)
p0=100         #reference pressure for potential temperature(kpa)
cpvmcl=cl-cpv  #derived constant
eps=rd/rv      #derived constant
rdocp=rd/cpd   #derived constant
epsi=1./eps    #derived constant
c=2.99792458e+08
h=6.62606876e-34
kb=1.3806503e-23
