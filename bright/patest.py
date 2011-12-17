import numpy, bright_ext as bright
import scipy.sandbox.netcdf.netcdf as NC
import satellite.channels as sc
import copy


wu = bright.wave_unit

print type(wu)

testnc=NC.NetCDFFile('testBright.cdf','r')
theRad=testnc.variables['sbdartRadiances'][:]
modisChan=31
wavelen=sc.getDict()[modisChan][1]
#wavelen=numpy.asarray(testnc.wavelen)[0]
print "calling brighttemp with wavelen=",wavelen
calcBright=bright.brighttemp(wu.WAVELENGTH_UNIT,wavelen,numpy.asarray(theRad))
print "calcBright: ",calcBright
print "getchannels: ",bright.getchannels()
calcModis=bright.modistemp(wu.WAVELENGTH_UNIT,modisChan,numpy.asarray(theRad))
print calcBright-calcModis

