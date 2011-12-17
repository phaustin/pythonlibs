import numpy, bright_ext as bright
import Scientific.IO.NetCDF as NC
import copy
wu = bright.wave_unit

#print bright.brighttemp.__doc__
testnc=NC.NetCDFFile('testBright.cdf','r')
theRad=testnc.variables['sbdartRadiances'][:]
theRad=numpy.asarray(theRad)
print theRad
theBright=testnc.variables['sbdartBrights'][:]
theBright=numpy.asarray(theBright)
wavelen=float(numpy.asarray(testnc.wavelen)[0])
print "wavelen: ",type(wavelen),type(12.)
#out.shape=(2,5)
print type(wavelen),type(theRad)
calcBright=bright.brighttemp(2,12.,theRad)
thechans=Numeric.zeros(100,Numeric.Int)
print thechans

print calcBright
print calcBright - theBright

#print bright.modistemp.__doc__
print bright.getchannels()
print "modistemp: start",theRad.typecode()
calcBright=bright.modistemp(wu.WAVELENGTH_UNIT,20,theRad)
print "modistemp: ",calcBright

#print bright.getchannels.__doc__


