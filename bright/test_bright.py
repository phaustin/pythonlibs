import numpy as np
import sys

sys.path.insert(0,'temp/testlib')
import bright

def calc_diff(channum):
    """
       do a roundtrip test of of the brightness temperature
       calculation for a modis channel
    """
    theTemps=np.linspace(280,300.,20.)
    #
    # calculate the radiance (W/m^2/sr/micron) given the channel
    # number and temperature (K)
    #
    theRads=bright.modisradiance(channum,theTemps)
    theRads=np.atleast_2d(theRads)
    theTemps=np.atleast_2d(theTemps)
    #
    # calculate the brightness temperature (K) given the
    # radiance (W/m^2/sr/micron) and the channel number
    #
    print "theRads: ",theRads
    newTemps=bright.modistemp(channum,theRads)
    print "newTemps: ",newTemps
    #
    # make sure the round trip is zero
    #
    np.testing.assert_array_almost_equal(theTemps,newTemps)

def test20():
    calc_diff(20)

def test29():
    calc_diff(29)

def test30():
    calc_diff(30)

def test31():
    calc_diff(31)

def test2d():
    twoD=np.array([[10.,11.],[12.,13.]],dtype=np.float32)
    print bright.modistemp(31,twoD)

if __name__=="__main__":

    print bright.get_version()
    print test31()
    test2d()
    
