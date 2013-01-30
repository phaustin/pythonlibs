#!/usr/bin/env python

import string
import numpy as N
from math import log

import re,sys
import collections

import matplotlib.pyplot as plt
from matplotlib.pyplot import figure, show, close, getp
from pylab import randn, hist
import os
import matplotlib.colors as colors
from matplotlib.colors import normalize
from netCDF3 import Dataset as NetCDFFile
from scipy.interpolate.fitpack2 import UnivariateSpline
from matplotlib import interactive
from matplotlib.cm import ScalarMappable
from matplotlib.colors import normalize
from matplotlib.pyplot import figure, show, close, getp
import matplotlib.pyplot as plt
import numpy
import matplotlib.colors as colors
import matplotlib
import matplotlib.ticker as ticker
import sys


def readsound(soundfile):
    """
    in: text string specifying filename
    out: dictionary with sounding
    read an ascii sounding file named soundfile and return
    T (K), wv (kg/kg), p (kPa), height (m)
    into a dictionary with the keys:
    ['heightM', 'wvKgkg', 'pkPa', 'tempK']
    """
    
    sounding = open(soundfile,"r")
    #read in all the lines
    gulpIt=sounding.read()

    # break to two big groups,1st group time vars, 2nd group var
    reBreak1=re.compile('Number of Multi-Level Fields:\n')
    grp=reBreak1.split(gulpIt)

#    print grp[1]
#    sys.exit(0)
    # get pressure level from grp[0]

#    reBreak01 = re.compile('Time t(nt):\n')
#    pl = reBreak01.split(grp[0])

#    print pl
#    reBreak02 = re.compile('Pressure Levels p(np)_mb :\n')
    data =   grp[0].split(":")
#    for i in range(0, len(data)):
#        print i, data[i]


    
    nt = int(float((data[1].strip()).split('\n')[0]))
    np = int(float((data[2].strip()).split('\n')[0]))
    pdata = N.fromstring((data[3].strip()).split('T')[0], dtype=float, sep=' ')




    reBreak=re.compile(r'\n\s+([a-z\_\s+\(a-z\/a-z\)\d\:]+)\n',re.IGNORECASE)


    reVarname=re.compile('[A-Z]+',re.IGNORECASE)
        
    reBlanks=re.compile('\s+')
    

    variables=reBreak.split(grp[1])
    
    varDict={}

#    print variables

    
    for count,line in enumerate(variables):
#        print count, line

        if reVarname.match(line):

             theData=variables.pop(count+1)
             theData=theData.replace('\n',' ')
             theData=reBlanks.split(theData.strip())
             theData=[string.atof(item) for item in theData]
             varDict[line]=theData

    sounding.close()

    return nt,np,pdata, varDict

        
if __name__== '__main__':
#    theSound=readsound("test.dat")
     if len(sys.argv)<1:
          print "./readforcing_layer.py file "
          print "file: string, input layer file"
          sys.exit(0)

     file1 = string.strip(sys.argv[1])

     nt1,np1,pdata1,varDict1=readsound(file1)

     fig = figure()
     ax1=fig.add_subplot(111)

     print varDict1.keys()
#     key1 = 'omega_(mb/hour)'
#     key1 = 'Temp_(K)'
     key1 = 'H2O_Mixing_Ratio_(g/kg)'
     temp = varDict1[key1]
     temp2d = N.resize(temp,(np1,nt1))

#     Rd=287.
#     cp=1004.
##     p0=1000.
#     p0=pdata1[0]
#     g=9.8

#     print pdata1
#     theta = N.zeros((np1,nt1))
     
#     for i in range(0,nt1):

#      theta[:,i] =  temp2d[:,i] * (p0/pdata1[:])**(Rd/cp)

#     ax1.plot(theta[:,0],pdata1)
     ax1.plot(temp2d[:,0],pdata1)
#     ax1.set_xlim([297,320])
     ax1.set_ylim([1000,600])
     ax1.set_title("%s "%(key1))
     show()
     fig.savefig('theta.png')
     #sys.exit(0)

##      os.system("rm -r layplot")
##      os.mkdir("layplot")
##      i=0
##      for key in varDict1.keys():

##          i= i+1 

##          tmp1 = N.resize(varDict1[key],(np1,nt1))


##          minl = pdata1[-1]
##          maxl = pdata1[0]


         
##          vmin = N.min(tmp1)
##          vmax = N.max(tmp1)
##          print key,vmin, vmax



##          fig = figure()
##          axes1=fig.add_subplot(111)
##          if vmax> vmin:

##              inspace = ((vmax-vmin)/10)


##              print key,vmin,vmax,inspace
##              norm = colors.normalize(vmin = vmin, vmax = vmax)
##              cmap = matplotlib.cm.jet
##              cmap.set_over('salmon', 1.0)
##              cmap.set_under('black', 0.25)


##              contplot = axes1.contourf(range(0,nt1),
##                              pdata1,
##                              tmp1,
##                              numpy.arange(vmin, vmax, inspace),
##                              norm = norm,
##                              extend = 'both',
##                              cmap = cmap)


##              axes1.yaxis.tick_left()
##              axes1.set_yscale('log')

    
##              # Label the axes and title
##              axes1.set_xlabel('Time(step)')
##              axes1.set_ylabel('Pressure (mbar)')
         
##              # make a colorbar
##              colorbarloc = [.89, .1, .025, .8]
##              colorbaraxes = fig.add_axes(colorbarloc)
##              fig.colorbar(contplot, colorbaraxes)
         
## #             if pv.var1_units:
##              axes1.set_title("%s " % (key))
##              axes1.set_ylim([1000,100])

## #             else:
## #                 axes1.set_title("%s" % (key))

##          else:
##               plt.plot(pdata1,tmp1[:,0])
##               axes1.set_ylim([1000,100])
       
##          var = key.split('/')

## #         print key, var[0]

##          fig.savefig('layplot/'+var[0]+'.png')

outDict={}
import pickle
theTemp=varDict1['Temp_(K)']
theTemp=N.resize(theTemp,(25,800))
theTemp=theTemp[:,0]
theQv=temp2d[:,0]
rho=pdata1*100./(287.*theTemp)
newQv=theQv*rho  #convert from g/kg to g/m^3
outDict['theTemp']=theTemp
outDict['newQv']=newQv
outDict['rho']=rho
outDict['pressure']=pdata1
outfile=open('s6.pic','w')
pickle.dump(outDict,outfile)
outfile.close()
    

#         del(fig)

#         show()

#         sys.exit(0)

         





