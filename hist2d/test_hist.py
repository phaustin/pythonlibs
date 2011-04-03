from __future__ import division
import matplotlib as mpl
import sys,os
mpl.use('Agg')
from ubcplot.stdplot import simplots
from pyutils import process

import hist2d as h2d

import numpy.random as nr
import numpy as np
import numpy.ma as ma

if len(sys.argv) == 1:
    plot_dir='figures'
else:
    plot_dir=sys.argv[1]

print 'here is plot_dir: ',plot_dir

if not os.path.isdir(plot_dir):
    os.makedirs(plot_dir)


invalues={'meanx':450.,
          'stdx':50,
          'meany':-180,
          'stdy':40,
          'rho':0.8}


def makeRandom(meanx=None,stdx=None,meany=None,stdy=None,rho=None):
    nr.seed(50)
    sigma=np.array([stdx**2., rho*stdx*stdy, rho*stdx*stdy, stdy**2.])
    sigma.shape=[2,2]
    meanvec=[meanx,meany]
    outRandom=nr.multivariate_normal(meanvec,sigma,[4000,])
    chan1=outRandom[:,0]
    chan2=outRandom[:,1]
    return (chan1,chan2)

chan1,chan2=makeRandom(**invalues)

bullseye={'meanx':50.,
          'stdx':14,
          'meany':-80,
          'stdy':14,
          'rho':0.0}

chan1B,chan2B=makeRandom(**bullseye)
chan1=np.concatenate((chan1,chan1B))
chan2=np.concatenate((chan2,chan2B))

num1=100
chan1Hist=h2d.fullhist(chan1,num1,0.,650.,-9999,-8888)
num2=75
chan2Hist=h2d.fullhist(chan2,num2,-350.,0.,-9999,-8888)
#put channel 2 on the y axis (rows), so it goes in position 1
theOut=h2d.hist2D(chan2Hist['fullbins'],chan1Hist['fullbins'],chan2Hist['numBins'],\
                  chan1Hist['numBins'])
binList=theOut['indexList']
chan1List=h2d.takeValues(chan1,binList)
chan2List=h2d.takeValues(chan2,binList)
print "chan 1: "

debug=False
if debug:
    for bin,theVec in enumerate(binList):
        row,col=divmod(bin,num2)
        print "index: %d, row %d, column %d" % (bin,row,col)
        print "edges1: ",chan1Hist['edges'][row],chan1Hist['edges'][row+1]
        print "edges2: ",chan2Hist['edges'][col],chan2Hist['edges'][col+1]
        out=chan1List[bin]
        out.sort()
        print out
        out=chan2List[bin]
        out.sort()
        print out
        print '*'*60

import matplotlib.pyplot as plt

fignum=1
options={'fignum':1}
figplot=simplots(option_dict=options)
the_axis=figplot.singleplot()
#for a regular plot, chan2 is the y variable
the_axis.plot(chan1,chan2,'b+')
the_axis.set_xlabel('mean chan 1= %5.0f' % invalues['meanx'])
the_axis.set_ylabel('mean chan 2= %5.0f' % invalues['meany'])
title='raw_coverage'
the_axis.set_title(title)
figname='%s/%s.png' % (plot_dir,title)
the_axis.figure.canvas.print_figure(figname)
process.command("firefox %s" % figname)

fignum+=1
options={'fignum':fignum}
figplot=simplots(option_dict=options)
the_axis=figplot.singleplot()
print theOut['coverage'].shape
#for a countour plot chan2 is the x (row) variable
coverage=theOut['coverage']
hit=theOut['coverage'] < 1
coverage[hit]=1.
logcoverage=np.log10(coverage)
coverage=ma.masked_where(logcoverage < 1.e-2,logcoverage)
the_axis.contourf(chan1Hist['centers'],chan2Hist['centers'],logcoverage)
title='contour_coverage'
the_axis.set_title(title)
figname='%s/%s.png' % (plot_dir,title)
the_axis.figure.canvas.print_figure(figname)
process.command("firefox %s" % figname)


fignum+=1
options={'fignum':fignum}
figplot=simplots(option_dict=options)
the_axis=figplot.singleplot()
the_axis.imshow(logcoverage)
title='log_coverage'
the_axis.set_title(title)
figname='%s/%s.png' % (plot_dir,title)
the_axis.figure.canvas.print_figure(figname)
process.command("firefox %s" % figname)

