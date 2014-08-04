from __future__ import division
from matplotlib import pyplot as plt
#plt.switch_backend('Agg') #batch
plt.switch_backend('Qt4Agg') #interactive
import numpy as np
from fastbin import do_bins,hist_2d
import numpy.random as nr
import seaborn as sns

from matplotlib import cm
from matplotlib.colors import Normalize
import matplotlib.pyplot as plt

sns.set_palette("deep", desat=.6)
sns.set_context(rc={"figure.figsize": (8, 4)})


def make_single(mean=0,sd=0.1,size=1000):
    nr.seed(50)
    outRandom=nr.normal(mean,sd,size)
    return outRandom

def calc_info(probs):
    hit = probs > 1.e-4
    vals=probs[hit]
    prob=np.sum((vals*np.log(vals)))
    return prob


if __name__=="__main__":

    firstRan=make_single(size=5000)
    fignum=1
    fig=plt.figure(fignum)
    fig.clf()
    ax1=fig.add_subplot(111)
    bin_1=do_bins(firstRan,200,-3,3,-999,-888)
    widths=np.diff(bin_1['bin_edges'])
    total=np.sum(bin_1['bin_count'])
    print "total: ",total
    p_i=bin_1['bin_count']/total
    ax1.bar(bin_1['bin_edges'][:-1],bin_1['bin_count'],widths)
    fig.canvas.draw()
    print calc_info(p_i)
    plt.show()
    
