"""
integrate and interpolate the hydrostatic equation
for a particular atmospheric sounding
"""

from scipy.integrate.quadrature import cumtrapz
import numpy as np
import scipy.interpolate as interp
from matplotlib.cbook import iterable

class hydrostat(object):
    """
    temp (K), press (Pa)
    """
    def __init__(self,temp,press,qv,height=None):
        self.temp=temp
        self.press=press
        self.height=height
        self.qv=qv
        self.Rd=287.
        self.g=9.8
        self.rho=self.press/(self.Rd*self.temp)
        if (not iterable(self.press)) and (not iterable(self.height)):
            raise ValueError('need either height or pressure vector')
        if iterable(self.press) and (not iterable(self.height)):
            self.height=self.hydroHeight()
        else:
            self.height=height

    def hydroHeight(self):
        """integrate the hydrostatic equation to get height"""
        y= -1./(self.rho*self.g)
        theHeight=cumtrapz(y,self.press)


    def pressInterp(self):
        thePspline=interp.UnivariateSpline(self.height,np.log10(self.press))
        return thePspline

    def heightInterp(self):
        theZspline=interp.UnivariateSpline(np.log10(self.press)[::-1],
                                           self.height[::-1])
        return theZspline

    def qvInterp(self):
        """find qv at any pressure
        """
        theQvspline=interp.UnivariateSpline(np.log10(self.press)[::-1],
                                           self.qv[::-1])
        return theQvspline

        

if __name__=="__main__":
    from mcclatchey import mccla
    z,press,temp,rvden,o3den,den = mccla('midsummer',33)
    press=press
    temp=temp
    z=z
    theSounding=hydrostat(temp,press,rvden,z)
    kPress=press*1.e-3 #convert to kPa
    zkm=z*1.e-3
    import matplotlib.pyplot as plt
    fig=plt.figure(1)
    fig.clf()
    ax = fig.add_subplot(111)
    ax.semilogy(temp,kPress)
    ax.hold(True)
    pressInterpolator=theSounding.pressInterp()
    thePoints=ax.semilogy(temp,1.e-3*10**pressInterpolator(z),'g+')
    thePoints[0].set_markersize(10)
    boty,topy=ax.get_ylim()
    ax.set_ylim(102.,1.)
    #ax.set_ylim(topy,boty)
    ax.set_ylabel('pressure (kPa)')
    ax.set_title('midlatitude summer')
    fig.canvas.draw()
    ax.hold(False)
    fig2=plt.figure(2)
    fig2.clf()
    fig2.canvas.draw()
    ax2 = fig2.add_subplot(111)
    ax2.plot(temp,zkm)
    ax2.hold(True)
    heightInterpolator=theSounding.heightInterp()
    thePoints=ax2.plot(temp,1.e-3*heightInterpolator(np.log10(press)),'g+')
    thePoints[0].set_markersize(10)
    #boty,topy=ax.get_ylim()
    #ax.set_ylim(102.,1.)
    #ax.set_ylim(topy,boty)
    ax2.set_ylabel('height (km)')
    ax2.set_title('midlatitude summerIII')
    fig2.canvas.draw()
    ax2.hold(False)

    qvInterpolator=theSounding.qvInterp()
    theQv=qvInterpolator(np.log10(press))
    fig3=plt.figure(3)
    fig3.clf()
    ax3 = fig3.add_subplot(111)
    ax3.plot(rvden,press*1.e-3,'b-')
    ax3.hold(True)
    thePoints=ax3.plot(theQv,press*1.e-3,'g+')
    thePoints[0].set_markersize(10)
    #boty,topy=ax.get_ylim()
    ax3.set_ylim(102.,1.)
    #ax.set_ylim(topy,boty)
    ax3.set_ylabel('press')
    ax3.set_title('vapor density (g/m^3)')
    fig3.canvas.draw()
    ax3.hold(False)



    plt.show()
