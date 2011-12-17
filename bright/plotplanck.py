import numpy as np
import bright
import matplotlib.pyplot as plt
from ubcplot.stdplot import simplots
from pyutils.thermconst import c,h,kb

def planckwavenum(nu,Temp):
    #input: wavnumber nu in inverse microns (vector), Temp in K (scalar)
    #output: intensity in W/ m^2*m/sr
    nu=nu*1.e6; #convert wavenumber to inverse meters
    c1=2.*h*c**2.
    c2=h*c/kb
    result=c1*nu**3./(np.exp(c2*nu/Temp) -1)
    return result

def planckwavelen(wavel,Temp):
    """input wavelength in m and Temp in K, output
    bbr in W/m^2/sr/m
    """
    c1=2.*h*c**2.
    c2=h*c/kb
    Blambda=c1/(wavel**5.*(np.exp(c2/(wavel*Temp)) -1))
    return Blambda

sigma=2.*np.pi**5*kb**4./(15*h**3.*c**2.)


microns=np.linspace(0.1,700,500000)
meters=microns*1.e-6
cms=microns*1.e-4
inv_cms=1./cms
inv_microns=1./microns
inv_meters=1/meters

temp=280.
planck_num=planckwavenum(inv_microns,temp)
planck_len=planckwavelen(meters,temp)
figfac=simplots()
ax1=figfac.singleplot()
conv=100.  #m^-1/cm^-1
ax1.plot(inv_cms,planck_num)
ax1.set_ylabel('BBR $(W\,m^{-2}\,cm\,sr^{-1})$')
ax1.set_xlim(0,2000)
integnum=np.sum(np.diff(inv_meters)*planck_num[:-1])*np.pi
integlen=np.sum(np.diff(meters)*planck_len[:-1])*np.pi
figfac.fignum=2
ax2=figfac.singleplot()
ax2.plot(microns,planck_len)
ax2.set_xlim(1,50)
plt.show()
print sigma*temp**4.
print integnum
print integlen
microns=np.array([10.,12.])
meters=microns*1.e-6
inv_microns=1/microns
planck_num=planckwavenum(inv_microns,temp)
planck_len=planckwavelen(meters,temp)
print np.mean(planck_num)*((1./10.e-4) - (1./12.e-4))
print np.mean(planck_len)*2.e-6



  
