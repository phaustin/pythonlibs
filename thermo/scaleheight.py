import numpy as np
import scipy as sp
import matplotlib.pyplot as plt
import mccla as m
import matplotlib.ticker as ticks
import pdb
#f2py --overwrite-signature -m mccla -h mccla.pyf mcclatchey.f
#f2py -c mccla.pyf mcclatchey.f

g=9.8
Rd=287.

def calcScaleHeight(T,p,z):
    dz=np.diff(z)
    TLayer=(T[1:] + T[0:-1])/2.
    oneOverH=g/(Rd*TLayer)
    Zthick=z[-1] - z[0]
    oneOverHbar=np.sum(oneOverH*dz)/Zthick
#    pdb.set_trace()
    return oneOverHbar

def calcDensHeight(T,p,z):
    g=9.8
    Rd=287.
    dz=np.diff(z)
    TLayer=(T[1:] + T[0:-1])/2.
    dTdz=np.diff(T)/np.diff(z)
    oneOverH=g/(Rd*TLayer) + 1/TLayer*dTdz
    Zthick=z[-1] - z[0]
    oneOverHbar=np.sum(oneOverH*dz)/Zthick
    return oneOverHbar

 
#z,press,Temp,rvden,o3den,den = m.mccla('midsummer',33)
z,press,Temp,rvden,o3den,den = m.mccla('tropics',33)
Hbar0 = 1./calcScaleHeight(Temp,press,z)
hit=z<10000.
zL,pressL,TempL=(z[hit],press[hit],Temp[hit])
rhoL=pressL/(Rd*TempL)
Hbar1= 1./calcScaleHeight(TempL,pressL,zL)
Hrho1= 1./calcDensHeight(TempL,pressL,zL)
print "pressure scale height",Hbar1
print "density scale height",Hrho1

keepFigs={}
figNum=0
theFig=plt.figure(figNum)
keepFigs[(figNum,'tempSounding')]=theFig
theFig.clf()
theAx=theFig.add_subplot(111)
theAx.semilogy(Temp,press/100.)
theAx.invert_yaxis()
tickvals=[1000,800, 600, 400, 200, 100, 50,1]
theAx.set_yticks(tickvals)
majorFormatter = ticks.FormatStrFormatter('%d')
theAx.yaxis.set_major_formatter(majorFormatter)
theAx.set_yticklabels(tickvals)
theAx.set_ylim([1000.,5.])
theAx.set_title('Midlatitude summer profile')
theAx.set_xlabel('Temperature (K)')
theAx.set_ylabel('pressure (hPa)')

figNum+=1
theFig=plt.figure(figNum)
keepFigs[(figNum,'pressureScale')]=theFig
theFig.clf()
theAx=theFig.add_subplot(111)
hydroPress=pressL[0]*np.exp(-zL/Hbar1)
dataLine=theAx.plot(pressL/100.,zL/1000.)
hydroLine=theAx.plot(hydroPress/100.,zL/1000.)
theAx.set_title('height vs. pressure for US standard atmosphere')
theAx.set_xlabel('pressure (hPa)')
theAx.set_ylabel('height (km)')
theAx.set_xlim([500,1000])
theAx.set_ylim([0,5])
tickVals=[500, 600, 700, 800, 900, 1000]
theAx.set_xticks(tickVals)
theAx.set_xticklabels(tickVals)
theAx.legend([dataLine,hydroLine],['data','hydrostat'],loc='best')
theFig.canvas.draw()


figNum+=1
theFig=plt.figure(figNum)
keepFigs[(figNum,'densityScale')]=theFig
theFig.clf()
theAx=theFig.add_subplot(111)
hydroDens=rhoL[0]*np.exp(-zL/Hrho1)
dataLine=theAx.plot(rhoL,zL/1000.)
hydroLine=theAx.plot(hydroDens,zL/1000.)
theAx.set_title('height vs. density for US standard atmosphere')
theAx.set_xlabel('density ($kg\,m^{-3}$)')
theAx.set_ylabel('height (km)')
theAx.set_ylim([0,5])
theAx.legend([dataLine,hydroLine],['data','hydrodens'],loc='best')
theFig.canvas.draw()
plt.show()

for key,theFig in keepFigs.items():
   fignum,figName=key
   figName='./png/%s.png' % figName
   theFig.canvas.draw()
   theFig.savefig(figName)


