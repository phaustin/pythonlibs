import mcclatchey
from hydrostat import hydrostat
import numpy as np
import pickle

infile=open('s6.pic','r')
inDict=pickle.load(infile)
infile.close()
qv=inDict['array']
qpess=inDict['pressure']
print qv.shape

z,press,temp,rvden,o3den,den = mcclatchey.mccla('subsummer',33)
theSounding=hydrostat(temp,press,z)
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
ax.set_xlabel('temperature (K)')
ax.set_title('midlatitude summer')
fig.canvas.draw()
ax.hold(False)

fig=plt.figure(2)
fig.clf()
ax = fig.add_subplot(111)
thePoints=ax.loglog(rvden*1.e3,press*1.e-3)
ax.set_ylim(102.,1.)
ax.set_xlim(1.e-5,10.)
#ax.set_ylim(topy,boty)
ax.set_ylabel('pressure (kPa)')
ax.set_xlabel('$\mathrm{q_v\ (g\ m^{-3})}$')
ax.set_title('midlatitude summer')
fig.canvas.draw()



plt.show()
