import bright as b
import numpy as np
binplk=b.plkavg(1400.,1401.,280.)  #W/m^2/sr
pointplk=b.planckfreq(np.array([1400.]),280.) #mW/m^2/sr/cm^-1
print binplk,pointplk*0.001
