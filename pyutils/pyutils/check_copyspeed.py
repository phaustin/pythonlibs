from __future__ import division
import dateutil as dt

start=['Wed Jun  4 13:28:20 PDT 2014',778]
stop=['Wed Jun  4 13:37:46 PDT 2014',792]
for i in [start,stop]:
    i[0]=dt.parser.parse(i[0])

elapsed=(stop[0] - start[0]).seconds
rate=(stop[1] - start[1])/elapsed

print("rate = {} Gybtes/minute".format(rate*60.))
    

