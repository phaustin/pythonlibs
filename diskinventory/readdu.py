#on osx
#/usr/local/Cellar/coreutils/8.22/libexec/gnubin/ls
#ls -R -l -Q --time-style=full-iso --time=status /home/phil/*  > listing.txt  file size in bytes
#-rw-r--r--    1 phil users         0 2005-10-06 12:28:09.000000000 -0700 "/home/phil/eosc211_fall2005.txt~"
#du -k /home/phil/* > ~/philprojects/disk_inventory/dulist.txt  file size in kbytes

#"/Users/phil/www.physics.mcgill.ca/~gang/ftp.transfer":
#total 0
#drwxr-xr-x 3 phil staff 102 2014-02-09 16:07:09.000000000 -0800 "final.book.figs.Feb.2012.pdf"

import re,os
import dateutil.parser as du
from pytz import timezone
import datetime as dt
from pandas import DataFrame, Series
import dataset, site
site.addsitedir('/Users/phil/repos/pythonlibs')
from pyutils.silent_remove import silent_remove


dbname='dulist.db'
dbstring='sqlite:///{:s}'.format(dbname)
silent_remove(dbname)
db = dataset.connect(dbstring)
table_name='direcs'
the_table = db.create_table(table_name)
the_table=db[table_name]


listfile='/Users/phil/dulist.txt'
columnNames=['size','level','directory']

counter=0
with open(listfile,'rb') as f:
    counter=0
    for the_line in f:
        if counter % 10000 == 0:
            print "linecount: ",counter
        newline=the_line.decode('utf8')
        newline=newline.strip()
        size,direc=newline.split('\t',1)
        size=int(size)
        level=direc.count('/')
        out=(size,level,direc)
        record=dict(zip(columnNames,out))
        the_table.insert(record)                                
        counter+=1
print counter

                                  
                    

                    
                                            
                    

                        
                        
                    


                
