#on osx
#/usr/local/Cellar/coreutils/8.22/libexec/gnubin/ls
#ls -R -l -Q --time-style=full-iso --time=status /home/phil/*  > listing.txt
#-rw-r--r--    1 phil users         0 2005-10-06 12:28:09.000000000 -0700 "/home/phil/eosc211_fall2005.txt~"
#du /home/phil/* > ~/philprojects/disk_inventory/dulist.txt

#"/Users/phil/www.physics.mcgill.ca/~gang/ftp.transfer":
#total 0
#drwxr-xr-x 3 phil staff 102 2014-02-09 16:07:09.000000000 -0800 "final.book.figs.Feb.2012.pdf"

import re,os
import dateutil as du
from pytz import timezone
import datetime as dt

blanks=re.compile('\s+')
stripQuotes=re.compile('.*\"(.*)\".*')
getName=re.compile('(?P<left>.*)\"(?P<name>.*)\".*')

listfile='/Users/phil/listing.txt'
columnNames=['permission','links','owner','theGroup','size','date','time','directory','name']

with open(listfile) as f:
    fileList=[]
    for the_line in f:
        newline=the_line.strip()
        if len(newline)>0:
            if newline[-1]==":":
                #"/Users/phil/www.physics.mcgill.ca/~gang/ftp.transfer":
                dirname=stripQuotes.match(newline)
                dirname=dirname.group(1)
                continue
            else:
                test=getName.match(newline)
                #-rw-r--r--    1 phil users         0 2005-10-06 12:28:09.000000000 -0700 "/home/phil/eosc211_fall2005.txt~"
                if test:
                    #
                    # skip directories and symbolic links
                    #
                    if test.group("left")[0] == 'd' or test.group("left")[0] == 'l':
                        continue
                    #
                    # check for a path name like /home/phil/eosc211_fall2005.txt
                    #
                    root,filename=os.path.split(test.group("name"))
                    if len(root) > 0:
                        dirname=root
                    else:
                        #
                        # we've got a plain file name
                        #
                        filename=test.group("name")
                    permission,links,owner,theGroup,size,date,time,offset =\
                            blanks.split(test.group("left").strip())
                    string_date=" ".join([date,time,offset])
                    date_with_tz=du.parser.parse(string_date)
                    date_utc = date_with_tz.astimezone(timezone('UTC'))
                    timestamp=int(date_utc.strftime('%s'))
                    ## print string_date
                    ## print date_utc
                    ## print dt.datetime.fromtimestamp(timestamp)
                    print newline
                    print dirname
                    print filename
                                  
                    

                    
                                            
                    

                        
                        
                    


                
