"""
#on osx
#/usr/local/Cellar/coreutils/8.22/libexec/gnubin/ls

require an ls_list ext file generated by, for example

ls -R -l -Q --time-style=full-iso --time=status /home/phil/*  > listing.txt  (file size in bytes)

example

python readls.py ls_list.txt --db_file ls_list.db  (default)

"""

import re,os
import dateutil.parser as du
from pytz import timezone
import datetime as dt
from pandas import DataFrame, Series
import dataset, site
import argparse, textwrap

home_dir=os.getenv('HOME')
site.addsitedir('%s/repos' % home_dir)
from pythonlibs.pyutils.silent_remove import silent_remove

linebreaks=argparse.RawTextHelpFormatter
descrip=textwrap.dedent(globals()['__doc__'])
parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
parser.add_argument('filelist', nargs=1, type=str,help='filelist generated by ls')
parser.add_argument('--files_db', nargs='?', default='file_list.db',type=str,help='output filelist db')
args=parser.parse_args()

listfile=args.filelist[0]
dbname=args.files_db

dbstring='sqlite:///{:s}'.format(dbname)
silent_remove(dbname)
db = dataset.connect(dbstring)
table_name='files'
the_table = db.create_table(table_name)
the_table=db[table_name]


blanks=re.compile('\s+')
stripQuotes=re.compile('.*\"(.*)\".*')
getName=re.compile('(?P<left>.*)\"(?P<name>.*)\".*')

columnNames=['permission','links','owner','theGroup','size','date','directory','name']

counter=0
with open(listfile) as f:
    counter=0
    fileList=[]
    for the_line in f:
        if counter % 10000 == 0:
            print "linecount: ",counter
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
                    size=int(size)
                    string_date=" ".join([date,time,offset])
                    date_with_tz=du.parse(string_date)
                    date_utc = date_with_tz.astimezone(timezone('UTC'))
                    timestamp=int(date_utc.strftime('%s'))
                    #columnNames=['permission','links','owner','theGroup','size','date','directory','name']
                    out=(permission,links,owner,theGroup,size,timestamp,dirname,filename)
                    record=dict(zip(columnNames,out))
                    the_table.insert(record)                                
                    ## print string_date
                    ## print date_utc
                    ## print dt.datetime.fromtimestamp(timestamp)
                    counter+=1
print counter

                                  
                    

                    
                                            
                    

                        
                        
                    


                
