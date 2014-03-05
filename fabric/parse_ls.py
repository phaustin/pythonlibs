#!/usr/bin/env python
"""
#on osx
#/usr/local/Cellar/coreutils/8.22/libexec/gnubin/ls

parses the output from:

ls -R -l -Q --time-style=full-iso --time=status ~/thedir   (file size in bytes)

and returns items as a pandas dataframe with

columnNames=['permission','links','owner','theGroup','size','date','directory','name']

"""
import re,os
import dateutil.parser as du
from pytz import timezone
import datetime as dt
from pandas import DataFrame
import cStringIO

def read_ls(listfile):
    """
       read lines from an open binary python file or itrerable
       and return a dataframe with
       columnNames=['permission','links','owner','theGroup','size','date','directory','name']
    """
    blanks=re.compile('\s+')
    stripQuotes=re.compile('.*\"(.*)\".*')
    getName=re.compile('(?P<left>.*)\"(?P<name>.*)\".*')

    columnNames=['permission','links','owner','theGroup','size','date','directory','name']
    counter=0
    fileList=[]
    for the_line in listfile:
        newline=the_line.decode('utf8')
        if (counter >= 10000) & (counter % 10000 == 0):
            print "linecount: ",counter
        newline=newline.strip()
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
                    #put the split date, time, offset back together for parsing
                    string_date=" ".join([date,time,offset])
                    date_with_tz=du.parse(string_date)
                    date_utc = date_with_tz.astimezone(timezone('UTC'))
                    timestamp=int(date_utc.strftime('%s'))
                    #columnNames=['permission','links','owner','theGroup','size','date','directory','name']
                    out=(permission,links,owner,theGroup,size,timestamp,dirname,filename)
                    record=dict(zip(columnNames,out))
                    fileList.append(record)
                    counter+=1
    df=DataFrame.from_records(fileList)                        
    return df

if __name__ == "__main__":
    from tempfile import NamedTemporaryFile as mkfile
    import textwrap

    sample_list="""
        "/Users/phil/repos/pythonlibs":
        total 4
        -rw-r--r--  1 phil staff   6 2014-03-02 21:22:34.000000000 -0800 "__init__.py"
        drwxr-xr-x 18 phil staff 612 2014-03-02 21:22:32.000000000 -0800 "bright"
        drwxr-xr-x 27 phil staff 918 2014-03-03 09:17:15.000000000 -0800 "diskinventory"
        drwxr-xr-x  7 phil staff 238 2014-03-03 09:38:52.000000000 -0800 "fabric"
        drwxr-xr-x  9 phil staff 306 2014-03-02 21:33:35.000000000 -0800 "hist2d"
        drwxr-xr-x 15 phil staff 510 2014-03-02 21:33:35.000000000 -0800 "pyutils"
        drwxr-xr-x 12 phil staff 408 2014-03-02 21:22:35.000000000 -0800 "thermo"

        "/Users/phil/repos/pythonlibs/bright":
        total 320
        -rw-r--r-- 1 phil staff      5 2014-03-02 21:22:32.000000000 -0800 "__init__.py"
        -rw-r--r-- 1 phil staff   4129 2014-03-02 21:22:32.000000000 -0800 "bright.f90"
        -rw-r--r-- 1 phil staff  11447 2014-03-02 21:22:31.000000000 -0800 "error_handler.f90"
        -rw-r--r-- 1 phil staff   6181 2014-03-02 21:22:32.000000000 -0800 "file_utility.f90"
        -rw-r--r-- 1 phil staff   8761 2014-03-02 21:22:32.000000000 -0800 "fundamental_constants.f90"
        -rw-r--r-- 1 phil staff    650 2014-03-02 21:22:32.000000000 -0800 "patest.py"
        -rw-r--r-- 1 phil staff 101497 2014-03-02 21:22:32.000000000 -0800 "planck_functions.f90"
        -rw-r--r-- 1 phil staff   1591 2014-03-02 21:22:31.000000000 -0800 "plotplanck.py"
        -rw-r--r-- 1 phil staff 127093 2014-03-02 21:22:32.000000000 -0800 "sensor_planck_functions.f90"
        -rw-r--r-- 1 phil staff   6477 2014-03-02 21:22:31.000000000 -0800 "sensor_planck_functions_test.f90"
        -rw-r--r-- 1 phil staff    520 2014-03-02 21:22:32.000000000 -0800 "setup_bright.py"
        -rw-r--r-- 1 phil staff   1913 2014-03-02 21:22:31.000000000 -0800 "test.f90"
        -rw-r--r-- 1 phil staff   1172 2014-03-02 21:22:31.000000000 -0800 "test_bright.py"
        -rw-r--r-- 1 phil staff    174 2014-03-02 21:22:31.000000000 -0800 "test_plk.py"
        -rw-r--r-- 1 phil staff    864 2014-03-02 21:22:32.000000000 -0800 "testit.py"
        -rw-r--r-- 1 phil staff   9733 2014-03-02 21:22:32.000000000 -0800 "type_kinds.f90"

        "/Users/phil/repos/pythonlibs/diskinventory":
        total 116
        -rw-r--r-- 1 phil staff  371 2014-03-02 21:33:35.000000000 -0800 "Readme.rst"
        -rwxr-xr-x 1 phil staff 3456 2014-03-02 21:22:33.000000000 -0800 "build_database.py"
        -rw-r--r-- 1 phil staff  913 2014-03-02 21:33:35.000000000 -0800 "check_disk.ipynb"
        -rw-r--r-- 1 phil staff  673 2014-03-02 21:33:35.000000000 -0800 "compstats.py"
        -rwxr-xr-x 1 phil staff 2417 2014-03-02 21:33:35.000000000 -0800 "du_disk.py"
        -rwxr-xr-x 1 phil staff 3126 2014-03-02 21:22:32.000000000 -0800 "findfiles.py"
        -rwxr-xr-x 1 phil staff 3130 2014-03-02 21:22:33.000000000 -0800 "findfiles_new.py"
        -rw-r--r-- 1 phil staff 1373 2014-03-02 21:22:33.000000000 -0800 "isascii.py"
        -rwxr-xr-x 1 phil staff 2716 2014-03-02 21:33:35.000000000 -0800 "make_ls.py"
        -rw-r--r-- 1 phil staff 6001 2014-03-03 09:17:15.000000000 -0800 "parse_files.py"
        -rwxr-xr-x 1 phil staff 7195 2014-03-02 21:22:33.000000000 -0800 "querymaker.py"
        -rwxr-xr-x 1 phil staff 3287 2014-03-02 21:33:35.000000000 -0800 "read_disk.py"
        -rwxr-xr-x 1 phil staff 1443 2014-03-02 21:33:35.000000000 -0800 "read_du_disk.py"

    """
    the_list=textwrap.dedent(sample_list)
    the_list=the_list.strip()
    listfile=cStringIO.StringIO(the_list)
    df=read_ls(listfile)    
    print df.to_string()
                    

                    
                                            
                    

                        
                        
                    


                
                                  
                    

                    
                                            
                    

                        
                        
                    


                
