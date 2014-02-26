#!/usr/bin/env python
"""

require  du_list text file generated by for example

du -k /home/phil/* > ~/philprojects/disk_inventory/dulist.txt  file size in kbytes

example:

python readdu.py du_list.txt

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

def read_du(dufile,the_table):
    """
       read lines from dufile and transfer to
       database du
    """
    columnNames=['size','level','directory']
    counter=0
    with open(dufile,'rb') as f:
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
    return counter


if __name__ == "__main__":

    linebreaks=argparse.RawTextHelpFormatter
    descrip=textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
    parser.add_argument('dulist', nargs=1, type=str,help='filelist generated by ls')
    args=parser.parse_args()

    dufile=args.dulist[0]
    head,ext=os.path.splitext(dufile)
    dbname=head + '.db'
    dbstring='sqlite:///{:s}'.format(dbname)
    silent_remove(dbname)

    print "reading file: ",dufile
    print "writing file: ",dbname
    db = dataset.connect(dbstring)

    table_name='direcs'
    the_table = db.create_table(table_name)
    the_table=db[table_name]

    counter=read_du(dufile,the_table)
    print "total lines: ",counter


                                  
                    

                    
                                            
                    

                        
                        
                    


                
