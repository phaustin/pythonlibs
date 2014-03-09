#!/usr/bin/env python
"""
  read a file generated with

  ls -R -l -Q --time-style=full-iso --time=status targetdir > listfile

  and save the output as a 

  columnNames=['permission','links','owner','theGroup','size','date','directory','name']

  examples:

    write_filelist.py the_list.txt tera_files.db

"""
import textwrap,subprocess,shlex
import os, cStringIO
from parse_ls import read_ls
import dataset
import argparse

import os,errno

def silent_remove(filename):
    print("{:s} will be destroyed if it exists".format(filename))
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT: # errno.ENOENT = no such file or directory
            raise # re-raise exception if a different error occured

def write_database(filename,dframe):
    dbstring='sqlite:///{:s}'.format(filename)
    silent_remove(filename)
    db = dataset.connect(dbstring)
    the_table = db.create_table('files')
    for i,item in dframe.iterrows():
        the_table.insert(dict(item))    
    return db

if __name__=="__main__":
    #write_filelist.py  grexhome /home/paustin/repos/dotfiles dotfiles.db
    linebreaks=argparse.RawTextHelpFormatter
    descrip=textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
    parser.add_argument('ls_file', nargs=1,type=str,help='text file to read')
    parser.add_argument('database', nargs=1,type=str,help='database name (with or without path)')
    args=parser.parse_args()

    listfile=args.ls_file[0]
    database=args.database[0]
    with open(listfile,'rb') as f:
        df=read_ls(f)
    write_database(database,df)
    
            
            
    
    
