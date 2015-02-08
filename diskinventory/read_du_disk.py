#!/usr/bin/env python3
"""

convert a du -k file into a database

./read_du_disk.py /backups/du_tera.txt /home/phil/repos/pythonlibs/diskinventory

will read the cronjob-produced du -k file du_tera.txt
and write an sqllite3 database with table direcs to
/home/phil/repos/diskinventory/du_tera_root.db
 
"""
from __future__ import absolute_import
from __future__ import print_function

import argparse, textwrap
import subprocess,shlex

from .parse_files import read_du,read_ls
import os,site
import dataset

home_dir=os.getenv('HOME')
site.addsitedir('%s/repos' % home_dir)
from pythonlibs.pyutils.silent_remove import silent_remove


linebreaks=argparse.RawTextHelpFormatter
descrip=textwrap.dedent(globals()['__doc__'])
parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
parser.add_argument('root_du', nargs=1, type=str,help='du text file produced by cronjob')
parser.add_argument('outfile_base', nargs=1,type=str,help='destination directory for database')
args=parser.parse_args()

du_fullname=args.root_du[0]
head,filename=os.path.split(du_fullname)
root,ext=os.path.splitext(filename)
dbname="%s/%s_root.db" % (args.outfile_base[0],root)
print("reading from: ",du_fullname)
print("writing to: ",dbname)
dbstring='sqlite:///{:s}'.format(dbname)
silent_remove(dbname)
db = dataset.connect(dbstring)

table_name='direcs'
the_table = db.create_table(table_name)
the_table=db[table_name]

counter=read_du(du_fulloname,the_table)
print("total lines: read from %s=%d" % (du_fullname,counter))




