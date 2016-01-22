#!/usr/bin/env python3
"""
./read_cron.py tera .

or newtera or short or users

read the ls and du output files ls_tera.txt and du_tera.txt

write an sqllite databas called

file_tera.db

with tables 'direcs' and 'files'

example:

python3 ~/repos/pythonlibs/diskinventory/read_cron.py short .

will read the files ls_short.txt and du_sort.txt and
write the database file_short.db

"""
from __future__ import absolute_import
from __future__ import print_function

import argparse, textwrap
import subprocess,shlex

from parse_files import read_du,read_ls
import os,site
import dataset

home_dir=os.getenv('HOME')
site.addsitedir('%s/repos' % home_dir)
from pythonlibs.pyutils.silent_remove import silent_remove
from pythonlibs.diskinventory.parse_files import read_du,read_ls


linebreaks=argparse.RawTextHelpFormatter
descrip=textwrap.dedent(globals()['__doc__'])
parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
parser.add_argument('--root_dir','-r',nargs='?', type=str,default='/backupspace/stats_newroc',
                       help='path to du_root.txt ant ls_root.txt')
parser.add_argument('root',  type=str,help='name of root directory (tera, newtera or users')
parser.add_argument('outdir',type=str,help='output directory')
args=parser.parse_args()

namedict=dict(outdir=args.outdir,root=args.root,base_dir=args.root_dir)

dbname="{outdir:s}/files_{root:s}.db".format(**namedict)
dbstring='sqlite:///{:s}'.format(dbname)
du_outfile="{base_dir:s}/du_{root:s}.txt".format(**namedict)
ls_outfile="{base_dir:s}/ls_{root:s}.txt".format(**namedict)
silent_remove(dbname)
db = dataset.connect(dbstring)
print("created %s" % dbname)

table_name='direcs'
the_table = db.create_table(table_name)
the_table=db[table_name]

counter=read_du(du_outfile,the_table)
print("total lines: read from %s=%d" % (du_outfile,counter))

table_name='files'
the_table = db.create_table(table_name)
the_table=db[table_name]

counter,errlist=read_ls(ls_outfile,the_table)
print("total lines: read from %s=%d" % (ls_outfile,counter))

if len(errlist) > 0:
    errlist_name="{outdir:s}/errors_{root:s}.txt".format(**namedict)
    print('writing {}'.format(errlist_name))
    with open(errlist_name,'w') as f:
        for item in errlist:
            f.write(item)




