#!/usr/bin/env python3
"""
./write_pandas.py tera

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
parser.add_argument('dbname',  type=str,help='path to file_tera.db')
parser.add_argument('outfile',type=str,help='path to pytables file for pandas')
args=parser.parse_args()

dbname=args.dbname
dbstring='sqlite:///{:s}'.format(dbname)
db = dataset.connect(dbstring)
print("reading %s" % dbname)





