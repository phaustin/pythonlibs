#!/usr/bin/env python
"""
 do a ls and adu of a file tree and save the output, converting
 the output files into two sql databases

./read_disk.py /tera/phil /home/phil/ls_tera_phil


will make stdout and stderr files for two processes:

 stdout1 = /home/phil/ls_tera_phil.txt
 stderr1 = /home/phil/ls_tera_phil_err.txt
 stdout2 = /home/phil/du_tera_phil.txt
 stderr2 = /home/phil/du_tera_phil_err.txt

 and run the following system commands
 
 ls -R -l -Q --time-style=full-iso --time=status /tera/phil
 du -k /tera/phil

 if these are successful, then the two stdout files will
 be turned into sqllite databases called

 /home/phil/file_tera_phil.db

 with tables 'direcs' and 'files'
 
"""

import argparse, textwrap
import subprocess,shlex

from parse_files import read_du,read_ls
import os,site
import dataset

home_dir=os.getenv('HOME')
site.addsitedir('%s/repos' % home_dir)
from pythonlibs.pyutils.silent_remove import silent_remove


linebreaks=argparse.RawTextHelpFormatter
descrip=textwrap.dedent(globals()['__doc__'])
parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
parser.add_argument('root', nargs=1, type=str,help='top directory to list')
parser.add_argument('outfile_base', nargs=1,type=str,help='output name base')
args=parser.parse_args()

root_dict=dict(root=args.root[0])
basename=args.outfile_base[0]
head,base=os.path.split(basename)
if len(head) == 0:
    head='.'

errfile='%s/du_%s_err.txt' % (head,base)
du_outfile='%s/du_%s.txt' % (head,base)

command_string='du -k {root:s}'
command=command_string.format(**root_dict)

try:
    with open(du_outfile,'w') as stdout:
        with open(errfile,'w') as stderr:
                  subprocess.check_call(shlex.split(command),stderr=stderr,stdout=stdout)
except subprocess.CalledProcessError:
    print "du command reported some errors"
    print "command: ",command
    with open(errfile) as f:
        errout=f.readlines()
    print "err messages: "
    print " ".join(errout)

print "executing: ",command
print "stdout and stderr set to: ",du_outfile,errfile

dbname="%s/files_%s.db" % (head,base)
dbstring='sqlite:///{:s}'.format(dbname)
silent_remove(dbname)
db = dataset.connect(dbstring)
print "created %s" % dbname

table_name='direcs'
the_table = db.create_table(table_name)
the_table=db[table_name]

counter=read_du(du_outfile,the_table)
print "total lines: read from %s=%d" % (du_outfile,counter)






