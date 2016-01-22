from __future__ import print_function, unicode_literals
import site,os
home_dir=os.getenv('HOME')
site.addsitedir('{}/repos/pythonlibs'.format(home_dir))

import fabric.parse_ls
reload(fabric.parse_ls)
from fabric.parse_ls import read_ls
with open('/backupspace/stats_newroc/ls_newtera.txt','r') as f:
    dbname='ls_newtera.db'
    the_df=read_ls(f,dbname)
    
