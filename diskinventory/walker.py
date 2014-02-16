import os, site, stat, pwd, errno, dataset
site.addsitedir('/Users/phil/repos/pythonlibs')
from pyutils.md5sum import md5sum
from datetime import datetime
from pandas import DataFrame, Series


def silentremove(filename):
    print("{:s} will be destroyed if it exists".format(filename))
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT: # errno.ENOENT = no such file or directory
            raise # re-raise exception if a different error occured

dbname='dirlist.db'
dbstring='sqlite:///{:s}'.format(dbname)
silentremove(dbname)
db = dataset.connect(dbstring)

table_name='files'
primary_id='fullpath'
the_table = db.create_table(table_name)
the_table=db[table_name]

base='/Users/phil/repos/pythonlibs'
for root, dirs, files in os.walk(base):
    files = [f for f in files if not f[0] == '.']
    dirs[:] = [d for d in dirs if not d[0] == '.']
    if len(dirs) > 0:
        print root
    path = root.split('/')
    for filename in files:
        fullpath='/'.join([root,filename])
        if os.path.islink(fullpath):
            continue
        fullStats = os.stat(fullpath)
        uid = fullStats[stat.ST_UID]
        uname=pwd.getpwuid(uid)
        timestamp=fullStats[stat.ST_MTIME]
        the_date=datetime.fromtimestamp(timestamp)
        the_table.insert(dict(fullpath=fullpath,date=the_date,owner=uname.pw_name,size=os.path.getsize(fullpath),
                              md5=md5sum(fullpath)))
        
        
