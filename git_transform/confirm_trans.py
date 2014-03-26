"""
   write three files that will transform one directory to another by
   a series of copies, deletes, mkdirs and git checkins and removes

   example:
     python confrim_trans.py /tera/jdawe/bomex/SAM/SRC /home/phil/repos/sam_devel/SRC

   output:  remove.sh  copy.sh  overwrite.sh  which need to be run in to_dir

     
"""


import os,stat,time,datetime,site
site.addsitedir('/home/phil/lib/python')

from md5sum import md5sum
from collections import defaultdict
import pickle
import os
import copy

if __name__== "__main__":

    import argparse,sys,textwrap
    from argparse import RawTextHelpFormatter

    descrip=textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(description=descrip, formatter_class=RawTextHelpFormatter)
    parser.add_argument('from_dir', nargs=1, type=str)
    parser.add_argument('to_dir', nargs=1, type=str)
    args=parser.parse_args()
    from_dir=args.from_dir[0]
    to_dir= args.to_dir[0]
    savedict={}
    for the_dir in (from_dir,to_dir):
        filelist=[]
        for root, dirs, files in os.walk(the_dir):
            #
            # remove .files and .dirs from comparison
            #
            files = [f for f in files if not f[0] == '.']
            dirs[:] = [d for d in dirs if not d[0] == '.']
            junk,dir_path = root.split(the_dir)
            dir_path=dir_path.strip('/')
            if len(dir_path) == 0:
                dir_path='.'
            for the_file in files:
                filelist.append('/'.join((dir_path,the_file)))
        savedict[the_dir]=set(filelist)
        
upgrade_removals=(savedict[from_dir] - savedict[to_dir])
upgrade_additions=(savedict[to_dir] - savedict[from_dir])
common_files=savedict[to_dir].intersection(savedict[from_dir])

print "upgrade_removals should be empty: ",upgrade_removals
print "upgrade_additions should be empty: ",upgrade_additions

for the_file in common_files:
    try:
        from_sum=md5sum("%s/%s" %  (from_dir,the_file))
    except Exception,e:
        print "caught from  exception: ",e
        continue
    try:
        to_sum=md5sum("%s/%s" %  (to_dir,the_file))
    except Exception,e:
        print "caught to exeption: ",e
        continue
    if from_sum != to_sum:
        print "md5sums don't match for %s" % the_file
