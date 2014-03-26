"""
   write three files that will transform one directory to another by
   a series of copies, deletes, mkdirs and git checkins and removes

   example:
     python add_rm.py /tera/jdawe/bomex/SAM/SRC /home/phil/repos/sam_devel/SRC

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

#files in from_dir (6.9.5)  but missing from to_dir (6.8.2)       
upgrade_additions=savedict[from_dir].difference(savedict[to_dir])
#files in to_dir (6.8.2) but missing from from_dir (6.9.5)
upgrade_removals=savedict[to_dir].difference(savedict[from_dir])
common_files=savedict[to_dir].intersection(savedict[from_dir])

overwrite_files=[]
for the_file in common_files:
    from_sum=md5sum("%s/%s" %  (from_dir,the_file))
    to_sum=md5sum("%s/%s" %  (to_dir,the_file))
    if from_sum != to_sum:
        overwrite_files.append(the_file)

with open('remove.sh','w') as f:
    for the_file in upgrade_removals:
        f.write("git rm %s\n" % the_file)

direcs=set()
copylist=[]
for the_file in upgrade_additions:
    copycommand="cp -af {fd:s}/{fn:s} {td:s}/{fn:s}\n".format(fd=from_dir,td=to_dir,fn=the_file)
    copylist.append(copycommand)
    head,tail=os.path.split(the_file)
    if len(head) == 0:
        head='.'
    direcs.add(head)
    gitcommand="git add -f %s\n" % the_file
    copylist.append(gitcommand)

with open('copy.sh','w') as f:
    for direc in direcs:
        f.write("mkdir -p %s\n" % direc)
    for cmd in copylist:
        f.write(cmd)

with open('overwrite.sh','w') as f:
    for the_file in overwrite_files:
        copycommand="cp -af {fd:s}/{fn:s} {td:s}/{fn:s}\n".format(fd=from_dir,td=to_dir,fn=the_file)
        f.write(copycommand)
        gitcommand='git add -f %s\n' % the_file
        f.write(gitcommand)

        


