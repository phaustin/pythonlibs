"""
   write three files that will transform one directory to another by
   a series of copies, deletes, mkdirs and git checkins and removes

   example:
     python add_rm.py /tera/jdawe/bomex/SAM/SRC /home/phil/repos/sam_devel/SRC

   output:  remove.sh  copy.sh  overwrite.sh  which need to be run in to_dir

     
"""



import site,os,textwrap
from shlex import quote
from scandir import walk
site.addsitedir('{}/repos'.format(os.environ['HOME']))

from pythonlibs.pyutils.md5sum import md5sum
import fnmatch
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
        for root, dirs, files in walk(the_dir):
            #
            # store the filename with root relative to the_dir
            #
            junk,rel_root=root.split(the_dir)
            rel_root=rel_root.strip('/')
            #
            # remove .files and .the_dirs from comparison
            # along with directories starting with _
            #
            dirs[:] = [d for d in dirs if d[0] != '.']
            dirs[:] = [d for d in dirs if  d != 'html']
            dirs[:] = [d for d in dirs if  d != 'latex']
            dirs[:] = [d for d in dirs if  d != 'temp']
            dirs[:] = [d for d in dirs if  d != '_static']
            dirs[:] = [d for d in dirs if  d != '_build']
            dirs[:] = [d for d in dirs if  d != 'sphinxext']
            files = [f for f in files if f[0] != '.']
            files = [f for f in files if not f.endswith('~')]
            files = [f for f in files if not f.endswith('#')]
            files = [f for f in files if not f.endswith('pyc')]
            for the_file in files:
                filelist.append('/'.join((rel_root,the_file)))
        savedict[the_dir]=set(filelist)

    #files in from_dir (6.9.5)  but missing from to_dir (6.8.2)       
    upgrade_additions=savedict[from_dir].difference(savedict[to_dir])
    #files in to_dir (6.8.2) but missing from from_dir (6.9.5)
    upgrade_removals=savedict[to_dir].difference(savedict[from_dir])
    common_files=savedict[to_dir].intersection(savedict[from_dir])

    def endsort(filename):
        front,back=os.path.splitext(filename)
        return (back,front)
    
    overwrite_files=[]
    for the_file in common_files:
        from_sum=md5sum("%s/%s" %  (from_dir,the_file))
        to_sum=md5sum("%s/%s" %  (to_dir,the_file))
        if from_sum != to_sum:
            overwrite_files.append(the_file)

    with open('remove.sh','w') as f:
        upgrade_removals=list(upgrade_removals)
        upgrade_removals.sort(key=endsort)
        for the_file in upgrade_removals:
            f.write("git rm %s\n" % quote(the_file))

    direcs=set()
    copylist=dict(cp=[],git=[])
    for the_file in upgrade_additions:
        copycommand="cp -af ../{fd:s}/{fn:s} {fn:s}\n".format(fd=quote(from_dir),fn=quote(the_file))
        copylist['cp'].append(copycommand)
        head,tail=os.path.split(the_file)
        if len(head) == 0:
            head='.'
        direcs.add(head)
        gitcommand="git add -f %s\n" % quote(the_file)
        copylist['git'].append(gitcommand)

    with open('copy.sh','w') as f:
        header="""
        #!/bin/bash -v
        #
        #execute in the repository you want to change
        #
        """
        f.write(textwrap.dedent(header).lstrip())
        for direc in direcs:
            f.write("mkdir -p %s\n" % quote(direc))
        copylist['cp'].sort(key=endsort)
        copylist['git'].sort(key=endsort)
        for cmd in copylist['cp']:
            f.write((cmd))
        for cmd in copylist['git']:
            f.write((cmd))

    with open('overwrite.sh','w') as f:
        overwrite_files.sort(key=endsort)
        for the_file in overwrite_files:
            copycommand="cp -af ../{fd:s}/{fn:s} {fn:s}\n".format(fd=quote(from_dir),fn=quote(the_file))
            f.write((copycommand))
        for the_file in overwrite_files:
            gitcommand='git add -f %s\n' % quote(the_file)
            f.write((gitcommand))

        


