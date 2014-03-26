"""
   check in every file by shell script
   example:  python initial_checkin.py /tera/phil/austinp/repos/SAM/
   produces the file git_add.sh
   run this before git init, the do bash < git_add.sh
"""

import argparse
from argparse import RawTextHelpFormatter


import os,stat,time,datetime
from collections import defaultdict
import pickle,os
import copy,site,textwrap

site.addsitedir('/home/austinp/lib/python')
from md5sum import md5sum
from walker import DirectoryStatWalker

if __name__== "__main__":
    descrip=textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(description=descrip, formatter_class=RawTextHelpFormatter)
    parser.add_argument('the_dir', type=str)
    args=parser.parse_args()

    filelist=[]
    for fileDict in DirectoryStatWalker(args.the_dir):
        if fileDict['isDir']:
            continue
        filelist.append(fileDict['fullname'])


with open('git_add.sh','w') as f:
    for the_file in filelist:
        head,tail = tuple(the_file.split(args.the_dir))
        f.write("git add -f %s\n" % tail)

