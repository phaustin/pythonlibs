#!/usr/bin/env python

"""
Script to update the svn snapshot of the current filelist.

Usage:     update_svn.py <options> [target_dir]

Arguments:
    target_dir    Location of svn checkout, defaults to current dir

Options:
    -h, --help    Print command help
    -t, --time    Output timing infomation
    --nocopy      Don't perform rsync operations
    --nosvn       Don't perform svn operations
"""

import os, os.path, sys, shutil, glob, getopt
import time

def update_svn():
    os.system("svn -q update")

###########

def commit_svn():
    os.system('svn -q commit -m "File listings update"')

##########

def rsync_txt_files():
    "Copies inventory files to current directory."

    os.system('rsync -aq /home/phil/lib/diskInventory/*.txt .')
    os.system('rsync -aq /nfs/kite/users/misc/*.txt .')

##########

def txt_to_csv(timing):
    "Turn txt files into csv."

    filelist = glob.glob('*_ls.txt')
    for item in filelist:
        time1 = time.clock()
        head, tail = os.path.split(item)
        server, partition = tail.split('_')[:2]
        build_ls(item, server, partition)
        time2 = time.clock()
        if timing: print "%s: %s seconds" % (tail, round(time2-time1, 3))

    filelist = glob.glob('*_du.txt')
    for item in filelist:
        time1 = time.clock()
        head, tail = os.path.split(item)
        server, partition = tail.split('_')[:2]
        build_du(item, server, partition)
        time2 = time.clock()
        if timing: print "%s: %s seconds" % (tail, round(time2-time1, 3))


##############

def build_ls(filename, server, partition):
    """
    Generates a table from a file consisting of the output of the
    ls command.
    """

    thefile = file(filename, 'r')
    savefile = file(filename[:-4] + '.csv', 'w')

    dirName = ""
    for line in thefile:
        if line[0] == '"':
            dirName = line.split('"')[1]
        elif line[0] == '-':
            line = line.strip()
            fields = line.split(None, 8)
            name = fields[8].strip('"')

            theList = [server,
                       partition,
                       fields[0],
                       fields[1],
                       fields[2],
                       fields[3],
                       fields[4],
                       fields[5],
                       fields[6].split('.')[0],
                       dirName,
                       name,
                       '\n']

            savefile.write(';'.join(theList))

    thefile.close()
    savefile.close()

#####################

def build_du(filename, server, partition):
    """
    Docstring goes here!
    """

    thefile = file(filename, 'r')
    savefile = file(filename[:-4] + '.csv', 'w')

    fileList = []
    for line in thefile:
        line = line.strip()
        size, name = line.split(None, 1)
        level = name.count('/')

        fields = [server,
                  partition,
                  size,
                  name,
                  str(level),
                  '\n']

        savefile.write(';'.join(fields))

    thefile.close()
    savefile.close()

############

def usage():
    print __doc__

#############

def main():
    # Parse command line options
    shortopts = 'ht'
    longopts = ['help','time','nosvn','nocopy']
    try:
        opts, args = getopt.getopt(sys.argv[1:], shortopts, longopts)
    except:
	usage()
        sys.exit(2)

    if len(args) > 1:
        print "Too many destination arguments"
        usage()
        sys.exit(2)

    dosvn = True
    docopy = True
    dotiming = False
    currentpath = None

    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            sys.exit()
        if o in ("-t", "--time"):
            dotiming = True
        if o == "--nosvn":
            dosvn = False
        if o == "--nocopy":
            docopy = False

    # Check for target directory
    if len(args) == 1:
        if os.path.isdir(args[0]):
            currentpath = os.getcwd()
            os.chdir(args[0])
        else:
            print "%s is not a valid directory" % args[0]
            sys.exit(2)

    # svn and copy opterations
    try:
        if dosvn: update_svn()
        if docopy: rsync_txt_files()
        txt_to_csv(dotiming)
        if dosvn: commit_svn()
    finally:
        if currentpath: os.chdir(currentpath)


if __name__ == '__main__':
    main()


