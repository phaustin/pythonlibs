#!/home/phil/usr243_Fc3/bin/python

"""
Script to perform file searches of the diskInventory database.  Finds all
file enteries where the path matches the sql_searchstring.  By default outputs
the server, disk partition, then full pathname of each file.

Usage:		findfiles.py <options> sql_searchstring

Arguments:
    sql_searchsting is a string to search for.  Two wildcards are supported:
        %  Matches any number of characters, even zero characters
        _  Matches exactly one character
        (String searches for % or _ may be performed using \% or \_)

Options:
    -h, --help                     Print command help
    -v, --verbose                  Output every field from the database
    -t, --terse                    Output only the file paths
    --length=n                     Restrict the length of the output to the
                                   last n items
    --sort=column1[,column2,...]   Sort output by column1, then column2, etc
                                   where 'column' may be one of
                                   name, directory, server, partition, size,
                                   date, time, permission, links, owner,
                                   or theGroup
Examples:

Find all python files sorted by server then date:
findfiles.py --sort=server,date %.py

Find the 10 largest files netcdf files in the database:
findfiles.py --length=10 --sort=size %.nc
"""

from querymaker import queryMaker
import sys, getopt

def usage():
    print __doc__

############

def main():
    # Parse command line options
    shortopts = "htv"
    longopts = ["help", "terse", "verbose", "length=", "sort="]
    try:
        opts, args = getopt.getopt(sys.argv[1:], shortopts, longopts)
    except:
        usage()
        sys.exit(2)

    if len(args) == 1:
        filter = args[0]
    else:
        usage()
        sys.exit(2)

    list_length = 0
    sorting = ""
    verbose = False
    terse = False

    for o, a in opts:
        if o in ('-h', '--help'):
            usage()
            sys.exit()
        if o in ('--length'):
            list_length = int(a)
        if o in ('--sort'):
            sorting = a
        if o in ('-v', '--verbose'):
            if terse:
                print "-t and -v are mutually exclusive."
                sys.exit(2)              
            verbose = True
        if o in ('-t', '--terse'):
            if verbose:
                print "-t and -v are mutually exclusive."
                sys.exit(2)              
            terse = True

    qm = queryMaker()
    result = qm.pathname(filter, sorting)

    if list_length:
        if list_length < len(result):
            result = result[-list_length:]

    for item in result:
        if verbose:
            print item
        elif terse:
            print "%s/%s" % (item['directory'], item['name'])
        else:
            print "%s %s: %s/%s" % (item['server'],
                                    item['partition'],
                                    item['directory'],
                                    item['name'])

if __name__ == '__main__':
    main()
