#!/usr/bin/env python
"""
#on osx
#/usr/local/Cellar/coreutils/8.22/libexec/gnubin/ls

parses the output from:

ls -R -l -Q --time-style=full-iso --time=status /home/phil/*   (file size in bytes)

or

du -k  (file size in kbytes)

and writes to a sqllite table

example:

parse_files.py du_list.txt ls_list.txt

"""
import re, os
import dateutil.parser as du
from pytz import timezone
import datetime as dt
import pandas as pd
from pyutils.check_md5 import check_md5
import logging
import pyarrow as pa
import pyarrow.parquet as pq


import pandas as pd

def write_all(du_frame,df_list,diskname):
    table = pa.Table.from_pandas(du_frame)
    filename=f'df_{diskname}_du.pq'
    pq.write_table(table, filename,compression='snappy')
    for counter,item in enumerate(df_list):
        if len(item) > 0:
            filename=f'df_{diskname}_ls_{counter:03d}.pq'
            item.reset_index(drop=True,inplace=True)
            print(f'writing file {counter}')
            table = pa.Table.from_pandas(item)
            pq.write_table(table, filename,compression='snappy')

def read_ls(listfile, root_path, blocksize=50000, buffer_length=1.e5,debug_interval=1000):
    """
       read lines from listfile and transfer to
       database dataframe


    Parameters
    ----------

    listfile: ls filename
    """
    blocksize = int(blocksize)
    blanks = re.compile('\s+')
    stripQuotes = re.compile('.*\"(.*)\".*')
    getName = re.compile('(?P<left>.*)\"(?P<name>.*)\".*')

    columnNames = ['permission', 'links', 'owner', 'theGroup', 'size', 'date',
                   'directory', 'name', 'hash']
    frame_counter = 0
    frame_list = []
    mylog = logging.getLogger('main')
    mylog.propagate = False
    print('here are the handlers', mylog.handlers)

    with open(listfile, 'r', encoding='utf-8') as f:
        errlist = []
        counter = 0
        collect = []
        for newline in f:
            if (counter > 0) & (counter % blocksize == 0):
                print("linecount: ", counter)
                frame_list.append(pd.DataFrame.from_records(collect))
                frame_counter += 1
                collect = []
            if counter % debug_interval == 0:
                mylog.info('debug %s %d', 'dump: ', counter)
            newline = newline.strip()
            if len(newline) > 0:
                if newline[-1] == ":":
                    # found a directory name
                    # "/Users/phil/www.physics.mcgill.ca/~gang/ftp.transfer":
                    dirname = stripQuotes.match(newline)
                    dirname_capture = dirname.group(1)
                    continue
                else:
                    test = getName.match(newline)
                    # -rw-r--r--    1 phil users         0 2005-10-06 12:28:09.000000000 -0700 "/home/phil/eosc211_fall2005.txt~"
                    if test:
                        #
                        # skip directories and symbolic links
                        #
                        if test.group("left")[0] == 'd' or test.group("left")[
                                0] == 'l':
                            continue
                        #
                        # check for a path name like /home/phil/eosc211_fall2005.txt
                        #
                        head_path, filename = os.path.split(test.group("name"))
                        if len(head_path) != 0:
                            raise ValueError(
                                "expecting a naked filename, got {}".format(
                                    test.group("name")))
                        try:
                            permission,links,owner,theGroup,size,date,time,offset =\
                                    blanks.split(test.group("left").strip())
                            #the_hash=hashlib.sha256('{}/{}'.format(dirname,filename).encode('utf-8')).hexdigest()
                            # the_hash = check_md5('{}/{}'.format(
                            #     dirname_capture, filename),
                            #                      buffer_length=buffer_length)
                            the_hash = 999.
                        except ValueError:
                            saveit = dict(newline=newline,
                                          splitout=repr(blanks.split(
                                              test.group("left").strip())),
                                          dirname=head_path,
                                          filename=filename,
                                          counter=counter)
                            errmsg=\
                                """
                                  __________
                                  caught ValueError trying to split {newline:s}
                                  output of split is {splitout:s}
                                  filename is {dirname:s}/{filename:s}
                                  counter value is {counter:d}
                                  __________
                                """.format(**saveit)
                            errlist.append(errmsg)
                            continue
                        size = int(size)
                        string_date = " ".join([date, time, offset])
                        date_with_tz = du.parse(string_date)
                        date_utc = date_with_tz.astimezone(timezone('UTC'))
                        timestamp = int(date_utc.strftime('%s'))
                        #columnNames=['permission','links','owner','theGroup','size','date','directory','name','hash']
                        if dirname_capture.find(root_path) < 0:
                            raise ValueError(
                                f'dirname root error for dirname= {dirname_capture} and rootpath= {root_path} with counter={counter}')
                        if dirname_capture == root_path:
                            dirname = '.'
                        else:
                            dirname = dirname_capture[len(root_path):]
                        out = (permission, links, owner, theGroup, size,
                               timestamp, dirname, filename, the_hash)

                        collect.append(dict(list(zip(columnNames, out))))
                        ## print string_date
                        ## print date_utc
                        ## print dt.datetime.fromtimestamp(timestamp)
                        counter += 1
        if len(collect) != 0:
            print('inserting final {} lines'.format(len(collect)))
            frame_list.append(pd.DataFrame.from_records(collect))
    return counter, frame_list, errlist


def read_du(dufile, root_path):
    """
       read lines from dufile and transfer to
       database table the_table
    """
    columnNames = ['size', 'level', 'directory']
    collect = []
    with open(dufile, 'r', encoding='utf-8') as f:
        for newline in f:
            newline = newline.strip()
            #print('debug: ',newline)
            size, direc = newline.split('\t', 1)
            size = int(size)
            level = direc.count('/')
            out = (size, level, direc)
            collect.append(dict(list(zip(columnNames, out))))
    if len(collect) != 0:
        new_frame = pd.DataFrame.from_records(collect)
    return new_frame


def main():
    import argparse, textwrap
    linebreaks = argparse.RawTextHelpFormatter
    descrip = textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,
                                     description=descrip)
    #parser.add_argument('rootdir',  type=str,help='root directory to be stored as prefix')
    parser.add_argument('dulist', type=str, help='filelist generated by du')
    parser.add_argument('listname', type=str, help='filelist generated by ls')
    parser.add_argument('store_root',
                        type=str,
                        help='string to appear in h5 files')
    args = parser.parse_args()

    dufile = args.dulist
    head, ext = os.path.splitext(dufile)
    print("reading file: ", dufile)
    h5_root = "du_{}".format(args.store_root)
    df_du = read_du(dufile, h5_root)

    listfile = args.listname
    head, ext = os.path.splitext(listfile)
    print("reading file: ", listfile)
    h5_root = "ls_{}".format(args.store_root)
    counter, frame_list, errlist = read_ls(listfile, args.store_root)
    write_all(df_du,frame_list,args.store_root)
    print(f'here is errlist: {errlist}')
    
if __name__ == "__main__":
    main()
    
