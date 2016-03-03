#!/usr/bin/env python3
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
import re,os
import dateutil.parser as du
from pytz import timezone
import datetime as dt
import pandas as pd
from pyutils.check_md5 import check_md5

blocksize=50000
def read_ls(listfile,out_name):
    """
       read lines from listfile and transfer to
       database dataframe
    """
    blanks=re.compile('\s+')
    stripQuotes=re.compile('.*\"(.*)\".*')
    getName=re.compile('(?P<left>.*)\"(?P<name>.*)\".*')

    columnNames=['permission','links','owner','theGroup','size','date','directory','name','hash']
    frame_counter=0
    frame_list=[]
    with open(listfile,'r',encoding='utf-8') as f:
        errlist=[]
        counter=0
        collect=[]
        for newline in f:
            if (counter > 0) & (counter % blocksize == 0):
                print("linecount: ",counter)
                frame_name='{}_{}.h5'.format(out_name,frame_counter)
                new_frame=pd.DataFrame.from_records(collect)
                with pd.HDFStore(frame_name,'w') as store:
                    store.put('ls_out',new_frame,format='table')
                frame_list.append(frame_name)
                del new_frame
                frame_counter+=1
                collect=[]
            newline=newline.strip()
            if len(newline)>0:
                if newline[-1]==":":
                    #"/Users/phil/www.physics.mcgill.ca/~gang/ftp.transfer":
                    dirname=stripQuotes.match(newline)
                    dirname=dirname.group(1)
                    continue
                else:
                    test=getName.match(newline)
                    #-rw-r--r--    1 phil users         0 2005-10-06 12:28:09.000000000 -0700 "/home/phil/eosc211_fall2005.txt~"
                    if test:
                        #
                        # skip directories and symbolic links
                        #
                        if test.group("left")[0] == 'd' or test.group("left")[0] == 'l':
                            continue
                        #
                        # check for a path name like /home/phil/eosc211_fall2005.txt
                        #
                        root,filename=os.path.split(test.group("name"))
                        if len(root) > 0:
                            dirname=root
                        else:
                            #
                            # we've got a plain file name
                            #
                            filename=test.group("name")
                        try:
                            permission,links,owner,theGroup,size,date,time,offset =\
                                    blanks.split(test.group("left").strip())
                            #the_hash=hashlib.sha256('{}/{}'.format(dirname,filename).encode('utf-8')).hexdigest()
                            the_hash=check_md5('{}/{}'.format(dirname,filename))
                        except ValueError:
                            saveit=dict(newline=newline,splitout=repr(blanks.split(test.group("left").strip())),
                                        dirname=dirname,filename=filename,counter=counter)
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
                        size=int(size)
                        string_date=" ".join([date,time,offset])
                        date_with_tz=du.parse(string_date)
                        date_utc = date_with_tz.astimezone(timezone('UTC'))
                        timestamp=int(date_utc.strftime('%s'))
                        #columnNames=['permission','links','owner','theGroup','size','date','directory','name','hash']
                        out=(permission,links,owner,theGroup,size,timestamp,dirname,filename,the_hash)
                        collect.append(dict(list(zip(columnNames,out))))
                        ## print string_date
                        ## print date_utc
                        ## print dt.datetime.fromtimestamp(timestamp)
                        counter+=1
        if len(collect) != 0:
            print('inserting final {} lines'.format(len(collect)))
            frame_name='{}_{}.h5'.format(out_name,frame_counter)
            new_frame=pd.DataFrame.from_records(collect)
            with pd.HDFStore(frame_name,'w') as store:
                store.put('ls_out',new_frame,format='table')
            del new_frame
    return counter,frame_list,errlist


def read_du(dufile,out_name):
    """
       read lines from dufile and transfer to
       database table the_table
    """
    columnNames=['size','level','directory']
    counter=0
    collect=[]
    with open(dufile,'r',encoding='utf-8') as f:
        for newline in f:
            #print(counter)
            if (counter > 0) & (counter % blocksize == 0):
                print("linecount: ",counter)
                the_table.insert_many(collect)
                collect=[]
            newline=newline.strip()
            size,direc=newline.split('\t',1)
            size=int(size)
            level=direc.count('/')
            print('here is the level',level)
            out=(size,level,direc)
            collect.append(dict(list(zip(columnNames,out))))
            counter+=1
    if len(collect) != 0:
        frame_name='{}.h5'.format(out_name)
        new_frame=pd.DataFrame.from_records(collect)
        with pd.HDFStore(frame_name,'w') as store:
            store.put('du_out',new_frame,format='table')
    return counter,frame_name


if __name__ == "__main__":
    import argparse, textwrap
    linebreaks=argparse.RawTextHelpFormatter
    descrip=textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
    parser.add_argument('dulist',  type=str,help='filelist generated by du')
    parser.add_argument('listname', type=str,help='filelist generated by ls')
    parser.add_argument('store_root', type=str,help='string to appear in h5 files')
    args=parser.parse_args()

    dufile=args.dulist
    head,ext=os.path.splitext(dufile)
    print("reading file: ",dufile)
    h5_root = "du_{}".format(args.store_root)
    out=read_du(dufile,h5_root)
    print(out)

    listfile=args.listname
    head,ext=os.path.splitext(listfile)
    print("reading file: ",listfile)
    h5_root = "ls_{}".format(args.store_root)
    out=read_ls(listfile,h5_root)
    print(out)

                                  
                    

                    
                                            
                    

                        
                        
                    


                
                                  
                    

                    
                                            
                    

                        
                        
                    


                
