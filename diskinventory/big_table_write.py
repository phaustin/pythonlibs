#!/usr/bin/env python

"""
   reads sqllite databases created by readlist.py  and readdu.py and writes out
    file information

   example:  retrieve.py files_tera_phil.db phil
"""
from __future__ import print_function

from sqlalchemy import and_
from sqlalchemy.orm import sessionmaker
import dataset
from pandas import DataFrame, Series
import pandas as pd
import argparse, textwrap
import datetime
import dateutil
import site,os
home_dir=os.getenv('HOME')
site.addsitedir('{}/repos/'.format(home_dir))
from pythonlibs.pyutils import check_md5 as c5

def get_frame_from_query(the_query,colnames):
    """make a dataframe from an sqlalchemy query"""
    df=DataFrame.from_records(list(the_query),columns=colnames)
    return df

file_db='/tera/phil/diskinventory/files_newtera.db'
dbstring='sqlite:///{:s}'.format(file_db)
db = dataset.connect(dbstring)
session=sessionmaker(bind=db.engine)
thesession=session()
file_table=db.metadata.tables['files']
direc_table=db.metadata.tables['direcs']

size=5.e5
large_query=thesession.query(file_table).filter(file_table.c.size > size).with_labels()
colnames=[item.name for item in list(db.metadata.tables['files'].columns)]

df_files=get_frame_from_query(large_query,colnames)

test=df_files.loc[:]
## records=test.to_dict('records')
## print(records)

buf_length=int(1.e5)

def fullnames(row):
    the_file='{}/{}'.format(row['directory'],row['name'])
    return the_file

def calc_hash(row):
    the_file='{}/{}'.format(row['directory'],row['name'])
    size,start,mid,stop=c5.check_md5(the_file,buf_length)
    return (size,'{}{}{}'.format(start,mid,stop))

out=test.groupby('size')
saveit=[]
for the_size,value in out.groups.items():
    if len(value) > 1:
       saveit.append(the_size)

file_count=0
save_names=[]
save_fullhash=[]

for framename in saveit:
    df=out.get_group(framename)
    save_names.append(df.apply(fullnames,axis=1))
    save_fullhash.append(df.apply(calc_hash,axis=1))
    file_count+=len(df)
    
    

## for group in the_groups:
##     df_size=out.get_group(group)
#print(test.apply(calc_hash,axis=1))


## df_files['size']=df_files['size']*1.e-9
## df_files=df_files.sort(columns='size',ascending=False)



## large_query=thesession.query(direc_table).filter((direc_table.c.level == 8))
## df_direcs=get_frame_from_query(large_query)
## hit=df_direcs['directory'].str.contains('/tera/phil/')
## out=df_direcs[hit]
## report=out[['directory','size']]
## report=report.sort(columns='size',ascending=False)
## report['size']=report['size']*1.e-6
## print(report['size'].sum())

## print(report.to_string())


## for id,row in report.iterrows():
##     left_size=len(row['directory'])
##     padding=70-left_size
##     row['space']=' '*padding
##     print("{directory:s}:{space:s}{size:>6.3f}".format(**row))


## for id,row in report.iterrows():
##     left_size=len(row['directory'])
##     print("rm -rf {directory:s}".format(**row))

big_names=[]
big_hash=[]
big_size=[]

for item in save_names:
    big_names.extend(item.tolist())

for item in save_fullhash:
    sep_hash=[the_tup[1] for the_tup in item.tolist()]
    sep_size=[the_tup[0] for the_tup in item.tolist()]
    big_hash.extend(sep_hash)
    big_size.extend(sep_size)

saveit=pd.DataFrame()
saveit['filenames']=big_names
saveit['fullhash']=big_hash
saveit['fullsize']=big_size
with pd.HDFStore('store.h5','w') as store:
    store.put('big_files',saveit,format='table')
