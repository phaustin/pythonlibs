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

file_db='/tera/phil/diskinventory/files_tera.db'
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

## test=df_files.loc[:]
## ## records=test.to_dict('records')
## ## print(records)

buf_length=int(1.e5)

def fullnames(row):
    the_file='{}/{}'.format(row['directory'],row['name'])
    return the_file

def calc_hash(row):
    the_file='{}/{}'.format(row['directory'],row['name'])
    size,start,mid,stop=c5.check_md5(the_file,buf_length)
    return (size,'{}{}{}'.format(start,mid,stop))

file_count=0
save_names=[]
save_fullhash=[]

save_names=df_files.apply(fullnames,axis=1)
save_fulltup=df_files.apply(calc_hash,axis=1)
pure_size=[item[0] for item in save_fulltup]
pure_hash=[item[1] for item in save_fulltup]

saveit=pd.DataFrame()
saveit['filenames']=save_names
saveit['fullhash']=pure_hash
saveit['fullsize']=pure_size
with pd.HDFStore('tera.h5','w') as store:
    store.put('tera_files',saveit,format='table')
