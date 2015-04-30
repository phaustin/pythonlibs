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
import argparse, textwrap
import sys
import datetime
import dateutil

def get_frame_from_query(the_query):
    """make a dataframe from an sqlalchemy query"""
    colnames=[col['name'] for col in the_query.column_descriptions]
    df=DataFrame.from_records(list(the_query),columns=colnames)
    return df

file_db='/tera/phil/diskinventory/files_tera.db'
dbstring='sqlite:///{:s}'.format(file_db)
print(dbstring)
db = dataset.connect(dbstring)
session=sessionmaker(bind=db.engine)
thesession=session()
file_table=db.metadata.tables['files']
direc_table=db.metadata.tables['direcs']
print(db.metadata.tables)

size=20.e6
large_query=thesession.query(file_table).filter(file_table.c.size > size)

df_files=get_frame_from_query(large_query)
records=df_files.to_dict('records')




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
    
