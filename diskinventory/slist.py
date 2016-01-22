#!/usr/bin/env python

"""
   reads sqllite databases created by readlist.py  and readdu.py and writes out
    file information

   example:  retrieve.py files_tera_phil.db phil
"""

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
