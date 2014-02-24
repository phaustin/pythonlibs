"""
   retrieve files lareger than 50 Mbytes owned by phil
"""

from sqlalchemy import and_
from sqlalchemy.orm import sessionmaker
import dataset
from pandas import DataFrame, Series

def get_frame_from_query(the_query):
    """make a dataframe from an sqlalchemy query"""
    colnames=[col['name'] for col in the_query.column_descriptions]
    df=DataFrame.from_records(list(the_query),columns=colnames)
    return df


dbname='filelist_save.db'
dbstring='sqlite:///{:s}'.format(dbname)
db = dataset.connect(dbstring)
session=sessionmaker(bind=db.engine)
thesession=session()
file_table=db.metadata.tables['files']

bigfile=int(1.e7)
large_query=thesession.query(file_table).filter(and_(file_table.c.size > bigfile,file_table.c.owner=='phil'))

df_files=get_frame_from_query(large_query)

dbname='dulist_save.db'
dbstring='sqlite:///{:s}'.format(dbname)
db = dataset.connect(dbstring)
session=sessionmaker(bind=db.engine)
thesession=session()
direc_table=db.metadata.tables['direcs']
#large_query=thesession.query(direc_table).filter(and_(direc_table.c.level==2))
large_query=thesession.query(direc_table).filter(and_(direc_table.c.level==3,direc_table.c.size>int(1.e6)))
df_direcs=get_frame_from_query(large_query)
df_direcs['size']=df_direcs['size']*1000*1.e-6  #du -k gives file size in kbytes



