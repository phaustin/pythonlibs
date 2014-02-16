import os, site, stat, pwd, errno, dataset
from datetime import datetime
from pandas import DataFrame, Series
from sqlalchemy.orm import sessionmaker

def get_frame_from_query(the_query):
    """make a dataframe from an sqlalchemy query"""
    colnames=[col['name'] for col in the_query.column_descriptions]
    df=DataFrame.from_records(list(the_query),columns=colnames)
    return df

dbname='dirlist.db'
dbstring='sqlite:///{:s}'.format(dbname)
db = dataset.connect(dbstring)
print db.tables
the_files=db.metadata.tables['files']

session=sessionmaker(bind=db.engine)
thesession=session()

the_query=thesession.query(the_files)

df_files=get_frame_from_query(the_query)
