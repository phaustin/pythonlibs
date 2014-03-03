#!/usr/bin/env python

"""
   reads sqllite databases created by readlist.py  and readdu.py and writes out
    file information

   example:  retrieve_root_du.py du_tera_root.db
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

linebreaks=argparse.RawTextHelpFormatter
descrip=textwrap.dedent(globals()['__doc__'])
parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
parser.add_argument('file_db',nargs=1,type=str,help='file list db  generated by read_disk.py')
parser.add_argument('owner', nargs=1, type=str,help='file owner')
parser.add_argument('--size',nargs='?',default=1.e6, type=float,help='float smallest size in bytes (default 1.e6)')
parser.add_argument('--older_than',nargs='?', default='now',type=str,help='only files modified before yyyy-mm-dd')
args=parser.parse_args()

file_db=args.file_db[0]
owner=args.owner[0]
size=int(args.size)
if args.older_than == 'now':
    older_than=datetime.datetime.now()
    older_than=int(older_than.strftime('%s'))
else:
    older_than=dateutil.parser.parse(args.older_than)
    older_than=int(older_than.strftime('%s'))
    
#print args.size,args.level

#sys.exit(0)

dbstring='sqlite:///{:s}'.format(file_db)
db = dataset.connect(dbstring)
session=sessionmaker(bind=db.engine)
thesession=session()
file_table=db.metadata.tables['files']
direc_table=db.metadata.tables['direcs']

large_query=thesession.query(file_table).filter(and_(file_table.c.size > size,file_table.c.owner==owner,\
                                                     file_table.c.date < older_than))

df_files=get_frame_from_query(large_query)

df_files['size']=df_files['size']*1.e-9
df_files=df_files.sort(columns='size',ascending=False)



large_query=thesession.query(direc_table).filter((direc_table.c.level == 8))
df_direcs=get_frame_from_query(large_query)
hit=df_direcs['directory'].str.contains('/tera/phil/')
out=df_direcs[hit]
report=out[['directory','size']]
report=report.sort(columns='size',ascending=False)
report['size']=report['size']*1.e-6
print report['size'].sum()

print report.to_string()


for id,row in report.iterrows():
    left_size=len(row['directory'])
    padding=70-left_size
    row['space']=' '*padding
    print "{directory:s}:{space:s}{size:>6.3f}".format(**row)


for id,row in report.iterrows():
    left_size=len(row['directory'])
    print "rm -rf {directory:s}".format(**row)
    
