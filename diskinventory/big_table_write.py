#!/usr/bin/env python
"""
   
big_table_write dbfile outh5
   reads sqllite databases created by readlist.py  and readdu.py and writes out
    file information

   example: python big_table_write dbfile outh5
"""
from __future__ import print_function

import argparse
import textwrap

import pandas as pd
import dataset
from pandas import DataFrame
from sqlalchemy.orm import sessionmaker

from pyutils import check_md5 as c5


def get_frame_from_query(the_query, colnames):
    """make a dataframe from an sqlalchemy query"""
    df = DataFrame.from_records(list(the_query), columns=colnames)
    return df


linebreaks = argparse.RawTextHelpFormatter
descrip = textwrap.dedent(globals()['__doc__'])
parser = argparse.ArgumentParser(formatter_class=linebreaks,
                                 description=descrip)
parser.add_argument('dbfile',
                    type=str,
                    help='name of sqllite database written by read_cron.py')
parser.add_argument('outh5', type=str, help='name of pandas h5 output')
args = parser.parse_args()

file_db = args.dbfile
out_h5 = args.outh5
dbstring = 'sqlite:///{:s}'.format(file_db)
db = dataset.connect(dbstring)
session = sessionmaker(bind=db.engine)
thesession = session()
file_table = db.metadata.tables['files']
direc_table = db.metadata.tables['direcs']

size = 0.
large_query = thesession.query(file_table).filter(file_table.c.size >
                                                  size).with_labels()
colnames = [item.name for item in list(db.metadata.tables['files'].columns)]
df_files = get_frame_from_query(large_query, colnames)

colnames = [item.name for item in list(db.metadata.tables['direcs'].columns)]
large_query=thesession.query(direc_table).filter((direc_table.c.level == 8))
df_direcs=get_frame_from_query(large_query,colnames)

with pd.HDFStore(out_h5, 'w') as store:
    store.put('files', df_files, format='table')
    store.put('direcs', df_direcs, format='table')

# test = df_files.loc[:]
# ## records=test.to_dict('records')
# ## print(records)

# buf_length = int(1.e5)


# def fullnames(row):
#     the_file = '{}/{}'.format(row['directory'], row['name'])
#     return the_file


# def calc_hash(row):
#     the_file = '{}/{}'.format(row['directory'], row['name'])
#     size, start, mid, stop = c5.check_md5(the_file, buf_length)
#     return (size, '{}{}{}'.format(start, mid, stop))


# out = test.groupby('size')
# saveit = []
# for the_size, value in out.groups.items():
#     if len(value) > 1:
#         saveit.append(the_size)

# file_count = 0
# save_names = []
# save_fullhash = []

# for framename in saveit:
#     df = out.get_group(framename)
#     save_names.append(df.apply(fullnames, axis=1))
#     save_fullhash.append(df.apply(calc_hash, axis=1))
#     file_count += len(df)

#     ## for group in the_groups:
#     ##     df_size=out.get_group(group)
#     #print(test.apply(calc_hash,axis=1))

#     ## df_files['size']=df_files['size']*1.e-9
#     ## df_files=df_files.sort(columns='size',ascending=False)

#     ## large_query=thesession.query(direc_table).filter((direc_table.c.level == 8))
#     ## df_direcs=get_frame_from_query(large_query)
#     ## hit=df_direcs['directory'].str.contains('/tera/phil/')
#     ## out=df_direcs[hit]
#     ## report=out[['directory','size']]
#     ## report=report.sort(columns='size',ascending=False)
#     ## report['size']=report['size']*1.e-6
#     ## print(report['size'].sum())

#     ## print(report.to_string())

#     ## for id,row in report.iterrows():
#     ##     left_size=len(row['directory'])
#     ##     padding=70-left_size
#     ##     row['space']=' '*padding
#     ##     print("{directory:s}:{space:s}{size:>6.3f}".format(**row))

#     ## for id,row in report.iterrows():
#     ##     left_size=len(row['directory'])
#     ##     print("rm -rf {directory:s}".format(**row))

# big_names = []
# big_hash = []
# big_size = []

# for item in save_names:
#     big_names.extend(item.tolist())

# for item in save_fullhash:
#     sep_hash = [the_tup[1] for the_tup in item.tolist()]
#     sep_size = [the_tup[0] for the_tup in item.tolist()]
#     big_hash.extend(sep_hash)
#     big_size.extend(sep_size)

# saveit = pd.DataFrame()
# saveit['filenames'] = big_names
# saveit['fullhash'] = big_hash
# saveit['fullsize'] = big_size
# with pd.HDFStore(out_h5, 'w') as store:
#     store.put('big_files', saveit, format='table')
