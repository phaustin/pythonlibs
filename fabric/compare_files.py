#!/usr/bin/env python

import dataset
from sqlalchemy.orm import sessionmaker
from pandas import DataFrame
import os.path

def get_frame_from_query(the_query):
    """make a dataframe from an sqlalchemy query"""
    colnames=[col['name'] for col in the_query.column_descriptions]
    df=DataFrame.from_records(list(the_query),columns=colnames)
    return df

def get_df_from_table(db,tablename):
    session=sessionmaker(bind=db.engine)
    thesession=session()
    the_table=db.metadata.tables[tablename]
    all_columns=thesession.query(the_table)
    df=get_frame_from_query(all_columns)
    thesession.close()
    return df


grex_db='grexfiles.db'
roc_db='rocfiles.db'

dbstring='sqlite:///{:s}'.format(roc_db)
db_roc=dataset.connect(dbstring)
df_roc=get_df_from_table(db_roc,'files')

dbstring='sqlite:///{:s}'.format(grex_db)
db_grex = dataset.connect(dbstring)
df_grex=get_df_from_table(db_grex,'files')

grex_names=list(df_grex['name'])

roc_names=list(df_roc['name'])

roc_set=set(roc_names)
if len(roc_set) != len(roc_names):
    raise Exception('duplicate entries in roc_names')
grex_set=set(grex_names)
if len(grex_set) != len(grex_names):
    raise Exception('duplicate entries in grex_names')


#
# find every .nc file on roc and every .bin3D file on grex
#
grex_dict={}
for id,row in df_grex.iterrows():
    the_name,the_ext=os.path.splitext(row['name'])
    if the_ext == '.bin3D':
        grex_dict[the_name]=dict(row)

roc_dict={}
for id,row in df_roc.iterrows():
    the_name,the_ext=os.path.splitext(row['name'])
    if the_ext == '.nc':
        roc_dict[the_name]=dict(row)

roc_keys=set(roc_dict.keys())
grex_keys=set(grex_dict.keys())

#
# files on both machines that can be deleted from
# grex
#
common_keys=roc_keys.intersection(grex_keys)

#
# write out a bash script to delete the files
#
with open('delete_grex.sh','w') as f:
    for i,key in enumerate(common_keys):
        #if i > 20: break
        command="rm -f %s/%s" % (grex_dict[key]['directory'],grex_dict[key]['name'])     
        f.write("%s\n" % command)
        
