#!/usr/bin/env python

"""
   ipython console:  run with %run -i

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
import numpy as np

home_dir=os.getenv('HOME')
site.addsitedir('{}/repos/'.format(home_dir))
import collections

def get_frame_from_query(the_query,colnames):
    """make a dataframe from an sqlalchemy query"""
    df=DataFrame.from_records(list(the_query),columns=colnames)
    return df

class keep_df:
    def __init__(self,df_files):
        self.df_files=df_files
        self.owners=self.find_owners()
        hit_owner={}
        owner_sizes={}
        for the_owner in the_data.find_owners():
            hit,the_size=the_data.find_size(the_owner)
            hit_owner[the_owner]=hit
            owner_sizes[the_owner]=the_size
        self.hit_owner=hit_owner
        self.owner_sizes=owner_sizes
        self.dirtree=collections.defaultdict(list)
        self.breakupdir=collections.defaultdict(list)

    def find_owners(self):
        owners=set(self.df_files['owner'])
        return owners

    def find_size(self,owner):
        hit=self.df_files['owner'] == owner
        sizes=(self.df_files['size'][hit]).sum()
        return(hit,sizes)

    def make_tree(self,owner):
        hit=self.hit_owner[owner]
        the_dirs=self.df_files[hit]['directory']
        for a_dir in the_dirs:
            self.dirtree[owner].append(a_dir.split('/'))

    def write_breakup(self,owner,depth):
        for fragments in self.dirtree[owner]:
            self.breakupdir[owner].append('/'.join(fragments[:depth]))
        
if __name__ == "__main__":
    if first_trip:
        file_db='./files_newtera.db'
        dbstring='sqlite:///{:s}'.format(file_db)
        db = dataset.connect(dbstring)
        session=sessionmaker(bind=db.engine)
        thesession=session()
        file_table=db.metadata.tables['files']
        direc_table=db.metadata.tables['direcs']

        size=1.e5
        large_query=thesession.query(file_table).filter(file_table.c.size > size).with_labels()
        colnames=[item.name for item in list(db.metadata.tables['files'].columns)]
        df_files_100K=get_frame_from_query(large_query,colnames)

        size=1.
        large_query=thesession.query(file_table).filter(file_table.c.size > size).with_labels()
        colnames=[item.name for item in list(db.metadata.tables['files'].columns)]
        df_files=get_frame_from_query(large_query,colnames)
    else:
        pass

    # if first_trip:
    #     with pd.HDFStore('tera_recover.h5','w') as store:
    #         store.put('all',df_files,format='table')
    #         store.put('g100K',df_files_100K,format='table')
    # owners=find_owner(df_files)
    the_data = keep_df(df_files)
    the_data.make_tree('vpopa')
    the_data.write_breakup('vpopa',7)    
    all_dirs=set(the_data.breakupdir['vpopa'])
    df_vpopa=df_files[the_data.hit_owner['vpopa']].copy()
    df_vpopa['breakupdir']=the_data.breakupdir['vpopa']
    vpopa_groups=df_vpopa.groupby(['breakupdir'])
    vpopa_size=vpopa_groups['size']
    vpopa_name=vpopa_groups['name']
    out=vpopa_size.agg(np.sum)
    width=80
    formatstring='{{0:<{}}} {{1:8.3f}}'.format(width)
    print(formatstring,'\n')
    sum=0
    for key,value in out.items():
        name=key.ljust(width)
        if value < 1.e9:
            continue
        sum+=value
        print(formatstring.format(key,value*1.e-9))
        
    # the_data.make_tree('nchaparr')
    # the_data.write_breakup('nchaparr',8)    
    # all_dirs=set(the_data.breakupdir['nchaparr'])
    # df_nchaparr=df_files[the_data.hit_owner['nchaparr']].copy()
    # df_nchaparr['breakupdir']=the_data.breakupdir['nchaparr']
    # nchaparr_groups=df_nchaparr.groupby(['breakupdir'])
    # nchaparr_size=nchaparr_groups['size']
    # out=nchaparr_size.agg(np.sum)
    # width=80
    # formatstring='{{0:<{}}} {{1:8.3f}}'.format(width)
    # print(formatstring,'\n')
    # sum=0
    # for key,value in out.items():
    #     name=key.ljust(width)
    #     if value < 1.e9:
    #         continue
    #     sum+=value
    #     print(formatstring.format(key,value*1.e-9))
