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
        self.hit_owner={}
        self.owner_sizes={}
        self.dirtree=collections.defaultdict(list)
        self.breakupdir=collections.defaultdict(list)

    def set_sizes(self):
        owners = list(self.find_owners()) + ['all']
        for the_owner in owners:
            hit,the_size=self.find_size(the_owner)
            self.hit_owner[the_owner]=hit
            self.owner_sizes[the_owner]=the_size

    def find_owners(self):
        owners=set(self.df_files['owner'])
        return owners

    def find_size(self,owner):
        if owner == 'all':
            hit=self.df_files['size']  > 0.
            sizes=(self.df_files['size'][hit]).sum()
        else:
            hit=self.df_files['owner'] == owner
            sizes=(self.df_files['size'][hit]).sum()
        return(hit,sizes)

    def make_tree(self,owner=None):
        if owner:
            hit=self.hit_owner[owner]
            the_dirs=self.df_files[hit]['directory']
            print('in make_tree ',len(the_dirs))
            self.dirtree[owner]=[]
            for a_dir in the_dirs:
                self.dirtree[owner].append(a_dir.split('/'))
        else:
            self.dirtree['all']=[]
            the_dirs=self.df_files['directory']
            for a_dir in the_dirs:
                self.dirtree['all'].append(a_dir.split('/'))

    def write_breakup(self,owner,depth):
        self.breakupdir[owner]=[]
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
        the_data = keep_df(df_files)
        the_data.set_sizes()
        the_data.make_tree()    
        the_data.write_breakup('all',3)    
        the_data.make_tree('vpopa')
        the_data.write_breakup('vpopa',7)    
        the_data.make_tree('hbarker')
        the_data.write_breakup('harker',8)    
        the_data.make_tree('nchaparr')
        the_data.make_tree('phil')
    else:
        pass
    the_data.write_breakup('all',4)    
    the_data.write_breakup('nchaparr',6)    
    the_data.make_tree('xwei')
    the_data.write_breakup('xwei',8)    
    the_data.write_breakup('phil',5)    
    

    vpopa=False
    if vpopa:
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

    nchaparr=True
    if nchaparr:
        all_dirs=set(the_data.breakupdir['nchaparr'])
        df_nchaparr=df_files[the_data.hit_owner['nchaparr']].copy()
        df_nchaparr['breakupdir']=the_data.breakupdir['nchaparr']
        nchaparr_groups=df_nchaparr.groupby(['breakupdir'])
        nchaparr_size=nchaparr_groups['size']
        out=nchaparr_size.agg(np.sum)
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

    all=True
    if all:
        all_dirs=set(the_data.breakupdir['all'])
        df_all=df_files[the_data.hit_owner['all']].copy()
        df_all['breakupdir']=the_data.breakupdir['all']
        all_groups=df_all.groupby(['breakupdir'])
        all_size=all_groups['size']
        out=all_size.agg(np.sum)
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


    phil=True
    if phil:
        all_dirs=set(the_data.breakupdir['phil'])
        df_phil=df_files[the_data.hit_owner['phil']].copy()
        df_phil['breakupdir']=the_data.breakupdir['phil']
        phil_groups=df_phil.groupby(['breakupdir'])
        phil_size=phil_groups['size']
        out=phil_size.agg(np.sum)
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

    print('xwei')        
    xwei=True
    if xwei:
        all_dirs=set(the_data.breakupdir['xwei'])
        df_xwei=df_files[the_data.hit_owner['xwei']].copy()
        df_xwei['breakupdir']=the_data.breakupdir['xwei']
        xwei_groups=df_xwei.groupby(['breakupdir'])
        xwei_size=xwei_groups['size']
        out=xwei_size.agg(np.sum)
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
