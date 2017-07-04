"""
example:

pythion -m diskinventory.read_parquet pqfiles/newtera out_newtera

"""

from dask.dataframe import read_parquet,to_parquet
import dask.dataframe
from pathlib import Path
import pandas as pd
import numpy as np

def convert_to_dask(users_dir,out_dir):
    users_dir=Path(users_dir)
    file_list=users_dir.glob('*pq')
    str_list=[str(item) for item in file_list]
    str_list=sorted(str_list)

    da_frame=read_parquet(str_list[0])
    stop=len(da_frame)
    index=np.arange(0,stop)
    da_frame['index']=pd.Series(index)
    da_frame.set_index('index',inplace=True,drop=True)

    for item in str_list[1:]:
        print(item)
        start=stop
        new_frame=read_parquet(item)
        stop=start + len(new_frame)
        index=np.arange(start,stop)
        new_frame['index']=pd.Series(index)
        new_frame.set_index('index',inplace=True,drop=True)
        print(new_frame.divisions)
        da_frame=dask.dataframe.multi.concat([da_frame,new_frame],interleave_partitions=True)
        
    print('writing dataframe: size is ',len(da_frame))
    to_parquet(out_dir,da_frame)


if __name__ == "__main__":    
    import argparse, textwrap
    linebreaks = argparse.RawTextHelpFormatter
    descrip = textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,
                                     description=descrip)
    #parser.add_argument('rootdir',  type=str,help='root directory to be stored as prefix')
    parser.add_argument('users_dir', type=str, help='directory with pq files created by parse_file.py')
    parser.add_argument('out_dir', type=str, help='directory for parquet.Dataset')
    
    

