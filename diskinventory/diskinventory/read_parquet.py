from dask.dataframe import read_table,read_parquet,to_parquet
import dask.dataframe
import pyarrow.parquet as pq
from pathlib import Path
import pandas as pd
import numpy as np

users_dir=Path('/localhome/datatmp/phil/pqfiles/newtera/')
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
    
print(len(da_frame))
out_dir='out_newtera'
to_parquet(out_dir,da_frame)
# for item in df_list[1:5]:
#     to_parquet(out_dir,item,append=True)
    
    

