"""

read and reindex a parquet file

"""
from dask.dataframe import read_parquet
import numpy as np
from fastparquet import write

def reprocess(dataset_name,parquet_file):
    df_newtera=read_parquet(dataset_name)
    row_count=len(df_newtera)
    # df_newtera['index']=pd.Series(np.arange(0,row_count))
    partitions=np.linspace(0,row_count,10)
    new_partitions=[int(np.floor(item)) for item in partitions]
    df=df_newtera.repartition(divisions=new_partitions,force=True)
    newdf=df.compute()
    write(parquet_file,newdf,row_group_offsets=new_partitions[:-1],
          object_encoding='utf8',
          compression='SNAPPY')

if __name__ == "__main__":
    import argparse, textwrap
    linebreaks = argparse.RawTextHelpFormatter
    descrip = textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,
                                     description=descrip)
    parser.add_argument('in_dir', type=str, help='directory with parquet dataset')
    parser.add_argument('out_dir', type=str, help='directory for transformed dataset')
    args=parser.parse_args()
    reprocess(args.in_dir,args.out_dir)
    
