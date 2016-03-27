#!/usr/bin/env python
"""
* read_ls_h5.py 
 ls -R -l -Q --time-style=full-iso --time=status /tera/phil
 du -k /tera/phil

 if these are successful, then  an h5 file will be generated with two dataframes

tera.h5

"""
import pandas as pd
import argparse
import textwrap
import numpy as np
from io import StringIO
import h5py
import ruamel.yaml
import os, path

if __name__ == "__main__":
    linebreaks = argparse.RawTextHelpFormatter
    descrip = textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,
                                     description=descrip)
    parser.add_argument('h5_filename',
                        type=str,
                        help='name of h5 file written with read_ls_h5')
    args = parser.parse_args()
    with pd.HDFStore(args.h5_filename, 'r') as infile:
        df_names = infile['three_cols']
        hit = df_names['name'].str.contains('bin3D')
        print(len(hit))
        print(np.sum(hit))
        df_fullname = df_names.loc[hit]
        print(df_fullname['size'].sum() * 1.e-12)

    with open('filelist.txt', 'w') as out:
        for index, row in df_fullname.iterrows():
            out.write('/newtera/tera/vpopa/sam_checkpoints/{directory:s}/{name:}\n'.format_map(row.to_dict()))
