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
from io import StringIO
import h5py
import ruamel.yaml
import os, path


def column_key(col_name):
    """
    sort: du, ls_0, ls_1,
    """
    out = col_name.split('_')
    if len(out) == 1:
        index = -1
    else:
        index = int(out[1])
    return index


def get_yaml(h5_filename):
    with h5py.File(h5_filename, 'r') as f:
        yaml_string = f.attrs['yaml_string']
        yaml_in = StringIO(yaml_string)
        yaml_dict = ruamel.yaml.load(yaml_in, ruamel.yaml.RoundTripLoader)
        return list(f.attrs.keys()), yaml_dict


def main(args):
    print(args.h5_filename)
    keys, yaml_dict = get_yaml(args.h5_filename)
    print(keys, yaml_dict)

    with pd.HDFStore(args.h5_filename, 'r') as f:
        df_frames = f.keys()
        print('dataframes: ', df_frames)
        df_ls = f['ls_0']

    print(df_ls.columns)
    print(df_ls['name'].head())
    print(df_ls['directory'].head())

    df_frames.sort(key=column_key)
    print(df_frames)

    size = 0

    with pd.HDFStore(args.h5_filename, 'r') as f:
        columns = ['size', 'directory', 'name']
        df_size_only = f[df_frames[1]][columns]
        print(len(df_size_only))
        for frame_name in df_frames[2:]:
            size += f[frame_name]['size'].sum()
            df_size_only = pd.concat((df_size_only, f[frame_name][columns]))
            print(len(df_size_only))

    return size, df_size_only
    #for frame_name in df_frames[1:]:


if __name__ == "__main__":
    linebreaks = argparse.RawTextHelpFormatter
    descrip = textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,
                                     description=descrip)
    parser.add_argument('h5_filename',
                        type=str,
                        help='name of h5 file written with make_ls')
    args = parser.parse_args()

    size, df_size_only = main(args)
    head, tail = os.path.splitext(args.h5_filename)
    outfile = '{}_thin.h5'.format(head)
    with pd.HDFStore(outfile, 'w') as out:
        out.put('three_cols', df_size_only, format='table')
    print('writing {}'.format(outfile))
    with pd.HDFStore(outfile, 'r') as input:
        print('reread, len = ',len(input['three_cols']))
    
