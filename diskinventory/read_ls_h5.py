#!/usr/bin/env python
"""

 
 ls -R -l -Q --time-style=full-iso --time=status /tera/phil
 du -k /tera/phil

 if these are successful, then  an h5 file will be generated with two dataframes

tera.h5
 
"""
import pandas as pd
from diskinventory.parse_files import read_du,read_ls
import argparse, textwrap
import subprocess,shlex
from io import StringIO
import h5py

linebreaks=argparse.RawTextHelpFormatter
descrip=textwrap.dedent(globals()['__doc__'])
parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
parser.add_argument('dump_info', type=str,help='name of yaml file with file information ')
args=parser.parse_args()

import ruamel.yaml,sys

with h5py.File(args.dump_info,'r') as f:
    yaml_string=f.attrs['yaml_string']
    yaml_filename=f.attrs['yaml_file']
    print("attributes: ",list(f.attrs.keys()))
    

yaml_in = StringIO(yaml_string)
yaml_dict = ruamel.yaml.load(yaml_in,ruamel.yaml.RoundTripLoader)
print(yaml_dict)

with pd.HDFStore(args.dump_info,'r') as f:
    print('dataframes: ',f.keys())
    




