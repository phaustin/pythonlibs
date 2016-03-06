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
import glob

linebreaks=argparse.RawTextHelpFormatter
descrip=textwrap.dedent(globals()['__doc__'])
parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
parser.add_argument('dump_info', type=str,help='name of yaml file with file information ')
args=parser.parse_args()

import ruamel.yaml,sys
from collections import OrderedDict
try:
    with open(args.dump_info,'r') as f:
        input_dict = ruamel.yaml.load(f,ruamel.yaml.RoundTripLoader)
except FileNotFoundError as e:
    yaml_dict=OrderedDict()
    yaml_dict['root'] = '/root/path/here'
    yaml_dict['h5_out']= '/path/to/h5file/here.h5'
    yaml_dict['buf_length']= 1.e-15
    with open(e.filename,'w') as f:
        ruamel.yaml.dump(yaml_dict,f,Dumper=ruamel.yaml.RoundTripDumper,default_flow_style=False)
    print('created yaml template {}'.format(e.filename))
    print('edit and rerun')
    sys.exit(0)

try:
    check_root = glob.glob(input_dict['root'])[0]
except IndexError:
    raise ValueError("root {} doesn't exist".format(input_dict['root']))

input_dict['root'] = check_root + '/'

command_string="ls -R -l -Q --time-style=full-iso --time=status {root:s}"

errfile='ls_{text_base:s}_err.txt'.format_map(input_dict)
ls_outfile='ls_{text_base:s}.txt'.format_map(input_dict)

command=command_string.format_map(input_dict)
print("executing: ",command)
print("stdout and stderr set to: ",ls_outfile,errfile)

with open(ls_outfile,'w') as stdout:
    with open(errfile,'w') as stderr:
        subprocess.check_call(shlex.split(command),stderr=stderr,stdout=stdout)

du_errfile='du_{text_base:s}_err.txt'.format_map(input_dict)
du_outfile='du_{text_base:s}.txt'.format_map(input_dict)

command_string='du -k {root:s}'
command=command_string.format_map(input_dict)

with open(du_outfile,'w') as stdout:
    with open(du_errfile,'w') as stderr:
        subprocess.check_call(shlex.split(command),stderr=stderr,stdout=stdout)

print("executing: ",command)
print("stdout and stderr set to: ",du_outfile,du_errfile)

truncate=False
if truncate:
    count=500
    tempfile='tempout'
    with open(ls_outfile,'r') as infile:
        with open(tempfile,'w') as outfile:
            for i in range(count):
                outfile.write(next(infile))
    import shutil
    shutil.move(tempfile,ls_outfile)

du_frame=read_du(du_outfile,input_dict['root'])

counter,ls_frames,err_list = read_ls(ls_outfile,input_dict['root'])
print(counter,len(ls_frames))

with pd.HDFStore(input_dict['h5_out'],'w') as store:
    table_name = input_dict['table_du']
    store.put(table_name,du_frame,format='table')
    for index,the_frame in enumerate(ls_frames):
        table_name='{}_{}'.format(input_dict['table_ls'],index)
        store.put(table_name,the_frame,format='table')
    
#
# now dump the yaml file to a string
#
yaml_string = ruamel.yaml.dump(input_dict,None,Dumper=ruamel.yaml.RoundTripDumper,
                               default_flow_style=False)

with h5py.File(input_dict['h5_out'],'a') as f:
    f.attrs['yaml_string']=yaml_string
    f.attrs['yaml_file'] = args.dump_info
    f.attrs['root'] = input_dict['root'][:-1]





