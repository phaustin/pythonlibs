#!/usr/bin/env python
"""

 
 ls -R -l -Q --time-style=full-iso --time=status /tera/phil
 du -k /tera/phil

 if these are successful, then  an h5 file will be generated with two dataframes

tera.h5
 
"""
import pandas as pd
from importlib import reload
import diskinventory.parse_files
reload(diskinventory.parse_files)
from diskinventory.parse_files import read_du, read_ls
import argparse
import textwrap
import subprocess
import shlex
import h5py
import glob
import ruamel.yaml
import sys
from collections import OrderedDict
import pyutils.make_logger
reload(pyutils.make_logger)
from pyutils.make_logger import make_logger
import logging

if __name__ == "__main__":
    linebreaks = argparse.RawTextHelpFormatter
    descrip = textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,
                                     description=descrip)
    parser.add_argument('dump_info',
                        type=str,
                        help='name of yaml file with file information ')
    args = parser.parse_args()

    try:
        with open(args.dump_info, 'r') as f:
            input_dict = ruamel.yaml.load(f, ruamel.yaml.RoundTripLoader)
        print('got input_dict: ',input_dict)
    except FileNotFoundError as e:
        yaml_dict = OrderedDict()
        yaml_dict['root'] = '/root/path/here'
        yaml_dict['h5_out'] = '/path/to/h5file/here.h5'
        yaml_dict['buf_length'] = 1.e-15
        with open(e.filename, 'w') as f:
            ruamel.yaml.dump(yaml_dict,
                             f,
                             Dumper=ruamel.yaml.RoundTripDumper,
                             default_flow_style=False)
        print('created yaml template {}'.format(e.filename))
        print('edit and rerun')
        sys.exit(0)

    try:
        check_root = glob.glob(input_dict['root'])[0]
    except IndexError:
        raise ValueError("root {} doesn't exist".format(input_dict['root']))

    input_dict['root'] = check_root + '/'

    errfile = 'ls_{text_base:s}_err.txt'.format_map(input_dict)
    ls_outfile = 'ls_{text_base:s}.txt'.format_map(input_dict)
    du_errfile = 'du_{text_base:s}_err.txt'.format_map(input_dict)
    du_outfile = 'du_{text_base:s}.txt'.format_map(input_dict)

    #
    # run ls and send to file
    #
    if input_dict['run_ls_du']:
        print('running ls and du')
        command_string = "ls -R -l -Q --time-style=full-iso --time=status {root:s}"
        command = command_string.format_map(input_dict)
        print("executing: ", command)

        with open(ls_outfile, 'w') as stdout:
            with open(errfile, 'w') as stderr:
                subprocess.check_call(
                    shlex.split(command),
                    stderr=stderr,
                    stdout=stdout)

        command_string = 'du -k {root:s}'
        command = command_string.format_map(input_dict)
        print("executing: ", command)

        with open(du_outfile, 'w') as stdout:
            with open(du_errfile, 'w') as stderr:
                subprocess.check_call(
                    shlex.split(command),
                    stderr=stderr,
                    stdout=stdout)
    else:
        print('reading from {} and {}'.format(ls_outfile, du_outfile))

    if input_dict['do_truncate']:
        count = 200000
        tempfile = 'tempout'
        with open(ls_outfile, 'r') as infile:
            with open(tempfile, 'w') as outfile:
                for i in range(count):
                    outfile.write(next(infile))
        import shutil
        shutil.move(tempfile, ls_outfile)

    mylog = make_logger(name='main')
    print('handlers are: ', mylog.handlers)
    if input_dict['debug']:
        mylog.setLevel(logging.DEBUG)

    du_frame = read_du(du_outfile, input_dict['root'])

    counter, ls_frames, err_list = read_ls(ls_outfile, input_dict['root'])
    print(counter, len(ls_frames))

    with pd.HDFStore(input_dict['h5_out'], 'w') as store:
        table_name = input_dict['table_du']
        store.put(table_name, du_frame, format='table')
        for index, the_frame in enumerate(ls_frames):
            table_name = '{}_{}'.format(input_dict['table_ls'], index)
            store.put(table_name, the_frame, format='table')

        #
        # now dump the yaml file to a string
        #
    yaml_string = ruamel.yaml.dump(input_dict,
                                   None,
                                   Dumper=ruamel.yaml.RoundTripDumper,
                                   default_flow_style=False)

    with h5py.File(input_dict['h5_out'], 'a') as f:
        f.attrs['yaml_string'] = yaml_string
        f.attrs['yaml_file'] = args.dump_info
        f.attrs['root'] = input_dict['root'][:-1]
