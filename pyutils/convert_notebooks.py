#!/usr/bin/env python
"""
   convert all ipython notebooks to python scripts (first time)
   convert all new or modified notebooks (subsequently)

   usage:

   ./convert_notebooks.py  -f notebook_folder -t html

   result:

      notebook_folder/html  contains the converted notebooks
"""

import os
import glob, stat
import datetime
import tzlocal  #pip install tzlocal
import pytz
import subprocess
import argparse
import textwrap
import sys, traceback


def find_modtime(the_file):
    """
       remove the .py or .ipynb extenstion from the file name
       to get the head, and return that name, plus the modification
       date in UTC.  
    """
    head,ext=os.path.splitext(the_file)
    #
    #  see os.stat docs for the format of the stat function.  It returns
    #  multiple fields (owner, date created, size, etc.) that are indexed by the stat object
    #
    the_date=datetime.datetime.fromtimestamp(os.stat(the_file)[stat.ST_MTIME])
    #
    # finding the local timezone is suprisingly hard -- need to install a
    # special module called tzlocal using pip install tzlocal
    #
    local_tz = tzlocal.get_localzone()
    the_date=local_tz.localize(the_date)
    #
    # convert every date to UTC
    #
    the_date = the_date.astimezone(pytz.utc)
    #
    # remove everything but the root filename
    #
    head = head.split('/')[-1] 
    return head,the_date

if __name__ == "__main__":
    
    linebreaks=argparse.RawTextHelpFormatter
    descrip=textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
    parser.add_argument('--folder','-f',type=str,help='folder containing ipynb files (default current directory)',default='.')
    parser.add_argument('--type','-t',type=str,help='py, pdf or html',default='html')
    args=parser.parse_args()
    try:
        currdir=os.getcwd()
        os.chdir(args.folder)
        if args.type not in ['py','html','pdf']:
            raise ValueError('type needs to be html or pdf')
        if args.type == 'pdf':
            suffix = 'tex'
            output_dir = 'pdf'
        elif args.type == 'py':
            suffix = 'py'
            output_dir = 'python'
        else:
            suffix = 'html'
            output_dir = 'html'
        if not os.path.exists(output_dir):
            print('creating output folder')
            os.makedirs(output_dir)
        #
        # get the ipynb and outpout files
        #
        notebooklist=glob.glob('*.ipynb')
        outlist=glob.glob('{0:}/*.{1:}'.format(output_dir,suffix))
        #
        #  build two dictionaries containing the root names and the modtimes
        #
        out_dict={}
        nb_dict={}
        
        for the_file in notebooklist:
            head, the_date = find_modtime(the_file)
            nb_dict[head] = the_date
        for the_file in outlist:
            head, the_date = find_modtime(the_file)
            out_dict[head] = the_date

        # use  sets to
        #find notebooks not in outlist
        #

        out_files=set(out_dict.keys())
        nb_files=set(nb_dict.keys())
        #
        # find all notebooks that don't have output files
        #
        build_new=list(nb_files - out_files)
        #
        # find all notebooks tat are newer than output
        old_files = nb_files.intersection(out_files)
        for the_file in old_files:
            if (out_dict[the_file] < nb_dict[the_file]):
                build_new.append(the_file)
        print('new files: {build_new!r}'.format_map(vars()))
        #
        #  run jupyter nbconvert on these notebook and put output in output_dir
        #
        if args.type == 'html':
            cmdstring='jupyter nbconvert --to html {0:s}.ipynb --output-dir html'
            for the_file in build_new:
                command=cmdstring.format(the_file)
                print('executing: ',command)
                out=subprocess.getstatusoutput(command)
                print(out)
        elif args.type == 'py':
            cmdstring='jupyter nbconvert --to python {0:s}.ipynb --output-dir python'
            for the_file in build_new:
                command=cmdstring.format(the_file)
                print('executing: ',command)
                out=subprocess.getstatusoutput(command)
                print(out)
        elif args.type == 'pdf':
            jupyter_string='jupyter nbconvert --to latex --output-dir pdf {0:s}.ipynb'
            latex_string='latexmk -pdf {0:s}.tex'
            for the_file in build_new:
                command=jupyter_string.format(the_file)
                print('executing nbconvert: ',command)
                out=subprocess.getstatusoutput(command)
                print('generated latex for {} output was {}'.format(the_file,out))
                try:
                    orig_dir = os.getcwd()
                    os.chdir('pdf')
                    command=latex_string.format(the_file)
                    print('executing latexmk: {} in {}'.format(command,os.getcwd()))
                    out=subprocess.getstatusoutput(command)
                    os.chdir(orig_dir)
                    print('back in {}: '.format(orig_dir))
                except Exception as e:
                    ex_type, ex_val, tb = sys.exc_info()
                    print('bummer: ',ex_val)
                    print('\nhere is the traceback:\n')
                    traceback.print_tb(tb)
                finally:
                    os.chdir(orig_dir)
        else:
            raise ValueError('type needs to be html or pdf, found type {}'.format(args.type))
    #
    # trap all exceptions and print a traceback
    #
    except Exception as e:
         ex_type, ex_val, tb = sys.exc_info()
         print('bummer: ',ex_val)
         print('\nhere is the traceback:\n')
         traceback.print_tb(tb)
    #
    # make sure we get back to the original folder
    #
    finally:
        os.chdir(currdir)
