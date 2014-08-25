#!/usr/bin/env python
"""
  run the a bash script on a remote using fabric,
  the script runs the command:

  ls -R -l -Q --time-style=full-iso --time=status targetdir

  and saves the output into a dataframe with columns

  columnNames=['permission','links','owner','theGroup','size','date','directory','name']


  example:

    write_filelist.py  grexhome /home/paustin/repos/dotfiles dotfiles.db
  
"""
from tempfile import NamedTemporaryFile as mkfile
from fabric.api import env, run, execute, hide, put, cd
import textwrap,subprocess,shlex
import os, cStringIO
from parse_ls import read_ls
import dataset

import os,errno

def silent_remove(filename):
    print("{:s} will be destroyed if it exists".format(filename))
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT: # errno.ENOENT = no such file or directory
            raise # re-raise exception if a different error occured

def grex_wrapper(command):
    """
      return a function that, when called,
      runs a shell command using fabric.api.run
      and returns the output
    """
    def grex_call():
        #run this on grex
        output=run(command)
        return output
    return grex_call

def command(the_command):
    """
       execute a local command, capturing the output of
       stdout and stderr 
    """
    try:
        with mkfile(delete=False) as  stdout:
            with mkfile(delete=False) as  stderr:
                subprocess.check_call(shlex.split(the_command),stderr=stderr,stdout=stdout)
    except subprocess.CalledProcessError:
        print "received output, check stdout and stderr files"
    return (stdout.name,stderr.name)
            
def write_database(filename,dframe):
    dbstring='sqlite:///{:s}'.format(filename)
    silent_remove(filename)
    db = dataset.connect(dbstring)
    the_table = db.create_table('files')
    for i,item in dframe.iterrows():
        the_table.insert(dict(item))    
    return db

if __name__=="__main__":
    linebreaks=argparse.RawTextHelpFormatter
    descrip=textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
    parser.add_argument('root', nargs=1, type=str,help='top directory to list')
    parser.add_argument('outfile_base', nargs=1,type=str,help='output name base')
    args=parser.parse_args()

    env.use_ssh_config = True
    homedir=os.getenv('HOME')
    env.ssh_config_path='%s/.ssh/config' % homedir
    import re

    #
    # this re will extract the home directory
    #
    get_payload=re.compile(".*sentry(.*)sentry.*",re.DOTALL)

    env.hosts = ['grexhome']

    #
    # here is the script we want to execute on grex
    #
    the_script=\
    """
    echo "sentry"
    ls -R -l -Q --time-style=full-iso --time=status {target:s}
    echo "sentry"
    """
    #/global/scratch/vladpopa/data_analysis/OUT_3D
    #/tera/vpopa/data_analysis/OUT_3D
    #
    # write the script to a tmpfile
    #
    the_script=textwrap.dedent(the_script)
    the_script=the_script.strip()
    root_dir="/home/paustin/repos/dotfiles"
    the_script=the_script.format(target=root_dir)
    with mkfile(delete=False) as scriptfile:
        print "writing script to %s" % scriptfile.name
        scriptfile.write(the_script)
        scriptfile.write('\n')
    #
    # find the HOME directory on grex
    #
    the_command='echo "sentry";echo $HOME;echo "sentry"'
    run_command=grex_wrapper(the_command)
    with hide('output'):
        test=execute(run_command)
        find_home=get_payload.match(test['grexhome'])
        grex_home=find_home.groups(1)[0].strip()
    #
    # make a directory to contain the scriptfile on grex
    #
    the_command='mkdir -p %s/scriptdir' % grex_home
    run_command=grex_wrapper(the_command)
    with hide('output'):
        test=execute(run_command)
    #
    #
    # scp the file to grex
    #
    the_command="scp %s grexhome:%s/scriptdir/scriptfile" % (scriptfile.name,grex_home)
    stdout,stderr=command(the_command)
    print stdout
    print stderr
    #
    # run the script we scp'd to scriptfile  on grex using bash
    #
    the_command='bash %s/scriptdir/scriptfile' % grex_home
    run_command=grex_wrapper(the_command)
    with hide('output'):
        test=execute(run_command)
        file_list=get_payload.match(test['grexhome'])
        file_list=file_list.groups(1)[0].strip()
        listfile=cStringIO.StringIO(file_list)
        df=read_ls(listfile)
    
    
            
            
    
    
