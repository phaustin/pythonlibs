"""
  write a bash script on roc and
  run the script on grex using fabric,
  capturing the output
"""
from tempfile import NamedTemporaryFile as mkfile
from fabric.api import env, run, execute, hide, put, cd
import textwrap,subprocess,shlex
import os
env.use_ssh_config = True
homedir=os.getenv('HOME')
env.ssh_config_path='%s/.ssh/config' % homedir
import re

#
# this re will extract the home directory
#
get_home=re.compile(".*sentry(.*)sentry.*",re.DOTALL)

env.hosts = ['grexhome']

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
            

if __name__=="__main__":

    #
    # here is the script we want to execute on grex
    #
    the_script=\
    """
    echo "begin script on grex"
    echo "hi!!!"
    echo "I am on grex"
    echo "arrived safely"
    """
    #
    # write the script to a tmpfile
    #
    the_script=textwrap.dedent(the_script)
    the_script=the_script.strip()
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
        find_home=get_home.match(test['grexhome'])
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
    # run the script on grex using bash
    #
    the_command='bash %s/scriptdir/scriptfile' % grex_home
    run_command=grex_wrapper(the_command)
    with hide('output'):
        test=execute(run_command)
        print "dumping the output"
        print test['grexhome']

            
            
    
    
