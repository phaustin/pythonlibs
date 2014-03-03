from tempfile import NamedTemporaryFile as mkfile
from fabric.api import env, run, execute, hide, put, cd
import textwrap,subprocess,shlex
env.use_ssh_config = True
env.ssh_config_path='/Users/phil/.ssh/config'
import StringIO as sio

env.hosts = ['grexhome']

def grex_wrapper(command):
    """
      create a function that, when called,
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
    # scp the file to grex
    #
    the_command="scp %s grexhome:/home/paustin/scriptdir/scriptfile" % scriptfile.name
    stdout,stderr=command(the_command)
    print stdout
    print stderr
    #
    # run the script using bash
    #
    the_command='bash /home/paustin/scriptdir/scriptfile'
    run_command=grex_wrapper(the_command)
    with hide('output'):
        test=execute(run_command)
        print "dumping the output"
        print test['grexhome']

            
            
    
    
