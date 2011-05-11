"""
module for various subprocess commands
"""
#http://article.gmane.org/gmane.comp.python.general/487577/match=getstatusoutput+subprocess

import shlex, subprocess
from subprocess import Popen, PIPE, STDOUT

def command(command_line):
     """
     usage: status,output=command(commandstring)
     """
     args = shlex.split(command_line)
     p = Popen(args, stdout=PIPE, stderr=STDOUT, shell=False)
     s = p.stdout.read()
     print "complete: III"
     return p.wait(), s
 
