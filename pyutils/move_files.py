"""
move files that don't start with . to a folder, leaving only directories

"""
import argparse
import re
import tempfile
dotre=re.compile(r'^\..*')
from pathlib import Path
tempdir =tempfile.mkdtemp(prefix='holdit_',dir='.')
print('made: ',tempdir)
p = Path('.')
for x in p.iterdir():
    if x.is_dir() or dotre.match(str(x)):
        continue
    newfile = Path(tempdir) / x
    x.rename(newfile)
    

# linebreaks=argparse.RawTextHelpFormatter
# descrip=__doc__.ljust()
# parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
# parser.add_argument('-t','--times',nargs='+',type=int,help='one or more time indices',required=True)
# parser.add_argument('-c','--cases',nargs='+',help='one or more case names',required=True)
# args=parser.parse_args()
# print(args)
