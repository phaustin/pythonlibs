import pyutils.check_md5
from importlib import reload
reload(pyutils.check_md5)
from pyutils.check_md5 import check_md5
import glob
filelist = glob.glob('vpopa*txt')[0]
print(filelist)

root='/newtera/tera/vpopa/'
with open(filelist, 'r') as infile:
    with open('delete.sh', 'w') as outfile:
        with open('checksum.csv', 'w') as check:
            for line in infile:
                print(line)
                filename=line.strip()
                check.write('{};{}\n'.format(filename,check_md5(filename)))
                outfile.write('rm -rf {}'.format(line))
