"""
move files that don't start with . to a folder, leaving only directories

example: python -m pyutils.moveit thedir
"""

import argparse
import re, os
import tempfile
from pathlib import Path
import errno, sys

dotre = re.compile(r'^\..*')


def mkdir_p(path):
    try:
        os.makedirs(path, exist_ok=False)
        print('made: ', dirname)
    except OSError as exc:
        if exc.errno == errno.EEXIST:
            print('{} already exists'.format(path))
        else:
            #permission error, etc.
            raise


if __name__ == "__main__":

    linebreaks = argparse.RawTextHelpFormatter
    descrip = __doc__.lstrip()
    parser = argparse.ArgumentParser(formatter_class=linebreaks,
                                     description=descrip)
    parser.add_argument(
        'dir', nargs='?',
        type=str, help='optional directory name')
    args = parser.parse_args()
    if args.dir:
        dirname = args.dir
        mkdir_p(dirname)
    else:
        confirm = input("confirm creation of temporary directory {}: y/n ")
        if confirm == 'y':
            dirname = tempfile.mkdtemp(prefix='holdit_', dir='.')
            p = Path('.')
            for x in p.iterdir():
                if x.is_dir() or dotre.match(str(x)):
                    continue
                newfile = Path(dirname) / x
                x.rename(newfile)
