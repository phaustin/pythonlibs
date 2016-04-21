from os import scandir
import os,stat
from pathlib import Path

def scantree(path):
    """Recursively yield DirEntry objects for given directory."""
    for entry in scandir(path):
        if entry.is_dir(follow_symlinks=False):
            yield from scantree(entry.path)  # see below for Python 2.x
        else:
            yield entry

if __name__ == '__main__':
    #http://stackoverflow.com/questions/16249440/changing-file-permission-in-python
    import sys
    for entry in scantree(sys.argv[1] if len(sys.argv) > 1 else '.'):
        if entry.path.find('.git') == -1:
            fullpath=Path(entry.path)
            os.chmod(str(fullpath), stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH)

