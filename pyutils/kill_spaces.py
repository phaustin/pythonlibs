import os
from pathlib import Path
import re

def scantree(path):
    """Recursively yield DirEntry objects for given directory."""
    for entry in os.scandir(path):
        if entry.is_dir(follow_symlinks=False):
            yield from scantree(entry.path)  # see below for Python 2.x
        else:
            yield entry
def ensure(path):
    path.parent.mkdir(parents=True, exist_ok=True)
    return path
            
contains= ' '
basePath='.'
for the_item in scantree(basePath):
    fullpath=Path(the_item).resolve()
    fullstring=str(fullpath)
    # if the contains string is not none and the filename does not contain
    # the supplied string, then ignore the file
    if contains is not None and fullstring.find(contains) == -1:
        continue
    print(fullpath)
    test=Path(fullstring.replace(' ','_'))
    try:
        fullpath.rename(ensure(test))
    except FileNotFoundError:
        print(f"failure for {test}")
    print(fullpath)

parts=re.compile(r"(.*)([\"\'].*[\"\'])(.*)")
copy_file=Path("/Users/phil/Nextcloud/e340_2019_fall/pythonprogs/copy_files.py")
with open(copy_file,'r') as the_file:
    all_lines=the_file.readlines()
keep_lines=[]
for line in all_lines:
    work = str(line)
    if line.find('file_new') > -1:
        out=parts.match(line)
        if out:
            if out.group(2).find(' ') > -1:
                print(f"before: {out.group(2)}")
                test=out.group(2).replace(' ','_')
                values=list(out.groups())
                print(values)
                values[1]=test
                work=''.join(values)
    keep_lines.append(work)

print(f'read {len(all_lines)}')
print(f'writing {len(keep_lines)}')
with open("test.py",'w') as newfile:
    for item in keep_lines:
        out=item.rstrip()
        newfile.write(f"{out}\n")
