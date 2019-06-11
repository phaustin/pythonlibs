from pathlib import Path
import sys
import site

curr_dir = Path(__file__).parent
# ~/repos/pythonlibs/diskinventory/notebooks/python/
if curr_dir.name == "python":
    root_dir = curr_dir.parents[1]
else:
    #~/repos/pythonlibs/diskinventory/notebooks
    root_dir = curr_dir.parent
#~/repos/pythonlibs/diskinventory/notebooks/datadir
data_dir = root_dir / 'datadir'
print(f"expecting datadir at {data_dir}")

sys.path.insert(0,root_dir)
sep='*'*30
site.removeduppaths()
print((f'{sep}\ncontext imported. Front of path:\n'
       f'{sys.path[0]}\n{sys.path[1]}\n{sep}\n'))
