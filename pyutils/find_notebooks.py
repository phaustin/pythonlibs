import re
from pathlib import Path

from pyutils.table import make_table
from orderedset import OrderedSet
notebook = re.compile('(\w+\.ipynb)')
keepit = OrderedSet()
with open('index.rst','r') as infile:
    for line in infile:
        for item in notebook.finditer(line):
            keepit.add(item.group(1))


table_list=[['notebook','html','pdf','python']]
namelist=[]
for item in keepit:
    name=Path(item).stem
    namelist.append(name)
    html=r"`{}_html`_"
    pdf=r"`{}_pdf`_"
    python = r"`{}_py`_"
    html,pdf,python = [item.format(name) for item in [html,pdf,python]]
    table_list.append([name,html,pdf,python])
    
with open('index_notebooks.txt','w') as notetxt:
   for name in namelist:
       notetxt.write('.. _{0:}_html: http://clouds.eos.ubc.ca/~phil/courses/atsc212/html/{0:}.html\n'.format(name))
       notetxt.write('.. _{0:}_pdf: http://clouds.eos.ubc.ca/~phil/courses/atsc212/pdf/{0:}.pdf\n'.format(name))
       notetxt.write('.. _{0:}_py: https://github.com/phaustin/A212/blob/master/notebooks/python/{0:}.py\n'.format(name))

header="""
.. include:: index_notebooks.txt             

.. _notebooks:
             
A212 notebooks in order of appearance
+++++++++++++++++++++++++++++++++++++

"""

with open('notebook_index.rst','w') as noterst:
    noterst.write(header)
    noterst.write(make_table(table_list))
