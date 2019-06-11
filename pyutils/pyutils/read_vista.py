#!/usr/bin/env python
# -*- coding: utf-8 -*-
import unicodedata, codecs
from decode import unescape
from cStringIO import StringIO

import csv
filename='readme.csv'
thefile=codecs.open(filename, "r", "utf-8" )
text=thefile.read()
thefile.close()
oldtext=unicodedata.normalize('NFKC',text)
text=oldtext.encode('ascii','ignore')
thefile=StringIO(text)
theDict=csv.DictReader(thefile)
import textwrap
for count,item in enumerate(theDict):
    print(60*'+')
    oldtext=item['Pre-class prep question -- SHORT ANSWER (Short Answer)']
    name="%s %s: " % (item['First name'],item['Last name'])
    newtext=unescape(oldtext)
    print(name,textwrap.fill(newtext,width=90))
    print(60*'+')
    
    
