#ls -R -l -Q --time-style=full-iso --time=status /home/phil/*  > listing.txt
#-rw-r--r--    1 phil users         0 2005-10-06 12:28:09.000000000 -0700 "/home/phil/eosc211_fall2005.txt~"
#du /home/phil/* > ~/philprojects/disk_inventory/dulist.txt
import MySQLdb,MySQLdb.cursors
import dateutil.parser as d
import re,os.path,string

filesDB = MySQLdb.Connect(db='philtest',user='phil',passwd='jeanluc',\
                           cursorclass=MySQLdb.cursors.DictCursor)
cursor = filesDB.cursor()
cursor.execute("use philtest;")
cursor.execute("show tables;")
print "tables: ",[item['Tables_in_philtest'] for item in cursor.fetchall()]
cursor.execute("show columns from philfiles;")
thecols=cursor.fetchall()
thecols=[item['Field'] for item in thecols]
print "columns from philfiles: \n%s" % thecols

buildLs=False
if buildLs:
    thefile=file('/nfs/kite/users/misc/kiteusers_ls.txt','r')
    cursor.execute("drop table IF EXISTS kitefiles;")
    columnNames=['permission','links','owner','theGroup','size','date','time','directory','name']
    blanks=re.compile('\s+')
    stripQuotes=re.compile('.*\"(.*)\".*')
    fileList=[]
    for line in thefile:
        line=line.strip()
        fields=blanks.split(line)
        if len(fields)==1:
            theMatch=stripQuotes.match(fields[0])
            if theMatch:
                dirName=theMatch.group(1)
        elif len(fields) > 6:
            theMatch=stripQuotes.match(line)
            if theMatch:
                filename=theMatch.group(1)
            else:
                raise ValueError("something wrong with %s" % line)
            if fields[0][0]=='d' or fields[0][0]=='l':
                continue
            theDatetime= "%s %s" % tuple(fields[5:7])
            try:
                theDatetime=d.parse(theDatetime)
            except:
                print "parse trouble with %s" % theDatetime
                continue
            if filename.find('/') > -1:
                dirName,filename=os.path.split(filename)
            lineDict=dict(zip(columnNames[0:5],fields[0:5]))
            lineDict['directory']=dirName
            lineDict['name']=filename
            lineDict['date']=theDatetime
            lineDict['time']=theDatetime
            lineDict['size']=string.atoi(lineDict['size'])
            theList=[lineDict[key] for key in columnNames]
            fileList.append(theList)

    thecols="""%s varchar(250) DEFAULT ' ' NOT NULL,
            %s varchar(250) DEFAULT ' ' NOT NULL,   
            %s varchar(250) DEFAULT ' ' NOT NULL,   
            %s varchar(250) DEFAULT ' ' NOT NULL,
            %s int DEFAULT '-999' NOT NULL,         
            %s date        DEFAULT ' ' NOT NULL,    
            %s time        DEFAULT ' ' NOT NULL,    
            %s varchar(250) DEFAULT ' ' NOT NULL,
            %s varchar(250) DEFAULT ' ' NOT NULL"""

    thecols=thecols % tuple(columnNames)

    command="""CREATE TABLE kitefiles (
            %s
    )"""

    command= command % thecols

    out=cursor.execute(command)

    command="""INSERT INTO kitefiles
      (%s, %s, %s, %s, %s, %s, %s, %s, %s)
      VALUES (%%s, %%s, %%s, %%s, %%s, %%s, %%s, %%s, %%s)"""

    command = command % tuple(columnNames)

    for item in fileList:
        cursor.execute(command,item)

    filesDB.commit()

buildDu=False
lineSplit=re.compile('^(\d+)\s+(.*)$')

if buildDu:
    thefile=file('/nfs/kite/users/misc/kiteusers_du.txt','r')
    cursor.execute("drop table IF EXISTS kitedu;")
    columnNames=['size','name','level']
    blanks=re.compile('\s+')
    fileList=[]
    for line in thefile:
        line=line.strip()
        fields=[]
        theMatch=lineSplit.match(line)
        try:
            theGroups=theMatch.groups()
            if len(theGroups) != 2:
                raise ValueError('failed to parse: %s' % line)
            level=theGroups[1].count('/')
            fields=[string.atoi(theGroups[0]),theGroups[1],level]
        except:
            raise ValueError('failed to parse: %s' % line)
        fileList.append(fields)
    thecols="""%s int DEFAULT ' ' NOT NULL,
            %s varchar(250) DEFAULT ' ' NOT NULL,   
            %s int DEFAULT ' ' NOT NULL"""

    thecols= thecols % tuple(columnNames)

    command="""CREATE TABLE kitedu (
            %s
    )"""

    command= command % thecols
    print command

    out=cursor.execute(command)

    command="""INSERT INTO kitedu
      (%s, %s, %s)
      VALUES (%%s, %%s, %%s);"""

    command = command % tuple(columnNames)


    for item in fileList:
        cursor.execute(command,item)

    filesDB.commit()

cursor.execute("show columns from kitedu;")
thecols=cursor.fetchall()
thecols=[item['Field'] for item in thecols]
print thecols

command="""select size,name from kitedu where level=2 order by size desc;"""
#command="""select distinct level from phildu;"""

cursor.execute(command)
newresults=cursor.fetchall()
for item in newresults:
    print item

print "bingo"

command="""select size,owner from kitefiles where date > 2006-04-20 group by owner order by size desc;"""
cursor.execute(command)
newresults=cursor.fetchall()
for item in newresults:
    print item

##command="""select sum(size),owner from kitefiles group by owner;"""
##command="""select sum(size) from kitefiles;"""
##cursor.execute(command)
##newresults=cursor.fetchall()
##for item in newresults:
##    print item['sum(size)']/1.e9

##command="""select sum(size) from tempfiles;"""
##cursor.execute(command)
##newresults=cursor.fetchall()
##for item in newresults:
##    print item['sum(size)']/1.e9
    

##command="""select size,directory,name from tempfiles order by size desc;"""
##cursor.execute(command)
##newresults=cursor.fetchall()
##for item in newresults:
##    print item['size']/1.e9,item['directory'] + '/' + item['name']


######picfile=file('filelist.pic','w')
######import pickle

######pickle.dump(newresults,picfile,protocol=2)
######picfile.close()




######for item in newresults[:10]:
######    print item

####filesDB.close()
