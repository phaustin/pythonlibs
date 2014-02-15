import MySQLdb,MySQLdb.cursors
import dateutil.parser as d
import re,os.path,string

filesDB = MySQLdb.Connect(db='philtest',user='phil',passwd='jeanluc',\
                           cursorclass=MySQLdb.cursors.DictCursor)
cursor = filesDB.cursor()
cursor.execute("use philtest;")
cursor.execute("show tables;")
print "tables: ",[item['Tables_in_philtest'] for item in cursor.fetchall()]
cursor.execute("show columns from kitefiles;")
thecols=cursor.fetchall()
thecols=[item['Field'] for item in thecols]
print "columns from kitefiles: \n%s" % thecols
command=r"""select size,name,date from kitefiles where name like "%.py" and date > '2006-04-30' order by date desc;"""
#command=r"""select size,name,date from kitefiles where name="odeinttest.py"  order by date desc;"""
cursor.execute(command)
newresults=cursor.fetchall()
print command
print len(newresults)
for item in newresults[:3000]:
    print item
