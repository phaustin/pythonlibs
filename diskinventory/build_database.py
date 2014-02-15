#!/home/phil/usr243_Fc3/bin/python
#ls -R -l -Q --time-style=full-iso --time=status /home/phil/*  > listing.txt
#-rw-r--r--    1 phil users         0 2005-10-06 12:28:09.000000000 -0700 "/home/phil/eosc211_fall2005.txt~"
#du /home/phil/* > ~/philprojects/disk_inventory/dulist.txt

import MySQLdb, MySQLdb.cursors
import glob, os.path

diskInfoDir='/home/phil/philprojects/diskInfoInventory-trunk'
# Jordan's diskInfoDir
#diskInfoDir='/home/jdawe/diskinfo/'


def create_ls(filesDB):
    """
    Sets up the ls table in the given database object.
    """
    cursor = filesDB.cursor()

    thecols = """server varchar(255) DEFAULT ' ' NOT NULL,
            partition varchar(255) DEFAULT ' ' NOT NULL,
            permission char(10) DEFAULT ' ' NOT NULL,
            links varchar(255) DEFAULT ' ' NOT NULL,   
            owner varchar(255) DEFAULT ' ' NOT NULL,   
            theGroup varchar(255) DEFAULT ' ' NOT NULL,
            size int DEFAULT '-999' NOT NULL,         
            date date        DEFAULT ' ' NOT NULL,    
            time time        DEFAULT ' ' NOT NULL,    
            directory varchar(255) DEFAULT ' ' NOT NULL,
            name varchar(255) DEFAULT ' ' NOT NULL"""

    command = """CREATE TABLE ls (%s)""" % thecols
    cursor.execute(command)

    cursor.close()

##############

def create_lsowner(filesDB, owner):
    """
    Sets up an lsowner table in the given database object.
    """
    cursor = filesDB.cursor()

    cursor.execute("drop table IF EXISTS ls%s;" % owner)

    command = """create table ls%s
                 select server, partition, size, date, time, directory, name
                 from ls
                 where owner = "%s";""" % (owner, owner)

    cursor.execute(command)

    cursor.close()

##############

def create_du(filesDB):
    """
    Sets up the du table in the given database object.
    """
    cursor = filesDB.cursor()

    thecols = """server varchar(255) DEFAULT ' ' NOT NULL,   
                 partition varchar(255) DEFAULT ' ' NOT NULL,   
                 size int unsigned DEFAULT ' ' NOT NULL,
                 name varchar(255) DEFAULT ' ' NOT NULL,   
                 level tinyint unsigned DEFAULT ' ' NOT NULL"""

    command = "CREATE TABLE du (%s)" % thecols
    cursor.execute(command)

    cursor.close()

#################

if __name__ == '__main__':

    filesDB = MySQLdb.Connect(db = 'filecensus',
                              user = 'phil',
                              passwd = 'jeanluc')

    cursor = filesDB.cursor()
    cursor.execute("drop table IF EXISTS ls;")
    cursor.execute("drop table IF EXISTS du;")

    create_ls(filesDB)
    create_du(filesDB)

    ls_glob = glob.glob('%s/*_ls.csv' % diskInfoDir)

    for filename in ls_glob:
        print filename
        cursor.execute("load data local infile '%s' into table ls fields terminated by ';';" % (filename))

    # Get list of owners
    cursor.execute("select owner from ls group by owner;")
    owners = cursor.fetchall()

    #for each owner, make a sub-database
    for owner in owners:
        # Testing code to not fill up /home partition.
        if owner[0] == 'phil':
            create_lsowner(filesDB, owner[0])

    du_glob = glob.glob('%s/*_ls.csv' % diskInfoDir)

    for filename in du_glob:
        print filename
        cursor.execute("load data local infile '%s' into table du fields terminated by ';';" % (filename))
 
    cursor.close()
    filesDB.close()

