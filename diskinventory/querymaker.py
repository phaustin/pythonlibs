#!/home/phil/usr243_Fc3/bin/python

import MySQLdb, MySQLdb.cursors
import os
import sets

class queryMaker:
    """Class for making queries of the filecensus database."""
    def __init__(self, table='ls'):
        self.filesDB = MySQLdb.Connect(db = 'filecensus',
                                       user = 'phil',
                                       passwd = 'jeanluc',
                                       cursorclass = MySQLdb.cursors.DictCursor)
        self.cursor = self.filesDB.cursor()
        self.table = table

    def generic_query(self, command):
	"""Executes a query on the database
           command is a string containing the query to execute
	   returns a list of dictionaries."""

        self.cursor.execute(command)
        return self.cursor.fetchall()

    def identical_filenames(self):
        """Returns a dictionary of duplicate files, indexed by filename,
           containting the number of times that filename appears in the
           filesystems."""

        command = """SELECT name,
                     COUNT(*) AS repetitions
                     FROM %s
                     GROUP BY name
                     HAVING repetitions > 1
                     ORDER BY repetitions
                     ;""" % self.table

        result = self.generic_query(command)

        tempdict = {}
        for item in result:
            tempdict[item['name']] = item['repetitions']
        return tempdict

    def pathname(self, searchstring, sortstring=""):
        """Given a SQL-formatted search string, returns all the database
        entries with a filename or directory that matches.
        sortstring is an optional string of column names seperated by
        commas on which MySQL should sort.
        """

        command = ['CREATE TEMPORARY TABLE tmp']
        command.append('SELECT * FROM %s' % self.table)
        command.append('WHERE name LIKE "%s"' % searchstring)
        command.append(';')
        command = ' '.join(command)
        self.cursor.execute(command)

        command = ['INSERT INTO tmp']
        command.append('SELECT * FROM %s' % self.table)
        command.append('WHERE directory LIKE "%s"' % searchstring)
        command.append(';')
        command = ' '.join(command)
        self.cursor.execute(command)

        command = ['SELECT DISTINCT * from tmp']
        if sortstring:
            command.append('ORDER BY %s' % sortstring)
        command.append(';')
        command = ' '.join(command)
        self.cursor.execute(command)

        result = self.generic_query(command)

        command = "DROP TABLE tmp;"
        self.cursor.execute(command)

        return result

    def __path_set_compare(self, path1, path2, operation):
        """helper function for comparing paths via sets."""

        command = '''SELECT directory, name
                     FROM %s
                     WHERE directory LIKE "%s%%"
                     AND server = "%s"
                     AND partition = "%s"
                     ;'''

        result1 = self.generic_query(command % (self.table,
                                                path1['dirroot'],
                                                path1['server'],
                                                path1['partition']))

        result2 = self.generic_query(command % (self.table
                                                path2['dirroot'],
                                                path2['server'],
                                                path2['partition']))

        list1 = []
        for item in result1:
           stripdir = item['directory'].lstrip( path1['dirroot'] )
           list1.append('/'.join( [stripdir, item['name']] ))
	
        list2 = []
        for item in result2:
           stripdir = item['directory'].lstrip( path2['dirroot'] )
           list2.append('/'.join( [stripdir, item['name']] ))

        fileset1 = sets.ImmutableSet(list1)
        fileset2 = sets.ImmutableSet(list2)

        if operation == 'difference':
            return fileset1.difference(fileset2)
        if operation == 'sdifference':
            return fileset1.symmetric_difference(fileset2)
        if operation == 'intersection':
            return fileset1.intersection(fileset2)

    def path_sdifference(self, path1, path2):
        """Given two dictionaries containing the server, partition and
        root directory of a directory tree, returns all the paths
        that are different between the two.
        ie. path = {'dirroot' : '/home/phil/',
                    'server' : 'kite',
                    'partition' : 'home'}
        """

        return self.__path_set_compare(path1, path2, 'sdifference')

    def path_difference(self, path1, path2):
        """Given two dictionaries containing the server, partition and
        root directory of a directory tree, returns the paths
        that are present in path1 but not in path2.
        ie. path = {'dirroot' : '/home/phil/',
                    'server' : 'kite',
                    'partition' : 'home'}
        """

        return self.__path_set_compare(path1, path2, 'difference')

    def path_intersection(self, path1, path2):
        """Given two dictionaries containing the server, partition and
        root directory of a directory tree, returns all the paths
        that are the same between the two.
        ie. path = {'dirroot' : '/home/phil/',
                    'server' : 'kite',
                    'partition' : 'home'}
        """

        return self.__path_set_compare(path1, path2, 'intersection')


    def diskquotas(self):
        """Retuns a list of dictionaries, indexed by server then disk
        partition then total diskspace used, containing the server,
        the disk partition, the user, and the space used by that user
        on the given server and partition."""

        command = """SELECT server, partition, owner,
                     SUM(size) AS totalsize
                     FROM %s
                     GROUP BY server, partition, owner
                     ORDER BY server, partition, totalsize
                     ;""" % self.table
        result =  self.generic_query(command)

        return result

def main():
    qm = queryMaker()

    path1 = {'dirroot' : '/home/datatmp/phil/',
             'server' : 'thrush',
             'partition' : 'home'}

    path2 = {'dirroot' : '/home/datatmp/phil/',
             'server' : 'tern',
             'partition' : 'home'}

#    result = qm.path_difference(path1, path2)

#    for item in result:
#         print item

#    result = qm.pathname("%ogden%")
#    result = qm.identical_filenames()
#    for item in result:
#        print "%s / %s / %s/%s" % (item['server'],
#                                   item['partition'],
#                                   item['directory'],
#                                   item['name'])

    result = qm.diskquotas()
    for item in result:
        print "%s / %s / %s: %d MB" % (item['server'],
                                       item['partition'],
                                       item['owner'],
                                       item['totalsize']/1e6)

if __name__ == '__main__':
    main()
