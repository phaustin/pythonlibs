"""
  read file database and find a file by string/name
"""
from sqlalchemy.orm import sessionmaker
from pandas import DataFrame
import os, site
homedir=os.getenv('HOME')
site.addsitedir('%s/repos' % homedir)
from pythonlibs.fabric import parse_ls as ps
import dataset
import argparse, textwrap
import pickle

def get_frame_from_query(the_query):
    """make a dataframe from an sqlalchemy query"""
    colnames=[col['name'] for col in the_query.column_descriptions]
    df=DataFrame.from_records(list(the_query),columns=colnames)
    return df

if __name__=="__main__":
    
    linebreaks=argparse.RawTextHelpFormatter
    descrip=textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
    parser.add_argument('dbname', nargs=1, type=str,help='sql database to search')
    parser.add_argument('filestring', nargs=1, type=str,help='file string to match')
    args=parser.parse_args()
    filestring=args.filestring[0]
    dbname=args.dbname[0]

    dbstring='sqlite:///{:s}'.format(dbname)
    the_db=dataset.connect(dbstring)
    session=sessionmaker(bind=the_db.engine)
    thesession=session()
    file_table=the_db.metadata.tables['files']
    the_query=thesession.query(file_table.c.directory,file_table.c.name).\
      filter(file_table.c.name.like('%scm15h%'))
    df=get_frame_from_query(the_query)
    print df.to_string()

    
