#!/usr/bin/env python3

## In [7]: print sys.stdout.encoding
## ------> print(sys.stdout.encoding)
## UTF-8


#http://kitchingroup.cheme.cmu.edu/blog/2014/11/04/Accessing-web-of-science-entry-citing-and-related-articles-from-a-doi-in-emacs/
#originally from http://www.thamnos.de/misc/look-up-bibliographical-information-from-a-doi/
#need to submit your email address to  http://www.crossref.org/requestaccount/  which
# then becomes your api key for requests via http://help.crossref.org/using_http
#

# crossref.py 10.1021/jp047349j
# http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/10.1021/jp047349j

from __future__ import print_function
import os,site
home_dir=os.getenv('HOME')
site.addsitedir('%s/repos' % home_dir)
import pdb

from pythonlibs.pyutils.bitly_helper import get_connection

def ascii_filt(my_string):
    return ''.join([i for i in my_string if ord(i) < 128])


bitly_conn=get_connection()

debug = False
 
crossref_api_key = 'paustin@eos.ubc.ca'
 
# get the doi
import sys

for arg in sys.argv[1:]:
    arg = arg.strip()
    arg = arg.strip("doi:")
    arg = arg.strip("http://")
    arg = arg.strip("dx.doi.org/")
    doi = arg.strip()

    wos_url=r"http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/{}".format(doi)
    citing_url=r"http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F{}&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.citing=yes".format(doi)
    related_url=r'http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F{}&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.related=yes'.format(doi)

    wos_url=bitly_conn.shorten(wos_url)['url']
    citing_url=bitly_conn.shorten(citing_url)['url']
    related_url=bitly_conn.shorten(related_url)['url']
    
    # clear from previous
    text_journal_title = ""
    text_year = ""
    text_volume = ""
    text_issue = ""
    text_title = ""
    text_first_author_surname = ""
    text_first_page = ""
    text_last_page = ""
    authorlist = []
 
    # download the xml
    import urllib
    from xml.dom import minidom
    usock = urllib.request.urlopen('http://www.crossref.org/openurl/?id=doi:'+doi+'&noredirect=true&pid='+crossref_api_key+'&format=unixref')
    xmldoc = minidom.parse(usock)
    usock.close()
 
    if debug:
        out=xmldoc.toxml()
        print(out)
    print("\n")
 
    a = xmldoc.getElementsByTagName("doi_records")[0]
    b = a.getElementsByTagName("doi_record")[0]
    c = b.getElementsByTagName("crossref")[0]
    d = c.getElementsByTagName("journal")[0]
 
    journal_meta = d.getElementsByTagName("journal_metadata")[0]
    journal_title = journal_meta.getElementsByTagName("full_title")[0]
    abbrev_title = journal_meta.getElementsByTagName("abbrev_title")[0]
    #pdb.set_trace()
    text_journal_title = journal_title.firstChild.data
    abbrev_journal_title = abbrev_title.firstChild.data
    journal_issue = d.getElementsByTagName("journal_issue")[0]
    date = journal_issue.getElementsByTagName("publication_date")[0]
    year = date.getElementsByTagName("year")[0]
    text_year = year.firstChild.data
    resource = d.getElementsByTagName("resource")[0]
    resource=resource.firstChild.data
 
    try:
        journal_volume = journal_issue.getElementsByTagName("journal_volume")[0]
        volume = journal_issue.getElementsByTagName("volume")[0]
        text_volume = volume.firstChild.data
    except IndexError:
        pass
 
    try:
        issue = journal_issue.getElementsByTagName("issue")[0]
        text_issue = issue.firstChild.data
    except IndexError:
        pass
 
    journal_article = d.getElementsByTagName("journal_article")[0]
    titles = journal_article.getElementsByTagName("titles")[0]
    title = titles.getElementsByTagName("title")[0]
    text_title = title.firstChild.data
 
    contributors = journal_article.getElementsByTagName("contributors")[0]
    for person_name in contributors.getElementsByTagName("person_name"):
        text_given_name = ""
        text_surname = ""
        # get names
        given_name = person_name.getElementsByTagName("given_name")[0]
        text_given_name = given_name.firstChild.data
        surname = person_name.getElementsByTagName("surname")[0]
        text_surname = surname.firstChild.data
        authorlist.append(text_surname+", "+text_given_name)
        #first author?
        sequence = person_name.attributes.getNamedItem("sequence")
        if sequence.nodeValue == 'first':
            text_first_author_surname = text_surname
 
    try:
        pages = journal_article.getElementsByTagName("pages")[0]
    except:
        pages = None
    try:
        first_page = pages.getElementsByTagName("first_page")[0]
        text_first_page = first_page.firstChild.data
    except:
        pass
    try:
        last_page = pages.getElementsByTagName("last_page")[0]
        text_last_page = last_page.firstChild.data
    except:
        pass
    # physical review
    if pages == None:
        try:
            pages = journal_article.getElementsByTagName("publisher_item")[0]
        except:
            pages = None
        try:
            first_page = pages.getElementsByTagName("item_number")[0]
            text_first_page = first_page.firstChild.data
        except:
            pass
 
    # output

    ascii_author=ascii_filt(text_first_author_surname)
    print("@ARTICLE{"+ascii_author+text_year[-2:]+",")
    print("author = {"+" and ".join(authorlist)+"},")
    print("title = {"+text_title+"},")
    print("journal = {"+abbrev_journal_title+"},")
    if not text_volume == "":
        print("volume = {"+text_volume+"},")
    if not text_issue == "":
        print("number = {"+text_issue+"},")
    print("year = {"+text_year+"},")
    if ((text_first_page != "") and (text_last_page != "")):
        print("pages = {"+text_first_page+"-"+text_last_page+"},")
    if ((text_first_page != "") and (text_last_page == "")):
        print("pages = {"+text_first_page+"},")
    print("doi = {"+doi+"},")
    print("resource = {http://ezproxy.library.ubc.ca/login?url="+resource+"},")
    print("wos = {{{}}},".format(wos_url))
    print("citing = {{{}}},".format(citing_url))
    print("related = {{{}}},".format(related_url))
    print("}")
