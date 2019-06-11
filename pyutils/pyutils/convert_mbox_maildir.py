#!/usr/bin/env python3

# http://www.hackvalue.nl/en/article/109/migrating%20from%20mbox%20to%20maildir

import datetime
import email
import email.Errors
import mailbox
import os
import sys
import time


def msgfactory(fp):
    try:
        return email.message_from_file(fp)
    except email.Errors.MessageParseError:
        # Don't return None since that will
        # stop the mailbox iterator
        return ''
dirname = sys.argv[1]
inbox = sys.argv[2]
fp = open(inbox, 'rb')
mbox = mailbox.UnixMailbox(fp, msgfactory)


try:
        storedir = os.mkdir(dirname, 0o750)
        os.mkdir(dirname + "/new", 0o750)
        os.mkdir(dirname + "/cur", 0o750)
except:
        pass

count = 0
for mail in mbox:
        count+=1
        #hammertime = time.time() # mail.get('Date', time.time())
        hammertime = datetime.datetime(*email.utils.parsedate(mail.get('Date',''))[:7]).strftime('%s')
        hostname = 'mb2mdpy'
        filename = dirname + "/cur/%s%d.%s:2,S" % (hammertime, count, hostname)
        mail_file = open(filename, 'w+')
        mail_file.write(mail.as_string())


print("Processed {0} mails".format(count))

