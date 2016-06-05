#http://stackoverflow.com/questions/16694907/how-to-download-large-file-in-python-with-requests-py

# f.flush() doesn't flush data to physical disk. It transfers
# the data to OS. Usually, it is enough unless there is a power
# failure. f.flush() makes the code slower here for no reason. The
# flush happens when the correponding file buffer (inside app) is
# full. If you need more frequent writes; pass buf.size parameter to
# open(). – J.F. Sebastian Sep 28 '15 at 19:08

import requests

def download_file(url):
    local_filename = url.split('/')[-1]
    # NOTE the stream=True parameter
    r = requests.get(url, stream=True)
    with open(local_filename, 'wb') as f:
        count=0
        for chunk in r.iter_content(chunk_size=1024):
            #print('reading chunk: {}'.format(count))
            count += 1
            if chunk: # filter out keep-alive new chunks
                f.write(chunk)
                #f.flush() commented by recommendation from J.F.Sebastian
    return local_filename
