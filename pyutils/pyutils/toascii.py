import unicodedata, string
import numpy as np

def toascii(unicode_text):
    """convert the unicode string to plain ascii
       removing all the null bytes
    """
    oldtext=unicodedata.normalize('NFKC',unicode_text)
    #convert from unicode to ascii dropping characters
    #if needed
    text=oldtext.encode('ascii','ignore')
    #
    # still lots of null bytes in the file so
    # removing everything that isn't an ascii letter
    # (positions 32 to 126) or whitespace
    #
    allowed=chr(32)
    for i in np.arange(33,127):
        allowed = allowed + chr(i)
    allowed=allowed + '\t\n\x0b\x0c\r '  #formfeed, vertical tab
    deletions = ''.join(ch for ch in map(chr,range(256)) if ch not in allowed)
    #
    # table doesn't remap, just passes allowed to allowed
    #
    tab = string.maketrans(allowed,allowed)
    #
    # remove bad characters (everything except allowed)
    #
    final_ascii=string.translate(text, tab, deletions=deletions)
    return final_ascii
