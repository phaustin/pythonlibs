import sys

PY2 = sys.version_info[0] == 2
_identity = lambda x: x

if not PY2:
    unichr = chr
    range_type = range
    text_type = str
    string_types = (str,)

    implements_to_string = _identity

    ifilter = filter
    imap = map
    izip = zip
    import mutagenx
    from mutagenx.easymp4 import EasyMP4
    from mutagenx.mp3 import EasyMP3
    mutagen=mutagenx
    
else:
    unichr = unichr
    text_type = unicode
    range_type = xrange
    string_types = (str, unicode)
    import mutagen
    from mutagen.easymp4 import EasyMP4
    from mutagen.mp3 import EasyMP3

    def implements_to_string(cls):
        cls.__unicode__ = cls.__str__
        cls.__str__ = lambda x: x.__unicode__().encode('utf-8')
        return cls

    from itertools import imap, izip, ifilter
    
