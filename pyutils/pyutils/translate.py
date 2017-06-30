#http://stackoverflow.com/questions/638893/what-is-the-most-efficient-way-in-python-to-convert-a-string-to-all-lowercase-str
import string
letter_set = frozenset(string.ascii_lowercase + string.ascii_uppercase)
## tab = string.maketrans(string.ascii_lowercase + string.ascii_uppercase,
##                        string.ascii_lowercase * 2)
tab = string.maketrans(string.ascii_lowercase + string.ascii_uppercase,
                       string.ascii_lowercase + string.ascii_uppercase)


deletions = ''.join(ch for ch in map(chr,range(256)) if ch not in letter_set)

## from string import maketrans, translate
## >>> table = maketrans('', '')
## >>> translate(orig, table, table[128:])[/color][/color][/color]

def test_translate(s):
    return string.translate(s, tab, deletions=deletions)
