#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    if not l:
      return None
    diff = abs(l[0] - v)
    elnt = l[0]
    for n in l:
      if abs(n - v) < diff:
        elnt = n
        diff = abs(n - v)
    return elnt

    raise Failure("to be written")

def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    res = dict()
    for k, v in zip(keys, values):
      res[k] = v
    return res
    raise Failure("to be written")
   
# file IO functions
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
    f = open(fn, 'r')
    res = dict()
    for line in f:
      words = re.findall('([A-Za-z0-9_]+)', line) 
      words = [w.lower() for w in words]
      for w in words:
        if res.has_key(w) == False:
          res[w] = 1
        else:
          res[w] = res[w] + 1
    return res
    raise Failure("to be written")








