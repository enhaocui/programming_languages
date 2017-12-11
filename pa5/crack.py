
from misc import *
import crypt
import re

def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""
    f = open(filename, 'r')
    res = []
    for line in f:
      words = re.findall(regexp, line)
      res.extend(words)
    return res
    raise Failure("to be written")

def transform_reverse(str):
    return [str, str[::-1]]
    raise Failure("to be written")

def transform_capitalize(str):
    res = []
    helper(str, res, 0)
    return res
    raise Failure("to be written")

def helper(str, res, start):
    if start == len(str):
      res.append(str)
      return
    res.append(str)
    for i in range(start, len(str)):
      if (str[i].islower()):
        c = str[i]
        helper(str[:i] + c.upper() + str[(i+1):], res, i + 1)
      if (str[i].isupper()):
        c = str[i]
        helper(str[:i] + c.lower() + str[(i+1):], res, i + 1)
    return

def transform_digits(str):
    dic = {'o':['0'],'z':['2'],'a':['4'],'b':['6','8'],'i':['1'],
    'l':['1'],'e':['3'],'s':['5'],'t':['7'],'g':['9'],'q':['9']}
    res = []
    replace(str, res, 0, dic)
    return res
    raise Failure("to be written")

def replace(str, res, start, dic):
    if (start == len(str)):
      res.append(str)
      return
    res.append(str)
    for i in range(start, len(str)):
      c = str[i].lower()
      if dic.has_key(c):
        for t in dic[c]:
          replace(str[:i] + t + str[(i+1):], res, i + 1, dic)
    return

def check_pass(plain,enc):
    """Check to see if the plaintext plain encrypts to the encrypted
       text enc"""
    return enc == crypt.crypt(plain, enc[0:2])
    raise Failure("to be written")

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    res = []
    f = open(filename, 'r')
    for line in f:
      temp = re.split(':', line)
      temp = [t.rstrip('\n') for t in temp]
      res.append(generate_dict(temp))
    return res
    raise Failure("to be written")

def generate_dict(str):
    dic = {'account':str[0],'password':str[1],'UID':str[2],'GID':str[3],'GECOS':str[4],'dictionary':str[5],'shell':str[6]}
    return dic

def crack_pass_file(pass_filename,words_filename,out_filename):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
    passwd = load_passwd(pass_filename)
    words = load_words(words_filename, r'([A-Za-z0-9_]+)$')
    out = open(out_filename, 'w')
    copy = []
    for ps in passwd:
      for w in words:
        if check_pass(w, ps['password']):
          out.write(ps['account'] + '=' + w + '\n')
          out.flush()
          print w
          break
        rev = transform_reverse(w)[1]
        if check_pass(rev, ps["password"]):
          out.write(ps["account"] + "=" + rev + "\n")
          out.flush()
          print rev
          break
        copy.append(ps)

    if len(copy) == 0:
      return

    passwd = copy

    copy = []
    for ps in passwd:
      for w in words:
        for c in transform_capitalize(w):
          if check_pass(c, ps['password']):
            out.write(ps['account'] + '=' + c + '\n')
            out.flush()
            print c
            break
          rev = transform_reverse(c)[1]
          if check_pass(rev, ps["password"]):
            out.write(user["account"] + "=" + rev + "\n")
            out.flush()
            print rev
            break
          copy.append(ps)

    if len(copy) == 0:
      return

    passwd = copy

    copy = []
    for ps in passwd:
      for w in words:
        for c in transform_digits(w):
          if check_pass(c, ps['password']):
            out.write(ps['account'] + '=' + c + '\n')
            out.flush()
            print c
            break
          rev = transform_reverse(c)[1]
          if check_pass(rev, ps["password"]):
            out.write(user["account"] + "=" + rev + "\n")
            out.flush()
            print rev
            break
          copy.append(ps)

    if len(copy) == 0:
      return

    passwd = copy

    for ps in passwd:
      for w in words:
        ok = False
        for c in transform_digits(w):
          if ok:
            break
          for d in transform_capitalize(c):
            if check_pass(d, ps['password']):
              out.write(ps['account'] + '=' + d + '\n')
              out.flush()
              ok = True
              print d
              break
            rev = transform_reverse(d)[1]
            if check_pass(rev, user["password"]):
              out.write(user["account"] + "=" + rev + "\n")
              out.flush()
              ok = True
              print rev
              break
    out.close()
    return

    raise Failure("to be written")

