# taken from https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Python

# Christopher P. Matthews
# christophermatthews1985@gmail.com
# Sacramento, CA, USA

def levenshtein(s, t):
        ''' From Wikipedia article; Iterative with two matrix rows. '''
        if s == t: return 0
        elif len(s) == 0: return len(t)
        elif len(t) == 0: return len(s)
        v0 = [None] * (len(t) + 1)
        v1 = [None] * (len(t) + 1)
        for i in range(len(v0)):
            v0[i] = i
        for i in range(len(s)):
            v1[0] = i + 1
            for j in range(len(t)):
                cost = 0 if s[i] == t[j] else 1
                v1[j + 1] = min(v1[j] + 1, v0[j + 1] + 1, v0[j] + cost)
            for j in range(len(v0)):
                v0[j] = v1[j]
                
        return v1[len(t)]

import urllib.request
# original url
url = 'https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt'
with urllib.request.urlopen(url) as f:
    words = f.readlines()
words = [word.decode('UTF8').strip() for word in words]
lengths = [len(word) for word in words]

def find_funny(target):
    print('*******************************************')
    print(target)
    print('*******************************************')
    splittarget = target.lower().split()
    for i, w in enumerate(splittarget):
        if len(w) > 3:
            maxl = len(w) + 1
            minl = len(w) - 1
            for j, w2 in enumerate(words):
                if lengths[j] <= maxl and lengths[j] >= minl:
                    if levenshtein(w, w2) == 1:
                        # print the new string
                        pun = splittarget.copy()
                        pun[i] = w2
                        print(" ".join(pun))

find_funny('creative writing')
find_funny('International Relations')
find_funny('Public Policy and Service')
find_funny('visual arts')
find_funny('urban teaching')
find_funny('law letters and society')
find_funny('ecology and evolution')
find_funny('cancer biology')


