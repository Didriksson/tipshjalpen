import requests
import json
import match
import re
import datetime
import matchrepository

monthDict = {
  "Jan": 1,
  "Feb": 2,
  "Mar": 3,
  "Apr": 4,
  "May": 5,
  "Jun": 6,
  "Jul": 7,
  "Aug": 8,
  "Sep": 9,
  "Oct": 10,
  "Nov": 11,
  "Dec": 12,
}


# https://github.com/openfootball/europe/blob/master/sweden/2020/1-allsvenskan.txt
# Repoklasserna blir tyvärr lite sådär...men TXT-filerna är bara ganska lika. Det skapar trubbel...

def parseAllsvenskan2020():
    return parseFile("src/data/sverige/1-allsvenskan.txt", 2020)

def parseSuperettan2020():
    return parseFile("src/data/sverige/2-superettan.txt", 2020)

def parseFile(file, ar):
    f = open(file, encoding='utf8')
    matches = []
    datum = None
    ligatitel = ""
    for line in f.readlines():
        if line.startswith("="):
            ligatitel = line[2:].strip()
            continue

        line = line.lstrip()
        datumRegex = re.search('\[.+\]', line)
        if datumRegex:
            month, day = datumRegex.group(0)[5:-1].split("/")
            if monthDict.get(month) == 1:
                if not aretRunt:
                    aretRunt = True
                    ar += 1

            datum = datetime.date(ar, monthDict.get(month), int(day))
        if len(line.split()) > 4:
            halfTime = None
            fullTime = None
            #Tar bort inledande nuffror
            substrings = re.split("  +", re.sub("\d\d.\d\d\s+", "", line))
            home = substrings[0].strip()
            away = substrings[2].strip()
            scoreString = substrings[1].split()
            if len(scoreString) > 1:
                halfTime = list(map(int, scoreString[1][1:-1].split("-")))
        
            if scoreString[0] != '-':
                fullTime = list(map(int, scoreString[0].split("-")))

            m = match.Match(datum, home, away, fullTime, halfTime, ligatitel)
            matches.append(m)
    return matches