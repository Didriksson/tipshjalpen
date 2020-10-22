import csv
from dataclasses import dataclass
import re
import datetime


@dataclass
class Match:
    date: datetime.date
    homeTeam: str
    awayTeam: str
    fullTime: tuple
    halfTime: tuple
    liga: str

    def gjordaMal(self, lag) -> int:
        if lag in self.homeTeam:
            return self.fullTime[0]
        else:
            return self.fullTime[1]

    def inslapptaMal(self, lag) -> int:
        if lag in self.homeTeam:
            return self.fullTime[1]
        else:
            return self.fullTime[0]

    def resultatForLag(self, lag) -> str:
        homescore, awayscore = self.fullTime
        if homescore == awayscore:
            return "D"
        elif homescore > awayscore:
            if lag in self.homeTeam:
                return 'W'
            else:
                return "L"
        else:
            if lag in self.homeTeam:
                return 'L'
            else:
                return "W"

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
ligor = {
    "PL_2020/21": "data/england/1-premierleague.txt",
    "PL_2019/20": "data/england/1-premierleague_20192020.txt",
    "PL_2015/16": "data/england/1-premierleague_20152016.txt",
    "Championship_2020/21": "data/england/2-championship.txt",
    "Allsvenskan_2020": "data/sverige/1-allsvenskan.txt",
    "Superettan_2020" : "data/sverige/2-superettan.txt",
    "CL_20192020": "data/championsleague/cl_20192020.txt"
}

def matcherPL():    
    return allaMatcherForSasong("PL_2019/20") + allaMatcherTillDatum(datetime.date.today().year, datetime.date.today().month, datetime.date.today().day, "PL_2020/21")

def matcherCL():    
    return allaMatcherForSasong("CL_20192020")

def matcherCH():    
    return allaMatcherForSasong("Championship_2020/21")


def parseFile(file, ligatitel):
    f = open(file, encoding='utf8')
    matches = []
    datum = None
    aretRunt = False
    ar = 2020

    for line in f.readlines():
        if line.startswith("="):
            arReg = re.search("\d\d\d\d", line)
            ar = int(arReg.group(0))
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

            match = Match(datum, home, away, fullTime, halfTime, ligatitel)
            matches.append(match)
    return matches

def allaMatcherTillDatum(ar, manad, dag, liga):
    datum = datetime.date(ar, manad, dag)
    matcher = allaMatcherForSasong(liga)
    return list(filter(lambda x: x.date < datum ,matcher))

def allaMatcherTillDatumForLag(ar, manad, dag, liga):
    datum = datetime.date(ar, manad, dag)
    matcher = allaMatcherForSasong(liga)
    return list(filter(lambda x: x.date < datum ,matcher))

def allaMatcherForSasong(liga):
    return parseFile(ligor.get(liga), liga)

def allaMatcherForLag(lag):
    results = []
    for liga in ligor.keys():       
        for match in allaMatcherForSasong(liga):
            if lag in match.homeTeam or lag in match.awayTeam:
                results.append(match)
    return results

def resultatForLag(lag):
    return list(filter(lambda x: x.fullTime, allaMatcherForLag(lag)))
