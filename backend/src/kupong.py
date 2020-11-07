from dataclasses import dataclass
import poissonresult


@dataclass(init=False)
class MatchinfoHallare:
    hemmalag : str
    kryss : str
    bortalag : str

@dataclass
class PredictedScore:
    hemmalag : int
    bortalag : int

@dataclass(init=False)
class Analys:
    predictedScore : PredictedScore
    outcomePercentage : MatchinfoHallare

    def predictedScore(self, hemmalag, bortalag):
        self.predictedScore = PredictedScore(hemmalag, bortalag)
    def outcomePercentage(self, hemmalag, kryss, bortalag):
        self.outcomePercentage= MatchinfoHallare(hemmalag, kryss, bortalag)

@dataclass(init=False)
class KupongRad:
    hemmalag : str
    bortalag : str
    liga : str
    svenskaFolket : MatchinfoHallare
    odds : MatchinfoHallare
    analys : Analys

@dataclass
class Kupong:
    namn: str
    matcher: []

    def __init__(self, namn : str):
        self.matcher = []
        self.namn = namn
    
    def addMatch(self, match):
        self.matcher.append(match)