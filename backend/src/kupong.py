from dataclasses import dataclass
import poissonresult

@dataclass
class PredictedScore:
    hemmalag : int
    bortalag : int

@dataclass(init=False)
class Analys:
    predictedScore : PredictedScore

    def predictedScore(self, hemmalag, bortalag):
        self.predictedScore = PredictedScore(hemmalag, bortalag)

@dataclass(init=False)
class Odds:
    hemmalag : str
    kryss : str
    bortalag : str

@dataclass(init=False)
class SvenskaFolket:
    hemmalag : str
    kryss : str
    bortalag : str

@dataclass(init=False)
class KupongRad:
    hemmalag : str
    bortalag : str
    liga : str
    svenskaFolket : SvenskaFolket
    odds : Odds
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