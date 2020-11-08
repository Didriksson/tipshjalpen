from dataclasses import dataclass
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
