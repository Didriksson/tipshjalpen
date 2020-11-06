from dataclasses import dataclass

@dataclass
class PoissonResult:
    homeProjected : []
    awayProjected : []

    def getProjectedForHome(self, goals):
        if len(self.homeProjected) < goals:
            raise "Problem! Du har angett fler mål än projectionen täcker."
        return self.homeProjected[goals]

    def getProjectedForAway(self, goals):
        if len(self.awayProjected) < goals:
            raise "Problem! Du har angett fler mål än projectionen täcker."
        return self.awayProjected[goals]

    def getWinDrawWin(self):
        home = 0
        draw = 0
        away = 0
        for x in range(len(self.homeProjected)):
            for y in range(len(self.awayProjected)):
                probability = self.homeProjected[x] * self.awayProjected[y]
                if x > y:
                    home += probability
                elif x == y:
                    draw += probability
                else:
                     away += probability
        return home,draw,away
                    

    def getMostProbableScore(self):
        highestProb = 0.0
        highestScorest = [-1, -1]
        for x in range(len(self.homeProjected)):
            for y in range(len(self.awayProjected)):
                probability = self.homeProjected[x] * self.awayProjected[y]
                if probability > highestProb:
                    highestProb = probability
                    highestScorest = [x, y]
        return highestScorest, highestProb