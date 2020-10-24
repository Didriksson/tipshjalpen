import statistics
from scipy.stats import poisson,skellam
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

def avgScoredHomeForLeague(matcher):
    return statistics.mean(list(map(lambda x: x.fullTime[0], matcher)))

def avgScoredAwayForLeague(matcher):
    return statistics.mean(list(map(lambda x: x.fullTime[1], matcher)))

def avgConcededHomeForLeague(matcher):
    return avgScoredAwayForLeague(matcher)

def avgConcededAwayForLeague(matcher):
    return avgScoredHomeForLeague(matcher)

def avgGoalsScoredForTeamAtHome(lag, matcher):
    return statistics.mean(list(map(lambda y: y.fullTime[0],filter(lambda x: lag in x.homeTeam, matcher))))    

def avgGoalsScoredForTeamAway(lag, matcher):
    return statistics.mean(list(map(lambda y: y.fullTime[1],filter(lambda x: lag in x.awayTeam, matcher))))    

def homeTeamAttackingStrength(lag, matcher):
    return avgGoalsScoredForTeamAtHome(lag, matcher)/avgScoredHomeForLeague(matcher)

def awayTeamAttackingStrength(lag, matcher):
    return avgGoalsScoredForTeamAway(lag, matcher)/avgScoredAwayForLeague(matcher)

def avgGoalsConcededAwayForTeamAway(lag, matcher):
    return statistics.mean(list(map(lambda y: y.fullTime[0],filter(lambda x: lag in x.awayTeam, matcher))))    

def avgGoalsConcededHomeForTeamHome(lag, matcher):
    return statistics.mean(list(map(lambda y: y.fullTime[1],filter(lambda x: lag in x.homeTeam, matcher))))    

def awayTeamDefenceStrength(lag, matcher):
    return avgGoalsConcededAwayForTeamAway(lag, matcher)/avgConcededAwayForLeague(matcher)

def homeTeamDefenceStrength(lag, matcher):
    return avgGoalsConcededHomeForTeamHome(lag, matcher)/avgConcededHomeForLeague(matcher)

def projectedExpectedHomeTeamGoals(hometeam, awayteam, matcher):
    return homeTeamAttackingStrength(hometeam, matcher) * awayTeamDefenceStrength(awayteam, matcher) * avgScoredHomeForLeague(matcher)

def projectedExpectedAwayTeamGoals(hometeam, awayteam, matcher):
    return awayTeamAttackingStrength(awayteam, matcher) * homeTeamDefenceStrength(hometeam, matcher) * avgScoredAwayForLeague(matcher)

def getPoissonForMatch(home, away, alla_matcher):
    if alla_matcher == []:
        print("Inga matcher hittade...")
        return PoissonResult(None, None)

    alla_matcher = list(filter(lambda y: y.fullTime, alla_matcher))
    projectedHomeGoals = projectedExpectedHomeTeamGoals(home, away, alla_matcher)
    projectedAwayGoals = projectedExpectedAwayTeamGoals(home, away, alla_matcher)
    homeProjectedPerGoal = []
    awayProjectedPerGoal = []
    for x in range(6):
        homeProjectedPerGoal.append(poisson.pmf(x, projectedHomeGoals))
        awayProjectedPerGoal.append(poisson.pmf(x, projectedAwayGoals))
    print("Home: " + str(projectedHomeGoals))
    print("Away: " + str(projectedAwayGoals))
    return PoissonResult(homeProjectedPerGoal, awayProjectedPerGoal)