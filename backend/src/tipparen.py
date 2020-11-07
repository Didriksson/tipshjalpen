import didriksformkalkylator
import resultatslangaren
import poissondistribution
import footballdataService
import sys
import json

#hemmalag = "Brage"
#matcher = resultatslangaren.resultatForLag(hemmalag)
#hemmalagForm = didriksformkalkylator.vadArFormen(hemmalag, matcher[-7:-2])

#bortalag = "Öster"
#matcher = resultatslangaren.resultatForLag(bortalag)
#bortalagForm = didriksformkalkylator.vadArFormen(bortalag, matcher[-7:-2])
#print("Form", hemmalag, hemmalagForm, " Bortalag:", bortalag, bortalagForm, "Skillnad mellan lagen:" , abs(hemmalagForm - bortalagForm))

# Svenskaspels namn -> Datat
svenskaSpelsLagnamn = {
    "Queens PR" : "Queens Park Rangers FC"
}

def skrivOmSvenskaspelsLagnamn(lag, matcher):
    if lag in svenskaSpelsLagnamn:
        for m in matcher:
            if m.homeTeam == svenskaSpelsLagnamn[lag]:
                m.homeTeam = lag
            if m.awayTeam == svenskaSpelsLagnamn[lag]:
                m.awayTeam = lag
    return matcher        


ligaCache = {}

def tippaMatch(hemma, borta, liga):
    f = open("fd_matches_championship.txt", "r")
    matchesJson = json.load(f)
    f.close()
    matchesCh = footballdataService.parseJson(matchesJson)
    ligaCache['Championship'] = matchesCh

    f = open("fd_matches_premierleague.txt", "r")
    matchesJson = json.load(f)
    f.close()
    matchesPl = footballdataService.parseJson(matchesJson)
    ligaCache['Premier League'] = matchesPl

    matcher = []
    if liga in ligaCache:
        matcher = ligaCache[liga]
    else:    
        if liga == "Premier League":
            matcher = resultatslangaren.matcherPL()
            ligaCache[liga] = matcher
        if liga == "CL":
            matcher = resultatslangaren.matcherCL()
            ligaCache[liga] = matcher
        if liga == "Championship":
            matcher = resultatslangaren.matcherCh()
            ligaCache[liga] = matcher
#    try:
    matcher = skrivOmSvenskaspelsLagnamn(hemma, matcher)
    matcher = skrivOmSvenskaspelsLagnamn(borta, matcher)
    result = poissondistribution.getPoissonForMatch(hemma, borta, matcher)
    print(hemma, "-", borta, ": ", result.getMostProbableScore())
    return result
 #   except:
 #       e = sys.exc_info()[0]
 #       print("Fångat fel vid resultat för match", hemma, "-", borta, "antal matcher: ", len(matcher), ",fel fångat", e)



