import didriksformkalkylator
import resultatslangaren
import poissondistribution
import sys

#hemmalag = "Brage"
#matcher = resultatslangaren.resultatForLag(hemmalag)
#hemmalagForm = didriksformkalkylator.vadArFormen(hemmalag, matcher[-7:-2])

#bortalag = "Öster"
#matcher = resultatslangaren.resultatForLag(bortalag)
#bortalagForm = didriksformkalkylator.vadArFormen(bortalag, matcher[-7:-2])
#print("Form", hemmalag, hemmalagForm, " Bortalag:", bortalag, bortalagForm, "Skillnad mellan lagen:" , abs(hemmalagForm - bortalagForm))

def tippaMatch(hemma, borta, liga):
    matcher = []
    if liga == "PL":
        matcher = resultatslangaren.matcherPL()
    if liga == "CL":
        matcher = resultatslangaren.matcherCL()
    if liga == "Ch":
        matcher = resultatslangaren.matcherCH()
    try:
        result = poissondistribution.getPoissonForMatch(hemma, borta, matcher)
        print(hemma, "-", borta, ": ", result.getMostProbableScore())
    except:
        e = sys.exc_info()[0]
        print("Fångat fel vid resultat för match", hemma, "-", borta, "antal matcher: ", len(matcher), "fel", e)

tippaMatch("Real Madrid", "Shaktar", "CL")
tippaMatch("Ajax", "Liverpool", "CL")
tippaMatch("Bayern München", "Atlético Madrid", "CL")
tippaMatch("Internazionale", "gladbach", "CL")
tippaMatch("Salzburg", "Lokomotiv Moskva", "CL")
tippaMatch("Manchester City", "Porto", "CL")
tippaMatch("Midjylland", "Atalanta", "CL")
tippaMatch("Olympiacos", "Marseille", "CL")
tippaMatch("Cardiff", "Bournemouth", "Ch")
tippaMatch("Sheffield Wednesday", "Brentford", "Ch")
tippaMatch("Queens Park Rangers", "Preston", "Ch")
tippaMatch("Stoke", "Barnsley", "Ch")
tippaMatch("Watford", "Blackburn", "Ch")