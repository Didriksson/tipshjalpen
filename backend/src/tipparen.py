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
        matcher = resultatslangaren.matcherCh()
    try:
        result = poissondistribution.getPoissonForMatch(hemma, borta, matcher)
        print(hemma, "-", borta, ": ", result.getMostProbableScore())
    except:
        e = sys.exc_info()[0]
        print("Fångat fel vid resultat för match", hemma, "-", borta, "antal matcher: ", len(matcher), "fel", e)

tippaMatch("Manchester United", "Chelsea", "PL")
tippaMatch("Liverpool","Sheffield U", "PL")
tippaMatch("Fulham", "Crystal P", "PL")
tippaMatch("Cardiff", "Middlesb", "Ch")
tippaMatch("Bristol C", "Swansea", "Ch")
tippaMatch("Coventry", "Blackburn", "Ch")
tippaMatch("Huddersfield", "Preston", "Ch")
tippaMatch("Stoke", "Brentford", "Ch")
tippaMatch("Millwall", "Barnsley", "Ch")
tippaMatch("Queens Park Rangers", "Birmingham", "Ch")
tippaMatch("Norwich", "Wycombe", "Ch")
tippaMatch("Reading", "Rotherham", "Ch")
tippaMatch("Sheffield W", "Luton", "Ch")