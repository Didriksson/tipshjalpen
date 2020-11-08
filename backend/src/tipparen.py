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
    "Queens PR" : "Queens Park Rangers FC",
    "Bayer Leverkusen" : "Bayer 04 Leverkusen" 
}

def skrivOmSvenskaspelsLagnamn(lag, matcher):
    if lag in svenskaSpelsLagnamn:
        for m in matcher:
            if m.homeTeam == svenskaSpelsLagnamn[lag]:
                m.homeTeam = lag
            if m.awayTeam == svenskaSpelsLagnamn[lag]:
                m.awayTeam = lag
    return matcher        



def tippaMatch(hemma, borta, liga):
    print("Analyserar", hemma, "-", borta,)
    matcher = resultatslangaren.matcher(liga)
    try:
        matcher = skrivOmSvenskaspelsLagnamn(hemma, matcher)
        matcher = skrivOmSvenskaspelsLagnamn(borta, matcher)
        result = poissondistribution.getPoissonForMatch(hemma, borta, matcher)
        print("Klar")
        return result
    except:
        e = sys.exc_info()[0]
        print("Fångat fel vid resultat för match", hemma, "-", borta, "antal matcher: ", len(matcher), ",fel fångat", e)
