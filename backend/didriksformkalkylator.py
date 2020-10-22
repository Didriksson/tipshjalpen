resultatViktaren = {
    "W" : 3,
    "D" : 1,
    "L" : 0
}

def vadArFormen(lag, matcher):
    resultatFormPoäng = formForSenasteMatcherna(lag, matcher)
    målskillnadspoäng = målskillnadssnitt(lag, matcher) * 2
    poäng = resultatFormPoäng + målskillnadspoäng
    return poäng

def målskillnadssnitt(lag, matcher):
    gjorda = 0
    inslappta = 0
    for match in matcher:
        gjorda = gjorda + match.gjordaMal(lag)
        inslappta = inslappta + match.inslapptaMal(lag)
    return (gjorda - inslappta) / len(matcher)

def formForSenasteMatcherna(lag, matcher):
    raknare = len(matcher)
    poängFörSenastematcherna = 0
    for match in matcher:
        resultatForlag = match.resultatForLag(lag)
        poang = resultatViktaren.get(resultatForlag)
        formPoangForMatch = poang / (1+((raknare*10)/100.0))
        raknare = raknare - 1
        poängFörSenastematcherna = poängFörSenastematcherna + formPoangForMatch
    return poängFörSenastematcherna