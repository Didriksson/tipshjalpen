import footballdataService


def fetch_Ch_2020_21():
    return footballdataService.getMatchesForLeague('ELC')

def fetch_PL_2020_21():
    return footballdataService.getMatchesForLeague('PL')

