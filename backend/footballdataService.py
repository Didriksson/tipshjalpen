import http.client
import match
import json
from dotenv import load_dotenv
import os

load_dotenv()
FD_API_KEY = os.environ.get('FOOTBALL_DATA_KEY')

def getMatchesForLeague(league):
    print("Hämtar för liga:" + league)
    connection = http.client.HTTPConnection('api.football-data.org')
    headers = { 'X-Auth-Token': FD_API_KEY }
    connection.request('GET', '/v2/competitions/'+league+"/matches", None, headers )
    responseObject = connection.getresponse()
    print(responseObject.status)
    if responseObject.status != 200:
        print("Fick oväntad returkod från api.football-data:", responseObject.status)
        return []

    response = json.loads(responseObject.read().decode())
    return parseJson(response)

def parseJson(matchesJson):
    matches = []
    competition = matchesJson['competition']['name']
    for m in matchesJson['matches']:
        if m['status'] == 'FINISHED':
            matches.append(match.Match(m['utcDate'], m['homeTeam']['name'], m['awayTeam']['name'], [m['score']['fullTime']['homeTeam'], m['score']['fullTime']['awayTeam']], [m['score']['halfTime']['homeTeam'], m['score']['halfTime']['awayTeam']], competition))
    return matches