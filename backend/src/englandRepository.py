import requests
import json
import match

# https://github.com/openfootball/football.json/tree/master/2020-21


def fetch_Ch_2020_21():
    data = fetch_data("https://raw.githubusercontent.com/openfootball/football.json/master/2020-21/en.2.json")
    return parse_json(data)

def fetch_PL_2020_21():
    data = fetch_data("https://raw.githubusercontent.com/openfootball/football.json/master/2020-21/en.1.json")
    return parse_json(data)

def fetch_data(url):
    response = requests.get(url)
    if response.status_code != 200:
        print("Could not fetch data.", response.status_code)    
        exit(1)

    return json.loads(response.content)


def parse_json(json):
    matches = []
    for m in json['matches']:
        matches.append(match.Match(m.get("date"), m.get("team1"), m.get("team2"), m.get("score", {}).get("ft"), m.get("score", {}).get("ht"), json["name"]))
    return matches
