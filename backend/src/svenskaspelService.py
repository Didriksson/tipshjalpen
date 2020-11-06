import requests
import json
import os
from dotenv import load_dotenv
import kupong

load_dotenv()
SV_API_KEY = os.environ.get('SV_API_KEY')

def fetchOpen(tips):
    r = requests.get('https://api.www.svenskaspel.se/external/draw/' + tips + '/draws?accesskey=' + SV_API_KEY)    

    if r.status_code is 404:
        print("Inget spel hittat mot stryktipset: " + str(r.status_code))
        return None

    if r.status_code is not 200:
        print("Fick felkod: " + str(r.status_code))
        return None    

    jsonDraws = json.loads(r.content.decode('UTF-8'))
    if len(jsonDraws['draws']) == 0:
        return None
    
    k = kupong.Kupong(jsonDraws['draws'][0]['drawComment'])
    for event in jsonDraws['draws'][0]['events']:    
        rad = kupong.KupongRad()
        rad.hemmalag = event['participants'][0]['name']
        rad.bortalag = event['participants'][1]['name']        
        rad.liga = event['league']['name']

        if event['distribution']:
            folket = kupong.SvenskaFolket()
            folket.hemmalag = event['distribution']['home']
            folket.kryss = event['distribution']['draw']
            folket.bortalag = event['distribution']['away']
            rad.svenskaFolket = folket
        
        if event['odds']:
            odds = kupong.Odds()
            odds.hemmalag = event['odds']['home']
            odds.kryss = event['odds']['draw']
            odds.bortalag = event['odds']['away']
            rad.odds = odds

        k.addMatch(rad)        
    return k
    
def fetchOpenStryktipset():
    return fetchOpen('stryktipset')

def fetchOpenEuropatipset():
    return fetchOpen('europatipset')

def fetchOpenTopptipset():
    return fetchOpen('topptipset')