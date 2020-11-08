import svenskaspelService
import jsonpickle
import tipparen
import kupong as Kupong

from flask import Flask
app = Flask(__name__)

@app.route('/hello/<name>')
def hello(name):
    return 'Hello ' + name + '!'

@app.route('/hamtaKupong')
def hamtarKupong():
    kupong = svenskaspelService.fetchOpenStryktipset()
    if kupong == None:
        kupong = svenskaspelService.fetchOpenEuropatipset()
    if kupong == None:
        kupong = svenskaspelService.fetchOpenTopptipset()

    for rad in kupong.matcher:
        result = tipparen.tippaMatch(rad.hemmalag, rad.bortalag, rad.liga)
        if result:
            analys = Kupong.Analys()
            rad.analys = analys        
            analys.predictedScore(result.getMostProbableScore()[0][0], result.getMostProbableScore()[0][1])
            analys.outcomePercentage(result.getWinDrawWin()[0], result.getWinDrawWin()[1], result.getWinDrawWin()[2])
    jsonpickle.set_preferred_backend('json')
    jsonpickle.set_encoder_options('json', ensure_ascii=False)
    return jsonpickle.encode(kupong, unpicklable=False)


print(hamtarKupong())