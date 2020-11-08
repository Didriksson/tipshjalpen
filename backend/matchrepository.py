import sqlite3
conn = sqlite3.connect('matches.db')
c = conn.cursor()

# Create table
c.execute('''CREATE TABLE IF NOT EXISTS matches
             (date date, home_team text, away_team text, home_ft integer,away_ft integer, home_ht integer, away_ht, league text
              , PRIMARY KEY(date, home_team, away_team)             
             )''')    

def insertMatch(match):
    if match.halfTime == None:
        home_ht = None
        away_ht = None
    else:
        home_ht = match.halfTime[0]
        away_ht = match.halfTime[1]

    c.execute("SELECT count(date) FROM matches WHERE date=? AND home_team=? AND away_team=?;", (match.date, match.homeTeam, match.awayTeam))
    if(len(c.fetchone())):
        print("Found match " + match.homeTeam  + " - " + match.awayTeam + " already inserted - ignoring")
        return

    c.execute("INSERT INTO matches VALUES(?, ?, ?, ?, ?, ?, ?, ?)", 
        (match.date,
        match.homeTeam,
        match.awayTeam,
        match.fullTime[0],
        match.fullTime[1],
        home_ht,
        away_ht,
        match.liga))
    conn.commit()
    print("Match added to table matches: ", c.fetchone())