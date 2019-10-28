from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import csv
import re
import time
import pandas as pd

#set chrome driver
driver = webdriver.Chrome(r'\ChromeDriver\chromedriver.exe')
#get nba team stats page for traditional stats
driver.get("https://stats.nba.com/players/advanced")

player_ids = []
player_names = []
player_stats = []
player_team = []
player_season = []



#pulls seasons 2018 - 2000
for i in range(2,21):
    #need to find appropriate year and regular season stats
    driver.find_element_by_xpath('/html/body/main/div[2]/div/div[2]/div/div/div[1]/div[1]/div/div/label/select/option[' +str(i) +']').click()
    wait_table = WebDriverWait(driver, 20)
    driver.find_element_by_xpath('/html/body/main/div[2]/div/div[2]/div/div/div[1]/div[2]/div/div/label/select/option[2]').click()
    wait_all = WebDriverWait(driver,20)
    wait_table.until(EC.presence_of_element_located((By.CLASS_NAME,'nba-stat-table__overflow')))
    driver.find_element_by_xpath('/html/body/main/div[2]/div/div[2]/div/div/nba-stat-table/div[1]/div/div/select/option[1]').click()
    #pause for browser to load
    ##find season
    season = driver.find_element_by_xpath('/html/body/main/div[2]/div/div[2]/div/div/div[1]/div[1]/div/div/label/select/option[' + str(i) + ']').text
    ##stats data table
    table = wait_table.until(EC.presence_of_element_located((By.CLASS_NAME,'nba-stat-table__overflow'))).text
    #split table into column headers, team names, and stats then append to list
    for line_id, lines in enumerate(table.split('\n')):
        if line_id==0:
            column_names = lines.split(' ')[1:]
        else:
            if line_id % 3 == 1:
                player_season.append(season)
                player_ids.append(lines)
            if line_id % 3 == 2:
                player_names.append(lines)
            if line_id % 3 == 0:
                player_stats.append([i for i in lines.split(' ')])

#add stats to pandas data frame
teamAdata = pd.DataFrame({ 'Season': player_season,
                    'Player': player_names,
                    'Team': [i[0] for i in player_stats],
                    'Age': [float(i[1]) for i in player_stats],
                    'GP': [float(i[2]) for i in player_stats],
                    'W': [float(i[3]) for i in player_stats],
                    'L': [float(i[4]) for i in player_stats],
                    'Min': [float(i[5]) for i in player_stats],
                    'OFFRTG': [float(i[6]) for i in player_stats],
                    'DEFRTG': [float(i[7]) for i in player_stats],
                    'NETRTG': [float(i[8]) for i in player_stats],
                    'AST_PCT': [float(i[9]) for i in player_stats],
                    'AST/TO': [float(i[10]) for i in player_stats],
                    'AST_RATIO': [float(i[11]) for i in player_stats],
                    'OREB_PCT': [float(i[12]) for i in player_stats],
                    'DREB_PCT': [float(i[13]) for i in player_stats],
                    'REB_PCT': [float(i[14]) for i in player_stats],
                    'TO_RATIO': [float(i[15]) for i in player_stats],
                    'EFG_PCT': [float(i[16]) for i in player_stats],
                    'TS_PCT': [float(i[17]) for i in player_stats],
                    'USG_PCT': [float(i[18]) for i in player_stats],
                    'PACE': [float(i[19]) for i in player_stats],
                    'PIE': [float(i[20]) for i in player_stats]
                    })

#export to csv
teamAdata.to_csv('NBA_Player_Advanced_Stats.csv',index=True)
