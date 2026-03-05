import pandas as pd
from datetime import datetime

df = pd.read_csv('csv/nba_2008-2025_cleaned.csv')
df['date'] = pd.to_datetime(df['date'])
df['original_index'] = df.index
df = df.sort_values('date').reset_index(drop=True)

df['days_rest_away'] = 5  # Default value
df['days_rest_home'] = 5  # Default value
df['last_game_ot_away'] = False  # Default value
df['last_game_ot_home'] = False  # Default value
df['win_loss_away'] = '0-0'  # Default value
df['win_loss_home'] = '0-0'  # Default value
df['last10_win_loss_away'] = '0-0'  # Default value
df['last10_win_loss_home'] = '0-0'  # Default value
df['west_conference_away'] = False  # Default value
df['west_conference_home'] = False  # Default value
df['away_record'] = '0-0'  # Default value
df['home_record'] = '0-0'  # Default value
df['implied_odds_away'] = 0.0  # Default value
df['implied_odds_home'] = 0.0  # Default value
df['profit_moneyline_away'] = 0.0  # Default value
df['profit_moneyline_home'] = 0.0  # Default value
df['points_for_away'] = pd.NA  # Default value
df['points_for_home'] = pd.NA  # Default value
df['points_against_away'] = pd.NA  # Default value
df['points_against_home'] = pd.NA  # Default value
df['points_for_last10_away'] = pd.NA  # Default value
df['points_for_last10_home'] = pd.NA  # Default value
df['points_against_last10_away'] = pd.NA  # Default value
df['points_against_last10_home'] = pd.NA  # Default value

west_conference_teams = {
    'dal', 'den', 'gs', 'hou', 'lac', 'lal', 'mem', 'min', 'no', 'okc', 'phx', 'por', 'sac', 'sa', 'utah'
}

last_game_date = {}
last_game_ot = {}
season_records = {}
last10_games = {}
home_away_records = {}
profit_tracking = {}
season_points = {}
last10_points = {}

for idx, row in df.iterrows():
    home_team = row['home']
    away_team = row['away']
    current_date = row['date']
    current_season = row['season']
    
    if home_team not in season_records:
        season_records[home_team] = {}
    if away_team not in season_records:
        season_records[away_team] = {}
    if current_season not in season_records[home_team]:
        season_records[home_team][current_season] = {'wins': 0, 'losses': 0}
    if current_season not in season_records[away_team]:
        season_records[away_team][current_season] = {'wins': 0, 'losses': 0}
    
    if home_team not in home_away_records:
        home_away_records[home_team] = {}
    if away_team not in home_away_records:
        home_away_records[away_team] = {}
    if current_season not in home_away_records[home_team]:
        home_away_records[home_team][current_season] = {'home_wins': 0, 'home_losses': 0, 'away_wins': 0, 'away_losses': 0}
    if current_season not in home_away_records[away_team]:
        home_away_records[away_team][current_season] = {'home_wins': 0, 'home_losses': 0, 'away_wins': 0, 'away_losses': 0}
    
    if home_team not in profit_tracking:
        profit_tracking[home_team] = {}
    if away_team not in profit_tracking:
        profit_tracking[away_team] = {}
    if current_season not in profit_tracking[home_team]:
        profit_tracking[home_team][current_season] = 0.0
    if current_season not in profit_tracking[away_team]:
        profit_tracking[away_team][current_season] = 0.0
    
    if home_team not in season_points:
        season_points[home_team] = {}
    if away_team not in season_points:
        season_points[away_team] = {}
    if current_season not in season_points[home_team]:
        season_points[home_team][current_season] = {'points_for': 0, 'points_against': 0, 'games_played': 0}
    if current_season not in season_points[away_team]:
        season_points[away_team][current_season] = {'points_for': 0, 'points_against': 0, 'games_played': 0}
    
    if home_team not in last10_points:
        last10_points[home_team] = []
    if away_team not in last10_points:
        last10_points[away_team] = []
    
    home_season_wins = season_records[home_team][current_season]['wins']
    home_season_losses = season_records[home_team][current_season]['losses']
    away_season_wins = season_records[away_team][current_season]['wins']
    away_season_losses = season_records[away_team][current_season]['losses']
    
    df.at[idx, 'win_loss_home'] = f'{home_season_wins}-{home_season_losses}'
    df.at[idx, 'win_loss_away'] = f'{away_season_wins}-{away_season_losses}'
    
    home_team_home_wins = home_away_records[home_team][current_season]['home_wins']
    home_team_home_losses = home_away_records[home_team][current_season]['home_losses']
    away_team_away_wins = home_away_records[away_team][current_season]['away_wins']
    away_team_away_losses = home_away_records[away_team][current_season]['away_losses']
    
    df.at[idx, 'home_record'] = f'{home_team_home_wins}-{home_team_home_losses}'
    df.at[idx, 'away_record'] = f'{away_team_away_wins}-{away_team_away_losses}'
    
    df.at[idx, 'west_conference_away'] = away_team in west_conference_teams
    df.at[idx, 'west_conference_home'] = home_team in west_conference_teams
    
    df.at[idx, 'profit_moneyline_away'] = round(profit_tracking[away_team][current_season], 2)
    df.at[idx, 'profit_moneyline_home'] = round(profit_tracking[home_team][current_season], 2)
    
    home_season_stats = season_points[home_team][current_season]
    away_season_stats = season_points[away_team][current_season]
    
    if home_season_stats['games_played'] > 0:
        df.at[idx, 'points_for_home'] = round(home_season_stats['points_for'] / home_season_stats['games_played'], 2)
        df.at[idx, 'points_against_home'] = round(home_season_stats['points_against'] / home_season_stats['games_played'], 2)
    else:
        df.at[idx, 'points_for_home'] = pd.NA
        df.at[idx, 'points_against_home'] = pd.NA
    
    if away_season_stats['games_played'] > 0:
        df.at[idx, 'points_for_away'] = round(away_season_stats['points_for'] / away_season_stats['games_played'], 2)
        df.at[idx, 'points_against_away'] = round(away_season_stats['points_against'] / away_season_stats['games_played'], 2)
    else:
        df.at[idx, 'points_for_away'] = pd.NA
        df.at[idx, 'points_against_away'] = pd.NA
    
    if len(last10_points[home_team]) > 0:
        home_last10_for = sum([game['points_for'] for game in last10_points[home_team]])
        home_last10_against = sum([game['points_against'] for game in last10_points[home_team]])
        df.at[idx, 'points_for_last10_home'] = round(home_last10_for / len(last10_points[home_team]), 2)
        df.at[idx, 'points_against_last10_home'] = round(home_last10_against / len(last10_points[home_team]), 2)
    else:
        df.at[idx, 'points_for_last10_home'] = pd.NA
        df.at[idx, 'points_against_last10_home'] = pd.NA
    
    if len(last10_points[away_team]) > 0:
        away_last10_for = sum([game['points_for'] for game in last10_points[away_team]])
        away_last10_against = sum([game['points_against'] for game in last10_points[away_team]])
        df.at[idx, 'points_for_last10_away'] = round(away_last10_for / len(last10_points[away_team]), 2)
        df.at[idx, 'points_against_last10_away'] = round(away_last10_against / len(last10_points[away_team]), 2)
    else:
        df.at[idx, 'points_for_last10_away'] = pd.NA
        df.at[idx, 'points_against_last10_away'] = pd.NA
    
    moneyline_away = row['moneyline_away']
    moneyline_home = row['moneyline_home']
    
    if pd.notna(moneyline_away):
        if moneyline_away < 0:
            implied_odds_away = abs(moneyline_away) / (abs(moneyline_away) + 100)
        else:
            implied_odds_away = 100 / (abs(moneyline_away) + 100)
        df.at[idx, 'implied_odds_away'] = round(implied_odds_away, 4)
    else:
        df.at[idx, 'implied_odds_away'] = 0.0
    
    if pd.notna(moneyline_home):
        if moneyline_home < 0:
            implied_odds_home = abs(moneyline_home) / (abs(moneyline_home) + 100)
        else:
            implied_odds_home = 100 / (abs(moneyline_home) + 100)
        df.at[idx, 'implied_odds_home'] = round(implied_odds_home, 4)
    else:
        df.at[idx, 'implied_odds_home'] = 0.0
    
    if home_team in last10_games:
        home_last10 = last10_games[home_team]
        home_last10_wins = sum(home_last10)
        home_last10_losses = len(home_last10) - home_last10_wins
        df.at[idx, 'last10_win_loss_home'] = f'{home_last10_wins}-{home_last10_losses}'
    else:
        df.at[idx, 'last10_win_loss_home'] = '0-0'
    
    if away_team in last10_games:
        away_last10 = last10_games[away_team]
        away_last10_wins = sum(away_last10)
        away_last10_losses = len(away_last10) - away_last10_wins
        df.at[idx, 'last10_win_loss_away'] = f'{away_last10_wins}-{away_last10_losses}'
    else:
        df.at[idx, 'last10_win_loss_away'] = '0-0'
    
    if home_team in last_game_date:
        days_between = (current_date - last_game_date[home_team]).days
        days_rest = days_between - 1 if days_between > 0 else 0
        df.at[idx, 'days_rest_home'] = min(days_rest, 5)
        if df.at[idx, 'days_rest_home'] == 5:
            df.at[idx, 'last_game_ot_home'] = False
        else:
            df.at[idx, 'last_game_ot_home'] = last_game_ot.get(home_team, False)
    else:
        df.at[idx, 'days_rest_home'] = 5
        df.at[idx, 'last_game_ot_home'] = False
    
    if away_team in last_game_date:
        days_between = (current_date - last_game_date[away_team]).days
        days_rest = days_between - 1 if days_between > 0 else 0
        df.at[idx, 'days_rest_away'] = min(days_rest, 5)
        if df.at[idx, 'days_rest_away'] == 5:
            df.at[idx, 'last_game_ot_away'] = False
        else:
            df.at[idx, 'last_game_ot_away'] = last_game_ot.get(away_team, False)
    else:
        df.at[idx, 'days_rest_away'] = 5
        df.at[idx, 'last_game_ot_away'] = False
    
    game_went_to_ot = (row['ot_away'] > 0) or (row['ot_home'] > 0)
    
    home_score = row['score_home']
    away_score = row['score_away']
    home_won = home_score > away_score
    away_won = away_score > home_score
    
    if home_won:
        season_records[home_team][current_season]['wins'] += 1
        season_records[away_team][current_season]['losses'] += 1
        home_away_records[home_team][current_season]['home_wins'] += 1
        home_away_records[away_team][current_season]['away_losses'] += 1
    elif away_won:
        season_records[away_team][current_season]['wins'] += 1
        season_records[home_team][current_season]['losses'] += 1
        home_away_records[home_team][current_season]['home_losses'] += 1
        home_away_records[away_team][current_season]['away_wins'] += 1
    
    bet_amount = 10.0
    
    if pd.notna(moneyline_home):
        if home_won:
            if moneyline_home < 0:
                profit = (100 / abs(moneyline_home)) * bet_amount
            else:
                profit = (moneyline_home / 100) * bet_amount
        else:
            profit = -bet_amount
        profit_tracking[home_team][current_season] += profit
    
    if pd.notna(moneyline_away):
        if away_won:
            if moneyline_away < 0:
                profit = (100 / abs(moneyline_away)) * bet_amount
            else:
                profit = (moneyline_away / 100) * bet_amount
        else:
            profit = -bet_amount
        profit_tracking[away_team][current_season] += profit
    
    if home_team not in last10_games:
        last10_games[home_team] = []
    if away_team not in last10_games:
        last10_games[away_team] = []
    
    last10_games[home_team].append(home_won)
    last10_games[away_team].append(away_won)
    
    if len(last10_games[home_team]) > 10:
        last10_games[home_team] = last10_games[home_team][-10:]
    if len(last10_games[away_team]) > 10:
        last10_games[away_team] = last10_games[away_team][-10:]
    
    season_points[home_team][current_season]['points_for'] += home_score
    season_points[home_team][current_season]['points_against'] += away_score
    season_points[home_team][current_season]['games_played'] += 1
    
    season_points[away_team][current_season]['points_for'] += away_score
    season_points[away_team][current_season]['points_against'] += home_score
    season_points[away_team][current_season]['games_played'] += 1
    
    last10_points[home_team].append({'points_for': home_score, 'points_against': away_score})
    last10_points[away_team].append({'points_for': away_score, 'points_against': home_score})
    
    if len(last10_points[home_team]) > 10:
        last10_points[home_team] = last10_points[home_team][-10:]
    if len(last10_points[away_team]) > 10:
        last10_points[away_team] = last10_points[away_team][-10:]
    
    last_game_date[home_team] = current_date
    last_game_date[away_team] = current_date
    last_game_ot[home_team] = game_went_to_ot
    last_game_ot[away_team] = game_went_to_ot

df = df.sort_values('original_index').reset_index(drop=True)
df = df.drop('original_index', axis=1)

df.to_csv('csv/nba_2008-2025_extended.csv', index=False)

print(f"Successfully created nba_2008-2025_extended.csv with {len(df)} rows")
print(f"Added columns: days_rest_away, days_rest_home, last_game_ot_away, last_game_ot_home, win_loss_away, win_loss_home, last10_win_loss_away, last10_win_loss_home, west_conference_away, west_conference_home, home_record, away_record, implied_odds_away, implied_odds_home, profit_moneyline_away, profit_moneyline_home, points_for_away, points_for_home, points_against_away, points_against_home, points_for_last10_away, points_for_last10_home, points_against_last10_away, points_against_last10_home")

