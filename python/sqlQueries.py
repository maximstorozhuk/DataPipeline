import matplotlib.pyplot as plt
import pandas as pd
from db_interface import SQLInterface

# DATABASE SETUP-----------------------------
db = SQLInterface()

file_path = r"D:\NBABetting\DataPipeline\csv\nba_2008-2025_cleaned.csv"  # change to your file path
db.load_csv(file_path, "games")  # table name must be a string

# PRINT-ONLY QUERIES-----------------------------

# output total entries
print(db.query("""
SELECT COUNT(*) AS total_entries
FROM games;
"""))
print("\n")

# output average points scored in a game
print(db.query("""
SELECT AVG(score_home + score_away) AS avg_total_points
FROM games;
"""))
print("\n")

# output average margin of victory
print(db.query("""
SELECT AVG(ABS(score_home - score_away)) AS avg_victory_margin
FROM games;
"""))
print("\n")

# output home vs away win percentage (overall)
print(db.query("""
SELECT 
    AVG(CASE WHEN score_home > score_away THEN 1.0 ELSE 0 END) AS home_win_pct,
    AVG(CASE WHEN score_away > score_home THEN 1.0 ELSE 0 END) AS away_win_pct
FROM games;
"""))
print("\n")

# outputs how many times the favoured team wins and the unfavoured team wins
print(db.query("""
SELECT 
    SUM(CASE 
        WHEN whos_favored = 'home' AND score_home > score_away THEN 1
        WHEN whos_favored = 'away' AND score_away > score_home THEN 1
        ELSE 0
    END) AS favorite_wins,

    SUM(CASE 
        WHEN whos_favored = 'home' AND score_home < score_away THEN 1
        WHEN whos_favored = 'away' AND score_away < score_home THEN 1
        ELSE 0
    END) AS underdog_wins
FROM games;
"""))
print("\n")

# outputs averages for the favoured team winning and the unfavoured team winning
print(db.query("""
SELECT 
    AVG(CASE 
        WHEN whos_favored = 'home' AND score_home > score_away THEN 1
        WHEN whos_favored = 'away' AND score_away > score_home THEN 1
        ELSE 0
    END) AS favorite_win_rate,

    AVG(CASE 
        WHEN whos_favored = 'home' AND score_home < score_away THEN 1
        WHEN whos_favored = 'away' AND score_away < score_home THEN 1
        ELSE 0
    END) AS underdog_win_rate
FROM games;
"""))
print("\n")

# output the over/under rates on the over/under bets
print(db.query("""
SELECT 
    AVG(CASE WHEN (score_home + score_away) > total THEN 1 ELSE 0 END) AS over_pct,
    AVG(CASE WHEN (score_home + score_away) < total THEN 1 ELSE 0 END) AS under_pct,
    AVG(CASE WHEN (score_home + score_away) = total THEN 1 ELSE 0 END) AS push_pct
FROM games;
"""))
print("\n")

# output the quarter averages (LIMIT 5 for terminal)
print(db.query("""
SELECT season,
    AVG(q1_home + q1_away) AS q1_avg,
    AVG(q2_home + q2_away) AS q2_avg,
    AVG(q3_home + q3_away) AS q3_avg,
    AVG(q4_home + q4_away) AS q4_avg
FROM games
GROUP BY season
LIMIT 5;
"""))
print("\n")

# output winrate for games with spread > 10
print(db.query("""
SELECT 
    AVG(CASE 
        WHEN whos_favored = 'home' AND score_home > score_away THEN 1
        WHEN whos_favored = 'away' AND score_away > score_home THEN 1
        ELSE 0
    END) AS fav_win_pct
FROM games
WHERE ABS(spread) > 10;
"""))
print("\n")

# DATAFRAMES FOR VISUALIZATIONS-----------------------------

# DF 1: Average points BY SEASON
df_season_avg = pd.DataFrame(db.query("""
SELECT season, AVG(score_home + score_away) AS avg_total
FROM games
GROUP BY season
ORDER BY season;
"""))
df_season_avg['season'] = df_season_avg['season'].astype(str) #convert to string for graph labels

# DF 2: Home and away win percentage by season
df_home_away_season = pd.DataFrame(db.query("""
SELECT season,
    AVG(CASE WHEN score_home > score_away THEN 1.0 ELSE 0 END) AS home_win_pct,
    AVG(CASE WHEN score_away > score_home THEN 1.0 ELSE 0 END) AS away_win_pct
FROM games
GROUP BY season
ORDER BY season;
"""))

df_home_away_season['season'] = df_home_away_season['season'].astype(str) #convert to string for graph labels

# DF 3: Quarter averages by season
df_quarters = pd.DataFrame(db.query("""
SELECT season,
    AVG(q1_home + q1_away) AS q1_avg,
    AVG(q2_home + q2_away) AS q2_avg,
    AVG(q3_home + q3_away) AS q3_avg,
    AVG(q4_home + q4_away) AS q4_avg
FROM games
GROUP BY season
ORDER BY season;
"""))

df_quarters['season'] = df_quarters['season'].astype(str) #convert to string for graph labels

# DF 4: Scoring distribution histogram
df_distribution = pd.DataFrame(db.query("""
SELECT 
    CASE 
        WHEN (score_home + score_away) < 180 THEN 'under 180'
        WHEN (score_home + score_away) < 190 THEN '180-189'
        WHEN (score_home + score_away) < 200 THEN '190-199'
        WHEN (score_home + score_away) < 210 THEN '200-209'
        WHEN (score_home + score_away) < 220 THEN '210-219'
        WHEN (score_home + score_away) < 230 THEN '220-229'
        ELSE '230+'
    END AS total_range,
    COUNT(*) AS game_count
FROM games
GROUP BY total_range
ORDER BY MIN(score_home + score_away);
"""))

# CLOSE DATABASE-----------------------------
db.close()

# VISUALIZATIONS-----------------------------

#DF"1" ----------------------------- create line chart to visualize average total points per game, per season
plt.figure(figsize=(12, 6))
plt.plot(df_season_avg['season'], df_season_avg['avg_total'], marker='D', linewidth=2.5, color='black', markerfacecolor='blue')
plt.xlabel('Season')
plt.ylabel('Average Total Points')
plt.title('NBA Average Total Points by Season (2008-2025)')
plt.xticks(rotation=45)
plt.grid(True, alpha=0.3, linestyle='--')
plt.tight_layout()
plt.show()

#DF"2" ----------------------------- create bar chart comparing winrates for home/away by season
plt.figure(figsize=(14, 6))
plt.bar(df_home_away_season['season'], df_home_away_season['home_win_pct'], 
        label='Home', color='maroon', alpha=0.8)
plt.bar(df_home_away_season['season'], df_home_away_season['away_win_pct'], 
        bottom=df_home_away_season['home_win_pct'], label='Away', color='slategrey', alpha=1)
plt.xlabel('Season')
plt.ylabel('Win Percentage')
plt.title('Home vs Away Win Percentage')
plt.xticks(rotation=45)
plt.legend()
plt.axhline(y=0.5, color='black', linewidth=2, linestyle='-')
plt.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.show()

# DF"3" ----------------------------- Line chart showing scoring per quarter by season
plt.figure(figsize=(14, 7))
plt.plot(df_quarters['season'], df_quarters['q1_avg'], marker='o', linewidth=2, label='1st Quarter', color='green')
plt.plot(df_quarters['season'], df_quarters['q2_avg'], marker='s', linewidth=2, label='2nd Quarter', color='blue')
plt.plot(df_quarters['season'], df_quarters['q3_avg'], marker='^', linewidth=2, label='3rd Quarter', color='orange')
plt.plot(df_quarters['season'], df_quarters['q4_avg'], marker='d', linewidth=2, label='4th Quarter', color='red')
plt.xlabel('Season')
plt.ylabel('Average Points')
plt.title('Average Points by Quarter Across Seasons')
plt.legend()
plt.xticks(rotation=45)
plt.grid(True, alpha=0.3, linestyle='--')
plt.tight_layout()
plt.show()

# DF"4" ----------------------------- Create bar chart showing scoring distribution by season
plt.figure(figsize=(7, 7))
plt.bar(df_distribution['total_range'], df_distribution['game_count'])
plt.xlabel('Total Points Range')
plt.ylabel('Number of Games')
plt.title('Distribution of Total Points in NBA Games (2008-2025)')
plt.xticks(rotation=45) #rotate the x-axis labels to avoid overlapping
plt.grid(axis='y', linestyle='--', color='grey', alpha=.3)
plt.tight_layout()
plt.show()
