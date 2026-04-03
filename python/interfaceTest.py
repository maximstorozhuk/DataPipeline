from db_interface import SQLInterface

db = SQLInterface()

file_path = r"D:\NBABetting\DataPipeline\csv\nba_2008-2025_cleaned.csv" #change to your file path
db.load_csv(file_path, "games") #table name must be a string

print(db.show_tables())

result = db.query("""
SELECT * FROM games LIMIT 5
""")

print(result)

# See columns with their data types
print(db.show_columns("games"))

# Get just column names as a list
columns = db.get_column_names("games")
print(f"Columns: {columns}")

# Use columns to build queries
if 'whos_favored' in columns:
    result = db.query("SELECT home, away, whos_favored FROM games LIMIT 5")
    print(result)

db.close()