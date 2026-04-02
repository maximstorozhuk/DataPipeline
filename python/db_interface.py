import sqlite3
import pandas as pd
import os

class SQLInterface:
    # Create/connect to database
    def __init__(self, db_name="nba.db"):
        self.conn = sqlite3.connect(db_name)

    # Load a single CSV into a table
    # usage: db.load_csv("path/file.csv", "table_name")
    def load_csv(self, csv_path, table_name):
        df = pd.read_csv(csv_path)
        df.to_sql(table_name, self.conn, if_exists="replace", index=False)

    # Load all CSVs in a folder as tables
    # usage: db.load_all_csvs("path/to/csv_folder")
    def load_all_csvs(self, folder_path):
        for file in os.listdir(folder_path):
            if file.endswith(".csv"):
                full_path = os.path.join(folder_path, file)
                table_name = os.path.splitext(file)[0]
                table_name = table_name.replace(" ", "_").replace("-", "_")
                self.load_csv(full_path, table_name)

    # Run SQL query, this returns pandas DataFrame
    # usage: db.query("SELECT * FROM table LIMIT 5;")
    def query(self, query):
        return pd.read_sql(query, self.conn)

    # Show all tables in DB
    # usage: print(db.show_tables())
    def show_tables(self):
        q = "SELECT name FROM sqlite_master WHERE type='table';"
        return pd.read_sql(q, self.conn)

    #Show columns with data types
    def show_columns(self, table_name):
        query = f"PRAGMA table_info({table_name});"
        result = pd.read_sql(query, self.conn)
        return result[['name', 'type']]
    
    #Get just column names as a list
    def get_column_names(self, table_name):
        query = f"PRAGMA table_info({table_name});"
        result = pd.read_sql(query, self.conn)
        return result['name'].tolist()

    # Close connection
    def close(self):
        self.conn.close()


# EXAMPLE USAGE OF INTERFACE (copy paste to quickly load all csv into a working SQL db, uncommented version in "interfaceTest.py")
# from db_interface import SQLInterface

# db = SQLInterface()

# This loops through the given directory auto loading every csvv file into an SQL table
# db.load_all_csvs("D:/NBABetting/DataPipeline/csv")

# Check that it worked by outputting
# print(db.show_tables())

# Example Query
# result = db.query("""
# SELECT player_name, points_per_game
# FROM nba_2008_2025_cleaned
# ORDER BY points_per_game DESC
# LIMIT 10;
# """)

# print(result)

# db.close()