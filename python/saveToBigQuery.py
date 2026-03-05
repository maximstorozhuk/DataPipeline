"""
Upload the local CSV `nba_2008-2025_extended.csv` to a BigQuery table.

Configuration:
  1) Set PROJECT_ID, DATASET_ID, TABLE_ID below.
  2) Point SERVICE_ACCOUNT_FILE to your GCP service account JSON key
     OR remove the explicit credentials block and rely on
     GOOGLE_APPLICATION_CREDENTIALS / other default auth.

Usage (from project root):
  python python/saveToBigQuery.py
"""

import os
from google.cloud import bigquery
from google.oauth2 import service_account

PROJECT_ID = "datapipelinenba"
DATASET_ID = "nba_2008_2025_extended"     
TABLE_ID = "nba_2008_2025_extended"

SERVICE_ACCOUNT_FILE = r"D:\COSC301\Project\datapipelinenba-108bb155b54f.json"
# Note from Maxim: This is where the service account key file is located on my machine.
# Running this script will not work unless if you change the path to the service account key file.

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
CSV_PATH = os.path.join(BASE_DIR, "..", "csv", "nba_2008-2025_extended.csv")

def get_bigquery_client() -> bigquery.Client:
    """Create and return a BigQuery client using the config above."""
    if SERVICE_ACCOUNT_FILE and SERVICE_ACCOUNT_FILE.strip():
        credentials = service_account.Credentials.from_service_account_file(
            SERVICE_ACCOUNT_FILE
        )
        return bigquery.Client(project=PROJECT_ID, credentials=credentials)

    # Fall back to default credentials (GOOGLE_APPLICATION_CREDENTIALS, etc.)
    return bigquery.Client(project=PROJECT_ID)


def load_csv_to_bigquery() -> None:
    """Load the local CSV into the configured BigQuery table."""
    if not os.path.exists(CSV_PATH):
        raise FileNotFoundError(f"CSV file not found at: {CSV_PATH}")

    client = get_bigquery_client()

    table_full_id = f"{PROJECT_ID}.{DATASET_ID}.{TABLE_ID}"
    job_config = bigquery.LoadJobConfig(
        source_format=bigquery.SourceFormat.CSV,
        skip_leading_rows=1,           # skip header row
        autodetect=True,               # let BigQuery infer schema
        write_disposition=bigquery.WriteDisposition.WRITE_TRUNCATE,
        field_delimiter=",",
        encoding="UTF-8",
    )

    print(f"Uploading '{CSV_PATH}' to BigQuery table '{table_full_id}'...")

    with open(CSV_PATH, "rb") as source_file:
        load_job = client.load_table_from_file(
            source_file,
            table_full_id,
            job_config=job_config,
        )

    load_job.result()  # Wait for the job to complete

    table = client.get_table(table_full_id)
    print(f"Loaded {table.num_rows} rows into {table_full_id}.")


if __name__ == "__main__":
    load_csv_to_bigquery()

