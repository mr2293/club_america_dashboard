import os
import time
import requests

sheet_id = os.getenv('RPE_SHEET_ID')
if not sheet_id:
    raise ValueError("RPE_SHEET_ID environment variable not set")

url = f'https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=xlsx'

output_dir = 'data'
output_path = os.path.join(output_dir, 'rpe_primera.xlsx')

os.makedirs(output_dir, exist_ok=True)

max_retries = 3
for attempt in range(1, max_retries + 1):
    try:
        response = requests.get(url, timeout=60)
        response.raise_for_status()
        break
    except (requests.exceptions.ChunkedEncodingError, requests.exceptions.ConnectionError, requests.exceptions.Timeout) as e:
        if attempt == max_retries:
            raise
        print(f'Attempt {attempt} failed ({e}), retrying in {attempt * 5}s...')
        time.sleep(attempt * 5)

with open(output_path, 'wb') as f:
    f.write(response.content)

print(f'Downloaded Google Sheet as {output_path}')
