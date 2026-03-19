import time
import requests
import pandas as pd
from datetime import date, timedelta

ARCHIVE_URL = "https://archive-api.open-meteo.com/v1/archive"
TIMEZONE = "Europe/Warsaw"
OUTPUT_CSV = "hel_hourly_10000_days.csv"

# Hel, Polska
LAT = 54.6089
LON = 18.8013

HOURLY_VARS = [
    "temperature_2m",
    "relative_humidity_2m",
    "dew_point_2m",
    "apparent_temperature",
    "precipitation",
    "rain",
    "snowfall",
    "snow_depth",
    "weather_code",
    "pressure_msl",
    "surface_pressure",
    "cloud_cover",
    "cloud_cover_low",
    "cloud_cover_mid",
    "cloud_cover_high",
    "et0_fao_evapotranspiration",
    "vapour_pressure_deficit",
    "wind_speed_10m",
    "wind_speed_100m",
    "wind_direction_10m",
    "wind_direction_100m",
    "wind_gusts_10m",
    "soil_temperature_0_to_7cm",
    "soil_temperature_7_to_28cm",
    "soil_temperature_28_to_100cm",
    "soil_temperature_100_to_255cm",
    "soil_moisture_0_to_7cm",
    "soil_moisture_7_to_28cm",
    "soil_moisture_28_to_100cm",
    "soil_moisture_100_to_255cm",
    "is_day",
    "sunshine_duration",
    "wet_bulb_temperature_2m",
    "total_column_integrated_water_vapour",
    "shortwave_radiation",
    "direct_radiation",
    "diffuse_radiation",
    "direct_normal_irradiance",
    "terrestrial_radiation",
    "shortwave_radiation_instant",
    "direct_radiation_instant",
    "diffuse_radiation_instant",
    "direct_normal_irradiance_instant",
    "terrestrial_radiation_instant",
]

def get_date_range(days=10000):
    end = date.today()
    start = end - timedelta(days=days - 1)
    return start.isoformat(), end.isoformat()

def chunk_list(lst, size):
    for i in range(0, len(lst), size):
        yield lst[i:i + size]

def fetch_with_retry(params, retries=6):
    for attempt in range(retries):
        response = requests.get(ARCHIVE_URL, params=params, timeout=180)

        if response.status_code == 200:
            return response.json()

        if response.status_code == 429:
            wait = min(60, 5 * (2 ** attempt))
            print(f"Rate limit 429. Czekam {wait}s...")
            time.sleep(wait)
            continue

        raise Exception(f"Error {response.status_code}: {response.text}")

    raise Exception("Przekroczono limit retry")

def main():
    start_date, end_date = get_date_range()
    print(f"Pobieranie danych godzinowych dla Helu: {start_date} → {end_date}")

    merged_df = None

    for i, chunk in enumerate(chunk_list(HOURLY_VARS, 8), start=1):
        print(f"Paczka {i}: {chunk}")

        params = {
            "latitude": LAT,
            "longitude": LON,
            "start_date": start_date,
            "end_date": end_date,
            "hourly": ",".join(chunk),
            "timezone": TIMEZONE,
        }

        data = fetch_with_retry(params)

        if "hourly" not in data:
            raise Exception("Brak sekcji 'hourly' w odpowiedzi API")

        df = pd.DataFrame(data["hourly"])

        if merged_df is None:
            merged_df = df
        else:
            merged_df = merged_df.merge(df, on="time", how="outer")

        time.sleep(3)

    merged_df.rename(columns={"time": "datetime"}, inplace=True)
    merged_df.sort_values("datetime", inplace=True)
    merged_df.to_csv(OUTPUT_CSV, index=False, encoding="utf-8")

    print(f"\nZapisano: {OUTPUT_CSV}")
    print(f"Liczba wierszy: {len(merged_df)}")
    print(f"Liczba kolumn: {len(merged_df.columns)}")
    print(merged_df.head())

if __name__ == "__main__":
    main()