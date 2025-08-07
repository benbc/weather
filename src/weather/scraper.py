from datetime import datetime, timedelta, timezone

import requests
from bs4 import BeautifulSoup


def get_forecast_date(url: str) -> str | None:
    """
    Scrape the latest forecast date from Met Office inshore waters forecast page.

    Args:
        url: The URL of the Met Office forecast page

    Returns:
        The forecast date string if found, None otherwise
    """
    response = requests.get(url)
    response.raise_for_status()

    soup = BeautifulSoup(response.content, "html.parser")

    # Find time elements in div#summary .times
    time_elements = soup.select("#summary .times time")

    # Check each time element to see if its enclosing paragraph contains "Issued at:"
    for time_elem in time_elements:
        # Find the enclosing paragraph
        p_element = time_elem.find_parent("p")
        if p_element and "issued at:" in p_element.get_text().lower():
            return time_elem.get_text().strip()

    raise Exception("Could not find latest forecast date")


def get_lyme_regis_lands_end_forecast(url: str) -> dict | None:
    """
    Scrape the forecast for "Lyme Regis to Lands End including the Isles of Scilly".

    Args:
        url: The URL of the Met Office forecast page

    Returns:
        Dictionary containing the structured forecast data, or None if not found
    """
    response = requests.get(url)
    response.raise_for_status()

    soup = BeautifulSoup(response.content, "html.parser")

    # Find area 8 section
    area8_h2 = soup.find("h2", {"id": "area8"})
    if not area8_h2:
        return None

    area8_section = area8_h2.find_parent("section", class_="marine-card")
    if not area8_section:
        return None

    # Extract structured data
    forecast_data = {"area_name": area8_h2.get_text().strip(), "sections": []}

    # Process each forecast section
    for h3 in area8_section.find_all("h3"):
        section_title = h3.get_text().strip()
        forecast_info = h3.find_next_sibling("div", class_="forecast-info")

        if forecast_info and forecast_info.find("dl"):
            dl = forecast_info.find("dl")
            section_data = {"title": section_title, "content": []}

            for dt, dd in zip(dl.find_all("dt"), dl.find_all("dd"), strict=False):
                section_data["content"].append(
                    {
                        "category": dt.get_text().strip(),
                        "description": dd.get_text().strip(),
                    }
                )

            forecast_data["sections"].append(section_data)

    return forecast_data if forecast_data["sections"] else None


def get_latest_ecmwf_base_time() -> dict:
    """
    Find the most recent available ECMWF forecast base time.

    Makes a request to ECMWF without a base_time parameter, which redirects
    to the latest available forecast. We extract the base time from the redirect.

    Returns:
        Dictionary containing base_time string (YYYYMMDDHHMM format) and
        human-readable description
    """
    base_url = "https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe"

    try:
        # Request without base_time - ECMWF will redirect to latest available
        response = requests.get(base_url, timeout=10, allow_redirects=True)

        # Extract base_time from the final URL after redirect
        final_url = response.url
        if "base_time=" in final_url:
            # Extract base_time parameter from URL
            import re

            match = re.search(r"base_time=(\d{12})", final_url)
            if match:
                base_time_str = match.group(1)

                # Parse the base time to create readable format
                year = int(base_time_str[:4])
                month = int(base_time_str[4:6])
                day = int(base_time_str[6:8])
                hour = int(base_time_str[8:10])
                minute = int(base_time_str[10:12])

                forecast_dt = datetime(year, month, day, hour, minute)
                readable_time = forecast_dt.strftime("%H:%M UTC on %a %d %b %Y")

                return {
                    "base_time": base_time_str,
                    "readable_time": readable_time,
                    "datetime": forecast_dt,
                }
    except requests.RequestException:
        pass  # Fall through to fallback

    # Fallback: if request fails, use most recent 12:00 UTC
    now = datetime.now(timezone.utc).replace(tzinfo=None)  # noqa: UP017
    fallback_time = now.replace(hour=12, minute=0, second=0, microsecond=0)
    if fallback_time > now:
        fallback_time -= timedelta(days=1)

    base_time_str = fallback_time.strftime("%Y%m%d%H%M")
    readable_time = fallback_time.strftime("%H:%M UTC on %a %d %b %Y")

    return {
        "base_time": base_time_str,
        "readable_time": readable_time,
        "datetime": fallback_time,
    }


def get_sailing_locations() -> list[dict]:
    """
    Get list of sailing locations with coordinates for meteogram generation.

    Returns:
        List of location dictionaries with name, description, lat, lon
    """
    return [
        {
            "name": "The Lizard",
            "description": "Lizard Peninsula area",
            "lat": 49.97,
            "lon": -4.95,
        },
        {
            "name": "River Dart",
            "description": "1nm off River Dart entrance",
            "lat": 50.32,
            "lon": -3.57,
        },
    ]


def generate_meteogram_url(lat: float, lon: float, base_time: str) -> str:
    """
    Generate ECMWF meteogram URL for specific coordinates.

    Args:
        lat: Latitude in decimal degrees
        lon: Longitude in decimal degrees
        base_time: ECMWF base time in YYYYMMDDHHMM format

    Returns:
        Complete meteogram URL
    """
    return (
        f"https://charts.ecmwf.int/products/opencharts_meteogram?"
        f"base_time={base_time}&epsgram=classical_15d&lat={lat}&lon={lon}"
    )


if __name__ == "__main__":
    url = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"
    forecast_date = get_forecast_date(url)
    print(f"Latest forecast date: {forecast_date}")

    lyme_regis_forecast = get_lyme_regis_lands_end_forecast(url)
    if lyme_regis_forecast:
        print(f"\n{lyme_regis_forecast['area_name']}")
        for section in lyme_regis_forecast["sections"]:
            print(f"\n{section['title']}")
            for item in section["content"]:
                print(f"  {item['category']}: {item['description']}")
    else:
        print("Could not retrieve Lyme Regis to Lands End forecast")

    ecmwf_data = get_latest_ecmwf_base_time()
    print(f"\nECMWF Latest forecast: {ecmwf_data['readable_time']}")
    print(f"Base time: {ecmwf_data['base_time']}")
    ecmwf_url = f"https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe&base_time={ecmwf_data['base_time']}"
    print(f"Chart URL: {ecmwf_url}")

    print("\nLocation-specific meteograms:")
    locations = get_sailing_locations()
    for location in locations:
        meteogram_url = generate_meteogram_url(
            location["lat"], location["lon"], ecmwf_data["base_time"]
        )
        print(f"{location['name']} ({location['description']}): {meteogram_url}")
