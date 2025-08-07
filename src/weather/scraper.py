import requests
from bs4 import BeautifulSoup
from datetime import datetime, timedelta, timezone


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
    Calculate the most recent likely available ECMWF forecast base time.
    
    ECMWF runs forecasts at 00, 06, 12, and 18 UTC daily, with dissemination 
    occurring approximately 6-8 hours after each base time.
    
    Returns:
        Dictionary containing base_time string (YYYYMMDDHHMM format) and 
        human-readable description
    """
    now = datetime.now(timezone.utc).replace(tzinfo=None)
    
    # Define the 4 daily forecast times
    forecast_hours = [0, 6, 12, 18]
    
    # Find the most recent forecast time that is likely available
    # Account for ~6-8 hour dissemination delay
    available_time = now - timedelta(hours=7)  # Conservative 7-hour delay
    
    # Find the most recent forecast hour before the available_time
    for hour in reversed(forecast_hours):
        forecast_time = available_time.replace(
            hour=hour, minute=0, second=0, microsecond=0
        )
        if forecast_time <= available_time:
            base_time = forecast_time
            break
    
    # Format as YYYYMMDDHHMM
    base_time_str = base_time.strftime("%Y%m%d%H%M")
    
    # Create human-readable description
    readable_time = base_time.strftime("%H:%M UTC on %a %d %b %Y")
    
    return {
        "base_time": base_time_str,
        "readable_time": readable_time,
        "datetime": base_time
    }


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
