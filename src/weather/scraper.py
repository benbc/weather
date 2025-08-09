from datetime import datetime

import requests
from bs4 import BeautifulSoup


def get_forecast_date(url: str) -> str:
    """
    Scrape the latest forecast date from Met Office inshore waters forecast page.

    Args:
        url: The URL of the Met Office forecast page

    Returns:
        The forecast date string

    Raises:
        Exception: If forecast date cannot be found or extracted
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


def get_lyme_regis_lands_end_forecast(url: str) -> dict:
    """
    Scrape the forecast for "Lyme Regis to Lands End including the Isles of Scilly".

    Args:
        url: The URL of the Met Office forecast page

    Returns:
        Dictionary containing the structured forecast data

    Raises:
        Exception: If forecast cannot be found or extracted
    """
    response = requests.get(url)
    response.raise_for_status()

    soup = BeautifulSoup(response.content, "html.parser")

    # Find area 8 section
    area8_h2 = soup.find("h2", {"id": "area8"})
    if not area8_h2:
        raise Exception(
            "Could not find Area 8 (Lyme Regis to Lands End) section in Met Office "
            "forecast"
        )

    area8_section = area8_h2.find_parent("section", class_="marine-card")
    if not area8_section:
        raise Exception("Found Area 8 heading but could not find marine-card section")

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

    if not forecast_data["sections"]:
        raise Exception("Found Area 8 section but could not extract any forecast data")

    return forecast_data


def get_latest_ecmwf_base_time() -> dict:
    """
    Find the most recent available ECMWF forecast base time.

    Uses a headless browser to navigate to the ECMWF page without a base_time
    parameter, which redirects to the latest available forecast. We extract
    the base time from the final URL after JavaScript execution and redirect.

    Returns:
        Dictionary containing base_time string (YYYYMMDDHHMM format) and
        human-readable description

    Raises:
        Exception: If ECMWF request fails or base_time cannot be extracted
    """
    import re

    from playwright.sync_api import sync_playwright

    base_url = (
        "https://charts.ecmwf.int/products/medium-wind-10m?"
        "projection=opencharts_north_west_europe"
    )

    with sync_playwright() as p:
        # Launch headless browser
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()

        try:
            # Navigate to ECMWF page - this will execute JS and handle redirects
            page.goto(base_url, timeout=30000)  # 30 second timeout

            # Wait a moment for any dynamic content to load
            page.wait_for_timeout(3000)  # 3 second wait

            # Get the final URL after all redirects and JS execution
            final_url = page.url

        finally:
            browser.close()

    # Extract base_time from the final URL
    if "base_time=" not in final_url:
        raise Exception(
            f"ECMWF redirect URL does not contain base_time parameter: {final_url}"
        )

    match = re.search(r"base_time=(\d{12})", final_url)
    if not match:
        raise Exception(f"Could not extract base_time from ECMWF URL: {final_url}")

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


def get_sailing_locations() -> list[dict]:
    """
    Get list of sailing locations with coordinates for meteogram generation.

    Returns:
        List of location dictionaries with name, description, lat, lon
    """
    return [
        {
            "name": "8M off Dartmouth",
            "lat": 50.23,
            "lon": -3.47,
        },
        {
            "name": "8M off Plymouth",
            "lat": 50.23,
            "lon": -4.11,
        },
        {
            "name": "4M off Dodman Point",
            "lat": 50.16,
            "lon": -4.73,
        },
        {
            "name": "1M off Lizard Point",
            "lat": 49.92,
            "lon": -5.20,
        },
    ]


def generate_meteogram_url(lat: float, lon: float) -> str:
    """
    Generate ECMWF meteogram URL for specific coordinates.

    Args:
        lat: Latitude in decimal degrees
        lon: Longitude in decimal degrees

    Returns:
        Complete meteogram URL (without base_time to show most recent forecast)
    """
    return (
        f"https://charts.ecmwf.int/products/opencharts_meteogram?"
        f"epsgram=classical_15d&lat={lat}&lon={lon}"
    )


def get_meteogram_forecast_time(lat: float, lon: float) -> dict:
    """
    Get the forecast time for a specific meteogram location.

    Uses a headless browser to navigate to the meteogram page and extract
    the forecast time from the final URL after JavaScript execution.

    Args:
        lat: Latitude in decimal degrees
        lon: Longitude in decimal degrees

    Returns:
        Dictionary containing base_time string (YYYYMMDDHHMM format) and
        human-readable description

    Raises:
        Exception: If meteogram request fails or base_time cannot be extracted
    """
    import re

    from playwright.sync_api import sync_playwright

    meteogram_url = generate_meteogram_url(lat, lon)

    with sync_playwright() as p:
        # Launch headless browser
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()

        try:
            # Navigate to meteogram page - this will execute JS and handle redirects
            page.goto(meteogram_url, timeout=30000)  # 30 second timeout

            # Wait a moment for any dynamic content to load
            page.wait_for_timeout(3000)  # 3 second wait

            # Get the final URL after all redirects and JS execution
            final_url = page.url

        finally:
            browser.close()

    # Extract base_time from the final URL
    if "base_time=" not in final_url:
        raise Exception(
            f"Meteogram redirect URL does not contain base_time parameter: {final_url}"
        )

    match = re.search(r"base_time=(\d{12})", final_url)
    if not match:
        raise Exception(f"Could not extract base_time from meteogram URL: {final_url}")

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


def get_shipping_forecast_period(url: str) -> str:
    """
    Extract the forecast period from the shipping forecast page.

    Args:
        url: The URL of the Met Office shipping forecast page

    Returns:
        The forecast period string (e.g.,
            "For the period 06:00 (UTC) on Sat 9 Aug to 06:00 (UTC) on Sun 10 Aug")

    Raises:
        Exception: If forecast period cannot be found or extracted
    """
    response = requests.get(url)
    response.raise_for_status()

    soup = BeautifulSoup(response.content, "html.parser")

    # Find the paragraph that contains "For the period"
    period_paragraph = None
    for p in soup.find_all("p"):
        if "For the period" in p.get_text():
            period_paragraph = p
            break

    if not period_paragraph:
        raise Exception(
            "Could not find 'For the period' paragraph in shipping forecast"
        )

    # Find all time elements within this paragraph
    time_elements = period_paragraph.find_all("time")

    if len(time_elements) < 2:
        raise Exception(
            "Could not find start and end time elements in forecast period paragraph"
        )

    # Extract the text from the time elements
    start_time = time_elements[0].get_text().strip()
    end_time = time_elements[1].get_text().strip()

    return f"For the period {start_time} to {end_time}"


def get_shipping_forecast_areas(url: str, area_names: list[str]) -> list[dict]:
    """
    Scrape shipping forecast data for specified sea areas.

    Args:
        url: The URL of the Met Office shipping forecast page
        area_names: List of sea area names to extract (e.g., ["portland", "plymouth"])

    Returns:
        List of dictionaries containing the structured forecast data for each area

    Raises:
        Exception: If forecast areas cannot be found or extracted
    """
    response = requests.get(url)
    response.raise_for_status()

    soup = BeautifulSoup(response.content, "html.parser")

    forecast_areas = []

    for area_name in area_names:
        # Find the section for this sea area
        area_section = soup.find("section", {"id": area_name.lower()})
        if not area_section:
            raise Exception(f"Could not find {area_name} section in shipping forecast")

        # Extract the area name from the h2 element
        h2_element = area_section.find("h2")
        if not h2_element:
            raise Exception(f"Could not find h2 element for {area_name} section")

        area_display_name = h2_element.get_text().strip()

        # Find the forecast info within this section
        forecast_info = area_section.find("div", class_="forecast-info")
        if not forecast_info:
            raise Exception(f"Could not find forecast-info div for {area_name}")

        dl_element = forecast_info.find("dl")
        if not dl_element:
            raise Exception(f"Could not find dl element for {area_name} forecast data")

        # Extract the forecast data
        forecast_data = {"area_name": area_display_name, "forecast": []}

        for dt, dd in zip(
            dl_element.find_all("dt"), dl_element.find_all("dd"), strict=False
        ):
            forecast_data["forecast"].append(
                {
                    "category": dt.get_text().strip(),
                    "description": dd.get_text().strip(),
                }
            )

        if not forecast_data["forecast"]:
            raise Exception(
                f"Found {area_name} section but could not extract any forecast data"
            )

        forecast_areas.append(forecast_data)

    return forecast_areas


if __name__ == "__main__":
    url = (
        "https://weather.metoffice.gov.uk/specialist-forecasts/"
        "coast-and-sea/inshore-waters-forecast"
    )
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
    ecmwf_url = (
        f"https://charts.ecmwf.int/products/medium-wind-10m?"
        f"projection=opencharts_north_west_europe&base_time={ecmwf_data['base_time']}"
    )
    print(f"Chart URL: {ecmwf_url}")

    print("\nLocation-specific meteograms:")
    locations = get_sailing_locations()
    for location in locations:
        meteogram_url = generate_meteogram_url(location["lat"], location["lon"])
        print(f"{location['name']}: {meteogram_url}")

    # Test shipping forecast scraping
    shipping_url = (
        "https://weather.metoffice.gov.uk/specialist-forecasts/"
        "coast-and-sea/shipping-forecast"
    )
    shipping_period = get_shipping_forecast_period(shipping_url)
    print(f"\nShipping forecast period: {shipping_period}")

    shipping_areas = get_shipping_forecast_areas(shipping_url, ["portland", "plymouth"])
    print("\nShipping forecast:")
    for area in shipping_areas:
        print(f"\n{area['area_name']}:")
        for item in area["forecast"]:
            print(f"  {item['category']}: {item['description']}")
