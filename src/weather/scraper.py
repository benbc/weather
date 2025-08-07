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


if __name__ == "__main__":
    url = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"
    forecast_date = get_forecast_date(url)
    print(f"Latest forecast date: {forecast_date}")
