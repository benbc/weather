import requests
from bs4 import BeautifulSoup
import re
from typing import Optional


def get_forecast_date(url: str) -> Optional[str]:
    """
    Scrape the latest forecast date from Met Office inshore waters forecast page.
    
    Args:
        url: The URL of the Met Office forecast page
        
    Returns:
        The forecast date string if found, None otherwise
    """
    try:
        response = requests.get(url)
        response.raise_for_status()
        
        soup = BeautifulSoup(response.content, 'html.parser')
        
        # Look for text containing forecast date patterns
        text = soup.get_text()
        
        # Look for the specific forecast date pattern at the start
        # Pattern like "06:00 (UTC) on Thu 7 Aug 2025"
        date_pattern = r'(\d{2}:\d{2} \(UTC\) on (?:Mon|Tue|Wed|Thu|Fri|Sat|Sun) \d{1,2} \w{3} \d{4})'
        match = re.search(date_pattern, text)
        
        if match:
            return match.group(1).strip()
            
        return None
        
    except requests.RequestException as e:
        print(f"Error fetching URL: {e}")
        return None
    except Exception as e:
        print(f"Error parsing page: {e}")
        return None


if __name__ == "__main__":
    url = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"
    forecast_date = get_forecast_date(url)
    
    if forecast_date:
        print(f"Latest forecast date: {forecast_date}")
    else:
        print("Could not extract forecast date")