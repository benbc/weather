import os
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

from .scraper import get_forecast_date


def render_html(template_path: str, output_path: str, forecast_date: Optional[str] = None) -> None:
    """
    Render the HTML template with forecast data and save to output path.
    
    Args:
        template_path: Path to the HTML template file
        output_path: Path where the rendered HTML should be saved
        forecast_date: The forecast date to display, if None will scrape fresh data
    """
    # Read the template
    with open(template_path, 'r', encoding='utf-8') as f:
        template = f.read()
    
    # Get forecast date if not provided
    if forecast_date is None:
        url = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"
        forecast_date = get_forecast_date(url)
    
    # Handle case where forecast date couldn't be retrieved
    if forecast_date is None:
        forecast_date = "Unable to retrieve forecast date"
    
    # Get current timestamp in UTC
    last_updated = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC")
    
    # Replace placeholders in template
    rendered_html = template.replace("{forecast_date}", forecast_date)
    rendered_html = rendered_html.replace("{last_updated}", last_updated)
    
    # Ensure output directory exists
    output_dir = Path(output_path).parent
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Write the rendered HTML
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(rendered_html)
    
    print(f"HTML rendered and saved to: {output_path}")


if __name__ == "__main__":
    # Default paths
    template_path = "src/weather/templates/index.html"
    output_path = "docs/index.html"
    
    render_html(template_path, output_path)