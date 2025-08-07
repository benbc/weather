from datetime import UTC, datetime
from pathlib import Path

from .scraper import get_forecast_date, get_lyme_regis_lands_end_forecast, get_latest_ecmwf_base_time


def _generate_forecast_html(forecast_content: dict | None) -> str:
    """
    Generate HTML for forecast content.

    Args:
        forecast_content: The forecast data dictionary

    Returns:
        HTML string for the forecast content
    """
    if not forecast_content:
        return "<p>Unable to retrieve forecast content</p>"

    html_parts = []

    # Add area name
    html_parts.append(f"<h3>{forecast_content['area_name']}</h3>")

    # Add each forecast section
    for section in forecast_content["sections"]:
        html_parts.append(f"<h4>{section['title']}</h4>")
        html_parts.append('<dl class="forecast-details">')

        for item in section["content"]:
            html_parts.append(f"<dt>{item['category']}</dt>")
            html_parts.append(f"<dd>{item['description']}</dd>")

        html_parts.append("</dl>")

    return "\n".join(html_parts)


def render_html(
    template_path: str,
    output_path: str,
    forecast_date: str | None = None,
    forecast_content: dict | None = None,
    ecmwf_data: dict | None = None,
) -> None:
    """
    Render the HTML template with forecast data and save to output path.

    Args:
        template_path: Path to the HTML template file
        output_path: Path where the rendered HTML should be saved
        forecast_date: The forecast date to display, if None will scrape fresh data
        forecast_content: The forecast content dict, if None will scrape fresh data
        ecmwf_data: The ECMWF data dict, if None will generate fresh data
    """
    # Read the template
    with open(template_path, encoding="utf-8") as f:
        template = f.read()

    url = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"

    # Get forecast date if not provided
    if forecast_date is None:
        forecast_date = get_forecast_date(url)

    # Handle case where forecast date couldn't be retrieved
    if forecast_date is None:
        forecast_date = "Unable to retrieve forecast date"

    # Get forecast content if not provided
    if forecast_content is None:
        forecast_content = get_lyme_regis_lands_end_forecast(url)

    # Generate forecast HTML
    forecast_html = _generate_forecast_html(forecast_content)

    # Get ECMWF data if not provided
    if ecmwf_data is None:
        ecmwf_data = get_latest_ecmwf_base_time()

    # Generate ECMWF chart URL
    ecmwf_chart_url = f"https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe&base_time={ecmwf_data['base_time']}"

    # Get current timestamp in UTC
    last_updated = datetime.now(UTC).strftime("%Y-%m-%d %H:%M:%S UTC")

    # Replace placeholders in template
    rendered_html = template.replace("{forecast_date}", forecast_date)
    rendered_html = rendered_html.replace("{last_updated}", last_updated)
    rendered_html = rendered_html.replace("{forecast_content}", forecast_html)
    rendered_html = rendered_html.replace("{ecmwf_forecast_time}", ecmwf_data["readable_time"])
    rendered_html = rendered_html.replace("{ecmwf_chart_url}", ecmwf_chart_url)

    # Ensure output directory exists
    output_dir = Path(output_path).parent
    output_dir.mkdir(parents=True, exist_ok=True)

    # Write the rendered HTML
    with open(output_path, "w", encoding="utf-8") as f:
        f.write(rendered_html)

    print(f"HTML rendered and saved to: {output_path}")


if __name__ == "__main__":
    # Default paths
    template_path = "src/weather/templates/index.html"
    output_path = "docs/index.html"

    render_html(template_path, output_path)
