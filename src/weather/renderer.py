import re
from datetime import UTC, datetime, timedelta
from pathlib import Path
from zoneinfo import ZoneInfo

from .scraper import (
    generate_meteogram_url,
    get_forecast_date,
    get_latest_ecmwf_base_time,
    get_lyme_regis_lands_end_forecast,
    get_sailing_locations,
)


def _format_relative_datetime(dt_str: str, source_format: str = "met_office") -> str:
    """
    Convert datetime string to relative format with am/pm times.

    Args:
        dt_str: Original datetime string
        source_format: Either "met_office", "ecmwf", or "timestamp"

    Returns:
        Formatted string with relative date and local timezone

    Raises:
        Exception: If datetime string cannot be parsed
    """
    london_tz = ZoneInfo("Europe/London")
    now = datetime.now(london_tz)
    today = now.date()

    if source_format == "met_office":
        # Handle different Met Office date formats
        # Format 1: "12:00 (UTC) on Thu 7 Aug 2025"
        match = re.match(
            r"(\d{1,2}:\d{2}) \(UTC\) on \w+ (\d{1,2}) (\w+) (\d{4})", dt_str
        )
        if match:
            time_str, day, month, year = match.groups()
            months = {
                "Jan": 1,
                "Feb": 2,
                "Mar": 3,
                "Apr": 4,
                "May": 5,
                "Jun": 6,
                "Jul": 7,
                "Aug": 8,
                "Sep": 9,
                "Oct": 10,
                "Nov": 11,
                "Dec": 12,
            }
            if month not in months:
                raise Exception(f"Unknown month abbreviation: {month}")
            month_num = months[month]
            utc_dt = datetime(
                int(year),
                month_num,
                int(day),
                int(time_str.split(":")[0]),
                int(time_str.split(":")[1]),
            )
            local_dt = utc_dt.replace(tzinfo=UTC).astimezone(london_tz)
        else:
            # Format 2: "Monday 06 Jan 2025" (assume midday UTC for tests)
            match = re.match(r"\w+ (\d{1,2}) (\w+) (\d{4})", dt_str)
            if match:
                day, month, year = match.groups()
                months = {
                    "Jan": 1,
                    "Feb": 2,
                    "Mar": 3,
                    "Apr": 4,
                    "May": 5,
                    "Jun": 6,
                    "Jul": 7,
                    "Aug": 8,
                    "Sep": 9,
                    "Oct": 10,
                    "Nov": 11,
                    "Dec": 12,
                }
                if month not in months:
                    raise Exception(f"Unknown month abbreviation: {month}")
                month_num = months[month]
                utc_dt = datetime(
                    int(year), month_num, int(day), 12, 0
                )  # Assume midday
                local_dt = utc_dt.replace(tzinfo=UTC).astimezone(london_tz)
            else:
                raise Exception(f"Could not parse Met Office datetime format: {dt_str}")

    elif source_format == "ecmwf":
        # Parse "00:00 UTC on Thu 07 Aug 2025"
        match = re.match(r"(\d{1,2}:\d{2}) UTC on \w+ (\d{1,2}) (\w+) (\d{4})", dt_str)
        if not match:
            raise Exception(f"Could not parse ECMWF datetime format: {dt_str}")

        time_str, day, month, year = match.groups()
        months = {
            "Jan": 1,
            "Feb": 2,
            "Mar": 3,
            "Apr": 4,
            "May": 5,
            "Jun": 6,
            "Jul": 7,
            "Aug": 8,
            "Sep": 9,
            "Oct": 10,
            "Nov": 11,
            "Dec": 12,
        }
        if month not in months:
            raise Exception(f"Unknown month abbreviation: {month}")
        month_num = months[month]

        utc_dt = datetime(
            int(year),
            month_num,
            int(day),
            int(time_str.split(":")[0]),
            int(time_str.split(":")[1]),
        )
        local_dt = utc_dt.replace(tzinfo=UTC).astimezone(london_tz)

    elif source_format == "timestamp":
        # Parse "2025-08-07 11:30:44 UTC"
        try:
            utc_dt = datetime.strptime(dt_str, "%Y-%m-%d %H:%M:%S UTC")
        except ValueError as e:
            raise Exception(f"Could not parse timestamp format: {dt_str}") from e
        local_dt = utc_dt.replace(tzinfo=UTC).astimezone(london_tz)

    else:
        raise Exception(f"Unknown source_format: {source_format}")

    # Determine relative day
    dt_date = local_dt.date()
    yesterday = today - timedelta(days=1)
    tomorrow = today + timedelta(days=1)

    if dt_date == today:
        relative_day = "today"
    elif dt_date == yesterday:
        relative_day = "yesterday"
    elif dt_date == tomorrow:
        relative_day = "tomorrow"
    else:
        # Use the day name for dates further away
        relative_day = local_dt.strftime("%A").lower()

    # Format time with am/pm, omit minutes if on the hour
    minute = local_dt.minute

    if minute == 0:
        time_str = local_dt.strftime("%I").lstrip("0") + local_dt.strftime("%p").lower()
    else:
        time_str = (
            local_dt.strftime("%I:%M").lstrip("0") + local_dt.strftime("%p").lower()
        )

    return f"{time_str} {relative_day}"


def _format_forecast_title(
    title: str, section_index: int, forecast_issue_time: str
) -> str:
    """
    Convert generic forecast titles to relative date format with start times.

    Args:
        title: Original forecast title from Met Office
        section_index: Index of the section (0 = first 24h, 1 = second 24h, etc.)
        forecast_issue_time: When the forecast was issued (Met Office format)

    Returns:
        Formatted title with relative dates and start times

    Raises:
        Exception: If forecast issue time cannot be parsed
    """
    # Calculate start time for each section based on issue time
    from datetime import datetime, timedelta
    from zoneinfo import ZoneInfo

    london_tz = ZoneInfo("Europe/London")

    # Parse the forecast issue time
    # Try format with time: "12:00 (UTC) on Thu 7 Aug 2025"
    import re

    match = re.match(
        r"(\d{1,2}:\d{2}) \(UTC\) on \w+ (\d{1,2}) (\w+) (\d{4})",
        forecast_issue_time,
    )
    if not match:
        raise Exception(
            f"Could not parse forecast issue time format: {forecast_issue_time}"
        )

    time_str, day, month, year = match.groups()
    months = {
        "Jan": 1,
        "Feb": 2,
        "Mar": 3,
        "Apr": 4,
        "May": 5,
        "Jun": 6,
        "Jul": 7,
        "Aug": 8,
        "Sep": 9,
        "Oct": 10,
        "Nov": 11,
        "Dec": 12,
    }
    if month not in months:
        raise Exception(f"Unknown month abbreviation: {month}")

    month_num = months[month]

    # Create issue datetime in UTC
    issue_utc = datetime(
        int(year),
        month_num,
        int(day),
        int(time_str.split(":")[0]),
        int(time_str.split(":")[1]),
    )
    issue_utc = issue_utc.replace(tzinfo=UTC)

    # Calculate section start time
    section_start_utc = issue_utc + timedelta(hours=24 * section_index)
    section_start_local = section_start_utc.astimezone(london_tz)

    # Format the time directly instead of double-converting
    now_local = datetime.now(london_tz)
    today = now_local.date()
    start_date = section_start_local.date()

    # Determine relative day
    yesterday = today - timedelta(days=1)
    tomorrow = today + timedelta(days=1)

    if start_date == today:
        day_text = "today"
    elif start_date == yesterday:
        day_text = "yesterday"
    elif start_date == tomorrow:
        day_text = "tomorrow"
    else:
        day_text = section_start_local.strftime("%A").lower()

    # Format time with am/pm, omit minutes if on the hour
    minute = section_start_local.minute
    if minute == 0:
        time_text = (
            section_start_local.strftime("%I").lstrip("0")
            + section_start_local.strftime("%p").lower()
        )
    else:
        time_text = (
            section_start_local.strftime("%I:%M").lstrip("0")
            + section_start_local.strftime("%p").lower()
        )

    start_time_formatted = f"{time_text} {day_text}"

    # Generate title based on section
    return f"From {start_time_formatted}:"


def _generate_forecast_html(forecast_content: dict, forecast_issue_time: str) -> str:
    """
    Generate HTML for forecast content.

    Args:
        forecast_content: The forecast data dictionary
        forecast_issue_time: When the forecast was issued (Met Office format)

    Returns:
        HTML string for the forecast content
    """

    html_parts = []

    # Add area name
    html_parts.append(f"<h3>{forecast_content['area_name']}</h3>")

    # Add each forecast section
    for i, section in enumerate(forecast_content["sections"]):
        formatted_title = _format_forecast_title(
            section["title"], i, forecast_issue_time
        )
        html_parts.append(f"<h4>{formatted_title}</h4>")
        html_parts.append('<dl class="forecast-details">')

        for item in section["content"]:
            html_parts.append(f"<dt>{item['category']}</dt>")
            html_parts.append(f"<dd>{item['description']}</dd>")

        html_parts.append("</dl>")

    return "\n".join(html_parts)


def _generate_meteogram_locations_html(base_time: str) -> str:
    """
    Generate HTML for meteogram location links.

    Args:
        base_time: ECMWF base time in YYYYMMDDHHMM format

    Returns:
        HTML string for the meteogram locations
    """
    locations = get_sailing_locations()
    html_parts = []

    for location in locations:
        meteogram_url = generate_meteogram_url(
            location["lat"], location["lon"], base_time
        )

        # Generate Google Maps URL with satellite view and pin
        maps_url = (
            f"https://www.google.com/maps/place/{location['lat']},{location['lon']}"
            f"/@{location['lat']},{location['lon']},10z/data=!3m1!1e3"
        )

        html_parts.append('<div class="location-item">')
        html_parts.append(f'<span class="location-name">{location["name"]}</span>')
        html_parts.append("<br>")
        html_parts.append(
            f'<a href="{meteogram_url}" class="meteogram-link">View Meteogram →</a>'
        )
        html_parts.append(" | ")
        html_parts.append(f'<a href="{maps_url}" class="maps-link">View Map →</a>')
        html_parts.append("</div>")

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

    # Get forecast content if not provided
    if forecast_content is None:
        forecast_content = get_lyme_regis_lands_end_forecast(url)

    # Generate forecast HTML
    forecast_html = _generate_forecast_html(forecast_content, forecast_date)

    # Get ECMWF data if not provided
    if ecmwf_data is None:
        ecmwf_data = get_latest_ecmwf_base_time()

    # Generate ECMWF chart URL
    ecmwf_chart_url = f"https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe&base_time={ecmwf_data['base_time']}"

    # Generate meteogram locations HTML
    meteogram_locations_html = _generate_meteogram_locations_html(
        ecmwf_data["base_time"]
    )

    # Get current timestamp and format it
    last_updated = datetime.now(UTC).strftime("%Y-%m-%d %H:%M:%S UTC")
    formatted_last_updated = _format_relative_datetime(last_updated, "timestamp")

    # Format the forecast dates
    formatted_forecast_date = _format_relative_datetime(forecast_date, "met_office")
    formatted_ecmwf_time = _format_relative_datetime(
        ecmwf_data["readable_time"], "ecmwf"
    )

    # Replace placeholders in template
    rendered_html = template.replace("{forecast_date}", formatted_forecast_date)
    rendered_html = rendered_html.replace("{last_updated}", formatted_last_updated)
    rendered_html = rendered_html.replace("{forecast_content}", forecast_html)
    rendered_html = rendered_html.replace("{ecmwf_forecast_time}", formatted_ecmwf_time)
    rendered_html = rendered_html.replace("{ecmwf_chart_url}", ecmwf_chart_url)
    rendered_html = rendered_html.replace(
        "{meteogram_locations}", meteogram_locations_html
    )

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
