import os
import tempfile
from unittest.mock import patch

import pytest

from weather.renderer import (
    _format_relative_datetime,
    _generate_forecast_html,
    render_html,
)


class TestRenderHtml:
    def test_renders_template_with_provided_forecast_date(self):
        """Test that template is rendered correctly with provided forecast date."""
        template_content = """<html>
<body>
    <p>Forecast: {forecast_date}</p>
    <p>Updated: {last_updated}</p>
</body>
</html>"""

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as template_file:
            template_file.write(template_content)
            template_path = template_file.name

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as output_file:
            output_path = output_file.name

        try:
            render_html(template_path, output_path, forecast_date="Monday 06 Jan 2025")

            with open(output_path) as f:
                result = f.read()

            assert "12pm" in result  # Time from the parsed date
            assert "am" in result or "pm" in result
            assert (
                "today" in result
                or "yesterday" in result
                or "tomorrow" in result
                or "monday" in result
            )
            assert "{forecast_date}" not in result
            assert "{last_updated}" not in result

        finally:
            os.unlink(template_path)
            os.unlink(output_path)

    @patch("weather.renderer.get_forecast_date")
    def test_scrapes_forecast_date_when_none_provided(self, mock_get_forecast_date):
        """Test that forecast date is scraped when not provided."""
        mock_get_forecast_date.return_value = "Tuesday 07 Jan 2025"

        template_content = "<p>Forecast: {forecast_date}</p>"

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as template_file:
            template_file.write(template_content)
            template_path = template_file.name

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as output_file:
            output_path = output_file.name

        try:
            render_html(template_path, output_path)

            with open(output_path) as f:
                result = f.read()

            assert "12pm" in result
            assert "am" in result or "pm" in result
            assert (
                "today" in result
                or "yesterday" in result
                or "tomorrow" in result
                or "tuesday" in result
            )
            mock_get_forecast_date.assert_called_once()

        finally:
            os.unlink(template_path)
            os.unlink(output_path)

    @patch("weather.renderer.get_forecast_date")
    def test_handles_none_forecast_date_from_scraper(self, mock_get_forecast_date):
        """Test that None forecast date from scraper is handled gracefully."""
        mock_get_forecast_date.return_value = None

        template_content = "<p>Forecast: {forecast_date}</p>"

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as template_file:
            template_file.write(template_content)
            template_path = template_file.name

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as output_file:
            output_path = output_file.name

        try:
            render_html(template_path, output_path)

            with open(output_path) as f:
                result = f.read()

            # When forecast date is None, it defaults to
            # "Unable to retrieve forecast date" which gets processed as current time
            assert "am" in result or "pm" in result

        finally:
            os.unlink(template_path)
            os.unlink(output_path)

    def test_creates_output_directory_if_missing(self):
        """Test that output directory is created if it doesn't exist."""
        template_content = "<p>Test</p>"

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as template_file:
            template_file.write(template_content)
            template_path = template_file.name

        with tempfile.TemporaryDirectory() as temp_dir:
            output_path = os.path.join(temp_dir, "subdir", "output.html")

            render_html(template_path, output_path, forecast_date="Test Date")

            assert os.path.exists(output_path)
            assert os.path.isdir(os.path.dirname(output_path))

        os.unlink(template_path)

    def test_last_updated_timestamp_format(self):
        """Test that last updated timestamp is in BST/GMT format with relative date."""
        template_content = "<p>{last_updated}</p>"

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as template_file:
            template_file.write(template_content)
            template_path = template_file.name

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as output_file:
            output_path = output_file.name

        try:
            render_html(template_path, output_path, forecast_date="Monday 06 Jan 2025")

            with open(output_path) as f:
                result = f.read()

            # Test that the timestamp is formatted with BST/GMT and relative date
            assert "am" in result or "pm" in result
            assert "today" in result  # Should be relative date
            assert ":" in result  # Should have time format

        finally:
            os.unlink(template_path)
            os.unlink(output_path)

    def test_file_encoding_utf8(self):
        """Test that files are read and written with UTF-8 encoding."""
        template_content = "<p>Forecast: {forecast_date} ✓</p>"

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False, encoding="utf-8"
        ) as template_file:
            template_file.write(template_content)
            template_path = template_file.name

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as output_file:
            output_path = output_file.name

        try:
            render_html(template_path, output_path, forecast_date="Test ✓")

            with open(output_path, encoding="utf-8") as f:
                result = f.read()

            # Non-standard format defaults to current time, preserving content
            assert "✓" in result
            assert "✓" in result

        finally:
            os.unlink(template_path)
            os.unlink(output_path)

    def test_template_not_found_raises_exception(self):
        """Test that missing template file raises appropriate exception."""
        with pytest.raises(FileNotFoundError):
            render_html(
                "/nonexistent/template.html",
                "/tmp/output.html",
                forecast_date="Monday 06 Jan 2025",
            )

    def test_multiple_placeholder_replacements(self):
        """Test that multiple instances of placeholders are all replaced."""
        template_content = """<html>
<body>
    <p>Forecast 1: {forecast_date}</p>
    <p>Forecast 2: {forecast_date}</p>
    <p>Updated 1: {last_updated}</p>
    <p>Updated 2: {last_updated}</p>
</body>
</html>"""

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as template_file:
            template_file.write(template_content)
            template_path = template_file.name

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as output_file:
            output_path = output_file.name

        try:
            render_html(template_path, output_path, forecast_date="Monday 06 Jan 2025")

            with open(output_path) as f:
                result = f.read()

            assert result.count("12pm") == 2  # The time from parsed date appears twice
            assert result.count("am") + result.count("pm") >= 2
            assert "{forecast_date}" not in result
            assert "{last_updated}" not in result

        finally:
            os.unlink(template_path)
            os.unlink(output_path)


class TestGenerateForecastHtml:
    def test_generates_forecast_html_successfully(self):
        """Test that forecast HTML is generated correctly from forecast data."""
        forecast_data = {
            "area_name": "Lyme Regis to Lands End including the Isles of Scilly (8)",
            "sections": [
                {
                    "title": "24 hour forecast:",
                    "content": [
                        {"category": "Wind", "description": "West or southwest 3 to 5"},
                        {"category": "Sea state", "description": "Smooth or slight"},
                    ],
                },
                {
                    "title": "Outlook for the following 24 hours:",
                    "content": [
                        {"category": "Wind", "description": "Variable 2 to 4"},
                        {"category": "Weather", "description": "Fair"},
                    ],
                },
            ],
        }

        result = _generate_forecast_html(forecast_data)

        assert (
            "<h3>Lyme Regis to Lands End including the Isles of Scilly (8)</h3>"
            in result
        )
        assert "<h4>24 hour forecast:</h4>" in result
        assert "<h4>Outlook for the following 24 hours:</h4>" in result
        assert '<dl class="forecast-details">' in result
        assert "<dt>Wind</dt>" in result
        assert "<dd>West or southwest 3 to 5</dd>" in result
        assert "<dt>Sea state</dt>" in result
        assert "<dd>Smooth or slight</dd>" in result

    def test_handles_none_forecast_data(self):
        """Test that None forecast data returns error message."""
        result = _generate_forecast_html(None)

        assert result == "<p>Unable to retrieve forecast content</p>"

    def test_handles_empty_forecast_data(self):
        """Test that empty forecast data returns error message."""
        result = _generate_forecast_html({})

        assert result == "<p>Unable to retrieve forecast content</p>"


class TestRenderHtmlWithForecastContent:
    @patch("weather.renderer.get_forecast_date")
    @patch("weather.renderer.get_lyme_regis_lands_end_forecast")
    def test_renders_with_forecast_content(self, mock_forecast, mock_date):
        """Test that renderer includes forecast content in output."""
        mock_date.return_value = "Tuesday 07 Jan 2025"
        mock_forecast.return_value = {
            "area_name": "Test Area",
            "sections": [
                {
                    "title": "24 hour forecast:",
                    "content": [{"category": "Wind", "description": "Light winds"}],
                }
            ],
        }

        template_content = """<html>
<body>
    <p>Date: {forecast_date}</p>
    <div class="content">{forecast_content}</div>
    <p>Updated: {last_updated}</p>
</body>
</html>"""

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as template_file:
            template_file.write(template_content)
            template_path = template_file.name

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as output_file:
            output_path = output_file.name

        try:
            render_html(template_path, output_path)

            with open(output_path) as f:
                result = f.read()

            assert "12pm" in result
            assert "am" in result or "pm" in result
            assert (
                "today" in result
                or "yesterday" in result
                or "tomorrow" in result
                or "tuesday" in result
            )
            assert "<h3>Test Area</h3>" in result
            assert "<h4>24 hour forecast:</h4>" in result
            assert "<dt>Wind</dt>" in result
            assert "<dd>Light winds</dd>" in result

        finally:
            os.unlink(template_path)
            os.unlink(output_path)

    def test_renders_with_provided_forecast_content(self):
        """Test that provided forecast content is used instead of scraping."""
        template_content = """<html>
<body>
    <div class="content">{forecast_content}</div>
</body>
</html>"""

        forecast_data = {
            "area_name": "Provided Area",
            "sections": [
                {
                    "title": "Test forecast:",
                    "content": [
                        {"category": "Wind", "description": "Provided wind data"}
                    ],
                }
            ],
        }

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as template_file:
            template_file.write(template_content)
            template_path = template_file.name

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as output_file:
            output_path = output_file.name

        try:
            render_html(
                template_path,
                output_path,
                forecast_date="Test Date",
                forecast_content=forecast_data,
            )

            with open(output_path) as f:
                result = f.read()

            assert "<h3>Provided Area</h3>" in result
            assert "<dd>Provided wind data</dd>" in result

        finally:
            os.unlink(template_path)
            os.unlink(output_path)


class TestFormatRelativeDateTime:
    def test_met_office_format_with_time_yesterday(self):
        """Test Met Office format with time for yesterday."""
        from datetime import datetime, timedelta
        from zoneinfo import ZoneInfo

        # Get yesterday's date dynamically
        london_tz = ZoneInfo("Europe/London")
        yesterday = datetime.now(london_tz) - timedelta(days=1)

        # Format yesterday as a Met Office datetime string
        day_names = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
        month_names = [
            "Jan",
            "Feb",
            "Mar",
            "Apr",
            "May",
            "Jun",
            "Jul",
            "Aug",
            "Sep",
            "Oct",
            "Nov",
            "Dec",
        ]

        day_name = day_names[yesterday.weekday()]
        month_name = month_names[yesterday.month - 1]

        yesterday_str = (
            f"12:00 (UTC) on {day_name} {yesterday.day} {month_name} {yesterday.year}"
        )

        result = _format_relative_datetime(yesterday_str, "met_office")

        assert "yesterday" in result
        assert "1pm" in result

    def test_met_office_format_with_time_tomorrow(self):
        """Test Met Office format with time for tomorrow."""
        from datetime import datetime, timedelta
        from zoneinfo import ZoneInfo

        # Get tomorrow's date dynamically
        london_tz = ZoneInfo("Europe/London")
        tomorrow = datetime.now(london_tz) + timedelta(days=1)

        # Format tomorrow as a Met Office datetime string
        day_names = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
        month_names = [
            "Jan",
            "Feb",
            "Mar",
            "Apr",
            "May",
            "Jun",
            "Jul",
            "Aug",
            "Sep",
            "Oct",
            "Nov",
            "Dec",
        ]

        day_name = day_names[tomorrow.weekday()]
        month_name = month_names[tomorrow.month - 1]

        tomorrow_str = (
            f"12:00 (UTC) on {day_name} {tomorrow.day} {month_name} {tomorrow.year}"
        )

        result = _format_relative_datetime(tomorrow_str, "met_office")

        assert "tomorrow" in result
        assert "1pm" in result
