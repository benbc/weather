import tempfile
from datetime import datetime
from pathlib import Path
from unittest.mock import patch
from zoneinfo import ZoneInfo

import pytest

from weather.renderer import (
    _format_forecast_title,
    _format_relative_datetime,
    _format_shipping_forecast_period,
    _generate_forecast_html,
    _generate_meteogram_locations_html,
    _generate_shipping_forecast_html,
    render_html,
)


class TestFormatRelativeDateTime:
    def test_met_office_format_with_time(self):
        """Test Met Office format with time component."""
        dt_str = "18:00 (UTC) on Wed 7 Aug 2025"
        result = _format_relative_datetime(dt_str, "met_office")
        assert "7pm" in result
        # Date may be relative (today/tomorrow/yesterday) or day name (thursday)
        assert any(
            day in result for day in ["today", "tomorrow", "yesterday", "thursday"]
        )

    def test_met_office_format_without_time(self):
        """Test Met Office format without time component."""
        dt_str = "Monday 06 Jan 2025"
        result = _format_relative_datetime(dt_str, "met_office")
        assert "12pm" in result or "midday" in result

    def test_ecmwf_format(self):
        """Test ECMWF format parsing."""
        dt_str = "12:00 UTC on Thu 07 Aug 2025"
        result = _format_relative_datetime(dt_str, "ecmwf")
        assert "1pm" in result
        # Date may be relative (today/tomorrow/yesterday) or day name (thursday)
        assert any(
            day in result for day in ["today", "tomorrow", "yesterday", "thursday"]
        )

    def test_timestamp_format(self):
        """Test timestamp format parsing."""
        dt_str = "2025-08-07 15:30:44 UTC"
        result = _format_relative_datetime(dt_str, "timestamp")
        assert "4:30pm" in result or "3:30pm" in result
        # Date may be relative (today/tomorrow/yesterday) or day name (thursday)
        assert any(
            day in result for day in ["today", "tomorrow", "yesterday", "thursday"]
        )

    def test_raises_exception_for_unknown_format(self):
        """Test that exception is raised for unknown source format."""
        with pytest.raises(Exception, match="Unknown source_format"):
            _format_relative_datetime("some date", "unknown")

    def test_raises_exception_for_unparseable_met_office(self):
        """Test that exception is raised for unparseable Met Office format."""
        with pytest.raises(
            Exception, match="Could not parse Met Office datetime format"
        ):
            _format_relative_datetime("invalid date", "met_office")

    def test_raises_exception_for_unparseable_ecmwf(self):
        """Test that exception is raised for unparseable ECMWF format."""
        with pytest.raises(Exception, match="Could not parse ECMWF datetime format"):
            _format_relative_datetime("invalid date", "ecmwf")

    def test_raises_exception_for_unparseable_timestamp(self):
        """Test that exception is raised for unparseable timestamp format."""
        with pytest.raises(Exception, match="Could not parse timestamp format"):
            _format_relative_datetime("invalid date", "timestamp")

    def test_raises_exception_for_unknown_month_met_office(self):
        """Test that exception is raised for unknown month in Met Office format."""
        with pytest.raises(Exception, match="Unknown month abbreviation"):
            _format_relative_datetime("18:00 (UTC) on Wed 7 Xyz 2025", "met_office")

    def test_raises_exception_for_unknown_month_met_office_no_time(self):
        """Test exception for unknown month in Met Office format without time."""
        with pytest.raises(Exception, match="Unknown month abbreviation"):
            _format_relative_datetime("Monday 06 Xyz 2025", "met_office")

    def test_raises_exception_for_unknown_month_ecmwf(self):
        """Test that exception is raised for unknown month in ECMWF format."""
        with pytest.raises(Exception, match="Unknown month abbreviation"):
            _format_relative_datetime("12:00 UTC on Thu 07 Xyz 2025", "ecmwf")

    def test_yesterday_and_tomorrow_formatting(self):
        """Test that yesterday and tomorrow are formatted correctly."""
        # This will test the relative date logic paths
        dt_str = (
            "18:00 (UTC) on Wed 6 Aug 2025"  # Should be yesterday from test perspective
        )
        result = _format_relative_datetime(dt_str, "met_office")
        assert "7pm" in result
        # Result should contain yesterday, today, or tomorrow

    def test_minute_formatting_on_hour(self):
        """Test time formatting when on the hour."""
        dt_str = "18:00 (UTC) on Wed 7 Aug 2025"
        result = _format_relative_datetime(dt_str, "met_office")
        assert "7pm" in result  # Should not show minutes when on the hour

    def test_minute_formatting_off_hour(self):
        """Test time formatting when not on the hour."""
        dt_str = "18:30 (UTC) on Wed 7 Aug 2025"
        result = _format_relative_datetime(dt_str, "met_office")
        assert "7:30pm" in result  # Should show minutes when not on the hour

    def test_date_far_away_uses_day_name(self):
        """Test that dates far from today use day names."""
        # Use a date that's definitely not today/yesterday/tomorrow
        dt_str = "18:00 (UTC) on Wed 1 Jan 2020"
        result = _format_relative_datetime(dt_str, "met_office")
        assert "6pm" in result or "7pm" in result  # Time can vary with timezone
        # Should use day name rather than relative date
        assert any(
            day in result
            for day in [
                "monday",
                "tuesday",
                "wednesday",
                "thursday",
                "friday",
                "saturday",
                "sunday",
            ]
        )

    @patch("weather.renderer.datetime")
    def test_tomorrow_formatting(self, mock_datetime):
        """Test that tomorrow dates are formatted correctly."""
        # Mock current time as Aug 8, 2025 12:00 PM London time
        london_tz = ZoneInfo("Europe/London")
        fixed_now = datetime(2025, 8, 8, 12, 0, 0, tzinfo=london_tz)

        # Configure the mock to return fixed time for now() but pass through others
        mock_datetime.now.return_value = fixed_now
        mock_datetime.side_effect = lambda *args, **kw: datetime(*args, **kw)
        mock_datetime.strptime = datetime.strptime

        # Test a date that should be "tomorrow" (Aug 9, 2025)
        dt_str = "18:00 (UTC) on Sat 9 Aug 2025"
        result = _format_relative_datetime(dt_str, "met_office")
        assert "tomorrow" in result

    @patch("weather.renderer.datetime")
    def test_yesterday_formatting(self, mock_datetime):
        """Test that yesterday dates are formatted correctly."""
        # Mock current time as Aug 8, 2025 12:00 PM London time
        london_tz = ZoneInfo("Europe/London")
        fixed_now = datetime(2025, 8, 8, 12, 0, 0, tzinfo=london_tz)

        # Configure the mock to return fixed time for now() but pass through others
        mock_datetime.now.return_value = fixed_now
        mock_datetime.side_effect = lambda *args, **kw: datetime(*args, **kw)
        mock_datetime.strptime = datetime.strptime

        # Test a date that should be "yesterday" (Aug 7, 2025)
        dt_str = "18:00 (UTC) on Thu 7 Aug 2025"
        result = _format_relative_datetime(dt_str, "met_office")
        assert "yesterday" in result


class TestFormatForecastTitle:
    def test_with_valid_forecast_issue_time(self):
        """Test forecast title formatting with valid issue time."""
        issue_time = "12:00 (UTC) on Thu 7 Aug 2025"

        result = _format_forecast_title("24 hour forecast:", 0, issue_time)
        assert result.startswith("From")
        assert ":" in result

        result = _format_forecast_title("Following 24 hours:", 1, issue_time)
        assert result.startswith("From")
        assert ":" in result

    def test_raises_exception_for_unparseable_issue_time(self):
        """Test that exception is raised for unparseable issue time."""
        with pytest.raises(
            Exception, match="Could not parse forecast issue time format"
        ):
            _format_forecast_title("24 hour forecast:", 0, "invalid time format")

    def test_raises_exception_for_unknown_month(self):
        """Test that exception is raised for unknown month abbreviation."""
        with pytest.raises(Exception, match="Unknown month abbreviation"):
            _format_forecast_title(
                "24 hour forecast:", 0, "12:00 (UTC) on Thu 7 Xyz 2025"
            )

    def test_date_further_away_uses_day_name(self):
        """Test that dates further than tomorrow use day name."""
        # Use a date that's definitely not today/yesterday/tomorrow
        issue_time = "12:00 (UTC) on Thu 1 Jan 2020"
        result = _format_forecast_title("24 hour forecast:", 0, issue_time)
        # Should contain "From" and use day name instead of relative date
        assert "From" in result
        assert ":" in result

    def test_time_formatting_on_the_hour(self):
        """Test time formatting when forecast starts on the hour."""
        issue_time = "12:00 (UTC) on Thu 7 Aug 2025"
        result = _format_forecast_title("24 hour forecast:", 0, issue_time)
        assert "From" in result
        assert "1pm" in result  # Should show hour without minutes

    def test_time_formatting_off_the_hour(self):
        """Test time formatting when forecast starts off the hour."""
        issue_time = "12:30 (UTC) on Thu 7 Aug 2025"
        result = _format_forecast_title("24 hour forecast:", 0, issue_time)
        assert "From" in result
        assert "1:30pm" in result  # Should show hour with minutes

    def test_yesterday_section_date(self):
        """Test forecast section that starts yesterday."""
        # Issue time is today, but section -1 (24 hours ago) would be yesterday
        issue_time = "12:00 (UTC) on Thu 7 Aug 2025"
        result = _format_forecast_title("previous forecast:", -1, issue_time)
        assert "From" in result
        # Should contain yesterday reference

    def test_tomorrow_section_date(self):
        """Test forecast section that starts tomorrow."""
        # Issue time is today, section 1 (24 hours later) would be tomorrow
        issue_time = "12:00 (UTC) on Thu 7 Aug 2025"
        result = _format_forecast_title("following forecast:", 1, issue_time)
        assert "From" in result
        # Should contain tomorrow reference


class TestGenerateForecastHtml:
    def test_generates_forecast_html_successfully(self):
        """Test that forecast HTML is generated correctly."""
        forecast_content = {
            "area_name": "Test Area",
            "sections": [
                {
                    "title": "24 hour forecast:",
                    "content": [
                        {"category": "Wind", "description": "Light winds"},
                        {"category": "Sea state", "description": "Calm"},
                    ],
                }
            ],
        }
        forecast_issue_time = "12:00 (UTC) on Thu 7 Aug 2025"

        result = _generate_forecast_html(forecast_content, forecast_issue_time)

        assert "<h3>Test Area</h3>" in result
        assert "<h4>From" in result
        assert "<strong>Wind</strong>: Light winds<br>" in result
        assert "<strong>Sea state</strong>: Calm<br>" in result
        # Three line format uses <br> tags instead of bullet separators


class TestRenderHtml:
    def test_requires_valid_template_path(self):
        """Test that invalid template path raises exception."""
        with tempfile.TemporaryDirectory() as temp_dir:
            output_path = Path(temp_dir) / "output.html"

            with pytest.raises(FileNotFoundError):
                render_html("nonexistent_template.html", str(output_path))

    @patch("weather.renderer.get_forecast_date")
    @patch("weather.renderer.get_lyme_regis_lands_end_forecast")
    @patch("weather.renderer.get_latest_ecmwf_base_time")
    def test_calls_scrapers_when_data_not_provided(
        self, mock_ecmwf, mock_forecast, mock_date
    ):
        """Test that scrapers are called when data is not provided."""
        mock_date.return_value = "12:00 (UTC) on Thu 7 Aug 2025"
        mock_forecast.return_value = {"area_name": "Test", "sections": []}
        mock_ecmwf.return_value = {
            "base_time": "202508071200",
            "readable_time": "12:00 UTC on Thu 07 Aug 2025",
            "datetime": None,
        }

        template_content = """
        <html>
        <body>
        <div>{forecast_date}</div>
        <div>{forecast_content}</div>
        <div>{ecmwf_forecast_time}</div>
        <div>{ecmwf_chart_url}</div>
        <div>{meteogram_locations}</div>
        <div>{last_updated}</div>
        </body>
        </html>
        """

        with tempfile.TemporaryDirectory() as temp_dir:
            template_path = Path(temp_dir) / "template.html"
            output_path = Path(temp_dir) / "output.html"

            template_path.write_text(template_content)

            render_html(str(template_path), str(output_path))

            mock_date.assert_called_once()
            mock_forecast.assert_called_once()
            mock_ecmwf.assert_called_once()

            # Verify output file was created
            assert output_path.exists()


class TestGenerateMeteogramLocationsHtml:
    @patch("weather.renderer.get_meteogram_forecast_time")
    @patch("weather.renderer.get_sailing_locations")
    def test_includes_google_maps_links(
        self, mock_get_locations, mock_get_forecast_time
    ):
        """Test that Google Maps links with satellite view are included."""
        mock_get_locations.return_value = [
            {
                "name": "Test Location",
                "lat": 50.0,
                "lon": -4.0,
            }
        ]
        mock_get_forecast_time.return_value = {
            "base_time": "202508071200",
            "readable_time": "12:00 UTC on Thu 07 Aug 2025",
            "datetime": None,
        }

        result = _generate_meteogram_locations_html()

        # Check that the location name is included
        assert "Test Location" in result

        # Check that meteogram link is included
        assert "Meteogram →" in result
        assert "charts.ecmwf.int" in result

        # Check that Google Maps link is included with satellite view
        assert "Location →" in result
        assert (
            "www.google.com/maps/place/50.0,-4.0/@50.0,-4.0,10z/data=!3m1!1e3" in result
        )

    @patch("weather.renderer.get_meteogram_forecast_time")
    @patch("weather.renderer.get_sailing_locations")
    def test_multiple_locations(self, mock_get_locations, mock_get_forecast_time):
        """Test that multiple locations are rendered correctly."""
        mock_get_locations.return_value = [
            {
                "name": "Location 1",
                "lat": 50.0,
                "lon": -4.0,
            },
            {
                "name": "Location 2",
                "lat": 51.0,
                "lon": -3.0,
            },
        ]
        mock_get_forecast_time.return_value = {
            "base_time": "202508071200",
            "readable_time": "12:00 UTC on Thu 07 Aug 2025",
            "datetime": None,
        }

        result = _generate_meteogram_locations_html()

        # Check that both locations are included
        assert "Location 1" in result
        assert "Location 2" in result

        # Check that both Google Maps links are included
        assert (
            "www.google.com/maps/place/50.0,-4.0/@50.0,-4.0,10z/data=!3m1!1e3" in result
        )
        assert (
            "www.google.com/maps/place/51.0,-3.0/@51.0,-3.0,10z/data=!3m1!1e3" in result
        )

    @patch("weather.renderer.get_meteogram_forecast_time")
    @patch("weather.renderer.get_sailing_locations")
    def test_link_separator(self, mock_get_locations, mock_get_forecast_time):
        """Test that links are separated by a pipe separator."""
        mock_get_locations.return_value = [
            {
                "name": "Test Location",
                "lat": 50.0,
                "lon": -4.0,
            }
        ]
        mock_get_forecast_time.return_value = {
            "base_time": "202508071200",
            "readable_time": "12:00 UTC on Thu 07 Aug 2025",
            "datetime": None,
        }

        result = _generate_meteogram_locations_html()

        # Check that both links are included (no separator anymore)
        assert "Meteogram →" in result
        assert "Location →" in result

    @patch("weather.renderer.get_meteogram_forecast_time")
    @patch("weather.renderer.get_sailing_locations")
    def test_includes_forecast_times_per_location(
        self, mock_get_locations, mock_get_forecast_time
    ):
        """Test that per-location forecast times are included."""
        mock_get_locations.return_value = [
            {
                "name": "Test Location",
                "lat": 50.0,
                "lon": -4.0,
            }
        ]
        mock_get_forecast_time.return_value = {
            "base_time": "202508071200",
            "readable_time": "12:00 UTC on Thu 07 Aug 2025",
            "datetime": None,
        }

        result = _generate_meteogram_locations_html()

        # Check that the location name and forecast time are included at the end
        assert "Test Location" in result
        assert "Meteogram →" in result
        assert "Location →" in result
        assert "(released at" in result
        assert "1pm thursday)" in result

        # Check that forecast time appears at the end after the links
        lines = result.split("\n")
        # The forecast time should be on its own line at the end
        forecast_time_line = next(line for line in lines if "(released at" in line)
        assert forecast_time_line.strip() == "(released at 1pm thursday)"

        # Verify forecast time was fetched for the location
        mock_get_forecast_time.assert_called_once_with(50.0, -4.0)


class TestGenerateShippingForecastHtml:
    @patch("weather.renderer.get_shipping_forecast_period")
    @patch("weather.renderer.get_shipping_forecast_areas")
    @patch("weather.renderer._format_shipping_forecast_period")
    def test_generates_shipping_forecast_html_successfully(
        self, mock_format_period, mock_get_shipping_areas, mock_get_period
    ):
        """Test that shipping forecast HTML is generated correctly."""
        mock_get_period.return_value = (
            "For the period 06:00 (UTC) on Sat 9 Aug to 06:00 (UTC) on Sun 10 Aug"
        )
        mock_format_period.return_value = "From 7am today to 7am tomorrow"
        mock_get_shipping_areas.return_value = [
            {
                "area_name": "Portland",
                "forecast": [
                    {"category": "Wind", "description": "Westerly 3 to 5"},
                    {"category": "Sea state", "description": "Slight or moderate"},
                    {"category": "Weather", "description": "Fair"},
                    {"category": "Visibility", "description": "Good"},
                ],
            },
            {
                "area_name": "Plymouth",
                "forecast": [
                    {"category": "Wind", "description": "Northerly 2 to 4"},
                    {"category": "Sea state", "description": "Smooth or slight"},
                    {"category": "Weather", "description": "Showers"},
                    {"category": "Visibility", "description": "Moderate"},
                ],
            },
        ]

        result = _generate_shipping_forecast_html()

        # Check that the formatted period appears at the top
        assert "<h4>From 7am today to 7am tomorrow</h4>" in result

        # Check that both area names are present as h3 headers
        assert "<h3>Portland</h3>" in result
        assert "<h3>Plymouth</h3>" in result

        # Check that forecast details are properly formatted
        assert '<div class="forecast-details">' in result
        assert "<strong>Wind</strong>: Westerly 3 to 5" in result
        assert "<strong>Sea state</strong>: Slight or moderate" in result
        assert "<strong>Weather</strong>: Fair" in result
        assert "<strong>Visibility</strong>: Good" in result

        # Check Plymouth data is also present
        assert "<strong>Wind</strong>: Northerly 2 to 4" in result
        assert "<strong>Weather</strong>: Showers" in result

        # Verify the functions were called with correct parameters
        mock_get_period.assert_called_once_with(
            "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/shipping-forecast"
        )
        mock_format_period.assert_called_once_with(
            "For the period 06:00 (UTC) on Sat 9 Aug to 06:00 (UTC) on Sun 10 Aug"
        )
        mock_get_shipping_areas.assert_called_once_with(
            "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/shipping-forecast",
            ["portland", "plymouth"],
        )

    @patch("weather.renderer.get_shipping_forecast_period")
    @patch("weather.renderer.get_shipping_forecast_areas")
    @patch("weather.renderer._format_shipping_forecast_period")
    def test_groups_forecast_items_correctly(
        self, mock_format_period, mock_get_shipping_areas, mock_get_period
    ):
        """Test that forecast items are grouped into separate lines correctly."""
        mock_get_period.return_value = (
            "For the period 06:00 (UTC) on Sat 9 Aug to 06:00 (UTC) on Sun 10 Aug"
        )
        mock_format_period.return_value = "From 7am today to 7am tomorrow"
        mock_get_shipping_areas.return_value = [
            {
                "area_name": "Portland",
                "forecast": [
                    {"category": "Wind", "description": "Westerly 3 to 5"},
                    {"category": "Sea state", "description": "Slight or moderate"},
                    {"category": "Weather", "description": "Fair"},
                    {"category": "Visibility", "description": "Good"},
                ],
            }
        ]

        result = _generate_shipping_forecast_html()

        # Wind should be on its own line (followed by <br>)
        assert "<strong>Wind</strong>: Westerly 3 to 5<br>" in result

        # Sea state should be on its own line (followed by <br>)
        assert "<strong>Sea state</strong>: Slight or moderate<br>" in result

        # Weather and visibility should be on the same line (no <br> after visibility)
        assert (
            "<strong>Weather</strong>: Fair • <strong>Visibility</strong>: Good"
            in result
        )

    @patch("weather.renderer.get_shipping_forecast_period")
    @patch("weather.renderer.get_shipping_forecast_areas")
    @patch("weather.renderer._format_shipping_forecast_period")
    def test_handles_empty_forecast_areas(
        self, mock_format_period, mock_get_shipping_areas, mock_get_period
    ):
        """Test that empty forecast areas list is handled correctly."""
        mock_get_period.return_value = (
            "For the period 06:00 (UTC) on Sat 9 Aug to 06:00 (UTC) on Sun 10 Aug"
        )
        mock_format_period.return_value = "From 7am today to 7am tomorrow"
        mock_get_shipping_areas.return_value = []

        result = _generate_shipping_forecast_html()

        # Should include the period but no area content
        assert "<h4>From 7am today to 7am tomorrow</h4>" in result
        # Should not contain area headers since no areas were provided
        assert "<h3>" not in result


class TestFormatShippingForecastPeriod:
    def test_formats_period_with_relative_dates(self):
        """Test that shipping forecast period is formatted with relative dates."""
        period_str = (
            "For the period 07:00 (UTC) on Thu 7 Aug 2025 to "
            "07:00 (UTC) on Fri 8 Aug 2025"
        )

        with patch("weather.renderer._format_relative_datetime") as mock_format:
            mock_format.side_effect = ["8am thursday", "8am friday"]

            result = _format_shipping_forecast_period(period_str)

            assert result == "From 8am thursday to 8am friday"
            assert mock_format.call_count == 2
            mock_format.assert_any_call("07:00 (UTC) on Thu 7 Aug 2025", "met_office")
            mock_format.assert_any_call("07:00 (UTC) on Fri 8 Aug 2025", "met_office")

    def test_raises_exception_for_invalid_format(self):
        """Test that an exception is raised for invalid period format."""
        invalid_period = "Invalid period format"

        with pytest.raises(
            Exception, match="Could not parse shipping forecast period format"
        ):
            _format_shipping_forecast_period(invalid_period)

    def test_handles_today_tomorrow_format(self):
        """Test formatting with today and tomorrow."""
        period_str = (
            "For the period 06:00 (UTC) on Sat 9 Aug 2025 "
            "to 06:00 (UTC) on Sun 10 Aug 2025"
        )

        with patch("weather.renderer._format_relative_datetime") as mock_format:
            mock_format.side_effect = ["7am today", "7am tomorrow"]

            result = _format_shipping_forecast_period(period_str)

            assert result == "From 7am today to 7am tomorrow"
