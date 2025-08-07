from unittest.mock import patch

import pytest
import requests
import responses

from weather.scraper import (
    generate_meteogram_url,
    get_forecast_date,
    get_latest_ecmwf_base_time,
    get_lyme_regis_lands_end_forecast,
    get_sailing_locations,
)


class TestGetForecastDate:
    @responses.activate
    def test_extracts_forecast_date_successfully(self):
        """Test that forecast date is extracted when found in HTML."""
        html_content = """
        <html>
            <div id="summary">
                <div class="times">
                    <p>Some other text</p>
                    <p>Issued at: <time>Tuesday 07 Jan 2025 at 05:00</time></p>
                </div>
            </div>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        result = get_forecast_date("https://example.com/forecast")

        assert result == "Tuesday 07 Jan 2025 at 05:00"

    @responses.activate
    def test_handles_multiple_time_elements(self):
        """Test that the correct time element is found when multiple exist."""
        html_content = """
        <html>
            <div id="summary">
                <div class="times">
                    <p>Updated: <time>Monday 06 Jan 2025 at 10:00</time></p>
                    <p>Issued at: <time>Tuesday 07 Jan 2025 at 05:00</time></p>
                </div>
            </div>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        result = get_forecast_date("https://example.com/forecast")

        assert result == "Tuesday 07 Jan 2025 at 05:00"

    @responses.activate
    def test_case_insensitive_issued_at_matching(self):
        """Test that 'issued at' matching is case insensitive."""
        html_content = """
        <html>
            <div id="summary">
                <div class="times">
                    <p>ISSUED AT: <time>Tuesday 07 Jan 2025 at 05:00</time></p>
                </div>
            </div>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        result = get_forecast_date("https://example.com/forecast")

        assert result == "Tuesday 07 Jan 2025 at 05:00"

    @responses.activate
    def test_raises_exception_when_no_forecast_date_found(self):
        """Test that an exception is raised when forecast date cannot be found."""
        html_content = """
        <html>
            <div id="summary">
                <div class="times">
                    <p>No forecast date here</p>
                </div>
            </div>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        with pytest.raises(Exception, match="Could not find latest forecast date"):
            get_forecast_date("https://example.com/forecast")

    @responses.activate
    def test_raises_exception_when_no_summary_div(self):
        """Test that an exception is raised when HTML structure is missing."""
        html_content = """
        <html>
            <p>No summary div here</p>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        with pytest.raises(Exception, match="Could not find latest forecast date"):
            get_forecast_date("https://example.com/forecast")

    @responses.activate
    def test_raises_exception_when_no_time_elements_found(self):
        """Test that an exception is raised when no time elements are found."""
        html_content = """
        <html>
            <div id="summary">
                <div class="times">
                    <p>No time elements here</p>
                </div>
            </div>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        with pytest.raises(Exception, match="Could not find latest forecast date"):
            get_forecast_date("https://example.com/forecast")

    @responses.activate
    def test_handles_http_error(self):
        """Test that HTTP errors are properly raised."""
        responses.add(responses.GET, "https://example.com/forecast", status=404)

        with pytest.raises(requests.HTTPError):
            get_forecast_date("https://example.com/forecast")

    @responses.activate
    def test_strips_whitespace_from_result(self):
        """Test that whitespace is stripped from the extracted date."""
        html_content = """
        <html>
            <div id="summary">
                <div class="times">
                    <p>Issued at: <time>  Tuesday 07 Jan 2025 at 05:00  </time></p>
                </div>
            </div>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        result = get_forecast_date("https://example.com/forecast")

        assert result == "Tuesday 07 Jan 2025 at 05:00"


class TestGetLymeRegisLandsEndForecast:
    @responses.activate
    def test_extracts_forecast_successfully(self):
        """Test that Area 8 forecast is extracted when found in HTML."""
        html_content = """
        <html>
            <section class="marine-card" aria-labelledby="area8">
                <h2 class="card-name" id="area8">
                    Lyme Regis to Lands End including the Isles of Scilly (8)
                </h2>
                <div class="card-content">
                    <h3>24 hour forecast:</h3>
                    <div class="forecast-info">
                        <dl>
                            <dt>Wind</dt>
                            <dd>West or southwest 3 to 5.</dd>
                            <dt>Sea state</dt>
                            <dd>Smooth or slight.</dd>
                        </dl>
                    </div>
                    <h3>Outlook for the following 24 hours:</h3>
                    <div class="forecast-info">
                        <dl>
                            <dt>Wind</dt>
                            <dd>Variable 2 to 4.</dd>
                            <dt>Weather</dt>
                            <dd>Fair.</dd>
                        </dl>
                    </div>
                </div>
            </section>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        result = get_lyme_regis_lands_end_forecast("https://example.com/forecast")

        assert result is not None
        assert (
            result["area_name"]
            == "Lyme Regis to Lands End including the Isles of Scilly (8)"
        )
        assert len(result["sections"]) == 2

        # Check first section
        assert result["sections"][0]["title"] == "24 hour forecast:"
        assert len(result["sections"][0]["content"]) == 2
        assert result["sections"][0]["content"][0]["category"] == "Wind"
        assert (
            result["sections"][0]["content"][0]["description"]
            == "West or southwest 3 to 5."
        )

        # Check second section
        assert result["sections"][1]["title"] == "Outlook for the following 24 hours:"
        assert result["sections"][1]["content"][0]["category"] == "Wind"
        assert result["sections"][1]["content"][1]["category"] == "Weather"

    @responses.activate
    def test_returns_none_when_area8_not_found(self):
        """Test that None is returned when Area 8 section is not found."""
        html_content = """
        <html>
            <section class="marine-card" aria-labelledby="area7">
                <h2 class="card-name" id="area7">Some Other Area</h2>
            </section>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        result = get_lyme_regis_lands_end_forecast("https://example.com/forecast")

        assert result is None

    @responses.activate
    def test_returns_none_when_section_has_no_marine_card_parent(self):
        """Test that None is returned when h2 exists but has no marine-card parent."""
        html_content = """
        <html>
            <h2 class="card-name" id="area8">
                Lyme Regis to Lands End including the Isles of Scilly (8)
            </h2>
            <!-- h2 exists but is not inside a section with marine-card class -->
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        result = get_lyme_regis_lands_end_forecast("https://example.com/forecast")

        assert result is None

    @responses.activate
    def test_returns_none_when_no_forecast_sections(self):
        """Test that None is returned when no forecast sections are found."""
        html_content = """
        <html>
            <section class="marine-card" aria-labelledby="area8">
                <h2 class="card-name" id="area8">
                    Lyme Regis to Lands End including the Isles of Scilly (8)
                </h2>
                <div class="card-content">
                    <p>No forecast sections here</p>
                </div>
            </section>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        result = get_lyme_regis_lands_end_forecast("https://example.com/forecast")

        assert result is None

    @responses.activate
    def test_handles_http_error(self):
        """Test that HTTP errors are properly raised."""
        responses.add(responses.GET, "https://example.com/forecast", status=404)

        with pytest.raises(requests.HTTPError):
            get_lyme_regis_lands_end_forecast("https://example.com/forecast")

    @responses.activate
    def test_handles_malformed_html_gracefully(self):
        """Test that malformed HTML structure is handled gracefully."""
        html_content = """
        <html>
            <section class="marine-card" aria-labelledby="area8">
                <h2 class="card-name" id="area8">
                    Lyme Regis to Lands End including the Isles of Scilly (8)
                </h2>
                <div class="card-content">
                    <h3>24 hour forecast:</h3>
                    <div class="forecast-info">
                        <!-- Missing dl element -->
                        <p>Some text without proper structure</p>
                    </div>
                </div>
            </section>
        </html>
        """
        responses.add(
            responses.GET, "https://example.com/forecast", body=html_content, status=200
        )

        result = get_lyme_regis_lands_end_forecast("https://example.com/forecast")

        assert result is None


class TestGetLatestEcmwfBaseTime:
    def test_returns_base_time_dict_with_required_fields(self):
        """Test that get_latest_ecmwf_base_time returns dict with required fields."""
        result = get_latest_ecmwf_base_time()

        assert isinstance(result, dict)
        assert "base_time" in result
        assert "readable_time" in result
        assert "datetime" in result

        # Check base_time format (YYYYMMDDHHMM)
        assert len(result["base_time"]) == 12
        assert result["base_time"].isdigit()

        # Check readable_time contains UTC
        assert "UTC" in result["readable_time"]

    def test_base_time_format_matches_ecmwf_pattern(self):
        """Test that base_time follows ECMWF format pattern."""
        result = get_latest_ecmwf_base_time()
        base_time = result["base_time"]

        # Extract components
        year = int(base_time[:4])
        month = int(base_time[4:6])
        day = int(base_time[6:8])
        hour = int(base_time[8:10])
        minute = int(base_time[10:12])

        # Validate ranges
        assert 2020 <= year <= 2030  # Reasonable year range
        assert 1 <= month <= 12
        assert 1 <= day <= 31
        assert hour in [0, 12]  # ECMWF forecast hours
        assert minute == 0  # Always 00 minutes

    def test_returns_consistent_format_across_calls(self):
        """Test that multiple calls return consistent format."""
        result1 = get_latest_ecmwf_base_time()
        result2 = get_latest_ecmwf_base_time()

        # Should have same structure
        assert set(result1.keys()) == set(result2.keys())

        # Base time format should be consistent
        assert len(result1["base_time"]) == len(result2["base_time"]) == 12

    def test_readable_time_format(self):
        """Test that readable_time has expected format."""
        result = get_latest_ecmwf_base_time()
        readable = result["readable_time"]

        # Should contain expected components
        assert "UTC" in readable
        assert ":" in readable  # Time separator
        assert any(
            day in readable for day in ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
        )

    def test_datetime_object_consistency(self):
        """Test that datetime object matches base_time string."""
        result = get_latest_ecmwf_base_time()

        # Extract datetime components
        dt = result["datetime"]
        expected_base_time = dt.strftime("%Y%m%d%H%M")

        assert result["base_time"] == expected_base_time

    @patch("weather.scraper.requests.get")
    def test_extracts_base_time_from_redirect(self, mock_get):
        """Test that base_time is extracted from ECMWF redirect URL."""
        # Mock the response to simulate ECMWF redirect
        mock_response = mock_get.return_value
        mock_response.url = "https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe&base_time=202508071200"

        result = get_latest_ecmwf_base_time()

        assert isinstance(result, dict)
        assert result["base_time"] == "202508071200"
        assert "12:00 UTC" in result["readable_time"]
        assert "07 Aug 2025" in result["readable_time"]

    @patch("weather.scraper.requests.get")
    def test_handles_redirect_without_base_time(self, mock_get):
        """Test fallback when redirect URL doesn't contain base_time."""
        # Mock response with URL that doesn't contain base_time parameter
        mock_response = mock_get.return_value
        mock_response.url = "https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe"

        result = get_latest_ecmwf_base_time()

        assert isinstance(result, dict)
        assert "base_time" in result
        assert "readable_time" in result
        assert "datetime" in result
        # Should use fallback logic
        assert result["base_time"].endswith("1200")

    @patch("weather.scraper.requests.get")
    def test_handles_request_exceptions(self, mock_get):
        """Test that request exceptions are handled gracefully."""
        # Mock requests.get to raise an exception
        mock_get.side_effect = requests.RequestException("Connection failed")

        # Should still return a result using fallback logic
        result = get_latest_ecmwf_base_time()

        assert isinstance(result, dict)
        assert "base_time" in result
        assert "readable_time" in result
        assert "datetime" in result
        # Should use fallback logic
        assert result["base_time"].endswith("1200")

    @patch("weather.scraper.requests.get")
    @patch("weather.scraper.datetime")
    def test_fallback_with_time_before_noon(self, mock_datetime, mock_get):
        """Test fallback logic when current time is before noon UTC."""
        from datetime import datetime

        # Mock requests.get to raise exception (force fallback)
        mock_get.side_effect = requests.RequestException("Connection failed")

        # Mock current time to be before noon UTC (e.g., 9am UTC)
        mock_now = datetime(2025, 8, 7, 9, 0, 0)  # 9am UTC
        mock_datetime.now.return_value = mock_now
        mock_datetime.side_effect = lambda *args, **kwargs: datetime(*args, **kwargs)

        # Should use yesterday's 12:00 UTC since today's noon hasn't happened yet
        result = get_latest_ecmwf_base_time()

        assert isinstance(result, dict)
        assert result["base_time"].endswith("1200")
        # Should be yesterday's date (day before mock_now)
        expected_date = "20250806"  # Day before 2025-08-07
        assert result["base_time"].startswith(expected_date)


class TestGetSailingLocations:
    def test_returns_list_of_locations(self):
        """Test that get_sailing_locations returns a list of location dictionaries."""
        result = get_sailing_locations()

        assert isinstance(result, list)
        assert len(result) > 0

        # Check first location structure
        location = result[0]
        assert isinstance(location, dict)
        assert "name" in location
        assert "description" in location
        assert "lat" in location
        assert "lon" in location

        # Check data types
        assert isinstance(location["name"], str)
        assert isinstance(location["description"], str)
        assert isinstance(location["lat"], int | float)
        assert isinstance(location["lon"], int | float)

    def test_contains_expected_locations(self):
        """Test that expected sailing locations are included."""
        result = get_sailing_locations()

        location_names = [loc["name"] for loc in result]
        assert "The Lizard" in location_names
        assert "River Dart" in location_names

        # Check specific location data
        lizard = next(loc for loc in result if loc["name"] == "The Lizard")
        assert lizard["lat"] == 49.97
        assert lizard["lon"] == -4.95

        dart = next(loc for loc in result if loc["name"] == "River Dart")
        assert dart["lat"] == 50.32
        assert dart["lon"] == -3.57


class TestGenerateMeteogramUrl:
    def test_generates_correct_url_format(self):
        """Test that meteogram URL is generated with correct format."""
        lat, lon, base_time = 49.97, -4.95, "202508070000"

        result = generate_meteogram_url(lat, lon, base_time)

        expected_url = (
            "https://charts.ecmwf.int/products/opencharts_meteogram?"
            "base_time=202508070000&epsgram=classical_15d&lat=49.97&lon=-4.95"
        )
        assert result == expected_url

    def test_handles_negative_coordinates(self):
        """Test that negative coordinates are handled correctly."""
        lat, lon, base_time = -12.34, -56.78, "202508070000"

        result = generate_meteogram_url(lat, lon, base_time)

        assert "lat=-12.34" in result
        assert "lon=-56.78" in result

    def test_handles_positive_coordinates(self):
        """Test that positive coordinates are handled correctly."""
        lat, lon, base_time = 12.34, 56.78, "202508070000"

        result = generate_meteogram_url(lat, lon, base_time)

        assert "lat=12.34" in result
        assert "lon=56.78" in result

    def test_includes_all_required_parameters(self):
        """Test that all required URL parameters are included."""
        result = generate_meteogram_url(49.97, -4.95, "202508070000")

        assert "base_time=" in result
        assert "epsgram=classical_15d" in result
        assert "lat=" in result
        assert "lon=" in result
        assert result.startswith(
            "https://charts.ecmwf.int/products/opencharts_meteogram?"
        )

    def test_different_base_times(self):
        """Test that different base times are handled correctly."""
        base_times = ["202508070000", "202508070600", "202508071200", "202508071800"]

        for base_time in base_times:
            result = generate_meteogram_url(49.97, -4.95, base_time)
            assert f"base_time={base_time}" in result
