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
    def test_raises_exception_when_area8_not_found(self):
        """Test that exception is raised when Area 8 section is not found."""
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

        with pytest.raises(
            Exception, match="Could not find Area 8.*section in Met Office forecast"
        ):
            get_lyme_regis_lands_end_forecast("https://example.com/forecast")

    @responses.activate
    def test_raises_exception_when_section_has_no_marine_card_parent(self):
        """Test exception is raised when h2 exists but has no marine-card parent."""
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

        with pytest.raises(
            Exception,
            match="Found Area 8 heading but could not find marine-card section",
        ):
            get_lyme_regis_lands_end_forecast("https://example.com/forecast")

    @responses.activate
    def test_raises_exception_when_no_forecast_sections(self):
        """Test that exception is raised when no forecast sections are found."""
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

        with pytest.raises(
            Exception,
            match="Found Area 8 section but could not extract any forecast data",
        ):
            get_lyme_regis_lands_end_forecast("https://example.com/forecast")

    @responses.activate
    def test_handles_http_error(self):
        """Test that HTTP errors are properly raised."""
        responses.add(responses.GET, "https://example.com/forecast", status=404)

        with pytest.raises(requests.HTTPError):
            get_lyme_regis_lands_end_forecast("https://example.com/forecast")

    @responses.activate
    def test_raises_exception_for_malformed_html(self):
        """Test that exception is raised for malformed HTML structure."""
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

        with pytest.raises(
            Exception,
            match="Found Area 8 section but could not extract any forecast data",
        ):
            get_lyme_regis_lands_end_forecast("https://example.com/forecast")


class TestGetLatestEcmwfBaseTime:
    @patch("playwright.sync_api.sync_playwright")
    def test_extracts_base_time_from_redirect(self, mock_playwright):
        """Test that base_time is extracted from ECMWF redirect URL."""
        # Mock the Playwright chain
        mock_p = mock_playwright.return_value.__enter__.return_value
        mock_browser = mock_p.chromium.launch.return_value
        mock_page = mock_browser.new_page.return_value
        mock_page.url = "https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe&base_time=202508071200"

        result = get_latest_ecmwf_base_time()

        assert isinstance(result, dict)
        assert result["base_time"] == "202508071200"
        assert "12:00 UTC" in result["readable_time"]
        assert "07 Aug 2025" in result["readable_time"]

        # Verify Playwright was called correctly
        mock_p.chromium.launch.assert_called_once_with(headless=True)
        mock_browser.new_page.assert_called_once()
        mock_page.goto.assert_called_once()
        mock_browser.close.assert_called_once()

    @patch("playwright.sync_api.sync_playwright")
    def test_raises_exception_when_no_base_time_in_redirect(self, mock_playwright):
        """Test that exception is raised when redirect URL doesn't contain base_time."""
        # Mock the Playwright chain
        mock_p = mock_playwright.return_value.__enter__.return_value
        mock_browser = mock_p.chromium.launch.return_value
        mock_page = mock_browser.new_page.return_value
        mock_page.url = "https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe"

        with pytest.raises(
            Exception, match="ECMWF redirect URL does not contain base_time parameter"
        ):
            get_latest_ecmwf_base_time()

    @patch("playwright.sync_api.sync_playwright")
    def test_raises_exception_on_browser_failure(self, mock_playwright):
        """Test that browser exceptions are raised properly."""
        # Mock the Playwright chain to raise an exception
        mock_p = mock_playwright.return_value.__enter__.return_value
        mock_browser = mock_p.chromium.launch.return_value
        mock_page = mock_browser.new_page.return_value
        mock_page.goto.side_effect = Exception("Connection failed")

        with pytest.raises(Exception, match="Connection failed"):
            get_latest_ecmwf_base_time()

        # Verify browser cleanup still happened
        mock_browser.close.assert_called_once()

    @patch("playwright.sync_api.sync_playwright")
    def test_raises_exception_when_base_time_malformed(self, mock_playwright):
        """Test that exception is raised when base_time format is invalid."""
        # Mock the Playwright chain
        mock_p = mock_playwright.return_value.__enter__.return_value
        mock_browser = mock_p.chromium.launch.return_value
        mock_page = mock_browser.new_page.return_value
        mock_page.url = "https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe&base_time=invalid"

        with pytest.raises(
            Exception, match="Could not extract base_time from ECMWF URL"
        ):
            get_latest_ecmwf_base_time()

    @patch("playwright.sync_api.sync_playwright")
    def test_browser_cleanup_on_success(self, mock_playwright):
        """Test that browser is properly closed on successful execution."""
        # Mock the Playwright chain
        mock_p = mock_playwright.return_value.__enter__.return_value
        mock_browser = mock_p.chromium.launch.return_value
        mock_page = mock_browser.new_page.return_value
        mock_page.url = "https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe&base_time=202508071200"

        get_latest_ecmwf_base_time()

        # Verify browser cleanup
        mock_browser.close.assert_called_once()


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
        assert "lat" in location
        assert "lon" in location

        # Check data types
        assert isinstance(location["name"], str)
        assert isinstance(location["lat"], int | float)
        assert isinstance(location["lon"], int | float)

    def test_contains_expected_locations(self):
        """Test that expected sailing locations are included."""
        result = get_sailing_locations()

        location_names = [loc["name"] for loc in result]
        assert "8M off Dartmouth" in location_names
        assert "8M off Plymouth" in location_names
        assert "4M off Dodman Point" in location_names
        assert "1M off Lizard Point" in location_names

        # Check specific location data
        dartmouth = next(loc for loc in result if loc["name"] == "8M off Dartmouth")
        assert dartmouth["lat"] == 50.23
        assert dartmouth["lon"] == -3.47

        plymouth = next(loc for loc in result if loc["name"] == "8M off Plymouth")
        assert plymouth["lat"] == 50.23
        assert plymouth["lon"] == -4.11

        dodman = next(loc for loc in result if loc["name"] == "4M off Dodman Point")
        assert dodman["lat"] == 50.16
        assert dodman["lon"] == -4.73

        lizard = next(loc for loc in result if loc["name"] == "1M off Lizard Point")
        assert lizard["lat"] == 49.92
        assert lizard["lon"] == -5.20


class TestGenerateMeteogramUrl:
    def test_generates_correct_url_format(self):
        """Test that meteogram URL is generated with correct format."""
        lat, lon, base_time = 49.95, -5.02, "202508070000"

        result = generate_meteogram_url(lat, lon, base_time)

        expected_url = (
            "https://charts.ecmwf.int/products/opencharts_meteogram?"
            "base_time=202508070000&epsgram=classical_15d&lat=49.95&lon=-5.02"
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
        result = generate_meteogram_url(49.95, -5.02, "202508070000")

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
            result = generate_meteogram_url(49.95, -5.02, base_time)
            assert f"base_time={base_time}" in result
