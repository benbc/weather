import pytest
import requests
import responses

from weather.scraper import get_forecast_date, get_lyme_regis_lands_end_forecast


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
