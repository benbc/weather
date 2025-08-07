import pytest
import responses
from weather.scraper import get_forecast_date


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
            responses.GET,
            "https://example.com/forecast",
            body=html_content,
            status=200
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
            responses.GET,
            "https://example.com/forecast",
            body=html_content,
            status=200
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
            responses.GET,
            "https://example.com/forecast",
            body=html_content,
            status=200
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
            responses.GET,
            "https://example.com/forecast",
            body=html_content,
            status=200
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
            responses.GET,
            "https://example.com/forecast",
            body=html_content,
            status=200
        )
        
        with pytest.raises(Exception, match="Could not find latest forecast date"):
            get_forecast_date("https://example.com/forecast")

    @responses.activate
    def test_handles_http_error(self):
        """Test that HTTP errors are properly raised."""
        responses.add(
            responses.GET,
            "https://example.com/forecast",
            status=404
        )
        
        with pytest.raises(Exception):
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
            responses.GET,
            "https://example.com/forecast",
            body=html_content,
            status=200
        )
        
        result = get_forecast_date("https://example.com/forecast")
        
        assert result == "Tuesday 07 Jan 2025 at 05:00"