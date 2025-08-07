import os
import tempfile
from datetime import UTC, datetime, timezone
from unittest.mock import patch

import pytest

from weather.renderer import render_html


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

            assert "Monday 06 Jan 2025" in result
            assert "UTC" in result
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

            assert "Tuesday 07 Jan 2025" in result
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

            assert "Unable to retrieve forecast date" in result

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
        """Test that last updated timestamp is in correct UTC format."""
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
            with patch("weather.renderer.datetime") as mock_datetime:
                mock_now = datetime(2025, 1, 7, 12, 30, 45, tzinfo=UTC)
                mock_datetime.now.return_value = mock_now
                mock_datetime.timezone = timezone

                render_html(template_path, output_path, forecast_date="Test")

                with open(output_path) as f:
                    result = f.read()

                assert "2025-01-07 12:30:45 UTC" in result

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

            assert "Test ✓" in result
            assert "✓" in result

        finally:
            os.unlink(template_path)
            os.unlink(output_path)

    def test_template_not_found_raises_exception(self):
        """Test that missing template file raises appropriate exception."""
        with pytest.raises(FileNotFoundError):
            render_html(
                "/nonexistent/template.html", "/tmp/output.html", forecast_date="Test"
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
            render_html(template_path, output_path, forecast_date="Test Date")

            with open(output_path) as f:
                result = f.read()

            assert result.count("Test Date") == 2
            assert result.count("UTC") == 2
            assert "{forecast_date}" not in result
            assert "{last_updated}" not in result

        finally:
            os.unlink(template_path)
            os.unlink(output_path)
