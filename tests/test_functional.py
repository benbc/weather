import tempfile
from pathlib import Path

import pytest

from weather.renderer import render_html

# Mark as integration test to exclude from coverage
pytestmark = pytest.mark.integration


class TestFunctionalEndToEnd:
    def test_full_weather_pipeline_works(self):
        """
        Functional test that demonstrates the whole script works end-to-end.

        This test calls the renderer without mocking, so it will:
        - Scrape live data from Met Office
        - Fetch live ECMWF data (including the redirect issue)
        - Generate a complete HTML page

        We expect this to currently be broken due to the ECMWF redirect issue.
        """
        # Create a temporary output file
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as tmp:
            temp_output_path = tmp.name

        try:
            # Use the actual template from the project
            template_path = "src/weather/templates/index.html"

            # This should work end-to-end without any mocking
            render_html(template_path, temp_output_path)

            # Verify output file was created and contains expected content
            output_file = Path(temp_output_path)
            assert output_file.exists()

            content = output_file.read_text()

            # Check that the HTML contains the expected sections
            assert "Lyme Regis to Lands End" in content
            assert "ECMWF" in content
            assert "Meteogram" in content
            assert "<html" in content
            assert "</html>" in content

        finally:
            # Clean up temporary file
            Path(temp_output_path).unlink(missing_ok=True)
