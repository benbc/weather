# Run the weather forecast scraper
scrape:
    uv run python src/weather/scraper.py

# Generate the weather HTML page
render:
    uv run python -m src.weather.renderer

# Run tests
test:
    uv run pytest

# Run tests with coverage report
coverage:
    uv run pytest --cov-report=html

# Pre-commit checks (tests with coverage)
pre-commit:
    uv run pytest
