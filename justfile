# Run the weather forecast scraper
scrape:
    uv run python src/weather/scraper.py

# Generate the weather HTML page (for deployment)
render:
    uv run python -m src.weather.renderer

# Generate the weather HTML page for local development
dev-render:
    uv run python -c "from src.weather.renderer import render_html; render_html('src/weather/templates/index.html', 'output/index.html')"

# Run tests
test:
    uv run pytest

# Run tests with coverage report
coverage:
    uv run pytest --cov-report=html

# Check code formatting and linting
lint:
    uv run ruff check .

# Format code with ruff
format:
    uv run ruff format .

# Fix auto-fixable linting issues
lint-fix:
    uv run ruff check --fix .

# Pre-commit checks (format, lint, tests with coverage)
pre-commit:
    uv run ruff format .
    uv run ruff check .
    uv run pytest
