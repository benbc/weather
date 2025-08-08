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

# Run tests with detailed coverage showing missing lines
coverage-check:
    uv run pytest --cov=src/weather --cov-report=term-missing --cov-fail-under=100

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

# Verify current sailing locations against actual ECMWF coordinates  
verify-locations:
    uv run python src/weather/location_tools.py

# Test a specific coordinate to see what ECMWF location it maps to
test-coordinate lat lon:
    uv run python -c "from src.weather.location_tools import *; from src.weather.scraper import *; import sys; base_time = get_latest_ecmwf_base_time()['base_time']; url = generate_meteogram_url({{lat}}, {{lon}}, base_time); print(f'Testing coordinates: {{lat}}°N, {{lon}}°W'); print(f'Meteogram URL: {url}'); img_bytes = fetch_meteogram_image(url); extract_coordinates_from_meteogram(img_bytes)"

# Test distance calculation with known points  
test-distance:
    uv run python -c "from src.weather.location_tools import calculate_distance_km; print('Testing distance calculation:'); d1 = calculate_distance_km(49.97, -4.95, 49.95, -5.02); print(f'The Lizard offset: {d1:.2f}km'); d2 = calculate_distance_km(50.32, -3.57, 50.30, -3.64); print(f'River Dart offset: {d2:.2f}km'); print('For reference: 1 nautical mile = 1.852km')"

# Optimize a location using grid search (small search for testing)
optimize-location-test name lat lon:
    uv run python -c "from src.weather.location_optimizer import *; results = grid_search_location('{{name}}', {{lat}}, {{lon}}, grid_step=0.02, search_radius=0.06, delay_seconds=0.5); print_optimization_summary('{{name}}', {{lat}}, {{lon}}, results); analyze_grid_pattern(results)"

# Optimize The Lizard location  
optimize-lizard:
    just optimize-location-test "The Lizard (Lizard Peninsula area)" 49.97 -4.95

# Optimize River Dart location
optimize-dart:  
    just optimize-location-test "River Dart (1M off entrance)" 50.32 -3.57
