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

# Deploy: pull, push, run workflow, and wait for completion
deploy:
    @echo "🚀 Starting deployment process..."
    @echo "📥 Pulling latest changes..."
    git pull --rebase
    @echo "📤 Pushing local changes..."
    git push
    @echo "⚡ Triggering workflow..."
    gh workflow run "Update Weather Forecast"
    @echo "⏳ Waiting for workflow to start..."
    sleep 10
    @echo "👀 Watching workflow until completion..."
    gh run watch --exit-status --compact $(gh run list --workflow="Update Weather Forecast" --limit=1 --json databaseId --jq '.[0].databaseId')
    @echo "✅ Deployment completed successfully!"

# Test deployment commands without actually deploying
deploy-dry-run:
    @echo "🧪 Testing deployment commands (dry run)..."
    @echo "📥 Would pull latest changes with: git pull --rebase"
    @echo "📤 Would push local changes with: git push" 
    @echo "⚡ Would trigger workflow with: gh workflow run 'Update Weather Forecast'"
    @echo "📊 Current workflow status:"
    gh run list --workflow="Update Weather Forecast" --limit=1
    @echo "✅ Dry run complete. Use 'just deploy' to actually deploy."

# Check the status of the latest deployment
check-deployment:
    @echo "📊 Latest deployment status:"
    gh run list --workflow="Update Weather Forecast" --limit=3
    @echo ""
    @echo "🔍 Detailed view of latest run:"
    gh run view $(gh run list --workflow="Update Weather Forecast" --limit=1 --json databaseId --jq '.[0].databaseId')

# Watch deployment progress in real-time  
watch-deployment:
    @echo "👀 Watching deployment progress (press Ctrl+C to stop)..."
    watch -n 10 'gh run list --workflow="Update Weather Forecast" --limit=1'

# View logs of the latest deployment
deployment-logs:
    @echo "📜 Viewing logs from latest deployment..."
    gh run view $(gh run list --workflow="Update Weather Forecast" --limit=1 --json databaseId --jq '.[0].databaseId') --log

# Find sailing locations from human-friendly descriptions
find-location description:
    @echo "🔍 Searching for sea points matching: {{description}}"
    uv run python -c "from src.weather.location_finder import find_location_from_description; results = find_location_from_description('{{description}}'); print(f'Found {len(results)} sea points'); [print(f'  {i+1}. {r.suggested_description} - ECMWF: {r.actual_ecmwf_coords[0]:.3f}°N, {abs(r.actual_ecmwf_coords[1]):.3f}°W') for i, r in enumerate(results[:5])]"

# Test location description parsing
parse-location description:
    @echo "🔤 Parsing location description: {{description}}"
    uv run python -c "from src.weather.location_finder import parse_location_description; result = parse_location_description('{{description}}'); print('Parsed result:', result)"

# Check current locations for sea vs land points  
check-location-types:
    @echo "🌊 Checking current location types (sea vs land)..."
    uv run python -c "from src.weather.location_finder import is_sea_point_from_meteogram; from src.weather.scraper import get_sailing_locations; locations = get_sailing_locations(); print('Current locations:'); [print(f'  {loc[\"name\"]}: {loc[\"description\"]}') for loc in locations]; print('\\nSea/Land status will be shown when meteograms are fetched.')"

# Test small search around a landmark (limited for testing)
test-search landmark distance:
    @echo "🧪 Testing location search: {{distance}}M around {{landmark}}"
    uv run python -c "from src.weather.location_finder import search_for_sea_point; results = search_for_sea_point('{{landmark}}', {{distance}}, max_search_radius_nm=1.0, grid_step_km=5.0); print(f'\\nFound {len(results)} sea points'); [print(f'{i+1}. {r.suggested_description} - ECMWF: {r.actual_ecmwf_coords[0]:.3f}°N, {abs(r.actual_ecmwf_coords[1]):.3f}°W - {r.distance_from_landmark_nm:.1f}M') for i, r in enumerate(results[:3])]"

# Test the description generator with rounding to nearest nautical mile
test-descriptions:
    @echo "🔤 Testing description generator with various distances..."
    uv run python -c "from src.weather.location_finder import generate_rounded_description; test_distances = [1.2, 1.7, 2.4, 3.8, 5.1]; [print(f'{dist}M → {generate_rounded_description(\"dartmouth\", dist)}') for dist in test_distances]"
