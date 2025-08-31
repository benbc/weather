# Generate the weather HTML page (for deployment)
render:
    uv run python -m src.weather.renderer

# Generate the weather HTML page for local development
dev-render:
    uv run python -m src.weather.dev_renderer

# Run tests
test:
    uv run pytest

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

# Find sailing locations from human-friendly descriptions
find-location description:
    uv run python -m src.weather.location_cli "{{description}}"

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

# Check the status of the latest deployment
check-deployment:
    @echo "📊 Latest deployment status:"
    gh run list --workflow="Update Weather Forecast" --limit=3
    @echo ""
    @echo "🔍 Detailed view of latest run:"
    gh run view $(gh run list --workflow="Update Weather Forecast" --limit=1 --json databaseId --jq '.[0].databaseId')

# View logs of the latest deployment
deployment-logs:
    @echo "📜 Viewing logs from latest deployment..."
    gh run view $(gh run list --workflow="Update Weather Forecast" --limit=1 --json databaseId --jq '.[0].databaseId') --log
