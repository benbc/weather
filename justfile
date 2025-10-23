run:
    cabal run weather

test:
    cabal test

# Generate the weather HTML page (for deployment)
render:
    uv run python -m src.weather.renderer

# Generate the weather HTML page for local development
dev-render:
    uv run python -m src.weather.dev_renderer

# Verify code quality and tests (read-only checks)
verify:
    uv run ruff check .
    uv run pytest

# Fix code formatting and linting issues, then verify
fix:
    uv run ruff format .
    uv run ruff check --fix .
    just verify

# Deploy: pull, push, run workflow, wait for completion, and show logs if there are problems
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
    @if gh run watch --exit-status --compact $(gh run list --workflow="Update Weather Forecast" --limit=1 --json databaseId --jq '.[0].databaseId'); then \
        echo "✅ Deployment completed successfully!"; \
    else \
        echo "❌ Deployment failed!"; \
        echo ""; \
        echo "📊 Deployment status:"; \
        gh run list --workflow="Update Weather Forecast" --limit=3; \
        echo ""; \
        echo "📜 Deployment logs:"; \
        gh run view $(gh run list --workflow="Update Weather Forecast" --limit=1 --json databaseId --jq '.[0].databaseId') --log; \
        exit 1; \
    fi
