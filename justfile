verify:
    just haskell/verify
    just python/verify

deploy:
    #!/usr/bin/env bash
    set -euo pipefail

    error() {
        echo >&2 "Error: $1"
        exit 1
    }

    timestamp() {
        curl --silent "$1" | grep --only-matching --perl-regexp 'Last updated: \K[^<]+'
    }

    [[ "$(git branch --show-current)" == "main" ]] || error "Not on main branch"

    just verify

    OLD_PYTHON_TIMESTAMP="$(timestamp https://benbc.github.io/weather/)"
    OLD_HASKELL_TIMESTAMP="$(timestamp https://benbc.github.io/weather/new/)"

    git push

    # Trigger a GitHub worflow run
    #
    # The GitHub API doesn't give us a way of identifying the specific run that we've triggered.
    # We use this tagging mechanism to avoid a race condition where we pick up another run that was
    # triggered right after this one. See update-forecast.yml for the rest of this mechanism.
    RUN_TAG="$(uuidgen)"
    gh workflow run "Update Weather Forecast" --field run_tag="$RUN_TAG"

    for i in {1..30}; do
        RUN_ID="$(gh run list --workflow="Update Weather Forecast" --limit 5 --json databaseId,name --jq ".[] | select(.name | contains(\"[TAG:$RUN_TAG]\")) | .databaseId")"
        [[ -n "$RUN_ID" ]] && break
        sleep 1
    done
    [[ -z "$RUN_ID" ]] && error "Failed to find triggered workflow run"

    gh run watch --exit-status "$RUN_ID"

    NEW_PYTHON_TIMESTAMP="$(timestamp https://benbc.github.io/weather/)"
    NEW_HASKELL_TIMESTAMP="$(timestamp https://benbc.github.io/weather/new/)"

    if [[ "$NEW_PYTHON_TIMESTAMP" != "$OLD_PYTHON_TIMESTAMP" ]] && [[ "$NEW_HASKELL_TIMESTAMP" != "$OLD_HASKELL_TIMESTAMP" ]]; then
        echo >&2 "Deployment successful"
    else
        [[ "$NEW_PYTHON_TIMESTAMP" == "$OLD_PYTHON_TIMESTAMP" ]] && echo >&2 "Python page timestamp unchanged"
        [[ "$NEW_HASKELL_TIMESTAMP" == "$OLD_HASKELL_TIMESTAMP" ]] && echo >&2 "Haskell page timestamp unchanged"
        error "Page timestamp(s) unchanged"
    fi
