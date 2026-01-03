#!/usr/bin/env bash
set -euo pipefail

# Format all Haskell files in the workspace using fourmolu

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

cd "$ROOT_DIR"

DIRS=(
    "fhir-types/src"
    "fhir-json/src"
    "fhir-xml/src"
    "fhir-validation/src"
)

MODE="${1:-inplace}"

if [[ "$MODE" != "inplace" && "$MODE" != "check" ]]; then
    echo "Usage: $0 [inplace|check]"
    echo "  inplace - format files in place (default)"
    echo "  check   - check formatting without modifying"
    exit 1
fi

for dir in "${DIRS[@]}"; do
    if [[ -d "$dir" ]]; then
        echo "Formatting $dir..."
        find "$dir" -name "*.hs" -exec fourmolu --mode "$MODE" {} +
    fi
done

echo "Done."
