"""CLI for finding sailing locations from descriptions."""

import sys

from .location_finder import find_location_from_description


def main() -> None:
    """Find sailing locations from description provided as command line argument."""
    if len(sys.argv) != 2:
        print("Usage: python -m src.weather.location_cli '<description>'")
        sys.exit(1)

    description = sys.argv[1]
    print(f"🔍 Searching for sea points matching: {description}")

    results = find_location_from_description(description)
    print(f"Found {len(results)} sea points")

    for i, result in enumerate(results[:5]):
        lat_str = f"{result.actual_ecmwf_coords[0]:.3f}°N"
        lon_str = f"{abs(result.actual_ecmwf_coords[1]):.3f}°W"
        print(
            f"  {i + 1}. {result.suggested_description} - ECMWF: {lat_str}, {lon_str}"
        )


if __name__ == "__main__":
    main()
