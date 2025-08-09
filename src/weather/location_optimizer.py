"""Grid search optimization for ECMWF meteogram locations."""

import time
from typing import NamedTuple

try:
    from .location_tools import (
        calculate_distance_km,
        extract_coordinates_from_meteogram,
        fetch_meteogram_image,
    )
except ImportError:
    # Handle direct execution
    import sys
    from pathlib import Path

    sys.path.insert(0, str(Path(__file__).parent))
    from location_tools import (
        calculate_distance_km,
        extract_coordinates_from_meteogram,
        fetch_meteogram_image,
    )

try:
    from .scraper import generate_meteogram_url
except ImportError:
    # Handle direct execution
    import sys
    from pathlib import Path

    sys.path.insert(0, str(Path(__file__).parent))
    from scraper import generate_meteogram_url


class GridSearchResult(NamedTuple):
    """Result of grid search optimization."""

    test_lat: float
    test_lon: float
    actual_lat: float | None
    actual_lon: float | None
    distance_from_target: float | None
    distance_from_original: float | None


def grid_search_location(
    target_description: str,
    original_lat: float,
    original_lon: float,
    grid_step: float = 0.01,
    search_radius: float = 0.1,
    delay_seconds: float = 1.0,
) -> list[GridSearchResult]:
    """
    Perform grid search to find coordinates that result in ECMWF locations
    closer to the target sailing description.

    Args:
        target_description: Description of the intended sailing location
        original_lat: Original latitude to search around
        original_lon: Original longitude to search around
        grid_step: Step size in degrees (default 0.01° ≈ 1.1km)
        search_radius: Search radius in degrees (default 0.1° ≈ 11km)
        delay_seconds: Delay between requests to be respectful to ECMWF

    Returns:
        List of grid search results sorted by distance from target
    """
    print(f"Starting grid search for: {target_description}")
    print(f"Original coordinates: {original_lat:.3f}°N, {abs(original_lon):.3f}°W")
    print(f"Grid step: {grid_step:.3f}° (≈ {grid_step * 111:.1f}km)")
    print(f"Search radius: {search_radius:.3f}° (≈ {search_radius * 111:.1f}km)")
    print()

    results = []

    # Generate grid points
    lat_min = original_lat - search_radius
    lat_max = original_lat + search_radius
    lon_min = original_lon - search_radius
    lon_max = original_lon + search_radius

    lat_steps = int((lat_max - lat_min) / grid_step) + 1
    lon_steps = int((lon_max - lon_min) / grid_step) + 1
    total_points = lat_steps * lon_steps

    print(f"Testing {total_points} grid points...")

    point_count = 0
    for i in range(lat_steps):
        test_lat = lat_min + i * grid_step

        for j in range(lon_steps):
            test_lon = lon_min + j * grid_step
            point_count += 1

            print(
                f"[{point_count}/{total_points}] Testing {test_lat:.3f}°N, "
                f"{abs(test_lon):.3f}°W",
                end=" ",
            )

            try:
                # Generate meteogram URL
                meteogram_url = generate_meteogram_url(test_lat, test_lon)

                # Fetch meteogram image
                image_bytes = fetch_meteogram_image(meteogram_url)

                # Extract actual coordinates (will save image for unknown locations)
                actual_coords = extract_coordinates_from_meteogram(image_bytes)

                if actual_coords:
                    actual_lat, actual_lon = actual_coords

                    # Calculate distances
                    distance_from_original = calculate_distance_km(
                        test_lat, test_lon, original_lat, original_lon
                    )

                    # For now, use original coordinates as "target" - can be refined
                    distance_from_target = calculate_distance_km(
                        actual_lat, actual_lon, original_lat, original_lon
                    )

                    print(
                        f"→ {actual_lat:.3f}°N, {abs(actual_lon):.3f}°W "
                        f"(offset: {distance_from_target:.2f}km)"
                    )

                else:
                    actual_lat = actual_lon = distance_from_target = (
                        distance_from_original
                    ) = None
                    print("→ coordinates extraction failed")

                result = GridSearchResult(
                    test_lat=test_lat,
                    test_lon=test_lon,
                    actual_lat=actual_lat,
                    actual_lon=actual_lon,
                    distance_from_target=distance_from_target,
                    distance_from_original=distance_from_original,
                )
                results.append(result)

            except Exception as e:
                print(f"→ error: {e}")
                result = GridSearchResult(
                    test_lat=test_lat,
                    test_lon=test_lon,
                    actual_lat=None,
                    actual_lon=None,
                    distance_from_target=None,
                    distance_from_original=None,
                )
                results.append(result)

            # Be respectful to ECMWF servers
            if delay_seconds > 0:
                time.sleep(delay_seconds)

    # Sort by distance from target (best matches first)
    valid_results = [r for r in results if r.distance_from_target is not None]
    valid_results.sort(key=lambda x: x.distance_from_target)

    return valid_results


def analyze_grid_pattern(results: list[GridSearchResult]) -> None:
    """
    Analyze grid search results to identify ECMWF's grid pattern.
    """
    print("\n=== GRID PATTERN ANALYSIS ===")

    # Group results by actual ECMWF coordinates
    ecmwf_locations = {}
    for result in results:
        if result.actual_lat is not None and result.actual_lon is not None:
            key = (round(result.actual_lat, 3), round(result.actual_lon, 3))
            if key not in ecmwf_locations:
                ecmwf_locations[key] = []
            ecmwf_locations[key].append(result)

    print(f"\nFound {len(ecmwf_locations)} unique ECMWF locations:")
    for (lat, lon), test_points in ecmwf_locations.items():
        print(f"\nECMWF location: {lat:.3f}°N, {abs(lon):.3f}°W")
        print(f"  Triggered by {len(test_points)} test points:")
        for tp in test_points[:5]:  # Show first 5
            print(f"    {tp.test_lat:.3f}°N, {abs(tp.test_lon):.3f}°W")
        if len(test_points) > 5:
            print(f"    ... and {len(test_points) - 5} more")

    # Look for grid spacing patterns
    if len(ecmwf_locations) >= 2:
        lats = [lat for lat, lon in ecmwf_locations.keys()]
        lons = [lon for lat, lon in ecmwf_locations.keys()]

        lat_diffs = []
        lon_diffs = []

        for i in range(len(lats)):
            for j in range(i + 1, len(lats)):
                lat_diffs.append(abs(lats[i] - lats[j]))
                lon_diffs.append(abs(lons[i] - lons[j]))

        if lat_diffs:
            min_lat_diff = min(
                [d for d in lat_diffs if d > 0.001]
            )  # Ignore tiny differences
            min_lon_diff = min([d for d in lon_diffs if d > 0.001])

            print("\nGrid spacing hints:")
            print(
                f"  Minimum latitude difference: {min_lat_diff:.3f}° "
                f"(≈ {min_lat_diff * 111:.1f}km)"
            )
            print(
                f"  Minimum longitude difference: {min_lon_diff:.3f}° "
                f"(≈ {min_lon_diff * 111:.1f}km)"
            )


def print_optimization_summary(
    target_description: str,
    original_lat: float,
    original_lon: float,
    results: list[GridSearchResult],
) -> None:
    """Print summary of grid search optimization."""
    print(f"\n=== OPTIMIZATION SUMMARY: {target_description} ===")
    print(f"Original coordinates: {original_lat:.3f}°N, {abs(original_lon):.3f}°W")

    if not results:
        print("No valid results found.")
        return

    valid_results = [r for r in results if r.distance_from_target is not None]
    if not valid_results:
        print("No results with valid coordinates found.")
        return

    print(f"\nTested {len(results)} points, {len(valid_results)} successful")
    print("\nTop 5 optimized coordinates (closest to original intent):")

    for i, result in enumerate(valid_results[:5]):
        distance_nm = (
            result.distance_from_target / 1.852
        )  # Convert km to nautical miles
        print(f"{i + 1}. Test: {result.test_lat:.3f}°N, {abs(result.test_lon):.3f}°W")
        print(f"   → ECMWF: {result.actual_lat:.3f}°N, {abs(result.actual_lon):.3f}°W")
        print(f"   → Offset: {result.distance_from_target:.2f}km ({distance_nm:.1f}M)")
        print()

    # Recommend best coordinate
    best = valid_results[0]
    best_distance_nm = best.distance_from_target / 1.852
    print("🎯 RECOMMENDATION:")
    print(f"   Use coordinates: {best.test_lat:.3f}°N, {abs(best.test_lon):.3f}°W")
    print(f"   Results in ECMWF: {best.actual_lat:.3f}°N, {abs(best.actual_lon):.3f}°W")
    print(
        f"   Offset from original intent: {best.distance_from_target:.2f}km "
        f"({best_distance_nm:.1f}M)"
    )


if __name__ == "__main__":
    # Example usage
    print("Grid search optimization tool")
    print("Use 'just optimize-location' command for interactive optimization")
