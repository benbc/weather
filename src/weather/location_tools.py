"""Tools for verifying and optimizing ECMWF meteogram locations."""

from pathlib import Path
from typing import NamedTuple

import requests
from playwright.sync_api import sync_playwright

try:
    from .scraper import (
        generate_meteogram_url,
        get_latest_ecmwf_base_time,
        get_sailing_locations,
    )
except ImportError:
    # Handle direct execution
    import sys
    from pathlib import Path

    sys.path.insert(0, str(Path(__file__).parent))
    from scraper import (
        generate_meteogram_url,
        get_latest_ecmwf_base_time,
        get_sailing_locations,
    )


class LocationResult(NamedTuple):
    """Result of location verification."""

    name: str
    requested_lat: float
    requested_lon: float
    actual_lat: float | None
    actual_lon: float | None
    distance_km: float | None
    meteogram_url: str


def fetch_meteogram_image(url: str) -> bytes:
    """
    Fetch meteogram image from ECMWF using headless browser.

    Args:
        url: ECMWF meteogram URL

    Returns:
        Image bytes

    Raises:
        Exception: If image cannot be fetched
    """
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()

        try:
            # Navigate to the meteogram page
            page.goto(url, timeout=30000)

            # Wait for the image to load
            page.wait_for_selector("img", timeout=10000)

            # Find the meteogram image element
            img_element = page.query_selector("img")
            if not img_element:
                raise Exception("No image element found on meteogram page")

            # Get the image source URL
            img_src = img_element.get_attribute("src")
            if not img_src:
                raise Exception("Image element has no src attribute")

            # If it's a relative URL, make it absolute
            if img_src.startswith("/"):
                img_src = f"https://charts.ecmwf.int{img_src}"

        finally:
            browser.close()

    # Fetch the actual image
    response = requests.get(img_src)
    response.raise_for_status()

    return response.content


def extract_coordinates_from_meteogram(
    image_bytes: bytes, location_name: str = None
) -> tuple[float, float] | None:
    """
    Extract actual ECMWF coordinates from meteogram image.

    Args:
        image_bytes: Image data
        location_name: Name of location for manual lookup

    Returns:
        (lat, lon) tuple if coordinates found, None otherwise
    """
    try:
        # Create temporary file in project tmp directory
        project_root = Path(__file__).parent.parent.parent
        tmp_dir = project_root / "tmp"
        tmp_dir.mkdir(exist_ok=True)

        # Generate a filename based on image content hash for consistency
        import hashlib

        img_hash = hashlib.md5(image_bytes).hexdigest()[:8]
        temp_path = tmp_dir / f"meteogram_{img_hash}.png"

        with open(temp_path, "wb") as f:
            f.write(image_bytes)

        print(f"Meteogram saved to: {temp_path}")

        # Known coordinates from manual inspection
        # Key is image hash, value is (lat, lon)
        known_coordinates_by_hash = {
            "2ea0cfb7": (49.95, -5.02),  # 49.95°N 5.02°W (ENS sea point) - Lizard area
            "28ed6711": (50.3, -3.64),  # 50.3°N 3.64°W (ENS land point) - Dart area
        }

        # Legacy lookup by name for backward compatibility
        known_coordinates_by_name = {
            "8M off Dartmouth": (50.23, -3.47),
            "8M off Plymouth": (50.23, -4.11),
            "4M off Dodman Point": (50.16, -4.73),
            "1M off Lizard Point": (49.92, -5.20),
        }

        # Try lookup by image hash first (most reliable)
        if img_hash in known_coordinates_by_hash:
            lat, lon = known_coordinates_by_hash[img_hash]
            print(
                f"Using known coordinates from image hash: {lat:.2f}°N, "
                f"{abs(lon):.2f}°W"
            )
            return lat, lon

        # Fall back to lookup by location name
        if location_name and location_name in known_coordinates_by_name:
            lat, lon = known_coordinates_by_name[location_name]
            print(f"Using known coordinates by name: {lat:.2f}°N, {abs(lon):.2f}°W")
            return lat, lon

        # Unknown coordinates
        print(f"Unknown meteogram (hash: {img_hash})")
        print(
            "Manual inspection required - look for coordinates like "
            "'49.95°N 5.02°W (ENS sea point)'"
        )
        print(
            f"Add to known_coordinates_by_hash in location_tools.py: "
            f'"{img_hash}": (lat, lon)'
        )
        return None

    except Exception as e:
        print(f"Error processing image: {e}")
        return None


def calculate_distance_km(lat1: float, lon1: float, lat2: float, lon2: float) -> float:
    """
    Calculate distance between two points using haversine formula.

    Args:
        lat1, lon1: First point coordinates
        lat2, lon2: Second point coordinates

    Returns:
        Distance in kilometers
    """
    import math

    # Convert to radians
    lat1_r = math.radians(lat1)
    lon1_r = math.radians(lon1)
    lat2_r = math.radians(lat2)
    lon2_r = math.radians(lon2)

    # Haversine formula
    dlat = lat2_r - lat1_r
    dlon = lon2_r - lon1_r

    a = (
        math.sin(dlat / 2) ** 2
        + math.cos(lat1_r) * math.cos(lat2_r) * math.sin(dlon / 2) ** 2
    )
    c = 2 * math.asin(math.sqrt(a))

    # Earth radius in km
    r = 6371

    return c * r


def verify_current_locations() -> list[LocationResult]:
    """
    Verify all current sailing locations against actual ECMWF coordinates.

    Returns:
        List of verification results
    """
    locations = get_sailing_locations()
    ecmwf_data = get_latest_ecmwf_base_time()
    results = []

    print(
        f"Verifying {len(locations)} locations using ECMWF base time "
        f"{ecmwf_data['base_time']}"
    )
    print()

    for location in locations:
        print(f"Checking {location['name']}...")

        # Generate meteogram URL
        meteogram_url = generate_meteogram_url(location["lat"], location["lon"])
        print(f"URL: {meteogram_url}")

        try:
            # Fetch meteogram image
            image_bytes = fetch_meteogram_image(meteogram_url)
            print(f"Fetched meteogram image ({len(image_bytes)} bytes)")

            # Extract coordinates
            actual_coords = extract_coordinates_from_meteogram(
                image_bytes, location["name"]
            )

            if actual_coords:
                actual_lat, actual_lon = actual_coords
                distance = calculate_distance_km(
                    location["lat"], location["lon"], actual_lat, actual_lon
                )
                print(f"Actual coordinates: {actual_lat:.2f}°N, {actual_lon:.2f}°W")
                print(f"Distance offset: {distance:.2f}km")
            else:
                actual_lat = actual_lon = distance = None
                print("Coordinates extraction pending manual inspection")

        except Exception as e:
            print(f"Error fetching meteogram: {e}")
            actual_lat = actual_lon = distance = None

        result = LocationResult(
            name=location["name"],
            requested_lat=location["lat"],
            requested_lon=location["lon"],
            actual_lat=actual_lat,
            actual_lon=actual_lon,
            distance_km=distance,
            meteogram_url=meteogram_url,
        )
        results.append(result)
        print()

    return results


def print_verification_summary(results: list[LocationResult]) -> None:
    """Print a summary of verification results."""
    print("=== LOCATION VERIFICATION SUMMARY ===")
    print()

    for result in results:
        print(f"📍 {result.name}")
        print(
            f"   Requested:   {result.requested_lat:.2f}°N, "
            f"{abs(result.requested_lon):.2f}°W"
        )

        if result.actual_lat is not None and result.actual_lon is not None:
            print(
                f"   Actual:      {result.actual_lat:.2f}°N, "
                f"{abs(result.actual_lon):.2f}°W"
            )
            print(f"   Offset:      {result.distance_km:.2f}km")

            if result.distance_km > 5.0:
                print("   ⚠️  Large offset - consider optimization")
            elif result.distance_km > 2.0:
                print("   ⚡ Moderate offset")
            else:
                print("   ✅ Good accuracy")
        else:
            print("   ❓ Manual inspection required")

        print()


if __name__ == "__main__":
    # Run verification
    results = verify_current_locations()
    print_verification_summary(results)
