"""Location finder for human-friendly sailing location descriptions."""

import re
from dataclasses import dataclass
from typing import NamedTuple

try:
    from .location_tools import (
        calculate_distance_km,
        extract_coordinates_from_meteogram,
        fetch_meteogram_image,
    )
    from .scraper import generate_meteogram_url
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
    from scraper import generate_meteogram_url


@dataclass
class Landmark:
    """A known landmark for distance calculations."""

    name: str
    lat: float
    lon: float


class SeaPointResult(NamedTuple):
    """Result from sea point search."""

    coords: tuple[float, float]  # (lat, lon)
    actual_ecmwf_coords: tuple[float, float]  # What ECMWF actually uses
    distance_from_landmark_km: float
    distance_from_landmark_nm: float
    is_sea_point: bool
    meteogram_url: str
    suggested_description: str


# Known landmarks for sailing areas
LANDMARKS = {
    "dartmouth": Landmark("Dartmouth", 50.3513, -3.5806),
    "plymouth": Landmark("Plymouth", 50.3675, -4.1436),
    "salcombe": Landmark("Salcombe", 50.2390, -3.7696),
    "torquay": Landmark("Torquay", 50.4619, -3.5253),
    "brixham": Landmark("Brixham", 50.3947, -3.5154),
    "lizard": Landmark("Lizard Point", 49.9581, -5.2032),
    "lizard point": Landmark("Lizard Point", 49.9581, -5.2032),
    "the lizard": Landmark("Lizard Point", 49.9581, -5.2032),
    "falmouth": Landmark("Falmouth", 50.1532, -5.0707),
    "dodman": Landmark("Dodman Point", 50.2082, -4.7932),
    "dodman point": Landmark("Dodman Point", 50.2082, -4.7932),
}


def parse_location_description(description: str) -> tuple[str, float] | None:
    """
    Parse a human-friendly location description.

    Args:
        description: Description like "2M off Dartmouth" or "3 miles south of Plymouth"

    Returns:
        (landmark_name, distance_nautical_miles) or None if not parseable
    """
    description = description.lower().strip()

    # Pattern: "XM off/south/north/east/west of LANDMARK"
    patterns = [
        r"(\d+(?:\.\d+)?)m?\s+off\s+(.+)",
        r"(\d+(?:\.\d+)?)\s*(?:miles?|m)\s+off\s+(.+)",
        r"(\d+(?:\.\d+)?)\s*(?:miles?|m)\s+(?:south|north|east|west)\s+of\s+(.+)",
        r"(\d+(?:\.\d+)?)m?\s+(?:south|north|east|west)\s+(.+)",
    ]

    for pattern in patterns:
        match = re.match(pattern, description)
        if match:
            distance_str, landmark_name = match.groups()
            distance = float(distance_str)
            landmark_name = landmark_name.strip()

            # Check if landmark is known
            if landmark_name in LANDMARKS:
                return landmark_name, distance

    return None


def is_sea_point_from_meteogram(image_bytes: bytes) -> bool | None:
    """
    Determine if ECMWF coordinates represent a sea point from meteogram image.

    Args:
        image_bytes: Meteogram image data

    Returns:
        True if sea point, False if land point, None if unknown
    """
    import hashlib

    img_hash = hashlib.md5(image_bytes).hexdigest()[:8]

    # Known sea/land points from our verification and searches
    known_point_types = {
        # Verified points
        "2ea0cfb7": True,  # The Lizard - sea point (49.95°N 5.02°W)
        "28ed6711": False,  # River Dart - land point (50.3°N 3.64°W)
        # Points discovered during Dartmouth search
        "1e9cf758": False,  # Land point (50.3°N 3.64°W) 54 m
        "14856168": False,  # Land point (50.37°N 3.64°W) 0 m
        "32e85c4e": False,  # Land point (50.37°N 3.64°W) 39 m
        "027af5c5": False,  # Land point (50.37°N 3.64°W) 54 m
        "d2ba246f": False,  # Land point (50.3°N 3.64°W) 47 m
        "7c90c169": True,  # Sea point (50.3°N 3.48°W) 0 m
        "3ba751ba": False,  # Land point (50.3°N 3.64°W) 22 m
        "4a673d58": False,  # Land point (50.3°N 3.64°W) 76 m
        "f9213154": True,  # Sea point (50.23°N 3.47°W) 0 m
        # Points discovered during Plymouth, Dodman, and Lizard searches
        "27a67ed0": True,  # Sea point (50.23°N 4.11°W) 0 m - Plymouth area
        "188652e6": True,  # Sea point (50.16°N 4.73°W) 0 m - Dodman area
        "07dd3724": True,  # Sea point (49.92°N 5.20°W) 0 m - Lizard area
        "87b31f9a": False,  # Land point (50.02°N 5.18°W) 0 m - Lizard coast
    }

    if img_hash in known_point_types:
        return known_point_types[img_hash]

    # For unknown points, save the image and return None
    # This allows manual inspection and future addition to known_point_types
    print(f"      Unknown meteogram hash: {img_hash} - manual inspection needed")
    return None


def add_known_point_type(image_bytes: bytes, is_sea: bool):
    """
    Add a newly discovered point type to the known points database.
    This is a helper function for expanding our sea/land point knowledge.
    """
    import hashlib

    img_hash = hashlib.md5(image_bytes).hexdigest()[:8]

    point_type = "sea" if is_sea else "land"
    print(f"📝 To add this {point_type} point to known types, add:")
    print(f'    "{img_hash}": {is_sea},  # Add description here')
    print("    to the known_point_types dictionary in location_finder.py")


def search_for_sea_point(
    landmark_name: str,
    target_distance_nm: float,
    max_search_radius_nm: float = 5.0,
    grid_step_km: float = 2.0,
) -> list[SeaPointResult]:
    """
    Search for sea points at approximately the target distance from a landmark.

    Args:
        landmark_name: Name of landmark (must be in LANDMARKS)
        target_distance_nm: Desired distance in nautical miles
        max_search_radius_nm: Maximum search radius around target distance
        grid_step_km: Grid step size for search in km

    Returns:
        List of sea points found, sorted by proximity to target distance
    """
    if landmark_name not in LANDMARKS:
        raise ValueError(f"Unknown landmark: {landmark_name}")

    landmark = LANDMARKS[landmark_name]

    target_distance_km = target_distance_nm * 1.852
    max_search_radius_km = max_search_radius_nm * 1.852

    results = []

    print(f"🔍 Searching for sea points ~{target_distance_nm}M from {landmark.name}")
    print(f"   Target distance: {target_distance_km:.1f}km")
    print(f"   Search radius: ±{max_search_radius_km:.1f}km")

    # Create a circular search pattern around the landmark
    # Search at various bearings from the landmark, focusing on likely offshore
    bearings = [
        135,
        180,
        225,
    ]  # South/Southwest directions (likely offshore for UK coast)
    distances = []

    # Create distance rings (fewer for testing)
    min_dist = max(1.0, target_distance_km - max_search_radius_km)
    max_dist = target_distance_km + max_search_radius_km

    dist = min_dist
    while dist <= max_dist:
        distances.append(dist)
        dist += max(grid_step_km, 3.0)  # Minimum 3km steps

    print(
        f"   Testing {len(bearings)} bearings at {len(distances)} distances = "
        f"{len(bearings) * len(distances)} points"
    )

    point_count = 0
    total_points = len(bearings) * len(distances)

    for bearing in bearings:
        for distance_km in distances:
            point_count += 1

            # Calculate test coordinates using bearing and distance
            import math

            bearing_rad = math.radians(bearing)

            # Earth radius in km
            R = 6371.0

            # Convert landmark to radians
            lat1 = math.radians(landmark.lat)
            lon1 = math.radians(landmark.lon)

            # Calculate destination point
            lat2 = math.asin(
                math.sin(lat1) * math.cos(distance_km / R)
                + math.cos(lat1) * math.sin(distance_km / R) * math.cos(bearing_rad)
            )

            lon2 = lon1 + math.atan2(
                math.sin(bearing_rad) * math.sin(distance_km / R) * math.cos(lat1),
                math.cos(distance_km / R) - math.sin(lat1) * math.sin(lat2),
            )

            # Convert back to degrees
            test_lat = math.degrees(lat2)
            test_lon = math.degrees(lon2)

            print(
                f"   [{point_count}/{total_points}] Testing {test_lat:.3f}°N, "
                f"{abs(test_lon):.3f}°W ({bearing}° @ {distance_km:.1f}km)"
            )

            try:
                # Generate meteogram URL and fetch
                meteogram_url = generate_meteogram_url(test_lat, test_lon)
                image_bytes = fetch_meteogram_image(meteogram_url)

                # Extract actual ECMWF coordinates
                actual_coords = extract_coordinates_from_meteogram(image_bytes)

                if actual_coords:
                    actual_lat, actual_lon = actual_coords

                    # Check if it's a sea point
                    is_sea = is_sea_point_from_meteogram(image_bytes)

                    # Calculate actual distance from landmark to ECMWF point
                    actual_distance_km = calculate_distance_km(
                        landmark.lat, landmark.lon, actual_lat, actual_lon
                    )
                    actual_distance_nm = actual_distance_km / 1.852

                    # Generate suggested description
                    suggested_desc = generate_rounded_description(
                        landmark_name, actual_distance_nm
                    )

                    result = SeaPointResult(
                        coords=(test_lat, test_lon),
                        actual_ecmwf_coords=(actual_lat, actual_lon),
                        distance_from_landmark_km=actual_distance_km,
                        distance_from_landmark_nm=actual_distance_nm,
                        is_sea_point=is_sea,
                        meteogram_url=meteogram_url,
                        suggested_description=suggested_desc,
                    )

                    results.append(result)

                    sea_status = (
                        "🌊 SEA"
                        if is_sea
                        else "🏔️ LAND"
                        if is_sea is False
                        else "❓ UNKNOWN"
                    )
                    print(
                        f"      → ECMWF: {actual_lat:.3f}°N, "
                        f"{abs(actual_lon):.3f}°W ({sea_status}) - "
                        f"{actual_distance_nm:.1f}M"
                    )

                # Small delay to be respectful
                import time

                time.sleep(0.2)

            except Exception as e:
                print(f"      → Error: {e}")

    # Filter and sort results
    sea_points = [r for r in results if r.is_sea_point is True]

    if sea_points:
        # Sort by proximity to target distance
        sea_points.sort(
            key=lambda x: abs(x.distance_from_landmark_nm - target_distance_nm)
        )
        print(f"\n✅ Found {len(sea_points)} sea points")
    else:
        print(f"\n❌ No sea points found. Found {len(results)} total points.")

    return sea_points


def generate_rounded_description(landmark_name: str, distance_nm: float) -> str:
    """
    Generate a human-friendly description rounded to the nearest nautical mile.

    Args:
        landmark_name: Name of the landmark (e.g., "dartmouth")
        distance_nm: Distance in nautical miles

    Returns:
        Description like "5M off Dartmouth"
    """
    if landmark_name not in LANDMARKS:
        raise ValueError(f"Unknown landmark: {landmark_name}")

    landmark = LANDMARKS[landmark_name]
    rounded_distance = round(distance_nm)

    return f"{rounded_distance}M off {landmark.name}"


def find_location_from_description(description: str) -> list[SeaPointResult]:
    """
    Find ECMWF sea points matching a human-friendly description.

    Args:
        description: Description like "2M off Dartmouth"

    Returns:
        List of matching sea points, sorted by relevance
    """
    parsed = parse_location_description(description)
    if not parsed:
        raise ValueError(f"Could not parse location description: {description}")

    landmark_name, target_distance = parsed

    return search_for_sea_point(landmark_name, target_distance)


if __name__ == "__main__":
    # Example usage
    print("Location Finder - Example Usage")

    # Test parsing
    test_descriptions = [
        "2M off Dartmouth",
        "3 miles south of Plymouth",
        "1M off Lizard Point",
    ]

    print("\n=== PARSING TESTS ===")
    for desc in test_descriptions:
        result = parse_location_description(desc)
        print(f"'{desc}' → {result}")
