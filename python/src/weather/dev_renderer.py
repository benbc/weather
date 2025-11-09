"""Development renderer entry point for local testing."""

from .renderer import render_html


def main() -> None:
    """Render HTML for local development."""
    template_path = "src/weather/templates/index.html"
    output_path = "../output/index.html"
    render_html(template_path, output_path)


if __name__ == "__main__":
    main()
