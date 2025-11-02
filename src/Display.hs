module Display (formatHtml) where

import Forecast qualified

formatHtml :: Forecast.AreaForecast -> String
formatHtml forecast =
    "<!DOCTYPE html>\n"
        ++ "<html>\n"
        ++ "<head>\n"
        ++ "<meta charset=\"UTF-8\">\n"
        ++ "<title>Weather</title>\n"
        ++ "</head>\n"
        ++ "<body>\n"
        ++ "<h1>"
        ++ Forecast.areaName forecast
        ++ "</h1>\n"
        ++ "<h2>24 Hour Forecast</h2>\n"
        ++ "<dl>\n"
        ++ "<dt>Wind</dt>\n"
        ++ "<dd>"
        ++ Forecast.wind (Forecast.current24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Sea State</dt>\n"
        ++ "<dd>"
        ++ Forecast.sea (Forecast.current24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Weather</dt>\n"
        ++ "<dd>"
        ++ Forecast.weather (Forecast.current24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Visibility</dt>\n"
        ++ "<dd>"
        ++ Forecast.visibility (Forecast.current24Hours forecast)
        ++ "</dd>\n"
        ++ "</dl>\n"
        ++ "<h2>Next 24 Hours</h2>\n"
        ++ "<dl>\n"
        ++ "<dt>Wind</dt>\n"
        ++ "<dd>"
        ++ Forecast.wind (Forecast.next24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Sea State</dt>\n"
        ++ "<dd>"
        ++ Forecast.sea (Forecast.next24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Weather</dt>\n"
        ++ "<dd>"
        ++ Forecast.weather (Forecast.next24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Visibility</dt>\n"
        ++ "<dd>"
        ++ Forecast.visibility (Forecast.next24Hours forecast)
        ++ "</dd>\n"
        ++ "</dl>\n"
        ++ "</body>\n"
        ++ "</html>\n"
