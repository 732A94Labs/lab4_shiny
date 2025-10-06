#' API client utilities for country metadata and geometry
#'
#' Functions to query OpenStreetMap-backed services:
#' the **Overpass API** for a list of countries and **Nominatim** for
#' fetching polygon geometry by OSM relation id.
#'
#' @section Etiquette & rate limiting:
#' - Always send a descriptive **User-Agent** including a contact address.
#' - Respect the providers' usage policies and rate limits.
#' - Consider caching results locally to avoid repeated calls during development.
#'
#' @references
#' - Overpass API: https://overpass-api.de/
#' - Nominatim Usage Policy: https://operations.osmfoundation.org/policies/nominatim/
#'
#' @keywords internal
#' @name api_client
NULL

.api_cache <- new.env(parent = emptyenv())
.api_cache$countries <- NULL
.api_cache$geoms <- new.env(parent = emptyenv())

#' Fetch the list of available countries from Overpass
#'
#' Queries the public Overpass API for OSM relations tagged as
#' country-level administrative boundaries (admin\_level = 2) and returns
#' a parsed data frame of the CSV response. The output includes columns like
#' `::type`, `::id`, `type`, `boundary`, `land_area`, `ISO3166-1`, `name:en`,
#' `name`, and `::count` (depending on availability in OSM).
#'
#' @details
#' This function POSTS an Overpass QL script that requests CSV output.
#' The request sets a custom `User-Agent` as recommended by the service
#' maintainers. The response body is parsed with `utils::read.csv()` using
#' `check.names = FALSE` so that header names like `"::id"` are preserved.
#'
#' **Important:** The Overpass API is a community resource. Be mindful of
#' query complexity and frequency. If you run many tests or scripts, add
#' caching and/or point to your own Overpass instance.
#'
#' @param force_refresh Logical. If `TRUE`, bypasses the in-memory cache and
#'   queries Overpass again. Defaults to `FALSE`.
#'
#' @return
#' A `data.frame` where each row represents a country relation (and possibly
#' a `land_area` relation where available). Column types are inferred by
#' `read.csv()` and may vary (e.g., `::id` may be character or integer).
#'
#' @examples
#' \dontrun{
#' countries <- fetch_countries()
#' head(countries)
#' # Example: filter by ISO code column if present
#' subset(countries, `ISO3166-1` %in% c("DE", "FR", "SE"))
#' }
#'
#' @export
#' @importFrom httr2 request req_user_agent req_method req_body_raw req_timeout req_perform resp_body_string
#' @importFrom utils read.csv

fetch_countries <- function(force_refresh = FALSE) {
  if (!force_refresh && !is.null(.api_cache$countries)) {
    return(.api_cache$countries)
  }

  overpass <- "https://overpass-api.de/api/interpreter"
  q <- '
  [out:csv(
  ::type, ::id, type, boundary, land_area, "ISO3166-1", "name:en", "name",
  ::count; true; ","
  )];
  (
  relation["type"="boundary"]["boundary"="administrative"]["admin_level"="2"];
  relation["type"="land_area"]["admin_level"="2"];
  );
  out;
  out count;
  '

  resp <- request(overpass) |>
    req_user_agent("GeoGuessr/1.0 (felun463@student.liu.se)") |>
    req_method("POST") |>
    req_body_raw(charToRaw(q)) |>
    req_timeout(seconds = 60) |>
    req_perform()

  txt <- resp_body_string(resp)

  df <- read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE)

  .api_cache$countries <- df

  return(df)
}

#' Fetch a country's geometry by OSM relation id (via Nominatim)
#'
#' Looks up an OSM relation id in Nominatim (`/lookup`) and returns the
#' polygon geometry as an `sf` object. The function requests GeoJSON, wraps
#' it into a FeatureCollection, writes it to a temporary file, then reads it
#' through **sf** and applies `st_make_valid()` for robustness.
#'
#' @details
#' - `rel_id` should be the numeric OSM **relation** id as found in the country
#'   list (e.g., from [fetch_countries()]).
#' - The request is made with a descriptive `User-Agent`.
#' - The function checks `resp_status(resp) == 200` and stops otherwise.
#' - Because Nominatim returns a compact `geojson` field (type + coordinates),
#'   we construct a minimal FeatureCollection to pass to `sf::st_read()`.
#'
#' @param rel_id Integer or character. An OSM relation id, **without** the
#'   `R` prefix (the function prepends it internally for the `osm_ids` query).
#' @param force_refresh Logical. If `TRUE`, the cached geometry for `rel_id`
#'   (if present) is ignored and a fresh request is made.
#'
#' @return
#' An `sf` object with one feature (the requested relation) containing:
#' - a geometry column (usually `POLYGON`/`MULTIPOLYGON`), and
#' - a property column `osm_id` equal to `rel_id`.
#'
#' @examples
#' \dontrun{
#' # Germany relation id (example; subject to change in OSM):
#' g <- fetch_country_geom_by_relation(51477)
#' plot(sf::st_geometry(g))
#' }
#'
#' @seealso [fetch_countries()]
#'
#' @export
#' @importFrom httr2 request req_user_agent req_url_query req_perform resp_status resp_body_json
#' @importFrom jsonlite toJSON
#' @importFrom sf st_read st_make_valid
fetch_country_geom_by_relation <- function(rel_id, force_refresh = FALSE) {
  cache_key <- as.character(rel_id)
  geom_cache <- .api_cache$geoms

  if (!force_refresh && exists(cache_key, envir = geom_cache, inherits = FALSE)) {
    return(geom_cache[[cache_key]])
  }

  req <- request("https://nominatim.openstreetmap.org/lookup") |>
    req_user_agent("GeoGuessr/1.0 (felun463@student.liu.se)") |>
    req_url_query(
      osm_ids = paste0("R", rel_id),
      format = "json",
      polygon_geojson = 1
    )

  resp <- req_perform(req)
  stopifnot(resp_status(resp) == 200)

  dat <- resp_body_json(resp, simplifyVector = TRUE)

  # geojson comes as a small data.frame with columns type + coordinates
  geom <- list(
    type = dat$geojson$type[1],
    coordinates = dat$geojson$coordinates[[1]]
  )

  # Wrap into a FeatureCollection, then read with sf
  feature <- list(type = "Feature", properties = list(osm_id = rel_id), geometry = geom)
  fc <- list(type = "FeatureCollection", features = list(feature))
  tmp <- tempfile(fileext = ".geojson")
  writeLines(toJSON(fc, auto_unbox = TRUE), tmp)
  result <- st_read(tmp, quiet = TRUE) |> st_make_valid()

  assign(cache_key, result, envir = geom_cache)

  result
}
