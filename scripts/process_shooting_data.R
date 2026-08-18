#Get the ODP school data and add it to the dashboard
#Sean Mason added on to a file that was most likely created by Tyler Tran initially


library(dplyr); library(sf); library(tidycensus); library(readr)



###############################################################
# Helper: safely read a geojson/csv from a URL, with logging on failure
# This keeps one bad/slow source from silently killing the whole run
# without at least telling us which source it was
safe_read <- function(url, reader = st_read, label = url, ...) {
  tryCatch({
    message("Reading: ", label)
    reader(url, ...)
  }, error = function(e) {
    stop("Failed to read '", label, "': ", conditionMessage(e), call. = FALSE)
  })
}

###############################################################
# This code chunk pulls ACS Census data

census_api_key(Sys.getenv("CENSUS_API_KEY"), overwrite = TRUE)

# Variables needed --------------------------------------------------------
vars <- c(
  # Poverty (block-group-safe table)
  poverty_denom   = "C17002_001",
  poverty_below50 = "C17002_002",
  poverty_below99 = "C17002_003",
  
  # Unemployment
  laborforce      = "B23025_003",
  unemployed      = "B23025_005",
  
  # Total population
  total_pop       = "B03002_001",
  
  # Race/ethnicity (non-Hispanic by race, + Hispanic any race)
  white_nh        = "B03002_003",
  black_nh        = "B03002_004",
  asian_nh        = "B03002_006",
  hispanic        = "B03002_012",
  
  # Age brackets - under 18 (male + female, 0-17)
  m_u5   = "B01001_003", m_5_9  = "B01001_004", m_10_14 = "B01001_005", m_15_17 = "B01001_006",
  f_u5   = "B01001_027", f_5_9  = "B01001_028", f_10_14 = "B01001_029", f_15_17 = "B01001_030",
  
  # Age brackets - 18 to 34
  m_18_19 = "B01001_007", m_20 = "B01001_008", m_21 = "B01001_009", m_22_24 = "B01001_010", m_25_29 = "B01001_011", m_30_34 = "B01001_012",
  f_18_19 = "B01001_031", f_20 = "B01001_032", f_21 = "B01001_033", f_22_24 = "B01001_034", f_25_29 = "B01001_035", f_30_34 = "B01001_036"
)


# Function to try pulling a given year --------------------------------------
# We have ACS 2024 data saved as a geojson; this code checks to see whether
# newer ACS data has been released; if so, it pulls the newer data. If not,
# it falls back to the 2024 data that we've saved
try_get_acs <- function(year) {
  tryCatch({
    message("Trying ACS 5-year data for year: ", year)
    get_acs(
      geography = "block group",
      state     = "PA",
      county    = "Philadelphia",
      variables = vars,
      year      = year,
      survey    = "acs5",
      output    = "wide",
      geometry  = TRUE
    )
  }, error = function(e) {
    message("  -> Failed for ", year, ": ", conditionMessage(e))
    NULL
  })
}

# Check current year and a couple years ahead, newest first -----------------
current_year <- as.integer(format(Sys.Date(), "%Y"))
candidate_years <- seq(current_year, 2024+1, by = -1)  # e.g. if it's 2027: 2027, 2026, 2025

acs_raw <- NULL
used_source <- NULL

for (yr in candidate_years) {
  result <- try_get_acs(yr)
  if (!is.null(result)) {
    acs_raw <- result
    used_source <- paste0("Census API (", yr, ")")
    break
  }
}

##### Save 2024 ACS data; this is a one-time thing
# acs_2024 <- try_get_acs(2024)
# st_write(acs_2024 %>% select(-NAME), 'acs_block_groups_2024_fallback.geojson')

# Fallback to saved geojson if no year worked ------------------------------------------
if (is.null(acs_raw)) {
  message("No newer ACS data available from API — falling back to saved geojson.")
  
  if (!file.exists("scripts/acs_block_groups_2024_fallback.geojson")) {
    stop("ACS API failed AND fallback file 'acs_block_groups_2024_fallback.geojson' ",
         "was not found in the working directory. Check that it's committed to the repo.",
         call. = FALSE)
  }
  
  acs_raw <- st_read("scripts/acs_block_groups_2024_fallback.geojson") %>%
    # make sure the columns read as numeric
    mutate(across(
      .cols = ends_with("E") | ends_with("M"),
      .fns = as.numeric
    ))
  used_source <- "Fallback Geojson (2024)"
}

message("Data source used: ", used_source)

# Calculate rates/percentages ------------------------------------------------
acs_data <- acs_raw %>%
  mutate(
    poverty_rate = (poverty_below50E + poverty_below99E) / poverty_denomE,
    unemployment_rate = unemployedE / laborforceE,
    pct_black_nh      = black_nhE / total_popE,
    pct_white_nh      = white_nhE / total_popE,
    pct_asian_nh      = asian_nhE / total_popE,
    pct_hispanic      = hispanicE / total_popE,
    pop_under_18      = m_u5E + m_5_9E + m_10_14E + m_15_17E + f_u5E + f_5_9E + f_10_14E + f_15_17E,
    pct_under_18      = pop_under_18 / total_popE,
    pop_18_34         = m_18_19E + m_20E + m_21E + m_22_24E + m_25_29E + m_30_34E +
      f_18_19E + f_20E + f_21E + f_22_24E + f_25_29E + f_30_34E,
    pct_18_34         = pop_18_34 / total_popE
  ) %>%
  select(geoid = GEOID, total_population = total_popE, 
         poverty_rate, unemployment_rate,
         pct_black_nh, pct_white_nh, pct_asian_nh, pct_hispanic,
         pct_under_18, pct_18_34, geometry) %>%
  rename_with(~ paste0("bg_", .x)) %>%
  st_transform(crs = 4326)


###############################################################
# Pull in all boundary / point layers, with error handling so a
# single failed source produces a clear message instead of a cryptic
# downstream error

neighborhoods <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/philly_analyses/refs/heads/master/neighborhoods.geojson',
  label = "neighborhoods"
)

zips <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/iv-philly/refs/heads/main/Zipcodes_Poly.geojson',
  label = "zips"
) %>%
  select(zip_code = CODE)

council_districts <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/iv/refs/heads/main/Council_Districts_2024.geojson',
  label = "council_districts"
) %>%
  select(council_district = DISTRICT)

schools <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/philly_analyses/refs/heads/master/SchoolDist_Catchments_ES.geojson',
  label = "schools"
) %>%
  select(school_catchment = ES_NAME)

school_locations <- safe_read(
  'https://raw.githubusercontent.com/seanfmason/raw_data_files/refs/heads/main/opendataphilly/Schools.geojson',
  label = "school_locations"
)

city_limits <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/iv/refs/heads/main/City_Limits.geojson',
  label = "city_limits"
) %>%
  st_transform(crs = 3857)

crashes <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/iv/refs/heads/main/crashes_2018_2022.geojson',
  label = "crashes"
)

parks_rec <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/iv/refs/heads/main/PPR_Properties.geojson',
  label = "parks_rec"
) %>%
  select(park_name = public_name)

phl_grid_2_blocks <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/iv/refs/heads/main/phl_grid_2_blocks.geojson',
  label = "phl_grid_2_blocks"
) %>%
  select(id_2cell)

phl_grid_5_blocks <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/iv/refs/heads/main/phl_grid_5_blocks.geojson',
  label = "phl_grid_5_blocks"
) %>%
  select(id_5cell)

phl_grid_10_blocks <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/iv/refs/heads/main/phl_grid_10_blocks.geojson',
  label = "phl_grid_10_blocks"
) %>%
  select(id_10cell)

shooting_victimsR <- safe_read(
  'https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings&filename=shootings&format=geojson&skipfields=cartodb_id',
  label = "shooting_victimsR (Carto SQL)"
) %>%
  rename(lat = point_y, lng = point_x) %>%
  st_join(zips) %>%
  st_join(neighborhoods) %>%
  st_join(council_districts) %>%
  st_join(schools) %>%
  st_join(phl_grid_2_blocks) %>%
  st_join(phl_grid_5_blocks) %>%
  st_join(phl_grid_10_blocks) %>%
  st_join(acs_data) %>%
  group_by(dc_key) %>%
  mutate(`# of Victims per DC` = n()) %>%
  ungroup() %>%
  mutate(date_controller = as.Date(date_),
         `# of Victims per DC - Category` = ifelse(`# of Victims per DC` == 1, 'Single', 'Multiple')) 


cells2 <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/philly_analyses/refs/heads/master/cells2.csv',
  reader = read_csv,
  label = "cells2"
)

cells5 <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/philly_analyses/refs/heads/master/cells5.csv',
  reader = read_csv,
  label = "cells5"
)

cells10 <- safe_read(
  'https://raw.githubusercontent.com/tylerjtran/philly_analyses/refs/heads/master/cells10.csv',
  reader = read_csv,
  label = "cells10"
)

#Get the nearest school to each shooting victim-------------------------------
#Project geometry (convert degrees to meters, which is location specific)
school_locations <- st_transform(school_locations, 3364)
shooting_victimsR <- st_transform(shooting_victimsR, 3364)

# 1. Find the nearest school for each shooting
nearest_idx <- st_nearest_feature(shooting_victimsR, school_locations)

# 2. Compute the distance to that school (meters)
nearest_dist <- st_distance(
  shooting_victimsR,
  school_locations[nearest_idx, ],
  by_element = TRUE
)

# 3. Add distance and school name as new columns
shooting_victimsR <- shooting_victimsR %>%
  mutate(
    nearest_school_dist_m = as.numeric(nearest_dist),
    nearest_school_name = school_locations$school_name[nearest_idx],
    within_two_blocks = nearest_school_dist_m <= 160.9 #one tenth of a mile
  ) %>%
  
  #Then drop geometry
  st_drop_geometry()


write_csv(shooting_victimsR, 'shooting_victims_processed.csv')

