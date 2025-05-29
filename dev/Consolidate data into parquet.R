# Goal: Consolidate all local fusion output into a small number of parquet files for dissemination

library(tidyverse)
library(fusionModel)
library(data.table)
library(collapse)
library(arrow)

arrow::set_cpu_count(fusionACS:::get_cores())

# Check if LZ4 codec is available
stopifnot(codec_is_available("LZ4"))

#--------------

# General crosswalk for geographic variables identified by state
state.merge <- readRDS("~/Documents/Projects/fusionData/geo-raw/miscellaneous/Geographic entities to merge on state.rds")

# # GeoCorr raw data
# gn <- names(fread("~/Documents/Projects/fusionData/geo-raw/concordance/geocorr2018_2116808121.csv.zip", nrows = 2))
# g <- fread("~/Documents/Projects/fusionData/geo-raw/concordance/geocorr2018_2116808121.csv.zip", skip = 1, na.strings = c("NA", "")) %>%
#   setnames(gn)

# GeoCorr processed data
geo <- fst::read_fst("~/Documents/Projects/fusionData/geo-processed/concordance/geo_concordance.fst", as.data.table = TRUE)
geo[, c("state", "county10", "tract10") := lapply(.SD, as.integer), .SDcols = c("state", "county10", "tract10")]

#--------------

# Function that uses UrbanPop to create a plausible sampling of ACS-PUMS households such that all census tracts are represented
sample_urbanpop <- function(fpath, sample_prop = 0.05) {

  # Load state-specific UrbanPop data from disk
  d <- fst::read_fst(fpath, as.data.table = TRUE)

  # Use only base implicate
  d <- d[rep == 0, ]

  # Restrict to state-county-tract combinations found in the 'geo' object
  keep <- d[, .(state, county10, tract10)] %iin% geo[, .(state, county10, tract10)]
  d <- d[keep, ]

  # Retain the 'state' integer
  state <- d$state[[1]]

  # Aggregate weight at census tract level (to remove block group resolution)
  d <- d[, .(weight = sum(weight)), by = .(year, hid, county10, tract10)]

  # For checking final results; no other use
  #d0 <- copy(d)

  # Total weight per tract, across all years
  d[, W := sum(weight), by = .(county10, tract10)]

  # Unique year-hid combination as large (64-bit) integer
  d[, ID := bit64::as.integer64(year) * 2^32 + bit64::as.integer64(hid)]

  # Total number of unique households in original data
  n.id <- uniqueN(d$ID)

  # Within each tract, calculate the maximum possible target number of households
  # This is 5% of the total number of unique households in the tract (across all years)
  # This would be the true target amount if each household was assigned to a single tract (not true)
  d[, N := ceiling(uniqueN(ID) * sample_prop), by = .(county10, tract10)]
  d[, N0 := N]

  # Intermediary results
  out <- data.table()

  # Loop here with progress bar
  cat("Processing state FIPS code", state, "\n")
  n0 <- nrow(d)
  pb <- txtProgressBar(min = 0, max = n0, style = 3)
  while(nrow(d) > 0) {

    # Randomize row order proportional to 'weight'
    # This re-randomizes each iteration
    d[, rand := -log(runif(.N)) / weight]

    # Order by number of additional households required by tract (N)
    setorder(d, N, rand)

    # Select the first N households within each tract
    x <- d[, .SD[1:min(N, .N)], by = .(county10, tract10)]

    # For each tract, retains the entries up to the first duplication of year-hid
    x[, ok := !duplicated(ID)]
    x <- x[, .SD[if (ok[1]) 1:(min(which(!ok)[1], .N + 1, na.rm = TRUE) - 1) else 0], by = .(county10, tract10)]

    # Safety checks
    stopifnot(all(x$ok))
    stopifnot(nrow(x) == uniqueN(x$ID))

    # Append current iteration result
    out <- rbind(out, x)

    # Remove assigned households from 'd'
    d <- d[!ID %in% x$ID, ]

    # Update 'N' to reflect remaining number of households to assign
    # And remove tracts from 'd' that already have 'N0' households assigned in 'out' (i.e. nothing more to add)
    temp <- out[, .(N_out = uniqueN(ID)), by = .(county10, tract10)]
    d <- temp[d, on = .(county10, tract10)]
    d <- d[, N := N0 - pmax(0, N_out, na.rm = TRUE)]
    d <- d[N > 0]
    d$N_out <- NULL

    # Update progress bar
    setTxtProgressBar(pb, value = n0 - nrow(d))

  }

  close(pb)

  #---

  # Explore 'out'
  # uniqueN(d0, by = c("county10", "tract10"))
  # uniqueN(out, by = c("county10", "tract10"))
  #
  # uniqueN(d0, by = c("year", "county10"))
  # uniqueN(out, by = c("year", "county10"))
  #
  # uniqueN(d0, by = c("year", "hid"))
  # uniqueN(out, by = c("year", "hid"))
  # nrow(out)

  #---

  # Retain rows from 'out' that provide the desired overall sampling

  # Maximum possible sampling rate
  max.samp <- nrow(out) / n.id

  # Sampling adjustment factor to sampling from observations in 'out'
  adj.samp <- sample_prop / max.samp

  # result <- out %>%
  #   group_by(county10, tract10) %>%
  #   mutate(target = ceiling(adj.samp * n())) %>%
  #   group_modify(~ slice_sample(.x, n = .x$target[1])) %>%
  #   ungroup() %>%
  #   select(year, hid, state, county10, tract10, weight)

  # Equivalent to dplyr code above but much faster
  result <- out[, .SD[sample(.N, min(ceiling(adj.samp * .N), .N))], by = .(county10, tract10)]
  result$state <- state
  result <- result[, .(year, hid, state, county10, tract10)]  # 'weight' not included, but could be.
  setorder(result, state, county10, tract10)

  #---

  # Visual check of the year-county weights using full sample and 'result'
  # test1 <- d0[, .(weight = sum(weight)), by = .(year, county10)] %>% mutate(weight1 = weight / sum(weight))
  # test2 <- result[, .(weight = sum(weight)), by = .(year, county10)] %>% mutate(weight2 = weight / sum(weight))
  # check <- inner_join(test1, test2, by = join_by(year, county10))
  # plot(check$weight1, check$weight2)x %iin% table
  # abline(0, 1, col = 2)
  # cor(check$weight1, check$weight2)

  # Visual check of the tract weights using full sample and 'result'
  # These aren't as good as the year-county results above
  # test1 <- d0[, .(weight = sum(weight)), by = .(county10, tract10)] %>% mutate(weight1 = weight / sum(weight))
  # test2 <- result[, .(weight = sum(weight)), by = .(county10, tract10)] %>% mutate(weight2 = weight / sum(weight))
  # check <- inner_join(test1, test2, by = join_by(county10, tract10))
  # plot(check$weight1, check$weight2)
  # abline(0, 1, col = 2)
  # cor(check$weight1, check$weight2)

  #---

  # Return result
  return(result)

}

#---

# UrbanPop state-specific file paths
flist <- list.files("~/Documents/Projects/fusionData/urbanpop/v3",
                    pattern = "00.fst$",
                    recursive = TRUE,
                    full.names = TRUE)

# Get sample for each state
upop <- flist %>%
  lapply(sample_urbanpop, sample_prop = 0.05) %>%
  rbindlist()

# TO DO: Integrate these results into the ACS sampling, below
# Add county and tract data to ACS microdata (as integer?)
# Create a geo crosswalk file and add to pseudo-sample files that allows any other geography to be merged on tract

#----------------------------
#----------------------------

# Create geo-crosswalk data table

# Restrict 'g' to tracts in 'upop' result
keep <- geo[, .(state, county10, tract10)] %iin% upop[, .(state, county10, tract10)]
geo <- geo[keep, ]

# Sum weight by group (pretty minimal effect)
geo <- geo[, .(weight = sum(puma_weight)), by = .(state, county10, tract10, state_postal, state_name, puma10, cbsa10, csa10, cd116, cousubfp10, zcta10)]

# Sample for each state-county-tract combination
sampled <- geo[, .SD[sample(.N, 1, prob = weight)], by = .(state, county10, tract10)]
sampled[, weight := NULL]

# This should match
dim(sampled)
uniqueN(upop[, .(state, county10, tract10)])

# Rename for use below
geoxwalk <- sampled

#----------------------------
#----------------------------

# PROCESS ACS DATA
acs.flist <- list.files(path = "~/Documents/Projects/fusionData/survey-processed/ACS",
                        pattern = "201[5-9]_P_processed\\.fst$",
                        recursive = TRUE,
                        full.names = TRUE)

acs <- lapply(acs.flist, function(x) {

  # Load person data and restrict to households in 'upop' sample
  v <- fst::fst.metadata(x)$columnNames
  v <- grep("rep_\\d+$", v, value = TRUE, invert = TRUE)  # Remove replicate weights
  p <- fst::read_fst(x, columns = v, as.data.table = TRUE) %>%
    select(-state) %>%  # Remove default state FIPS factor variable in PUMS in favor of integer used for UrbanPop data
    merge(upop %>% select(year, hid), by = c('year', 'hid'))

  # Load household data and restrict to households in 'upop' sample
  x <- sub("P_processed.fst$", "H_processed.fst", x)
  v <- fst::fst.metadata(x)$columnNames
  v <- grep("rep_\\d+$", v, value = TRUE, invert = TRUE)  # Remove replicate weights
  h <- fst::read_fst(x, columns = v, as.data.table = TRUE) %>%
    select(-state) %>%  # Remove default state FIPS factor variable in PUMS in favor of integer used for UrbanPop data
    merge(upop , by = c('year', 'hid')) %>%
    merge(geoxwalk, by = c('state', 'county10', 'tract10', 'puma10'))  # Adds geo crosswalk variables to data directly; not best for file size, but easiest for now

  # Note that there is a difference due to inclusion of group quarters in person data (excluded from household data)
  # uniqueN(select(h, year, hid))
  # uniqueN(select(p, year, hid))

  list(h, p)

}) %>%
  transpose() %>%
  map(rbindlist, use.names = TRUE, fill = TRUE, ignore.attr = TRUE)

# NOTE: Warnings here are expected; they indicate variance in levels across years for identically-named variables
# The rbindlist() arguments ensure that affected factor levels are enlarged (and possibly unordered) as necessary to allow row binding

# Identify the data as person or household
names(acs) <- ifelse(sapply(acs, function(x) "pid" %in% names(x)), "P", "H")

# test save to check file size
write_parquet(acs$H, sink = "fusionACS_data/ACS_H.parquet", compression = "LZ4")
write_parquet(acs$P, sink = "fusionACS_data/ACS_P.parquet", compression = "LZ4")

rm(acs)
gc()

#-------------

# Paths to donor survey fusion results
dir_in <- "~/Documents/Projects/fusionDaget_directory()ta/fusion/RECS/2020"

flist <- list.files(path = dir_in,
                    pattern = "_fused\\.fsd$",
                    recursive = TRUE,
                    full.names = TRUE)

# Split the 'flist' into "H" and "P" file paths
flist <- split(flist, str_extract(flist, "_[HP]_"))
names(flist) <- str_remove_all(names(flist), "_")

# PROCESS FUSION OUTPUT
# Load the sampled year-hid combinations from processed ACS person data already saved to disk
s <- unique(read_parquet(file = "fusionACS_data/ACS_P.parquet", col_select = c('year', 'hid')))
getData <- function(flist) {
  test <- lapply(flist, function(x) {
    i <- fusionModel::read_fsd(x, columns = names(s), M = 1) %iin% s
    h <- fusionModel::read_fsd(x, M = 1, df = s) %>%
      select(-M)  # Drop implicates column
    if ('pid' %in% fst::fst.metadata(x)$columnNames) {
      arrange(h, year, hid, pid)
    } else {
      arrange(h, year, hid)
    }
  }) %>%
    rbindlist(use.names = TRUE, fill = TRUE, ignore.attr = TRUE)
}

h <- getData(flist$H)
p <- getData(flist$P)

# Save to disk
fpath <- file.path("fusionACS_data", paste(str_split_i(dir_in, "/", i = -2), str_split_i(dir_in, "/", i = -1), sep = "_"))
if (length(h)) write_parquet(h, sink = paste0(fpath, "_H.parquet"), compression = "LZ4")
if (length(p)) write_parquet(p, sink = paste0(fpath, "_P.parquet"), compression = "LZ4")

rm(h, p)
gc()

#-------


# Construct variable metadata for the non-universal variables in 'd'
cat("Constructing variable metadata\n")

data(dictionary, package = "fusionData")
uvar <- c('year', 'hid', 'pid', 'weight')

getMetadata <- function(f) {

  d <- read_parquet(f)
  acs <- startsWith(basename(f), "ACS_")
  hh <- str_sub(basename(f), -9, -9) == "H"

  out <- lapply(setdiff(names(d), uvar), function(v) {

    # Summary of values
    x <- d[[v]]
    if (is.character(x)) x <- factor(x)  # Catch inadvertent case when 'x' is character (e.g. puma10)
    m <- if (is.numeric(x)) {
      paste(
        c("Min:", "1st Quartile:", "Median:", "Mean:", "3rd Quartile:", "Max:"),
        summary(x, digits = 4),
        collapse = ", ")
    } else {
      if (is.logical(x)) list(c(TRUE, FALSE)) else list(levels(x))  # factor(x) only to
    }

    # Number of unique values, excluding NA values
    #u <- uniqueN(x, na.rm = TRUE)

    # Data type
    p <- switch(vctrs::vec_ptype_abbr(x),
                fct = 'categorical (unordered)',
                ord = 'categorical (ordered)',
                int = 'integer',
                dbl = 'double',
                lgl = 'logical')

    # Years for which there is complete data (no NA values)
    y <- collapse::fsum(is.na(x), g = d$year)
    y <- list(as.integer(names(y)[y == 0]))

    # Output
    out <- tibble(variable = v,
                  survey = ifelse(acs, "ACS", str_sub(basename(f), 1, -11)),
                  respondent = ifelse(hh, "household", "person"),
                  years = y,
                  type = p,
                  #n_unique = u,
                  values = m,
                  file = basename(f))

    # Add the variable description/label
    desc <- dictionary %>%
      filter(Survey == if (acs) "ACS" else str_split_i(out$survey, "_", 1),
             Vintage %in% if (acs) unlist(y) else str_split_i(out$survey, "_", 2),
             Respondent == ifelse(hh, "Household", "Person"),
             Variable == v)
    out$description <- if (nrow(desc) > 0) {
      desc %>%
        slice(1) %>%
        pull(Description)
    } else {
      NA  # TEMPORARY. Only here to handle undefined variables (e.g. county10)
    }
    return(out)
  }) %>%
    rbindlist() %>%
    select(variable, description, everything())

}

# Construct complete metadata
flist <- list.files("fusionACS_data", pattern = "[HP].parquet$", full.names = TRUE)

meta <- flist %>%
  lapply(getMetadata) %>%
  rbindlist() %>%
  arrange(survey, respondent, variable)

# Save to disk
write_parquet(meta, sink = "fusionACS_data/dictionary.parquet", compression = "LZ4")
