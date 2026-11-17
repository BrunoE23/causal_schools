# All-grade regular-round SAE counts for the paper's 2018-2020 process years.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
out <- "data/clean/sae_applications_by_grade"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

counts <- list()
audit <- list()
for (year in 2018:2020) {
  folder <- file.path(data_root, "data/raw", year, paste0("SAE_", year))
  path <- list.files(folder, pattern = "^C1_.*[.]csv$", full.names = TRUE)
  stopifnot(length(path) == 1L)
  # Select five fields from each <100 MB source; process one year at a time.
  x <- fread(path, sep = ";", select = c("mrun", "rbd", "cod_nivel", "cod_curso", "agregada_por_continuidad"))
  stopifnot(!anyNA(x$mrun), all(x$mrun > 0L), !anyNA(x$agregada_por_continuidad),
            all(x$agregada_por_continuidad %in% 0:1), !anyNA(x$cod_nivel))
  audit[[as.character(year)]] <- x[, .(source_rows = .N,
    automatically_added_continuity = sum(agregada_por_continuidad == 1L)), by = cod_nivel][,
    `:=`(sae_year = year, path = path)]
  x <- x[agregada_por_continuidad == 0L]
  z <- x[, .(applicants = uniqueN(mrun), application_rows = .N,
    program_applications = uniqueN(.SD)), by = cod_nivel, .SDcols = c("mrun", "rbd", "cod_curso")]
  pairs <- unique(x[, .(cod_nivel, mrun, rbd)])[, .(school_applications = .N), by = cod_nivel]
  z <- merge(z, pairs, by = "cod_nivel")
  stopifnot(all(z$applicants <= z$school_applications),
    all(z$school_applications <= z$program_applications),
    all(z$program_applications <= z$application_rows), sum(z$application_rows) == nrow(x))
  z[, sae_year := year]
  counts[[as.character(year)]] <- z
}
counts <- rbindlist(counts)
setorder(counts, cod_nivel, sae_year)
pooled <- counts[, .(applicant_years = sum(applicants), school_applications = sum(school_applications),
  program_applications = sum(program_applications), application_rows = sum(application_rows)), by = cod_nivel]
fwrite(counts, file.path(out, "counts_by_grade_year.csv"))
fwrite(pooled, file.path(out, "pooled_counts_by_grade.csv"))
fwrite(rbindlist(audit), file.path(out, "source_and_continuity_audit.csv"))
print(dcast(counts, cod_nivel ~ sae_year, value.var = "applicants"))
print(dcast(counts, cod_nivel ~ sae_year, value.var = "school_applications"))
print(pooled)
