# Extend the existing three peer populations without changing their definitions.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
base <- "data/clean/applicant_composition_trends"
dest <- file.path(base, "all_school_group_averages")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
ranking_path <- "data/clean/school_preference_va/student_school_rankings.rds"
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
prob_paths <- file.path(data_root, "data/clean/DA_probs", paste0("DA_probs_", 2018:2020, ".csv"))
original_path <- file.path(base, "group_averages/school_group_averages_2018_2020.csv")
protected <- c(ranking_path, original_path)
manifest <- c(protected, prob_paths)
fwrite(data.table(path = manifest, bytes = file.info(manifest)$size, modified = as.character(file.info(manifest)$mtime)),
  file.path(dest, "ranking_source_manifest.csv"))
hashes <- data.table(path = protected, md5_before = unname(tools::md5sum(protected)))
checks <- data.table(check = character(), passed = logical())
check <- function(label, ok) {
  checks <<- rbind(checks, data.table(check = label, passed = isTRUE(ok)))
  if (!isTRUE(ok)) stop(label)
}
message("Reusing full genuine submitted-school rankings.")
listed <- as.data.table(readRDS(ranking_path))[, .(mrun, sae_proceso, cohort_gr8, rbd)]
check("Full paper applicant roster and genuine school pairs", nrow(listed) == 1007253L && uniqueN(listed$mrun) == 294198L &&
  uniqueN(listed$rbd) == 2524L && !anyDuplicated(listed[, .(mrun, sae_proceso, rbd)]) && all(listed$sae_proceso == listed$cohort_gr8))
audits <- list()
weights <- rbindlist(lapply(seq_along(prob_paths), function(j) {
  year <- (2018:2020)[j]
  ids <- unique(listed[sae_proceso == year, mrun])
  p <- fread(prob_paths[j], select = c("student_id", "school_id", "prob"), showProgress = FALSE)[student_id %in% ids]
  check(paste(year, "program probabilities are valid and unique"), !anyNA(p$prob) && all(p$prob >= 0 & p$prob <= 1) &&
    !anyDuplicated(p[, .(student_id, school_id)]))
  p[, unmatched := tolower(trimws(school_id)) %chin% c("unmatched", "-99")]
  p[, rbd := suppressWarnings(as.integer(sub("_.*$", "", school_id)))]
  check(paste(year, "matched probability destinations parse"), p[unmatched == FALSE, all(!is.na(rbd) & rbd > 0)])
  mass <- p[, .(mass = sum(prob)), by = student_id]
  check(paste(year, "complete probability distribution for every applicant"), setequal(mass$student_id, ids) && max(abs(mass$mass - 1)) < 1e-6)
  p <- p[unmatched == FALSE, .(admission_probability = sum(prob)), by = .(student_id, rbd)]
  z <- merge(listed[sae_proceso == year], p, by.x = c("mrun", "rbd"), by.y = c("student_id", "rbd"), all.x = TRUE)
  absent <- sum(is.na(z$admission_probability))
  z[is.na(admission_probability), admission_probability := 0]
  check(paste(year, "school probabilities preserve unique genuine lists"), nrow(z) == listed[sae_proceso == year, .N] &&
    all(z$admission_probability >= 0 & z$admission_probability <= 1 + 1e-12))
  audits[[j]] <<- data.table(year, applicants = length(ids), genuine_school_pairs = nrow(z), absent_probability_rows = absent,
    positive_probability_pairs = sum(z$admission_probability > 0), max_mass_error = max(abs(mass$mass - 1)))
  z
}))
old_weights <- as.data.table(readRDS(file.path(base, "probability_weighted/applicant_school_frame.rds")))[,
  .(mrun, cohort_gr8, rbd, old_probability = admission_probability)]
wcheck <- merge(old_weights, weights, by = c("mrun", "cohort_gr8", "rbd"))
check("All original top-100 probabilities reproduce", nrow(wcheck) == 212608L && max(abs(wcheck$old_probability - wcheck$admission_probability)) < 1e-12)
saveRDS(weights, file.path(dest, "applicant_school_weights.rds"))
fwrite(rbindlist(audits), file.path(dest, "probability_linkage_audit.csv"))
# Reuse the original aggregation implementation, with a broader school roster.
# This roster includes all submitted destinations and positive most-attended or offered RBDs.
options(school_composition.all_schools = TRUE)
source("code/codex/applicant_composition_trends/09_school_group_averages.R")
options(school_composition.all_schools = NULL)
original <- fread(original_path)
expanded <- fread(file.path(dest, "school_group_averages_2018_2020.csv"))
check("Expanded school-group joins are unique", !anyDuplicated(expanded[, .(rbd, group)]))
check("All submitted destination schools are represented", all(listed$rbd %in% expanded$rbd))
comparison <- merge(original, expanded, by = c("rbd", "group"), suffixes = c("_old", "_new"))
check("All 300 original school-group rows remain", nrow(comparison) == 300L)
numeric_fields <- names(original)[vapply(original, is.numeric, logical(1))]
numeric_fields <- setdiff(numeric_fields, c("rank", "rbd"))
for (v in numeric_fields) check(paste("Original top-100 field reproduces:", v),
  isTRUE(all.equal(comparison[[paste0(v, "_old")]], comparison[[paste0(v, "_new")]], tolerance = 1e-10)))
hashes[, md5_after := unname(tools::md5sum(path))]
check("Original rankings and supplied top-100 peer file unchanged", all(hashes$md5_before == hashes$md5_after))
fwrite(hashes, file.path(dest, "source_preservation.csv"))
fwrite(checks, file.path(dest, "verification.csv"))
message("Extended to ", uniqueN(expanded$rbd), " schools; original 100-school measures reproduce.")
