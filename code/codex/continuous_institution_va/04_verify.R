# Independent output and sample checks; avoids loading the 1.54 GB fitted model.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean <- file.path(repo, "data/clean/continuous_institution_va")
d <- as.data.table(readRDS(file.path(clean, "analysis_data.rds")))
ids <- fread(file.path(clean, "institution_fe_enrolled_m1_estimation_ids.csv"))
stopifnot(!anyDuplicated(ids$MRUN), all(ids$MRUN %in% d$MRUN))
u <- d[match(ids$MRUN, MRUN)]
stopifnot(all(u$school_rbd == ids$school_rbd), all(u$enrolled),
  all(is.finite(u$institution_fe)), all(u$institution_fe_enrolled_m1 == u$institution_fe),
  d[enrolled == FALSE, all(is.na(institution_fe_enrolled_m1))])
# Exact connected components of the high-school/middle-school mobility graph.
hs <- sort(unique(u$school_rbd)); ms <- sort(unique(u$most_time_RBD_middle))
edges <- unique(u[, .(a = match(school_rbd, hs), b = length(hs) + match(most_time_RBD_middle, ms))])
parents <- seq_len(length(hs) + length(ms)); sizes <- rep(1L, length(parents))
root <- function(a) {while (parents[a] != a) a <- parents[a]; a}
for (i in seq_len(nrow(edges))) {
  a <- root(edges$a[i]); b <- root(edges$b[i])
  if (a != b) {
    if (sizes[a] < sizes[b]) {z <- a; a <- b; b <- z}
    parents[b] <- a; sizes[a] <- sizes[a] + sizes[b]
  }
}
components <- data.table(school_rbd = hs, component_root = vapply(seq_along(hs), root, integer(1)))
components <- merge(components, u[, .(students = .N), by = school_rbd], by = "school_rbd")
component_summary <- components[, .(schools = .N, students = sum(students)), by = component_root][order(-students)]
fwrite(components, file.path(clean, "school_fe_components.csv"))
fwrite(component_summary, file.path(clean, "fe_component_summary.csv"))
print(component_summary)
stopifnot(nrow(component_summary) == 1L)
values_path <- file.path(clean, "school_value_added.csv")
if (!file.exists(values_path)) {
  message("Sample and connected-component checks pass; rerun after EB completion for final checks")
} else {
  v <- fread(values_path)
  counts <- u[, .(n_check = .N), by = school_rbd]
  counts <- merge(counts, v, by = "school_rbd", all = TRUE)
  stopifnot(!anyNA(counts$n_check), all(counts$n_check == counts$n_students_regression),
    all(v$n_students_regression_total == nrow(u)), !anyDuplicated(v$school_rbd))
  w <- v$n_students_regression; raw <- v$controlled_value_added_centered_student
  tau2 <- max(sum(w*(raw-weighted.mean(raw,w))^2)/sum(w) - sum(w*v$controlled_value_added_se^2)/sum(w), 0)
  lambda <- if (tau2 == 0) rep(0,nrow(v)) else tau2/(tau2+v$controlled_value_added_se^2)
  eb <- weighted.mean(raw,w) + lambda*(raw-weighted.mean(raw,w))
  eb <- eb - weighted.mean(eb,w)
  stopifnot(max(abs(v$eb_tau2-tau2)) < 1e-12, max(abs(v$eb_reliability-lambda)) < 1e-12,
    max(abs(v$controlled_value_added_eb_centered_student-eb)) < 1e-12,
    abs(weighted.mean(raw,w)) < 1e-12, all(is.finite(v$controlled_value_added_se)))
  manifest <- fread(file.path(clean, "input_manifest.csv"), na.strings = c("", "NA"))
  now <- file.info(manifest$path)
  stopifnot(all(now$size == manifest$bytes), all(as.character(now$mtime) == as.character(manifest$modified)))
  h <- which(!is.na(manifest$md5) & nzchar(manifest$md5))
  stopifnot(all(unname(tools::md5sum(manifest$path[h])) == manifest$md5[h]))
  fwrite(data.table(check = c("non_enrollees_are_na", "exact_enrollee_fe", "estimation_ids_and_counts",
    "connected_school_middle_school_graph", "independent_eb_reconstruction", "source_integrity"), passed = TRUE),
    file.path(clean, "final_verification.csv"))
  message("All final checks passed")
}
