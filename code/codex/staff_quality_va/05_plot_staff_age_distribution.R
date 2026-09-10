# Read-only source diagnostic; writes only the requested aggregate figure.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
root <- normalizePath(file.path(dirname(script), "../../.."), winslash = "/")
setDTthreads(4L)
staff <- readRDS(file.path(root, "data/clean/staff_quality_va/staff_person_school_year_features.rds"))
leaders <- readRDS(file.path(root, "data/clean/leadership_quality_va/leadership_person_school_year.rds"))
groups <- rbindlist(list(
  unique(staff[AGNO == 2024 & TEACHER_HS_ANY == 1L, .(MRUN, RBD)])[, ROLE := "HS teachers"],
  unique(staff[AGNO == 2024 & ORIENTADOR_ANY == 1L, .(MRUN, RBD)])[, ROLE := "Orientadores"],
  unique(leaders[AGNO == 2024, .(MRUN, RBD)])[, ROLE := "Leadership"]
))
manifest <- fread(file.path(root, "data/clean/leadership_quality_va/leadership_source_manifest.csv"))
path <- manifest[AGNO == 2024, SOURCE_PATH]
stopifnot(length(path) == 1, file.info(path)$size == manifest[AGNO == 2024, SOURCE_BYTES])
# Source is ~99 MB; read only three columns, then immediately restrict to cohort.
birth <- fread(path, sep = ";", select = c("MRUN", "RBD", "DOC_FEC_NAC"), colClasses = "character")
birth[, RBD := as.integer(RBD)]
birth <- unique(birth[MRUN %chin% groups$MRUN])
matched <- merge(groups, birth, by = c("MRUN", "RBD"), all.x = TRUE, allow.cartesian = TRUE)
matched[, `:=`(YOB = suppressWarnings(as.integer(substr(DOC_FEC_NAC, 1, 4))),
  MONTH = suppressWarnings(as.integer(substr(DOC_FEC_NAC, 5, 6))))]
matched[, VALID := !is.na(DOC_FEC_NAC) & grepl("^[0-9]{6}$", DOC_FEC_NAC) &
  DOC_FEC_NAC != "190001" & MONTH %in% 1:12 & YOB >= 1924L & YOB <= 2006L]
matched[, AGE := fifelse(VALID, 2024L - YOB, NA_integer_)]
# A person working at several schools counts once per role. Ambiguous birth
# reports are missing, never an arbitrary first appointment or mean birthdate.
person <- matched[, .(N_DATES = uniqueN(DOC_FEC_NAC[VALID]),
  AGE = if (uniqueN(DOC_FEC_NAC[VALID]) == 1L) AGE[VALID][1] else NA_integer_), by = .(ROLE, MRUN)]
summary <- person[, .(N_TOTAL = .N, N_VALID = sum(!is.na(AGE)), N_MISSING = sum(is.na(AGE)),
  N_CONFLICT = sum(N_DATES > 1L), MEAN = mean(AGE, na.rm = TRUE), MEDIAN = median(AGE, na.rm = TRUE),
  P25 = quantile(AGE, .25, na.rm = TRUE), P75 = quantile(AGE, .75, na.rm = TRUE)), by = ROLE]
stopifnot(nrow(person) == uniqueN(groups, by = c("ROLE", "MRUN")))
print(summary)
valid <- person[!is.na(AGE)]
stopifnot(all(valid$AGE >= 18 & valid$AGE < 100))
order <- c("HS teachers", "Orientadores", "Leadership")
valid[, ROLE := factor(ROLE, levels = order)]
valid[, WEIGHT := 100 / .N, by = ROLE]
summary[, ROLE := factor(ROLE, levels = order)]
labels <- setNames(sprintf("%s\nn = %s; median = %.0f", summary$ROLE,
  format(summary$N_VALID, big.mark = ",", trim = TRUE), summary$MEDIAN), as.character(summary$ROLE))
plot <- ggplot(valid, aes(AGE, weight = WEIGHT, fill = ROLE)) +
  geom_histogram(breaks = seq(15, 100, 5), closed = "left", color = "white", linewidth = .35) +
  geom_vline(data = summary, aes(xintercept = MEDIAN),
    linetype = "dashed", color = "#263b49", linewidth = .6) +
  facet_wrap(~ROLE, nrow = 1, labeller = as_labeller(labels)) +
  scale_fill_manual(values = c("HS teachers" = "#3578a3", "Orientadores" = "#318d80", "Leadership" = "#b27745"), guide = "none") +
  scale_x_continuous(breaks = seq(20, 100, 10)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, .08))) +
  labs(title = "Age distribution by staff role", subtitle = "2024 staff at schools in the VA sample | Five-year age bins",
    x = "Age attained during 2024", y = "Share of people within role",
    caption = sprintf("Source: MINEDUC DOC_FEC_NAC (birth year/month). Dashed line: median.\nEach person counted once per role; roles may overlap. Missing/conflicting ages excluded: teachers %d, orientadores %d, leadership %d.",
      summary[ROLE == "HS teachers", N_MISSING], summary[ROLE == "Orientadores", N_MISSING], summary[ROLE == "Leadership", N_MISSING])) +
  theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 13, lineheight = 1.15),
    plot.title = element_text(face = "bold", size = 21), plot.subtitle = element_text(color = "#556674", margin = margin(b = 16)),
    plot.caption = element_text(hjust = 0, color = "#556674", size = 10, lineheight = 1.2, margin = margin(t = 15)),
    panel.spacing = grid::unit(1.3, "lines"), plot.margin = margin(18, 18, 16, 15))
out <- file.path(root, "output/figures/staff_quality_va/staff_age_distribution_2024.png")
ggsave(out, plot, width = 12, height = 5.5, dpi = 180, bg = "white")
message("Saved ", out)
