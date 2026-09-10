suppressPackageStartupMessages({library(data.table); library(ggplot2)})
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
root <- normalizePath(file.path(dirname(script), "../../.."), winslash = "/")
input <- file.path(root, "data/clean/staff_quality_va")
out <- file.path(root, "output/figures/staff_quality_va")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
results <- fread(file.path(input, "staff_va_correlations.csv"))
outcomes <- c("z_year_math_max", "z_year_leng_max", "admission_exam_taker", "higher_ed_enrolled_m1",
              "stem_enrollment_m1", "high_paying_field_m1", "high_inst_m1", "log_program_income_full_clp_m1")
outcome_labels <- c("Math", "Language", "Exam\ntaking", "Higher-ed\nenrollment", "STEM", "High-pay\nfield", "High-pay\ninstitution", "Projected\nincome")
common <- c("balanced_index", "career_index", "credentials_index", "pca_index", "prior_role_years",
            "school_role_spell_years", "exclusive_history_share", "primary_role_share", "dedicated_primary_share",
            "prior_main_role_change_rate", "university_share")
common_labels <- c("Balanced index", "Experience block", "Credentials block", "PCA index", "Prior years in role",
                   "Role-at-school spell", "Always in role to date", "Primary-function share", "Dedicated primary share",
                   "Prior main-role switching", "University qualification")
for (role in c("counselor", "teacher")) {
  extra <- if (role == "counselor") c("teaching_title_share", "orientation_mention_evidence_share", "role_presence_share", "log_va_students_per_staff") else
    c("hs_teaching_title_share", "math_subject_match", "language_subject_match", "role_presence_share")
  extra_labels <- if (role == "counselor") c("Teaching qualification", "Recorded orientation mention", "Years with counselor present", "Log VA students / counselor") else
    c("HS teaching qualification", "Math specialty match", "Language specialty match", "Years with HS teachers present")
  d <- results[ROLE == role & METRIC %chin% c(common, extra) & OUTCOME %chin% outcomes]
  d[, ROW := factor(METRIC, levels = rev(c(common, extra)), labels = rev(c(common_labels, extra_labels)))]
  d[, COL := factor(OUTCOME, levels = outcomes, labels = outcome_labels)]
  stopifnot(nrow(d) == 15L*8L, all(d$STATUS == "estimated"))
  p <- ggplot(d, aes(COL, ROW, fill = PEARSON_R)) + geom_tile(color = "white", linewidth = .7) +
    geom_text(aes(label = sprintf("%.2f", PEARSON_R), color = abs(PEARSON_R) > .36), size = 4.1) +
    scale_color_manual(values = c("FALSE" = "#243443", "TRUE" = "white"), guide = "none") +
    scale_fill_gradient2(low = "#a54839", mid = "#f8f9f7", high = "#247773", midpoint = 0, limits = c(-.65,.65),
                         breaks = c(-.6,-.3,0,.3,.6), name = "Pearson r",
                         guide = guide_colourbar(barwidth = grid::unit(3.3, "in"), barheight = grid::unit(.17, "in"))) +
    labs(x = NULL, y = NULL) + coord_cartesian(clip = "off") +
    theme_minimal(base_size = 14) + theme(panel.grid = element_blank(), legend.position = "bottom",
      axis.text.x = element_text(size = 12, color = "#243443"), axis.text.y = element_text(size = 12, color = "#243443"),
      plot.margin = margin(10, 10, 10, 5))
  ggsave(file.path(out, paste0(role, "_correlations.png")), p, width = 10.4, height = 7.2, dpi = 220, bg = "white")
}
wide <- fread(file.path(input, "school_staff_indices_and_measures.csv"))
va <- fread(file.path(root, "output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv"),
  select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"))
va <- va[analysis_sample == "All" & outcome %chin% c("z_year_math_max", "log_program_income_full_clp_m1")]
setnames(va, c("school_rbd", "outcome", "controlled_value_added_eb_centered_student"), c("RBD", "OUTCOME", "Y"))
d <- merge(wide[, .(RBD, ROLE, balanced_index)], va[, .(RBD, OUTCOME, Y)], by = "RBD", allow.cartesian = TRUE)
d <- d[is.finite(balanced_index) & is.finite(Y)]
d[, Y_Z := (Y - mean(Y))/sd(Y), by = .(ROLE, OUTCOME)]
d[, BIN := ceiling(frank(balanced_index, ties.method = "average") / .N * 20), by = .(ROLE, OUTCOME)]
d[, ROLE := factor(ROLE, levels = c("counselor", "teacher"), labels = c("Orientadores", "HS teachers"))]
d[, OUTCOME := factor(OUTCOME, levels = c("z_year_math_max", "log_program_income_full_clp_m1"), labels = c("Math-score VA", "Projected-income VA"))]
bins <- d[, .(X = mean(balanced_index), Y = mean(Y_Z), N = .N), by = .(ROLE, OUTCOME, BIN)]
p <- ggplot(d, aes(balanced_index, Y_Z)) + geom_hline(yintercept = 0, color = "#dce3e8") +
  geom_point(color = "#798b99", alpha = .15, size = .65) +
  geom_point(data = bins, aes(X, Y), inherit.aes = FALSE, color = "#126e70", size = 2.5) +
  facet_grid(ROLE ~ OUTCOME) + labs(x = "Balanced staff-characteristics index (SD)", y = "School VA (SD within each panel)") +
  theme_minimal(base_size = 15) + theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
    panel.spacing = grid::unit(1.4, "lines"), plot.margin = margin(15, 15, 10, 10))
ggsave(file.path(out, "balanced_index_scatter.png"), p, width = 10.2, height = 7.2, dpi = 220, bg = "white")
message("Created two correlation matrices and the balanced-index scatter figure.")
