###############################################################################
# Revisit MiFuturo log-income imputation by institution + carrera
#
# This script implements the chosen MiFuturo imputation candidate: missing
# income ranges are predicted from additive institution and AREA_CARRERA_GENERICA
# fixed effects. It uses log income only, writes diagnostics and candidate
# predictions, and does not modify official clean data. Title/career names are
# retained only to audit variation within generic careers.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

# ------------------------- Configuration -------------------------

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)

repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(
    getwd(),
    "C:/Users/brunem/Research/causal_schools",
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
  ),
  "repo_wd"
)

dollar_clp_conversion <- 913
cv_folds <- 5L
set.seed(20260630)

raw_mifuturo_dir <- file.path(data_wd, "data", "raw", "mifuturo")
output_dir <- file.path(repo_wd, "output", "tables", "mifuturo_matricula_income")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

mifuturo_csv <- file.path(
  raw_mifuturo_dir,
  "Buscador_Empleabilidad_ingresos_2025_2026_SIES(Carreras e IES (2025-2026)).csv"
)

model_summary_path <- file.path(output_dir, "mifuturo_income_fe_model_summary.csv")
cv_summary_path <- file.path(output_dir, "mifuturo_income_fe_cv_summary.csv")
predictions_path <- file.path(output_dir, "mifuturo_income_fe_predictions.csv")
unsupported_path <- file.path(output_dir, "mifuturo_income_fe_unsupported_missing.csv")
inst_effects_path <- file.path(output_dir, "mifuturo_income_fe_institution_effects.csv")
carrera_effects_path <- file.path(output_dir, "mifuturo_income_fe_carrera_effects.csv")
carrera_title_examples_path <- file.path(output_dir, "mifuturo_carrera_generica_title_variation_examples.csv")
model_artifact_path <- file.path(output_dir, "mifuturo_income_fe_model_artifact.rds")
report_path <- file.path(output_dir, "mifuturo_income_fe_report.md")

# ------------------------- Helpers -------------------------

required_file <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
  path
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

normalize_col_name <- function(x) {
  out <- normalize_text(x)
  out <- tolower(gsub(" ", "_", out))
  out <- gsub("^_|_$", "", out)
  out[nchar(out) == 0] <- paste0("unnamed_", seq_len(sum(nchar(out) == 0)))
  out
}

blank_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "n/a", "s/i", "S/I")] <- NA_character_
  x
}

parse_percent <- function(x) {
  x <- blank_to_na(x)
  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x)) / 100
}

extract_amounts_clp <- function(s) {
  if (is.na(s)) {
    return(numeric())
  }

  s <- normalize_text(gsub("\\$", "", s))
  pattern <- "(\\d+) MILLON(?:ES)?(?: (\\d+) MIL)?|(\\d+) MIL"
  pieces <- regmatches(s, gregexpr(pattern, s, perl = TRUE))[[1]]

  if (length(pieces) == 0 || identical(pieces, character(0))) {
    return(numeric())
  }

  vapply(pieces, function(piece) {
    parts <- regmatches(piece, regexec(pattern, piece, perl = TRUE))[[1]]
    million <- suppressWarnings(as.numeric(parts[2]))
    thousand_after_million <- suppressWarnings(as.numeric(parts[3]))
    thousand_only <- suppressWarnings(as.numeric(parts[4]))

    if (!is.na(million)) {
      return(million * 1e6 + ifelse(is.na(thousand_after_million), 0, thousand_after_million * 1e3))
    }
    if (!is.na(thousand_only)) {
      return(thousand_only * 1e3)
    }
    NA_real_
  }, numeric(1))
}

parse_income_midpoint_clp <- function(x) {
  x <- blank_to_na(x)

  vapply(x, function(s) {
    if (is.na(s)) {
      return(NA_real_)
    }

    if (normalize_text(s) == "SOBRE 3 MILLONES 500 MIL") {
      return(4e6)
    }

    amounts <- extract_amounts_clp(s)
    amounts <- amounts[is.finite(amounts)]
    if (length(amounts) >= 2) {
      return(mean(amounts[1:2]))
    }
    NA_real_
  }, numeric(1))
}

safe_min <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  min(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  stats::median(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (sum(!is.na(x)) < 2) {
    return(NA_real_)
  }
  stats::sd(x, na.rm = TRUE)
}

rmse <- function(x) {
  sqrt(mean(x^2, na.rm = TRUE))
}

mae <- function(x) {
  mean(abs(x), na.rm = TRUE)
}

# ------------------------- Load MiFuturo -------------------------

read_mifuturo_income <- function(path) {
  message("Reading MiFuturo income file: ", path)
  raw <- fread(required_file(path), encoding = "UTF-8", na.strings = c("", "NA", "n/a", "s/i"))
  setnames(raw, normalize_col_name(names(raw)))

  required_cols <- c(
    "codigo",
    "tipo_de_institucion",
    "nombre_de_institucion",
    "area",
    "nombre_carrera_generica",
    "nombre_carrera_del_titulo",
    "empleabilidad_1er_ano",
    "empleabilidad_2_ano",
    "ingreso_promedio_al_4_ano"
  )
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    stop(
      "MiFuturo file is missing expected columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dt <- raw[, .(
    mifuturo_row_id = .I,
    mifuturo_codigo = blank_to_na(codigo),
    TIPO_INST_1 = blank_to_na(tipo_de_institucion),
    NOMB_INST = blank_to_na(nombre_de_institucion),
    mifuturo_area = blank_to_na(area),
    AREA_CARRERA_GENERICA = blank_to_na(nombre_carrera_generica),
    NOMB_CARRERA_TITULO = blank_to_na(nombre_carrera_del_titulo),
    employment_1st_year = parse_percent(empleabilidad_1er_ano),
    employment_2nd_year = parse_percent(empleabilidad_2_ano),
    income_4th_year_raw = blank_to_na(ingreso_promedio_al_4_ano)
  )]

  dt <- dt[
    !is.na(mifuturo_codigo) &
      !is.na(TIPO_INST_1) &
      !is.na(NOMB_INST) &
      !is.na(AREA_CARRERA_GENERICA)
  ]

  dt[, `:=`(
    COD_INST = suppressWarnings(as.integer(mifuturo_codigo)),
    income_4th_year_mid_clp = parse_income_midpoint_clp(income_4th_year_raw)
  )]
  dt[, `:=`(
    income_4th_year_mid_usd = income_4th_year_mid_clp / dollar_clp_conversion,
    log_income_4th_year_mid_clp = log(income_4th_year_mid_clp),
    log_income_4th_year_mid_usd = log(income_4th_year_mid_clp / dollar_clp_conversion),
    income_observed = !is.na(income_4th_year_mid_clp)
  )]

  dt[, `:=`(
    institution_key = paste(
      as.character(COD_INST),
      normalize_text(TIPO_INST_1),
      normalize_text(NOMB_INST),
      sep = " || "
    ),
    carrera_generica_key = normalize_text(AREA_CARRERA_GENERICA)
  )]

  dt[]
}

# ------------------------- Log-Income Model -------------------------

model_specs <- data.table(
  model_name = "institution_plus_carrera_generica",
  carrera_key_col = "carrera_generica_key",
  carrera_label_col = "AREA_CARRERA_GENERICA"
)

fit_income_model <- function(train_dt, carrera_key_col) {
  fit_dt <- copy(train_dt)
  fit_dt[, institution_fe := factor(institution_key)]
  fit_dt[, carrera_fe := factor(get(carrera_key_col))]

  stats::lm(log_income_4th_year_mid_clp ~ institution_fe + carrera_fe, data = fit_dt)
}

predict_log_income <- function(model, new_dt, carrera_key_col) {
  pred_dt <- copy(new_dt)
  pred_dt[, institution_fe := factor(institution_key, levels = model$xlevels$institution_fe)]
  pred_dt[, carrera_fe := factor(get(carrera_key_col), levels = model$xlevels$carrera_fe)]
  level_supported <- !is.na(pred_dt$institution_fe) & !is.na(pred_dt$carrera_fe)

  pred <- rep(NA_real_, nrow(pred_dt))
  if (any(level_supported)) {
    pred[level_supported] <- as.numeric(stats::predict(
      model,
      newdata = pred_dt[level_supported],
      rankdeficient = "NA"
    ))
  }

  list(
    pred = pred,
    level_supported = level_supported,
    estimable = level_supported & is.finite(pred)
  )
}

effect_table <- function(model, variable_name, model_name, label_name) {
  levels <- model$xlevels[[variable_name]]
  coefs <- stats::coef(model)
  effects <- setNames(rep(0, length(levels)), levels)

  prefix <- paste0(variable_name)
  hits <- grep(paste0("^", prefix), names(coefs), value = TRUE)
  hit_levels <- substring(hits, nchar(prefix) + 1L)
  effects[hit_levels] <- coefs[hits]

  centered_effects <- effects - mean(effects, na.rm = TRUE)

  data.table(
    model_name = model_name,
    effect_type = label_name,
    level = names(effects),
    effect_log_clp_raw = as.numeric(effects),
    effect_log_clp_centered = as.numeric(centered_effects)
  )
}

evaluate_model <- function(dt, spec) {
  carrera_key_col <- spec$carrera_key_col
  model_name <- spec$model_name

  train <- dt[income_observed == TRUE]
  missing <- dt[income_observed == FALSE]

  model <- fit_income_model(train, carrera_key_col)
  residuals_log <- as.numeric(stats::residuals(model))
  smear <- mean(exp(residuals_log), na.rm = TRUE)

  missing_pred <- predict_log_income(model, missing, carrera_key_col)
  all_pred <- predict_log_income(model, dt, carrera_key_col)

  fitted_log <- as.numeric(stats::fitted(model))
  fitted_clp <- exp(fitted_log) * smear

  summary_model <- data.table(
    model_name = model_name,
    outcome = "log_income_4th_year_mid_clp",
    n_total_rows = nrow(dt),
    n_observed_income = nrow(train),
    n_missing_income = nrow(missing),
    n_institutions_observed = uniqueN(train$institution_key),
    n_carreras_observed = uniqueN(train[[carrera_key_col]]),
    n_missing_level_supported = sum(missing_pred$level_supported),
    n_missing_nonestimable = sum(missing_pred$level_supported & !missing_pred$estimable),
    n_missing_predicted = sum(missing_pred$estimable),
    n_missing_unsupported = sum(!missing_pred$level_supported),
    share_missing_predicted = mean(missing_pred$estimable),
    r_squared_in_sample = summary(model)$r.squared,
    adj_r_squared_in_sample = summary(model)$adj.r.squared,
    rmse_log_in_sample = rmse(residuals_log),
    mae_log_in_sample = mae(residuals_log),
    rmse_clp_from_log_in_sample = rmse(train$income_4th_year_mid_clp - fitted_clp),
    mae_clp_from_log_in_sample = mae(train$income_4th_year_mid_clp - fitted_clp),
    smear_factor = smear,
    n_model_coefficients = length(stats::coef(model)),
    n_model_rank = model$rank
  )

  predictions <- copy(dt)
  predictions[, `:=`(
    model_name = model_name,
    prediction_level_supported = all_pred$level_supported,
    prediction_estimable = all_pred$estimable,
    log_income_hat_clp = all_pred$pred,
    income_hat_clp = exp(all_pred$pred) * smear,
    income_hat_usd = exp(all_pred$pred) * smear / dollar_clp_conversion,
    income_regression_source = fcase(
      income_observed, "observed_range_midpoint",
      !income_observed & all_pred$estimable, "predicted_log_institution_carrera_fe",
      !income_observed & all_pred$level_supported & !all_pred$estimable, "nonestimable_rank_deficient_fe",
      !income_observed & !all_pred$level_supported, "unsupported_for_prediction",
      default = "unclassified"
    )
  )]

  list(
    model = model,
    smear_factor = smear,
    summary = summary_model,
    predictions = predictions,
    institution_effects = effect_table(model, "institution_fe", model_name, "institution"),
    carrera_effects = effect_table(model, "carrera_fe", model_name, "carrera")
  )
}

cross_validate_model <- function(dt, spec, k = cv_folds) {
  observed <- copy(dt[income_observed == TRUE])
  observed[, fold := sample(rep(seq_len(k), length.out = .N))]

  rbindlist(lapply(seq_len(k), function(fold_id) {
    train <- observed[fold != fold_id]
    test <- observed[fold == fold_id]
    model <- fit_income_model(train, spec$carrera_key_col)
    residuals_log <- as.numeric(stats::residuals(model))
    smear <- mean(exp(residuals_log), na.rm = TRUE)
    pred <- predict_log_income(model, test, spec$carrera_key_col)

    eval_dt <- copy(test)
    eval_dt[, `:=`(
      prediction_level_supported = pred$level_supported,
      prediction_estimable = pred$estimable,
      log_income_hat_clp = pred$pred,
      income_hat_clp = exp(pred$pred) * smear
    )]
    supported_eval <- eval_dt[prediction_estimable == TRUE]

    data.table(
      model_name = spec$model_name,
      fold = fold_id,
      n_train = nrow(train),
      n_test = nrow(test),
      n_test_level_supported = sum(eval_dt$prediction_level_supported),
      n_test_nonestimable = sum(eval_dt$prediction_level_supported & !eval_dt$prediction_estimable),
      n_test_predicted = nrow(supported_eval),
      share_test_predicted = nrow(supported_eval) / nrow(test),
      rmse_log = rmse(supported_eval$log_income_4th_year_mid_clp - supported_eval$log_income_hat_clp),
      mae_log = mae(supported_eval$log_income_4th_year_mid_clp - supported_eval$log_income_hat_clp),
      mean_error_log = mean(supported_eval$log_income_4th_year_mid_clp - supported_eval$log_income_hat_clp, na.rm = TRUE),
      rmse_clp_from_log = rmse(supported_eval$income_4th_year_mid_clp - supported_eval$income_hat_clp),
      mae_clp_from_log = mae(supported_eval$income_4th_year_mid_clp - supported_eval$income_hat_clp)
    )
  }), use.names = TRUE)
}

fit_fallback_models <- function(dt) {
  train <- copy(dt[income_observed == TRUE])
  train[, institution_fe := factor(institution_key)]
  train[, carrera_fe := factor(carrera_generica_key)]

  carrera_model <- stats::lm(log_income_4th_year_mid_clp ~ carrera_fe, data = train)
  institution_model <- stats::lm(log_income_4th_year_mid_clp ~ institution_fe, data = train)

  carrera_resid <- as.numeric(stats::residuals(carrera_model))
  institution_resid <- as.numeric(stats::residuals(institution_model))
  global_log_mean <- mean(train$log_income_4th_year_mid_clp, na.rm = TRUE)

  list(
    carrera_model = carrera_model,
    carrera_smear_factor = mean(exp(carrera_resid), na.rm = TRUE),
    institution_model = institution_model,
    institution_smear_factor = mean(exp(institution_resid), na.rm = TRUE),
    global_log_mean = global_log_mean,
    global_smear_factor = mean(exp(train$log_income_4th_year_mid_clp - global_log_mean), na.rm = TRUE)
  )
}

# ------------------------- Run -------------------------

mifuturo <- read_mifuturo_income(mifuturo_csv)

model_results <- lapply(seq_len(nrow(model_specs)), function(i) {
  evaluate_model(mifuturo, model_specs[i])
})
fallback_models <- fit_fallback_models(mifuturo)

model_summary <- rbindlist(lapply(model_results, `[[`, "summary"), use.names = TRUE)
predictions <- rbindlist(lapply(model_results, `[[`, "predictions"), use.names = TRUE, fill = TRUE)
institution_effects <- rbindlist(lapply(model_results, `[[`, "institution_effects"), use.names = TRUE)
carrera_effects <- rbindlist(lapply(model_results, `[[`, "carrera_effects"), use.names = TRUE)

cv_by_fold <- rbindlist(lapply(seq_len(nrow(model_specs)), function(i) {
  cross_validate_model(mifuturo, model_specs[i])
}), use.names = TRUE)

cv_summary <- cv_by_fold[, .(
  folds = .N,
  mean_share_test_predicted = mean(share_test_predicted, na.rm = TRUE),
  mean_rmse_log = mean(rmse_log, na.rm = TRUE),
  mean_mae_log = mean(mae_log, na.rm = TRUE),
  mean_error_log = mean(mean_error_log, na.rm = TRUE),
  mean_rmse_clp_from_log = mean(rmse_clp_from_log, na.rm = TRUE),
  mean_mae_clp_from_log = mean(mae_clp_from_log, na.rm = TRUE),
  min_share_test_predicted = min(share_test_predicted, na.rm = TRUE)
), by = model_name]

carrera_title_examples <- mifuturo[
  ,
  .(
    n_mifuturo_rows = .N,
    n_titles = uniqueN(NOMB_CARRERA_TITULO),
    n_institutions = uniqueN(institution_key),
    observed_income_rows = sum(income_observed),
    missing_income_rows = sum(!income_observed),
    income_min_clp = safe_min(income_4th_year_mid_clp),
    income_median_clp = safe_median(income_4th_year_mid_clp),
    income_max_clp = safe_max(income_4th_year_mid_clp),
    income_sd_clp = safe_sd(income_4th_year_mid_clp),
    example_titles = paste(head(sort(unique(na.omit(NOMB_CARRERA_TITULO))), 12), collapse = " | "),
    example_institutions = paste(head(sort(unique(na.omit(NOMB_INST))), 8), collapse = " | ")
  ),
  by = .(AREA_CARRERA_GENERICA)
][order(-n_titles, AREA_CARRERA_GENERICA)]

unsupported_missing <- predictions[
  income_observed == FALSE & prediction_estimable == FALSE,
  .(
    mifuturo_row_id,
    model_name,
    mifuturo_codigo,
    COD_INST,
    TIPO_INST_1,
    NOMB_INST,
    AREA_CARRERA_GENERICA,
    NOMB_CARRERA_TITULO,
    income_4th_year_raw,
    employment_1st_year,
    employment_2nd_year,
    institution_key,
    carrera_generica_key
  )
][order(model_name, COD_INST, AREA_CARRERA_GENERICA)]

prediction_cols <- c(
  "mifuturo_row_id",
  "model_name",
  "mifuturo_codigo",
  "COD_INST",
  "TIPO_INST_1",
  "NOMB_INST",
  "mifuturo_area",
  "AREA_CARRERA_GENERICA",
  "NOMB_CARRERA_TITULO",
  "employment_1st_year",
  "employment_2nd_year",
  "income_4th_year_raw",
  "income_4th_year_mid_clp",
  "income_4th_year_mid_usd",
  "log_income_4th_year_mid_clp",
  "income_observed",
  "prediction_level_supported",
  "prediction_estimable",
  "log_income_hat_clp",
  "income_hat_clp",
  "income_hat_usd",
  "income_regression_source"
)

message("Writing diagnostics to: ", output_dir)
fwrite(model_summary, model_summary_path)
fwrite(cv_summary, cv_summary_path)
fwrite(predictions[, ..prediction_cols], predictions_path)
fwrite(unsupported_missing, unsupported_path)
fwrite(institution_effects, inst_effects_path)
fwrite(carrera_effects, carrera_effects_path)
fwrite(carrera_title_examples, carrera_title_examples_path)

saveRDS(
  list(
    model_name = model_specs$model_name[[1]],
    outcome = "log_income_4th_year_mid_clp",
    model = model_results[[1]]$model,
    smear_factor = model_results[[1]]$smear_factor,
    fallback_models = fallback_models,
    dollar_clp_conversion = dollar_clp_conversion,
    carrera_key_col = model_specs$carrera_key_col[[1]],
    carrera_label_col = model_specs$carrera_label_col[[1]],
    institution_key_definition = "COD_INST + normalized TIPO_INST_1 + normalized NOMB_INST",
    carrera_key_definition = "normalized AREA_CARRERA_GENERICA",
    generated_at = Sys.time()
  ),
  model_artifact_path
)

report <- c(
  "# MiFuturo Generic-Career Log-Income Regression Diagnostic",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "This revisits the earlier imputation idea in `code/processing_college_apps_outcome.R`:",
  "",
  "`log(income) ~ institution + carrera`",
  "",
  "The chosen carrera variable is `AREA_CARRERA_GENERICA`. The script treats this as a candidate construction, not an official clean-data modification.",
  "",
  "## Model",
  "",
  "- `institution_plus_carrera_generica`: institution FE plus MiFuturo generic career FE.",
  "",
  "Institution is coded as `Codigo + Tipo de institucion + Nombre de institucion`.",
  "The outcome is the log midpoint of MiFuturo's 4th-year monthly income range in CLP.",
  "Predicted CLP levels use Duan smearing from the fitted log model.",
  "`NOMB_CARRERA_TITULO` is retained only to audit within-generic-career variation; it is not used as a fixed effect in the chosen model.",
  "",
  "## In-Sample Model Summary",
  "",
  paste(capture.output(print(model_summary)), collapse = "\n"),
  "",
  "## Five-Fold Cross-Validation Summary",
  "",
  paste(capture.output(print(cv_summary)), collapse = "\n"),
  "",
  "## Carrera Generica / Title Variation Examples",
  "",
  paste(capture.output(print(head(carrera_title_examples, 12))), collapse = "\n"),
  "",
  "## Outputs",
  "",
  paste0("- `", model_summary_path, "`"),
  paste0("- `", cv_summary_path, "`"),
  paste0("- `", predictions_path, "`"),
  paste0("- `", unsupported_path, "`"),
  paste0("- `", inst_effects_path, "`"),
  paste0("- `", carrera_effects_path, "`"),
  paste0("- `", carrera_title_examples_path, "`"),
  paste0("- `", model_artifact_path, "`"),
  "",
  "## Cautions",
  "",
  "- `AREA_CARRERA_GENERICA` is the preferred career dimension for this imputation because it has much better support than title-specific career names with similar cross-validated log-income error.",
  "- This model imputes missing MiFuturo income rows from observed MiFuturo rows; it does not solve the separate `COD_SIES` merge problem.",
  "- A row is predicted only if its institution and carrera levels are observed in the income-training sample and the corresponding two-way FE prediction is estimable.",
  "- The old object name `income_imp` was in USD in the earlier script; this diagnostic writes explicit CLP and USD columns."
)

writeLines(report, report_path)

message("Done.")
print(model_summary)
print(cv_summary)
