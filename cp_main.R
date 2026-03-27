# Code to load chest pain clinical note files, create a JSONL file, submit to Azure OpenAI,
# and store returned results.

suppressPackageStartupMessages({
  library(readxl)
  library(ggplot2)
  library(tidyr)
  library(scales)
  library(readr)
  library(jsonlite)
  library(httr2)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

setwd(dir = "/phi/sbi/chest_pain/cp_github_files/")

# -------------------------------
# Configuration
# -------------------------------
excel_path <- "/phi/sbi/chest_pain/gould_cp_full.csv"
jsonl_output_path <- "/phi/sbi/chest_pain/cp_requests.jsonl"
results_csv_path <- "/phi/sbi/chest_pain/cp_results.csv"
results_raw_json_path <- "/phi/sbi/chest_pain/cp_results_raw.json"
dates_raw_path <- "/phi/sbi/chest_pain/visit_dates.xlsx"

api_url <- Sys.getenv(
  "AZURE_OPENAI_RESPONSES_URL",
  unset = "https://researchinformatics-che-resource.cognitiveservices.azure.com/openai/responses?api-version=2025-04-01-preview"
)

key_df <- readr::read_csv(file = "/phi/sbi/chest_pain/cp_key.csv", show_col_types = FALSE)

api_key <- key_df$key[[1]]
model_name <- Sys.getenv("AZURE_OPENAI_DEPLOYMENT", unset = "cp_5.2")

# Set TRUE to submit each note to the API.
run_api_requests <- TRUE


normalize_colnames <- function(df) {
  names(df) <- names(df) |>
    tolower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("(^_|_$)", "")
  df
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

coalesce_chr <- function(x, y) {
  out <- ifelse(is.na(x) | x == "", y, x)
  as.character(out)
}

clean_text <- function(x) {
  x <- as.character(x)

  # Convert from current/native encoding to UTF-8
  x <- iconv(x, from = "", to = "UTF-8", sub = " ")

  # Remove any remaining control characters except tab/newline if desired
  x <- stringr::str_replace_all(x, "[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F]", " ")

  # Optional: normalize whitespace
  x <- stringr::str_replace_all(x, "[[:space:]]+", " ")
  x <- stringr::str_trim(x)

  x
}

build_user_payload <- function(row) {
  paste0(
    "Patient metadata:\n",
    "- patient_id (csn): ", row$patient_id, "\n",
    "- mrn: ", row$mrn, "\n",
    "- age_years: ", row$visitage, "\n",
    "- sex: ", row$sex, "\n",
    "- ecg_performed: ", row$ecg_yn, "\n",
    "- ecg_summary: ", row$ecg_summary, "\n\n",
    "Clinical note:\n",
    row$extracted_note
  )
}

extract_response_text <- function(resp_obj) {
  if (!is.null(resp_obj$output_text) && nzchar(resp_obj$output_text)) {
    return(resp_obj$output_text)
  }

  if (!is.null(resp_obj$output) && length(resp_obj$output) > 0) {
    texts <- c()
    for (item in resp_obj$output) {
      if (!is.null(item$content) && length(item$content) > 0) {
        for (ct in item$content) {
          if (!is.null(ct$text)) texts <- c(texts, ct$text)
        }
      }
    }
    if (length(texts) > 0) return(paste(texts, collapse = "\n"))
  }

  NA_character_
}

parse_model_json <- function(txt) {
  if (is.na(txt)) return(NULL)
  txt <- stringr::str_trim(txt)
  if (!nzchar(txt)) return(NULL)

  tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

auc_json_schema <- function() {
  list(
    type = "json_schema",
    name = "chest_pain_auc_result",
    strict = TRUE,
    schema = list(
      type = "object",
      additionalProperties = FALSE,
      properties = list(
        patient_id = list(
          type = "string",
          description = "Patient CSN as a string."
        ),
        auc_category = list(
          type = "string",
          enum = c("Appropriate", "May Be Appropriate", "Rarely Appropriate")
        ),
        applicable_auc_criteria = list(
          type = "array",
          description = "All AUC criteria applicable to the note.",
          items = list(type = "string")
        ),
        rationale = list(
          type = "string",
          description = "One to two sentence rationale based directly on the note."
        ),
        supporting_phrases = list(
          type = "array",
          description = "Direct quotes copied from the note.",
          items = list(type = "string")
        ),
        confidence_score = list(
          type = "integer",
          enum = 1:5,
          description = "Confidence score from 1 to 5."
        )
      ),
      required = c(
        "patient_id",
        "auc_category",
        "applicable_auc_criteria",
        "rationale",
        "supporting_phrases",
        "confidence_score"
      )
    )
  )
}


# -------------------------------
# Load data
# -------------------------------
prompt_text <- "You are a clinical reasoning assistant with expertise in pediatric cardiology. Your task is to read outpatient clinical notes from pediatric cardiology visits for patients whose chief complaint is chest pain. Using the information in the note, determine whether an echocardiogram is Appropriate, May Be Appropriate, or Rarely Appropriate based on the 2014 Appropriate Use Criteria (AUC) for Initial Transthoracic Echocardiography in Outpatient Pediatric Cardiology.

You should identify key symptoms, family history elements, physical exam findings, vital signs, and ECG findings when present.

DECISION RULES

Use the most conservative applicable category.
- Choose Appropriate if any Appropriate criteria are present.
- Choose May Be Appropriate only if no Appropriate criteria apply.
- Choose Rarely Appropriate only if no Appropriate or May Be Appropriate criteria apply.

AUC CATEGORIES:

Appropriate
Choose Appropriate if any of the following AUC criteria are present:

1. Exertional chest pain
Any chest pain occurring or worse during physical exertion, even if it occurred only once or has resolved. In order to classify under this criteria, it must be the chest pain that is exertional and not a different symptom (e.g. shortness of breath).

2. Non-exertional chest pain with abnormal ECG
ECG rules
- If ECG interpretation says 'abnormal' or 'borderline', classify as abnormal regardless of specific findings.
- If ECG interpretation says 'normal' or 'otherwise normal', classify as normal.
- If the HPI describes a previous abnormal ECG, count it as abnormal even if the ECG summary from that visit is normal.
- If a previous ECG was inconclusive, but the current ECG is normal, treat the ECG as normal.

3. Chest pain with family history of sudden unexplained death or cardiomyopathy
Definitions
- Sudden unexplained death: death without a known cause under age 50
Family history may include:
- Parents
- Siblings
- Grandparents
- Aunts or uncles

May Be Appropriate
Choose May Be Appropriate only if no Appropriate criteria apply and one of the following AUC criteria is present.

1. Chest pain with other symptoms or signs of cardiovascular disease, a benign family history, and a normal ECG
Symptoms or signs of cardiovascular disease
Symptoms:
- Palpitations
- 'Heart racing' or similar (assume this refers to palpitations unless explicitly denied)
- Syncope
- Exertional presyncope / dizziness / lightheadedness
- Shortness of breath or dyspnea not explained by another condition (e.g., asthma)

Physical exam findings:
- Cardiac murmur
- Any abnormal cardiac exam finding documented in the physical exam

Benign family history means none of the following:
- Sudden unexplained death
- Cardiomyopathy
- Premature coronary artery disease

2. Chest pain with family history of premature coronary artery disease
Define premature coronary artery disease as:
- Myocardial infarction
- Coronary stent
- Coronary artery bypass grafting
- Any coronary artery disease occurring before age 50 in any family member.

3. Chest pain with recent onset of fever
Definition: Fever >= 38.0 C (100.4 F) within the last two weeks

4. Chest pain with recent illicit drug use
Definition: Illicit drug use within the past two weeks
For this study:
- Marijuana counts as illicit drug use

Rarely Appropriate
Classify as Rarely Appropriate only if no Appropriate or May Be Appropriate criteria are present. Relevant Rarely Appropriate patterns include:
1. Chest pain with no other symptoms or signs of cardiovascular disease, benign family history, and normal ECG
2. Non-exertional chest pain with no recent ECG
3. Non-exertional chest pain with normal ECG
4. Reproducible chest pain with palpation or deep inspiration

ADDITIONAL CLARIFICATIONS

Symptoms considered cardiovascular symptoms
- Palpitations
- 'Heart racing'
- Syncope
- Exertional dizziness / presyncope
- Dyspnea not explained by another condition

Abnormal cardiovascular exam examples
- Murmur
- Gallop
- Hepatomegaly
- Rales
- Peripheral edema
- Any abnormal cardiac exam finding

OUTPUT INSTRUCTIONS

Return your answer only in the structured JSON format requested by the schema.

Field requirements:
- patient_id: copy the patient CSN exactly as provided in the input.
- auc_category: must be exactly one of 'Appropriate', 'May Be Appropriate', or 'Rarely Appropriate'.
- applicable_auc_criteria: Specify all AUC criteria that were applicable to this note. Include all A criteria, M criteria, and R criteria that apply to the note (e.g., A1 – Exertional chest pain, M1 – Chest pain with other cardiovascular symptoms, R4 – Reproducible chest pain with palpation).
- rationale: 1 to 2 sentences explaining the classification and explicitly referencing the applicable AUC criterion or criteria.
- supporting_phrases: an array of direct quotes copied verbatim from the clinical note that support the chosen auc category.
- confidence_score: an integer from 1 = Low confidence, 2 = Some uncertainty, 3 = Moderate confidence, 4 = High confidence,and 5 = Complete confidence

Do not return pipe-separated text, CSV-style rows, markdown, or any extra keys."

### Code to store current version of prompt ###
writeLines(
  prompt_text,
  con = "/phi/sbi/chest_pain/cp_github_files/prompt_v03.txt",
  useBytes = TRUE
)

prompt_text <- paste(readLines("prompt_v01.txt"), collapse = "\n")


df_full <- readr::read_csv(excel_path, show_col_types = FALSE)
df_full <- df_full %>% rename(chiefcomp = chiefcomplaint, visitdiagnc = visitdiagnosis)
df_full <- df_full |>
  normalize_colnames() |>
  dplyr::mutate(
    extracted_note = clean_text(extracted_note),
    ecg_summary    = clean_text(ecg_summary),
    sex            = clean_text(sex),
    ecg_yn         = clean_text(ecg_yn),
    chiefcomp      = clean_text(chiefcomp),
    visitdiagnc    = clean_text(visitdiagnc)
  )

# drop useless visit_date column
df_full <- df_full %>% dplyr::select(-visitdate)

# Load in date info to ensure first 20 used for training, then validation, etc
dates_raw <-  read_excel(path = dates_raw_path)


# Add dates to df_full
df_full <- df_full %>% left_join(dates_raw %>% dplyr::select(csn, visitdate), by = "csn")
df_full <- df_full %>% arrange(visitdate)


df <- df_full %>% filter(mrn %in% list_of_ids) # 1:20 training set, 21:42 validation, 42:130


required_cols <- c("csn", "mrn", "visitage", "sex", "chiefcomp", "visitdiagnc", "ecg_yn", "ecg_summary", "extracted_note")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing expected columns in spreadsheet: ", paste(missing_cols, collapse = ", "))
}

requests_tbl <- df |>
  mutate(
    patient_id = as.character(csn),
    mrn = as.character(mrn),
    extracted_note = coalesce_chr(extracted_note, "")
  ) |>
  transmute(
    custom_id = paste0("cp_", row_number(), "_", patient_id),
    patient_id,
    body = pmap(
      list(patient_id, mrn, visitage, sex, ecg_yn, ecg_summary, extracted_note),
      function(patient_id, mrn, visitage, sex, ecg_yn, ecg_summary, extracted_note) {
        row <- list(
          patient_id = patient_id,
          mrn = mrn,
          visitage = visitage,
          sex = sex,
          ecg_yn = ecg_yn,
          ecg_summary = ecg_summary,
          extracted_note = extracted_note
        )

        list(
          model = model_name,
          instructions = prompt_text,
          input = build_user_payload(row),
          text = list(
            format = auc_json_schema()
          )
        )
      }
    )
  )

# -------------------------------
# Write JSONL file for batch submission
# -------------------------------
jsonl_lines <- requests_tbl |>
  mutate(
    request = pmap(
      list(custom_id, body),
      \(custom_id, body) {
        list(
          custom_id = custom_id,
          method = "POST",
          url = "/openai/responses",
          body = body
        )
      }
    ),
    json = map_chr(request, ~ toJSON(.x, auto_unbox = TRUE, null = "null"))
  ) |>
  pull(json)

writeLines(jsonl_lines, con = jsonl_output_path, useBytes = TRUE)
message("Wrote JSONL file: ", jsonl_output_path)

# -------------------------------
# Submit requests and save outputs
# -------------------------------

run_api_requests <- TRUE


if (run_api_requests) {
  if (!nzchar(api_key)) {
    stop("AZURE_OPENAI_API_KEY is empty. Set it in your environment before enabling run_api_requests.")
  }

  raw_results <- requests_tbl |>
    transmute(
      custom_id,
      patient_id,
      response = map(body, \(b) {
        req <- request(api_url) |>
          req_method("POST") |>
          req_headers(
            `api-key` = api_key,
            `Content-Type` = "application/json"
          ) |>
          req_body_json(b, auto_unbox = TRUE)

        resp <- tryCatch(
          req_perform(req),
          error = function(e) {
            resp <- e$resp

            if (!is.null(resp)) {
              raw_txt <- tryCatch(
                resp_body_string(resp),
                error = function(e2) "<Could not read response body>"
              )

              stop(
                paste0(
                  "Azure request failed.\n",
                  "Status: ", tryCatch(resp_status(resp), error = function(e2) "unknown"), "\n",
                  "Content-Type: ", tryCatch(resp_header(resp, "content-type") %||% "", error = function(e2) "unknown"), "\n",
                  "Body (first 4000 chars):\n",
                  substr(raw_txt, 1, 4000)
                ),
                call. = FALSE
              )
            } else {
              stop(
                paste0(
                  "Azure request failed during req_perform(), but no response body was available.\n",
                  "Original error: ", conditionMessage(e)
                ),
                call. = FALSE
              )
            }
          }
        )

        resp_body_json(resp, simplifyVector = FALSE)
      })
    )

  parsed_results <- raw_results |>
    mutate(
      output_text = map_chr(response, extract_response_text),
      parsed = map(output_text, parse_model_json),
      returned_patient_id = map_chr(parsed, ~ .x$patient_id %||% NA_character_),
      auc_category = map_chr(parsed, ~ .x$auc_category %||% NA_character_),
      applicable_auc_criteria = map_chr(parsed, ~ {
        x <- .x$applicable_auc_criteria
        if (is.null(x)) return(NA_character_)
        paste(unlist(x), collapse = " | ")
      }),
      rationale = map_chr(parsed, ~ .x$rationale %||% NA_character_),
      supporting_phrases = map_chr(parsed, ~ {
        phrases <- .x$supporting_phrases
        if (is.null(phrases)) return(NA_character_)
        paste(unlist(phrases), collapse = " | ")
      }),
      confidence_score = map_dbl(parsed, ~ {
        x <- .x$confidence_score
        if (is.null(x) || length(x) != 1) return(NA_real_)
        suppressWarnings(as.numeric(x))
      })
    ) |>
    dplyr::select(
      custom_id,
      patient_id,
      returned_patient_id,
      auc_category,
      applicable_auc_criteria,
      rationale,
      supporting_phrases,
      confidence_score,
      output_text
    )

  write.csv(parsed_results, results_csv_path, row.names = FALSE, na = "")
  writeLines(
    map_chr(raw_results$response, ~ toJSON(.x, auto_unbox = TRUE, null = "null")),
    con = results_raw_json_path,
    useBytes = TRUE
  )

  message("Saved parsed results to: ", results_csv_path)
  message("Saved raw JSON responses to: ", results_raw_json_path)

} else {
  message("run_api_requests is FALSE. JSONL was generated but API calls were skipped.")
}

# QC, ensure that all study_id values match the returned study id values
print(paste0("# of non-matched study ID values = ", sum(parsed_results$patient_id != parsed_results$returned_patient_id))) # should return 0

# Fix dates df, then bind dates with parsed df
dates_df <- dates_raw %>% rename(patient_id = csn)
dates_df$patient_id <- as.character(dates_df$patient_id)

final_model_scores <- parsed_results %>% left_join(dates_df %>% dplyr::select(patient_id, visitdate), by = "patient_id")
final_model_scores <- final_model_scores %>% relocate(patient_id, returned_patient_id, custom_id, visitdate)

View(final_model_scores %>% dplyr::select(patient_id, returned_patient_id, visitdate, auc_category, applicable_auc_criteria, rationale,
                                          supporting_phrases, confidence_score) %>% arrange(visitdate))

ids <- df %>% dplyr::select(mrn, csn) %>% mutate(csn = as.character(csn))
final_model_scores <- final_model_scores %>% left_join(ids %>% rename(patient_id = csn))


# Load in gold standard labels
# gold <- read_xls(path = "/phi/sbi/chest_pain/gold_labels.xlsx")

# Temp code to add random outcome labels
set.seed(123)  # optional, for reproducibility
gold_temp <- data.frame(
  truth_label = sample(
    x = c("Appropriate", "May Be Appropriate", "Rarely Appropriate"),
    size = nrow(final_model_scores),
    replace = TRUE
  ),
  stringsAsFactors = FALSE
)

analysis_df <- bind_cols(final_model_scores, gold_temp)

# Now perform analysis using the ground truth label #

### ------------------------------ ###
### 1) Prepare evaluation dataset  ###
### ------------------------------ ###

# Expected ordered class labels
ordered_levels <- c(
  "Rarely Appropriate",
  "May Be Appropriate",
  "Appropriate"
)

# Basic label checks
if (!all(stats::na.omit(unique(analysis_df$truth_label)) %in% ordered_levels)) {
  stop("Unexpected value(s) found in truth_label.")
}

if (!all(stats::na.omit(unique(analysis_df$auc_category)) %in% ordered_levels)) {
  stop("Unexpected value(s) found in auc_category.")
}

# Keep only rows with non-missing truth/prediction for the core evaluation
eval_df <- analysis_df %>%
  dplyr::filter(!is.na(truth_label), !is.na(auc_category)) %>%
  dplyr::mutate(
    truth_label  = factor(truth_label, levels = ordered_levels, ordered = TRUE),
    auc_category = factor(auc_category, levels = ordered_levels, ordered = TRUE),
    truth_num    = as.integer(truth_label),
    pred_num     = as.integer(auc_category),
    signed_diff  = pred_num - truth_num,
    abs_diff     = abs(signed_diff),
    exact_match  = abs_diff == 0
  )

n_eval <- nrow(eval_df)

### -------------------------------- ###
### 2) Confusion matrix              ###
### -------------------------------- ###

conf_mat <- table(
  Truth = eval_df$truth_label,
  Prediction = eval_df$auc_category
)

conf_mat_df <- as.data.frame.matrix(conf_mat)

### -------------------------------- ###
### 3) Overall accuracy              ###
### -------------------------------- ###

overall_accuracy <- mean(eval_df$exact_match)

# Optional exact binomial 95% CI for accuracy
overall_accuracy_ci <- stats::binom.test(
  x = sum(eval_df$exact_match),
  n = n_eval
)$conf.int

accuracy_results <- tibble::tibble(
  n = n_eval,
  correct = sum(eval_df$exact_match),
  accuracy = overall_accuracy,
  accuracy_ci_lower = overall_accuracy_ci[1],
  accuracy_ci_upper = overall_accuracy_ci[2]
)

### -------------------------------- ###
### 4) Weighted Cohen's kappa        ###
###    (quadratic weights)           ###
### -------------------------------- ###

quadratic_weighted_kappa <- function(truth, pred, levels) {
  truth_f <- factor(truth, levels = levels, ordered = TRUE)
  pred_f  <- factor(pred,  levels = levels, ordered = TRUE)

  O_counts <- table(truth_f, pred_f)
  O <- O_counts / sum(O_counts)

  row_marg <- rowSums(O)
  col_marg <- colSums(O)
  E <- outer(row_marg, col_marg)

  k <- length(levels)

  # Quadratic disagreement weights:
  # 0 on diagonal, 1 for maximal disagreement
  W <- outer(
    1:k, 1:k,
    FUN = function(i, j) ((i - j) / (k - 1))^2
  )

  kappa_w <- 1 - (sum(W * O) / sum(W * E))

  list(
    weighted_kappa = as.numeric(kappa_w),
    observed_matrix = O_counts,
    observed_prop = O,
    expected_prop = E,
    weights = W
  )
}

kappa_out <- quadratic_weighted_kappa(
  truth = eval_df$truth_label,
  pred  = eval_df$auc_category,
  levels = ordered_levels
)

weighted_kappa <- kappa_out$weighted_kappa

kappa_results <- tibble::tibble(
  metric = "Quadratic weighted Cohen's kappa",
  value = weighted_kappa
)

### ---------------------------------------------- ###
### 5) Exact / adjacent / extreme disagreement     ###
### ---------------------------------------------- ###

disagreement_summary <- eval_df %>%
  dplyr::mutate(
    disagreement_type = dplyr::case_when(
      abs_diff == 0 ~ "Exact agreement",
      abs_diff == 1 ~ "Adjacent disagreement",
      abs_diff == 2 ~ "Extreme disagreement",
      TRUE ~ "Other"
    )
  ) %>%
  dplyr::count(disagreement_type, name = "n") %>%
  dplyr::mutate(
    prop = n / sum(n)
  ) %>%
  dplyr::arrange(
    factor(
      disagreement_type,
      levels = c(
        "Exact agreement",
        "Adjacent disagreement",
        "Extreme disagreement",
        "Other"
      )
    )
  )

### ------------------------------------------------------------- ###
### 6) Per-class sensitivity, specificity, PPV, NPV, precision,  ###
###    recall, F1 (one-vs-rest for each class)                    ###
### ------------------------------------------------------------- ###

safe_divide <- function(num, den) {
  ifelse(den == 0, NA_real_, num / den)
}

per_class_metrics <- lapply(ordered_levels, function(cls) {

  tp <- sum(eval_df$truth_label == cls & eval_df$auc_category == cls)
  fn <- sum(eval_df$truth_label == cls & eval_df$auc_category != cls)
  fp <- sum(eval_df$truth_label != cls & eval_df$auc_category == cls)
  tn <- sum(eval_df$truth_label != cls & eval_df$auc_category != cls)

  sensitivity <- safe_divide(tp, tp + fn)
  specificity <- safe_divide(tn, tn + fp)
  ppv         <- safe_divide(tp, tp + fp)
  npv         <- safe_divide(tn, tn + fn)

  # In one-vs-rest multiclass classification:
  # precision == PPV, recall == sensitivity
  precision <- ppv
  recall    <- sensitivity
  f1        <- safe_divide(2 * precision * recall, precision + recall)

  tibble::tibble(
    class = cls,
    support = sum(eval_df$truth_label == cls),
    predicted_n = sum(eval_df$auc_category == cls),
    prevalence = mean(eval_df$truth_label == cls),
    tp = tp,
    fn = fn,
    fp = fp,
    tn = tn,
    sensitivity = sensitivity,
    specificity = specificity,
    PPV = ppv,
    NPV = npv,
    precision = precision,
    recall = recall,
    F1 = f1
  )
}) %>%
  dplyr::bind_rows()

macro_metrics <- tibble::tibble(
  class = "Macro average",
  support = sum(per_class_metrics$support),
  predicted_n = sum(per_class_metrics$predicted_n),
  prevalence = NA_real_,
  tp = NA_integer_,
  fn = NA_integer_,
  fp = NA_integer_,
  tn = NA_integer_,
  sensitivity = mean(per_class_metrics$sensitivity, na.rm = TRUE),
  specificity = mean(per_class_metrics$specificity, na.rm = TRUE),
  PPV = mean(per_class_metrics$PPV, na.rm = TRUE),
  NPV = mean(per_class_metrics$NPV, na.rm = TRUE),
  precision = mean(per_class_metrics$precision, na.rm = TRUE),
  recall = mean(per_class_metrics$recall, na.rm = TRUE),
  F1 = mean(per_class_metrics$F1, na.rm = TRUE)
)

per_class_metrics_with_macro <- dplyr::bind_rows(
  per_class_metrics,
  macro_metrics
)

### -------------------------------- ###
### 7) Direction of disagreement     ###
### -------------------------------- ###

# signed_diff:
# +1 or +2 = LLM more permissive than gold standard
# -1 or -2 = LLM more conservative than gold standard
#  0        = exact agreement

direction_summary_all <- tibble::tibble(
  category = c(
    "Exact agreement",
    "LLM more conservative than truth",
    "LLM more permissive than truth"
  ),
  n = c(
    sum(eval_df$signed_diff == 0),
    sum(eval_df$signed_diff < 0),
    sum(eval_df$signed_diff > 0)
  )
) %>%
  dplyr::mutate(
    prop_all_cases = n / sum(n)
  )

direction_summary_errors_only <- eval_df %>%
  dplyr::filter(signed_diff != 0) %>%
  dplyr::summarise(
    n_errors = dplyr::n(),
    n_more_conservative = sum(signed_diff < 0),
    n_more_permissive = sum(signed_diff > 0)
  ) %>%
  dplyr::mutate(
    prop_more_conservative_among_errors = safe_divide(n_more_conservative, n_errors),
    prop_more_permissive_among_errors = safe_divide(n_more_permissive, n_errors)
  )

direction_by_magnitude <- eval_df %>%
  dplyr::filter(signed_diff != 0) %>%
  dplyr::mutate(
    direction = dplyr::if_else(signed_diff < 0, "More conservative", "More permissive"),
    magnitude = abs(signed_diff)
  ) %>%
  dplyr::count(direction, magnitude, name = "n") %>%
  dplyr::mutate(prop = n / sum(n))

### ---------------------------------------------- ###
### 8) Exploratory confidence-score analysis       ###
### ---------------------------------------------- ###

# Restrict to rows with non-missing confidence score
conf_df <- eval_df %>%
  dplyr::filter(!is.na(confidence_score))

# A) Performance stratified by confidence score
confidence_by_score <- conf_df %>%
  dplyr::group_by(confidence_score) %>%
  dplyr::summarise(
    n = dplyr::n(),
    exact_accuracy = mean(exact_match),
    within_one_category = mean(abs_diff <= 1),
    mean_abs_error = mean(abs_diff),
    median_abs_error = stats::median(abs_diff),
    mean_signed_diff = mean(signed_diff),
    .groups = "drop"
  ) %>%
  dplyr::arrange(confidence_score)

# B) Compare confidence for correct vs incorrect predictions
confidence_by_correctness <- conf_df %>%
  dplyr::group_by(exact_match) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_confidence = mean(confidence_score),
    sd_confidence = stats::sd(confidence_score),
    median_confidence = stats::median(confidence_score),
    iqr_confidence = stats::IQR(confidence_score),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    exact_match = dplyr::if_else(exact_match, "Correct", "Incorrect")
  )

# C) Simple logistic regression:
#    Is higher confidence associated with a higher chance of exact correctness?
if (
  nrow(conf_df) > 0 &&
  length(unique(conf_df$confidence_score)) > 1 &&
  length(unique(conf_df$exact_match)) > 1
) {
  conf_glm <- stats::glm(
    exact_match ~ confidence_score,
    data = conf_df,
    family = stats::binomial()
  )

  conf_glm_coef <- summary(conf_glm)$coefficients

  confidence_glm_results <- tibble::tibble(
    term = rownames(conf_glm_coef),
    estimate = conf_glm_coef[, "Estimate"],
    std_error = conf_glm_coef[, "Std. Error"],
    z_value = conf_glm_coef[, "z value"],
    p_value = conf_glm_coef[, "Pr(>|z|)"]
  )

  # Odds ratio for a 1-point increase in confidence score
  conf_or <- tibble::tibble(
    term = "confidence_score",
    odds_ratio = exp(stats::coef(conf_glm)[["confidence_score"]]),
    conf_low = exp(stats::confint(conf_glm)["confidence_score", 1]),
    conf_high = exp(stats::confint(conf_glm)["confidence_score", 2])
  )
} else {
  conf_glm <- NULL
  confidence_glm_results <- NULL
  conf_or <- NULL
}

# D) Spearman correlation between confidence and absolute error
#    Higher confidence should ideally correlate with LOWER abs error
if (
  nrow(conf_df) > 2 &&
  length(unique(conf_df$confidence_score)) > 1 &&
  length(unique(conf_df$abs_diff)) > 1
) {
  spearman_conf_abs_error <- stats::cor.test(
    ~ confidence_score + abs_diff,
    data = conf_df,
    method = "spearman",
    exact = FALSE
  )

  spearman_results <- tibble::tibble(
    measure = "Spearman correlation: confidence_score vs abs_diff",
    rho = unname(spearman_conf_abs_error$estimate),
    p_value = spearman_conf_abs_error$p.value
  )
} else {
  spearman_results <- NULL
}

### -------------------------------- ###
### 9) Optional: collect all outputs ###
### -------------------------------- ###

results_list <- list(
  eval_df = eval_df,
  confusion_matrix = conf_mat,
  confusion_matrix_df = conf_mat_df,
  accuracy_results = accuracy_results,
  kappa_results = kappa_results,
  disagreement_summary = disagreement_summary,
  per_class_metrics = per_class_metrics_with_macro,
  direction_summary_all = direction_summary_all,
  direction_summary_errors_only = direction_summary_errors_only,
  direction_by_magnitude = direction_by_magnitude,
  confidence_by_score = confidence_by_score,
  confidence_by_correctness = confidence_by_correctness,
  confidence_glm_results = confidence_glm_results,
  confidence_odds_ratio = conf_or,
  spearman_results = spearman_results
)

### -------------------------------- ###
### 10) Print key outputs            ###
### -------------------------------- ###

conf_mat
accuracy_results
kappa_results
disagreement_summary
per_class_metrics_with_macro
direction_summary_all
direction_summary_errors_only
direction_by_magnitude
confidence_by_score
confidence_by_correctness
confidence_glm_results
conf_or
spearman_results

# Now create relevant figures
plot_theme_auc <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )


cm_plot_df <- as.data.frame(conf_mat) %>%
  dplyr::rename(
    truth_label = Truth,
    auc_category = Prediction,
    n = Freq
  ) %>%
  dplyr::group_by(truth_label) %>%
  dplyr::mutate(
    row_total = sum(n),
    row_prop = n / row_total,
    cell_label = paste0(n, "\n(", scales::percent(row_prop, accuracy = 1), ")")
  ) %>%
  dplyr::ungroup()

p_confusion_heatmap <- ggplot(
  cm_plot_df,
  aes(x = auc_category, y = truth_label, fill = row_prop)
) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = cell_label), size = 4.2, fontface = "bold") +
  scale_fill_gradient(
    low = "#f7fbff",
    high = "#08519c",
    labels = scales::percent_format(accuracy = 1),
    name = "Row %"
  ) +
  labs(
    title = "Confusion Matrix",
    subtitle = "Cell labels show count and row percentage",
    x = "LLM prediction",
    y = "SME truth label"
  ) +
  plot_theme_auc

p_confusion_heatmap



severity_plot_df <- eval_df %>%
  dplyr::mutate(
    error_severity = dplyr::case_when(
      abs_diff == 0 ~ "Correct",
      abs_diff == 1 ~ "Off by 1",
      abs_diff == 2 ~ "Off by 2"
    ),
    error_severity = factor(
      error_severity,
      levels = c("Correct", "Off by 1", "Off by 2")
    )
  ) %>%
  dplyr::count(truth_label, error_severity, name = "n") %>%
  dplyr::group_by(truth_label) %>%
  dplyr::mutate(
    prop = n / sum(n),
    label = ifelse(prop >= 0.07, scales::percent(prop, accuracy = 1), "")
  ) %>%
  dplyr::ungroup()

p_error_severity <- ggplot(
  severity_plot_df,
  aes(x = truth_label, y = prop, fill = error_severity)
) +
  geom_col(width = 0.75, color = "white") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c(
      "Correct" = "#1b9e77",
      "Off by 1" = "#d95f02",
      "Off by 2" = "#7570b3"
    )
  ) +
  labs(
    title = "Prediction Error Severity by True Category",
    subtitle = "Within each truth category, cases are divided into correct, off by 1, and off by 2",
    x = "SME truth label",
    y = "Percent within truth category",
    fill = NULL
  ) +
  plot_theme_auc

p_error_severity

# Now divergent bar chart
direction_plot_df <- eval_df %>%
  dplyr::filter(signed_diff != 0) %>%
  dplyr::count(signed_diff, name = "n") %>%
  tidyr::complete(signed_diff = c(-2, -1, 1, 2), fill = list(n = 0)) %>%
  dplyr::mutate(
    error_group = dplyr::case_when(
      signed_diff == -2 ~ "More conservative by 2",
      signed_diff == -1 ~ "More conservative by 1",
      signed_diff ==  1 ~ "More permissive by 1",
      signed_diff ==  2 ~ "More permissive by 2"
    ),
    error_group = factor(
      error_group,
      levels = c(
        "More conservative by 2",
        "More conservative by 1",
        "More permissive by 1",
        "More permissive by 2"
      )
    ),
    direction = ifelse(signed_diff < 0, "More conservative", "More permissive"),
    prop = n / sum(n),
    signed_n = ifelse(signed_diff < 0, -n, n),
    text_y = ifelse(signed_n < 0, signed_n - 0.35, signed_n + 0.35),
    label = paste0(n, " (", scales::percent(prop, accuracy = 1), ")")
  )

p_direction_error <- ggplot(
  direction_plot_df,
  aes(x = error_group, y = signed_n, fill = direction)
) +
  geom_col(width = 0.72, color = "white") +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  geom_text(
    aes(y = text_y, label = label),
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "More conservative" = "#4daf4a",
      "More permissive" = "#e41a1c"
    )
  ) +
  labs(
    title = "Direction and Magnitude of Disagreement",
    subtitle = "Negative bars = LLM more conservative; positive bars = LLM more permissive",
    x = NULL,
    y = "Number of cases",
    fill = NULL
  ) +
  plot_theme_auc

p_direction_error

# Now look at confidence score vs. actual accuracy (sort of like a calibration plot)
confidence_acc_plot_df <- eval_df %>%
  dplyr::filter(!is.na(confidence_score)) %>%
  dplyr::group_by(confidence_score) %>%
  dplyr::summarise(
    n = dplyr::n(),
    correct = sum(exact_match),
    accuracy = mean(exact_match),
    .groups = "drop"
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    ci_low = stats::binom.test(correct, n)$conf.int[1],
    ci_high = stats::binom.test(correct, n)$conf.int[2],
    n_label = paste0("n=", n)
  ) %>%
  dplyr::ungroup()

p_confidence_accuracy <- ggplot(
  confidence_acc_plot_df,
  aes(x = confidence_score, y = accuracy)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3.5) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.12,
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = n_label),
    vjust = -1,
    size = 4,
    fontface = "bold"
  ) +
  scale_x_continuous(breaks = sort(unique(confidence_acc_plot_df$confidence_score))) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Observed Accuracy by LLM Confidence Score",
    subtitle = "Exact-match accuracy with exact binomial 95% confidence intervals",
    x = "Confidence score",
    y = "Exact-match accuracy"
  ) +
  plot_theme_auc

p_confidence_accuracy

