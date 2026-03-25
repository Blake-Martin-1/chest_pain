# Code to load chest pain clinical note files, create a JSONL file, submit to Azure OpenAI,
# and store returned results.

suppressPackageStartupMessages({
  library(readxl)
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
Any chest pain occurring or worse during physical exertion, even if it occurred only once or has resolved.

2. Non-exertional chest pain with abnormal ECG
ECG rules
- If ECG interpretation says 'abnormal' or 'borderline', classify as abnormal regardless of specific findings.
- If ECG interpretation says 'normal' or 'otherwise normal', classify as normal.
- If the HPI describes a previous abnormal ECG, count it as abnormal.
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
  con = "phi/sbi/chest_pain/cp_github_files/prompt_v01.txt",
  useBytes = TRUE
)



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


df <- df_full[1:20, ] # 1:20 training set, 21:42 validation, 42:end as test set?


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

