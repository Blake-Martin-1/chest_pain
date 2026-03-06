# Code to load chest pain clinical note files, create a JSONL file, submit to Azure OpenAI,
# and store returned results.

suppressPackageStartupMessages({
  library(readxl)
  library(jsonlite)
  library(httr2)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

# -------------------------------
# Configuration
# -------------------------------
excel_path <- "/phi/sbi/chest_pain/sample_cp.xlsx"
jsonl_output_path <- "cp_requests.jsonl"
results_csv_path <- "cp_results.csv"
results_raw_json_path <- "cp_results_raw.json"

api_url <- Sys.getenv(
  "AZURE_OPENAI_RESPONSES_URL",
  unset = "https://researchinformatics-che-resource.cognitiveservices.azure.com/openai/responses?api-version=2025-04-01-preview"
)
api_key <- Sys.getenv("AZURE_OPENAI_API_KEY", unset = "")
model_name <- Sys.getenv("AZURE_OPENAI_DEPLOYMENT", unset = "cp_5.2")

# Set TRUE to submit each note to the API.
run_api_requests <- FALSE

# -------------------------------
# Helpers
# -------------------------------
read_prompt_file <- function() {
  if (file.exists("init_prompt.txt")) {
    return(readr::read_file("init_prompt.txt"))
  }

  if (file.exists("init_prompt.rtf")) {
    raw_rtf <- paste(readLines("init_prompt.rtf", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    # Minimal RTF-to-text cleanup for prompt use.
    txt <- raw_rtf |>
      str_replace_all("\\\\par[d]?", "\n") |>
      str_replace_all("\\\\'[0-9a-fA-F]{2}", "") |>
      str_replace_all("\\\\[a-zA-Z]+-?[0-9]* ?", "") |>
      str_replace_all("[{}]", "") |>
      str_squish()
    return(txt)
  }

  stop("Could not find init_prompt.txt or init_prompt.rtf in the working directory.")
}

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

build_user_payload <- function(row) {
  paste0(
    "Patient metadata:\n",
    "- patient_id (csn): ", row$patient_id, "\n",
    "- mrn: ", row$mrn, "\n",
    "- age_years: ", row$visitage, "\n",
    "- sex: ", row$sex, "\n",
    "- chief_complaint: ", row$chiefcomp, "\n",
    "- visit_diagnosis: ", row$visitdiagnc, "\n",
    "- ecg_performed: ", row$ecg_yn, "\n",
    "- ecg_summary: ", row$ecg_summary, "\n\n",
    "Clinical note:\n",
    row$extracted_note, "\n\n",
    "Return ONLY valid JSON with keys exactly: ",
    "patient_id, auc_category, rationale, supporting_phrases, confidence_score. ",
    "supporting_phrases must be an array of direct quotes from the note."
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
  if (is.na(txt) || !nzchar(txt)) return(list())

  parsed <- tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(parsed)) return(parsed)

  # Fallback: extract first JSON object from free text.
  snippet <- str_extract(txt, "\\{[\\s\\S]*\\}")
  if (is.na(snippet)) return(list())

  tryCatch(fromJSON(snippet, simplifyVector = FALSE), error = function(e) list())
}

# -------------------------------
# Load data
# -------------------------------
prompt_text <- read_prompt_file()

df <- read_xlsx(path = excel_path) |>
  normalize_colnames()

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
      list(patient_id, mrn, visitage, sex, chiefcomp, visitdiagnc, ecg_yn, ecg_summary, extracted_note),
      \(patient_id, mrn, visitage, sex, chiefcomp, visitdiagnc, ecg_yn, ecg_summary, extracted_note) {
        row <- list(
          patient_id = patient_id,
          mrn = mrn,
          visitage = visitage,
          sex = sex,
          chiefcomp = chiefcomp,
          visitdiagnc = visitdiagnc,
          ecg_yn = ecg_yn,
          ecg_summary = ecg_summary,
          extracted_note = extracted_note
        )

        list(
          model = model_name,
          input = list(
            list(role = "system", content = prompt_text),
            list(role = "user", content = build_user_payload(row))
          ),
          temperature = 0
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
# Optionally submit requests and save outputs
# -------------------------------
if (run_api_requests) {
  if (!nzchar(api_key)) {
    stop("AZURE_OPENAI_API_KEY is empty. Set it in your environment before enabling run_api_requests.")
  }

  raw_results <- requests_tbl |>
    transmute(custom_id, patient_id, response = map(body, \(b) {
      req <- request(api_url) |>
        req_method("POST") |>
        req_headers(`api-key` = api_key, `Content-Type` = "application/json") |>
        req_body_json(b, auto_unbox = TRUE)

      resp <- req_perform(req)
      resp_body_json(resp, simplifyVector = FALSE)
    }))

  parsed_results <- raw_results |>
    mutate(
      output_text = map_chr(response, extract_response_text),
      parsed = map(output_text, parse_model_json),
      auc_category = map_chr(parsed, ~ .x$auc_category %||% NA_character_),
      rationale = map_chr(parsed, ~ .x$rationale %||% NA_character_),
      supporting_phrases = map_chr(parsed, ~ {
        phrases <- .x$supporting_phrases
        if (is.null(phrases)) return(NA_character_)
        if (is.character(phrases)) return(paste(phrases, collapse = " | "))
        NA_character_
      }),
      confidence_score = map_dbl(parsed, ~ as.numeric(.x$confidence_score %||% NA_real_))
    ) |>
    select(patient_id, auc_category, rationale, supporting_phrases, confidence_score, output_text)

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
