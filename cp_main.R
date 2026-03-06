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

setwd(dir = "/phi/sbi/chest_pain/cp_github_files/")

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

key_df <- read_csv(file = "/phi/sbi/chest_pain/cp_key.csv")

api_key <- key_df$key
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
  if (is.na(txt) || !nzchar(txt)) return(NULL)
  tryCatch(
    fromJSON(txt, simplifyVector = FALSE),
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
prompt_text <- "You are a clinical reasoning assistant with expertise in pediatric cardiology. Your task is to read outpatient clinical notes from pediatric cardiology visits for patients whose chief complaint is chest pain. You will use these notes to determine whether an echocardiogram is Appropriate, May Be Appropriate, or Rarely Appropriate based on the 2014 Appropriate Use Criteria (AUC) for Initial Transthoracic Echocardiography in Outpatient Pediatric Cardiology.
You should identify key symptoms, family history elements, physical exam findings, and ECG findings when present.
You must classify the note using the following categories:

a. Appropriate
  i. Exertional chest pain
  ii. Non-exertional chest pain with abnormal ECG
  iii. Chest pain with family history of sudden unexplained death or cardiomyopathy.

b. May Be Appropriate
  i. Chest pain with other symptoms or signs of cardiovascular disease, a benign family history, and a normal ECG
  ii. Chest pain with family history of premature coronary artery disease
  iii. Chest pain with recent onset of fever
  iv. Chest pain with recent illicit drug use

c. Rarely Appropriate
  i. Chest pain with no other symptoms or signs of cardiovascular disease, a benign family history, and a normal ECG
  ii. Non-exertional chest pain with no recent ECG
  iii. Non-exertional chest pain with normal ECG
  iv. Reproducible chest pain with palpation or deep inspiration

You must provide a brief rationale (one to two sentences) explaining why you chose that category. Your rationale should refer directly to specific findings described in the note. Also list the specific phrases from the note that support your conclusion. You must assign a confidence score from 1 (low confidence) to 5 (complete confidence).

The following is clarification for some of the phrases in the appropriate use criteria above:
1.	Other symptoms or signs of cardiovascular disease

Examples of this could include:
  i. Symptoms of cardiovascular disease such as (but not limited to) exertional syncope or presyncope, exertional dyspnea out of proportion to peers, palpitations, or orthopnea
  ii. Abnormal vital signs indicative of heart disease such as (but not limited to) unexplained tachycardia or hypotension for age
  iii. Abnormal cardiovascular exam such as (but not limited to) a pathologic murmur, gallop, hepatomegaly, rales, or peripheral edema

2.	Recent onset of fever

Treat as present if the note documents subjective or objective fever ≥ 38.0°C (100.4°F) that started within the past 14 days
Examples include phrases like:
  i. Fever to 102°F yesterday with chest pain.
  ii. Several days of fever this week and now chest pain.
  iii. Viral illness with fever over the 2 weeks.

3.	Premature coronary artery disease in the family

Treat as present if there is a history in a first-degree relative (parent, sibling) of:
  i. Myocardial infarction, coronary stent, or coronary artery bypass grafting (CABG) before age 55 in males or before age 65 in females, or
  ii. Sudden cardiac death due to presumed myocardial ischemia in those age ranges.

Examples include phrases like:
  i. Father had an MI at 48 and needed a stent.
  ii. Mother had bypass surgery at 52.
  iii. Dad died suddenly of a heart attack in his early 50s.

Return your answer using the required structured output fields.
For supporting_phrases, provide an array of direct quotes copied verbatim from the note.
For rationale, provide 1–2 sentences.
For confidence_score, return an integer from 1 to 5.
"




df <- read_excel("/phi/sbi/chest_pain/sample_cp.xlsx") |>
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
      function(patient_id, mrn, visitage, sex, chiefcomp, visitdiagnc, ecg_yn, ecg_summary, extracted_note) {
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
# Optionally submit requests and save outputs
# -------------------------------

run_api_requests <- TRUE

# if (run_api_requests) {
#   if (!nzchar(api_key)) {
#     stop("AZURE_OPENAI_API_KEY is empty. Set it in your environment before enabling run_api_requests.")
#   }
#
#   raw_results <- requests_tbl |>
#     transmute(custom_id, patient_id, response = map(body, \(b) {
#       req <- request(api_url) |>
#         req_method("POST") |>
#         req_headers(`api-key` = api_key, `Content-Type` = "application/json") |>
#         req_body_json(b, auto_unbox = TRUE)
#
#       resp <- req_perform(req)
#       resp_body_json(resp, simplifyVector = FALSE)
#     }))
#
#   parsed_results <- raw_results |>
#     mutate(
#       output_text = map_chr(response, extract_response_text),
#       parsed = map(output_text, parse_model_json),
#       auc_category = map_chr(parsed, ~ .x$auc_category %||% NA_character_),
#       rationale = map_chr(parsed, ~ .x$rationale %||% NA_character_),
#       supporting_phrases = map_chr(parsed, ~ {
#         phrases <- .x$supporting_phrases
#         if (is.null(phrases)) return(NA_character_)
#         if (is.character(phrases)) return(paste(phrases, collapse = " | "))
#         NA_character_
#       }),
#       confidence_score = map_dbl(parsed, ~ as.numeric(.x$confidence_score %||% NA_real_))
#     ) |>
#     select(patient_id, auc_category, rationale, supporting_phrases, confidence_score, output_text)
#
#   write.csv(parsed_results, results_csv_path, row.names = FALSE, na = "")
#   writeLines(
#     map_chr(raw_results$response, ~ toJSON(.x, auto_unbox = TRUE, null = "null")),
#     con = results_raw_json_path,
#     useBytes = TRUE
#   )
#
#   message("Saved parsed results to: ", results_csv_path)
#   message("Saved raw JSON responses to: ", results_raw_json_path)
# } else {
#   message("run_api_requests is FALSE. JSONL was generated but API calls were skipped.")
# }

##### QC to understand what's going wrong #####

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
      auc_category = map_chr(parsed, ~ .x$auc_category %||% NA_character_),
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
    select(
      patient_id,
      auc_category,
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





