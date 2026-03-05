# Codex test
### Code to load in chest pain clinical note files, create jsonl file, send to OpenAI for analysis via API, and store
### the returned results

df <- read_xlsx(path = "/phi/sbi/chest_pain/sample_cp.xlsx")