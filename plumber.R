library(plumber)
library(stringr)
library(httr2)
library(tidyr)
library(dplyr)
library(purrr)

claude_req <- function(content, endpoint = "messages", system_prompt=NULL,
                       query_prompt = "Derive a policy recommendation drawing from the documents in the provided content.",
                       max_tokens = 1024, model = "claude-3-5-haiku-latest",
                       api_version = "2023-06-01") {
  json_body <- list(
    model = model,
    messages = list(
      list(role = "user",
           content = str_glue(
             "<content>{content}</content>{query_prompt}"))
    )
  )
  if (!is.null(system_prompt)) {
    json_body$system = system_prompt
  }
  if (endpoint == "messages") {
    json_body$max_tokens <- max_tokens
  } else if (endpoint == "messages/token_count") {
    
  }
  json_resp <- request(str_glue("https://api.anthropic.com/v1/{endpoint}")) |>
    req_headers(
      `x-api-key` = readLines("anthropic_api_key.txt"),
      `content-type` = "application/text",
      `anthropic-version` = api_version
    ) |>
    req_body_json(
      json_body
    ) |>
    req_perform() |>
    resp_body_json()
  return(json_resp)
}

fetch_wb_docs <- function(query = "psychology interventions", num_results = 5) {
  wb_data <- request("https://search.worldbank.org/api/v3/wds") |>
    req_url_query(rows = num_results, os = 0, qterm = query) |>
    req_perform() |>
    resp_body_json() |>
    as_tibble() |>
    unnest_wider(documents) |>
    filter(!is.na(txturl)) |>
    mutate(txturl_resp = map(txturl, ~ request(.x) |> req_perform())) |>
    mutate(txturl_body = map(txturl_resp, ~ .x |> resp_body_raw())) |>
    mutate(document = map_chr(txturl_body, ~ .x |> rawToChar() |> str_conv("UTF-8"))) |>
    mutate(txturl_code = map_int(txturl_resp, ~ .x |> resp_status())) |>
    filter(txturl_code == 200) |>
    select(display_title, txturl, document)
  return(wb_data)
}

doc_context <- function(docs, num_docs = NULL, strip_references = TRUE,
                        refs_thresh = 0.4) {
  if (!is.null(num_docs)) {
    docs <- docs[1:num_docs]
  }
  for (i in 1:length(docs)) {
    ref_idx <- str_locate_all(docs[i], "References")[[1]]
    
    if (nrow(ref_idx) > 0) {
      last_ref_idx <- ref_idx[nrow(ref_idx), 1]
      ref_pct <- last_ref_idx / str_length(docs[i])
      if (ref_pct > refs_thresh) {
        docs[i] <<- str_sub(docs[i], 1, last_ref_idx - 1)
      }
    }
  }
  str_c(
    "<document>",
    str_c(docs, collapse = "</document><document>"),
    "</document>"
  )
}

#* @apiTitle Ask the World Bank
#* @apiDescription Provide policy advice over World Bank documents.

#* Provide policy advice over World Bank documents.
#* @param query:str The query for World Bank documents.
#* @param num_docs:int The number of documents to retrieve.
#* @param query_prompt:str Prompt for the retrieved documents.
#* @param system_prompt:str Role of the responder.
#* @get /advice
#* @serializer json
function(query = "social psychology interventions", num_docs = 10,
         query_prompt = "Derive a policy recommendation drawing from the documents in the provided content.",
         system_prompt = "You are a World Bank task team leader (TTL).") {
  num_docs <- as.numeric(num_docs)
  docs <- fetch_wb_docs(query, num_results = num_docs)
  context <- doc_context(docs$document, num_docs)
  token_count <- claude_req(context, endpoint = "messages/count_tokens")$input_tokens
  while (token_count > 2e5) {
    num_docs <- num_docs - 1
    context <- doc_context(docs$document, num_docs)
    token_count <- claude_req(context, endpoint = "messages/count_tokens")$input_tokens
  }
  claude_res <- claude_req(context, query_prompt = query_prompt,
                           system_prompt = system_prompt)
  return(list(
    #response = claude_res$response$content[[1]]$text[[1]],
    response = claude_res,
    sources = docs |> select(-document) |> head(num_docs)
  ))
}

