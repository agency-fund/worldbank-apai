library(plumber)
library(tidyverse)
library(httr2)

claude_req <- function(content, endpoint = "messages", system_prompt=NULL,
                       query_prompt = "Derive a policy recommendation drawing from the documents in the provided content.",
                       max_tokens = 1024) {
  
  json_body <- list(
    model = "claude-3-5-sonnet-20241022",
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
      `anthropic-version` = "2023-06-01"
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
    # unnest_wider(where(~ is.list(.x)), names_sep = "_") |>
    # unnest_wider(where(~ is.list(.x)), names_sep = "_") |>
    # unite("authors", starts_with("authors_"), sep = ", ") |>
    # mutate(authors = str_remove_all(authors, ", NA")) |>
    # unite("keywords", starts_with("keywd_"), sep = ", ") |>
    # mutate(keywords = str_remove_all(keywords, ", NA")) |>
    filter(!is.na(txturl)) |>
    mutate(txturl_resp = map(txturl, ~ request(.x) |> req_perform())) |>
    # mutate(document = map_chr(txturl_resp, ~ .x |> resp_body_string())) |>
    mutate(txturl_body = map(txturl_resp, ~ .x |> resp_body_raw())) |>
    mutate(document = map_chr(txturl_body, ~ .x |> rawToChar() |> str_conv("UTF-8"))) |>
    mutate(txturl_code = map_int(txturl_resp, ~ .x |> resp_status())) |>
    filter(txturl_code == 200) %>%
    select(display_title, txturl, document)
  
  return(wb_data)
}

doc_context <- function(docs, num_docs = NULL) {
  if (!is.null(num_docs)) {
    docs = docs[1:num_docs]
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
    response = claude_res$response$content[[1]]$text[[1]],
    sources = docs |> select(-document) |> head(num_docs)
  ))
}