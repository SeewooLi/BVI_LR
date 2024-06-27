library(openai)
library(glue)
library(jsonlite)

get_completion <- function(prompt, model = "gpt-4o", temperature = 0){
  messages <- list(
    list(
      "role" = "system",
      "content" = "You are a helpful assistant having an expertise in conducting literature review."
    ),
    list(
      "role" = "user",
      "content" = prompt
    )
  )
  res <- openai::create_chat_completion(
    model = model,
    messages = messages,
    temperature = temperature
  )
  message(paste("Used tokens:", res$usage$prompt_tokens, "+", res$usage$completion_tokens, "=",  res$usage$total_tokens))
  message(paste("Price: $", res$usage$prompt_tokens/1000000*5+res$usage$completion_tokens/1000000*15))
  return(res$choices$message.content)
}

recommend_search_terms <- function(purpose){
  prom <- glue(
    "
The purpose of the current literature review is to {purpose}. Generate search words to retrieve bibliometric data from websites such as Web of Science.

Specifications:
  1. The search words should include all resources aligned with the purpose, while effectively excluding irrelevant resources.

Provide the result in JSON format without delimiters, heading, and explanations.
"
  )

  response <- get_completion(prom)

  return(jsonlite::fromJSON(response))
}

literature_review <- function(r_questions, bib_data,
                              structure = "'Present Situation', 'Challenges', and 'Prospects'"){

  structure2 <- if(is.null(structure)) "" else paste('\n    B. Use', structure, 'as the top-level structure.')

  prom <- glue(
    "
The purpose of the current literature review is to answer the 'Research Questions' below. Generate a structured summary using the 'Bibliographic Data' and by following the specifications below.

SPECIFICATION:
  1. Considering both 'Research Questions' and 'Bibliometric Data', determine the structure of summary that you think is most appropriate.
    A. While flexibly determining the structure of summary, exclude 'Introduction' and 'Conclusion', and include 'Reference' in APA style.{structure2}
  2. Use APA style for citations (e.g., Author, Year). However, avoid using author(s) as a subject of a sentence. Use '(Author, Year)' at the end of the sentence if possible.
  3. If there is no resource provided from 'Bibliometric Data' to be summarized under a particular heading, include the heading for the integrity of the structure and simply print the content as 'NA'.
  4. NEVER make up the content to make a pausible result. The transparency and credibility are integral parts of the literature review.
    A. You will be penalized if the generated literature review is fallacious, flawed, or is not based on the given data.
  5. NEVER evaluate or interpret abstracts.
    A. Abstracts are well-written summary of the studies. While, of course, you need to evaluate or interpret abstracts when understanding abstracts, NEVER make your own conclusion on implications, limitations, future usages of studies.
  6. If a statement needs to be double-checked by manually reading the full paper, for example due to a lack of clarity of the abstract, italicize the statement.
  7. If an article is used and cited in the literature review more than once, include them in the Reference. Otherwise, simply exclude them from the Reference.
  8. Maximize the usages of the given data.
  9. Follow the markdown syntax in generating the final output.

Research Questions:{r_questions}

Bibliographic Data:
{example_data}

Provide only the summary as an output.
"
  )

  response <- get_completion(prom)

  return(response)
}
