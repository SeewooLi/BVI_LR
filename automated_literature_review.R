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
  5. NEVER evaluate or interpret abstracts on your own.
    A. Abstracts are well-written summaries of the studies. While, of course, you need to evaluate or interpret abstracts when understanding abstracts, NEVER make your own conclusion on implications, limitations, future usages of studies.
  6. If a statement needs to be double-checked by manually reading the full paper, for example due to a lack of clarity of the abstract, italicize the statement.
  7. Once an article is used and cited, include it in the Reference. Otherwise, simply exclude it from the Reference.
  8. Maximize the usages of the given data. The output should be lengthy if the 'Bibliometric Data' is voluminous or there are many research questions.
  9. Follow the markdown syntax in generating the final output.

Research Questions:
```{r_questions}
```

Bibliographic Data:
{example_data}

Provide only the summary as an output.
"
  )

  response <- get_completion(prom)

  return(response)
}


merge_results <- function(results){
  merged_lr <- ""

  for(i in 1:length(results)){
    merged_lr <- paste0(
      merged_lr,
      glue(
"
# ***INPUT {i}:***
{results[[i]]}

---

"
      )
    )
  }


  prom <- glue(
    "
ROLE: Merge the multiple inputs delimited by triple backticks.

SPECIFICATION:
  1. Determine the structure of the merged result that you think is most appropriate.
    A. While flexibly determining the structure, exclude 'Introduction', 'Conclusion', and 'Reference'.
  2. The merged result must retain all information provided in the inputs.
  3. If there is no resource provided from the input to be summarized under a particular heading, include the heading for the integrity of the structure and simply print the content as 'NA'.
  4. NEVER make up the content to make a pausible result. The transparency and credibility are integral parts of this process.
    A. You will be penalized if the generated result is fallacious, flawed, or is not based on the given input.
  5. NEVER evaluate or interpret the input on your own.
    A. The inputs are well-written summaries of studies. While, of course, you need to evaluate or interpret the inputs to understand them, NEVER make your own conclusion on implications, limitations, future usages of studies.
  4. Each input starts with a header, where the header is '# ***INPUT n:***' for the nth input.
  5. Each input ends with '---'.
  6. Exclude the reference part at the end of each input, while retaining inline citations.
  7. Follow the markdown syntax in generating the final output.

INPUTS:
```
{merged_lr}
```

Provide only the markdown-formatted output.
    "
  )
  response <- get_completion(prom)

  return(response)
}


labelling <- function(r_questions, bib_data){
  format <- '
{
  "ANSWER1":"The answer to the first question.",
  "ANSWER2":"The answer to the second question.",
  ...
  "ANSWERn":"The answer to the nth question."
}'

  prom <- glue(
    "
Role: Providing brief answers to multiple questions on an academic article. Based on a title and an abstract of the article, provide answers to the questions with less than 10 words per question. The answers should be as concise as possible. Generate the output using the following format. If a question cannot be answered by the given title and abstract of the article, print 'N/A'.

Article:
{bib_data}

Questions:
{r_questions}

Format:
```{format}
```

Provide the result in JSON format without delimiters, heading, and explanations.
"
  )

  response <- get_completion(prom)

  return(response)
}
