#' Start Interactive Chatbot Session
#'
#' This function initiates an interactive chatbot session in the R console using the ellmer package.
#' The user can input prompts, and the AI will respond accordingly.
#'
#' @param model Character string specifying the OpenAI model to use (default is "gpt-4o").
#' @param system_prompt Character string specifying the system prompt to guide the AI's behavior.
#' @return None. This function runs an interactive session in the console.
#' @export
#' @examples
#' \dontrun{
#' fusionAI()
#' }


fusionAI <- function() {

  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("The 'ellmer' package is required but not installed. Please install it using install.packages('ellmer').")
  }

  # Build the local dictionary
  d <- dictionary()
  d <- truncate(d, max_char = 300)

  # Restrict columns for testing (maybe not helpful?)
  dtext <- df2text(select(d, -values, -type))

  # Construct the system prompt
  sys.prompt <- paste("You are the AI assistant for the fusionACS R package (https://ummel.github.io/fusionACS/). The package helps users access and analyze the U.S. survey microdata made available by the fusionACS project (https://www.nature.com/articles/s41597-023-02788-7).",
                      "The fusionACS project simulates how respondents to the American Community Survey (ACS) might have answered the questionnaire of unrelated surveys. This allows variables from disconnected surveys to be assembled into a single microdata product. The microdata consists of actual ACS-PUMS microdata (household or person records) plus additional columns for variables “fused” from non-ACS “donor” surveys.",
                      "The full set of available fusionACS survey variables are described in the following metadata.",
                      dtext,
                      "Your job is to help the user decide which of the available variables (‘variables’ in the metadata) are needed to address the research question. You have complete knowledge of the fusionACS metadata, but the user does not. Therefore, you will need to converse with the user to understand the research question, possibly seek clarification, and suggest which of the available variables are most appropriate.",
                      "You must also decide on appropriate ‘year’ input values to indicate the time period of the microdata (examples: 2019 or 2015:2019). The ‘acs_years’ column in the metadata indicates the year(s) for which a variable is available. All of the proposed variables must be available for all of the ‘year’ input values. This may affect which combinations of variables it is possible to use. Any temporal information (e.g. years) in the metadata ‘description’ column should be ignored.",
                      "You must also decide if the analysis should use ‘household’ or ‘person’ microdata observations (‘respondent’ in the metadata). In general, you should use the respondent type associated with the research question’s primary variable(s) of interest.",
                      #"You are provided with a tool – checkAssemble – that you can execute to confirm if the proposed variables, years, and respondent type are valid given the available fusionACS data.",
                      #"You are provided with a tool – assemble – that you can execute to confirm if the proposed variables, years, and respondent type are valid given the available fusionACS data.",
                      "Once the user has confirmed a final selection of valid inputs for variables, year, and respondent, you will return the response: ‘Inputs complete.’",
                      sep = "\n")

  # chat <- ellmer::chat_gemini(
  #   model = "gemini-2.0-flash",
  #   system_prompt = sys.prompt,
  #   echo = "none"
  # )

  chat <- ellmer::chat_openai(
    model = "gpt-4o-mini",
    system_prompt = sys.prompt,
    echo = "none"
  )

  #---

  # Register assemble() tool
  # ellmer::create_tool_def(assemble)
  chat$register_tool(
    tool(
      assemble,
      "A function to assemble fusionACS survey microdata, given a user's requested variable(s), year(s), and respondent type. It also enables modification of the microdata via arbitrary expressions passed to mutate(), filter(), and select() internally.",
      variables = type_array(
        "Character vector specifying the names of survey variables.",
        items = type_string()
      ),
      year = type_array(
        "Numeric vector specifying the year(s) of ACS-PUMS microdata to use.",
        items = type_number()
      ),
      respondent = type_string(
        "Ether 'household' or 'person' to indicate the desired microdata respondent type."
      ),
      path = type_string(
        "The directory path to the local fusionACS database.",
        required = FALSE
      ),
      cores = type_integer(
        "The number of cores to be used for processing.",
        required = FALSE
      )
    )
  )

  # TESTING -- what if AI has its own function that takes 'expressions' argument?
  library(rlang)
  f0 <- function(...) enquos(...)

  f <- function(expr_strings) {
    env <- parent.frame()

    quosures <- lapply(expr_strings, function(expr_string) {
      expr <- parse(text = expr_string)[[1]]

      # If the expression is a call with `=`, treat the LHS as a name
      if (is.call(expr) && identical(expr[[1]], as.name("="))) {
        name <- as.character(expr[[2]])
        value_expr <- expr[[3]]
        quo <- new_quosure(value_expr, env)
        attr(quo, "name") <- name  # temporarily attach name
        return(quo)
      } else {
        # Unnamed expression
        return(new_quosure(expr, env))
      }
    })

    # Add names where we found them
    names(quosures) <- sapply(quosures, function(q) attr(q, "name", exact = TRUE) %||% "")
    quosures[names(quosures) == ""] <- unname(quosures[names(quosures) == ""])

    return(quosures)
  }

  # Examples test
  f0(x = y + 1, z > 5)
  f(c("x = y + 1", "z > 5"))

  #----

  # Register checkAssemble() tool
  # ellmer::create_tool_def(checkAssemble)
  # chat$register_tool(tool(
  #   checkAssemble,
  #   "Takes proposed inputs for microdata assembly - variables and year - and determines if the inputs are valid given the available microdata. If the proposed inputs are valid, returns a list with named slots 'variables' and 'year'. If the proposed inputs are NOT valid, returns a character string indicating what is wrong.",
  #   variables = type_array(
  #     "Character vector specifying the names of survey variables.",
  #     items = type_string()
  #   ),
  #   year = type_array(
  #     "Numeric vector specifying the year(s) of ACS-PUMS microdata to use.",
  #     items = type_number()
  #   ),
  #   respondent = type_string(
  #     "Ether 'household' or 'person' to indicate the desired microdata respondent type."
  #   )
  # ))

  # Register assembleAI() tool
  # ellmer::create_tool_def(assembleAI)
  # chat$register_tool(tool(
  #   assembleAI,
  #   "Assembles microdata from a local fusionACS database, given requested variable(s), year(s), and respondent type. Returns a temporary file path to a fst file contaning the assembled microdata.",
  #   variables = type_array(
  #     "Character vector specifying the names of survey variables.",
  #     items = type_string()
  #   ),
  #   year = type_array(
  #     "Numeric vector specifying the year(s) of ACS-PUMS microdata to use.",
  #     items = type_number()
  #   ),
  #   respondent = type_string(
  #     "Ether 'household' or 'person' to indicate the desired microdata respondent type."
  #   )
  # ))

  #---

  # Setup the chat in a live browser?
  #live_browser(chat)

  #---

  # TEST
  #chat$chat("I want to study how electricity consumption varies across space within the state of Texas")


  # x <- "/tmp/RtmpbVAR4p/file448f2f1ea002.fst"
  # test <- fst::read_fst(x)

  # Check if the last turn invoked a tool call
  # x <- chat$last_turn(role = "assistant")
  # "ellmer::ContentToolRequest"


  #---

  cat("fusionAI: Hello! How can I assist you today?\n")
  chat$chat("I want to study how electricity consumption varies across space within the state of Texas")
  repeat {

    user_input <- readline(prompt = "You: ")

    # Close chat if user requests it
    if (tolower(user_input) %in% c("exit", "quit", "done")) {
      cat("fusionAI: Goodbye!\n")
      break
    }

    # Is this really necessary?
    # Attempt to interpret anything in the prompt between <> as R code
    # evals <- unlist(regmatches(user_input, gregexpr("<([^>]+)>", user_input)))
    # if (length(evals)) {
    #   for (x in evals) {
    #     u <- try(eval(parse(text = gsub("^.|.$", "", x))), silent = TRUE)
    #     if (inherits(u, "try-error")) {
    #       warning("Failed to evaluate youselectionr code (inside <>): ", x, ". That part of the prompt was submitted without modification.", immediate. = TRUE)
    #     } else {
    #       user_input <- sub(x, u, user_input)
    #     }
    #   }
    # }

    # Get AI's response to user input
    response <- chat$chat(user_input)

    # Check if there was successful tool evaluation during the user's last turn
    #x <- chat$last_turn(role = "user")
    x <- chat$last_turn(role = "user")@contents
    ctr <- sapply(x, inherits, what = "ellmer::ContentToolResult")
    if (any(ctr)) {
      x <- x[[ctr]]
      inputs <- x@value
      cat("fusionAI: Assembling the requested microdata...\n")
      md <- assemble(variables = unlist(inputs$variables),
                     year = unlist(inputs$year),
                     respondent = inputs$respondent)
      assign("my.data", md, envir = globalenv())
      cat("fusionAI: The assembled microdata is available in a data.table called 'my.data'. This is what it looks like:\n\n")
      print(head(md))
    } else {
      cat("fusionAI:", response, "\n")
    }
  }
}

#------------------

# Truncate the cells of a data frame to some maximum number of characters
truncate <- function(df, max_char = 200) {
  note <- "...[TRUNCATED]"
  mx <- max_char - nchar(note)

  # If there are any list columns, convert to text
  for (i in which(sapply(df, inherits, what = "list"))) {
    data.table::set(df, j = i, value = sapply(df[[i]], paste, collapse = " "))
  }

  for (i in 1:ncol(df)) {
    x <- df[[i]]
    z <- nchar(x, keepNA = FALSE) > mx
    if (any(z)) {
      x[z] <- paste0(substring(x[z], 1, mx), note)
      data.table::set(df, j = i, value = x)
    }
  }
  return(df)
}

# Function to convert data.frame to text representation that can be passed to LLM
df2text <- function(df) {

  # If there are any list columns, convert to text
  for (i in which(sapply(df, inherits, what = "list"))) {
    data.table::set(df, j = i, value = sapply(df[[i]], paste, collapse = " "))
  }

  # Create a text connection to capture the output
  con <- textConnection("data_text", "w", local = TRUE)

  # Write the data frame to the text connection with a custom separator
  write.table(df, file = con, sep = "|", row.names = FALSE, col.names = TRUE, quote = FALSE)

  # Close the text connection
  close(con)

  # Interpretation prompt for the LLM:
  prompt <- paste0("The following tabular data uses '|' as a separator. The first line is a header containing column names. There are ", ncol(df), " total columns. Each subsequent line contains a single row of data (observation). There are ", nrow(df), " total observations. Interpret accordingly. Here is the data:")

  # Combine the captured lines into a single string
  data_string <- paste(c(prompt, data_text), collapse = "\n")

  return(data_string)

}

# #---
#
# # EXAMPLE
# df <- dictionary()
# df <- truncate(df, max_char = 250)
# sapply(df, function(x) max(nchar(x)))
#
# df.text <- df2text(df)
#




