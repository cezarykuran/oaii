ui <- fluidPage(
  titlePanel("OpenAI API R implementation"),

  # resources ----
  shinyjs::useShinyjs(),
  includeScript(file.path("www", "widgets.js")),
  includeCSS(file.path("www", "widgets.css")),
  includeCSS(file.path("www", "app.css")),

  # tab content ----
  shiny::tabsetPanel(
    id = "menu",

    ## home ----
    tabPanel(
      icon("home"),
      passwordInput(
        "api_key",
        htmltools::tagList(
          "openai API key",
          htmltools::a(
            href = "https://platform.openai.com/account/api-keys",
            fontawesome::fa("arrow-up-right-from-square")
          ),
        ),
        value = Sys.getenv("API_KEY", unset = "")
      )
    ),

    ## chat ----
    tabPanel(
      "Chat",
      wellPanel(
        class = "well-sm",
        fluidRow(
          column(4, selectInput(
            "chatModel",
            tooltipLabel(
              "model",
              "ID of the model to use. See the model endpoint compatibility table for details on which models work with the Chat API."
            ),
            c("gpt-4", "gpt-4-0314", "gpt-4-32k", "gpt-4-32k-0314", "gpt-3.5-turbo", "gpt-3.5-turbo-0301")
          )),
          column(4, sliderInput(
            "chatN",
            tooltipLabel(
              "n",
              "How many chat completion choices to generate for each input message."
            ),
            1, 50, 1, 1
          )),
          column(4, sliderInput(
            "chatMaxTokens",
            tooltipLabel(
              "max_tokens",
              "The maximum number of tokens to generate in the chat completion."
            ),
            1, 1000, 200, 1
          )),
          column(4, sliderInput(
            "chatTemperature",
            tooltipLabel(
              "temperature",
              "What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic."
            ),
            0, 2, 0.7, 0.1
          )),
          # top_p
          # stream
          # stop
          column(4, sliderInput(
            "chatPresencePenalty",
            tooltipLabel(
              "presence_penalty",
              "Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics."
            ),
            -2, 2, 0, 0.1
          )),
          column(4, sliderInput(
            "chatFrequencyPenalty",
            tooltipLabel(
              "frequency_penalty",
              "Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim."
            ),
            -2, 2, 0, 0.1
          )),
          # logit_bias
          # user
        )
      ),
      dialogContainer("chatDialogContainer", TRUE, TRUE),
      wellPanel(
        class = "well-sm",
        textConsole("chatQ", "Chat console [Enter = send, Shift+Enter = new line]")
      )
    ),

    ## image generator ----
    tabPanel(
      "Image generator",
      wellPanel(
        class = "well-sm",
        fluidRow(
          column(6, sliderInput(
            "imgGenN",
            tooltipLabel(
              "n",
              "The number of images to generate."
            ),
            1, 10, 1, 1
          )),
          column(6, selectInput(
            "imgGenSize",
            tooltipLabel(
              "size",
              "The size of the generated images."
            ),
            c("256x256", "512x512", "1024x1024")
          ))
        ),
        textConsole(
          "imgGenPrompt",
          tooltipLabel(
            "Image description [Enter = send, Shift+Enter = new line]",
            "A text description of the desired image(s). The maximum length is 1000 characters."
          )
        )
      ),
      imagesContainer("imgGenContainer")
    ),

    ## image edit ----
    tabPanel(
      "Image edit",
      value = "image_edit",
      wellPanel(
        class = "well-sm",
        shiny::fluidRow(
          column(8,
            div(class = "text-center",
              htmltools::tags$label(
                class = "control-label",
                tooltipLabel(
                  "Edit uploaded image",
                  "Select area by your mouse that you want to edit."
                )
              )
            ),
            div(
              id = "imgEditCanvas",
              class = "oaii-imgEditCanvas",
              tags$canvas(),
              tags$canvas(),
              tags$script(
                "oaii.images.edit.init('imgEditCanvas', 'imgEditFileOut')"
              )
            )
          ),
          column(4,
            class = "shiny-input-container-fw",
            shiny::fileInput(
              "imgEditFileIn",
              tooltipLabel(
                "Source png",
                "Uload image you want to edit."
              )
            ),
            colourpicker::colourInput("imgEditColorBg", "Backgroud color", value = "#444"),
            colourpicker::colourInput("imgEditColorDraw", "Draw color", value = "#777"),
            sliderInput(
              "imgEditN",
              tooltipLabel(
                "n",
                "The number of images to generate."
              ),
              1, 10, 1, 1
            ),
            selectInput(
              "imgEditSize",
              tooltipLabel(
                "size",
                "The size of the generated images."
              ),
              c("256x256", "512x512", "1024x1024")
            ),
            textConsole(
              "imgEditPrompt",
              tooltipLabel(
                "Edit description [Enter = send, Shift+Enter = new line]",
                "A text description of the desired image(s). The maximum length is 1000 characters."
              )
            )
          )
        )
      ),
      imagesContainer("imgEditContainer")
    ),

    ## files ----
    tabPanel(
      "Files",
      wellPanel(
        class = "well-sm",
        fileInput("filesUpload", "Upload")
      ),
      shiny::dataTableOutput("filesTable")
    ),

    ## fine-tunes ----
    tabPanel(
      "Fine-tunes",
      wellPanel(
        class = "well-sm",
        fluidRow(
          column(4, selectInput("fineTunesTrainingFile", "training_file", choices = c())),
          column(4, selectInput("fineTunesModel", "model", c("ada", "babbage", "curie", "davinci"))),
          column(4, actionButton("fineTunesCreate", "Create"))
        )
      ),
      div(style = "overflow: auto; white-space: nowrap",
        shiny::dataTableOutput("fineTunesTable")
      )
    ),

    ## completions ----
    tabPanel(
      "Completions",
      wellPanel(
        class = "well-sm",
        fluidRow(
          column(4, selectInput(
            "completionsModel",
            tooltipLabel(
              "model",
              "ID of the model to use. See the model endpoint compatibility table for details on which models work with the Chat API."
            ),
            c("text-davinci-003", "text-davinci-002", "text-curie-001", "text-babbage-001", "text-ada-001")
          )),
          column(4, sliderInput(
            "completionsN",
            tooltipLabel(
              "n",
              "How many chat completion choices to generate for each input message."
            ),
            1, 50, 1, 1
          )),
          column(4, sliderInput(
            "completionsMaxTokens",
            tooltipLabel(
              "max_tokens",
              "The maximum number of tokens to generate in the chat completion."
            ),
            1, 1000, 200, 1
          )),
          column(4, sliderInput(
            "completionsTemperature",
            tooltipLabel(
              "temperature",
              "What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic."
            ),
            0, 2, 0.7, 0.1
          )),
          # top_p
          # stream
          # stop
          column(4, sliderInput(
            "completionsPresencePenalty",
            tooltipLabel(
              "presence_penalty",
              "Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics."
            ),
            -2, 2, 0, 0.1
          )),
          column(4, sliderInput(
            "completionsFrequencyPenalty",
            tooltipLabel(
              "frequency_penalty",
              "Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim."
            ),
            -2, 2, 0, 0.1
          )),
          # logit_bias
          # user
        )
      ),
      dialogContainer("completionsDialogContainer"),
      wellPanel(
        class = "well-sm",
        textConsole("completionsPrompt", "Completions console [Enter = send, Shift+Enter = new line]")
      )
    )
  ),

  # tooltips ignite ----
  tooltipIgnite()
)
