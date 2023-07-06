ui <- fluidPage(
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
      value = "home",
      wellPanelSm(
        passwordInput(
          "api_key",
          "OpenAI API key",
          value = Sys.getenv("API_KEY", unset = "")
        ),
        htmltools::tagList(
          "The OpenAI API uses API keys for authentication. Visit your",
          htmltools::a(
            href = "https://platform.openai.com/account/api-keys",
            target = "_blank",
            "API keys page"
          ),
          "to retrieve the API key you'll use in your requests."
        )
      ),
      appMd("home")
    ),

    ## chat ----
    tabPanel(
      "Chat",
      apiPanel(
        md_file = "chat",
        fluidRow(
          column(4, wellPanelSm(
            selectInput(
              "chatModel",
              tooltipLabel(
                "model",
                "ID of the model to use. See the model endpoint compatibility table for details on which models work with the Chat API."
              ),
              c("gpt-4", "gpt-4-0314", "gpt-4-32k", "gpt-4-32k-0314", "gpt-3.5-turbo", "gpt-3.5-turbo-0301"),
              selected = "gpt-3.5-turbo"
            ),
            sliderInput(
              "chatN",
              tooltipLabel(
                "n",
                "How many chat completion choices to generate for each input message."
              ),
              1, 50, 1, 1
            ),
            sliderInput(
              "chatMaxTokens",
              tooltipLabel(
                "max_tokens",
                "The maximum number of tokens to generate in the chat completion."
              ),
              1, 1000, 200, 1
            ),
            sliderInput(
              "chatTemperature",
              tooltipLabel(
                "temperature",
                "What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic."
              ),
              0, 2, 0.7, 0.1
            ),
            # top_p
            # stream
            # stop
            sliderInput(
              "chatPresencePenalty",
              tooltipLabel(
                "presence_penalty",
                "Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics."
              ),
              -2, 2, 0, 0.1
            ),
            sliderInput(
              "chatFrequencyPenalty",
              tooltipLabel(
                "frequency_penalty",
                "Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim."
              ),
              -2, 2, 0, 0.1
            ),
            # logit_bias
            # user
          )),
          column(8,
            dialogContainer("chatDialogContainer", TRUE, TRUE),
            wellPanelSm(
              textConsole(
                "chatQ",
                tagList(
                  "message content",
                  tags$small("[Enter = send, Shift+Enter = new line]")
                )
              )
            )
          )
        )
      )
    ),

    ## files ----
    tabPanel(
      "Files",
      apiPanel(
        md_file = "files",
        wellPanelSm(
          fluidRow(
            column(8, fileInput("filesUpload", "file")),
            column(4, buttonContainer(actionButton("filesUploadExecute", "Create", class = "btn-primary")))
          )
        ),
        tableContainer(
          shiny::dataTableOutput("filesTable")
        )
      )
    ),

    ## fine-tunes ----
    tabPanel(
      "Fine-tunes",
      apiPanel(
        md_file = "fine-tunes",
        wellPanelSm(
          fluidRow(
            column(2, selectInput("fineTunesModel", "model", c("ada", "babbage", "curie", "davinci"))),
            column(4, selectInput("fineTunesTrainingFile", "training_file", choices = c())),
            column(3, numericInput("fineTunesNEpoch", "n_epochs", value = 4, min = 1, max = 10, step = 1)),
            column(3, numericInput("fineTunesLearningRateMultiplier", "learning_rate_multiplier", value = 0.1, min = 0.02, max = 0.2, step = 0.01))
          ),
          actionButton("fineTunesCreate", "Create", class = "btn-primary pull-right"),
          div(class = "clearfix")
        ),
        tableContainer(
          shiny::dataTableOutput("fineTunesTable")
        )
      )
    ),

    ## completions ----
    tabPanel(
      "Completions",
      apiPanel(
        md_file = "completions",
        fluidRow(
          column(4,
            wellPanelSm(
              selectInput(
                "completionsModel",
                tooltipLabel(
                  "model",
                  "ID of the model to use. See the model endpoint compatibility table for details on which models work with the Chat API."
                ),
                c()
              ),
              sliderInput(
                "completionsN",
                tooltipLabel(
                  "n",
                  "How many chat completion choices to generate for each input message."
                ),
                1, 50, 1, 1
              ),
              sliderInput(
                "completionsMaxTokens",
                tooltipLabel(
                  "max_tokens",
                  "The maximum number of tokens to generate in the chat completion."
                ),
                1, 1000, 200, 1
              ),
              sliderInput(
                "completionsTemperature",
                tooltipLabel(
                  "temperature",
                  "What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic."
                ),
                0, 2, 0.7, 0.1
              ),
              # top_p
              # stream
              # stop
              sliderInput(
                "completionsPresencePenalty",
                tooltipLabel(
                  "presence_penalty",
                  "Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics."
                ),
                -2, 2, 0, 0.1
              ),
              sliderInput(
                "completionsFrequencyPenalty",
                tooltipLabel(
                  "frequency_penalty",
                  "Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim."
                ),
                -2, 2, 0, 0.1
              ),
              # logit_bias
              # user
            )
          ),
          column(8,
            dialogContainer("completionsDialogContainer"),
            wellPanelSm(
              textConsole(
                "completionsPrompt",
                tagList(
                  "completion content",
                  tags$small("[Enter = send, Shift+Enter = new line]")
                )
              )
            )
          )
        )
      )
    ),

    ## image generator ----
    tabPanel(
      "Image generator",
      apiPanel(
        md_file = "image-generator",
        wellPanelSm(
          fluidRow(
            column(4,
              sliderInput(
                "imgGenN",
                tooltipLabel(
                  "n",
                  "The number of images to generate."
                ),
                1, 10, 1, 1
              ),
              selectInput(
                "imgGenSize",
                tooltipLabel(
                  "size",
                  "The size of the generated images."
                ),
                c("256x256", "512x512", "1024x1024")
              )
            ),
            column(8,
              textConsole(
                "imgGenPrompt",
                tooltipLabel(
                  tagList(
                    "prompt",
                    tags$small("[Enter = send, Shift+Enter = new line]")
                  ),
                  "A text description of the desired image(s). The maximum length is 1000 characters."
                )
              )
            )
          )
        ),
        imagesContainer("imgGenContainer")
      )
    ),

    ## image edit ----
    tabPanel(
      "Image edit",
      value = "image_edit",
      apiPanel(
        md_file = "image-edit",
        wellPanelSm(
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
                  "image",
                  "Uload image you want to edit."
                )
              ),
              colourpicker::colourInput("imgEditColorBg", "edit backgroud color", value = "#444"),
              colourpicker::colourInput("imgEditColorDraw", "edit draw color", value = "#777"),
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
                  tagList(
                    "edit description",
                    tags$small("[Enter = send, Shift+Enter = new line]")
                  ),
                  "A text description of the desired image(s). The maximum length is 1000 characters."
                )
              )
            )
          )
        ),
        imagesContainer("imgEditContainer")
      )
    )
  ),

  # tooltips ignite ----
  tooltipIgnite()
)
