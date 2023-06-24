server <- function(input, output, session) {
  log_info("starting new server session..")


  # helpers ----

  df_null_replace <- function(df, replacement = "") {
    df[sapply(df, is.null)] <- replacement
    df
  }

  df_dt_cols <- function(df, cols, dt_format = "%Y-%m-%d %H:%M:%S") {
    for (col in cols) {
      df[, col] <-
        list(lapply(
          df[, col],
          function(dt) {
            format(
              as.POSIXct(dt, origin="1970-01-01"),
              dt_format,
              usetz = FALSE
            )
          }
        ))
    }
    df
  }

  df_obj_cols <- function(df, cols, props, nested = TRUE) {
    for (col in cols) {
      df[, col] <-
        lapply(df[, col], function(obj) {
          if (!nested) obj <- list(obj)
          paste0(
            lapply(obj, function(element) {
              paste0(
                lapply(props, function(prop) {
                  paste0(prop, ": ", element[[prop]])
                }),
                collapse = as.character(htmltools::br())
              )
            }),
            collapse = as.character(htmltools::hr())
          )
        })
    }
    df
  }

  df_exclude_cols <- function(df, cols) {
    df[, -which(names(df) %in% cols)]
  }

  df_col_manage <- function(df, column, id) {
    manage <- vapply(
      df[, column],
      function(value) {paste0(
        htmltools::tags$button(
          class = "btn btn-danger btn-xs",
          onclick = paste0("oaii.tableBtn('", id,"','", value, "')"),
          fontawesome::fa("trash")
        )
      )},
      character(1)
    )
    cbind(df, manage)
  }


  # chat ----

  chatMessages <- reactiveVal()

  output$chatDialogContainer <- renderUI({
    log_debug("output$chatDialogContainer <- renderUI({..})")
    dialogMessages(chatMessages(), "chatDialogContainer")
  })

  observeEvent(input$chatQ, {
    .api_key <- req(input$api_key)
    .chatQ <- req(input$chatQ)
    log_debug("observeEvent(input$chatQ, {..})")

    textConsoleDisable(session, "chatQ")

    q <- oaii::chat_message(.chatQ)
    res_content <- oaii::chat_request(
      .api_key,
      oaii::chat_merge_messages(chatMessages(), q),
      model = input$chatModel,
      temperature = as.double(input$chatTemperature),
      n = as.integer(input$chatN),
      max_tokens = as.integer(input$chatMaxTokens),
      presence_penalty = as.double(input$chatPresencePenalty),
      frequency_penalty = as.double(input$chatFrequencyPenalty)
    )
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    else {
      a <- oaii::chat_fetch_messages(res_content)
      chatMessages(oaii::chat_merge_messages(chatMessages(), q, a))
      textConsoleReset(session, "chatQ")
    }
    textConsoleEnable(session, "chatQ")
  })

  output$chatDialogContainerDownload <- shiny::downloadHandler(
    function() {
      log_debug("observeEvent(input$chatDialogContainerDownload, {..}) [filename]")
      paste0("chat ", format(Sys.time(), "%Y.%m.%d %H.%M"), ".csv")
    },
    function(file) {
      log_debug("observeEvent(input$chatDialogContainerDownload, {..}) [content]")
      df <- do.call(rbind, chatMessages())
      df[, "content"] <- utils::URLencode(df[, "content"])
      write.table(
        df,
        file = file,
        quote = TRUE,
        sep = "\t",
        row.names = FALSE
      )
    }
  )

  observeEvent(input$chatDialogContainerUpload, {
    log_debug("observeEvent(input$chatDialogContainerUpload, {..})")

    tryCatch(
      expr = {
        df <- read.table(
          input$chatDialogContainerUpload$datapath,
          sep = "\t",
          header = TRUE
        )
        df[, "content"] <- utils::URLdecode(df[, "content"])
        chatMessages(
          lapply(seq_len(NROW(df)), function(n) df[n, , drop = TRUE])
        )
        showNotification("File uploaded successfully!", type = "message")
      },
      error = function(e) {
        showNotification(
          paste0("An error occurred while loading the file, error message '", e$message, "'"),
          type = "error"
        )
      }
    )
    shinyjs::reset("chatUpload")
  })


  # image generator ----

  imagesGenSets <- reactiveVal()

  output$imgGenContainer  <- renderUI({
    log_debug("observeEvent(output$imgGenContainer  <- renderUI({..})")
    imagesSets(imagesGenSets(), "imgGenContainer")
  })

  observeEvent(input$imgGenPrompt, {
    .api_key <- req(input$api_key)
    .imgGenPrompt <- req(input$imgGenPrompt)
    log_debug("observeEvent(input$imgGenPrompt, {..})")

    textConsoleDisable(session, "imgGenPrompt")

    res_content <- oaii::images_generator_request(
      .api_key,
      prompt = .imgGenPrompt,
      response_format = "b64_json",
      size = input$imgGenSize,
      n = as.integer(input$imgGenN)
    )

    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    else {
      imagesGenSets(oaii::images_merge_sets(
        imagesGenSets(),
        oaii::images_fech_set(res_content, .imgGenPrompt, input$imgGenSize)
      ))
      textConsoleReset(session, "imgGenPrompt")
    }
    textConsoleEnable(session, "imgGenPrompt")
  })


  # image edit ----

  menuEvent <- observeEvent(input$menu, {
    .menu <- req(input$menu)
    log_debug("observeEvent(menuEvent <- observeEvent(input$menu, {..})")

    if (.menu == "image_edit") {
      session$sendCustomMessage(
        "oaii.images.edit",
        list(
          cmd = "resize",
          data = NULL
        )
      )
      menuEvent$destroy()
    }
  })

  observeEvent(input$imgEditFileIn, {
    log_debug("observeEvent(input$imgEditFileIn, {..})")

    session$sendCustomMessage(
      "oaii.images.edit",
      list(
        cmd = "file",
        data = base64enc::base64encode(input$imgEditFileIn$datapath)
      )
    )
  })

  observeEvent(input$imgEditColorBg, {
    log_debug("observeEvent(input$imgEditColorBg, {..})")

    session$sendCustomMessage(
      "oaii.images.edit",
      list(
        cmd = "colorBg",
        data = input$imgEditColorBg
      )
    )
  })

  observeEvent(input$imgEditColorDraw, {
    log_debug("observeEvent(input$imgEditColorDraw, {..})")

    session$sendCustomMessage(
      "oaii.images.edit",
      list(
        cmd = "colorDraw",
        data = input$imgEditColorDraw
      )
    )
  })

  imgEditSets <- reactiveVal()

  output$imgEditContainer  <- renderUI({
    log_debug("output$imgEditContainer  <- renderUI({..})")

    imagesSets(imgEditSets(), "imgEditContainer")
  })

  observeEvent(input$imgEditPrompt, {
    .api_key <- req(input$api_key)
    .imgEditPrompt <- req(input$imgEditPrompt)
    .imgEditFileOut <- req(input$imgEditFileOut)
    log_debug("observeEvent(input$imgEditPrompt, {..})")

    textConsoleDisable(session, "imgEditPrompt")

    res_content <- oaii::images_edit_request(
      api_key = .api_key,
      image = base64enc::base64decode(
        sub("data:image/png;base64,", "", .imgEditFileOut)
      ),
      prompt = .imgEditPrompt,
      size = input$imgEditSize,
      n = as.integer(input$imgEditN)
    )

    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    else {
      imgEditSets(oaii::images_merge_sets(
        imgEditSets(),
        oaii::images_fech_set(res_content, .imgEditPrompt, input$imgEditSize)
      ))
      textConsoleReset(session, "imgEditPrompt")
    }
    textConsoleEnable(session, "imgEditPrompt")
  })


  # files ----

  files_df_update <- reactiveVal(TRUE)
  trigger_files_df_update <- function() {
    files_df_update(!files_df_update())
  }

  files_df <- reactive({
    .api_key <- req(input$api_key)
    log_debug("files_df <- reactive({..})")

    files_df_update()

    res_content <- oaii::files_list_request(.api_key)
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
      NULL
    }
    else {
      as.data.frame(do.call(rbind, res_content$data))
    }
  })

  observeEvent(input$filesUpload, {
    .api_key <- req(input$api_key)
    log_debug("observeEvent(input$files_upload, {..})")

    file_uploaded <- file.path(
      dirname(input$files_upload$datapath),
      gsub("[^a-zA-Z0-9\\.]", "_", input$files_upload$name, perl = TRUE)
    )
    file.rename(input$files_upload$datapath, file_uploaded)
    res_content <- oaii::files_upload_request(
      .api_key,
      file_uploaded,
      "fine-tune"
    )
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    unlink(file_uploaded)
    trigger_files_df_update()
  })

  output$filesTable <- shiny::renderDataTable(
    expr = {
      log_debug("output$filesTable <- shiny::renderDataTable({..})")

      .files_df <- files_df()
      if (is.null(.files_df)) {
        data.frame(list(files = "empty"))
      }
      else {
        .files_df %>%
          df_null_replace() %>%
          df_dt_cols("created_at") %>%
          df_exclude_cols("object") %>%
          df_col_manage("id", "filesTableRm")
      }
    },
    options = list(
      searching = FALSE,
      columnDefs = list()
    ),
    escape = FALSE
  )

  observeEvent(input$filesTableRm, {
    .api_key <- req(input$api_key)
    log_debug("observeEvent(input$filesTableRm, {..})")

    res_content <- oaii::files_delete_request(
      input$api_key,
      input$filesTableRm
    )
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    trigger_files_df_update()
  })


  # fine-tunes ----

  fine_tunes_update <- reactiveVal(TRUE)
  trigger_fine_tunes_update <- function() {
    fine_tunes_update(!fine_tunes_update())
  }

  fine_tunes_df <- reactive({
    .api_key <- req(input$api_key)
    log_debug("fine_tunes_df <- reactive({..})")

    fine_tunes_update()

    res_content <- fine_tunes_list_request(.api_key)
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
      NULL
    }
    else {
      as.data.frame(do.call(rbind, res_content$data))
    }
  })

  observe({
    .files_df <- req(files_df())

    choices_df <- .files_df[.files_df$purpose == "fine-tune", c("id", "filename")]
    if (NROW(choices_df)) {
      choices  <- unlist(choices_df$id)
      names(choices) <- unlist(choices_df$filename)
      updateSelectInput(session, "fineTunesTrainingFile", choices = choices)
    }
  })

  observeEvent(input$fineTunesCreate, {
    .api_key <- req(input$api_key)
    .fineTunesTrainingFile <- req(input$fineTunesTrainingFile)
    .fineTunesModel <- req(input$fineTunesModel)

    res_content <- oaii::fine_tunes_create_request(
      .api_key,
      training_file = .fineTunesTrainingFile,
      model = .fineTunesModel
    )
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    trigger_fine_tunes_update()
  })

  output$fineTunesTable <- shiny::renderDataTable(
    expr = {
      log_debug("output$fineTunes_table <- shiny::renderDataTable({..})")
      .fine_tunes_df <- fine_tunes_df()

      if (is.null(.fine_tunes_df)) {
        data.frame("no data")
      }
      else {
        .fine_tunes_df %>%
          df_dt_cols(
            c("created_at", "updated_at")
          ) %>%
          df_obj_cols(
            c("training_files", "result_files"),
            c("filename", "id")
          ) %>%
          df_obj_cols(
            "hyperparams",
            c("n_epochs", "batch_size", "prompt_loss_weight", "learning_rate_multiplier"),
            nested = FALSE
          ) %>%
          df_exclude_cols(c("object", "organization_id"))
          #df_col_manage("id", "fineTunesTableRm")
      }
    },
    options = list(
      searching = FALSE,
      columnDefs = list()
    ),
    escape = FALSE
  )

  observeEvent(input$fineTunesTableRm, {
    .api_key <- req(input$api_key)
    log_debug("observeEvent(input$fineTunesTableRm, {..})")

    res_content <- oaii::files_delete_request(
      input$api_key,
      input$filesTableRm
    )
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    trigger_fine_tunes_update()
  })


  # completions ----

  observeEvent(input$completionsPrompt, {
    .api_key <- req(input$api_key)
    .completionsPrompt <- req(input$completionsPrompt)
    log_debug("observeEvent(input$completionsPrompt, {..})")

    res_content <- oaii::completions_create_request(
      .api_key,
      prompt = .completionsPrompt,
      model = input$completionsModel,
      max_tokens = as.integer(input$completionsMaxTokens),
      n = as.integer(input$completionsN),
      temperature = as.double(input$completionsTemperature),
      presence_penalty = as.double(input$completionsPresencePenalty),
      frequency_penalty = as.double(input$completionsFrequencyPenalty)
    )
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    else {
      output$completionsDialogContainer <- renderUI({
        dialogMessages(c(
          list(oaii::completion_message(.completionsPrompt, "user")),
          lapply(
            oaii::completions_fetch_text(res_content),
            function(text) {
              oaii::completion_message(
                sub("^[\\s]+", "", text, perl = TRUE),
                "ai"
              )
            }
          )
        ))
      })

      textConsoleReset(session, "completionsPrompt")
    }
  })
}
