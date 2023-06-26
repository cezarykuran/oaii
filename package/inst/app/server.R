server <- function(input, output, session) {
  log_info("starting new server session..")


  # api_key ----

  api_key <- reactiveVal()
  observeEvent(debounce(input$api_key, 1000), {
    log_debug("observeEvent(debounce(input$api_key, 1000), {..}")

    # update the api_key() reactive value based on
    # the debounced and verified input$api_key
    .debounced_api_key <- input$api_key
    if (is.character(.debounced_api_key) && nchar(.debounced_api_key)) {
      res <- oaii::request("https://api.openai.com/v1/chat/completions", .debounced_api_key)
      if (is.null(res$status_code) || res$status_code == 401) {
        showNotification(res$message_long, type = "error", duration = 5)
        inputSetState("api_key", "error")
        api_key(NULL)
      }
      else {
        inputSetState("api_key", "success")
        api_key(.debounced_api_key)
      }
    }
    else {
      inputSetState("api_key", "error")
      api_key(NULL)
    }
  })


  # panels enable/disable ----

  observe({
    log_debug("observe({..}) [enable/disable panels]")

    selector <- ".tab-pane:not([data-value=home])"
    if (is.null(api_key())) shinyjs::disable(selector = selector)
    else shinyjs::enable(selector = selector)
  })


  # chat ----

  chatMessages <- reactiveVal()

  # render chat dialog container
  output$chatDialogContainer <- renderUI({
    log_debug("output$chatDialogContainer <- renderUI({..})")
    dialogMessages(chatMessages(), "chatDialogContainer")
  })

  # send message(s)
  observeEvent(input$chatQ, {
    .api_key <- req(api_key())
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

  # export chat to csv
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

  # import csv file
  observeEvent(input$chatDialogContainerUpload, {
    log_debug("observeEvent(input$chatDialogContainerUpload, {..})")

    tryCatch(
      expr = {
        datapath <- input$chatDialogContainerUpload$datapath
        df <- read.table(
          datapath,
          sep = "\t",
          header = TRUE
        )
        df[, "content"] <- utils::URLdecode(df[, "content"])
        chatMessages(
          lapply(seq_len(NROW(df)), function(n) df[n, , drop = TRUE])
        )
        showNotification(
          paste0("File '", datapath ,"' uploaded successfully!"),
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          paste0("An error occurred while loading the file, error message '", e$message, "'"),
          type = "error"
        )
      }
    )
    shinyjs::reset("chatDialogContainerUpload")
  })


  # image generator ----

  imagesGenSets <- reactiveVal()

  # render images container
  output$imgGenContainer  <- renderUI({
    log_debug("observeEvent(output$imgGenContainer  <- renderUI({..})")
    imagesSets(imagesGenSets(), "imgGenContainer")
  })

  # generate new images
  observeEvent(input$imgGenPrompt, {
    .api_key <- req(api_key())
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

  # self-destroy observer to set the initial image edit "input" size
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

  # forward loaded image (js -> R -> js)
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

  # forward edit bg color (js -> R -> js)
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

  # forward edit draw color (js -> R -> js)
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

  # render images container
  output$imgEditContainer  <- renderUI({
    log_debug("output$imgEditContainer  <- renderUI({..})")

    imagesSets(imgEditSets(), "imgEditContainer")
  })

  # send a request for new (edited) images
  observeEvent(input$imgEditPrompt, {
    .api_key <- req(api_key())
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
    .api_key <- req(api_key())
    log_debug("files_df <- reactive({..})")

    files_df_update()

    res_content <- oaii::files_list_request(.api_key)
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
      data.frame()
    }
    else {
      as.data.frame(do.call(rbind, res_content$data))
    }
  })

  # append manage column (helper function)
  files_df_col_manage <- function(df, column, id) {
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

  # send upload file reques
  observeEvent(input$filesUploadExecute, {
    .api_key <- req(api_key())
    .filesUpload <- req(input$filesUpload)
    req(file.exists(.filesUpload$datapath))
    .filesPurpose <- req(input$filesPurpose)
    log_debug("observeEvent(input$filesUploadExecute, {..})")

    file_uploaded <- file.path(
      dirname(.filesUpload$datapath),
      gsub("[^a-zA-Z0-9\\.]", "_", .filesUpload$name, perl = TRUE)
    )
    file.rename(.filesUpload$datapath, file_uploaded)
    res_content <- oaii::files_upload_request(
      .api_key,
      file_uploaded,
      .filesPurpose
    )
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    else {
      showNotification(
        paste0("File '", .filesUpload$name ,"' uploaded successfully!"),
        type = "message"
      )
    }
    unlink(file_uploaded)
    shinyjs::reset("filesUpload")
    trigger_files_df_update()
  })

  # render files table
  output$filesTable <- shiny::renderDataTable(
    expr = {
      log_debug("output$filesTable <- shiny::renderDataTable({..})")

      .files_df <- files_df()
      if (NROW(.files_df)) {
        .files_df %>%
          oaii::df_exclude_cols("object") %>%
          oaii::df_null_replace() %>%
          oaii::df_col_dt_format("created_at") %>%
          files_df_col_manage("id", "filesTableRm")
      }
      else data.frame()
    },
    options = list(
      searching = FALSE,
      columnDefs = list()
    ),
    escape = FALSE
  )

  # send delete file request
  observeEvent(input$filesTableRm, {
    .api_key <- req(api_key())
    log_debug("observeEvent(input$filesTableRm, {..})")

    res_content <- oaii::files_delete_request(
      .api_key,
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
    .api_key <- req(api_key())
    log_debug("fine_tunes_df <- reactive({..})")

    fine_tunes_update()

    res_content <- oaii::fine_tunes_list_request(.api_key)
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
      data.frame()
    }
    else {
      as.data.frame(do.call(rbind, res_content$data))
    }
  })

  # df_col_obj_implode html table version (helper)
  fine_tunes_df_impode_table <- function(df, col, obj_prop, nested = TRUE) {
    oaii::df_col_obj_implode(
      df, col, obj_prop, nested,
      objs_glue = "",
      cell_header = "<table class='tableTd'>",
      cell_footer = "</table>",
      prop_fmt = "<tr><td>%s</td><td>%s</td></tr>"
    )
  }

  # update files selectize input
  observe({
    log_debug("observe({..}) [input$fineTunesTrainingFile update]")

    # default (empty)
    choices <- c()

    # update by files data.frame
    .files_df <- files_df()
    df_choices_cols <- c("id", "filename")
    if (all(df_choices_cols %in% colnames(.files_df))) {
      choices_df <- .files_df[.files_df$purpose == "fine-tune", df_choices_cols]
      if (NROW(choices_df)) {
        choices  <- unlist(choices_df$id)
        names(choices) <- unlist(choices_df$filename)
      }
    }
    updateSelectInput(session, "fineTunesTrainingFile", choices = choices)
  })

  # send new fine-tunes request
  observeEvent(input$fineTunesCreate, {
    .api_key <- req(api_key())
    .fineTunesTrainingFile <- req(input$fineTunesTrainingFile)
    .fineTunesModel <- req(input$fineTunesModel)
    .fineTunesNEpoch <- req(input$fineTunesNEpoch)
    .fineTunesLearningRateMultiplier <- req(input$fineTunesLearningRateMultiplier)

    res_content <- oaii::fine_tunes_create_request(
      .api_key,
      training_file = .fineTunesTrainingFile,
      model = .fineTunesModel,
      n_epochs = as.integer(.fineTunesNEpoch),
      learning_rate_multiplier = as.double(.fineTunesLearningRateMultiplier)
    )
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    trigger_fine_tunes_update()
  })

  # render fine-tunes table
  output$fineTunesTable <- shiny::renderDataTable(
    expr = {
      log_debug("output$fineTunes_table <- shiny::renderDataTable({..})")
      .fine_tunes_df <- fine_tunes_df()

      if (NROW(.fine_tunes_df)) {
        .fine_tunes_df %>%
          oaii::df_exclude_cols(c("object", "organization_id")) %>%
          oaii::df_col_dt_format(
            c("created_at", "updated_at")
          ) %>%
          fine_tunes_df_impode_table(
            c("training_files", "result_files"),
            c("filename", "id")
          ) %>%
          fine_tunes_df_impode_table(
            "hyperparams",
            c("n_epochs", "batch_size", "prompt_loss_weight", "learning_rate_multiplier"),
            nested = FALSE
          )
      }
      else data.frame()
    },
    options = list(
      searching = FALSE,
      columnDefs = list()
    ),
    escape = FALSE
  )

  # send delete fine-tunes model request
  # observeEvent(input$fineTunesTableRm, {
  #   .api_key <- req(api_key())
  #   log_debug("observeEvent(input$fineTunesTableRm, {..})")
  #
  #   res_content <- oaii::files_delete_request(
  #     .api_key,
  #     input$fineTunesTableRm
  #   )
  #   if (oaii::is_error(res_content)) {
  #     showNotification(res_content$message_long, type = "error")
  #   }
  #   trigger_fine_tunes_update()
  # })


  # completions ----

  # update model selectize input
  observe({
    log_debug("observe({..}) [input$completionsModel update]")

    # default
    choices <- c("text-davinci-003", "text-davinci-002", "text-curie-001", "text-babbage-001", "text-ada-001")

    # update by fine-tunes data.frame
    .fine_tunes_df <- fine_tunes_df()
    if (all(c("status", "fine_tuned_model") %in% colnames(.fine_tunes_df))) {
      choices_ft <- .fine_tunes_df[.fine_tunes_df$status == "succeeded", "fine_tuned_model", drop = TRUE]
      if (length(choices_ft)) {
        choices  <- list(ft = choices_ft, default = choices)
      }
    }
    updateSelectInput(session, "completionsModel", choices = choices)
  })

  # send new completion request
  observeEvent(input$completionsPrompt, {
    .api_key <- req(api_key())
    .completionsPrompt <- req(input$completionsPrompt)
    log_debug("observeEvent(input$completionsPrompt, {..})")

    textConsoleDisable(session, "completionsPrompt")

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

    textConsoleEnable(session, "completionsPrompt")
  })
}
