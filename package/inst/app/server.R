server <- function(input, output, session) {

  # chat ----

  chatMessages <- reactiveVal()

  output$chatDialogMessages <- renderUI({
    chatDialogMessages(chatMessages(), "chatDialogMessages")
  })

  observeEvent(input$chatQ, {
    .api_key <- req(input$api_key)
    .chatQ <- req(input$chatQ)

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

  output$chatDownload <- shiny::downloadHandler(
    function() {
      paste0("chat ", format(Sys.time(), "%Y.%m.%d %H.%M"), ".csv")
    },
    function(file) {
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

  observeEvent(input$chatUpload, {
    tryCatch(
      expr = {
        df <- read.table(input$chatUpload$datapath, sep = "\t", header = TRUE)
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
    imagesSets(imagesGenSets(), "imgGenContainer")
  })

  observeEvent(input$imgGenPrompt, {
    .api_key <- req(input$api_key)
    .imgGenPrompt <- req(input$imgGenPrompt)

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
    req(input$menu)
    if (input$menu == "image_edit") {
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
    session$sendCustomMessage(
      "oaii.images.edit",
      list(
        cmd = "file",
        data = base64enc::base64encode(input$imgEditFileIn$datapath)
      )
    )
  })

  observeEvent(input$imgEditColorBg, {
    session$sendCustomMessage(
      "oaii.images.edit",
      list(
        cmd = "colorBg",
        data = input$imgEditColorBg
      )
    )
  })

  observeEvent(input$imgEditColorDraw, {
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
    imagesSets(imgEditSets(), "imgEditContainer")
  })

  observeEvent(input$imgEditPrompt, {
    .api_key <- req(input$api_key)
    .imgEditPrompt <- req(input$imgEditPrompt)
    .imgEditFileOut <- req(input$imgEditFileOut)

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
  files_table_update <- reactiveVal(TRUE)
  trigger_table_update <- function() {
    files_table_update(!files_table_update())
  }

  observe({
    req(input$api_key)
    files_table_update()

    res_content <- oaii::files_list_request(input$api_key)
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    else {
      files_df <- as.data.frame(do.call(rbind, res_content$data))
      files_df[sapply(files_df, is.null)] <- ""
      files_df$created_at <- lapply(
        files_df[, "created_at"],
        function(u) as.POSIXct(u, origin="1970-01-01")
      )
      manage <- vapply(
        files_df[, "id"],
        function(id){
          as.character(htmltools::tags$button(
            class = "btn btn-danger btn-xs",
            onclick = paste0("oaii.files.rm('files_table_rm','", id, "')"),
            fontawesome::fa("trash")
          ))
        },
        character(1)
      )
      files_df <- cbind(files_df, manage)

      output$files_table <- shiny::renderDataTable(
        files_df,
        options = list(
          searching = FALSE,
          columnDefs = list()
        ),
        escape = FALSE
      )
    }
  })

  observeEvent(input$files_upload, {
    req(input$api_key)

    file_uploaded <- file.path(
      dirname(input$files_upload$datapath),
      gsub("[^a-zA-Z0-9\\.]", "_", input$files_upload$name, perl = TRUE)
    )
    file.rename(input$files_upload$datapath, file_uploaded)
    res_content <- oaii::files_upload_request(
      input$api_key,
      file_uploaded,
      "fine-tune"
    )
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    unlink(file_uploaded)
    trigger_table_update()
  })

  observeEvent(input$files_table_rm, {
    req(input$api_key)

    res_content <- oaii::files_delete_request(
      input$api_key,
      input$files_table_rm
    )
    if (oaii::is_error(res_content)) {
      showNotification(res_content$message_long, type = "error")
    }
    trigger_table_update()
  })
}
