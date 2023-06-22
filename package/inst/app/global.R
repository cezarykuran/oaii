# textConsole ----

textConsoleInputId <- function(id) paste0(id, "Input")

textConsole <- function(id, label = NULL, send_on_enter = TRUE) {
  inputId <- textConsoleInputId(id)
  htmltools::tagList(
    shiny::textAreaInput(inputId, label, width = "100%", resize = "none"),
    if (send_on_enter) {
      htmltools::tags$script(
        type = "text/javascript",
        paste0("oaii.textConsole.attachEvent('", id ,"', '", inputId ,"')")
      )
    }
  )
}

textConsoleCommand <- function(session, id, command) {
  session$sendCustomMessage(
    "oaii.textConsole",
    list(inputId = textConsoleInputId(id), command = command)
  )
}

textConsoleDisable <- function (session, id) {
  textConsoleCommand(session, id, "disable")
}

textConsoleEnable <- function (session, id) {
  textConsoleCommand(session, id, "enable")
}

textConsoleReset <- function (session, id) {
  textConsoleCommand(session, id, "reset")
}


# chat ----

chatDialogContainer <- function(idDownload, idUpload, idDialog) {
  htmltools::div(
    class = "oaii-chatDialogContainer",
    htmltools::div(
      class = "oaii-chatDialogContainerToolbar",
      shiny::downloadButton(idDownload, label = NULL),
      shiny::fileInput(idUpload, label = NULL)
    ),
    shiny::uiOutput(idDialog, class = "oaii-chatDialogContainerDialog"),
  )
}

#' @export
chatDialogMessages <- function(messages, idDialog) {
  htmltools::tagList(
    htmltools::tags$table(
      class = "table",
      lapply(messages, function(message) {
        htmltools::tags$tr(
          htmltools::tags$td(message$role),
          htmltools::tags$td(htmltools::HTML(gsub("\n", "<br>", message$content)))
        )
      })
    ),
    htmltools::tags$script(paste0("oaii.scrollDown('", idDialog  , "')"))
  )
}


# images ----

imagesContainer <- function(id) {
  shiny::uiOutput(id, class = "oaii-imagesContainer")
}

imagesSet <- function(set) {
  download_filename <- gsub("[^a-zA-Z0-9-\\.]", "_", set$prompt, perl = TRUE)

  shiny::fluidRow(
    lapply(set$data, function(png64) {
      shiny::column(
        6, class = "col-md-4 col-lg-3",
        htmltools::div(
          class = "oaii-imagesSetImageContainer",
          onclick = "oaii.images.container.fsInOut(this)",
          htmltools::div(
            class = "oaii-imagesSetImage",
            htmltools::tags$img(
              src = paste0("data:image/png;base64,", png64)
            ),
            htmltools::tags$button(
              onclick = paste0(
                "oaii.images.container.download(this, event, '", download_filename, "')"
              ),
              shiny::icon("download")
            )
          )
        )
      )
    })
  )
}

imagesSets <- function(images, idContainer) {
  htmltools::tagList(
    lapply(rev(images), function(set) {
      htmltools::div(
        class = "oaii-imagesSetContainer",
        htmltools::div(
          class = "oaii-imagesSetPrompt",
          set$prompt
        ),
        imagesSet(set)
      )
    })
  )
}


# tooltips ----

tooltip <- function(x, content, placement = "auto", html = FALSE) {
  x$attribs["data-toggle"] <- "tooltip"
  x$attribs["data-title"] <- as.character(content)
  x$attribs["data-placement"] <- placement
  x$attribs["data-html"] <- html
  x
}

tooltipLabel <- function(label, tooltipContent, tooltipIcon = "question-circle") {
  htmltools::tagList(
    label,
    tooltip(
      htmltools::a(fontawesome::fa(tooltipIcon)),
      tooltipContent
    )
  )
}

tooltipIgnite <- function() {
  htmltools::tags$script(
    type = "text/javascript",
    "$('[data-toggle=tooltip]').tooltip()"
  )
}
