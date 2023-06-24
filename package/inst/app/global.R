# general ----

`%>%` = magrittr::`%>%`


# logger ----

logger <- log4r::logger("DEBUG")
log_error <- function(...) {
  log4r::error(logger, ...)
}
log_warning <- function(...) {
  log4r::warn(logger, ...)
}
log_info <- function(...) {
  log4r::info(logger, ...)
}
log_debug <- function(...) {
  log4r::debug(logger, ...)
}
oaii::set_logger(
  error = log_error,
  warning = log_warning,
  info = log_info,
  debug = log_debug
)


# textConsole ----

textConsoleInputId <- function(id) paste0(id, "Input")

textConsole <- function(id, label = NULL) {
  inputId <- textConsoleInputId(id)
  htmltools::tagList(
    shiny::textAreaInput(
      inputId,
      label,
      width = "100%",
      resize = "none",
    ),
    htmltools::tags$script(
      type = "text/javascript",
      paste0("oaii.textConsole.attachEvent('", id ,"', '", inputId ,"')")
    )
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


# dialog ----

dialogContainer <- function(id, btnUpload = FALSE, btnDownload = FALSE) {
  htmltools::div(
    class = "oaii-chatDialogContainer",
    if (any(btnUpload, btnDownload))
      htmltools::div(
        class = "oaii-chatDialogContainerToolbar",
        if (btnDownload) shiny::downloadButton(paste0(id, "Download"), label = NULL),
        if (btnUpload) shiny::fileInput(paste0(id, "Upload"), label = NULL)
      ),
    shiny::uiOutput(id, class = "oaii-chatDialogContainerDialog"),
  )
}

dialogMessages <- function(messages, idDialogContainer = NULL) {
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
    if (!is.null(idDialogContainer))
      htmltools::tags$script(paste0("oaii.scrollDown('", idDialogContainer  , "')"))
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


# addons ----

# well sm
wellPanelSm <- function(...) {
  shiny::wellPanel(class = "well-sm", ...)
}

# table container
tableContainer <- function(...) {
  htmltools::div(
    class = "oaii-tableContainer",
    div(
      class = "oaii-tableContainerContent",
      ...
    )
  )
}

# button container
buttonContainer <- function(..., label = NULL) {
  htmltools::div(
    class = "form-group",
    htmltools::tags$label(htmltools::HTML(paste0(label, "&nbsp;"))),
    div(
      class = "oaii-tableContainerContent",
      ...
    )
  )
}
