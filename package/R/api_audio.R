#' API audio: text to speech request
#'
#' Generates audio from the input text. To get more details, visit
#' https://platform.openai.com/docs/api-reference/audio/createSpeech
#' https://platform.openai.com/docs/guides/speech-to-text
#' @inherit request params return
#' @param model string, one of the available TTS models: 'tts-1' or 'tts-1-hd'
#' @param input string, the text to generate audio for.
#' The maximum length is 4096 characters.
#' @param voice string, the voice to use when generating the audio. Supported
#' voices are alloy, echo, fable, onyx, nova, and shimmer.
#' Previews of the voices are available in the Text to speech guide -
#' https://platform.openai.com/docs/guides/text-to-speech/voice-options
#' @param response_format string, the format to audio in. Supported formats are
#' mp3 (default), opus, aac, and flac
#' @param speed double, the speed of the generated audio. Select a value from
#' 0.25 to 4.0, 1.0 is the default.
#' @export
#'
#' @examples
#' \dontrun{
#' res_content <- audio_speech_request(
#'   "tts-1",
#'   "When the power of love overcomes the love of power, the world will know peace.",
#'   "nova"
#' )
#' if (!is_error(res_content)) {
#'   writeBin(res_content, "peace.mp3")
#' }
#' }
#'
audio_speech_request <- function(
    model,
    input,
    voice,
    response_format = NULL,
    speed = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`model` must be a non-empty string" =
      checkmate::testString(model, min.chars = 1),
    "`input` must be a non-empty string" =
      checkmate::testString(input, min.chars = 1, max.chars = 4096),
    "`voice` must be a non-empty string" =
      checkmate::testString(voice, min.chars = 1),
    "`response_format` must NULL or a non-empty string" =
      checkmate::testString(response_format, min.chars = 1, null.ok = TRUE),
    "`speed` must be NULL or a double" =
      checkmate::testDouble(speed, lower = 0, null.ok = TRUE)
  )

  request(
    endpoint = "https://api.openai.com/v1/audio/speech",
    api_key = api_key,
    method = "POST",
    body = list(
      model = model,
      input = input,
      voice = voice,
      response_format = response_format,
      speed = speed
    ),
    content_class = paste0(c("oaii_content_audio", response_format), collapse = "_")
  )
}

#' API audio: speech to text (transcryption)
#'
#' Transcribes audio into the input language. To get more details, visit
#' https://platform.openai.com/docs/api-reference/audio/createTranscription
#' https://platform.openai.com/docs/guides/speech-to-text
#' @inherit request params return
#' @param file string/raw, content of the audio file or path to the audio file
#' to transcribe, in one of these formats: flac, mp3, mp4, mpeg, mpga, m4a, ogg,
#' wav, or webm.
#' @param model string, ID of the model to use. Only 'whisper-1' is currently
#' available.
#' @param language NULL/string, the language of the input audio. Supplying the input
#' language in ISO-639-1 format will improve accuracy and latency. See
#' https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes
#' @param prompt NULL/string, an optional text to guide the model's style or continue
#' a previous audio segment. The prompt
#' (https://platform.openai.com/docs/guides/speech-to-text/prompting) should
#' match the audio language.
#' @param response_format NULL/string, The format of the transcript output, in one
#' of these options: json (default), text, srt, verbose_json, or vtt.
#' @param temperature NULL/double, the sampling temperature, between 0 and 1. Higher
#' values like 0.8 will make the output more random, while lower values like 0.2
#' will make it more focused and deterministic. If set to 0, the model will use
#' log probability to automatically increase the temperature until certain
#' thresholds are hit. 0 is default.
#' @param file_type NULL/string mime type of file (e.g. "audio/mpeg").
#' If NULL (default), will be guess by mime::guess_type() when needed.
#' @export
#'
#' @examples
#' \dontrun{
#' res_content <- audio_speech_request(
#'   "path/to/audio/file.mp3",
#'   "whisper-1",
#'   "en",
#'   response_format = "text"
#' )
#' if (!is_error(res_content)) {
#'   message(res_content)
#' }
#' }
#'
audio_transcription_request <- function(
    file,
    model,
    language = NULL,
    prompt = NULL,
    response_format = NULL,
    temperature = NULL,
    file_type = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`file` must be a fle path or raw content" =
      checkmate::testFileExists(file) || checkmate::testRaw(file),
    "`model` must be a non-empty string" =
      checkmate::testString(model, min.chars = 1),
    "`language` must be NULL or a non-empty string" =
      checkmate::testString(language, min.chars = 1, null.ok = TRUE),
    "`prompt` must be NULL or a non-empty string" =
      checkmate::testString(prompt, min.chars = 1, null.ok = TRUE),
    "`response_format` must be NULL or a non-empty string" =
      checkmate::testString(response_format, min.chars = 1, null.ok = TRUE),
    "`temperature` must be NULL or a double" =
      checkmate::testDouble(temperature, lower = 0, upper = 1, null.ok = TRUE),
    "`file_type` must be NULL or a non-empty string" =
      checkmate::testString(file_type, min.chars = 1, null.ok = TRUE)
  )

  request(
    endpoint = "https://api.openai.com/v1/audio/transcriptions",
    api_key = api_key,
    method = "POST",
    body = list(
      file = api_upload_file(file, file_type),
      model = model,
      language = language,
      prompt = prompt,
      response_format = response_format,
      temperature = temperature
    ),
    encode = "multipart",
    content_class = "oaii_content_transcription"
  )
}


#' API audio: translate audio file into English text
#'
#' Translates audio into English. To get more details, visit
#' https://platform.openai.com/docs/api-reference/audio/createTranslation
#' https://platform.openai.com/docs/guides/speech-to-text
#' @inherit request params return
#' @param file string/raw, content of the input audio file or path to the input
#' audio file to translate, in one of these formats: flac, mp3, mp4, mpeg, mpga,
#' m4a, ogg, wav, or webm.#'
#' @param model string, ID of the model to use. Only 'whisper-1' is currently
#' available.
#' @param prompt string, An optional text to guide the model's style
#' or continue a previous audio segment. The prompt should be in English.
#' @param response_format string, the format of the transcript output, in one of
#' these options: json (default), text, srt, verbose_json, or vtt.
#' @param temperature double, the sampling temperature, between 0 and 1. Higher
#' values like 0.8 will make the output more random, while lower values like 0.2
#' will make it more focused and deterministic. If set to 0, the model will use
#' log probability to automatically increase the temperature until certain
#' thresholds are hit. 0 is default.
#' @export
#'
#' @examples
#' \dontrun{
#' res_content <- audio_translation_request(
#'   "path/to/audio/file.mp3",
#'   "whisper-1",
#'   response_format = "text"
#' )
#' if (!is_error(res_content)) {
#'   message(res_content)
#' }
#' }
#'
audio_translation_request <- function(
    file,
    model,
    prompt = NULL,
    response_format = NULL,
    temperature = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`file` must be a fle path or raw content" =
      checkmate::testFileExists(file) || checkmate::testRaw(file),
    "`model` must be a non-empty string" =
      checkmate::testString(model, min.chars = 1),
    "`prompt` must be NULL or a non-empty string" =
      checkmate::testString(prompt, min.chars = 1, null.ok = TRUE),
    "`response_format` must be NULL or a non-empty string" =
      checkmate::testString(response_format, min.chars = 1, null.ok = TRUE),
    "`temperature` must be NULL or a double" =
      checkmate::testDouble(temperature, lower = 0, upper = 1, null.ok = TRUE)
  )

  request(
    endpoint = "https://api.openai.com/v1/audio/transcriptions",
    api_key = api_key,
    method = "POST",
    body = list(
      file = api_upload_file(file),
      model = model,
      prompt = prompt,
      response_format = response_format,
      temperature = temperature
    ),
    encode = "multipart"
  )
}
