# OpenAI API R implementation

This is a very early version of the R library to facilitate communication with the OpenAI API.

**The oaii package contains:**

- a set of helpers for communication with the API  
- a set of helper to manipulate data
- shiny demo application (if you want, you can test it at https://cezarykuran.shinyapps.io/oaii/).
- CLI chat demo application
  
**Endpoint helpers implemented so far:**

- files
- fine tunes
- completions
- models  
- chat
- image generator, image editor (rather for fun)

## Repo structure
All API helpers can be found in the [package/R](https://github.com/cezarykuran/oaii/tree/master/package/R) directory.
Shiny demo application is in the [package/inst/app](https://github.com/cezarykuran/oaii/tree/master/package/inst/app).

## Instalation

`devtools::install_github("https://github.com/cezarykuran/oaii.git", subdir = "package")`

## Other

**OpenAI useful links:**

- [homepage](https://openai.com/)
- [API](https://platform.openai.com/docs/api-reference/)
- [docs](https://platform.openai.com/docs/introduction)
- [openai-cookbook](https://github.com/openai/openai-cookbook/)
