# OpenAI API R implementation

This is an early version of an R library aimed at streamlining communication with the OpenAI API.

**The oaii package offers the following features:**

- A comprehensive set of helper functions for seamless communication with API.
- Useful tools for efficient data manipulation and transformation.
- An interactive Shiny demo application, which can be accessed at https://cezarykuran.shinyapps.io/oaii/ (feel free to explore its functionality!).
- Several demonstration terminal applications written as R scripts. These applications serve as practical examples, demonstrating how to effectively leverage the package's functions for API communication.

**Endpoint helpers implemented so far:**

- files
- fine tunes
- completions
- models  
- chat
- image generator, image editor (rather for fun)

## Repo structure

All API helpers can be found in the [package/R](https://github.com/cezarykuran/oaii/tree/master/package/R) directory.

Shiny demo application is in the [package/inst/app/shiny](https://github.com/cezarykuran/oaii/tree/master/package/inst/app/shiny)  directory. To find information on how to run the application, type `?oaii::demo_shiny()`.
CLI demo R scrips are in the [package/inst/app/cli](https://github.com/cezarykuran/oaii/tree/master/package/inst/app/cli)  directory. For more information, type `?oaii::demo_cli()`.

## Instalation

`devtools::install_github("https://github.com/cezarykuran/oaii.git", subdir = "package")`

## Other

**[OpenAI](https://openai.com/) useful links:**

- [API reference](https://platform.openai.com/docs/api-reference/)
- [API help pages](https://help.openai.com/en/collections/3675931-openai-api)
- [docs](https://platform.openai.com/docs/introduction)
- [openai-cookbook](https://github.com/openai/openai-cookbook/)
