<!-- badges: start -->
[![R-CMD-check](https://github.com/cezarykuran/oaii/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cezarykuran/oaii/actions/workflows/R-CMD-check.yaml)
[![CodeFactor](https://www.codefactor.io/repository/github/cezarykuran/oaii/badge)](https://www.codefactor.io/repository/github/cezarykuran/oaii)
[![CRAN](https://www.r-pkg.org/badges/version/oaii)](https://cran.r-project.org/package=oaii)
[![CRAN](http://cranlogs.r-pkg.org/badges/last-month/oaii)](https://cran.r-project.org/package=oaii)
<!-- badges: end -->

# OpenAI API R implementation

This is an R library aimed at streamlining communication with the OpenAI API.

**The oaii package offers the following features:**

- A comprehensive set of helper functions for seamless communication with API.
- Useful tools for efficient data manipulation and transformation.

**Endpoint helpers implemented so far:**

- audio
- chat
- completions
- embeddings
- files
- fine tunes
- images
- models
- moderations

## Instalation

`install.packages('oaii')`

`devtools::install_github("https://github.com/cezarykuran/oaii.git", subdir = "package")`

## Demo

There exists a sister package named [oaiiDemoApps](https://github.com/cezarykuran/oaiiDemoApps) containing applications that demonstrate the practical use of the `oaii` package.
The [oaiiDemoApps](https://github.com/cezarykuran/oaiiDemoApps) package contains:

- An interactive Shiny demo application, which can be accessed at https://cezarykuran.shinyapps.io/oaii/ (feel free to explore its functionality!).
- Several demonstration terminal applications written as R scripts. These applications serve as practical examples, demonstrating how to effectively leverage the package's functions for API communication.

## Other

**[OpenAI](https://openai.com/) useful links:**

- [API reference](https://platform.openai.com/docs/api-reference/)
- [API help pages](https://help.openai.com/en/collections/3675931-openai-api)
- [docs](https://platform.openai.com/docs/introduction)
- [openai-cookbook](https://github.com/openai/openai-cookbook/)
