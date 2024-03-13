<!-- badges: start -->
[![R-CMD-check](https://github.com/cezarykuran/oaii/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cezarykuran/oaii/actions/workflows/R-CMD-check.yaml)
[![CodeFactor](https://www.codefactor.io/repository/github/cezarykuran/oaii/badge)](https://www.codefactor.io/repository/github/cezarykuran/oaii)
[![CRAN](https://www.r-pkg.org/badges/version/oaii)](https://cran.r-project.org/package=oaii)
[![CRAN](http://cranlogs.r-pkg.org/badges/last-month/oaii)](https://cran.r-project.org/package=oaii)
<!-- badges: end -->

# OpenAI API R implementation

The "oaii" package is a well-designed R library, crafted specifically to interface seamlessly with the OpenAI API. This package unlocks a broad spectrum of capabilities for R users, including text generation, language translation, and question-answering functionalities. By leveraging the robust capabilities of the API, the "oaii" package offers a comprehensive toolkit for users, enabling them to harness the full power of state-of-the-art AI models in their R-based applications.

**Implemented endpoint helpers:**

- stable: audio, chat, completions, embeddings, files, fine tunes, images, models, moderations
- beta: assistants, threads, messages, runs


## Package instalation

**stable**  
`install.packages('oaii')`  
or  
`remotes::install_github("https://github.com/cezarykuran/oaii.git", subdir = "package", ref = "master")`  
`devtools::install_github("https://github.com/cezarykuran/oaii.git", subdir = "package", ref = "master")`

**develop**  
`remotes::install_github("https://github.com/cezarykuran/oaii.git", subdir = "package", ref = "develop")`  
`devtools::install_github("https://github.com/cezarykuran/oaii.git", subdir = "package", ref = "develop")`

You will need to provide your OpenAI API key to interact with the API.

## Demo applications

There exists a sister package named [oaiiDemoApps](https://github.com/cezarykuran/oaiiDemoApps) containing applications that demonstrate the practical use of the `oaii` package.
The [oaiiDemoApps](https://github.com/cezarykuran/oaiiDemoApps) package contains:

- An interactive Shiny demo application, which can be accessed at https://r.cezarykuran.it/shiny/oaii/ (feel free to explore its functionality!).
- Several demonstration terminal applications written as R scripts. These applications serve as practical examples, demonstrating how to effectively leverage the package's functions for API communication.

## Contributions

Contributions to the package are welcome. If you encounter any issues or have suggestions for improving the package, feel free to open an issue or submit a pull request.

## Other

**[OpenAI](https://openai.com/) useful links:**

- [API reference](https://platform.openai.com/docs/api-reference/)
- [API help pages](https://help.openai.com/en/collections/3675931-openai-api)
- [docs](https://platform.openai.com/docs/introduction)
- [openai-cookbook](https://github.com/openai/openai-cookbook/)
