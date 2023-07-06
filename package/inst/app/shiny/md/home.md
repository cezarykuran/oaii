### Intro
This is **just a demo app** to get you familiar with the [OpenAI API](https://platform.openai.com/docs/api-reference/) (you can also find a lot of interesting information at [OpenAI API help pages](https://help.openai.com/en/collections/3675931-openai-api)). The code of this application uses functions from the oaii package, so you can see how to use them. The package is located in the GitHub repository at https://github.com/cezarykuran/oaii/.

For full operation, the application requires an "api key". **It is not saved or stored anywhere**. If this does not convince you, download the repository and run the application locally (`oaii:demoShinyApp()`)

### Application parts

#### chat  

What can I say, go see for yourself, and have fun!  

#### files, fine-tunes and completions

At this point, this is probably the most interesting part of this application. These three elements allow you to train models and then use them. You have to:
- upload jsonl file (tab files)
- create model (tab fine-tunes)
- use model (tab completions)

You can learn more at https://platform.openai.com/docs/guides/fine-tuning.  
See also https://github.com/openai/openai-cookbook/.

#### image generator, image edit

These two modules have little use (at least at this point) in science, etc. But ... they allow you to play with image processing/generation by AI using the R / Shiny interface.
