pacman::p_load(shiny, shiny.semantic, tidyverse, purrr, furrr, keras)
#load("data/wiki_tokens.Rdata")
source("helper.R")

s_cnn_gru_char <- keras::load_model_hdf5("../models/cnn_gru_char")
char_tokenizer <- keras::load_text_tokenizer("../models/char_tokenizer_mac")

ui <- function(){
  semanticPage(
    div(class = "ui red header", "Ideological Tagging"),
    div(class = "ui form",
      div(class = "fields",
        div(class = "ten wide field",
          div(class = "ui fluid input",
            shiny::tags$input(type="text", id = "target_text", value = "For too long, a small group in our nation's Capital has reaped the rewards of government while the people have borne the cost. Washington flourished -- but the people did not share in its wealth. Politicians prospered -- but the jobs left, and the factories closed.
The establishment protected itself, but not the citizens of our country. Their victories have not been your victories; their triumphs have not been your triumphs; and while they celebrated in our nation's capital, there was little to celebrate for struggling families all across our land.
                              That all changes -- starting right here, and right now, because this moment is your moment: it belongs to you.
                              It belongs to everyone gathered here today and everyone watching all across America. This is your day. This is your celebration. And this, the United States of America, is your country.
                              What truly matters is not which party controls our government, but whether our government is controlled by the people. January 20th 2017, will be remembered as the day the people became the rulers of this nation again. The forgotten men and women of our country will be forgotten no longer.
                              Everyone is listening to you now.
                              You came by the tens of millions to become part of a historic movement the likes of which the world has never seen before. At the center of this movement is a crucial conviction: that a nation exists to serve its citizens.
                              Americans want great schools for their children, safe neighborhoods for their families, and good jobs for themselves. These are the just and reasonable demands of a righteous public.
                              But for too many of our citizens, a different reality exists: Mothers and children trapped in poverty in our inner cities; rusted-out factories scattered like tombstones across the landscape of our nation; an education system flush with cash, but which leaves our young and beautiful students deprived of knowledge; and the crime and gangs and drugs that have stolen too many lives and robbed our country of so much unrealized potential.
                              This American carnage stops right here and stops right now.
                              We are one nation -- and their pain is our pain. Their dreams are our dreams; and their success will be our success. We share one heart, one home, and one glorious destiny.
                              The oath of office I take today is an oath of allegiance to all Americans.
                              For many decades, we've enriched foreign industry at the expense of American industry; subsidized the armies of other countries while allowing for the very sad depletion of our military; we've defended other nation's borders while refusing to defend our own; and spent trillions of dollars overseas while America's infrastructure has fallen into disrepair and decay.
                              We've made other countries rich while the wealth, strength, and confidence of our country has disappeared over the horizon.
                              One by one, the factories shuttered and left our shores, with not even a thought about the millions upon millions of American workers left behind.
                              The wealth of our middle class has been ripped from their homes and then redistributed across the entire world.
                              But that is the past. And now we are looking only to the future. We assembled here today are issuing a new decree to be heard in every city, in every foreign capital, and in every hall of power.
                              From this day forward, a new vision will govern our land.
                              From this moment on, it's going to be America First.
                              Every decision on trade, on taxes, on immigration, on foreign affairs, will be made to benefit American workers and American families. We must protect our borders from the ravages of other countries making our products, stealing our companies, and destroying our jobs. Protection will lead to great prosperity and strength.
                              I will fight for you with every breath in my body -- and I will never, ever let you down.
                              America will start winning again, winning like never before.
                              We will bring back our jobs. We will bring back our borders. We will bring back our wealth. And we will bring back our dreams.
                              We will build new roads, and highways, and bridges, and airports, and tunnels, and railways all across our wonderful nation.
                              We will get our people off of welfare and back to work -- rebuilding our country with American hands and American labor.
                              We will follow two simple rules: Buy American and hire American.
                              We will seek friendship and goodwill with the nations of the world -- but we do so with the understanding that it is the right of all nations to put their own interests first.
                              We do not seek to impose our way of life on anyone, but rather to let it shine as an example for everyone to follow.
                              We will reinforce old alliances and form new ones -- and unite the civilized world against radical Islamic terrorism, which we will eradicate completely from the face of the Earth.
                              At the bedrock of our politics will be a total allegiance to the United States of America, and through our loyalty to our country, we will rediscover our loyalty to each other.")
          )
        ),
        div(class = "four wide field",
          dropdown("outcome", choices = outcomes, value = outcomes[1])
        ),
        div(class = "two wide field",
          actionButton("predict", label = "Predict", class = "ui button")
        )
      )    
    ),
    uiOutput("text_output"),
    plotOutput("probs")
  )
}

server <- function(input, output, session){
  
  input_text <- eventReactive(input$predict, {
    
    if(is.null(input$target_text)) return(NULL)
    if(input$target_text == "") return(NULL)
    
    char_maxlen <- 300L
    
    text_container <- input$target_text %>% 
    #"hahahah asdjwd wie gehts? und asdk dsaskldnajksfdadffdmsc asdjfa dfa dsasda afd sf fsdf  sdf" %>%
      transform_text(., n_grams = 5) %>% 
      transform_seq(char_tokenizer, char_maxlen) %>% 
      keras_predict(s_cnn_gru_char)
    
    return(text_container)
  })
  
  output$probs <- renderPlot({
    
    req(input_text())
    
    input_text()$probs %>% 
      gather(variable, probability) %>% 
      mutate(variable = forcats::fct_reorder(variable, probability)) %>%
      ggplot(aes(variable, probability)) +
      geom_point() +
      coord_flip() 
      #geom_hline(yintercept = mean(res_cnn_gru$accuracy)) +
      #ggtitle("Multi-Class Prediction Accuracy from hold-out Test Set", subtitle = "CNN-GRU (word level)")
  })
  
  # output$text_output <- renderUI({
  #   req(input_text())
  #   input_text()$data$word
  # })
  
}

shinyApp(ui(), server)