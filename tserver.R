setwd("C:/Users/harla/Desktop/Coursera/jhdscapstone")

library(shiny)
library(ggplot2)
library(data.table)
library(dplyr)
library(plyr)
library(tidyr)

unigram <- as.data.table(read.csv("./data/unigram.csv"))
bigram.s <- as.data.table(read.csv("./data/bigram.csv"))
trigram.s <- as.data.table(read.csv("./data/trigram.csv"))
quadgram.s <- as.data.table(read.csv("./data/quadgram.csv"))
pentagram.s <- as.data.table(read.csv("./data/pentagram.csv"))

unigram <- unigram[, feature:=as.character(feature)]

# Server
shinyServer(function(input, output) {
      
      user.word.list <- reactive({
            as.list(strsplit(input$usertext, '\\s+')[[1]])
      })
      
      user.length <- reactive({
            length(user.word.list())
      })
      
      last.word <- reactive({
            user.word.list()[[user.length()]]
      })
      
      n1.word <- reactive({
            user.word.list()[[user.length() - 1]]
      })
      
      n2.word <- reactive({
            user.word.list()[[user.length() - 2]]
      })
      
      n3.word <- reactive({
            user.word.list()[[user.length() - 3]]
      })
      
# Predicted Word      
      
      output$pred.word <- renderText({
            
            if(user.length() > 3) {
            
            pentagram.base <- reactive({
                  filter(pentagram.s, base1 == n3.word(), base2 == n2.word(), base3 == n1.word(),
                         base4 == last.word())
            })
            
            if(nrow(pentagram.base()) > 0) {
                  
                        as.character(pentagram.base()[1, 6])
                  
            } else {
                  
                  quadgram.base <- reactive({
                        filter(quadgram.s, base1 == n2.word(), base2 == n1.word(), base3 == last.word())
                  })
                  
                  if(nrow(quadgram.base()) > 0) {
                        
                        as.character(quadgram.base()[1, 5])
                        
                  } else {
                        
                        trigram.base <- reactive({
                              filter(trigram.s, base1 == n1.word(), base2 == last.word())
                        })
                        
                        if(nrow(trigram.base()) > 0) {
                              
                              as.character(trigram.base()[1, 4])
                              
                        } else {
                              
                              bigram.base <- reactive({
                                    filter(bigram.s, base1 == last.word())
                              })
                              
                              if(nrow(bigram.base()) > 0) {
                                    
                                    as.character(bigram.base()[1, 3])
                                    
                              } else {
                                    
                                    unigram <- setorder(unigram, -probability)
                                    if(unigram[1, 1] == last.word()) {
                                          
                                          as.character(unigram[2, 2])
                                          
                                    } else {
                                          
                                          as.character(unigram[1, 2])
                                          
                                    } 
                              }
                              
                        }
                  }
                        
            }
            
      } else if(user.length() == 3) {
                  
                  quadgram.base <- reactive({
                        filter(quadgram.s, base1 == n2.word(), base2 == n1.word(), base3 == last.word())
                  })
                  
                  if(nrow(quadgram.base()) > 0) {
                        
                        as.character(quadgram.base()[1, 5])
                        
                  } else {
                        
                        trigram.base <- reactive({
                              filter(trigram.s, base1 == n1.word(), base2 == last.word())
                        })
                        
                        if(nrow(trigram.base()) > 0) {
                              
                              as.character(trigram.base()[1, 4])
                              
                        } else {
                              
                              bigram.base <- reactive({
                                    filter(bigram.s, base1 == last.word())
                              })
                              
                              if(nrow(bigram.base()) > 0) {
                                    
                                    as.character(bigram.base()[1, 3])
                                    
                              } else {
                                    
                                    unigram <- setorder(unigram, -probability)
                                    if(unigram[1, 2] == last.word()) {
                                          
                                          as.character(unigram[2, 2])
                                          
                                    } else {
                                          
                                          as.character(unigram[1, 2])
                                          
                                    }
                              }
                        }
                  }
                  
      } else if(user.length() == 2) {
                  
                  trigram.base <- reactive({
                        filter(trigram.s, base1 == n1.word(), base2 == last.word())
                  })
                  
                  if(nrow(trigram.base()) > 0) {
                        
                        as.character(trigram.base()[1, 4])
                        
                  } else {
                        
                        bigram.base <- reactive({
                              filter(bigram.s, base1 == last.word())
                        })
                        
                        if(nrow(bigram.base()) > 0) {
                              
                              as.character(bigram.base()[1, 3])
                              
                        } else {
                              
                              unigram <- setorder(unigram, -probability)
                              if(unigram[1, 1] == last.word()) {
                                    
                                          as.character(unigram[2, 2])
                                    
                                    } else {
                                    
                                          as.character(unigram[1, 2])
                                    
                                    } 
                        }
                  }      
      } else {
                  
                  bigram.base <- reactive({
                        filter(bigram.s, base1 == last.word())
                  })
                  
                  if(nrow(bigram.base()) > 0) {
                        
                        as.character(bigram.base()[1, 3])
                        
                  } else {
                        
                        unigram <- setorder(unigram, -probability)
                        if(unigram[1, 2] == last.word()) {
                              
                              as.character(unigram[2, 2])
                              
                        } else {
                              
                              as.character(unigram[1, 2])
                              
                        } 
                  }      
            }
      })

# Alternative Suggestions      
      
      output$sugg.words <- renderText({
            
            if(user.length() > 3) {
                  
                  pentagram.base <- reactive({
                        filter(pentagram.s, base1 == n3.word(), base2 == n2.word(), base3 == n1.word(),
                               base4 == last.word())
                  })
                  
                  if(nrow(pentagram.base()) > 0) {
                        
                        if(nrow(pentagram.base()) == 1) {
                              
                              print("No Suggestions")
                              
                        } else {
                              
                              as.character(na.omit(pentagram.base()[2:5, 6]))
                              
                        }
                        
                  } else {
                        
                        quadgram.base <- reactive({
                              filter(quadgram.s, base1 == n2.word(), base2 == n1.word(), base3 == last.word())
                        })
                        
                        if(nrow(quadgram.base()) > 0) {
                              
                              if(nrow(quadgram.base()) == 1) {
                                    
                                    print("No Suggestions")
                                    
                              } else {
                                    
                                    as.character(na.omit(quadgram.base()[2:5, 5]))
                                    
                              }
                              
                        } else {
                              
                              trigram.base <- reactive({
                                    filter(trigram.s, base1 == n1.word(), base2 == last.word())
                              })
                              
                              if(nrow(trigram.base()) > 0) {
                                    
                                    if(nrow(trigram.base()) == 1) {
                                          
                                          print("No Suggestions")
                                          
                                    } else {
                                          
                                          as.character(na.omit(trigram.base()[2:5, 4]))
                                          
                                    }
                                    
                              } else {
                                    
                                    bigram.base <- reactive({
                                          filter(bigram.s, base1 == last.word())
                                    })
                                    
                                    if(nrow(bigram.base()) > 0) {
                                          
                                          if(nrow(bigram.base()) == 1) {
                                                
                                                as.character(na.omit(unigram[1:4, 2]))
                                                
                                          } else {
                                                
                                                as.character(na.omit(bigram.base()[2:5, 3]))
                                                
                                          }
                                          
                                    } else {
                                          
                                          unigram <- setorder(unigram, -probability)
                                          if(unigram[1, 2] == last.word()) {
                                                
                                                as.character(na.omit(unigram[3:6, 2]))
                                                
                                          } else {
                                                
                                                as.character(na.omit(unigram[2:5, 2]))
                                                
                                          } 
                                    }
                                    
                              }
                        }
                        
                  }
                  
            } else if(user.length() == 3) {
                  
                  quadgram.base <- reactive({
                        filter(quadgram.s, base1 == n2.word(), base2 == n1.word(), base3 == last.word())
                  })
                  
                  if(nrow(quadgram.base()) > 0) {
                        
                        if(nrow(quadgram.base()) == 1) {
                              
                              print("No Suggestions")
                              
                        } else {
                              
                              as.character(na.omit(quadgram.base()[2:5, 5]))
                              
                        }
                        
                  } else {
                        
                        trigram.base <- reactive({
                              filter(trigram.s, base1 == n1.word(), base2 == last.word())
                        })
                        
                        if(nrow(trigram.base()) > 0) {
                              
                              if(nrow(trigram.base()) == 1) {
                                    
                                    print("No Suggestions")
                                    
                              } else {
                                    
                                    as.character(na.omit(trigram.base()[2:5, 4]))
                                    
                              }
                              
                        } else {
                              
                              bigram.base <- reactive({
                                    filter(bigram.s, base1 == last.word())
                              })
                              
                              if(nrow(bigram.base()) > 0) {
                                    
                                    if(nrow(bigram.base()) == 1) {
                                          
                                          as.character(na.omit(unigram[1:4, 2]))
                                          
                                    } else {
                                          
                                          as.character(na.omit(bigram.base()[2:5, 3]))
                                          
                                    }
                                    
                              } else {
                                    
                                    unigram <- setorder(unigram, -probability)
                                    if(unigram[1, 2] == last.word()) {
                                          
                                          as.character(na.omit(unigram[3:6, 2]))
                                          
                                    } else {
                                          
                                          as.character(na.omit(unigram[2:5, 2]))
                                          
                                    }
                              }
                        }
                  }
                  
            } else if(user.length() == 2) {
                  
                  trigram.base <- reactive({
                        filter(trigram.s, base1 == n1.word(), base2 == last.word())
                  })
                  
                  if(nrow(trigram.base()) > 0) {
                        
                        if(nrow(trigram.base()) == 1) {
                              
                              print("No Suggestions")
                              
                        } else {
                              
                              as.character(na.omit(trigram.base()[2:5, 4]))
                              
                        }
                        
                  } else {
                        
                        bigram.base <- reactive({
                              filter(bigram.s, base1 == last.word())
                        })
                        
                        if(nrow(bigram.base()) > 0) {
                              
                              if(nrow(bigram.base()) == 1) {
                                    
                                    as.character(na.omit(unigram[1:4, 2]))
                                    
                              } else {
                                    
                                    as.character(na.omit(bigram.base()[2:5, 3]))
                                    
                              }
                              
                        } else {
                              
                              unigram <- setorder(unigram, -probability)
                              if(unigram[1, 2] == last.word()) {
                                    
                                    as.character(na.omit(unigram[3:6, 2]))
                                    
                              } else {
                                    
                                    as.character(na.omit(unigram[2:5, 2]))
                                    
                              } 
                        }
                  }      
            
            } else {
                  
                  bigram.base <- reactive({
                        filter(bigram.s, base1 == last.word())
                  })
                  
                  if(nrow(bigram.base()) > 0) {
                        
                        if(nrow(bigram.base()) == 1) {
                              
                              as.character(na.omit(unigram[1:4, 2]))
                              
                        } else {
                              
                              as.character(na.omit(bigram.base()[2:5, 3]))
                              
                        }
                        
                  } else {
                        
                        unigram <- setorder(unigram, -probability)
                        if(unigram[1, 2] == last.word()) {
                              
                              as.character(na.omit(unigram[3:6, 2]))
                              
                        } else {
                              
                              as.character(na.omit(unigram[2:5, 2]))
                              
                        } 
                  }      
            }
      })
                  
# Plot of Frequencies
      
      output$plot1 <- renderPlot({
            
            if(user.length() > 3) {
                  
                  pentagram.base <- reactive({
                        filter(pentagram.s, base1 == n3.word(), base2 == n2.word(), base3 == n1.word(),
                               base4 == last.word())
                  })
                  
                  if(nrow(pentagram.base()) > 0) {
                        
                        ggplot(na.omit(pentagram.base()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                              geom_point() + 
                              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                              labs(x = "Pentagram Predicted Words")
                        
                  } else {
                        
                        quadgram.base <- reactive({
                              filter(quadgram.s, base1 == n2.word(), base2 == n1.word(), base3 == last.word())
                        })
                        
                        if(nrow(quadgram.base()) > 0) {
                              
                              ggplot(na.omit(quadgram.base()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                                    geom_point() + 
                                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                    labs(x = "Quadgram Predicted Words")
                              
                        } else {
                              
                              trigram.base <- reactive({
                                    filter(trigram.s, base1 == n1.word(), base2 == last.word())
                              })
                              
                              if(nrow(trigram.base()) > 0) {
                                    
                                    ggplot(na.omit(trigram.base()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                                          geom_point() + 
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                          labs(x = "Trigram Predicted Words")
                                    
                              } else {
                                    
                                    bigram.base <- reactive({
                                          filter(bigram.s, base1 == last.word())
                                    })
                                    
                                    if(nrow(bigram.base()) > 0) {
                                          
                                          ggplot(na.omit(bigram.base()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                                                geom_point() + 
                                                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                                labs(x = "Bigrams")
                                          
                                    } else {
                                          
                                          unigram <- setorder(unigram, -probability)
                                                
                                                ggplot(na.omit(unigram[1:50,]), aes(x = reorder(feature, -frequency), y = frequency)) +
                                                      geom_point() + 
                                                      theme(axis.text.x = element_text(angle = 90, hjust = 1))
                                                
                                          } 
                                    }
                                    
                              }
                        }
                        
            } else if(user.length() == 3) {
                  
                  quadgram.base <- reactive({
                        filter(quadgram.s, base1 == n2.word(), base2 == n1.word(), base3 == last.word())
                  })
                  
                  if(nrow(quadgram.base()) > 0) {
                        
                        ggplot(na.omit(quadgram.base()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                              geom_point() + 
                              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                              labs(x = "Quadgram Predicted Words")
                        
                  } else {
                        
                        trigram.base <- reactive({
                              filter(trigram.s, base1 == n1.word(), base2 == last.word())
                        })
                        
                        if(nrow(trigram.base()) > 0) {
                              
                              ggplot(na.omit(trigram.base()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                                    geom_point() + 
                                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                    labs(x = "Trigram Predicted Words")
                              
                        } else {
                              
                              bigram.base <- reactive({
                                    filter(bigram.s, base1 == last.word())
                              })
                              
                              if(nrow(bigram.base()) > 0) {
                                    
                                    ggplot(na.omit(bigram.base()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                                          geom_point() + 
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                          labs(x = "Bigrams")
                                    
                              } else {
                                    
                                    unigram <- setorder(unigram, -probability)
                                    
                                    ggplot(na.omit(unigram[1:50,]), aes(x = reorder(feature, -frequency), y = frequency)) +
                                          geom_point() + 
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1))
                                          
                                    }
                              }
                        }
            
            } else if(user.length() == 2) {
                  
                  trigram.base <- reactive({
                        filter(trigram.s, base1 == n1.word(), base2 == last.word())
                  })
                  
                  if(nrow(trigram.base()) > 0) {
                        
                        ggplot(na.omit(trigram.base()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                              geom_point() + 
                              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                              labs(x = "Trigram Predicted Words")
                        
                  } else {
                        
                        bigram.base <- reactive({
                              filter(bigram.s, base1 == last.word())
                        })
                        
                        if(nrow(bigram.base()) > 0) {
                              
                              ggplot(na.omit(bigram.base()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                                    geom_point() + 
                                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                    labs(x = "Bigrams")
                              
                        } else {
                              
                              unigram <- setorder(unigram, -probability)
                              
                              ggplot(na.omit(unigram[1:50,]), aes(x = reorder(feature, -frequency), y = frequency)) +
                                    geom_point() + 
                                    theme(axis.text.x = element_text(angle = 90, hjust = 1))
                                    
                        } 
                  }
            } else {
                  
                  bigram.base <- reactive({
                        filter(bigram.s, base1 == last.word())
                  })
                  
                  if(nrow(bigram.base()) > 0) {
                        
                        ggplot(na.omit(bigram.base()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                              geom_point() + 
                              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                              labs(x = "Bigrams")
                        
                  } else {
                        
                        unigram <- setorder(unigram, -probability)
                        
                        ggplot(na.omit(unigram[1:50,]), aes(x = reorder(feature, -frequency), y = frequency)) +
                              geom_point() + 
                              theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
                  }      
            }
            
      })
            
})