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
                  penta.p1 <- reactive({
                        pentagram.base()$frequency/sum(pentagram.s$frequency)
                  })
                  penta.p2 <- reactive({
                        as.numeric(rep((filter(quadgram.s, base1 == n3.word(), base2 == n2.word(), 
                                               base3 == n1.word(), predicted == last.word())[, 6]), times = length(penta.p1())))
                  })
                  penta.p3 <- reactive({
                        as.numeric(rep(sum(quadgram.s$frequency), times = length(penta.p1())))
                  })
                  penta.p4 <- reactive({
                        penta.p2()/penta.p3()
                  })
                  pentaprobability <- reactive({
                        penta.p1()/penta.p4()
                  })
                  pentagram.base.2 <- reactive({
                        cbind(pentagram.base(), pentaprobability())
                  })
                        as.character(pentagram.base.2()[1, 6])
                  
            } else {
                  
                  quadgram.base <- reactive({
                        filter(quadgram.s, base1 == n2.word(), base2 == n1.word(), base3 == last.word())
                  })
                  
                  if(nrow(quadgram.base()) > 0) {
                        q.p1 <- reactive({
                              quadgram.base()$frequency/sum(quadgram.s$frequency)
                        })
                        q.p2 <- reactive({
                              as.numeric(rep((filter(trigram.s, base1 == n2.word(), base2 == n1.word(), predicted == last.word())[, 5]), times = length(q.p1())))
                        })
                        q.p3 <- reactive({
                              as.numeric(rep(sum(trigram.s$frequency), times = length(q.p1())))
                        })
                        q.p4 <- reactive({
                              q.p2()/q.p3()
                        })
                        quadprobability <- reactive({
                              q.p1()/q.p4()
                        })
                        quadgram.base.2 <- reactive({
                              cbind(quadgram.base(), quadprobability())
                        })
                        as.character(quadgram.base.2()[1, 5])
                        
                  } else {
                        
                        trigram.base <- reactive({
                              filter(trigram.s, base1 == n1.word(), base2 == last.word())
                        })
                        
                        if(nrow(trigram.base()) > 0) {
                              t.p1 <- reactive({
                                    trigram.base()$frequency/sum(trigram.s$frequency)
                              })
                              t.p2 <- reactive({
                                    as.numeric(rep((filter(bigram.s, base1 == n1.word(), predicted == last.word())[, 4]), times = length(t.p1())))
                              })
                              t.p3 <- reactive({
                                    as.numeric(rep(sum(bigram.s$frequency), times = length(t.p1())))
                              })
                              t.p4 <- reactive({
                                    t.p2()/t.p3()
                              })
                              triprobability <- reactive({
                                    t.p1()/t.p4()
                              })
                              trigram.base.2 <- reactive({
                                    cbind(trigram.base(), triprobability())
                              })
                              as.character(trigram.base.2()[1, 4])
                              
                        } else {
                              
                              bigram.base <- reactive({
                                    filter(bigram.s, base1 == last.word())
                              })
                              
                              if(nrow(bigram.base()) > 0) {
                                    b.p1 <- reactive({
                                          bigram.base()$frequency/sum(bigram.s$frequency)
                                    })
                                    b.p2 <- reactive({
                                          as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                                    })
                                    biprobability <- reactive({
                                          b.p1()/b.p2()
                                    })
                                    bigram.base.2 <- reactive({
                                          cbind(bigram.base(), biprobability())
                                    })
                                    as.character(bigram.base.2()[1, 3])
                                    
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
                        q.p1 <- reactive({
                              quadgram.base()$frequency/sum(quadgram.s$frequency)
                        })
                        q.p2 <- reactive({
                              as.numeric(rep((filter(trigram.s, base1 == n2.word(), base2 == n1.word(), predicted == last.word())[, 5]), times = length(q.p1())))
                        })
                        q.p3 <- reactive({
                              as.numeric(rep(sum(trigram.s$frequency), times = length(q.p1())))
                        })
                        q.p4 <- reactive({
                              q.p2()/q.p3()
                        })
                        quadprobability <- reactive({
                              q.p1()/q.p4()
                        })
                        quadgram.base.2 <- reactive({
                              cbind(quadgram.base(), quadprobability())
                        })
                        as.character(quadgram.base()[1, 5])
                        
                  } else {
                        
                        trigram.base <- reactive({
                              filter(trigram.s, base1 == n1.word(), base2 == last.word())
                        })
                        
                        if(nrow(trigram.base()) > 0) {
                              t.p1 <- reactive ({
                                    trigram.base()$frequency/sum(trigram.s$frequency)
                              })
                              t.p2 <- reactive({
                                    as.numeric(rep((filter(bigram.s, base1 == n1.word(), predicted == last.word())[, 4]), times = length(t.p1())))
                              })
                              t.p3 <- reactive({
                                    as.numeric(rep(sum(bigram.s$frequency), times = length(t.p1())))
                              })
                              t.p4 <- reactive({
                                    t.p2()/t.p3()
                              })
                              triprobability <- reactive({
                                    t.p1()/t.p4()
                              })
                              trigram.base.2 <- reactive({
                                    cbind(trigram.base(), triprobability())
                              })
                              as.character(trigram.base.2()[1, 4])
                              
                        } else {
                              
                              bigram.base <- reactive({
                                    filter(bigram.s, base1 == last.word())
                              })
                              
                              if(nrow(bigram.base()) > 0) {
                                    b.p1 <- reactive({
                                          bigram.base()$frequency/sum(bigram.s$frequency)
                                    })
                                    b.p2 <- reactive({
                                          as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                                    })
                                    biprobability <- reactive({
                                          b.p1()/b.p2()
                                    })
                                    bigram.base.2 <- reactive({
                                          cbind(bigram.base(), biprobability())
                                    })
                                    as.character(bigram.base.2()[1, 3])
                                    
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
                        t.p1 <- reactive({
                              trigram.base()$frequency/sum(trigram.s$frequency)
                        })
                        t.p2 <- reactive({
                              as.numeric(rep((filter(bigram.s, base1 == n1.word(), predicted == last.word())[, 4]), times = length(t.p1())))
                        })
                        t.p3 <- reactive({
                              as.numeric(rep(sum(bigram.s$frequency), times = length(t.p1())))
                        })
                        t.p4 <- reactive({
                              t.p2()/t.p3()
                        })
                        triprobability <- reactive({
                              t.p1()/t.p4()
                        })
                        trigram.base.2 <- reactive({
                              cbind(trigram.base(), triprobability())
                        })
                        as.character(trigram.base.2()[1, 4])
                        
                  } else {
                        
                        bigram.base <- reactive({
                              filter(bigram.s, base1 == last.word())
                        })
                        
                        if(nrow(bigram.base()) > 0) {
                              b.p1 <- reactive({
                                    bigram.base()$frequency/sum(bigram.s$frequency)
                              })
                              b.p2 <- reactive({
                                    as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                              })
                              biprobability <- reactive({
                                    b.p1()/b.p2()
                              })
                              bigram.base.2 <- reactive({
                                    cbind(bigram.base(), biprobability())
                              })
                              as.character(bigram.base.2()[1, 3])
                              
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
                        b.p1 <- reactive({
                              bigram.base()$frequency/sum(bigram.s$frequency)
                        })
                        b.p2 <- reactive({
                              as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                        })
                        biprobability <- reactive({
                              b.p1()/b.p2()
                        })
                        bigram.base.2 <- reactive({
                              cbind(bigram.base(), biprobability())
                        })
                        as.character(bigram.base.2()[1, 3])
                        
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
                        penta.p1 <- reactive({
                              pentagram.base()$frequency/sum(pentagram.s$frequency)
                        })
                        penta.p2 <- reactive({
                              as.numeric(rep((filter(quadgram.s, base1 == n3.word(), base2 == n2.word(), 
                                                     base3 == n1.word(), predicted == last.word())[, 6]), times = length(penta.p1())))
                        })
                        penta.p3 <- reactive({
                              as.numeric(rep(sum(quadgram.s$frequency), times = length(penta.p1())))
                        })
                        penta.p4 <- reactive({
                              penta.p2()/penta.p3()
                        })
                        pentaprobability <- reactive({
                              penta.p1()/penta.p4()
                        })
                        pentagram.base.2 <- reactive({
                              cbind(pentagram.base(), pentaprobability())
                        })
                        
                        if(nrow(pentagram.base.2()) == 1) {
                              
                              print("No Suggestions")
                              
                        } else {
                              
                              as.character(na.omit(pentagram.base.2()[2:5, 6]))
                              
                        }
                        
                  } else {
                        
                        quadgram.base <- reactive({
                              filter(quadgram.s, base1 == n2.word(), base2 == n1.word(), base3 == last.word())
                        })
                        
                        if(nrow(quadgram.base()) > 0) {
                              q.p1 <- reactive({
                                    quadgram.base()$frequency/sum(quadgram.s$frequency)
                              })
                              q.p2 <- reactive({
                                    as.numeric(rep((filter(trigram.s, base1 == n2.word(), base2 == n1.word(), predicted == last.word())[, 5]), times = length(q.p1())))
                              })
                              q.p3 <- reactive({
                                    as.numeric(rep(sum(trigram.s$frequency), times = length(q.p1())))
                              })
                              q.p4 <- reactive({
                                    q.p2()/q.p3()
                              })
                              quadprobability <- reactive({
                                    q.p1()/q.p4()
                              })
                              quadgram.base.2 <- reactive({
                                    cbind.fill(quadgram.base(), quadprobability())
                              })
                              
                              if(nrow(quadgram.base.2()) == 1) {
                                    
                                    print("No Suggestions")
                                    
                              } else {
                                    
                                    as.character(na.omit(quadgram.base.2()[2:5, 5]))
                                    
                              }
                              
                        } else {
                              
                              trigram.base <- reactive({
                                    filter(trigram.s, base1 == n1.word(), base2 == last.word())
                              })
                              
                              if(nrow(trigram.base()) > 0) {
                                    t.p1 <- reactive({
                                          trigram.base()$frequency/sum(trigram.s$frequency)
                                    })
                                    t.p2 <- reactive({
                                          as.numeric(rep((filter(bigram.s, base1 == n1.word(), predicted == last.word())[, 4]), times = length(t.p1())))
                                    })
                                    t.p3 <- reactive({
                                          as.numeric(rep(sum(bigram.s$frequency), times = length(t.p1())))
                                    })
                                    t.p4 <- reactive({
                                          t.p2()/t.p3()
                                    })
                                    triprobability <- reactive({
                                          t.p1()/t.p4()
                                    })
                                    trigram.base.2 <- reactive({
                                          cbind(trigram.base(), triprobability())
                                    })
                                    
                                    if(nrow(trigram.base.2()) == 1) {
                                          
                                          print("No Suggestions")
                                          
                                    } else {
                                          
                                          as.character(na.omit(trigram.base.2()[2:5, 4]))
                                          
                                    }
                                    
                              } else {
                                    
                                    bigram.base <- reactive({
                                          filter(bigram.s, base1 == last.word())
                                    })
                                    
                                    if(nrow(bigram.base()) > 0) {
                                          b.p1 <- reactive({
                                                bigram.base()$frequency/sum(bigram.s$frequency)
                                          })
                                          b.p2 <- reactive({
                                                as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                                          })
                                          biprobability <- reactive({
                                                b.p1()/b.p2()
                                          })
                                          bigram.base.2 <- reactive({
                                                cbind(bigram.base(), biprobability())
                                          })
                                          
                                          if(nrow(bigram.base.2()) == 1) {
                                                
                                                as.character(na.omit(unigram[1:4, 2]))
                                                
                                          } else {
                                                
                                                as.character(na.omit(bigram.base.2()[2:5, 3]))
                                                
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
                        q.p1 <- reactive({
                              quadgram.base()$frequency/sum(quadgram.s$frequency)
                        })
                        q.p2 <- reactive({
                              as.numeric(rep((filter(trigram.s, base1 == n2.word(), base2 == n1.word(), predicted == last.word())[, 5]), times = length(q.p1())))
                        })
                        q.p3 <- reactive({
                              as.numeric(rep(sum(trigram.s$frequency), times = length(q.p1())))
                        })
                        q.p4 <- reactive({
                              q.p2()/q.p3()
                        })
                        quadprobability <- reactive({
                              q.p1()/q.p4()
                        })
                        quadgram.base.2 <- reactive({
                              cbind(quadgram.base(), quadprobability())
                        })
                        
                        if(nrow(quadgram.base.2()) == 1) {
                              
                              print("No Suggestions")
                              
                        } else {
                              
                              as.character(na.omit(quadgram.base.2()[2:5, 5]))
                              
                        }
                        
                  } else {
                        
                        trigram.base <- reactive({
                              filter(trigram.s, base1 == n1.word(), base2 == last.word())
                        })
                        
                        if(nrow(trigram.base()) > 0) {
                              t.p1 <- reactive({
                                    trigram.base()$frequency/sum(trigram.s$frequency)
                              })
                              t.p2 <- reactive({
                                    as.numeric(rep((filter(bigram.s, base1 == n1.word(), predicted == last.word())[, 4]), times = length(t.p1())))
                              })
                              t.p3 <- reactive({
                                    as.numeric(rep(sum(bigram.s$frequency), times = length(t.p1())))
                              })
                              t.p4 <- reactive({
                                    t.p2()/t.p3()
                              })
                              triprobability <- reactive({
                                    t.p1()/t.p4()
                              })
                              trigram.base.2 <- reactive({
                                    cbind(trigram.base(), triprobability())
                              })
                              
                              if(nrow(trigram.base.2()) == 1) {
                                    
                                    print("No Suggestions")
                                    
                              } else {
                                    
                                    as.character(na.omit(trigram.base.2()[2:5, 4]))
                                    
                              }
                              
                        } else {
                              
                              bigram.base <- reactive({
                                    filter(bigram.s, base1 == last.word())
                              })
                              
                              if(nrow(bigram.base()) > 0) {
                                    b.p1 <- reactive({
                                          bigram.base()$frequency/sum(bigram.s$frequency)
                                    })
                                    b.p2 <- reactive({
                                          as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                                    })
                                    biprobability <- reactive({
                                          b.p1()/b.p2()
                                    })
                                    bigram.base.2 <- reactive({
                                          cbind(bigram.base(), biprobability())
                                    })
                                    
                                    if(nrow(bigram.base.2()) == 1) {
                                          
                                          as.character(na.omit(unigram[1:4, 2]))
                                          
                                    } else {
                                          
                                          as.character(na.omit(bigram.base.2()[2:5, 3]))
                                          
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
                        t.p1 <- reactive({
                              trigram.base()$frequency/sum(trigram.s$frequency)
                        })
                        t.p2 <- reactive({
                              as.numeric(rep((filter(bigram.s, base1 == n1.word(), predicted == last.word())[, 4]), times = length(t.p1())))
                        })
                        t.p3 <- reactive({
                              as.numeric(rep(sum(bigram.s$frequency), times = length(t.p1())))
                        })
                        t.p4 <- reactive({
                              t.p2()/t.p3()
                        })
                        triprobability <- reactive({
                              t.p1()/t.p4()
                        })
                        trigram.base.2 <- reactive({
                              cbind(trigram.base(), triprobability())
                        })
                        
                        if(nrow(trigram.base.2()) == 1) {
                              
                              print("No Suggestions")
                              
                        } else {
                              
                              as.character(na.omit(trigram.base.2()[2:5, 4]))
                              
                        }
                        
                  } else {
                        
                        bigram.base <- reactive({
                              filter(bigram.s, base1 == last.word())
                        })
                        
                        if(nrow(bigram.base()) > 0) {
                              b.p1 <- reactive({
                                    bigram.base()$frequency/sum(bigram.s$frequency)
                              })
                              b.p2 <- reactive({
                                    as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                              })
                              biprobability <- reactive({
                                    b.p1()/b.p2()
                              })
                              bigram.base.2 <- reactive({
                                    cbind(bigram.base(), biprobability())
                              })
                              
                              if(nrow(bigram.base.2()) == 1) {
                                    
                                    as.character(na.omit(unigram[1:4, 2]))
                                    
                              } else {
                                    
                                    as.character(na.omit(bigram.base.2()[2:5, 3]))
                                    
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
                        b.p1 <- reactive({
                              bigram.base()$frequency/sum(bigram.s$frequency)
                        })
                        b.p2 <- reactive({
                              as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                        })
                        biprobability <- reactive({
                              b.p1()/b.p2()
                        })
                        bigram.base.2 <- reactive({
                              cbind(bigram.base(), biprobability())
                        })
                        
                        if(nrow(bigram.base.2()) == 1) {
                              
                              as.character(na.omit(unigram[1:4, 2]))
                              
                        } else {
                              
                              as.character(na.omit(bigram.base.2()[2:5, 3]))
                              
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
                        penta.p1 <- reactive({
                              pentagram.base()$frequency/sum(pentagram.s$frequency)
                        })
                        penta.p2 <- reactive({
                              as.numeric(rep((filter(quadgram.s, base1 == n3.word(), base2 == n2.word(), 
                                                     base3 == n1.word(), predicted == last.word())[, 6]), times = length(penta.p1())))
                        })
                        penta.p3 <- reactive({
                              as.numeric(rep(sum(quadgram.s$frequency), times = length(penta.p1())))
                        })
                        penta.p4 <- reactive({
                              penta.p2()/penta.p3()
                        })
                        pentaprobability <- reactive({
                              penta.p1()/penta.p4()
                        })
                        pentagram.base.2 <- reactive({
                              cbind(pentagram.base(), pentaprobability())
                        })
                        
                        ggplot(na.omit(pentagram.base.2()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                              geom_point() + 
                              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                              labs(x = "Pentagram Predicted Words")
                        
                  } else {
                        
                        quadgram.base <- reactive({
                              filter(quadgram.s, base1 == n2.word(), base2 == n1.word(), base3 == last.word())
                        })
                        
                        if(nrow(quadgram.base()) > 0) {
                              q.p1 <- reactive({
                                    quadgram.base()$frequency/sum(quadgram.s$frequency)
                              })
                              q.p2 <- reactive({
                                    as.numeric(rep((filter(trigram.s, base1 == n2.word(), base2 == n1.word(), predicted == last.word())[, 5]), times = length(q.p1())))
                              })
                              q.p3 <- reactive({
                                    as.numeric(rep(sum(trigram.s$frequency), times = length(q.p1())))
                              })
                              q.p4 <- reactive({
                                    q.p2()/q.p3()
                              })
                              quadprobability <- reactive({
                                    q.p1()/q.p4()
                              })
                              quadgram.base.2 <- reactive({
                                    cbind(quadgram.base(), quadprobability())
                              })
                              
                              ggplot(na.omit(quadgram.base.2()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                                    geom_point() + 
                                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                    labs(x = "Quadgram Predicted Words")
                              
                        } else {
                              
                              trigram.base <- reactive({
                                    filter(trigram.s, base1 == n1.word(), base2 == last.word())
                              })
                              
                              if(nrow(trigram.base()) > 0) {
                                    t.p1 <- reactive({
                                          trigram.base()$frequency/sum(trigram.s$frequency)
                                    })
                                    t.p2 <- reactive({
                                          as.numeric(rep((filter(bigram.s, base1 == n1.word(), predicted == last.word())[, 4]), times = length(t.p1())))
                                    })
                                    t.p3 <- reactive({
                                          as.numeric(rep(sum(bigram.s$frequency), times = length(t.p1())))
                                    })
                                    t.p4 <- reactive({
                                          t.p2()/t.p3()
                                    })
                                    triprobability <- reactive({
                                          t.p1()/t.p4()
                                    })
                                    trigram.base.2 <- reactive({
                                          cbind(trigram.base(), triprobability())
                                    })
                                    
                                    ggplot(na.omit(trigram.base.2()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                                          geom_point() + 
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                          labs(x = "Trigram Predicted Words")
                                    
                              } else {
                                    
                                    bigram.base <- reactive({
                                          filter(bigram.s, base1 == last.word())
                                    })
                                    
                                    if(nrow(bigram.base()) > 0) {
                                          b.p1 <- reactive({
                                                bigram.base()$frequency/sum(bigram.s$frequency)
                                          })
                                          b.p2 <- reactive({
                                                as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                                          })
                                          biprobability <- reactive({
                                                b.p1()/b.p2()
                                          })
                                          bigram.base.2 <- reactive({
                                                cbind(bigram.base(), biprobability())
                                          })
                                          
                                          ggplot(na.omit(bigram.base.2()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
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
                        q.p1 <- reactive({
                              quadgram.base()$frequency/sum(quadgram.s$frequency)
                        })
                        q.p2 <- reactive({
                              as.numeric(rep((filter(trigram.s, base1 == n2.word(), base2 == n1.word(), predicted == last.word())[, 5]), times = length(q.p1())))
                        })
                        q.p3 <- reactive({
                              as.numeric(rep(sum(trigram.s$frequency), times = length(q.p1())))
                        })
                        q.p4 <- reactive({
                              q.p2()/q.p3()
                        })
                        quadprobability <- reactive({
                              q.p1()/q.p4()
                        })
                        quadgram.base.2 <- reactive({
                              cbind(quadgram.base(), quadprobability())
                        })
                        
                        ggplot(na.omit(quadgram.base.2()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                              geom_point() + 
                              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                              labs(x = "Quadgram Predicted Words")
                        
                  } else {
                        
                        trigram.base <- reactive({
                              filter(trigram.s, base1 == n1.word(), base2 == last.word())
                        })
                        
                        if(nrow(trigram.base()) > 0) {
                              t.p1 <- reactive({
                                    trigram.base()$frequency/sum(trigram.s$frequency)
                              })
                              t.p2 <- reactive({
                                    as.numeric(rep((filter(bigram.s, base1 == n1.word(), predicted == last.word())[, 4]), times = length(t.p1())))
                              })
                              t.p3 <- reactive({
                                    as.numeric(rep(sum(bigram.s$frequency), times = length(t.p1())))
                              })
                              t.p4 <- reactive({
                                    t.p2()/t.p3()
                              })
                              triprobability <- reactive({
                                    t.p1()/t.p4()
                              })
                              trigram.base.2 <- reactive({
                                    cbind(trigram.base(), triprobability())
                              })
                              
                              ggplot(na.omit(trigram.base.2()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                                    geom_point() + 
                                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                    labs(x = "Trigram Predicted Words")
                              
                        } else {
                              
                              bigram.base <- reactive({
                                    filter(bigram.s, base1 == last.word())
                              })
                              
                              if(nrow(bigram.base()) > 0) {
                                    b.p1 <- reactive({
                                          bigram.base()$frequency/sum(bigram.s$frequency)
                                    })
                                    b.p2 <- reactive({
                                          as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                                    })
                                    biprobability <- reactive({
                                          b.p1()/b.p2()
                                    })
                                    bigram.base.2 <- reactive({
                                          cbind(bigram.base(), biprobability())
                                    })
                                    
                                    ggplot(na.omit(bigram.base.2()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
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
                        t.p1 <- reactive({
                              trigram.base()$frequency/sum(trigram.s$frequency)
                        })
                        t.p2 <- reactive({
                              as.numeric(rep((filter(bigram.s, base1 == n1.word(), predicted == last.word())[, 4]), times = length(t.p1())))
                        })
                        t.p3 <- reactive({
                              as.numeric(rep(sum(bigram.s$frequency), times = length(t.p1())))
                        })
                        t.p4 <- reactive({
                              t.p2()/t.p3()
                        })
                        triprobability <- reactive({
                              t.p1()/t.p4()
                        })
                        trigram.base.2 <- reactive({
                              cbind(trigram.base(), triprobability())
                        })
                        
                        ggplot(na.omit(trigram.base.2()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                              geom_point() + 
                              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                              labs(x = "Trigram Predicted Words")
                        
                  } else {
                        
                        bigram.base <- reactive({
                              filter(bigram.s, base1 == last.word())
                        })
                        
                        if(nrow(bigram.base()) > 0) {
                              b.p1 <- reactive({
                                    bigram.base()$frequency/sum(bigram.s$frequency)
                              })
                              b.p2 <- reactive({
                                    as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                              })
                              biprobability <- reactive({
                                    b.p1()/b.p2()
                              })
                              bigram.base.2 <- reactive({
                                    cbind(bigram.base(), biprobability())
                              })
                              
                              ggplot(na.omit(bigram.base.2()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
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
                        b.p1 <- reactive({
                              bigram.base()$frequency/sum(bigram.s$frequency)
                        })
                        b.p2 <- reactive({
                              as.numeric(rep((filter(unigram, feature == last.word())[, 3]), times = length(b.p1())))
                        })
                        biprobability <- reactive({
                              b.p1()/b.p2()
                        })
                        bigram.base.2 <- reactive({
                              cbind(bigram.base(), biprobability())
                        })
                        
                        ggplot(na.omit(bigram.base.2()[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
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