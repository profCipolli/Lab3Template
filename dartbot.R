########################################################################################################################################
#library(tidyverse)
library(stringr)   # "T20" "T20" "T20"
########################################################################################################################################
########################################################################################################################################
# Specify Dartboard
#
# dartboard:     451mm
# playable area: 340mm
# double bull:   0mm    - 6.35mm
# single bull:   6.35mm - 16mm
# lower single:  16mm   - 99mm
# treble:        99mm   - 107mm
# upper single:  107mm  - 162mm 
# double:        162mm  - 170mm

# height: 173cm to bullseye
# distance: 237 from board
########################################################################################################################################
########################################################################################################################################
# Create a tibble (data frame) that contains the information about the dartboard:
dartboard <- data.frame(
  # target: lower single, single, double, treble for each value around the dartboard
  target = paste(c("S","T","S","D"), 
                 rep(c(6, 13, 4, 18, 1, 20, 5, 12, 9, 14, 11, 8, 16, 7, 19, 3, 17, 2, 15, 10), each=4), 
                 # ^ specifying each makes r repeat each item 4 times (e.g., 6,6,6,6, 13,13,13,13)
                 sep=""),
  # score:  how much is that bed worth?
  score = c(1,3,1,2)*rep(c(6, 13, 4, 18, 1, 20, 5, 12, 9, 14, 11, 8, 16, 7, 19, 3, 17, 2, 15, 10), each=4),
  # ISSUE! later, we find that SIX having 351 as a start and 9 as an end to be a problem  
  #theta.start = rep(c(351, seq(9, 351-18, 18)), each=4),
  #theta.end = rep(seq(9,351,18), each=4),
  # theta.start: what angle starts the bed? Take 0 degrees at the start of 6, then following usual quadrants  
  theta.start  = rep(seq(0,360-18, 18), each=4),
  # theta.end: what angle ends the bed? (note the result should stay 0-360 degrees)
  theta.end = rep(seq(18,360,18), each=4),
  # r.start: what radius starts the bed? See above.
  r.start = rep(c(16, 99, 107, 162), 20), 
  # ^ not specifying each makes r repeat the whole vector 20 times (e.g., 32, 99, 115, 162,32, 99, 115, 162,..)
  # r.end: what radius ends the bed? See above.
  r.end = rep(c(99, 107, 162, 170), 20)) 

  # add Bull, double bull, and MISS manually
  dartboard <- rbind(dartboard, data.frame(target="B", score=25, theta.start=0, theta.end=360, r.start=6.35, r.end= 16))
  dartboard <- rbind(dartboard, data.frame(target="DB", score=50, theta.start=0, theta.end=360, r.start=0, r.end= 6.35))
  dartboard <- rbind(dartboard, data.frame(target="MISS", score=0, theta.start=NA, theta.end=NA, r.start=NA, r.end= NA))
  dartboard <- rbind(dartboard, data.frame(target="BOUNCE-OUT", score=0, theta.start=NA, theta.end=NA, r.start=NA, r.end= NA))


########################################################################################################################################
########################################################################################################################################
# Specify Checkouts -- https://www.darts501.com/download/darts501-checkout-chart-UK-2019.pdf
########################################################################################################################################
########################################################################################################################################
checkouts <- data.frame(
  total  = c(170, 167, 164, 161, 160, 158:2),
  target = c("T20",                                                                      # 170 
             "T20",               "T20",               "T20", "T20",                     # 169 to 160
             "T20", "T20", "T20", "T20", "T20", "T20", "T20", "T20", "T20",              # 159 to 150
             "T20", "T20", "T20", "T20", "T20", "T20", "T20", "T20", "T20", "T20",       # 149 to 140
             "T19", "T20", "T19", "T20", "T20", "T20", "T20", "T20", "T20", "T20",       # 139 to 130
             "T19", "T18", "T20", "T19", "B",   "T20", "T19", "T18", "T17", "T20",       # 129 to 120
             "T19", "T20", "T20", "T20", "T20", "T20", "T20", "T20", "T20", "T20",       # 119 to 110
             "T19", "T20", "T19", "T20", "T19", "T18", "T20", "T20", "T20", "T20",       # 109 to 100
             "T19", "T20", "T19", "T20", "T19", "T18", "T19", "T20", "T17", "T20",       # 99 to 90
             "T19", "T16", "T17", "T18", "T15", "T20", "T17", "T14", "T19", "T20",       # 89 to 80
             "T19", "T18", "T19", "T20", "T17", "T14", "T19", "T16", "T13", "T10",       # 79 to 70
             "T15", "T20", "T17", "T10", "T19", "T16", "T13", "T10", "T15", "S20",       # 69 to 60
             "S19", "S18", "S17", "S16", "S15", "S14", "S13", "S20", "S19", "DB",        # 59 to 50 
             "S17", "S16", "S15", "S14", "S13", "S12", "S11", "S10", "S9",  "D20",       # 49 to 40
             "S7",  "D19", "S5",  "D18", "S3",  "D17", "S1",  "D16", "S15", "D15",       # 39 to 30
             "S13", "D14", "S11", "D13", "S9",  "D12", "S7",  "D11", "S5",  "D10",       # 29 to 20
             "S3",  "D9",  "S1",   "D8", "S7",  "D7",  "S5",  "D6",  "S3",  "D5",        # 19 to 10
             "S1",  "D4",  "S1",   "D3", "S1",  "D2",  "S1",  "D1"                       # 9 to 2
  )
)


########################################################################################################################################
########################################################################################################################################
# Specify Difficulty

# We define difficulty as the cpu player's ability to hit the treble cpuLevel% of the time
# treble bed is 8mm across and the treble bed covers 360/20=18 degrees of the board
# below we find the sd that makes the simulated errors within tolerance for the treble cpuLevel% of the time
########################################################################################################################################
########################################################################################################################################
# Create function getDifficulty(cpuLevel)
getDifficulty <- function(cpuLevel){
  probability.goal <- sqrt((cpuLevel/100) * (1/2))  # 50% is very high for treble accuracy
                                                    # sqrt() splits the error between radius and angle
  difficulty <- c(NA,NA)

  # Find sigma so that the probability of hitting a triple 20 wrt r is probability.goal*(1/sqrt(1.6)) (more likely to miss than angle)
  # center of treble bed is 103 mm from center of double bull and 8mm wide
  difficulty[1] <- uniroot(f = function(x){((pnorm(q = 4, mean = 0, sd = x) - pnorm(q = -4, mean = 0, sd = x))-probability.goal*(1/sqrt(1.6)))},
                           interval=c(0,10000))$root
  
  
  
  # Find sigma so that the probability of hitting a triple 20 wrt theta is probability.goal*(1.6/sqrt(1.6)) (lower weight than radius)
  difficulty[2] <- uniroot(f = function(x){((pnorm(q = 9, mean = 0, sd = x) - pnorm(q = -9, mean = 0, sd = x))-probability.goal*(1.6/sqrt(1.6)))},
                           interval=c(0,10000))$root
  
  difficulty
}

########################################################################################################################################
########################################################################################################################################
# Get target to aim for based on current score

# We pull the defined dartboard 
# The aimed r is the center of the section we're aiming for  -- (r.start + r.end)/2
# The aimed theta is the center of the section we're aiming for -- (theta.start + theta.end)/2
# NOTE: there are two single beds -- when aiming we assume the player aims for the top (which they do)
########################################################################################################################################
########################################################################################################################################
# Create function getTarget(currentScore)
getTarget <- function(currentScore){
  # Check if the currentScore is in the checkout table
  # If it is, return the recommended target. If not, return T20 (the highest score)
  if(currentScore %in% checkouts$total){
    target <- checkouts$target[checkouts$total == currentScore]
  }else{
    target <- "T20"
  }
  target
}

########################################################################################################################################
########################################################################################################################################
# Get coordinates of where we're aiming

# We pull the defined dartboard 
# The aimed r is the center of the section we're aiming for  -- (r.start + r.end)/2
# The aimed theta is the center of the section we're aiming for -- (theta.start + theta.end)/2
# NOTE: there are two single beds -- when aiming we assume the player aims for the top (which they do)
########################################################################################################################################
########################################################################################################################################
# Create function getAimingCoord(target, dartboard)
getAimingCoord <- function(aiming.target, dartboard){
  # return the (r, theta) to aim at for the provided target and dartboard specification
  aiming.coords <- dartboard[dartboard$target==aiming.target,]
  aiming.coords <- data.frame(aiming.r      = (max(aiming.coords$r.start) + max(aiming.coords$r.end))/2,  # middle of bed wrt radius
              aiming.theta  = (max(aiming.coords$theta.start) + max(aiming.coords$theta.end))/2)          # middle of bed wrt theta
  
  if(aiming.target=="DB"){
    aiming.coords[1] = 0 
  }
  
  aiming.coords
}


########################################################################################################################################
########################################################################################################################################
# Get coordinates of where dart was thrown (simulated)

# We input where the thrower is aiming, and their difficulty level
# Random noise is added to r and theta according to their difficulty level
########################################################################################################################################
########################################################################################################################################
# Create a function simulateThrow(r, theta, difficulty)
simulateThrow <- function(r, theta, difficulty){
  # simulate r.error from the normal with mean0 and difficulty[1] as sd
  thrown.r <- r + rnorm(n = 1, mean = 0, sd = difficulty[1])
  # simulate theta.error from the normal with mean0 and difficulty[2] as sd
  thrown.theta <- theta + rnorm(n=1, mean = 0, sd = difficulty[2])
  
  # NOTE: if r is negative we need to adjust (-180) and theta should be between 0 and 360
  # if r<0 I'm on the other side of the board, so add 180
  if(thrown.r<0){
    thrown.r <- abs(thrown.r)
    thrown.theta <- thrown.theta + 180
  }
  # fix if theta > 360 return the remainder (modulus) when divided by 360
  thrown.theta <- thrown.theta %% 360
  
  # return resulting throw
  c(thrown.r, thrown.theta)
}

########################################################################################################################################
########################################################################################################################################
# Evaluate the throw

# We input where the throw landed and use r and theta to discern which bed
# the dart landed in. We return the bed and the score.
########################################################################################################################################
########################################################################################################################################
# Create a function evaluateThrow(r, theta)
evaluateThrow <- function(r, theta){
  # filter through dartboard to find where r and theta reside
  throw <- dartboard[r>=dartboard$r.start & r<=dartboard$r.end & theta>=dartboard$theta.start & theta<=dartboard$theta.end & !is.na(dartboard$r.start), ]
  
  # NOTE: catch misses (MISS)
  if(nrow(throw)==0){
    throw <- rbind(throw, c("MISS",0))
  }
  
  # Simulate bounce-out on R if necessary
  r.boundaries <- c(6.35, 16, 99, 107, 162, 170)
  d <- abs(as.numeric(r)-r.boundaries)
  
  if(any(d<0.4)){
    if(runif(1)<pbeta(q=min(d)*2.5, shape1=1, shape2=10, lower.tail=F)){
      throw <- dartboard[dartboard$target == "BOUNCE-OUT",]
      throw <- throw[, c("target", "score")]
    }
  }

  # Simulate bounce-out on theta if necessary
  if(r > 16){ # if not in single or double bulls
    theta.c <- round(theta/18)*18
    d <- sqrt(2*r^2*(1-cos(theta-theta.c)))
    if(d<0.4){
      if(runif(1)<pbeta(q=d*2.5, shape1=1, shape2=10, lower.tail=F)){
        throw <- dartboard[dartboard$target == "BOUNCE-OUT",]
        throw <- throw[, c("target", "score")]
      }
    }
  }
  
  throw
}


########################################################################################################################################
########################################################################################################################################
# Evaluate the throw

# We input which bed the dart landed in. We return the bed and the score.
########################################################################################################################################
########################################################################################################################################
# Create a function scoreHuman(hit) that pulls the score for the reported bed
scoreHuman <- function(hit){
  # select row matching reported bed and return score (max is for case of single, when two rows are present)
  max(dartboard$score[which(dartboard$target == hit)])
}
########################################################################################################################################
########################################################################################################################################
# Play a match


# Throw 3 darts
# Subtract total
# Repeat Throw and subtract

# Game is over if the total is 0 and the last dart thrown was a double
# Player goes back to their original total if the score goes negative, or goes to 0 on a non-double.
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
# Start at 501
human.score <- 501
bot.score   <- 501

# SIDO or DIDO?
DIDO = FALSE

# Start player by getting the difficulty object for a specified difficulty 1-99
diff <- 25
difficulty <- getDifficulty(diff)
# Starting scores for bot and human

# REPLACING THIS WITH DIDDLE
# Create object for denoting who goes first
# human.first <- TRUE

# Create object for counting darts thrown for bot and human
human.darts <- 0
bot.darts <- 0

#stop.game
stop.game=FALSE
iter=0
repeat{
  iter=iter+1
  ###########################################################################################
  ###########################################################################################
  # Diddle
  ###########################################################################################
  ###########################################################################################
  if(iter==1){
    cat("Diddle for the middle. Shoot for bulls to determine who goes first.")
    repeat{
      ###################################
      # Human
      ###################################
      human.diddle <- toupper(readline(prompt = "What did you throw? Enter as B or DB for example:        "))
      
      ###################################
      # Bot
      ###################################
      # Get coordinates to aim at
      bot.aim <- getAimingCoord(aiming.target = "DB", dartboard = dartboard)
      # Simulate Throw
      bot.hit <- simulateThrow(r = as.numeric(bot.aim[1]), theta = as.numeric(bot.aim[2]), difficulty = difficulty)
      # Evaluate Throw
      bot.diddle <- evaluateThrow(r = as.numeric(bot.hit[1]), theta = as.numeric(bot.hit[2]))$target
      
      cat(paste("Dartbot hit ", bot.diddle, "\n", sep=""))
      
      ###################################
      # Is diddle over?
      ###################################
      if(human.diddle == bot.diddle){ #both single or double bulls
        next; # tied so we try again
      }else if((human.diddle == "B" & bot.diddle != "DB") | human.diddle == "DB"){
        human.first = TRUE
        break; # diddle is over
      }else if((bot.diddle == "B" & human.diddle != "DB") | bot.diddle == "DB"){
        human.first = FALSE
        break; # diddle is over
      }else{
        #do nothing, neither player hit B or DB
      }
    }
  }
  ###########################################################################################
  ###########################################################################################
  # SIDO / DIDO
  ###########################################################################################
  ###########################################################################################
  if(iter==1 & DIDO){
    human.doubled.in <- FALSE
    bot.doubled.in <- FALSE
  }
  ###########################################################################################
  ###########################################################################################
  # Human
  ###########################################################################################
  ###########################################################################################
  if(iter!=1 | human.first){ # if human goes first, do this, otherwise wait until next iteration
    # prompt for user input "What did you throw? Enter as T20, T20, T20, for example."
    # split input into dart beds
    human.input <- toupper(readline(prompt = "What did you throw? Enter as T20 T20 T20 for example:        "))
    # splits input at the spaces (if applicable)
    human.hits <- str_split(human.input, 
                            pattern = " ",
                            simplify = T)
    # score the input: applies scoreHuman to all reported hits
    human.turn.score <- sum(sapply(X=human.hits, FUN=scoreHuman))
    # calculate a temporary score
    human.temp.score <- human.score - human.turn.score
    # add the number of darts thrown
    human.darts <- human.darts + length(human.hits)
    
    # Check if the game is over or if there was a bust: Is temp.score 0? was the last dart a double?
    if(human.temp.score == 0){
      # if it's a double --> game over
      if(substring(human.hits[length(human.hits)], first = 1,last =  1) == "D"){
        cat("Human has won in ", human.darts," darts.\n", sep="" )
        break; #end game
      }else{       # if not, it's a bust and this turn doesn' count
        cat("Human has busted. This turn doesn't count toward their score.\n")
      }
      # if score is <= 1 they also bust -- there's no way to go out on a double
    }else if(human.temp.score<=1){
      cat("Human has busted. This turn doesn't count toward their score.\n")
    }else{
      if(DIDO && !human.doubled.in){
        # Check to see if the human doubled in
        if(any(grepl(x=human.hits, pattern="D"))){
          human.doubled.in <- TRUE
          # Calculate scores based on double and darts after
          human.hits <- human.hits[min(which(grepl(x=human.hits, pattern="D"))):length(human.hits)]
          human.turn.score <- sum(sapply(X=human.hits, FUN=scoreHuman))
          # calculate a temporary score
          human.score <- human.score - human.turn.score
          
          cat("Human has doubled in. Human has ", human.score, " remaining.\n")
        }else{
          cat("Human has not doubled in. This turn doesn't count.\n")
        }
      }else{ # SIDO or human doubled in
        human.score <- human.temp.score #update the score.
        cat("Human has ", human.score, " remaining.\n", sep="") 
      }
    }
  }
  ###########################################################################################
  ###########################################################################################
  # Computer
  ###########################################################################################
  ###########################################################################################
  curr.bot.dart <- 1   # track number of darts <=3
  bot.turn.hits <- c() # track hits
  bot.turn.score <- 0  # tracks score of turn
  repeat{ # for at most three turns:
    # Get target
    bot.target <- ifelse(DIDO && !bot.doubled.in, "D16", getTarget(bot.score))
    # Get coordinates to aim at
    bot.aim <- getAimingCoord(aiming.target = bot.target, dartboard = dartboard)
    # Simulate Throw
    bot.hit <- simulateThrow(r = as.numeric(bot.aim[1]), theta = as.numeric(bot.aim[2]), difficulty = difficulty)
    # Evaluate Throw
    bot.dart.score <- evaluateThrow(r = as.numeric(bot.hit[1]), theta = as.numeric(bot.hit[2]))
    # calcuate temporary score
    #bot.turn.score <- bot.turn.score + bot.dart.score$score
    
    if(DIDO && !bot.doubled.in){
      if(bot.dart.score[1]!="MISS" && grepl(x=bot.dart.score$target, pattern="D")){
        bot.doubled.in <- TRUE
        cat("Dartbot has doubled in.\n")
      }else{
        if(curr.bot.dart == 3){
          cat("Dartbot has not doubled in. ")
        }
      }
    }
    # Update Score
    if(!DIDO || bot.doubled.in){
      bot.temp.score <- bot.score - bot.dart.score$score 
    }else{
      bot.temp.score <- bot.score
    }
    # record what bot hit
    bot.turn.hits <- c(bot.turn.hits, bot.dart.score$target)
    # add the number of darts thrown
    bot.darts <- bot.darts + 1
    
    # Check if the game is over or if there was a bust: Is temp.score 0? was the last dart a double?
    if(bot.temp.score == 0){
      # if it's a double --> game over
      if(substring(bot.dart.score$target, first = 1, last=1) == "D"){
        cat("Dartbot hit ", paste(bot.turn.hits, collapse=" "),
            " and has won in ", bot.darts," darts.\n", sep="" )
        stop.game <- TRUE
        break; # stop dartbot's turn
      }else{ # if not, it's a bust and this turn doesn' count
        cat("Dartbot has busted. This turn doesn't count toward their score.\n")
        break; # stop dartbot's turn
      }
    }else if(bot.temp.score<=1){
      cat("Dartbot has busted. This turn doesn't count toward their score.\n")
      break; # stop dartbot's turn
    }else{ # record dart and check if dartbot's turn is over
      # Update Score
      bot.score <- bot.temp.score
      if(curr.bot.dart == 3){
        cat("Dartbot hit ", paste(bot.turn.hits, collapse=" "),
            " and has ", bot.score, " remaining.\n", sep="")
        break;
      }
    }
    curr.bot.dart <- curr.bot.dart + 1 # add one dart to turn
  }
  
  if(stop.game){
    break; # stop game
  }
}