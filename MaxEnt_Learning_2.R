

# MaxEnt Learning 2: Batch Learning, reading from OT-Soft formatted file

# To paraphrase Nazarov & Pater 2017, the defining feature of a MaxEnt grammar is the way in which the probability distribution 
# over a candidate set is defined. Specifically, the probability that a given candidate associated with a given input will be selected
# as a winner is proportional to the exponential of the weighted sum of the constraints violated by that candidate.
# Thus, as one would intuitively expect, the more violations that a candidate incurs and the greater the weight of the 
# constraints violated by that candidate, the smaller the proportion of the probability mass assigned to that candidate
# relative to other candidates with fewer violations. 
# The weights of the constraints themselves, while part of the learning problem, can be calculated in numerous different fashions:
# either through online learning with an update rule for the constraint weights, or batch learning using any suitable optimization
# function set to minimize the log-probability of a set of weights, violations, and candidate probabilities.
  # Cf. Goldwater Johnson 2003, Hayes & Wilson 2008 on batch learning objective functions and regularization terms.
  # Quotation with citations from Hayes & Wilson 2008: 
    #"A maximum entropy grammar uses weighted constraints to assign probabilities to outputs. 
   #For general background on maximum entropy (hereafter, maxent) grammars, see Jaynes 1983, Jelinek 1999:ch. 13, 
    #Manning and Schutze 1999, and Klein and Manning 2003. We will rely here on particular results developed in 
    #Berger et al. 1996, Rosenfeld 1996, Della Pietra et al. 1997, and Eisner 2001. For earlier applications of 
    #maxent grammars to phonology, in particular to the learning and analysis of input-output mappings, see 
    #Goldwater and Johnson 2003 and Jäger 2004."
  # Note also fn. 2 in Hayes & Wilson 2008: "The use of scores, but without their theoretical interpretation under maximum entropy as probability, is the basis of “linear optimality theory” (Keller 2000, 2006)."
  # Hayes & Wilson 2008, section 3.3.2 gives probably the most thorough description in the (phonological) literature on the search procedure 
  # for constraint weights using an optimization function.


# Read in tableaux and get candidate frequencies plus candidate violation profiles
prepare_data <- function(OT_Table_File){
  raw_file <- read.table(file =  OT_Table_File, sep="\t", header=F, fill=T, stringsAsFactors = F, skip=2, quote=NULL) # read in an OTSoft-formatted file, ignoring the first two rows with constaint names
  raw_file[is.na(raw_file)] <- 0 # replace all empty constraint evaluations with 0
  header <- unlist(strsplit(scan(OT_Table_File, what="char", sep="\n")[1], split="\t")) # get the constraint names from the same files
  colnames(raw_file) <- header # add the constraint names as a header to the candidate data frame
  constraint_names <- header[4:length(header)] # get the names of the constraints
  number_of_constraints <- length(constraint_names) # get the number of constraints

  cand_freqs <- list()
  current_tableau <- 0
  # for(i in 1:length(nrow(raw_file))){
  #   current_cand_freqs <- c()
  #   if(raw_file[i,1] != ""){
  #     current_tableau <- current_tableau + 1
  #     current_cand_freqs <- append(current_cand_freqs, raw_file[i,3])
  #   }
  #   else{
  #   }
  #   for(j in i+1:length(nrow(raw_file))){
  #     if(raw_file[j,1] == ""){
  #       current_cand_freqs <- append(current_cand_freqs, raw_file[i+1,3])
  #     }
  #     else{
  #       break
  #     }
  #   }
  #   cand_freqs[[current_tableau]] <- current_cand_freqs
  # }
  # 
  
  cand_freqs <- list()
  current_tableau <- 0
  for(i in which(raw_file[,1] != "")){
    current_tableau <- current_tableau +1
    current_cand_freqs <- c()
    current_cand_freqs <- append(current_cand_freqs, raw_file[i,3])
    iterator <- i
    while((raw_file[iterator+1,1] == "")){
      current_cand_freqs <- append(current_cand_freqs, raw_file[iterator+1,3])
      if(iterator+1 < nrow(raw_file)){
        iterator <- iterator + 1
      }
      else{
        break
      }
    }
    cand_freqs[[current_tableau]] <- current_cand_freqs
  }
  
  violations <- list()
  current_tableau <- 0
  for(i in which(raw_file[,1] != "")){
    number_candidates <- 1
    current_tableau <- current_tableau +1
    current_tableau_violations <- c()
    current_tableau_violations <- append(current_tableau_violations, as.numeric(raw_file[i,4:ncol(raw_file)]))
    iterator <- i
    while((raw_file[iterator+1,1] == "")){
      current_tableau_violations <- append(current_tableau_violations, as.numeric(raw_file[iterator+1,4:ncol(raw_file)]))
      number_candidates <- number_candidates + 1
      if(iterator+1 < nrow(raw_file)){
        iterator <- iterator + 1
      }
      else{
        break
      }
    }
    total_tableau_violations <- matrix(current_tableau_violations, nrow=number_candidates, byrow = T)
    violations[[current_tableau]] <- total_tableau_violations
  }
  
  candidates <- list()
  current_tableau <- 0
  for(i in which(raw_file[,1] != "")){
    current_tableau <- current_tableau +1
    current_candidates <- c()
    current_candidates <- append(current_candidates, raw_file[i,2])
    iterator <- i
    while((raw_file[iterator+1,1] == "")){
      current_candidates <- append(current_candidates, raw_file[iterator+1,2])
      if(iterator+1 < nrow(raw_file)){
        iterator <- iterator + 1
      }
      else{
        break
      }
    }
    candidates[[current_tableau]] <- current_candidates
  }
  
  tableaux <- list(cand_freqs, violations, candidates)
  
  names(tableaux[[1]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])
  names(tableaux[[2]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])
  names(tableaux[[3]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])
  
  return(tableaux)
  
}

# Store candidate frequencies and violation profiles in a list. Input the name of the OT-Soft file here.
tableaux <- prepare_data("Vedic_Test_2 copy.txt")
# Store the sum token frequencies of all candidates in each tableau in a vector and divide this by the total token frequency 
  # of all forms in the data to obtain a probability of sampling a given tableux.
tableaux_token_freqs <- scan("Vedic_Test_2_token_freqs.txt", sep="\t")
tableaux_probs <- tableaux_token_freqs/sum(tableaux_token_freqs)

# Function for sampling "winning" candidates from probability distributions provided in the data.
sample_candidate <- function(tableaux, token_freqs_of_types=F){
  # Can be improved by adding token frequencies of the types and then using that as a probability for the tableaux sampling
  if(token_freqs_of_types == T){
    current_tableaux <- sample(length(tableaux[[1]]), 1, prob = tableaux_probs)
  }
  else{
    current_tableaux <- sample(length(tableaux[[1]]), 1)
  }
  current_candidate <- sample(unlist(tableaux[[3]][current_tableaux]), size=1, prob = unlist(tableaux[[1]][current_tableaux]))
  return(current_candidate)
}

# Initial Learner Setup
learner_weights <- rep(0,ncol(tableaux[[2]][[1]]))
learner_violations <- tableaux[[2]]
#learner_winner_probs <- 
get_cand_prob <- function(violations, weights){
  # Because vectors are "recycled" by column, violation vectors need to be transposed to be arranged by column, rather than row, then transposed back.
  applied_weights <- t(t(violations)*weights) # Calculate the "force" of each violation
  harmonies = apply(applied_weights, 1, sum) # Sum across each row to obtain the Harmony (H) of each candidate.
  e_harmonies = exp(-1*harmonies) # Take the exponential of the harmonies (e-harmony)
  Z = sum(e_harmonies) # Sum the e-harmonies to obtain the denominator (Z) for calculating candidate probabilities
  cand_probs <- e_harmonies/Z # Candidate probabilites are the e-harmony of each candidate divided by their sum.
  return(cand_probs)
}

initial_learner_candidate_probs <- function(learner_violations = learner_violations, learner_weights = learner_weights){
  learner_candidate_probs <- list()
  for(i in 1:length(learner_violations)){
    current_candidate_probs <- get_cand_prob(violations = learner_violations[[i]], weights = learner_weights)
    learner_candidate_probs[[i]] <- current_candidate_probs
  }
  return(learner_candidate_probs)
}
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)

# Online Learning: Sampling plus update with delta rule
  # Important question here: should negative changes in weights be set to 0, or should any resulting negative learner weights be set at 0.
update_rule <- function(teacher_winner, learner_winner, rate, learner_weights){
  change_in_weights <- rate*(learner_winner - teacher_winner) # learner-teacher or teacher-learner -- I think the former for positive weights; cf. Jarosz 2016: 204
  #negatives <- which(change_in_weights < 0)
  #change_in_weights[negatives] <- 0
  learner_weights <- learner_weights + change_in_weights
  negatives <- which(learner_weights < 0) # GLA allows negative weights, I think
  learner_weights[negatives] <- 0
  return(learner_weights)
}

# Run a learning loop with a specified number of iterations and a learning rate
learner_weights <- rep(0,ncol(tableaux[[2]][[1]])) # With my Vedic_Test_2 test data, it is converging on the categorical outcomes that would result from regular OT. What does this tell me?
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
learning_loop <- function(number_iterations = 50000, learning_rate = 0.01, multiple_teachers = F, student_teachers = F, num_agents = number_agents, prev_gen = previous_generation, current_gen = current_generation, learner_cand_probs = learner_cand_probs){ # For learning categorical distributions, this comes pretty close. You could program some variability in the learning rate (sometimes faster, sometimes slower)
  for(i in 1:number_iterations){
    if(multiple_teachers == T){ # If the agent that is learning has multiple teachers, randomly sample a teacher based on the number of agents provided. 
                                # This only makes sense in the context of a multi-generational learning design.
      current_teacher <- sample(1:num_agents, 1)
      current_sampled_teacher_datum <- sample_candidate(prev_gen[[current_teacher]])
    }
    if(student_teachers == T){ # 
      current_teacher <- sample(1:num_agents, 1)
      current_sampled_teacher_datum <- sample_candidate(current_gen[[current_teacher]])
    }
    else{
      current_sampled_teacher_datum <- sample_candidate(tableaux)
    }
      
    #print(current_sampled_teacher_datum)
    current_learner_tableaux <- which(names(learner_violations) == gsub("/[0-9][0-9]?[0-9]?", "/", names(current_sampled_teacher_datum))) # Check which tableaux matches the sampled datum. Will work for up to 999 tableaux. 
    current_teacher_candidate_number <- which(tableaux[[3]][[current_learner_tableaux]] == current_sampled_teacher_datum)
    
    current_sampled_learner_datum <- sample(tableaux[[3]][[current_learner_tableaux]], size=1, prob = learner_cand_probs[[current_learner_tableaux]])
    #print(current_sampled_learner_datum)
    current_learner_candidate_number <- which(tableaux[[3]][[current_learner_tableaux]] == current_sampled_learner_datum)
    
    # Get violation profiles of the sampled items 
    temp_teacher_violations <- tableaux[[2]][[current_learner_tableaux]][current_teacher_candidate_number, ]
    temp_learner_violations <- tableaux[[2]][[current_learner_tableaux]][current_learner_candidate_number, ]
    learner_weights <- update_rule(temp_teacher_violations, temp_learner_violations, rate = learning_rate, learner_weights)
    #learner_weights <- update_rule(temp_teacher_violations, temp_learner_violations, rate=0.001, learner_weights)
    #print(learner_weights)
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
    #print(learner_cand_probs) # I think the violation prfiles are wrong!!!
  }
  final_learner_outcome <- list(learner_cand_probs, learner_weights)
  return(final_learner_outcome)
}
output <- learning_loop()


  
  
# Function for calculating PLOG (Probability Log)
# The PLOG is the objective function to be optimized: the log probability of each candidate, multipled by their frequencies, summed.
get_plog <- function(weights, violations, cand_freqs){ # Note: it doesn't matter what the starting weights are for the purposes of optimization. The get_plog function will return a PLOG for a given set of weights, but the optimization function looks for the minimum possible PLOG.
  # THERE's A HUGE PROBLEM HERE: DIFFERENT RESULTS OBTAIN WITH DIFFERENT ORDERS OF VIOLATIONS*WEIGHTS OR TRANSPOSITIONS
  # No, seems ok now. 10:08 PM 12.10.2019
  # out <- optim(rep(0,ncol(violations[[1]])), get_plog, violations=violations, cand_freqs=cand_freqs, method="L-BFGS-B", lower=0, control = list(fnscale=-1))
  plogs <- c()
  for(i in 1:length(cand_freqs)){
    current_cand_freqs <- cand_freqs[[i]]
    applied_weights <- t(t(violations[[i]])*weights) # Violation vectors need to be transposed when actually using the weights
    #applied_weights <- violations[[i]]*weights
    harmonies = apply(applied_weights, 1, sum)
    e_harmonies = exp(-1*harmonies)
    Z = sum(e_harmonies)
    temp_cand_probs <- e_harmonies/Z
    temp_cand_prob_logs <- log(temp_cand_probs)
    #cand_prob_logs <- append(cand_prob_logs, temp_cand_prob_logs)
    temp_plog <- sum(temp_cand_prob_logs*current_cand_freqs)
    plogs <- append(plogs, temp_plog)
  }
  #plog = sum(cand_freq*cand_freqs)
  return(sum(plogs))
}


## FOR FULL BATCH PROCESSING
weights <- rep(0,ncol(tableaux[[2]][[1]]))
violations <- tableaux[[2]]
cand_freqs <- tableaux[[1]]

## SAVE THIS. Optimization. Number of weights to optimize, objective function (get_plog), violation profiles, candidate frequencies
out <- optim(rep(0,ncol(violations[[1]])), get_plog, violations=violations, cand_freqs=cand_freqs, method="L-BFGS-B", lower=0, upper=50, control = list(fnscale=-1))
## SAVE THIS. Retrieve optimized weights.
weights <- out$par
get_cand_prob(violations = violations[[1]], weights = weights)
  
  
learning_loop <- function(number_iterations = 50000, learning_rate = 0.01, learner_cand_probs = learner_cand_probs){ # For learning categorical distributions, this comes pretty close. You could program some variability in the learning rate (sometimes faster, sometimes slower)
  for(i in 1:number_iterations){
    #if(multiple_teachers == T){ # If the agent that is learning has multiple teachers, randomly sample a teacher based on the number of agents provided. 
      # This only makes sense in the context of a multi-generational learning design.
     # current_teacher <- sample(1:num_agents, 1)
     # current_sampled_teacher_datum <- sample_candidate(prev_gen[[current_teacher]])
   # }
    #if(student_teachers == T){ # 
     # current_teacher <- sample(1:num_agents, 1)
    #  current_sampled_teacher_datum <- sample_candidate(current_gen[[current_teacher]])
   # }
    #else{
      current_sampled_teacher_datum <- sample_candidate(tableaux)
   # }
    
    #print(current_sampled_teacher_datum)
    current_learner_tableaux <- which(names(learner_violations) == gsub("/[0-9][0-9]?[0-9]?", "/", names(current_sampled_teacher_datum))) # Check which tableaux matches the sampled datum. Will work for up to 999 tableaux. 
    current_teacher_candidate_number <- which(tableaux[[3]][[current_learner_tableaux]] == current_sampled_teacher_datum)
    
    current_sampled_learner_datum <- sample(tableaux[[3]][[current_learner_tableaux]], size=1, prob = learner_cand_probs[[current_learner_tableaux]])
    #print(current_sampled_learner_datum)
    current_learner_candidate_number <- which(tableaux[[3]][[current_learner_tableaux]] == current_sampled_learner_datum)
    
    # Get violation profiles of the sampled items 
    temp_teacher_violations <- tableaux[[2]][[current_learner_tableaux]][current_teacher_candidate_number, ]
    temp_learner_violations <- tableaux[[2]][[current_learner_tableaux]][current_learner_candidate_number, ]
    learner_weights <- update_rule(temp_teacher_violations, temp_learner_violations, learning_rate, learner_weights)
    #learner_weights <- update_rule(temp_teacher_violations, temp_learner_violations, rate=0.001, learner_weights)
    #print(learner_weights)
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
    #print(learner_cand_probs) # I think the violation prfiles are wrong!!!
  }
  final_learner_outcome <- list(learner_cand_probs, learner_weights)
  return(final_learner_outcome)
}
output <- learning_loop()
  
  