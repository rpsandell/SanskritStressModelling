  # vectors for abbreviations in regular expressions
C <- "[bBcCdDfFgGhHjJkKlLmMnNpPrsStTvyYzZwWxX]"
v <- "[aiuRQ]"
V <- "[AIUeoEO]"


# Read in and create frequency table based on Zurich text
setwd("~/Documents/Dropbox/Rigvedic Formularity/Corpora/Parsed Corpus from Kevin")
rv_zurich <- read.table("processed_zurich.txt", header=T, sep="\t")
tokens_rv <- rv_zurich$Pada
# replace the diphthongs ai and au with E and O, because matching ai and au sequences gets too complicated.
# Other digraphs must be replaced too.
tokens_rv <- gsub("ai", "E", tokens_rv)
tokens_rv <- gsub("au", "O", tokens_rv)
#tokens_rv <- gsub("([ptckbdjgTD])h", "\\1h", tokens_rv)
# NB T, D = retroflexes; G = velar nasal
tokens_rv <- gsub("ph", "P", tokens_rv)
tokens_rv <- gsub("th", "F", tokens_rv) 
tokens_rv <- gsub("ch", "C", tokens_rv)
tokens_rv <- gsub("kh", "K", tokens_rv)
tokens_rv <- gsub("bh", "B", tokens_rv)
tokens_rv <- gsub("dh", "Z", tokens_rv)
tokens_rv <- gsub("jh", "J", tokens_rv)
tokens_rv <- gsub("gh", "w", tokens_rv) # Add to list; G already used
tokens_rv <- gsub("Th", "x", tokens_rv) # Add to list; T already used
tokens_rv <- gsub("Dh", "X", tokens_rv) # Add to list; D already used
# remove H and L since you need those in the prosodic shape parsing
tokens_rv <- gsub("H", "7", tokens_rv) # Add to list; T already used
tokens_rv <- gsub("L", "8", tokens_rv) # Add to list; D already used
tokens_rv <- gsub("-", "", tokens_rv)
tokens_rv <- gsub("\\+", "", tokens_rv)
tokens_rv <- gsub("\\*", "", tokens_rv)
tokens_rv <- gsub("!", "", tokens_rv)
tokens_rv <- gsub("\\?", "", tokens_rv)
tokens_rv <- gsub("=", "", tokens_rv)


stressed_tokens_rv <- tokens_rv[grep(";", tokens_rv)]
single_stressed_tokens_rv <- stressed_tokens_rv[-c(grep(";.*;", stressed_tokens_rv))]


C <- "[bBcCdDfFgGhjJkKlmMnNpPrsStTvyYzZwWxX78]"
v <- "[aiuRQ]"
V <- "[AIUeoEO]"

parse_prosodic_shape <- function(word){
  word <- gsub(paste(v, ";", "$", sep=""), "V1", word)
  word <- gsub(paste(v, "$", sep=""), "V", word)
  word <- gsub(paste(v, ";", C, "$", sep=""), "V1C", word)
  word <- gsub(paste(v, C, "$", sep=""), "VC", word)
  word <- gsub(paste(V, ";", "$", sep=""), "V:1", word)
  word <- gsub(paste(V, "$", sep=""), "V:", word)
  word <- gsub(paste(V, ";", C,  "$", sep=""), "V:1C", word)
  word <- gsub(paste(V, C, "$", sep=""), "V:C", word)
  

  word <- gsub(paste(v, ";", C, C, "+", sep=""), "H1 ", word)
  word <- gsub(paste(v,C, C, "+", sep=""), "H ", word)
  
  
  word <- gsub(paste(V, ";", C, C, "+", sep=""), "H1 ", word)
  word <- gsub(paste(V,C, C, "+", sep=""), "H ", word)
  
  
  word <- gsub(paste(V, ";", C, "?", sep=""), "H1 ", word)
  word <- gsub(paste(V,C, "?", sep=""), "H ", word)
  
  word <- gsub(paste(v, ";", C, "?", "~", "?", sep=""), "L1 ", word)
  word <- gsub(paste(v, C, "?", "~", "?", sep=""), "L ", word)

  
  #C <- "[bBcCdDfFgGhjJkKlmMnNpPrsStTvyYzZwWxX78]"
  word <- gsub(paste("^", C, "+", sep=""), "", word)
  return(word)
}

all_prosodic_shapes <- c()
for(i in 1:length(single_stressed_tokens_rv)){
  temp_result <- parse_prosodic_shape(single_stressed_tokens_rv[i])
  all_prosodic_shapes <- append(all_prosodic_shapes, temp_result)
} 

all_prosodic_shapes_table <- table(all_prosodic_shapes)


### Maxent learning ###
tableaux <- prepare_data("~/Documents/Dropbox/RyanTex/Sandell Habilitation/OT Grammars/RIP Feet/New_Berlin/RIP_Stress_Grammar_BERLIN.txt")
  # Element 1 in the list object is the input name and a vector of candidate probabilities
  # Element 2 in the list object is the tableaux: the full violation profiles of each candidate
  # Element 3 in the list object is the list of candidates.

# limited to 2- and 3-syllable words
limited_2_3 <- grep("^[HL]1? [HL]?1? ?V:?1?C?$", names(all_prosodic_shapes_table))
# limited to 2-, 3-, 4-, and 5-syllable word
limited_2_3_4_5 <- grep("^[HL]1? [HL]?1? ?[HL]?1? ?[HL]?1? ?V:?1?C?$", names(all_prosodic_shapes_table))

# Find the tableaux where the UR matches the datum sampled. 
sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))

## Find the candidates that could be winners
possible_winners <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])


## BEGIN HERE ##
# Set initial weights at 10 for markedness, 0 for one faithfulness
# See file "MaxEnt_Learning_2.R" for functions
  # Set the weight of all constraints initially to 10
learner_weights <- rep(10,ncol(tableaux[[2]][[1]]))
  # Set the weight of IDENT-PROM to 0
learner_weights[length(learner_weights)] <- 0

learner_violations <- tableaux[[2]]

learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
# E.g., In tableaux number five, UR /L1 VC/, candidates 1-3 are all of equal probability, because they have equal numbers of markedness violations; 

simple_single_learning_loop <- function(number_iterations = 5000, learning_rate = 0.01){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  old_weights_ident_prom <- c(0)
  number_updates <- 0
  no_learning <- 0
   for(i in 1:number_iterations){
        # The first sampling behaviors will need to be changed and updated
    sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
    sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
    
    possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
    #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
    
    learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
    learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
    if(learner_winner_number %in% possible_winner_numbers){
      # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
      old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      no_learning <- no_learning + 1
    }
    else{
      # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
      teacher_winner_number <- sample(possible_winner_numbers, 1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)])
      teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ]
      # Update constraint weights
      learner_weights <- update_rule(teacher_winner, learner_winner, rate = learning_rate, learner_weights)
      old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      # Update candidate probabilities
      learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
      number_updates <- number_updates + 1
      no_learning <- 0
    }
    # Store the final candidate probabilties and weights in a list
   if(no_learning > (number_iterations * 0.01)){
     break
   }
  }
  final_learner_outcome <- list(learner_cand_probs, learner_weights, old_weights_ident_prom, number_updates)
  names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
  names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
  for(i in 1:length(final_learner_outcome[[1]])){
    names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
  }
  return(final_learner_outcome)
}
  # On 7000 iterations, there were only 2275 instances of constraint updating
  # Overall consistent tendency to promote NonFinalityFoot, NonInitialityFoot, Trochee, Iamb, and Ident-Prom. Everything else is demoted.
ptm <- proc.time()
out <- simple_single_learning_loop(number_iterations = 10000, learning_rate = 0.005)
proc.time() - ptm
names(out[[1]]) <- names(tableaux[[1]])

  # Intergenerationally, what changes are the probabilities of particular prosodic shapes: 

# I ran a single generation of learning with a single agent 10 times and then compared the constraint weights. I suspect that there will be little difference.
simulations_list <- list()
for(i in 1:10){
  temp_out <- simple_single_learning_loop(number_iterations = 10000, learning_rate = 0.005)
  simulations_list[[i]] <- temp_out
}

# Create data for next generation
new_generation_data <- c()
for(i in 1:length(limited_2_3_4_5)){
  prosodic_shape <- names(all_prosodic_shapes_table[limited_2_3_4_5])[i]
  shape_tableau_number <- grep(paste("/", prosodic_shape, "/", sep=""),  names(tableaux[[3]]))
  temp_outputs <- sample(tableaux[[3]][[shape_tableau_number]], all_prosodic_shapes_table[limited_2_3_4_5][i], prob = out[[1]][[shape_tableau_number]], replace = TRUE)
  temp_outputs <- gsub(" \\\\->.*$", "", temp_outputs)
  temp_outputs <- gsub("\\[", "", temp_outputs)
  temp_outputs <- gsub("\\]", "", temp_outputs)
  new_generation_data <- append(new_generation_data, temp_outputs)
}

new_generation_data_table <- table(new_generation_data)
  # But -- the number of different outputs is already vastly restricted in the second generation!
common_shapes <- all_prosodic_shapes_table[limited_2_3_4_5][which(names(all_prosodic_shapes_table[limited_2_3_4_5]) %in% names(new_generation_data_table))]
missing_shapes <- all_prosodic_shapes_table[limited_2_3_4_5][-(c(which(names(all_prosodic_shapes_table[limited_2_3_4_5]) %in% names(new_generation_data_table))))]
new_shapes <- new_generation_data_table[-c(which(names(new_generation_data_table) %in% names(all_prosodic_shapes_table[limited_2_3_4_5])))]
new_shapes_numbers <- c()
for(i in 1:length(new_shapes)){
  new_shapes_numbers <- append(new_shapes_numbers, which(names(new_generation_data_table) == names(new_shapes)[i]))
}
# remove the "new shapes" because you don't have tableaux for them
new_generation_data_table <- new_generation_data_table[-c(new_shapes_numbers)]
  # Some interesting behaviors:  H1 H L L VC >> H H1 L L VC
                                # H L H V:1C >> H L H1 V:1C
# Remove the missing inputs from the tableaux for the new generation
tableaux_to_remove <- c()
for(i in 1:length(names(all_prosodic_shapes_table[limited_2_3_4_5]))){
  temp_shape <- names(all_prosodic_shapes_table[limited_2_3_4_5])[i]
  if(temp_shape %in% names(new_generation_data_table)){
    
  }
  else{
    shape_tableau_number <- grep(paste("/", temp_shape, "/", sep=""),  names(tableaux[[3]]))
    tableaux_to_remove <- append(tableaux_to_remove, shape_tableau_number)
  }
}

new_generation_tableaux <- tableaux
new_generation_tableaux[[1]] <- new_generation_tableaux[[1]][-c(tableaux_to_remove)]
new_generation_tableaux[[2]] <- new_generation_tableaux[[2]][-c(tableaux_to_remove)]
new_generation_tableaux[[3]] <- new_generation_tableaux[[3]][-c(tableaux_to_remove)]
new_generation_tableaux[[4]] <- new_generation_tableaux[[4]][-c(tableaux_to_remove)]

# In the learning loop, replace "tableaux" with "new_generation_tableaux" and replace "all_prosodic_shapes_table[limited_2_3_4_5]" with "new_generation_data_table

# Set the weight of all constraints initially to 10
learner_weights <- rep(10,ncol(tableaux[[2]][[1]]))
# Set the weight of IDENT-PROM to 0
learner_weights[length(learner_weights)] <- 0

learner_violations <- new_generation_tableaux[[2]]

learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
simple_single_learning_loop_G2 <- function(number_iterations = 5000, learning_rate = 0.01){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  old_weights_ident_prom <- c(0)
  number_updates <- 0
  no_learning <- 0
  for(i in 1:number_iterations){
    # The first sampling behaviors will need to be changed and updated
    sampled_item <- sample(names(new_generation_data_table), 1, prob = as.numeric(new_generation_data_table)/sum(new_generation_data_table))
    sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(new_generation_tableaux[[3]]))
    if(length(sampled_tableau_number) == 0){
      next
    }
    # For 2nd generations
    # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
    
    possible_winner_numbers <- grep(sampled_item, new_generation_tableaux[[3]][[sampled_tableau_number]])
    #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
    
    learner_winner_number <- sample(c(1:nrow(new_generation_tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
    learner_winner <- new_generation_tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
    if(learner_winner_number %in% possible_winner_numbers){
      # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
      old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      no_learning <- no_learning + 1
    }
    else{
      # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
      teacher_winner_number <- sample(possible_winner_numbers, 1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)])
      teacher_winner <- new_generation_tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ]
      # Update constraint weights
      learner_weights <- update_rule(teacher_winner, learner_winner, rate = learning_rate, learner_weights)
      old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      # Update candidate probabilities
      learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
      number_updates <- number_updates + 1
      no_learning <- 0
    }
    # Store the final candidate probabilties and weights in a list
    if(no_learning > (number_iterations * 0.01)){
      break
    }
  }
  final_learner_outcome <- list(learner_cand_probs, learner_weights, old_weights_ident_prom, number_updates)
  names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names of constraints
  names(final_learner_outcome[[1]]) <- names(new_generation_tableaux[[1]])
  for(i in 1:length(new_generation_tableaux[[1]])){
    names(final_learner_outcome[[1]][[i]]) <- new_generation_tableaux[[3]][[i]]
  }
  return(final_learner_outcome)
}

out_G2 <- simple_single_learning_loop_G2(number_iterations = 10000, learning_rate = 0.005)

new_generation_data_2 <- c()
for(i in 1:length(new_generation_data_table)){
  prosodic_shape <- names(new_generation_data_table )[i]
  shape_tableau_number <- grep(paste("/", prosodic_shape, "/", sep=""),  names(new_generation_tableaux[[3]]))
  temp_outputs <- sample(new_generation_tableaux[[3]][[shape_tableau_number]], new_generation_data_table[i], prob = out_G2[[1]][[shape_tableau_number]], replace = TRUE)
  temp_outputs <- gsub(" \\\\->.*$", "", temp_outputs)
  temp_outputs <- gsub("\\[", "", temp_outputs)
  temp_outputs <- gsub("\\]", "", temp_outputs)
  new_generation_data_2 <- append(new_generation_data_2, temp_outputs)
}

new_generation_data_table_2 <- table(new_generation_data_2)

missing_shapes <- new_generation_data_table[-(c(which(names(new_generation_data_table) %in% names(new_generation_data_table_2))))]
new_shapes_2 <- new_generation_data_table_2[-c(which(names(new_generation_data_table_2) %in% names(new_generation_data_table)))]

# Now try to iterate what you have achieved here. Note that in the second generation, substantially less learning occurs -- the constraints remain closer to the initial state


  # Summary of basic simulation: With enough data -- if weights are permitted to move much closer to convergence -- it is possible to generate a set of weights that will predict 
    # winners that fit the input data with high probability.
# There are 108830 tokens of polysyllabic words (= 84%)
# Stressed monosyllables aren't common, but disyllables make up almost half (!) of the remaining words.
  # Your next step is to add tableaux for 4- and 5-syllable words
  # Then maybe add to the code to track the probabilities of candidates over the course of learning.


### COLLECTING 4- AND 5-SYLLABLE FORMS and BUILDING TABLEAUX ###
length_4_syll <- c()
length_5_syll <- c()
for(shape in names(all_prosodic_shapes_table)){
  temp_split <- unlist(strsplit(shape, split = " "))
  if(length(temp_split) == 4){
    length_4_syll <- append(length_4_syll, shape)
  }
  if(length(temp_split) == 5){
    length_5_syll <- append(length_5_syll, shape)
  }
}

length_4_5_syll <- c(length_4_syll, length_5_syll)



# I'm assuming that all inputs are 4 or 5 syllables
generate_foot_parses <- function(prosodic_shape){
  #prosodic_shape is assumed to be between / /
  #left_side <- gsub("\\[", prosodic_shape)
  #left_side <- gsub("\\]", prosodic_shape)
  
  prosodic_shape <- gsub("1", "", prosodic_shape)
  foot_parses_vector <- c()
  
  temp_split <- unlist(strsplit(prosodic_shape, split = " "))
  if(length(temp_split) == 4){
      # monosyllabic parses, plus different options for final consonants. If it is in a foot, it gets stress!
    foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1?", "\\(\\11\\)", prosodic_shape) )
    foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])1?", " \\(\\11\\) \\2", prosodic_shape) )
    foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])1? V", " \\1 \\(\\21\\) V", prosodic_shape) )
    if(length(grep("C$", prosodic_shape)) == 0){
      foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?$", "\\(\\11\\)", prosodic_shape) )
    }
    if(length(grep("C$", prosodic_shape)) > 0){
      foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?C$", "\\(\\11C\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?C$", "\\(\\11\\)C", prosodic_shape) )
    }
    #disyllabic parses
    foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1?", "\\(\\11 \\2\\)", prosodic_shape) )
    foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1?", "\\(\\1 \\21\\)", prosodic_shape) )
    
    foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])", " \\(\\11 \\2\\)", prosodic_shape) )
    foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])", " \\(\\1 \\21\\)", prosodic_shape) )
    
    if(length(grep("C$", prosodic_shape)) == 0){
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? (V:?)", " \\(\\11 \\2\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?$", "\\(\\11\\)", prosodic_shape) )
    }
    if(length(grep("C$", prosodic_shape)) > 0){
      foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\11 \\2C\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\11 \\2\\)C", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\1 \\21C\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\1 \\21)C", prosodic_shape) )
    }
  }
    
    if(length(temp_split) == 5){
      # monosyllabic parses, plus different options for final consonants. If it is in a foot, it gets stress!
      foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1?", "\\(\\11\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])1? ([HL])1?", " \\(\\11\\) \\2 \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])1? ([HL])1?", " \\1 \\(\\21\\) \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])1? ([HL])1?", " \\1 \\2 \\(\\31\\)", prosodic_shape) )
      #foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? V", " \\(\\11\\) V", prosodic_shape) )
      if(length(grep("C$", prosodic_shape)) == 0){
        foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?$", "\\(\\11\\)", prosodic_shape) )
      }
      if(length(grep("C$", prosodic_shape)) > 0){
        foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?C$", "\\(\\11C\\)", prosodic_shape) )
        foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?C$", "\\(\\11\\)C", prosodic_shape) )
      }
      #disyllabic parses
      foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1? ([HL])1?", "\\(\\11 \\2\\) \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1? ([HL])1?", "\\(\\1 \\21\\) \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1? ([HL])1?", "\\1 \\(\\21 \\3\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1? ([HL])1?", "\\1 \\(\\2 \\31\\)", prosodic_shape) )
      
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL]) ([HL])1?", " \\(\\11 \\2\\) \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL]) ([HL])1?", " \\(\\1 \\21\\) \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL]) ([HL])1?", " \\1 \\(\\21\\ \\3\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL]) ([HL])1?", " \\1 \\(\\2 \\31\\)", prosodic_shape) )
      
      if(length(grep("C$", prosodic_shape)) == 0){
        foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? (V:?)", " \\(\\11 \\2\\)", prosodic_shape) )
        foot_parses_vector <- append(foot_parses_vector, gsub("(V:?1?)$", "\\(\\11\\)", prosodic_shape) )
      }
      if(length(grep("C$", prosodic_shape)) > 0){
        foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\11 \\2C\\)", prosodic_shape) )
        foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\11 \\2\\)C", prosodic_shape) )
        foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\1 \\21C\\)", prosodic_shape) )
        foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\1 \\21)C", prosodic_shape) )
      }      
    }
  
  return(foot_parses_vector)
}

# Generate a list containing the foot parses for all of the items in the vector length_4_5_syll
list_4_5_syllable_parses <- list()
for(i in 1:length(length_4_5_syll)){
  temp_parses <- generate_foot_parses(length_4_5_syll[i])
  list_4_5_syllable_parses[[i]] <- temp_parses
}
names(list_4_5_syllable_parses) <- length_4_5_syll


generate_stress_positions <- function(prosodic_shape){
  stress_positions_vector <- c()
  temp_split <- unlist(strsplit(prosodic_shape, split = " "))
  temp_stressless <- gsub("1", "", prosodic_shape)
  temp_stressless_split <- unlist(strsplit(temp_stressless, split = " "))
  for(i in 1:length(temp_stressless_split)){
    temp_stressed_syll <- gsub("^([HLV]:?)", "\\11", temp_stressless_split[i])
    temp_stressless_split[i] <- temp_stressed_syll
    new_stressed <- paste(temp_stressless_split, collapse = " ")
    new_stressed <- gsub("^(.)", "\\[\\1", new_stressed)
    new_stressed <- gsub("(.)$", "\\1\\]", new_stressed)
    stress_positions_vector <- append(stress_positions_vector, new_stressed)
    temp_stressless <- gsub("1", "", prosodic_shape)
    temp_stressless_split <- unlist(strsplit(temp_stressless, split = " "))
  }
  return(stress_positions_vector)
}

list_4_5_syllable_stresses <- list()
for(i in 1:length(length_4_5_syll)){
  temp_stresses <- generate_stress_positions(length_4_5_syll[i])
  list_4_5_syllable_stresses[[i]] <- temp_stresses
}
names(list_4_5_syllable_stresses) <- length_4_5_syll

# list_4_5_syllable_parses, list_4_5_syllable_stresses

make_surface_to_parse_pairs <- function(list_parses, list_stresses){
  stress_to_parse_list <- list()
  for(i in 1:length(list_parses)){
    temp_pair_vector <- c()
    for(j in 1:length(list_4_5_syllable_parses[[i]])){
      temp_split <- unlist(strsplit(list_4_5_syllable_parses[[i]][[j]], split = " "))
      stressed_number <- grep("1", temp_split)
      matching_stressed_form <- list_stresses[[i]][[stressed_number]]
      temp_pair <- paste(matching_stressed_form, "\\->", list_4_5_syllable_parses[[i]][[j]], sep = " ")
      temp_pair_vector <- append(temp_pair_vector, temp_pair)
    }
    stress_to_parse_list[[i]] <- temp_pair_vector
  }
  return(stress_to_parse_list)
}
names(big_stress_parse_list) <- length_4_5_syll


#Parse-Seg	Parse-Syllable	FtBin	NonFinality	NonFinality-Foot	NonInitiality-Ft	WSP	Trochee	Iamb	All-Ft-L	All-Ft-R	Leftmost	Rightmost	Align-wd-L	Align-wd-R	Rh-Contour	Ident-Prom
generate_violation_profile <- function(UR, prosodic_shape){
  default_violations <- rep(0, 17)
  
  # For Parse-Seg
  if(length(grep("\\)C", prosodic_shape)) > 0){
    default_violations[1] <- 1
  }
  
  # For Parse-Syllable
  if(length(grep("\\([HLV]:?1C?\\)", prosodic_shape)) > 0 ){
    temp_length <- length(unlist(strsplit(prosodic_shape, split = " ")))
    default_violations[2] <- temp_length -1
  }
  else{
    temp_length <- length(unlist(strsplit(prosodic_shape, split = " ")))
    default_violations[2] <- temp_length - 2
  }
  
  # For FtBin
  if(length(grep("\\([LV]1\\)", prosodic_shape)) > 0){
    default_violations[3] <- 1
  }
  
  # For NonFinality
  if(length(grep("V:?1", prosodic_shape)) > 0){
    default_violations[4] <- 1
  }
  
  # For NonFinality-Foot
  if(length(grep("\\)$", prosodic_shape)) > 0 ){
    default_violations[5] <- 1
  }
  
  # For NonInitiality-Ft
  if(length(grep("^\\(", prosodic_shape)) > 0 ){
    default_violations[6] <- 1
  }
  
  # For WSP
  unstressed_heavy <- c("H", "H)", "V:", "V:)", "VC", "VC)", "V:C", "V:)C", "V:C)")
  for(i in unlist(strsplit(prosodic_shape, split = " "))){
    if(i %in% unstressed_heavy){
      default_violations[7] <- default_violations[7] + 1
    }
  }
  
  # For Trochee
  if(length(grep("\\([HL] ", prosodic_shape)) > 0){
    default_violations[8] <- 1
  }
  
  # For Iamb
  if(length(grep("[HLV]:?\\)", prosodic_shape)) > 0){
    default_violations[9] <- 1
  }
  
  # For All-Ft-L
  left_edge <- grep("\\(", unlist(strsplit(prosodic_shape, split = " ")))
  default_violations[10] <- abs(1 - left_edge)
  
  # For All-Ft-R
  right_edge <- grep("\\)", unlist(strsplit(prosodic_shape, split = " ")))
  default_violations[11] <- length(unlist(strsplit(prosodic_shape, split = " "))) - right_edge
  
  # For Leftmost
  stressed_position <- grep("1", unlist(strsplit(prosodic_shape, split = " ")))
  default_violations[[12]] <- abs(1 - stressed_position)
  
  # For Rightmost
  stressed_position <- grep("1", unlist(strsplit(prosodic_shape, split = " ")))
  default_violations[13] <- length(unlist(strsplit(prosodic_shape, split = " "))) - stressed_position
  
  # For Align-Wd-L
  if(length(grep("^\\(", prosodic_shape)) == 0){
    default_violations[14] <- 1
  }
  
  # For Align-Wd-R
  if(length(grep("\\)$", prosodic_shape)) == 0){
    default_violations[15] <- 1
  }
  
  
  # For Rh-Contour
    # do as regex
  rh_shapes <- "(\\(L L1\\))|(\\(L V1\\))|(\\(H1 L\\))|(\\(H1 V\\))"
  if(length(grep(rh_shapes, prosodic_shape)) > 0){
    default_violations[16] <- 1
  }
  
  # For Ident-Prom
  temp_UR <- gsub("\\/", "", UR)
  #temp_UR <- gsub("\\\ ", "", temp_UR)
  temp_prosodic_shape <- gsub("\\(", "", prosodic_shape)
  temp_prosodic_shape <- gsub("\\)", "", temp_prosodic_shape)
  if(temp_UR != temp_prosodic_shape){
    default_violations[17] <- 1
  }
  return(default_violations)
}


big.data.frame <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# Generate the big tableau with all violations
for(i in 1:length(list_4_5_syllable_parses)){
  UR <- names(list_4_5_syllable_parses)[i]
  for(j in 1:length(list_4_5_syllable_parses[[i]])){
    violation_profile <- generate_violation_profile(UR, list_4_5_syllable_parses[[i]][j])
    if(j > 1){
      UR <- ""
    }
    else{
      UR <- gsub("^(.)", "/\\1", UR)
      UR <- gsub("(.)$", "\\1/", UR)
    }
    input_parse_pair <- big_stress_parse_list[[i]][j]
    if(j == 1){
      freq <- 1
    }
    else{
      freq <- ""
    }
    temp_vector <- c(UR, input_parse_pair, freq, violation_profile)
    big.data.frame <- rbind(big.data.frame, temp_vector)
  }

}
final_frame <- big.data.frame[-c(1), ]

# 
write.table(final_frame, "Syllable_4_5_Tableaux.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)


