
# Read in the file of tableaux.
    # Set your working directory to the directory with the file "RIP_Stress_Grammar_BERLIN.txt"
tableaux <- prepare_data("~/Documents/Dropbox/RyanTex/Sandell Habilitation/OT Grammars/RIP Feet/New_Berlin/RIP_Stress_Grammar_BERLIN.txt")
# Make a copy of the original tableaux
original_tableaux_object <- tableaux
# Element 1 in the list object is the input name and a vector of candidate probabilities
# Element 2 in the list object is the tableaux: the full violation profiles of each candidate
# Element 3 in the list object is the list of candidates.

# limited to 2- and 3-syllable words
limited_2_3 <- grep("^[HL]1? [HL]?1? ?V:?1?C?$", names(all_prosodic_shapes_table))
# limited to 2-, 3-, 4-, and 5-syllable word
limited_2_3_4_5 <- grep("^[HL]1? [HL]?1? ?[HL]?1? ?[HL]?1? ?V:?1?C?$", names(all_prosodic_shapes_table))



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

# Used for the first generation of learning
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
#ptm <- proc.time()
#out <- simple_single_learning_loop(number_iterations = 10000, learning_rate = 0.005)
#proc.time() - ptm
#names(out[[1]]) <- names(tableaux[[1]])

# Used for any subsequent generations of learning beyond the first
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
  # for(i in 1:length(new_generation_tableaux[[1]])){
  #   names(final_learner_outcome[[1]][[i]]) <- new_generation_tableaux[[3]][[i]]
  # }
  return(final_learner_outcome)
}



iterated_generations <- function(number_generations = 5, no_it = 5000, l_r = 0.01){
  
  
  generational_data <- list()
  original_tableaux_object <- tableaux
  for(i in 1:number_generations){
    if(i == 1){
      # Set initial learner weights
      learner_weights <- rep(10,ncol(tableaux[[2]][[1]]))
      # Set the weight of IDENT-PROM to 0
      learner_weights[length(learner_weights)] <- 0
      
      learner_violations <- tableaux[[2]]
      
      learner_candidate_probs <- list()
      learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
      
      first_generation <- simple_single_learning_loop(number_iterations = no_it, learning_rate = l_r)
      # Save the results from the first generation
      generational_data[[1]] <- first_generation
      
      # Create learning data for the second generation
      new_generation_data <- c()
      for(j in 1:length(limited_2_3_4_5)){
        prosodic_shape <- names(all_prosodic_shapes_table[limited_2_3_4_5])[j]
        shape_tableau_number <- grep(paste("/", prosodic_shape, "/", sep=""),  names(tableaux[[3]]))
        temp_outputs <- sample(tableaux[[3]][[shape_tableau_number]], all_prosodic_shapes_table[limited_2_3_4_5][j]*10, prob = first_generation[[1]][[shape_tableau_number]], replace = TRUE)
        temp_outputs <- gsub(" \\\\->.*$", "", temp_outputs)
        temp_outputs <- gsub("\\[", "", temp_outputs)
        temp_outputs <- gsub("\\]", "", temp_outputs)
        new_generation_data <- append(new_generation_data, temp_outputs)
      }
      new_generation_data_table <- table(new_generation_data)
      
      new_shapes <- new_generation_data_table[-c(which(names(new_generation_data_table) %in% names(all_prosodic_shapes_table[limited_2_3_4_5])))]
      new_shapes_numbers <- c()
      for(j in 1:length(new_shapes)){
        new_shapes_numbers <- append(new_shapes_numbers, which(names(new_generation_data_table) == names(new_shapes)[j]))
      }
      # remove the "new shapes" because you don't have tableaux for them
      new_generation_data_table <- new_generation_data_table[-c(new_shapes_numbers)]
      generational_data[[1]][[6]] <- new_generation_data_table
      
      # Remove the missing inputs from the tableaux for the new generation
      tableaux_to_remove <- c()
      for(j in 1:length(names(all_prosodic_shapes_table[limited_2_3_4_5]))){
        temp_shape <- names(all_prosodic_shapes_table[limited_2_3_4_5])[j]
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
    }
    # Here begin learning for all generations beyond the first
    else{
      # Set initial learner weights
      learner_weights <- rep(10,ncol(new_generation_tableaux[[2]][[1]]))
      # Set the weight of IDENT-PROM to 0
      learner_weights[length(learner_weights)] <- 0
      
      learner_violations <- new_generation_tableaux[[2]]
      
      learner_candidate_probs <- list()
      learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
      
      
      current_generation <- simple_single_learning_loop_G2(number_iterations = no_it, learning_rate = l_r)
      generational_data[[i]] <- current_generation
      
      # Create new data for the next generation
      previous_generation_data_table <- new_generation_data_table
      previous_generation_tableaux <- new_generation_tableaux
      
      new_generation_data <- c()
      for(j in 1:length(previous_generation_data_table)){ 
        prosodic_shape <- names(previous_generation_data_table)[j]
        shape_tableau_number <- grep(paste("/", prosodic_shape, "/", sep=""),  names(previous_generation_tableaux[[3]]))
        if(length(shape_tableau_number) == 0){
          
        }
        else{
          temp_outputs <- sample(previous_generation_tableaux[[3]][[shape_tableau_number]], previous_generation_data_table[j], prob = current_generation[[1]][[shape_tableau_number]], replace = TRUE)
          temp_outputs <- gsub(" \\\\->.*$", "", temp_outputs)
          temp_outputs <- gsub("\\[", "", temp_outputs)
          temp_outputs <- gsub("\\]", "", temp_outputs)
          new_generation_data <- append(new_generation_data, temp_outputs)
        }
      }
      
      new_generation_data_table <- table(new_generation_data)
      
      new_shapes <- new_generation_data_table[-c(which(names(new_generation_data_table) %in% names(previous_generation_data_table)))]
      new_shapes_numbers <- c()
      for(j in 1:length(new_shapes)){
        new_shapes_numbers <- append(new_shapes_numbers, which(names(new_generation_data_table) == names(new_shapes)[j]))
      }
      # remove the "new shapes" because you don't have tableaux for them -- or, determine if those "new shapes" are in the original shape numbers
      if(length(new_shapes) > 0){
        for(j in 1:length(new_shapes)){
          if((names(new_shapes)[j] %in% names(all_prosodic_shapes_table[limited_2_3_4_5])) == TRUE){
            new_shapes_numbers <- new_shapes_numbers[-c(i)]
          }
        }
      }
      

      new_generation_data_table <- new_generation_data_table[-c(new_shapes_numbers)]
      generational_data[[i]][[6]] <- new_generation_data_table
      
      # Remove the missing inputs from the tableaux for the new generation
      tableaux_to_remove <- c()
      for(j in 1:length(names(previous_generation_data_table))){
        temp_shape <- names(previous_generation_data_table)[j]
        if(temp_shape %in% names(new_generation_data_table)){
          
        }
        else{
          shape_tableau_number <- grep(paste("/", temp_shape, "/", sep=""),  names(previous_generation_tableaux[[3]]))
          tableaux_to_remove <- append(tableaux_to_remove, shape_tableau_number)
        }
      }
      
      
      if(length(tableaux_to_remove > 0)){
        new_generation_tableaux <- previous_generation_tableaux
        new_generation_tableaux[[1]] <- new_generation_tableaux[[1]][-c(tableaux_to_remove)]
        new_generation_tableaux[[2]] <- new_generation_tableaux[[2]][-c(tableaux_to_remove)]
        new_generation_tableaux[[3]] <- new_generation_tableaux[[3]][-c(tableaux_to_remove)]
        new_generation_tableaux[[4]] <- new_generation_tableaux[[4]][-c(tableaux_to_remove)]
      }
      # Add back in any tableau that were present in the original tableaux. We want to find tableaux that are present in the original object, but not in the previous generation
      tableaux_to_add <- c()
      for(j in 1:length(names(all_prosodic_shapes_table[limited_2_3_4_5]))){
        temp_shape <- names(all_prosodic_shapes_table[limited_2_3_4_5])[j]
        if(((temp_shape %in% names(previous_generation_data_table)) == FALSE) & ((temp_shape %in% names(new_generation_data_table) ) == TRUE) ){
          shape_tableau_number <- grep(paste("/", temp_shape, "/", sep=""),  names(original_tableaux_object[[3]]))
          tableaux_to_add <- append(tableaux_to_add, shape_tableau_number)
        }
      }
      tableaux_to_add <- unique(tableaux_to_add)
      
      # When adding, you have to start at length(new_generation_tableaux[[1]])
      if(length(tableaux_to_add > 0)){
        new_item <- length(new_generation_tableaux[[1]])
        for(j in 1:length(tableaux_to_add)){
          new_item <- new_item +1
          new_generation_tableaux[[1]][new_item] <- original_tableaux_object[[1]][c(tableaux_to_add[j])]
          new_generation_tableaux[[2]][new_item] <- original_tableaux_object[[2]][c(tableaux_to_add[j])]
          new_generation_tableaux[[3]][new_item] <- original_tableaux_object[[3]][c(tableaux_to_add[j])]
          new_generation_tableaux[[4]][new_item] <- original_tableaux_object[[4]][c(tableaux_to_add[j])]
        }
      }
    }
  }
  return(generational_data)
}
