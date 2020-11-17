## regression helper functions
# functions

limit_outdf <- function(df) {
  df %>% 
    ungroup() %>% 
    select(VANID, cem_group)
}
make_analysis_vf <- function(match_res, voterfile = voterfile) {
  match_res %>% 
    ungroup() %>% 
    select(VANID, cem_group) %>% 
    distinct() %>% 
    left_join(voterfile)
}


attach_competition <- function(df) {# note requires vf_compet to be loaded
  df %>% 
    left_join(vf_compet)
}

create_model_factors <- function(df){
  df <- df %>% 
    mutate(Race = relevel(as.factor(Race), ref = "W"),
           election_type = relevel(as.factor(election_type), ref = "g"))
  df
}

create_model_data <- function(df, model_form) {
  df <- create_model_factors(df) %>% 
    simcf::extractdata(model_form, ., na.rm = TRUE)
  df
}

preprocess_lmer <- function(match_res, model_form) {
  ## process analysis df to pb_long df for analysis (creating wide pb table along the way)
  df <- make_analysis_vf(match_res, voterfile)
  df <- create_pb_long(df) %>% 
    ungroup() %>% 
    attach_competition()
  df
}


fit_lmer_model <- function(df, model_form) {
  ### run model
  res <- glmer(model_form, data = df, family = binomial(), nAGQ = 0) 
  # progbar$tick()$print()
  res
}
calc_margin_effect <- function(data, model_res){
  margins::dydx(data, model_res, "after_pb", change = c(0,1))[[1]] %>% mean() 
}


load_vf_compet <- function(vanids) {
  
  vf_compet <- readRDS("data/cleaned_R_results/wide_compet_clean.rds") %>% 
    filter(VANID %in% vanids)
  
  # load presidential results
  source("R_competitiveness/BOE_pres_process.R")
  
  ## attach pres, make long w/election & year
  vf_compet <- vf_compet %>% 
    rename_all(~str_remove(., "comp_")) %>% 
    attach_pres(pres_wide) 
  
  names(vf_compet) <- str_replace(names(vf_compet), "general", "g")
  names(vf_compet) <- str_replace(names(vf_compet), "primary", "p")
  
  vf_compet <- vf_compet %>% 
    gather(key = election, value = compet, -VANID) %>% 
    mutate(election = str_remove_all(election, "comp_")) %>%
    separate(election, c("year", "election_type")) %>% 
    mutate(year = as.numeric(year)) %>% 
    group_by(year, election_type) %>% 
    mutate(compet = replace_na(compet, mean(compet[compet >0], na.rm = T))) %>% 
    ungroup()
  
  # replace years where that person have an election w/NA
  vf_compet <- vf_compet %>% 
    mutate(compet = na_if(compet, "-99"))
  # return vf_compet
  vf_compet
}

## function adapted from margins package methods for glmer models (merMod)
sim_marginal_effects <- function(model, data, iterations = 100, type = "response", variables = "after_pb") {
  tmpmodel <- model
  vcov <- vcov(model)
  iterations = 100
  if (inherits(model, "merMod")) {
    coefs <- lme4::fixef(model)
    # Removing data from model for memory, but S4 class requires "frame"
    # to be data.frame class --- hacky way of "removing" it
    model@frame <- model@frame[NULL] 
  } else {
    coefs <- coef(model)
    tmpmodel[["model"]] <- NULL # remove data from model for memory
  }
  
  # check that vcov() only contains coefficients from model
  if (nrow(vcov) != length(coefs)) {
    vcov <- vcov[intersect(rownames(vcov), names(coef(model))), intersect(rownames(vcov), names(coef(model)))]
  }
  
  # simulate from multivariate normal
  coefmat <- MASS::mvrnorm(iterations, coefs, vcov)
  
  # estimate AME from from each simulated coefficient vector
  effectmat <- apply(coefmat, 1, function(coefrow) {
    tmpmodel@beta <- coefrow <- coefrow
    means <- colMeans(marginal_effects(model = tmpmodel, data = data, variables = "after_pb"), na.rm = TRUE)
    
    if (!is.matrix(means)) {
      matrix(means, ncol = 1L)
    }
    return(means)
  })
  # When length(variables) == 1, effectmat is a vector
  if (!is.matrix(effectmat)) {
    # Coerce to 1 row matrix
    effectmat <- matrix(effectmat, nrow = 1)
    # Rownames are lost in these cases
    rownames(effectmat) <- paste0("dydx_", variables)
  }
  # calculate the variance of the simulated AMEs
  vc <- var(t(effectmat))
  variances <- diag(vc)
  res <- list(meaneffect = mean(effectmat),
              CI = sqrt(variances)*1.96)
  res  
}
