hertzToBark <- function(Fi){
  # Transforms from Hertz to Bark
  return (26.81/(1+1960/Fi) - 0.53)
}

hertzToBarkDf <- function(Fi){
  # Transforms data frame from Hertz to Bark
  Fi = (26.81/(1+1960/Fi) - 0.53)
  colnames(Fi) = paste0(colnames(Fi), "_bark")
  return(Fi)
}

h2st <- function(h,base) {
  # Transforms from Hertz to semitone
  semi <- log(2^(1/12))
  return((log(h)-log(base))/semi)
}

hertzToSemitones <- function(f0, group){
  # Transforms from Hertz to semitone by group
  unique_group = unique(group)
  len.g = length(unique_group)
  semitones = rep(0, len.g)
  for(g in 1:len.g){
    # fix f0 being 0
    f0.g = f0[group==unique_group[g]]
    f0_min = min(f0.g[f0.g > 80])
    f0.g[f0.g==0] = f0_min
    
    # get base
    quant = quantile(f0[group==unique_group[g]], c(.1))
    f0_base = mean(f0[group==unique_group[g] & f0 < quant])
    semitones[group==unique_group[g]] = h2st(f0.g, f0_base)
  }
  return(semitones) 
}


hertzToSemitonesDf <- function(f0, group){
  # Transforms data frame from Hertz to semitone by group
  unique_group = unique(group)
  len.g = length(unique_group)
  semitones = cbind(f0)
  for(g in 1:len.g){
    # fix f0 being 0
    f0.g = f0[group==unique_group[g],]
    f0_min = min(f0.g[f0.g > 70])
    f0.g[f0.g==0] = f0_min
    
    # get base
    quant = quantile(unlist(f0.g), c(.1))
    f0_base = mean(f0.g[f0.g < quant])
    semitones[group==unique_group[g],] = h2st(f0.g, f0_base)
  }
  colnames(semitones) = paste0(colnames(semitones), "_st")
  return(semitones) 
}

zNormDf <- function(measure, group){
  # z-normalizes data frame by group
  unique_group = unique(group)
  len.g = length(unique_group)
  zmeasures = cbind(measure)
  for(g in 1:len.g){
    measure.g = measure[group==unique_group[g],]
    measure.mean = mean(unlist(measure.g))
    measure.sd = sd(unlist(measure.g))
    zmeasures[group==unique_group[g],] = (measure.g-measure.mean)/measure.sd
  }
  colnames(zmeasures) = paste0("z", colnames(measure))
  return(zmeasures) 
}