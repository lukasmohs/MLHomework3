###markov random field
#inputs: list of functions mapping from (y,u,v) tuple to multiplier

#1 age
#2 smoke
#3 gender (F/M)
#4 HDL
#5 LDL
#6 diabetes
#7 fh (cvd)
#8 bp
#9 angina
#10 stroke
#11 mi
#12 depression
#13 statin (remember the other variables are pre-treatment values,
#     and DAG reflects that #
# Variant of code from Weiss et al. AMIA Annual Symposium 2015.

mi_model <- function(exposed_to=2) {
  randomize_on <- vector()
  p_on <- vector()
  
  nvar <- 13
  outcome <- 11
  exposure <- exposed_to
  
  ## NOTE: These potential definitions work only when the
  ## desired value is the last effective line in the
  ## function. Thus the structure should always use else ifs and
  ## only a single lone if at the start of the function. For
  ## example:
  ## potential_function <- function(x,failv=vector()) {
  ##      if (...) 0.4
  ##      else if (...) 0.3
  ##      else 0
  ## }        
  ## TODO - for clarity and to reduce risk of bugs, should
  ## probably change to explicitly use return(value) instead.
  fsmoke <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[2] == 1) -0.5
    else 0
  }
  fgender <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[3] == 1) -0.1
    else 0
  }
  fhdl <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[4] == 1) -1
    else 0
  }
  fldl <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[5] == 1) -0.5
    else 0
  }
  fdiabetes <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[6] == 1) -0.5
    else 0
  }
  ffh <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[7] == 1) -1
    else 0
  }
  fbp <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[8] == 1) -1
    else 0
  }
  fangina <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[9] == 1) -1
    else 0
  }
  fstroke <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[10] == 1) -3
    else 0
  }
  fmi <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[11] == 1) -4
    else 0
  }
  fdep <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[12] == 1) 1
    else 0
  }
  fstatin <- function(x,failv=vector()) { #x is (y,u,v)
    if(x[13] == 1) -0.5
    else 0
  }
  
  # functions will fail if randomize_on affects them (=removes arc) #
  fage_angina <- function(x,failv=vector()) {
    if(9 %in% failv) 0
    else if(x[1] == 1 & x[9] == 1) 0.5
    else 0
  }
  fage_diabetes <- function(x,failv=vector()) {
    if(6 %in% failv) 0
    else if(x[1] == 1 & x[6] == 1) 0.25
    else 0
  }
  fage_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if(x[1] == 1 & x[11] == 1) 0.5
    else 0
  }
  fage_stroke <- function(x,failv=vector()) {
    if(10 %in% failv) 0
    else if(x[1] == 1 & x[10] == 1) 0.25
    else 0
  }	
  fsmoke_bp <- function(x,failv=vector()) {
    if(8 %in% failv) 0
    else if(x[2] == 1 & x[8] == 1) 0.25
    else 0
  }
  fsmoke_ldl <- function(x,failv=vector()) {
    if(5 %in% failv) 0
    else if(x[2] == 1 & x[5] == 1) 0.25
    else 0
  }
  fsmoke_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if(x[2] == 1 & x[11] == 1) 0.1
    else 0
  }
  fsmoke_diabetes <- function(x,failv=vector()) {
    if(6 %in% failv) 0
    else if(x[2] == 1 & x[6] == 1) 0.25
    else 0
  }
  fgender_smoke <- function(x,failv=vector()) {
    if(2 %in% failv) 0
    else if(x[3] == 1 & x[2] == 1) -0.25
    else 0
  }
  fgender_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if(x[3] == 1 & x[11] == 1) 0.25
    else 0
  }
  fhdl_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if(x[4] == 1 & x[11] == 1) -0.25
    else 0
  }
  fldl_bp <- function(x,failv=vector()) {
    if(8 %in% failv) 0
    else if(x[5] == 1 & x[8] == 1) 0.25
    else 0
  }
  fldl_angina <- function(x,failv=vector()) {
    if(9 %in% failv) 0
    else if(x[5] == 1 & x[9] == 1) 0.5
    else 0
  }
  fldl_statin <- function(x,failv=vector()) {
    if(13 %in% failv) 0
    else if(x[5] == 1 & x[13] == 1) 1
    else if(x[5] == 0 & x[13] == 1) -4 # was -10, but our system is testing the hypo of broader use of statins in borderline LDL cases. e^-4:1 is not that unreasonable #
    else 0
  }
  fldl_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if(x[5] == 1 & x[11] == 1) 0.5
    else 0
  }
  fdiabetes_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if(x[6] == 1 & x[11] == 1) 0.25
    else 0
  }
  fdiabetes_stroke <- function(x,failv=vector()) {
    if(10 %in% failv) 0
    else if(x[6] == 1 & x[10] == 1) 0.5
    else if(x[6] == 0 & x[10] == 0) 0.5
    else 0
  }
  ffh_diabetes <- function(x,failv=vector()) {
    if(6 %in% failv) 0
    else if(x[7] == 1 & x[6] == 1) 0.5
    else if(x[7] == 0 & x[6] == 0) 0.25
    else 0
  }
  ffh_stroke <- function(x,failv=vector()) {
    if(10 %in% failv) 0
    else if(x[7] == 1 & x[10] == 1) 0.25
    else if(x[7] == 0 & x[10] == 0) 0.25
    else 0
  }
  ffh_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if(x[7] == 1 & x[11] == 1) 1
    else if(x[7] == 0 & x[11] == 0) 0.5
    else 0
  }
  fbp_angina <- function(x,failv=vector()) {
    if(9 %in% failv) 0
    else if(x[8] == 1 & x[9] == 1) 0.25
    else if(x[8] == 0 & x[9] == 0) 0.25
    else 0
  }
  fbp_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if(x[8] == 1 & x[11] == 1) 0.25
    else 0
  }
  fsmokediabetesfh_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if( sum(x[2]==1)+sum(x[6] == 1)+sum(x[7] == 1)==1 & x[11] == 1) 0.5
    else if( x[2] == 1 & x[6] == 1 & x[11] == 1) 1
    else if( sum(x[2]==1)+sum(x[6] == 1)+sum(x[7] == 1)==2 & x[11] == 1) 0.75
    else 0
  }
  fbp_stroke <- function(x,failv=vector()) {
    if(10 %in% failv) 0
    else if(x[8] == 1 & x[10] == 1) 0.25
    else 0
  }
  fdep_smoke <- function(x,failv=vector()) {
    if(2 %in% failv) 0
    else if(xor(x[12]==1,x[2]==1)) 0
    else 1
  }
  fdep_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if(xor(x[12]==1,x[11]==1)) 0
    else 0.5
  }
  fstatinhdlldlsmoking_mi <- function(x,failv=vector()) {
    if(11 %in% failv) 0
    else if(x[2]==0 & x[4]==1 & x[5]==1 & x[13]==1 & x[11] == 1) 0.1
    else if(x[2]==1 & x[4]==1 & x[5]==1 & x[13]==1 & x[11] == 1) 0.4 # smoking-statin combo is bad for diabetes therefore mi, worse than not taking statin #
    else if(x[4]==0 & x[5]==1 & x[13]==1 & x[11] == 1) 0
    else if(x[4]==1 & x[5]==1 & x[13]==0 & x[11] == 1) 0.2 # not taking a recommended statin is bad #
    else if(x[4]==0 & x[5]==1 & x[13]==0 & x[11] == 1) 1
    else if(          x[5]==0 & x[13]==1 & x[11] == 1) 0.5 # statin administered to normal LDL px #
    else 0
  }
  fstatin_diabetes <- function(x,failv=vector()) {
    if(6 %in% failv) 0
    else if(xor(x[6]==1,x[13]==1)) 0
    else 0.25
  }
  
  fs <- list(fsmoke_mi,fage_angina,fage_diabetes,fage_mi,fage_stroke,
             fsmoke_bp,fsmoke_ldl,fsmoke_mi,fsmoke_diabetes,fgender_smoke,
             fgender_mi,fhdl_mi,fldl_bp,fldl_angina,fldl_mi,fldl_statin,fdiabetes_mi,
             fdiabetes_stroke,ffh_diabetes,ffh_stroke,fbp_angina,fbp_mi,
             ffh_mi,fbp_stroke,
             fsmoke, fgender, fhdl, fldl, fdiabetes, ffh, fbp, fangina,
             fmi, fstroke, fsmokediabetesfh_mi,
             fdep, fdep_smoke, fdep_mi,
             fstatin, fstatinhdlldlsmoking_mi
  )
  ll <- function(s=sample,funs = fs, randomize_on=vector()) {
    sum(unlist(lapply(funs,do.call,list(s,randomize_on))))
  }
  
  # convert number into binary #
  number2binary = function(number, noBits) {
    binary_vector = rev(as.numeric(intToBits(number)))
    if(missing(noBits)) {
      return(binary_vector)
    } else {
      binary_vector[-(1:(length(binary_vector) - noBits))]
    }
  }
  
  values <- t(matrix(unlist(lapply(0:(2^nvar-1),number2binary,noBits=nvar)),nvar,2^nvar))
  joint <- exp(apply(values,1,ll,randomize_on=randomize_on))
  joint <- joint/sum(joint)
  
  model <- data.frame(cbind(values,joint))
  names(model) <- c("age","smoke","gender","HDL","LDL","diabetes","fh_cvd","bp","angina","stroke","mi","depression","statin","joint")
  
  require(plyr)
  # make randomization adjustments #
  if(length(randomize_on)>0) {
    randv <- names(model)[randomize_on]
    margs <- ddply(model,randv,function(x) sum(x$joint))
    names(margs)[names(margs)=="V1"] <- "actual"
    margs$desired <- apply(subset(margs,select=-actual),1,function(x) prod(abs(p_on-x)))
    model <- merge(model,margs,by=randv)[c(names(model),"actual","desired")]
    model$joint <- with(model, joint*desired/actual)
    model <- subset(model,select=-c(actual,desired))
  }
  
  # make into factors #
  model[,-which(names(model)=="joint")] <- lapply(model[,-which(names(model)=="joint")],factor, labels=c("no","yes"))
  detach("package:plyr", unload=TRUE)
    
  model
}

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}

yuv_sample <- function(j=joint, vars=3) {
  #	min(which(runif(1)<cumsum(j)))-1
  number2binary(min(which(runif(1)<cumsum(j)))-1,vars)
  #which is u,v,y?
}

### Given joint, sample a data set ###
get_data = function(n, model) {
  df = data.frame(
    replicate(n,yuv_sample(model$joint,log2(nrow(model)))) %>% t()
    )
  names(df) = names(model)[-length(names(model))]
  df
}

m = mi_model(exposed_to = 13)
get_data(100, m)
