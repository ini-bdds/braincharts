################################################################################
#
# This is the primary code for the brain growth chart project.  You can use
# this if you are trying to extend the code; however, you may want to see the 
# wrapper scripts in the parent directory.
#
# Authors: Kristi Clark (kclark@ini.usc.edu)
#          Ryan Cabeen (rcabeen@ini.usc.edu)
#
# Date: December 2017
#
################################################################################

library(gamlss)
library(ggplot2)

split_input <- function(imagfile, covfile, outdir) 
{
    cat("... started splitting data\n")

    cat("... setting up\n")

    #imagfile="PNC_PNG_FS_imaging.csv"
    #covfile="PNC_PNG_ages_gender_WBvals.csv"
    outlier_criteria = 3 # Number of IQR to be considered an outlier

    # Load image data
    imag_data = read.csv(gzfile(imagfile, 'rt'))
    cov_data = read.csv(covfile)

    # Remove any duplicates
    imag_data = unique(imag_data)
    cov_data = unique(cov_data)

    # Whole brain values are: 
    # regions LeftHemi, RightHemi, EstimatedTotalIntraCranialVol 
    # (in there twice as part of FS_aseg and FS_wmparc)

    # Error checking
    imag_names = names(imag_data)
    my_imagnames = NULL
    index_imag = NULL
    index_value = 0
    index_imag_subjID = 0
    index_hemi = 0
    index_morph = 0

    a=1
    for (aa in 1:length(imag_names)) 
    {
        if ((imag_names[aa]!="subjID") & 
            (imag_names[aa]!="value")) 
        {
           my_imagnames[a]=imag_names[aa]
           index_imag[a]=aa
           a=a+1
        }
        if (imag_names[aa] == "value") 
        {
           index_value = aa
        }    
        if (imag_names[aa] == "hemi") 
        {
           index_hemi = aa
        }    
        if (imag_names[aa] == "morph") 
        {
           index_morph = aa
        }    

        if (imag_names[aa] == "subjID")
        {
           index_imag_subjID = aa
        }
    }

    if (index_value  ==  0) 
    {
       stop('ERROR: missing imaging data. looking for a column in ',imagfile,' called value\n')
    }

    if (index_imag_subjID  ==  0) 
    {
       stop('ERROR: looking for a column called subjID ',imagfile,' \n')
    }

    imag_variables=unique(imag_data[,c(my_imagnames)])
    num_imag=dim(imag_variables)[1]

    cov_names=names(cov_data)
    my_covnames=NULL
    index_cov=NULL
    index_cov_subjID=0
    index_gender=0
    index_age=0
    index_icv=0
    index_lharea=0
    index_rharea=0
    index_lhthick=0
    index_rhthick=0

    a=1
    for (aa in 1:length(cov_names)) 
    {
        if ((cov_names[aa]!="subjID") & 
            (cov_names[aa]!="gender") & 
            (cov_names[aa]!="age") & 
            (cov_names[aa]!="LHarea") & 
            (cov_names[aa]!="RHarea") & 
            (cov_names[aa]!="LHthick") & 
            (cov_names[aa]!="RHthick") & 
            (cov_names[aa]!="ICV")) 
        {
           my_covnames[a] = cov_names[aa] 
           index_cov[a] = aa
           a = a+1
        }

        if (cov_names[aa] == "subjID") 
        {
           index_cov_subjID = aa
        }    

        if (cov_names[aa] == "gender") 
        {
           index_gender = aa
        }    

        if (cov_names[aa] == "age")
        {
           index_age = aa
        }    

        if (cov_names[aa] == "ICV")
        {
           index_icv = aa
        }    
        if (cov_names[aa] == "LHarea") 
        {
           index_lharea = aa
        }    

        if (cov_names[aa] == "RHarea") 
        {
           index_rharea = aa
        }    

        if (cov_names[aa] == "LHthick") 
        {
           index_lhthick = aa
        }    

        if (cov_names[aa] == "RHthick") 
        {
           index_rhthick = aa
        }    

        if (cov_names[aa]!="subjID") 
        {
           index_cov[a] = aa
           my_covnames[a] = cov_names[aa] 
           a = a+1
        }
    }

    if (index_age == 0) 
    {
       stop('ERROR: missing age data. looking for a column in ',covfile,' called age\n')
    }

    if (index_gender == 0) 
    {
       stop('ERROR: missing gender data. looking for a column in ',covfile,' called gender\n')
    }

    if (index_cov_subjID == 0) 
    {
       stop('ERROR: looking for a column called subjID ',covfile,' \n')
    }

    if (index_icv == 0) 
    {
       stop('ERROR: looking for a column called ICV ',covfile,' \n')
    }

    if (index_lharea == 0) 
    {
       stop('ERROR: looking for a column called LHarea ',covfile,' \n')
    }

    if (index_rharea == 0)
    {
       stop('ERROR: looking for a column called RHarea ',covfile,' \n')
    }

    if (index_lhthick == 0) 
    {
       stop('ERROR: looking for a column called LHthick ',covfile,' \n')
    }

    if (index_rhthick == 0) 
    {
       stop('ERROR: looking for a column called RHthick ',covfile,' \n')
    }

    cov_variables=unique(cov_data[,c(my_covnames)])
    num_cov=dim(cov_variables)[2]

    cat("... processing data\n")

    # create the output directory if needed

    cat(sprintf("... creating: %s\n", outdir))
    dir.create(outdir, recursive=T, showWarnings = F)

    # Loop through image variables
    for (i in 1:num_imag) 
    {
        # Select a vector of data
        k = 1
        imag_subset_data = imag_data[which(imag_data[,c(names(imag_variables[k]))]==imag_variables[i,k]),]	   

        for (k in 2:dim(imag_variables)[2])
        {
             imag_subset_data = imag_subset_data[which(imag_subset_data[,c(names(imag_variables[k]))]==imag_variables[i,k]),]
        }

        imag_subset_data=imag_subset_data[,c(index_imag_subjID,index_value)]

        if ((!(is.nan(mean(imag_subset_data[,2])))) & 
            ((mean(imag_subset_data[,2]))!=0)) 
        {
          # remove outliers from image data
          min_outlier = quantile(imag_subset_data$value, 0.25) - outlier_criteria*IQR(imag_subset_data$value)

          max_outlier = quantile(imag_subset_data$value, 0.75) + outlier_criteria*IQR(imag_subset_data$value)

          imag_clean_subset = imag_subset_data[(imag_subset_data$value > quantile(imag_subset_data$value, 0.25) - outlier_criteria*IQR(imag_subset_data$value)) & (imag_subset_data$value < quantile(imag_subset_data$value, 0.75) + outlier_criteria*IQR(imag_subset_data$value)),]

           # Combine with covariate data
           imag_cov_subset = merge(x=imag_clean_subset,y=cov_data, by.x="subjID", by.y="subjID")

           # Stop here and save the files
           bn=paste("PNC_PNG",imag_variables[i,2],imag_variables[i,3],imag_variables[i,4],imag_variables[i,5],"data.csv",sep="_")
           outfile = file.path(outdir, bn)

           write.csv(imag_cov_subset,outfile,quote=FALSE,row.names=FALSE)
           cat(sprintf("...... wrote %s\n", outfile))
        }	  
    }

    cat("... finished splitting data\n")
}

estimate_brain_LMS_combo = function(infile, outbn, normalize)
{
    Moutfile = sprintf("%s_M.RData", outbn)
    Foutfile = sprintf("%s_F.RData", outbn)
    Soutfile = sprintf("%s_summary.csv", outbn)

    if (!file.exists(Soutfile)) {
      try(estimate_brain_LMS_nostart(infile, Moutfile, Foutfile, Soutfile, normalize))
    }

    if (!file.exists(Soutfile)) {
        try(estimate_brain_LMS(infile, Moutfile, Foutfile, Soutfile, normalize))
    }
}

estimate_brain_LMS <- function (infile, Moutfile, Foutfile, Soutfile, normalize) {
    indata=read.csv(infile)
    if (normalize==2) {
       # divide by ICV
       indata$value=indata$value/indata$ICV*1000000
    }
    if (normalize==3) {
       # divide by LHarea
       indata$value=indata$value/indata$LHarea
    }
    if (normalize==4) {
       # divide by RHarea
       indata$value=indata$value/indata$RHarea
    }
    if (normalize==5) {
       # divide by LHthick
       indata$value=indata$value/indata$LHthick
    }
    if (normalize==6) {
       # divide by RHthick
       indata$value=indata$value/indata$RHthick
    }

    # First split the data into males and females because these functions are
    # only applicable if "one" explanatory variable is available

    fd = indata[which(indata[,3]=="F"),]
    md = indata[which(indata[,3]=="M"),]

    md=md[which(md$value > 0),]
    fd=fd[which(fd$value > 0),]


    # choose best model
    # Start with males
    # Start with finding optimal BCCG model

    # Generic
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ age, family=BCCG,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=BCCG,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=BCCG,data=md,sigma.start=start_sigma)   
    }
    minBIC=modelfit$sbc
    bestmodel=modelfit
    Mmodel="BCCG"
    Msmoothing="none"

    # pb smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ pb(age), family=BCCG,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=BCCG,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=BCCG,data=md,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCCG"
         Msmoothing="pb"
    }

    # cs smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ cs(age), family=BCCG,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=BCCG,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=BCCG,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCCG"
         Msmoothing="cs"
    }

    # scs smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ scs(age), family=BCCG,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=BCCG,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=BCCG,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCCG"
         Msmoothing="scs"
    }

    # bfp smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ bfp(age), family=BCCG,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=BCCG,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=BCCG,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCCG"
         Msmoothing="bfp"
    }

    # Second: find optimal BCPE model
    # Generic
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ age, family=BCPE,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=BCPE,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=BCPE,data=md,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Mmodel="BCPE"
       Msmoothing="none"
    }

    # pb smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ pb(age), family=BCPE,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=BCPE,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=BCPE,data=md,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCPE"
         Msmoothing="pb"
    }

    # cs smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ cs(age), family=BCPE,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=BCPE,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=BCPE,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCPE"
         Msmoothing="cs"
    }

    # scs smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ scs(age), family=BCPE,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=BCPE,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=BCPE,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCPE"
         Msmoothing="scs"
    }

    # bfp smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ bfp(age), family=BCPE,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=BCPE,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=BCPE,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCPE"
         Msmoothing="bfp"
    }

    # Third find optimal GA model
    # Generic
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ age, family=GA,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=GA,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=GA,data=md,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Mmodel="GA"
       Msmoothing="none"
    }

    # pb smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ pb(age), family=GA,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=GA,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=GA,data=md,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="GA"
         Msmoothing="pb"
    }

    # cs smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ cs(age), family=GA,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=GA,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=GA,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="GA"
         Msmoothing="cs"
    }

    # scs smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ scs(age), family=GA,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=GA,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=GA,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="GA"
         Msmoothing="scs"
    }

    # bfp smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ bfp(age), family=GA,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=GA,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=GA,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="GA"
         Msmoothing="bfp"
    }

    # Find optimal TF model
    # Generic
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ age, family=TF,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=TF,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=TF,data=md,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Mmodel="TF"
       Msmoothing="none"
    }

    # pb smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ pb(age), family=TF,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=TF,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=TF,data=md,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="TF"
         Msmoothing="pb"
    }

    # cs smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ cs(age), family=TF,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=TF,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=TF,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="TF"
         Msmoothing="cs"
    }

    # scs smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ scs(age), family=TF,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=TF,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=TF,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="TF"
         Msmoothing="scs"
    }

    # bfp smoothing
    start_sigma=0.2
    if (log(sd(md$value))>0) {
       start_sigma=log(sd(md$value))
    }
    modelfit<-gamlss(value ~ bfp(age), family=TF,data=md,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=TF,data=md,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=TF,data=md,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="TF"
         Msmoothing="bfp"
    }
    save(bestmodel,md,file=Moutfile)

    # Now females
    # Start with finding optimal BCCG model

    # Generic
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ age, family=BCCG,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=BCCG,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=BCCG,data=fd,sigma.start=start_sigma)   
    }
    minBIC=modelfit$sbc
    bestmodel=modelfit
    Fmodel="BCCG"
    Fsmoothing="none"

    # pb smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ pb(age), family=BCCG,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=BCCG,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=BCCG,data=fd,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCCG"
         Fsmoothing="pb"
    }

    # cs smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ cs(age), family=BCCG,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=BCCG,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=BCCG,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCCG"
         Fsmoothing="cs"
    }

    # scs smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ scs(age), family=BCCG,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=BCCG,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=BCCG,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCCG"
         Fsmoothing="scs"
    }

    # bfp smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ bfp(age), family=BCCG,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=BCCG,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=BCCG,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCCG"
         Fsmoothing="bfp"
    }

    # Second: find optimal BCPE model
    # Generic
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ age, family=BCPE,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=BCPE,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=BCPE,data=fd,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Fmodel="BCPE"
       Fsmoothing="none"
    }

    # pb smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ pb(age), family=BCPE,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=BCPE,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=BCPE,data=fd,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCPE"
         Fsmoothing="pb"
    }

    # cs smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ cs(age), family=BCPE,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=BCPE,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=BCPE,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCPE"
         Fsmoothing="cs"
    }

    # scs smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ scs(age), family=BCPE,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=BCPE,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=BCPE,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCPE"
         Fsmoothing="scs"
    }

    # bfp smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ bfp(age), family=BCPE,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=BCPE,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=BCPE,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCPE"
         Fsmoothing="bfp"
    }

    # Third find optimal GA model
    # Generic
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ age, family=GA,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=GA,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=GA,data=fd,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Fmodel="GA"
       Fsmoothing="none"
    }

    # pb smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ pb(age), family=GA,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=GA,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=GA,data=fd,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="GA"
         Fsmoothing="pb"
    }

    # cs smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ cs(age), family=GA,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=GA,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=GA,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="GA"
         Fsmoothing="cs"
    }

    # scs smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ scs(age), family=GA,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=GA,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=GA,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="GA"
         Fsmoothing="scs"
    }

    # bfp smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ bfp(age), family=GA,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=GA,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=GA,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="GA"
         Fsmoothing="bfp"
    }

    # Find optimal TF model
    # Generic
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ age, family=TF,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=TF,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ age, family=TF,data=fd,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Fmodel="TF"
       Fsmoothing="none"
    }

    # pb smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ pb(age), family=TF,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=TF,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ pb(age), family=TF,data=fd,sigma.start=start_sigma)   
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="TF"
         Fsmoothing="pb"
    }

    # cs smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ cs(age), family=TF,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=TF,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ cs(age), family=TF,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="TF"
         Fsmoothing="cs"
    }

    # scs smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ scs(age), family=TF,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=TF,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ scs(age), family=TF,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="TF"
         Fsmoothing="scs"
    }

    # bfp smoothing
    start_sigma=0.2
    if (log(sd(fd$value))>0) {
       start_sigma=log(sd(fd$value))
    }
    modelfit<-gamlss(value ~ bfp(age), family=TF,data=fd,sigma.start=start_sigma)
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=TF,data=fd,sigma.start=start_sigma)
    }
    if (modelfit$converged==FALSE) {
       start_sigma=0.2
       if (modelfit$sigma.coefficients[1]>0) {
          start_sigma=modelfit$sigma.coefficients[1]
       }
       modelfit<-gamlss(value ~ bfp(age), family=TF,data=fd,sigma.start=start_sigma)
    }
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="TF"
         Fsmoothing="bfp"
    }

    save(bestmodel,fd,file=Foutfile)

    model_results=cbind(Mmodel,Msmoothing,Fmodel,Fsmoothing)
    write.csv(model_results,Soutfile,quote=FALSE,row.names=FALSE)
}

estimate_brain_LMS_nostart <- function (infile, Moutfile, Foutfile, Soutfile, normalize) {

    indata=read.csv(infile)
    if (normalize==2) {
       # divide by ICV
       indata$value=indata$value/indata$ICV*1000000
    }
    if (normalize==3) {
       # divide by LHarea
       indata$value=indata$value/indata$LHarea
    }
    if (normalize==4) {
       # divide by RHarea
       indata$value=indata$value/indata$RHarea
    }
    if (normalize==5) {
       # divide by LHthick
       indata$value=indata$value/indata$LHthick
    }
    if (normalize==6) {
       # divide by RHthick
       indata$value=indata$value/indata$RHthick
    }

    # First split the data into males and females because these functions are only
    # applicable if "one" explanatory variable is available

    fd = indata[which(indata[,3]=="F"),]
    md = indata[which(indata[,3]=="M"),]

    md=md[which(md$value > 0),]
    fd=fd[which(fd$value > 0),]

    # choose best model
    # Start with males
    # Start with finding optimal BCCG model

    # No smoothing
    modelfit<-gamlss(value ~ age, family=BCCG,data=md)
    minBIC=modelfit$sbc
    bestmodel=modelfit
    Mmodel="BCCG"
    Msmoothing="none"

    # pb smoothing
    modelfit<-gamlss(value ~ pb(age), family=BCCG,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCCG"
         Msmoothing="pb"
    }

    # cs smoothing
    modelfit<-gamlss(value ~ cs(age), family=BCCG,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCCG"
         Msmoothing="cs"
    }

    # scs smoothing
    modelfit<-gamlss(value ~ scs(age), family=BCCG,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCCG"
         Msmoothing="scs"
    }

    # bfp smoothing
    modelfit<-gamlss(value ~ bfp(age), family=BCCG,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCCG"
         Msmoothing="bfp"
    }

    # Second: find optimal BCPE model
    # Generic
    modelfit<-gamlss(value ~ age, family=BCPE,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Mmodel="BCPE"
       Msmoothing="none"
    }

    # pb smoothing
    modelfit<-gamlss(value ~ pb(age), family=BCPE,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCPE"
         Msmoothing="pb"
    }

    # cs smoothing
    modelfit<-gamlss(value ~ cs(age), family=BCPE,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCPE"
         Msmoothing="cs"
    }

    # scs smoothing
    modelfit<-gamlss(value ~ scs(age), family=BCPE,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCPE"
         Msmoothing="scs"
    }

    # bfp smoothing
    modelfit<-gamlss(value ~ bfp(age), family=BCPE,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="BCPE"
         Msmoothing="bfp"
    }

    # Third find optimal GA model
    # Generic
    modelfit<-gamlss(value ~ age, family=GA,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Mmodel="GA"
       Msmoothing="none"
    }

    # pb smoothing
    modelfit<-gamlss(value ~ pb(age), family=GA,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="GA"
         Msmoothing="pb"
    }

    # cs smoothing
    modelfit<-gamlss(value ~ cs(age), family=GA,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="GA"
         Msmoothing="cs"
    }

    # scs smoothing
    modelfit<-gamlss(value ~ scs(age), family=GA,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="GA"
         Msmoothing="scs"
    }

    # bfp smoothing
    modelfit<-gamlss(value ~ bfp(age), family=GA,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="GA"
         Msmoothing="bfp"
    }

    # Find optimal TF model
    # Generic
    modelfit<-gamlss(value ~ age, family=TF,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Mmodel="TF"
       Msmoothing="none"
    }

    # pb smoothing
    modelfit<-gamlss(value ~ pb(age), family=TF,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="TF"
         Msmoothing="pb"
    }

    # cs smoothing
    modelfit<-gamlss(value ~ cs(age), family=TF,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="TF"
         Msmoothing="cs"
    }

    # scs smoothing
    modelfit<-gamlss(value ~ scs(age), family=TF,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="TF"
         Msmoothing="scs"
    }

    # bfp smoothing
    modelfit<-gamlss(value ~ bfp(age), family=TF,data=md)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Mmodel="TF"
         Msmoothing="bfp"
    }
    save(bestmodel,md,file=Moutfile)

    # Now females
    # Start with finding optimal BCCG model
    # Generic
    modelfit<-gamlss(value ~ age, family=BCCG,data=fd)
    minBIC=modelfit$sbc
    bestmodel=modelfit
    Fmodel="BCCG"
    Fsmoothing="none"

    # pb smoothing
    modelfit<-gamlss(value ~ pb(age), family=BCCG,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCCG"
         Fsmoothing="pb"
    }

    # cs smoothing
    modelfit<-gamlss(value ~ cs(age), family=BCCG,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCCG"
         Fsmoothing="cs"
    }

    # scs smoothing
    modelfit<-gamlss(value ~ scs(age), family=BCCG,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCCG"
         Fsmoothing="scs"
    }

    # bfp smoothing
    modelfit<-gamlss(value ~ bfp(age), family=BCCG,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCCG"
         Fsmoothing="bfp"
    }

    # Second: find optimal BCPE model
    # Generic
    modelfit<-gamlss(value ~ age, family=BCPE,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Fmodel="BCPE"
       Fsmoothing="none"
    }

    # pb smoothing
    modelfit<-gamlss(value ~ pb(age), family=BCPE,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCPE"
         Fsmoothing="pb"
    }

    # cs smoothing
    modelfit<-gamlss(value ~ cs(age), family=BCPE,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCPE"
         Fsmoothing="cs"
    }

    # scs smoothing
    modelfit<-gamlss(value ~ scs(age), family=BCPE,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCPE"
         Fsmoothing="scs"
    }

    # bfp smoothing
    modelfit<-gamlss(value ~ bfp(age), family=BCPE,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="BCPE"
         Fsmoothing="bfp"
    }

    # Third find optimal GA model
    # Generic
    modelfit<-gamlss(value ~ age, family=GA,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Fmodel="GA"
       Fsmoothing="none"
    }

    # pb smoothing
    modelfit<-gamlss(value ~ pb(age), family=GA,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="GA"
         Fsmoothing="pb"
    }

    # cs smoothing
    modelfit<-gamlss(value ~ cs(age), family=GA,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="GA"
         Fsmoothing="cs"
    }

    # scs smoothing
    modelfit<-gamlss(value ~ scs(age), family=GA,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="GA"
         Fsmoothing="scs"
    }

    # bfp smoothing
    modelfit<-gamlss(value ~ bfp(age), family=GA,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="GA"
         Fsmoothing="bfp"
    }

    # Find optimal TF model
    # Generic
    modelfit<-gamlss(value ~ age, family=TF,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
       minBIC=modelfit$sbc
       bestmodel=modelfit
       Fmodel="TF"
       Fsmoothing="none"
    }

    # pb smoothing
    modelfit<-gamlss(value ~ pb(age), family=TF,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="TF"
         Fsmoothing="pb"
    }

    # cs smoothing
    modelfit<-gamlss(value ~ cs(age), family=TF,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="TF"
         Fsmoothing="cs"
    }

    # scs smoothing
    modelfit<-gamlss(value ~ scs(age), family=TF,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="TF"
         Fsmoothing="scs"
    }

    # bfp smoothing
    modelfit<-gamlss(value ~ bfp(age), family=TF,data=fd)
    if ((modelfit$sbc < minBIC) && (modelfit$converged==TRUE)) {
         minBIC=modelfit$sbc
         bestmodel=modelfit
         Fmodel="TF"
         Fsmoothing="bfp"
    }

    save(bestmodel,fd,file=Foutfile)

    model_results=cbind(Mmodel,Msmoothing,Fmodel,Fsmoothing)
    write.csv(model_results,Soutfile,quote=FALSE,row.names=FALSE)
}

CompareIndividual <- function (subjfile, gender, age, databasedir, databaseprefix, outfile) {

    cat("... started comparing individual\n")

    cat("... setting up\n")

    #subjfile="PNC0015_M10_FS_imaging.csv"
    #gender="M"
    #age=125
    #databasedir="output/models/none"
    #databaseprefix="PNC_PNG"
    #outfile="PNC0015_M10_percentiles.csv"

    # Error checking 
    if ((gender != "M") & (gender != "F")) {
       stop('ERROR: gender argument should be M or F\n')
    }

    # Load single subject data
    subj_data=read.csv(subjfile)

    # Remove any duplicates
    subj_data=unique(subj_data)

    # Error checking
    subj_names=names(subj_data)
    my_subjnames=NULL
    index_value = 0
    a=1
    for (aa in 1:length(subj_names)) {
        if ((subj_names[aa]!="value") & (subj_names[aa]!="subjID")) {
           my_subjnames[a]=subj_names[aa]
           a=a+1
        }
        if (subj_names[aa]=="value") {
           index_value = aa
        }    
    }
    if (index_value == 0) {
       stop('ERROR: missing imaging data. looking for a column in ',subjfile,' called value\n')
    }

    imag_variables=unique(subj_data[,c(my_subjnames)])
    num_imag=dim(imag_variables)[1]

    resultsALL=NULL

    cat("... processing morphometrics\n")

    # Cycle throug the data
    for (i in 1:num_imag) {

        # Find single data point for subject
        k = 1
        subj_subset_data = subj_data[which(subj_data[,c(names(imag_variables[k]))] == imag_variables[i,k]),]	   
        for (k in 2:dim(imag_variables)[2]) {
            subj_subset_data = subj_subset_data[which(subj_subset_data[,c(names(imag_variables[k]))] == imag_variables[i,k]),]
        }
        subj_subset_data=subj_subset_data[,index_value]

        # Find data in the database
        aa=paste(databasedir,databaseprefix,sep="/")
        bb=paste(aa,imag_variables[i,2],imag_variables[i,3],imag_variables[i,4],imag_variables[i,5],"model",gender,sep="_")
        DBfilename=paste(bb,"RData",sep=".")


        if (file.exists(DBfilename)) {
           cat(sprintf("...... processing: %s\n", DBfilename))
           missing_DB="N"
           load(DBfilename)
           # percentile calculation
           if (gender == "M") {
              percentile=round(pnorm(centiles.pred(bestmodel,xvalues=age,yval=subj_subset_data,type="z-scores",data=md,xname="age"))*100,0)
           }
           else {
                percentile=round(pnorm(centiles.pred(bestmodel,xvalues=age,yval=subj_subset_data,type="z-scores",data=fd,xname="age"))*100,0)
           }
        }
        else {
           cat(sprintf("...... skipped database: %s\n", DBfilename))
           missing_DB="Y"
           percentile=NA
        }

        subj_value=as.numeric(subj_subset_data)
        addrow <- cbind(imag_variables[i,],subj_value,percentile,missing_DB)	      
        resultsALL = rbind(resultsALL,addrow)

        # Save output one row at a time, slower but updates as it goes
        write.csv(resultsALL,outfile,quote=FALSE,row.names=FALSE)
    }
}

chart.plot <- function(model, dataset, atlas, region, hemi, metric, gender, output) {

  # example:

  # model: none
  # dataset: PNC_PNG
  # atlas: FS_aparc_2009
  # region: ctx_lh_G_and_S_cingul-Ant
  # hemi: L
  # metric: volume
  # gender: M
  # output: myplot.pdf 

  cent = c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6)

  # load the model data
  input = sprintf("output/models/%s/%s_%s_%s_%s_%s_model_%s.RData", model, dataset, atlas, region, hemi, metric, gender)
  load(input)
  obj <- bestmodel

  if (exists("md"))
  {
    xvar <- md$age / 12.0
  }
  else
  { 
    xvar <- fd$age / 12.0
  }

  fname <- obj$family[1]
  qfun <- paste("q", fname, sep = "")
  oxvar <- xvar[order(xvar)]
  oyvar <- obj$y[order(xvar)]

  scatter.df <- data.frame(age=oxvar, metric=oyvar)

  lpar <- length(obj$parameters)
  per <- rep(0, length(cent))

  lines.base.df <- data.frame(oxvar=oxvar)
  lines.df <- NULL

  for (var in cent) 
  {
      if (lpar == 1) 
      {
          newcall <- call(qfun, var/100, mu = fitted(obj, "mu")[order(xvar)])
      }
      else if (lpar == 2) 
      {
          newcall <- call(qfun, var/100, mu = fitted(obj, "mu")[order(xvar)],
              sigma = fitted(obj, "sigma")[order(xvar)])
      }
      else if (lpar == 3) 
      {
          newcall <- call(qfun, var/100, mu = fitted(obj, "mu")[order(xvar)],
              sigma = fitted(obj, "sigma")[order(xvar)], nu = fitted(obj,
                "nu")[order(xvar)])
      }
      else
      {
          newcall <- call(qfun, var/100, mu = fitted(obj, "mu")[order(xvar)],
              sigma = fitted(obj, "sigma")[order(xvar)], nu = fitted(obj,
                "nu")[order(xvar)], tau = fitted(obj, "tau")[order(xvar)])
      }

      ll <- eval(newcall)

      lines.df <- rbind(lines.df, data.frame(age=oxvar, metric=ll, centile=var))
  }

  lines.df$group <- as.factor(lines.df$centile)
  lines.df$size <- 5 * (1.0 - abs(lines.df$centile - 50.0) / 50.0)

  myplot <- ggplot()
  myplot <- myplot + geom_point(data=scatter.df, aes(x=age, y=metric, alpha=0.25)) 
  myplot <- myplot + geom_line(data=lines.df, aes(x=age, y=metric, color=group, group=group, size=size, alpha=0.25))
  myplot <- myplot + ggtitle(sprintf("Brain Growth Chart: %s %s %s %s", model, gender, region, metric))
  myplot <- myplot + labs(x="age (years)",y=metric) 

  ggsave(output, device="pdf")
}

################################################################################
# End
################################################################################
