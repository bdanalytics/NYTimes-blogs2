# NYTimes:Blogs:: Popular classification:: txtfeat
bdanalytics  

**  **    
**Date: (Fri) May 15, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTrain.csv
    New:        https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTest.csv  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4)
#packageVersion("snow")

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTrain.csv"
glb_newdt_url <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTest.csv"
glb_out_pfx <- "NYTBlogs_txtfeat_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newent_dataset <- TRUE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
glb_split_newdata_condition <- "<col_name> <condition_operator> <value>"    # or NULL
glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
glb_split_sample.seed <- 123               # or any integer
glb_drop_vars <- c(NULL) # or c("<col_name>")

#glb_max_fitent_obs <- 2238 # NULL # or any integer
glb_max_fitent_obs <- NULL # or any integer                         
glb_is_regression <- FALSE; glb_is_classification <- TRUE; glb_is_binomial <- TRUE

glb_rsp_var_raw <- "Popular"
# for classification, the response variable has to be a factor
glb_rsp_var <- "Popular.fctr"
# if the response factor is based on numbers e.g (0/1 vs. "A"/"B"), 
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- function(raw) {
    relevel(factor(ifelse(raw == 1, "Y", "N")), as.factor(c("Y", "N")), ref="N")
    #as.factor(paste0("B", raw))
    #as.factor(raw)    
}
glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0))
```

```
## [1] Y Y N N N
## Levels: N Y
```

```r
glb_map_rsp_var_to_raw <- function(var) {
    as.numeric(var) - 1
    #as.numeric(var)
    #levels(var)[as.numeric(var)]
    #c(" <=50K", " >50K")[as.numeric(var)]
}
glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0)))
```

```
## [1] 1 1 0 0 0
```

```r
if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# NewsDesk = the New York Times desk that produced the story
# SectionName = the section the article appeared in (Opinion, Arts, Technology, etc.)
# SubsectionName = the subsection the article appeared in (Education, Small Business, Room for Debate, etc.)
# Headline = the title of the article
# Snippet = a small portion of the article text
# Abstract = a summary of the blog article, written by the New York Times
# WordCount = the number of words in the article
#   created WordCount.log

# PubDate = the publication date, in the format "Year-Month-Day Hour:Minute:Second"
glb_date_vars <- c("PubDate")

# UniqueID = a unique identifier for each article
glb_id_vars <- c("UniqueID")

glb_is_textual <- TRUE # vs. glb_is_numerical ???
#Sys.setlocale("LC_ALL", "C") # For english
glb_txt_vars <- c("Headline", "Snippet", "Abstract")   
glb_append_stop_words <- list() # NULL # or c("<freq_word>") 

# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitent_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBent_df))
#       numrows(glb_OOBent_df) = 1.1 * numrows(glb_newent_df)
#glb_sprs_thresholds <- c(0.982, 0.965, 0.965)
glb_sprs_thresholds <- c(0.982, 0.970, 0.970)
names(glb_sprs_thresholds) <- glb_txt_vars

# List transformed vars  
glb_exclude_vars_as_features <- c(NULL) # or c("<var_name>") 
if (glb_is_textual)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)
# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")
# List output vars (useful during testing in console)          
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                         grep(glb_rsp_var_out, names(glb_trnent_df), value=TRUE)) 

glb_impute_na_data <- TRUE            # or TRUE
glb_mice_complete.seed <- 144               # or any integer

glb_models_lst <- list(); glb_models_df <- data.frame()
# rpart:  .rnorm messes with the models badly
#         caret creates dummy vars for factor feats which messes up the tuning
#             - better to feed as.numeric(<feat>.fctr) to caret 
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](NYTBlogs_txtfeat_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 8.129  NA      NA
```

## Step `1.0: import data`

```r
glb_trnent_df <- myimport_data(url=glb_trnng_url, comment="glb_trnent_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/NYTimesBlogTrain.csv..."
## [1] "dimensions of data in ./data/NYTimesBlogTrain.csv: 6,532 rows x 10 cols"
##   NewsDesk      SectionName SubsectionName
## 1 Business Crosswords/Games               
## 2  Culture             Arts               
## 3 Business     Business Day       Dealbook
## 4 Business     Business Day       Dealbook
## 5  Science           Health               
## 6  Science           Health               
##                                            Headline
## 1                                  More School Daze
## 2      New 96-Page Murakami Work Coming in December
## 3 Public Pension Funds Stay Mum on Corporate Expats
## 4                             Boot Camp for Bankers
## 5                     Of Little Help to Older Knees
## 6                     A Benefit of Legal Marijuana 
##                                                                                                                                                                                                                           Snippet
## 1                                                                                                                                                                  A puzzle from Ethan Cooper that reminds me that a bill is due.
## 2                                                                            The Strange Library will arrive just three and a half months after Mr. Murakamis latest novel, Colorless Tsukuru Tazaki and His Years of Pilgrimage.
## 3                                      Public pension funds have major stakes in American companies moving overseas to cut their tax bills. But they are saying little about the strategy, which could hurt the nations tax base.
## 4                                                                         As they struggle to find new business to bolster sluggish earnings, banks consider the nations 25 million veterans and service members ideal customers.
## 5                                         Middle-aged and older patients are unlikely to benefit in the long term from surgery to repair tears in the meniscus, pads of cartilage in the knee, a new review of studies has found.
## 6 A new study has found evidence that legal access to marijuana is associated with fewer opioid overdose deaths, but researchers said their findings should not be used as the basis for the wide adoption of legalized cannabis.
##                                                                                                                                                                                                                          Abstract
## 1                                                                                                                                                                  A puzzle from Ethan Cooper that reminds me that a bill is due.
## 2                                                                            The Strange Library will arrive just three and a half months after Mr. Murakamis latest novel, Colorless Tsukuru Tazaki and His Years of Pilgrimage.
## 3                                      Public pension funds have major stakes in American companies moving overseas to cut their tax bills. But they are saying little about the strategy, which could hurt the nations tax base.
## 4                                                                         As they struggle to find new business to bolster sluggish earnings, banks consider the nations 25 million veterans and service members ideal customers.
## 5                                         Middle-aged and older patients are unlikely to benefit in the long term from surgery to repair tears in the meniscus, pads of cartilage in the knee, a new review of studies has found.
## 6 A new study has found evidence that legal access to marijuana is associated with fewer opioid overdose deaths, but researchers said their findings should not be used as the basis for the wide adoption of legalized cannabis.
##   WordCount             PubDate Popular UniqueID
## 1       508 2014-09-01 22:00:09       1        1
## 2       285 2014-09-01 21:14:07       0        2
## 3      1211 2014-09-01 21:05:36       0        3
## 4      1405 2014-09-01 20:43:34       1        4
## 5       181 2014-09-01 18:58:51       1        5
## 6       245 2014-09-01 18:52:22       1        6
##      NewsDesk      SectionName SubsectionName
## 226    Styles                                
## 995                                          
## 3327                                         
## 4753                Multimedia               
## 4802 Business Crosswords/Games               
## 6463   TStyle                                
##                                                   Headline
## 226  For Tavi Gevinson, Fashion Takes a Back Seat, for Now
## 995          Reconsidering What to Call an Extremist Group
## 3327     Clinton's Diagnosis of What's Wrong With Politics
## 4753       'Off Color' and on Target About Race in America
## 4802                      Daniel Finkel's Circle-Toss Game
## 6463                                     Entering the Void
##                                                                                                                                                                            Snippet
## 226                                                Tavi Gevinson, the teenage fashion star turned Broadway actress, wont be much of a player at New York Fashion Week this season.
## 995                                                    Editors have decided to adjust how The Times refer to an Islamic extremist group that controls territory in Syria and Iraq.
## 3327 Hillary Rodham Clinton continued to laugh off questions about her presidential aspirations on Tuesday, but she did shed some light on what she thinks is wrong in Washington.
## 4753              Off Color, a New York Times video series, looks at how artists of color are making sharp social commentary about race in America through comedy and performance.
## 4802                                                                                            By math educator Daniel Finkel, a puzzle thats childs play. Can you figure it out?
## 6463                      The Spanish artist Miquel Barcel closely examines the basic materials of life in response to Edward Hirsch questioning his own belief in a higher power.
##                                                                                                                                                                           Abstract
## 226                                                Tavi Gevinson, the teenage fashion star turned Broadway actress, wont be much of a player at New York Fashion Week this season.
## 995                                                    Editors have decided to adjust how The Times refer to an Islamic extremist group that controls territory in Syria and Iraq.
## 3327 Hillary Rodham Clinton continued to laugh off questions about her presidential aspirations on Tuesday, but she did shed some light on what she thinks is wrong in Washington.
## 4753              Off Color, a New York Times video series, looks at how artists of color are making sharp social commentary about race in America through comedy and performance.
## 4802                                                                                            By math educator Daniel Finkel, a puzzle thats childs play. Can you figure it out?
## 6463                      The Spanish artist Miquel Barcel closely examines the basic materials of life in response to Edward Hirsch questioning his own belief in a higher power.
##      WordCount             PubDate Popular UniqueID
## 226        459 2014-09-04 16:55:57       0      226
## 995        301 2014-09-15 16:05:13       0      995
## 3327       236 2014-10-14 14:45:51       0     3327
## 4753       393 2014-11-02 05:00:13       0     4753
## 4802      1628 2014-11-03 12:00:04       1     4802
## 6463       264 2014-11-27 12:00:09       0     6463
##      NewsDesk SectionName  SubsectionName
## 6527  Foreign                            
## 6528              Opinion Room For Debate
## 6529  Foreign                            
## 6530   TStyle                            
## 6531           Multimedia                
## 6532 Business                            
##                                                                        Headline
## 6527                                     1914: Russians Dominate in East Poland
## 6528                                             Finding a Secretary of Defense
## 6529                         1889: Metropolitan Opera House Reopens in New York
## 6530                         The Daily Gift: Picasso Plates for Creative Dining
## 6531                                          Racing From New York to Barcelona
## 6532 Math Anxiety: Why Hollywood Makes Robots of Alan Turing and Other Geniuses
##                                                                                                                                                                                             Snippet
## 6527                                                                                                      From the International Herald Tribune archives: Russians dominate in East Poland in 1914.
## 6528                                                                                             If Chuck Hagel isn't the right Pentagon chief to respond to an onslaught of global crises, who is?
## 6529                                                                                      From the International Herald Tribune archives: The Metropolitan Opera House reopens in New York in 1889.
## 6530                                                                                                                      Each day until Christmas, the editors of T share a new holiday gift idea.
## 6531                                                      A sailboat race from New York to Barcelona was the setting for a thrilling  and sometimes terrifying  video about this challenging sport.
## 6532 The visionary who stares at formulas written on walls or mirrors  or better yet, thin air  has become a Hollywood trope. So has the depiction of the genius who cant connect with real people.
##                                                                                                                                                                                            Abstract
## 6527                                                                                                      From the International Herald Tribune archives: Russians dominate in East Poland in 1914.
## 6528                                                                                             If Chuck Hagel isn't the right Pentagon chief to respond to an onslaught of global crises, who is?
## 6529                                                                                      From the International Herald Tribune archives: The Metropolitan Opera House reopens in New York in 1889.
## 6530                                                                                                                      Each day until Christmas, the editors of T share a new holiday gift idea.
## 6531                                                      A sailboat race from New York to Barcelona was the setting for a thrilling  and sometimes terrifying  video about this challenging sport.
## 6532 The visionary who stares at formulas written on walls or mirrors  or better yet, thin air  has become a Hollywood trope. So has the depiction of the genius who cant connect with real people.
##      WordCount             PubDate Popular UniqueID
## 6527       176 2014-11-30 13:48:40       0     6527
## 6528      1597 2014-11-30 13:27:23       0     6528
## 6529       214 2014-11-30 09:44:57       0     6529
## 6530        61 2014-11-30 09:00:43       0     6530
## 6531       441 2014-11-30 09:00:22       0     6531
## 6532       921 2014-11-30 07:00:40       0     6532
## 'data.frame':	6532 obs. of  10 variables:
##  $ NewsDesk      : chr  "Business" "Culture" "Business" "Business" ...
##  $ SectionName   : chr  "Crosswords/Games" "Arts" "Business Day" "Business Day" ...
##  $ SubsectionName: chr  "" "" "Dealbook" "Dealbook" ...
##  $ Headline      : chr  "More School Daze" "New 96-Page Murakami Work Coming in December" "Public Pension Funds Stay Mum on Corporate Expats" "Boot Camp for Bankers" ...
##  $ Snippet       : chr  "A puzzle from Ethan Cooper that reminds me that a bill is due." "The Strange Library will arrive just three and a half months after Mr. Murakamis latest novel, Colorless Tsukuru Tazaki and His"| __truncated__ "Public pension funds have major stakes in American companies moving overseas to cut their tax bills. But they are saying little"| __truncated__ "As they struggle to find new business to bolster sluggish earnings, banks consider the nations 25 million veterans and service "| __truncated__ ...
##  $ Abstract      : chr  "A puzzle from Ethan Cooper that reminds me that a bill is due." "The Strange Library will arrive just three and a half months after Mr. Murakamis latest novel, Colorless Tsukuru Tazaki and His"| __truncated__ "Public pension funds have major stakes in American companies moving overseas to cut their tax bills. But they are saying little"| __truncated__ "As they struggle to find new business to bolster sluggish earnings, banks consider the nations 25 million veterans and service "| __truncated__ ...
##  $ WordCount     : int  508 285 1211 1405 181 245 258 893 1077 188 ...
##  $ PubDate       : chr  "2014-09-01 22:00:09" "2014-09-01 21:14:07" "2014-09-01 21:05:36" "2014-09-01 20:43:34" ...
##  $ Popular       : int  1 0 0 1 1 1 0 1 1 0 ...
##  $ UniqueID      : int  1 2 3 4 5 6 7 8 9 10 ...
##  - attr(*, "comment")= chr "glb_trnent_df"
## NULL
```

```r
if (glb_is_separate_newent_dataset) {
    glb_newent_df <- myimport_data(url=glb_newdt_url, comment="glb_newent_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_entity_df <- myrbind_df(glb_trnent_df, glb_newent_df); 
    comment(glb_entity_df) <- "glb_entity_df"
} else {
    glb_entity_df <- glb_trnent_df; comment(glb_entity_df) <- "glb_entity_df"
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newent_df <- glb_trnent_df[sample(1:nrow(glb_trnent_df),
                                          max(2, nrow(glb_trnent_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newent_df <- do.call("subset", 
                list(glb_trnent_df, parse(text=glb_split_newdata_condition)))
            glb_trnent_df <- do.call("subset", 
                list(glb_trnent_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnent_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newent_df <- glb_trnent_df[!split, ] 
                glb_trnent_df <- glb_trnent_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnent_df <- glb_entity_df
            comment(glb_trnent_df) <- "glb_trnent_df"
            glb_newent_df <- glb_entity_df
            comment(glb_newent_df) <- "glb_newent_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newent_df) <- "glb_newent_df"
    myprint_df(glb_newent_df)
    str(glb_newent_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_trnent_df)
        str(glb_trnent_df)        
    }
}         
```

```
## [1] "Reading file ./data/NYTimesBlogTest.csv..."
## [1] "dimensions of data in ./data/NYTimesBlogTest.csv: 1,870 rows x 9 cols"
##   NewsDesk      SectionName SubsectionName
## 1  Culture                                
## 2  Culture             Arts               
## 3 Business Crosswords/Games               
## 4 Business     Business Day       Dealbook
## 5  Science           Health               
## 6  Science           Health               
##                                                             Headline
## 1                                         'Birdman' Tops the Gothams
## 2                     'Sleepy Hollow' Recap: A Not-So-Shocking Death
## 3                                        Drinking Buddy For Falstaff
## 4 Encouraging Public Service, Through Wall Street's 'Revolving Door'
## 5                           Therapy Prevents Repeat Suicide Attempts
## 6                                            Hoping for a Good Death
##                                                                                                                                                 Snippet
## 1                                                    The backstage tale won two awards; Citizenfour, the Edward Snowden documentary, was also a winner.
## 2                                                                      In the fall season finale, a question of where the series has many places to go.
## 3                                                                                                       In which Timothy Polin reveals his potty mouth.
## 4 The debate about pay for Wall Street executives who take government jobs appears to be based more on a populist shakedown than on good public policy.
## 5                                                                Short-term psychotherapy may be an effective way to prevent repeated suicide attempts.
## 6                          What I hadnt considered before my fathers heart attack was the precise meaning of not wanting to live hooked up to machines.
##                                                                                                                                                Abstract
## 1                                                    The backstage tale won two awards; Citizenfour, the Edward Snowden documentary, was also a winner.
## 2                                                                      In the fall season finale, a question of where the series has many places to go.
## 3                                                                                                       In which Timothy Polin reveals his potty mouth.
## 4 The debate about pay for Wall Street executives who take government jobs appears to be based more on a populist shakedown than on good public policy.
## 5                                                                Short-term psychotherapy may be an effective way to prevent repeated suicide attempts.
## 6                          What I hadnt considered before my fathers heart attack was the precise meaning of not wanting to live hooked up to machines.
##   WordCount             PubDate UniqueID
## 1       111 2014-12-01 22:45:24     6533
## 2       558 2014-12-01 22:01:34     6534
## 3       788 2014-12-01 22:00:26     6535
## 4       915 2014-12-01 21:04:13     6536
## 5       213 2014-12-01 19:13:20     6537
## 6       938 2014-12-01 19:05:12     6538
##     NewsDesk      SectionName SubsectionName
## 3   Business Crosswords/Games               
## 334     OpEd          Opinion               
## 725   TStyle                                
## 732 Business     Business Day       Dealbook
## 752 Business     Business Day       Dealbook
## 864                                         
##                                                            Headline
## 3                                       Drinking Buddy For Falstaff
## 334 Facts & Figures: America&rsquo;s Unique Take on Maternity Leave
## 725                               Ansel Elgort Buttons Up in Brioni
## 732      A Shake-Up as the Financial World Infiltrates Philanthropy
## 752    Coupang, a South Korean E-Commerce Site, Raises $300 Million
## 864                                               Today in Politics
##                                                                                                                                                 Snippet
## 3                                                                                                       In which Timothy Polin reveals his potty mouth.
## 334                                                                                In the U.S., paid parental leave is more of a perk than a guarantee.
## 725                                                        The actor brought a tinge of youthfulness to the classic Italian houses retro-tailored look.
## 732 Donor-advised funds help investors get deductions for charitable donations in one year, but society doesnt get the benefit of the money right away.
## 752                                 The latest financing round underscores Coupangs maturity and its ambitions to one day be a publicly traded company.
## 864          The 113th Congress is concluding with partisan brinksmanship and one last mad scramble for votes to pass a $1.1 trillion spending package.
##                                                                                                                                                Abstract
## 3                                                                                                       In which Timothy Polin reveals his potty mouth.
## 334                                                                                In the U.S., paid parental leave is more of a perk than a guarantee.
## 725                                                        The actor brought a tinge of youthfulness to the classic Italian houses retro-tailored look.
## 732 Donor-advised funds help investors get deductions for charitable donations in one year, but society doesnt get the benefit of the money right away.
## 752                                 The latest financing round underscores Coupangs maturity and its ambitions to one day be a publicly traded company.
## 864          The 113th Congress is concluding with partisan brinksmanship and one last mad scramble for votes to pass a $1.1 trillion spending package.
##     WordCount             PubDate UniqueID
## 3         788 2014-12-01 22:00:26     6535
## 334       160 2014-12-04 11:45:20     6866
## 725        89 2014-12-10 12:30:47     7257
## 732      1172 2014-12-10 12:00:38     7264
## 752       353 2014-12-10 08:30:41     7284
## 864      1544 2014-12-11 07:09:25     7396
##      NewsDesk   SectionName SubsectionName
## 1865                                      
## 1866 Business    Technology               
## 1867    Metro N.Y. / Region               
## 1868             Multimedia               
## 1869  Foreign         World   Asia Pacific
## 1870  Science        Health               
##                                                       Headline
## 1865                                         Today in Politics
## 1866                         Uber Suspends Operations in Spain
## 1867                         New York Today: The Year in News 
## 1868                   New Year, Old Memories, in Times Square
## 1869 Hong Kong Police Criticized After 14-Year-Old's Detention
## 1870          The Super-Short Workout and Other Fitness Trends
##                                                                                                                                                                                                                                                   Snippet
## 1865                                                                                                               House Republicans are ending the year on a defensive note over Representative Steve Scalises 2002 speech to a white supremacist group.
## 1866                                                                              In a first in the growing pushback against Ubers global expansion, a judges ruling barred telecommunications operators and banks from supporting the companys services.
## 1867                                                                                                                                                              Wednesday: The most read stories of 2014, teeth-chattering cold, and its New Years Eve.
## 1868                                                                         What happens when you combine Burning Man, Independence Day fireworks, the last day of school and a full-contact Black Friday sale-a-bration? New Years Eve in Times Square.
## 1869 The authorities have been accused of trying to intimidate young pro-democracy protesters and their families after a 14-year-old girl was detained on suspicion of drawing flowers in chalk near government headquarters and sent to a juvenile home.
## 1870                                                                                                                 The big story in exercise science this year was the super-short workout, although many other fitness-related themes emerged in 2014.
##                                                                                                                                                                                                                                                  Abstract
## 1865                                                                                                               House Republicans are ending the year on a defensive note over Representative Steve Scalises 2002 speech to a white supremacist group.
## 1866                                                                              In a first in the growing pushback against Ubers global expansion, a judges ruling barred telecommunications operators and banks from supporting the companys services.
## 1867                                                                                                                                                              Wednesday: The most read stories of 2014, teeth-chattering cold, and its New Years Eve.
## 1868                                                                         What happens when you combine Burning Man, Independence Day fireworks, the last day of school and a full-contact Black Friday sale-a-bration? New Years Eve in Times Square.
## 1869 The authorities have been accused of trying to intimidate young pro-democracy protesters and their families after a 14-year-old girl was detained on suspicion of drawing flowers in chalk near government headquarters and sent to a juvenile home.
## 1870                                                                                                                 The big story in exercise science this year was the super-short workout, although many other fitness-related themes emerged in 2014.
##      WordCount             PubDate UniqueID
## 1865      1616 2014-12-31 07:03:46     8397
## 1866       292 2014-12-31 06:09:32     8398
## 1867      1010 2014-12-31 06:06:58     8399
## 1868       387 2014-12-31 05:00:19     8400
## 1869       717 2014-12-31 04:16:29     8401
## 1870       818 2014-12-31 00:01:10     8402
## 'data.frame':	1870 obs. of  9 variables:
##  $ NewsDesk      : chr  "Culture" "Culture" "Business" "Business" ...
##  $ SectionName   : chr  "" "Arts" "Crosswords/Games" "Business Day" ...
##  $ SubsectionName: chr  "" "" "" "Dealbook" ...
##  $ Headline      : chr  "'Birdman' Tops the Gothams" "'Sleepy Hollow' Recap: A Not-So-Shocking Death" "Drinking Buddy For Falstaff" "Encouraging Public Service, Through Wall Street's 'Revolving Door'" ...
##  $ Snippet       : chr  "The backstage tale won two awards; Citizenfour, the Edward Snowden documentary, was also a winner." "In the fall season finale, a question of where the series has many places to go." "In which Timothy Polin reveals his potty mouth." "The debate about pay for Wall Street executives who take government jobs appears to be based more on a populist shakedown than "| __truncated__ ...
##  $ Abstract      : chr  "The backstage tale won two awards; Citizenfour, the Edward Snowden documentary, was also a winner." "In the fall season finale, a question of where the series has many places to go." "In which Timothy Polin reveals his potty mouth." "The debate about pay for Wall Street executives who take government jobs appears to be based more on a populist shakedown than "| __truncated__ ...
##  $ WordCount     : int  111 558 788 915 213 938 1336 2644 752 99 ...
##  $ PubDate       : chr  "2014-12-01 22:45:24" "2014-12-01 22:01:34" "2014-12-01 22:00:26" "2014-12-01 21:04:13" ...
##  $ UniqueID      : int  6533 6534 6535 6536 6537 6538 6539 6540 6541 6542 ...
##  - attr(*, "comment")= chr "glb_newent_df"
## NULL
```

```r
if (nrow(glb_trnent_df) == nrow(glb_entity_df))
    warning("glb_trnent_df same as glb_entity_df")
if (nrow(glb_newent_df) == nrow(glb_entity_df))
    warning("glb_newent_df same as glb_entity_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_entity_df <- glb_entity_df[, setdiff(names(glb_entity_df), glb_drop_vars)]
    glb_trnent_df <- glb_trnent_df[, setdiff(names(glb_trnent_df), glb_drop_vars)]    
    glb_newent_df <- glb_newent_df[, setdiff(names(glb_newent_df), glb_drop_vars)]    
}

# Check for duplicates in glb_id_vars
if (length(glb_id_vars) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_entity_df$.rownames <- rownames(glb_entity_df)
    glb_id_vars <- ".rownames"
}
if (sum(duplicated(glb_entity_df[, glb_id_vars, FALSE])) > 0)
    stop(glb_id_vars, " duplicated in glb_entity_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_vars)

# Combine trnent & newent into glb_entity_df for easier manipulation
glb_trnent_df$.src <- "Train"; glb_newent_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_entity_df <- myrbind_df(glb_trnent_df, glb_newent_df)
comment(glb_entity_df) <- "glb_entity_df"
glb_trnent_df <- glb_newent_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 8.129 9.043   0.915
## 2 inspect.data          2          0 9.044    NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_entity_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

dsp_problem_data <- function(df) {
    print(sprintf("numeric data missing in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(is.na(df[, col]))))
    
    print(sprintf("numeric data w/ 0s in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(df[, col] == 0, na.rm=TRUE)))
    
    print(sprintf("numeric data w/ Infs in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(df[, col] == Inf, na.rm=TRUE)))
    
    print(sprintf("numeric data w/ NaNs in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(df[, col] == NaN, na.rm=TRUE)))
    
    print(sprintf("string data missing in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(myfind_chr_cols_df(df), ".src"), 
                        function(col) sum(df[, col] == "")))
}

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnent_df & glb_newent_df
    print(myplot_histogram(glb_entity_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_entity_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    dsp_problem_data(glb_entity_df)
}
glb_chk_data()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Loading required package: reshape2
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-1.png) 

```
##       Popular.0 Popular.1 Popular.NA
## Test         NA        NA       1870
## Train      5439      1093         NA
##       Popular.0 Popular.1 Popular.NA
## Test         NA        NA          1
## Train 0.8326699 0.1673301         NA
## [1] "numeric data missing in glb_entity_df: "
## WordCount   Popular  UniqueID 
##         0      1870         0 
## [1] "numeric data w/ 0s in glb_entity_df: "
## WordCount   Popular  UniqueID 
##       109      5439         0 
## [1] "numeric data w/ Infs in glb_entity_df: "
## WordCount   Popular  UniqueID 
##         0         0         0 
## [1] "numeric data w/ NaNs in glb_entity_df: "
## WordCount   Popular  UniqueID 
##         0         0         0 
## [1] "string data missing in glb_entity_df: "
##       NewsDesk    SectionName SubsectionName       Headline        Snippet 
##           2408           2899           6176              0             13 
##       Abstract        PubDate 
##             17              0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_entity_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_entity_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_entity_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: tcltk
```

```
##   Popular Popular.fctr   .n
## 1       0            N 5439
## 2      NA         <NA> 1870
## 3       1            Y 1093
```

```
## Warning: Removed 1 rows containing missing values (position_stack).
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-2.png) 

```
##       Popular.fctr.N Popular.fctr.Y Popular.fctr.NA
## Test              NA             NA            1870
## Train           5439           1093              NA
##       Popular.fctr.N Popular.fctr.Y Popular.fctr.NA
## Test              NA             NA               1
## Train      0.8326699      0.1673301              NA
```

```r
#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors
myextract_dates_df <- function(df, vars, rsp_var) {
    keep_feats <- c(NULL)
    for (var in vars) {
        dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df[, rsp_var] <- df[, rsp_var]
        dates_df[, paste0(var, ".POSIX")] <- dates_df$.date
        dates_df[, paste0(var, ".year")] <- as.numeric(format(dates_df$.date, "%Y"))
        dates_df[, paste0(var, ".year.fctr")] <- as.factor(format(dates_df$.date, "%Y")) 
        dates_df[, paste0(var, ".month")] <- as.numeric(format(dates_df$.date, "%m"))
        dates_df[, paste0(var, ".month.fctr")] <- as.factor(format(dates_df$.date, "%m"))
        dates_df[, paste0(var, ".date")] <- as.numeric(format(dates_df$.date, "%d"))
        dates_df[, paste0(var, ".date.fctr")] <- 
            cut(as.numeric(format(dates_df$.date, "%d")), 5) # by month week  
        
        # wkday Sun=0; Mon=1; ...; Sat=6
        dates_df[, paste0(var, ".wkday")] <- as.numeric(format(dates_df$.date, "%w"))
        dates_df[, paste0(var, ".wkday.fctr")] <- as.factor(format(dates_df$.date, "%w"))
        
        # Federal holidays 1.9., 13.10.,         27.11., 25.12.
        # NYState holidays 1.9., 13.10., 11.11., 27.11., 25.12.
        months <- dates_df[, paste0(var, ".month")]
        dates  <- dates_df[, paste0(var, ".date")]
        dates_df[, paste0(var, ".hlday")] <- 
            ifelse( ((months == 09) & (dates  == 01)) |
                    ((months == 10) & (dates  == 13)) |  
                    ((months == 11) & (dates  == 27)) |         
                    ((months == 12) & (dates  == 25)) ,                                 
                    1, 0)
        dates_df[, paste0(var, ".wkend")] <- as.numeric(
            (dates_df[, paste0(var, ".wkday")] %in% c(0, 6)) | 
            dates_df[, paste0(var, ".hlday")] )
        dates_df[, paste0(var, ".hour")] <- as.numeric(format(dates_df$.date, "%H"))
        dates_df[, paste0(var, ".hour.fctr")] <- 
            cut(as.numeric(format(dates_df$.date, "%H")), 3) # by work-shift    
        dates_df[, paste0(var, ".minute")] <- as.numeric(format(dates_df$.date, "%M")) 
        dates_df[, paste0(var, ".minute.fctr")] <- 
            cut(as.numeric(format(dates_df$.date, "%M")), 4) # by quarter-hours    
        dates_df[, paste0(var, ".second")] <- as.numeric(format(dates_df$.date, "%S")) 
        dates_df[, paste0(var, ".second.fctr")] <- 
            cut(as.numeric(format(dates_df$.date, "%S")), 4) # by quarter-hours    
        
        print(gp <- myplot_box(df=dates_df, ycol_names="PubDate.second", 
                               xcol_name=rsp_var))
        print(gp <- myplot_bar(df=dates_df, ycol_names="PubDate.second.fctr", 
                               xcol_name=rsp_var, colorcol_name="PubDate.second.fctr"))                
        keep_feats <- union(keep_feats, paste(var, 
            c(".POSIX", ".year.fctr", ".month.fctr", ".date.fctr", ".wkday.fctr", 
              ".wkend", ".hour.fctr", ".minute.fctr", ".second.fctr"), sep=""))        
    }
    #myprint_df(dates_df)
    return(dates_df[, keep_feats])
}

if (!is.null(glb_date_vars)) {
    glb_entity_df <- cbind(glb_entity_df, 
        myextract_dates_df(df=glb_entity_df, vars=glb_date_vars, rsp_var=glb_rsp_var))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          paste(glb_date_vars, c("", ".POSIX"), sep=""))
}
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-3.png) 

```
## Warning in mean.default(X[[1L]], ...): argument is not numeric or logical:
## returning NA
```

```
## Warning in mean.default(X[[2L]], ...): argument is not numeric or logical:
## returning NA
```

```
## Warning in mean.default(X[[1L]], ...): argument is not numeric or logical:
## returning NA
```

```
## Warning in mean.default(X[[2L]], ...): argument is not numeric or logical:
## returning NA
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-4.png) 

```r
srt_entity_df <- orderBy(~PubDate.POSIX, glb_entity_df)
print(myplot_scatter(subset(srt_entity_df, 
                            PubDate.POSIX < strptime("2014-09-02", "%Y-%m-%d")), 
                            xcol_name="PubDate.POSIX", ycol_name=glb_rsp_var,
                           colorcol_name=glb_rsp_var
                     ))
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-5.png) 

```r
# Create features that measure the gap between previous timestamp in the data
require(zoo)
```

```
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
pd = as.POSIXlt(srt_entity_df$PubDate)
z = zoo(as.numeric(pd))
srt_entity_df[, "PubDate.zoo"] <- z
print(head(srt_entity_df))
```

```
##    NewsDesk  SectionName SubsectionName
## 33  Science       Health               
## 32  Foreign        World   Asia Pacific
## 31            Multimedia               
## 30  Culture         Arts               
## 29 Business Business Day       Dealbook
## 28 Magazine     Magazine               
##                                                                      Headline
## 33                                           Don't Catch What Ails Your House
## 32                   Ukraine Conflict Has Been a Lift for China, Scholars Say
## 31                                        Revisiting Life and Death in Africa
## 30                                                  Fabio Luisi Has a New Gig
## 29                  Heineken to Sell Mexican Packaging Unit to Crown Holdings
## 28 Behind the Cover Story: Emily Bazelon on Medical Abortion Through the Mail
##                                                                                                                                                                                                                                                Snippet
## 33                                                                                            It doesnt take a flood to encourage the growth of mold in a home. A moist environment will do. A runny nose, coughing and all the rest typically follow.
## 32                                    As the United States and the European Union have imposed sanctions on Russia over the unrest in eastern Ukraine, China has been able to stand apart and gain concrete advantages, experts on foreign policy say.
## 31                                                                                                                                       Yunghi Kim went to Somalia 20 years ago expecting to cover a famine. She found herself instead in a war zone.
## 30                                                                               The music director of the Zurich Opera and principal conductor of the Metropolitan Opera will be named principal conductor of the Danish National Symphony Orchestra.
## 29                                                                                           The deal values the container unit Empaque at about $1.2 billion and would make Crown Holdings the second-largest beverage can producer in North America.
## 28 Emily Bazelon, a contributing writer for the magazine, wrote this weeks cover story about the online distribution of medical abortions. Here she discusses reporting on a group of activists working to provide medical abortions through the mail.
##                                                                                                                                                                                                                                               Abstract
## 33                                                                                            It doesnt take a flood to encourage the growth of mold in a home. A moist environment will do. A runny nose, coughing and all the rest typically follow.
## 32                                    As the United States and the European Union have imposed sanctions on Russia over the unrest in eastern Ukraine, China has been able to stand apart and gain concrete advantages, experts on foreign policy say.
## 31                                                                                                                                       Yunghi Kim went to Somalia 20 years ago expecting to cover a famine. She found herself instead in a war zone.
## 30                                                                               The music director of the Zurich Opera and principal conductor of the Metropolitan Opera will be named principal conductor of the Danish National Symphony Orchestra.
## 29                                                                                           The deal values the container unit Empaque at about $1.2 billion and would make Crown Holdings the second-largest beverage can producer in North America.
## 28 Emily Bazelon, a contributing writer for the magazine, wrote this weeks cover story about the online distribution of medical abortions. Here she discusses reporting on a group of activists working to provide medical abortions through the mail.
##    WordCount             PubDate Popular UniqueID  .src Popular.fctr
## 33       962 2014-09-01 00:01:32       1       33 Train            Y
## 32       529 2014-09-01 02:48:41       0       32 Train            N
## 31       832 2014-09-01 03:00:15       0       31 Train            N
## 30       166 2014-09-01 04:00:06       0       30 Train            N
## 29       442 2014-09-01 04:11:20       0       29 Train            N
## 28      1190 2014-09-01 05:00:26       0       28 Train            N
##          PubDate.POSIX PubDate.year.fctr PubDate.month.fctr
## 33 2014-09-01 00:01:32              2014                 09
## 32 2014-09-01 02:48:41              2014                 09
## 31 2014-09-01 03:00:15              2014                 09
## 30 2014-09-01 04:00:06              2014                 09
## 29 2014-09-01 04:11:20              2014                 09
## 28 2014-09-01 05:00:26              2014                 09
##    PubDate.date.fctr PubDate.wkday.fctr PubDate.wkend PubDate.hour.fctr
## 33          (0.97,7]                  1             1     (-0.023,7.67]
## 32          (0.97,7]                  1             1     (-0.023,7.67]
## 31          (0.97,7]                  1             1     (-0.023,7.67]
## 30          (0.97,7]                  1             1     (-0.023,7.67]
## 29          (0.97,7]                  1             1     (-0.023,7.67]
## 28          (0.97,7]                  1             1     (-0.023,7.67]
##    PubDate.minute.fctr PubDate.second.fctr PubDate.zoo
## 33       (-0.059,14.8]         (29.5,44.2]  1409544092
## 32         (44.2,59.1]         (29.5,44.2]  1409554121
## 31       (-0.059,14.8]         (14.8,29.5]  1409554815
## 30       (-0.059,14.8]       (-0.059,14.8]  1409558406
## 29       (-0.059,14.8]         (14.8,29.5]  1409559080
## 28       (-0.059,14.8]         (14.8,29.5]  1409562026
```

```r
print(myplot_scatter(subset(srt_entity_df, 
                            PubDate.POSIX < strptime("2014-09-02", "%Y-%m-%d")), 
                            xcol_name="PubDate.zoo", ycol_name=glb_rsp_var,
                           colorcol_name=glb_rsp_var
                     ))
```

```
## Don't know how to automatically pick scale for object of type zoo. Defaulting to continuous
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-6.png) 

```r
n = nrow(srt_entity_df)
b = zoo(, seq(n))

last1 = as.numeric(merge(z-lag(z, -1), b, all = TRUE))
srt_entity_df[, "PubDate.last1"] <- last1
srt_entity_df[is.na(srt_entity_df$PubDate.last1), "PubDate.last1"] <- 0
srt_entity_df[, "PubDate.last1.log"] <- log(1 + srt_entity_df[, "PubDate.last1"])
print(gp <- myplot_box(df=subset(srt_entity_df, PubDate.last1.log > 0), 
                       ycol_names="PubDate.last1.log", 
                       xcol_name=glb_rsp_var))
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-7.png) 

```r
last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
srt_entity_df[, "PubDate.last10"] <- last10
srt_entity_df[is.na(srt_entity_df$PubDate.last10), "PubDate.last10"] <- 0
srt_entity_df[, "PubDate.last10.log"] <- log(1 + srt_entity_df[, "PubDate.last10"])
print(gp <- myplot_box(df=subset(srt_entity_df, PubDate.last10.log > 0), 
                       ycol_names="PubDate.last10.log", 
                       xcol_name=glb_rsp_var))
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-8.png) 

```r
last100 = as.numeric(merge(z-lag(z, -100), b, all = TRUE))
srt_entity_df[, "PubDate.last100"] <- last100
srt_entity_df[is.na(srt_entity_df$PubDate.last100), "PubDate.last100"] <- 0
srt_entity_df[, "PubDate.last100.log"] <- log(1 + srt_entity_df[, "PubDate.last100"])
print(gp <- myplot_box(df=subset(srt_entity_df, PubDate.last100.log > 0), 
                       ycol_names="PubDate.last100.log", 
                       xcol_name=glb_rsp_var))
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-9.png) 

```r
sav_entity_df <- glb_entity_df
glb_entity_df <- srt_entity_df
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
    c("PubDate.zoo", "PubDate.last1", "PubDate.last10", "PubDate.last100"))
# all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
# all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
# all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
# all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
# all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
# 
# 
# # order table
# all2 = all2[order(all2$id),]
# 
# ## fill in NAs
# # count averages
# na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
#     last1=mean(last1, na.rm=TRUE),
#     last3=mean(last3, na.rm=TRUE),
#     last5=mean(last5, na.rm=TRUE),
#     last10=mean(last10, na.rm=TRUE),
#     last20=mean(last20, na.rm=TRUE),
#     last50=mean(last50, na.rm=TRUE)
# )
# 
# # fill in averages
# na.merge = merge(all2, na.avg, by=c("weekend","hour"))
# na.merge = na.merge[order(na.merge$id),]
# for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
#     y = paste0(i, ".y")
#     idx = is.na(all2[[i]])
#     all2[idx,][[i]] <- na.merge[idx,][[y]]
# }
# rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)

# check distribution of all numeric data
dsp_numeric_vars_dstrb <- function(vars_lst) {
    for (var in vars_lst) {
        print(sprintf("var: %s", var))
        gp <- myplot_box(df=glb_entity_df, ycol_names=var, xcol_name=glb_rsp_var)
        if (inherits(glb_entity_df[, var], "factor"))
            gp <- gp + facet_wrap(reformulate(var))
        print(gp)
    }    
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_entity_df), 
#                                 union(myfind_chr_cols_df(glb_entity_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_entity_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(1 + <col.name>),        
          WordCount.log = log(1 + WordCount),        
#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

        .rnorm=rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newent_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
# Add WordCount.log since WordCount is not distributed normally
glb_entity_df <- add_new_diag_feats(glb_entity_df)
```

```
## Loading required package: plyr
```

```r
print("Replacing WordCount with WordCount.log in potential feature set")
```

```
## [1] "Replacing WordCount with WordCount.log in potential feature set"
```

```r
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, "WordCount")

# Remove PubDate.year since all entity data is from 2014
# Remove PubDate.month.fctr since all newent data is from December
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("PubDate.year", "PubDate.month.fctr"))

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_vars_dstrb(setdiff(names(glb_entity_df), 
    union(myfind_chr_cols_df(glb_entity_df), 
        union(glb_rsp_var_raw, 
            union(glb_rsp_var, glb_exclude_vars_as_features)))))
```

```
## [1] "var: PubDate.year.fctr"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-10.png) 

```
## [1] "var: PubDate.date.fctr"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-11.png) 

```
## [1] "var: PubDate.wkday.fctr"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-12.png) 

```
## [1] "var: PubDate.wkend"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-13.png) 

```
## [1] "var: PubDate.hour.fctr"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-14.png) 

```
## [1] "var: PubDate.minute.fctr"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-15.png) 

```
## [1] "var: PubDate.second.fctr"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-16.png) 

```
## [1] "var: PubDate.last1.log"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-17.png) 

```
## [1] "var: PubDate.last10.log"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-18.png) 

```
## [1] "var: PubDate.last100.log"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-19.png) 

```
## [1] "var: WordCount.log"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-20.png) 

```
## [1] "var: .rnorm"
```

![](NYTBlogs_txtfeat_files/figure-html/inspect.data-21.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnent_df, select=-c(col_symbol)))
# Check for glb_newent_df & glb_trnent_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnent_df, <col1_name> == max(glb_trnent_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnent_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnent_df[which.max(glb_trnent_df$<col_name>),])

# print(<col_name>_freq_glb_trnent_df <- mycreate_tbl_df(glb_trnent_df, "<col_name>"))
# print(which.min(table(glb_trnent_df$<col_name>)))
# print(which.max(table(glb_trnent_df$<col_name>)))
# print(which.max(table(glb_trnent_df$<col1_name>, glb_trnent_df$<col2_name>)[, 2]))
# print(table(glb_trnent_df$<col1_name>, glb_trnent_df$<col2_name>))
# print(table(is.na(glb_trnent_df$<col1_name>), glb_trnent_df$<col2_name>))
# print(table(sign(glb_trnent_df$<col1_name>), glb_trnent_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnent_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnent_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnent_df <- 
#   mycreate_xtab_df(glb_trnent_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnent_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnent_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnent_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnent_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnent_df$<col1_name>, glb_trnent_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnent_df$<col1_name>.NA, glb_trnent_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnent_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnent_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnent_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_entity_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5))

rm(srt_entity_df)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cleanse.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0  9.044 27.361  18.317
## 3 cleanse.data          2          1 27.362     NA      NA
```

### Step `2.1: cleanse data`

```r
# Options:
#   1. Not fill missing vars
#   2. Fill missing numerics with a different algorithm
#   3. Fill missing chars with data based on clusters 

dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##           WordCount             Popular            UniqueID 
##                   0                1870                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                1870                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                   0 
##              .rnorm 
##                   0 
## [1] "numeric data w/ 0s in : "
##           WordCount             Popular            UniqueID 
##                 109                5439                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                 378 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                7624                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                  11 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                  11                  10                  10 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                 100                 100                 109 
##              .rnorm 
##                   0 
## [1] "numeric data w/ Infs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                   0 
##              .rnorm 
##                   0 
## [1] "numeric data w/ NaNs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                   0 
##              .rnorm 
##                   0 
## [1] "string data missing in : "
##       NewsDesk    SectionName SubsectionName       Headline        Snippet 
##           2408           2899           6176              0             13 
##       Abstract        PubDate 
##             17              0
```

```r
warning("Forcing ", nrow(subset(glb_entity_df, WordCount.log == 0)),
        " obs with WordCount.log 0s to NA")
```

```
## Warning: Forcing 109 obs with WordCount.log 0s to NA
```

```r
glb_entity_df[glb_entity_df$WordCount.log == 0, "WordCount.log"] <- NA

dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##           WordCount             Popular            UniqueID 
##                   0                1870                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                1870                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                 109 
##              .rnorm 
##                   0 
## [1] "numeric data w/ 0s in : "
##           WordCount             Popular            UniqueID 
##                 109                5439                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                 378 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                7624                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                  11 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                  11                  10                  10 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                 100                 100                   0 
##              .rnorm 
##                   0 
## [1] "numeric data w/ Infs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                   0 
##              .rnorm 
##                   0 
## [1] "numeric data w/ NaNs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                   0 
##              .rnorm 
##                   0 
## [1] "string data missing in : "
##       NewsDesk    SectionName SubsectionName       Headline        Snippet 
##           2408           2899           6176              0             13 
##       Abstract        PubDate 
##             17              0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_entity_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_entity_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_entity_df$SubsectionName))
}

sel_obs <- function(Popular=NULL, 
                    NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
        Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
        Headline.pfx=NULL, NewsDesk.nb=NULL) {
    tmp_entity_df <- glb_entity_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_entity_df <- tmp_entity_df[is.na(tmp_entity_df$Popular), ] else   
            tmp_entity_df <- tmp_entity_df[tmp_entity_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_entity_df <- tmp_entity_df[tmp_entity_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_entity_df <- tmp_entity_df[tmp_entity_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_entity_df <- tmp_entity_df[tmp_entity_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_entity_df <- 
            tmp_entity_df[grep(Headline.contains, tmp_entity_df$Headline), ]
    if (!is.null(Snippet.contains))
        tmp_entity_df <- 
            tmp_entity_df[grep(Snippet.contains, tmp_entity_df$Snippet), ]
    if (!is.null(Abstract.contains))
        tmp_entity_df <- 
            tmp_entity_df[grep(Abstract.contains, tmp_entity_df$Abstract), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_entity_df), fixed=TRUE, value=TRUE))
            > 0) tmp_entity_df <- 
                tmp_entity_df[tmp_entity_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_entity_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_entity_df), fixed=TRUE)) > 0) 
            tmp_entity_df <- 
                tmp_entity_df[tmp_entity_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_entity_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    
    return(glb_entity_df$UniqueID %in% tmp_entity_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_entity_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_entity_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_entity_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_entity_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
glb_entity_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
              "NewsDesk"] <- "Styles"
glb_entity_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
                      SubsectionName=""),
              "SubsectionName"] <- "Education"
glb_entity_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
              "SectionName"] <- "Business Day"
glb_entity_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
              "SubsectionName"] <- "Small Business"
glb_entity_df[sel_obs(Headline.contains="Readers Respond:"),
              "SectionName"] <- "Opinion"
glb_entity_df[sel_obs(Headline.contains="Readers Respond:"),
              "SubsectionName"] <- "Room For Debate"

# glb_entity_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_entity_df[glb_entity_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_entity_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_entity_df[glb_entity_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_entity_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_entity_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_entity_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_entity_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_entity_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_entity_df[glb_entity_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
glb_entity_df$myCategory <- paste(glb_entity_df$NewsDesk, 
                                  glb_entity_df$SectionName,
                                  glb_entity_df$SubsectionName,
                                  sep="#")

dsp_obs( Headline.contains="Music:"
        #,NewsDesk=""
        #,SectionName=""  
        #,SubsectionName="Fashion & Style"
        #,Popular=1 #NA
        ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
                "NewsDesk", "SectionName", "SubsectionName"),
        all=TRUE)
```

```
##      UniqueID Popular
## 305       305       0
## 844       844       1
## 1331     1331       0
## 1974     1974       0
## 2563     2563       0
## 3091     3091       0
## 3589     3589       0
## 4631     4631       0
## 5125     5125       0
## 5630     5630       0
## 6095     6095       0
## 6513     6513       1
## 6927     6927      NA
## 7473     7473      NA
## 7931     7931      NA
## 8217     8217      NA
##                                                       Headline
## 305              Friday Night Music: Lucius Covers John Lennon
## 844                         Friday Night Music: Cheryl Wheeler
## 1331            Friday Night Music: Cheryl Wheeler, Summer Fly
## 1974                                 Friday Night Music: Quilt
## 2563                   Friday Night Music: Lucius in Asheville
## 3091 Friday Night Music: Sarah Jarosz and the Milk Carton Kids
## 3589               Friday Night Music: Lucius Covers the Kinks
## 4631                                Friday Night Music: Amason
## 5125     Friday Night Music: Suzanne Vega, Jacob and the Angel
## 5630      Friday Night Music: Suzanne Vega, I Never Wear White
## 6095      Friday Night Music: Jessica Hernandez and the Deltas
## 6513                         Saturday Morning Music: Stay Gold
## 6927                      Friday Night Music: Lucius, Monsters
## 7473                   Friday Night Music: Peter Gabriel, 1993
## 7931         Friday Night Music: The Roches, Winter Wonderland
## 8217      Friday Night Music: Sarah Jarosz and Aoife O'Donovan
##         myCategory NewsDesk SectionName SubsectionName
## 305  OpEd#Opinion#     OpEd     Opinion               
## 844  OpEd#Opinion#     OpEd     Opinion               
## 1331 OpEd#Opinion#     OpEd     Opinion               
## 1974 OpEd#Opinion#     OpEd     Opinion               
## 2563 OpEd#Opinion#     OpEd     Opinion               
## 3091 OpEd#Opinion#     OpEd     Opinion               
## 3589 OpEd#Opinion#     OpEd     Opinion               
## 4631 OpEd#Opinion#     OpEd     Opinion               
## 5125 OpEd#Opinion#     OpEd     Opinion               
## 5630 OpEd#Opinion#     OpEd     Opinion               
## 6095 OpEd#Opinion#     OpEd     Opinion               
## 6513 OpEd#Opinion#     OpEd     Opinion               
## 6927 OpEd#Opinion#     OpEd     Opinion               
## 7473     #Opinion#              Opinion               
## 7931 OpEd#Opinion#     OpEd     Opinion               
## 8217 OpEd#Opinion#     OpEd     Opinion
```

```r
dsp_obs( Headline.contains="."
        ,NewsDesk=""
        ,SectionName="Opinion"  
        ,SubsectionName=""
        #,Popular=1 #NA
        ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
                "NewsDesk", "SectionName", "SubsectionName"),
        all=TRUE)
```

```
##      UniqueID Popular
## 516       516       0
## 918       918       0
## 7473     7473      NA
## 7445     7445      NA
## 7419     7419      NA
## 7505     7505      NA
## 7509     7509      NA
##                                                              Headline
## 516             This Is Life Among the Roma, Europes Forgotten People
## 918          What Might Happen If Iran Becomes America's Covert Ally?
## 7473                          Friday Night Music: Peter Gabriel, 1993
## 7445 Senate Committee Bothered to Authorize War Against Islamic State
## 7419                                       Joe on WNYCs Money Talking
## 7505           Rev. Dr. William Barber II on Todays Protest Movements
## 7509                          Did Salaita Cross the Line of Civility?
##      myCategory NewsDesk SectionName SubsectionName
## 516   #Opinion#              Opinion               
## 918   #Opinion#              Opinion               
## 7473  #Opinion#              Opinion               
## 7445  #Opinion#              Opinion               
## 7419  #Opinion#              Opinion               
## 7505  #Opinion#              Opinion               
## 7509  #Opinion#              Opinion
```

```r
# Merge some categories
glb_entity_df$myCategory <-
    plyr::revalue(glb_entity_df$myCategory, c(      
        "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
        "#Business Day#Small Business"      = "Business#Business Day#Small Business",
        "#Crosswords/Games#"                = "Business#Crosswords/Games#",
        "Business##"                        = "Business#Technology#",
        "#Open#"                            = "Business#Technology#",
        "#Technology#"                      = "Business#Technology#",
        
        "#Arts#"                            = "Culture#Arts#",        
        "Culture##"                         = "Culture#Arts#",        
        
        "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
        "Foreign##"                         = "Foreign#World#",    
        
        "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
        
        "#Opinion#"                         = "OpEd#Opinion#",                
        "OpEd##"                            = "OpEd#Opinion#",        

        "#Health#"                          = "Science#Health#",
        "Science##"                         = "Science#Health#",        
        
        "Styles##"                          = "Styles##Fashion",                        
        "Styles#Health#"                    = "Science#Health#",                
        "Styles#Style#Fashion & Style"      = "Styles##Fashion",        

        "#Travel#"                          = "Travel#Travel#",                
        
        "Magazine#Magazine#"                = "myOther",
        "National##"                        = "myOther",
        "National#U.S.#Politics"            = "myOther",        
        "Sports##"                          = "myOther",
        "Sports#Sports#"                    = "myOther",
        "#U.S.#"                            = "myOther",        
        

#         "Business##Small Business"        = "Business#Business Day#Small Business",        
#         
#         "#Opinion#"                       = "#Opinion#Room For Debate",        
        "##"                                = "##"
#         "Business##" = "Business#Business Day#Dealbook",
#         "Foreign#World#" = "Foreign##",
#         "#Open#" = "Other",
#         "#Opinion#The Public Editor" = "OpEd#Opinion#",
#         "Styles#Health#" = "Styles##",
#         "Styles#Style#Fashion & Style" = "Styles##",
#         "#U.S.#" = "#U.S.#Education",
    ))

ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                          mycreate_sqlxtab_df(glb_entity_df,
    c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                       myCategory + NewsDesk + SectionName + SubsectionName ~ 
                           Popular.fctr, sum, value.var=".n"))
myprint_df(ctgry_cast_df)
```

```
##                        myCategory NewsDesk      SectionName SubsectionName
## 33                  OpEd#Opinion#     OpEd          Opinion               
## 36                Science#Health#  Science           Health               
## 1                              ##                                         
## 11     Business#Crosswords/Games# Business Crosswords/Games               
## 40                   Styles#U.S.#   Styles             U.S.               
## 7  Business#Business Day#Dealbook Business     Business Day       Dealbook
##       N   Y  NA
## 33  113 407 141
## 36   73 119  55
## 1  1163 110 338
## 11   19 103  38
## 40   77 100  62
## 7   864  88 291
##                              myCategory NewsDesk  SectionName
## 35                      Science#Health#  Science             
## 17                        Culture#Arts#  Culture             
## 16                        Culture#Arts#                  Arts
## 8  Business#Business Day#Small Business          Business Day
## 13                 Business#Technology#            Technology
## 28                              myOther National         U.S.
##    SubsectionName N Y NA
## 35                0 2  2
## 17                1 0 70
## 16                0 0 11
## 8  Small Business 1 0  4
## 13                0 0  1
## 28       Politics 2 0  0
##         myCategory NewsDesk SectionName  SubsectionName N Y NA
## 27         myOther National                             2 0  0
## 28         myOther National        U.S.        Politics 2 0  0
## 29         myOther   Sports                             1 0  0
## 30         myOther   Sports      Sports                 1 0  0
## 37 Science#Health#   Styles      Health                 1 0  0
## 39 Styles##Fashion   Styles       Style Fashion & Style 2 0  0
```

```r
write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
            row.names=FALSE)

print(ctgry_sum_tbl <- table(glb_entity_df$myCategory, glb_entity_df[, glb_rsp_var], 
                             useNA="ifany"))
```

```
##                                       
##                                           N    Y <NA>
##   ##                                   1163  110  338
##   #Multimedia#                          139    2   52
##   #Opinion#Room For Debate               69    7   24
##   #Opinion#The Public Editor              4   16   10
##   #U.S.#Education                       325    0   90
##   Business#Business Day#Dealbook        864   88  304
##   Business#Business Day#Small Business  135    5   42
##   Business#Crosswords/Games#             20  103   42
##   Business#Technology#                  288   51  113
##   Culture#Arts#                         626   50  244
##   Foreign#World#                        172    0   47
##   Foreign#World#Asia Pacific            200    3   56
##   Metro#N.Y. / Region#                  181   17   67
##   myOther                                38    0    3
##   OpEd#Opinion#                         115  408  164
##   Science#Health#                        74  122   57
##   Styles##Fashion                       118    1   15
##   Styles#U.S.#                           77  100   62
##   Travel#Travel#                        116    1   35
##   TStyle##                              715    9  105
```

```r
dsp_chisq.test <- function(...) {
    sel_df <- glb_entity_df[sel_obs(...) & 
                            !is.na(glb_entity_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_entity_df[!is.na(glb_entity_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_vars, "Popular")],
                    sel_df[, c(glb_id_vars, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_entity_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_entity_df$NewsDesk, glb_entity_df$SectionName))
# print(table(glb_entity_df$SectionName, glb_entity_df$SubsectionName))
# print(table(glb_entity_df$NewsDesk, glb_entity_df$SectionName, glb_entity_df$SubsectionName))

glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
print(glb_entity_df[nchar(glb_entity_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
```

```
##                                                            Headline
## 2838            First Draft Focus: Off to Raise Money for Democrats
## 3728                      Verbatim: Obama as Supreme Court Justice?
## 4904                                   Election 2014: Live Coverage
## 4994                                   Election 2014: Live Coverage
## 5065                   First Draft Focus: Honoring a Civil War Hero
## 5029                        First Draft Focus: Perry's Day in Court
## 5160                 Supreme Court to Hear New Health Law Challenge
## 5254                                 Verbatim: Will Rick Perry Run?
## 5472                        First Draft Focus: A Red Carpet Welcome
## 7164 Does Torture Work? C.I.A.'s Claims vs. Senate Panel's Findings
## 7129                                 First Draft Focus: Pass a Bill
## 7368                              Verbatim: The People's Priorities
## 7364                              First Draft Focus: Three Wise Men
##      Snippet
## 2838        
## 3728        
## 4904        
## 4994        
## 5065        
## 5029        
## 5160        
## 5254        
## 5472        
## 7164        
## 7129        
## 7368        
## 7364
```

```r
print(glb_entity_df[glb_entity_df$Headline == glb_entity_df$Snippet, 
                    c("UniqueID", "Headline", "Snippet")])
```

```
## [1] UniqueID Headline Snippet 
## <0 rows> (or 0-length row.names)
```

```r
glb_entity_df[nchar(glb_entity_df[, "Snippet"]) == 0, "Snippet"] <- 
    glb_entity_df[nchar(glb_entity_df[, "Snippet"]) == 0, "Headline"]

print(glb_entity_df[nchar(glb_entity_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
```

```
##                                                            Headline
## 2838            First Draft Focus: Off to Raise Money for Democrats
## 3728                      Verbatim: Obama as Supreme Court Justice?
## 4904                                   Election 2014: Live Coverage
## 4994                                   Election 2014: Live Coverage
## 5065                   First Draft Focus: Honoring a Civil War Hero
## 5029                        First Draft Focus: Perry's Day in Court
## 5160                 Supreme Court to Hear New Health Law Challenge
## 5254                                 Verbatim: Will Rick Perry Run?
## 5472                        First Draft Focus: A Red Carpet Welcome
## 7164 Does Torture Work? C.I.A.'s Claims vs. Senate Panel's Findings
## 7129                                 First Draft Focus: Pass a Bill
## 7368                              Verbatim: The People's Priorities
## 7364                              First Draft Focus: Three Wise Men
## 7329                Obama Works the Phones to Get Funding Deal Done
## 7315              House Democrats Vent Frustration With White House
## 7310                   Funding Bill Hangs in Balance as House Votes
## 7309             Spending Bill Passes House With Democratic Support
##      Abstract
## 2838         
## 3728         
## 4904         
## 4994         
## 5065         
## 5029         
## 5160         
## 5254         
## 5472         
## 7164         
## 7129         
## 7368         
## 7364         
## 7329         
## 7315         
## 7310         
## 7309
```

```r
print(glb_entity_df[glb_entity_df$Headline == glb_entity_df$Abstract, 
                    c("UniqueID", "Headline", "Abstract")])
```

```
## [1] UniqueID Headline Abstract
## <0 rows> (or 0-length row.names)
```

```r
glb_entity_df[nchar(glb_entity_df[, "Abstract"]) == 0, "Abstract"] <- 
    glb_entity_df[nchar(glb_entity_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_entity_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])

glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 3        cleanse.data          2          1 27.362 31.544   4.182
## 4 manage.missing.data          2          2 31.544     NA      NA
```

### Step `2.2: manage missing data`

```r
# print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_trnent_df <- na.omit(glb_trnent_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##           WordCount             Popular            UniqueID 
##                   0                1870                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                1870                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                 109 
##              .rnorm 
##                   0 
## [1] "numeric data w/ 0s in : "
##           WordCount             Popular            UniqueID 
##                 109                5439                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                 378 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                7624                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                  11 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                  11                  10                  10 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                 100                 100                   0 
##              .rnorm 
##                   0 
## [1] "numeric data w/ Infs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                   0 
##              .rnorm 
##                   0 
## [1] "numeric data w/ NaNs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                   0 
##              .rnorm 
##                   0 
## [1] "string data missing in : "
##       NewsDesk    SectionName SubsectionName       Headline        Snippet 
##           2407           2883           6156              0              0 
##       Abstract        PubDate     myCategory 
##              0              0              0
```

```r
# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_entity_df[, setdiff(names(glb_entity_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    return(out_impent_df[, "WordCount.log"])
}

if (glb_impute_na_data) 
    glb_entity_df[, "WordCount.log"] <- glb_impute_missing_data()
```

```
## Loading required package: mice
## Loading required package: Rcpp
## Loading required package: lattice
## mice 2.22 2014-06-10
```

```
## [1] "Summary before imputation: "
##  PubDate.year.fctr PubDate.date.fctr PubDate.wkday.fctr PubDate.wkend   
##  2014:8402         (0.97,7]:1981     0: 378             Min.   :0.0000  
##                    (7,13]  :1757     1:1605             1st Qu.:0.0000  
##                    (13,19] :1808     2:1559             Median :0.0000  
##                    (19,25] :1650     3:1614             Mean   :0.0926  
##                    (25,31] :1206     4:1539             3rd Qu.:0.0000  
##                                      5:1470             Max.   :1.0000  
##                                      6: 237                             
##      PubDate.hour.fctr    PubDate.minute.fctr    PubDate.second.fctr
##  (-0.023,7.67]:1610    (-0.059,14.8]:3119     (-0.059,14.8]:2134    
##  (7.67,15.3]  :4484    (14.8,29.5]  :1671     (14.8,29.5]  :2063    
##  (15.3,23]    :2308    (29.5,44.2]  :1995     (29.5,44.2]  :2112    
##                        (44.2,59.1]  :1617     (44.2,59.1]  :2093    
##                                                                     
##                                                                     
##                                                                     
##  PubDate.last1.log PubDate.last10.log PubDate.last100.log WordCount.log   
##  Min.   : 0.000    Min.   : 0.000     Min.   : 0.00       Min.   :0.6932  
##  1st Qu.: 5.263    1st Qu.: 8.516     1st Qu.:11.37       1st Qu.:5.2679  
##  Median : 6.292    Median : 8.868     Median :11.43       Median :5.9480  
##  Mean   : 6.094    Mean   : 9.048     Mean   :11.49       Mean   :5.8263  
##  3rd Qu.: 7.126    3rd Qu.: 9.424     3rd Qu.:11.78       3rd Qu.:6.6067  
##  Max.   :10.875    Max.   :11.744     Max.   :12.95       Max.   :9.2977  
##                                                           NA's   :109     
##      .rnorm           myCategory       
##  Min.   :-3.281785   Length:8402       
##  1st Qu.:-0.681275   Class :character  
##  Median : 0.007735   Mode  :character  
##  Mean   :-0.000264                     
##  3rd Qu.: 0.673409                     
##  Max.   : 3.987726                     
##                                        
## 
##  iter imp variable
##   1   1  WordCount.log
##   1   2  WordCount.log
##   1   3  WordCount.log
##   1   4  WordCount.log
##   1   5  WordCount.log
##   2   1  WordCount.log
##   2   2  WordCount.log
##   2   3  WordCount.log
##   2   4  WordCount.log
##   2   5  WordCount.log
##   3   1  WordCount.log
##   3   2  WordCount.log
##   3   3  WordCount.log
##   3   4  WordCount.log
##   3   5  WordCount.log
##   4   1  WordCount.log
##   4   2  WordCount.log
##   4   3  WordCount.log
##   4   4  WordCount.log
##   4   5  WordCount.log
##   5   1  WordCount.log
##   5   2  WordCount.log
##   5   3  WordCount.log
##   5   4  WordCount.log
##   5   5  WordCount.log
##  PubDate.year.fctr PubDate.date.fctr PubDate.wkday.fctr PubDate.wkend   
##  2014:8402         (0.97,7]:1981     0: 378             Min.   :0.0000  
##                    (7,13]  :1757     1:1605             1st Qu.:0.0000  
##                    (13,19] :1808     2:1559             Median :0.0000  
##                    (19,25] :1650     3:1614             Mean   :0.0926  
##                    (25,31] :1206     4:1539             3rd Qu.:0.0000  
##                                      5:1470             Max.   :1.0000  
##                                      6: 237                             
##      PubDate.hour.fctr    PubDate.minute.fctr    PubDate.second.fctr
##  (-0.023,7.67]:1610    (-0.059,14.8]:3119     (-0.059,14.8]:2134    
##  (7.67,15.3]  :4484    (14.8,29.5]  :1671     (14.8,29.5]  :2063    
##  (15.3,23]    :2308    (29.5,44.2]  :1995     (29.5,44.2]  :2112    
##                        (44.2,59.1]  :1617     (44.2,59.1]  :2093    
##                                                                     
##                                                                     
##                                                                     
##  PubDate.last1.log PubDate.last10.log PubDate.last100.log WordCount.log   
##  Min.   : 0.000    Min.   : 0.000     Min.   : 0.00       Min.   :0.6931  
##  1st Qu.: 5.263    1st Qu.: 8.516     1st Qu.:11.37       1st Qu.:5.2730  
##  Median : 6.292    Median : 8.868     Median :11.43       Median :5.9480  
##  Mean   : 6.094    Mean   : 9.048     Mean   :11.49       Mean   :5.8267  
##  3rd Qu.: 7.126    3rd Qu.: 9.424     3rd Qu.:11.78       3rd Qu.:6.6067  
##  Max.   :10.875    Max.   :11.744     Max.   :12.95       Max.   :9.2977  
##                                                                           
##      .rnorm           myCategory       
##  Min.   :-3.281785   Length:8402       
##  1st Qu.:-0.681275   Class :character  
##  Median : 0.007735   Mode  :character  
##  Mean   :-0.000264                     
##  3rd Qu.: 0.673409                     
##  Max.   : 3.987726                     
## 
```

```r
dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##           WordCount             Popular            UniqueID 
##                   0                1870                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                1870                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                   0 
##              .rnorm 
##                   0 
## [1] "numeric data w/ 0s in : "
##           WordCount             Popular            UniqueID 
##                 109                5439                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                 378 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                7624                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                  11 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                  11                  10                  10 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                 100                 100                   0 
##              .rnorm 
##                   0 
## [1] "numeric data w/ Infs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                   0 
##              .rnorm 
##                   0 
## [1] "numeric data w/ NaNs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr       PubDate.POSIX   PubDate.year.fctr 
##                   0                   0                   0 
##  PubDate.month.fctr   PubDate.date.fctr  PubDate.wkday.fctr 
##                   0                   0                   0 
##       PubDate.wkend   PubDate.hour.fctr PubDate.minute.fctr 
##                   0                   0                   0 
## PubDate.second.fctr         PubDate.zoo       PubDate.last1 
##                   0                   0                   0 
##   PubDate.last1.log      PubDate.last10  PubDate.last10.log 
##                   0                   0                   0 
##     PubDate.last100 PubDate.last100.log       WordCount.log 
##                   0                   0                   0 
##              .rnorm 
##                   0 
## [1] "string data missing in : "
##       NewsDesk    SectionName SubsectionName       Headline        Snippet 
##           2407           2883           6156              0              0 
##       Abstract        PubDate     myCategory 
##              0              0              0
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "encode.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 4 manage.missing.data          2          2 31.544 36.532   4.989
## 5         encode.data          2          3 36.533     NA      NA
```

### Step `2.3: encode data`

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_trnent_df <- mymap_codes(glb_trnent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newent_df <- mymap_codes(glb_newent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_trnent_df$<col_name>.fctr <- factor(glb_trnent_df$<col_name>, 
#                     as.factor(union(glb_trnent_df$<col_name>, glb_newent_df$<col_name>)))
# glb_newent_df$<col_name>.fctr <- factor(glb_newent_df$<col_name>, 
#                     as.factor(union(glb_trnent_df$<col_name>, glb_newent_df$<col_name>)))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn   end elapsed
## 5      encode.data          2          3 36.533 36.59   0.057
## 6 extract.features          3          0 36.591    NA      NA
```

## Step `3.0: extract features`

```r
#```{r extract_features, cache=FALSE, eval=glb_is_textual}
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnent_df$<col_name>), -2, na.pad=TRUE)
# glb_trnent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newent_df$<col_name>), -2, na.pad=TRUE)
# glb_newent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newent_df[1, "<col_name>.lag.2"] <- glb_trnent_df[nrow(glb_trnent_df) - 1, 
#                                                    "<col_name>"]
# glb_newent_df[2, "<col_name>.lag.2"] <- glb_trnent_df[nrow(glb_trnent_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     A.has.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnent_df <- mutate(glb_trnent_df,
#                     )
# 
# glb_newent_df <- mutate(glb_newent_df,
#                     )

#   Create factors of string variables
print(str_vars <- myfind_chr_cols_df(glb_entity_df))
```

```
##         NewsDesk      SectionName   SubsectionName         Headline 
##       "NewsDesk"    "SectionName" "SubsectionName"       "Headline" 
##          Snippet         Abstract          PubDate             .src 
##        "Snippet"       "Abstract"        "PubDate"           ".src" 
##       myCategory 
##     "myCategory"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               glb_exclude_vars_as_features)) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_entity_df[, var])))
        glb_entity_df[, paste0(var, ".fctr")] <- factor(glb_entity_df[, var], 
                        as.factor(unique(glb_entity_df[, var])))
#         glb_trnent_df[, paste0(var, ".fctr")] <- factor(glb_trnent_df[, var], 
#                         as.factor(unique(glb_entity_df[, var])))
#         glb_newent_df[, paste0(var, ".fctr")] <- factor(glb_newent_df[, var], 
#                         as.factor(unique(glb_entity_df[, var])))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}
```

```
## Warning: Creating factors of string variable: myCategory: # of unique
## values: 20
```

```r
if (glb_is_textual) {
    require(tm)
    
    glb_corpus_lst <- list(); glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Building corpus for %s...", txt_var))
        
        # Combine "new york" to "newyork"
        #   shd be created as a tm_map::content_transformer
        txt_df <- glb_entity_df[, txt_var]
        txt_df <- gsub("[Nn]ew [Dd]elhi",   "newdelhi",   txt_df)                        
        txt_df <- gsub("[Nn]ew [Gg]uinea",  "newguinea",   txt_df)                                
        txt_df <- gsub("[Nn]ew [Jj]ersey",  "newjersey",  txt_df)                
        txt_df <- gsub("[Nn]ew [Oo]rleans", "neworleans",  txt_df)                
        txt_df <- gsub("[Nn]ew [Yy]ear",    "newyear",    txt_df)        
        txt_df <- gsub("[Nn]ew [Yy]ork",    "newyork",    txt_df)
        txt_df <- gsub("[Nn]ew [Zz]ealand", "newzealand", txt_df)
        
        if (txt_var == "Headline") {
#             dsp_chisq.test(Headline.contains="[Nn]ew ")
#             print(head(txt_df[grep("[Nn]ew ", txt_df)]))
#             print(tail(txt_df[grep("[Nn]ew ", txt_df)]))
#             print(sample(txt_df[grep("[Nn]ew ", txt_df)], 5))
#             print(length(txt_df[grep("[Nn]ew ", txt_df)]))
#             print(txt_df[grep("[Nn]ew ", txt_df)][01:20])
#             print(txt_df[grep("[Nn]ew ", txt_df)][21:40])        
#             print(txt_df[grep("[Nn]ew ", txt_df)][41:60])                
#             print(txt_df[grep("[Nn]ew ", txt_df)][61:80])                       
#             print(txt_df[grep("[Nn]ew ", txt_df)][81:100])                               
#             #print(length(txt_df[grep("[Nn]ew [Zz]ealand", txt_df)]))
            
#             dsp_chisq.test(Headline.contains="[Nn]ew [Yy]ork")
#             dsp_chisq.test(Headline.contains="[Re]eport")
#             dsp_chisq.test(Snippet.contains="[Re]eport")
#             
#             dsp_chisq.test(Headline.contains="[Ww]eek")
#             dsp_chisq.test(Headline.contains="[Dd]ay")
#             dsp_chisq.test(Headline.contains="[Ff]ashion")
#             dsp_chisq.test(Headline.contains="[Tt]oday")
#             dsp_chisq.test(Headline.contains="[Dd]ail")
#             dsp_chisq.test(Headline.contains="2014")
#             dsp_chisq.test(Headline.contains="2015")            
            glb_append_stop_words[["Headline"]] <- c(NULL)
        }

        if (txt_var == "Snippet") {
#             dsp_chisq.test(Snippet.contains="[Nn]ew ")
#             print(head(txt_df[grep("[Nn]ew ", txt_df)]))
#             print(tail(txt_df[grep("[Nn]ew ", txt_df)]))
#             print(sample(txt_df[grep("[Nn]ew ", txt_df)], 5))
#             print(length(txt_df[grep("[Nn]ew ", txt_df)]))
#             print(txt_df[grep("[Nn]ew ", txt_df)][11:20])
#             print(txt_df[grep("[Nn]ew ", txt_df)][21:30])        
#             print(txt_df[grep("[Nn]ew ", txt_df)][31:40])                
#             print(txt_df[grep("[Nn]ew ", txt_df)][41:50])                       
#             print(txt_df[grep("[Nn]ew ", txt_df)][51:60])                               
#             #print(length(txt_df[grep("[Nn]ew [Zz]ealand", txt_df)]))
    
#             dsp_chisq.test(Snippet.contains="[Ww]ill")
#             dsp_chisq.test(Snippet.contains="[Tt]ime")
#             dsp_chisq.test(Snippet.contains="[Ww]eek")
#             dsp_chisq.test(Snippet.contains="[Yy]ear")        
#             dsp_chisq.test(Snippet.contains="[Ne]w [Yy]ork")
#             dsp_chisq.test(Snippet.contains="[Cc]ompan")
#             dsp_chisq.test(Snippet.contains="[Oo]ne")
#             dsp_chisq.test(Snippet.contains="[Rr]eport")
#             dsp_chisq.test(Snippet.contains="[Pp]resid")
#             dsp_chisq.test(Snippet.contains="[Ss]aid")
#             dsp_chisq.test(Snippet.contains="[Cc]an")
#             dsp_chisq.test(Snippet.contains="[Dd]ay")
            
            glb_append_stop_words[["Snippet"]] <- c(NULL)
                #c("can")
        }

        if (txt_var == "Abstract") {
#             dsp_chisq.test(Abstract.contains="[Nn]ew ")
#             print(head(txt_df[grep("[Nn]ew ", txt_df)]))
#             print(tail(txt_df[grep("[Nn]ew ", txt_df)]))
#             print(sample(txt_df[grep("[Nn]ew ", txt_df)], 5))
#             print(length(txt_df[grep("[Nn]ew ", txt_df)]))
#             print(txt_df[grep("[Nn]ew ", txt_df)][11:20])
#             print(txt_df[grep("[Nn]ew ", txt_df)][21:30])        
#             print(txt_df[grep("[Nn]ew ", txt_df)][31:40])                
#             print(txt_df[grep("[Nn]ew ", txt_df)][41:50])                       
#             print(txt_df[grep("[Nn]ew ", txt_df)][51:60])                               
#             #print(length(txt_df[grep("[Nn]ew [Zz]ealand", txt_df)]))
#     
#             dsp_chisq.test(Abstract.contains="[Ww]ill")
#             dsp_chisq.test(Abstract.contains="[Tt]ime")
#             dsp_chisq.test(Abstract.contains="[Ww]eek")
#             dsp_chisq.test(Abstract.contains="[Yy]ear")        
#             dsp_chisq.test(Abstract.contains="[Ne]w [Yy]ork")
#             dsp_chisq.test(Abstract.contains="[Cc]ompan")
#             dsp_chisq.test(Abstract.contains="[Oo]ne")
#             dsp_chisq.test(Abstract.contains="[Rr]eport")
#             dsp_chisq.test(Abstract.contains="[Pp]resid")
#             
#             dsp_chisq.test(Abstract.contains="[Ss]aid")
#             dsp_chisq.test(Abstract.contains="[Cc]an")
#             dsp_chisq.test(Abstract.contains="[Dd]ay")
#             dsp_chisq.test(Abstract.contains="[Ss]tate")            
#             dsp_chisq.test(Abstract.contains="[Mm]ake")            
#             dsp_chisq.test(Abstract.contains="[Bb]ank")                        
            
            glb_append_stop_words[["Abstract"]] <- c(NULL)
                #c("fashion", "first", "intern", "make", "newyork", "report", 
                #  "said", "share", "show", "state", "week", "year")
        }

        txt_corpus <- Corpus(VectorSource(txt_df))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation)
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))        
        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english")))
        txt_corpus <- tm_map(txt_corpus, stemDocument)
        
        full_freqs_DTM <- DocumentTermMatrix(txt_corpus)
        print("   Full freqs:"); print(full_freqs_DTM)
        full_freqs_vctr <- colSums(as.matrix(full_freqs_DTM))
        names(full_freqs_vctr) <- dimnames(full_freqs_DTM)[[2]]
        full_freqs_df <- as.data.frame(full_freqs_vctr)
        names(full_freqs_df) <- "freq.full"
        full_freqs_df$term <- rownames(full_freqs_df)
        full_freqs_df <- orderBy(~ -freq.full, full_freqs_df)
        
        sprs_freqs_DTM <- removeSparseTerms(full_freqs_DTM, 
                                            glb_sprs_thresholds[txt_var])
        print("   Sparse freqs:"); print(sprs_freqs_DTM)
        sprs_freqs_vctr <- colSums(as.matrix(sprs_freqs_DTM))
        names(sprs_freqs_vctr) <- dimnames(sprs_freqs_DTM)[[2]]
        sprs_freqs_df <- as.data.frame(sprs_freqs_vctr)
        names(sprs_freqs_df) <- "freq.sprs"
        sprs_freqs_df$term <- rownames(sprs_freqs_df)
        sprs_freqs_df <- orderBy(~ -freq.sprs, sprs_freqs_df)
        
        terms_freqs_df <- merge(full_freqs_df, sprs_freqs_df, all.x=TRUE)
        melt_freqs_df <- orderBy(~ -value, melt(terms_freqs_df, id.var="term"))
        print(ggplot(melt_freqs_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        melt_freqs_df <- orderBy(~ -value, 
                        melt(subset(terms_freqs_df, !is.na(freq.sprs)), id.var="term"))
        print(myplot_hbar(melt_freqs_df, "term", "value", 
                          colorcol_name="variable"))
        melt_freqs_df <- orderBy(~ -value, 
                        melt(subset(terms_freqs_df, is.na(freq.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_freqs_df, 10), "term", "value", 
                          colorcol_name="variable"))
        
        glb_corpus_lst[[txt_var]] <- txt_corpus
        glb_full_DTM_lst[[txt_var]] <- full_freqs_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_freqs_DTM
    }

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_entity_df) # warning otherwise
        log_X_df <- log(1 + txt_X_df)
        colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
        #glb_entity_df <- cbind(glb_entity_df, txt_X_df)
        glb_entity_df <- cbind(glb_entity_df, log_X_df)        
        
        # Create <txt_var>.has.http
        glb_entity_df[, paste(txt_var_pfx, ".has.http", sep="")] <- 
            sapply(1:nrow(glb_entity_df), 
    function(row_ix) ifelse(grepl("http", glb_entity_df[row_ix, txt_var], fixed=TRUE), 
                            1, 0))
    
        # Create user-specified term vectors 
        #   UniqueID == 4020, H.has.ebola
#         dsp_chisq.test(Headline.contains="[Ee]bola")                            
#         dsp_chisq.test( Snippet.contains="[Ee]bola")
#         dsp_chisq.test(Abstract.contains="[Ee]bola")
        if (txt_var == "Headline") {
        glb_entity_df[, paste(txt_var_pfx, ".has.ebola", sep="")] <- 
            sapply(1:nrow(glb_entity_df), 
    function(row_ix) ifelse(grepl("[Ee]bola", glb_entity_df[row_ix, txt_var]), 
                            1, 0))            
        }
    
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        glb_entity_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + rowSums(as.matrix(glb_full_DTM_lst[[txt_var]])))
        glb_entity_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(as.matrix(glb_full_DTM_lst[[txt_var]]) != 0))

        # Create <txt_var>.nchrs.log
        glb_entity_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_entity_df[, txt_var]))
        glb_entity_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_entity_df[, txt_var]))
        glb_entity_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_entity_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", "<", "=", 
                        ">", "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
        for (punct_ix in 1:length(punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s char:", punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL
#             print(results)
            glb_entity_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(punct_vctr[punct_ix], 
                                            glb_entity_df[, txt_var]))
        }
#         print(head(glb_entity_df[glb_entity_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    

        # Create <txt_var>.has.year.colon
#         mycount_pattern_occ("[0-9]{4}:", glb_entity_df$Headline[13:19])
        glb_entity_df[, paste0(txt_var_pfx, ".has.year.colon")] <-
            as.integer(0 + mycount_pattern_occ("[0-9]{4}:", glb_entity_df[, txt_var]))

#         for (feat in paste(txt_var_pfx, 
#                             c(".num.chars"), sep="")) {
#             #print(myplot_box(glb_entity_df, paste0(feat, ".log"), glb_rsp_var))
#         }            
    }        

    # Generate summaries
#     print(summary(glb_entity_df))
#     print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
#     print(summary(glb_trnent_df))
#     print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
#     print(summary(glb_newent_df))
#     print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

    rm(full_freqs_df, melt_freqs_df, terms_freqs_df, log_X_df, txt_X_df)
}
```

```
## Loading required package: tm
## Loading required package: NLP
## 
## Attaching package: 'NLP'
## 
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

```
## [1] "Building corpus for Headline..."
## [1] "   Full freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 9205)>>
## Non-/sparse entries: 44361/77296049
## Sparsity           : 100%
## Maximal term length: 31
## Weighting          : term frequency (tf)
## [1] "   Sparse freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 10)>>
## Non-/sparse entries: 2407/81613
## Sparsity           : 97%
## Maximal term length: 7
## Weighting          : term frequency (tf)
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTBlogs_txtfeat_files/figure-html/extract.features-1.png) ![](NYTBlogs_txtfeat_files/figure-html/extract.features-2.png) ![](NYTBlogs_txtfeat_files/figure-html/extract.features-3.png) 

```
## [1] "Building corpus for Snippet..."
## [1] "   Full freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 13822)>>
## Non-/sparse entries: 105519/116026925
## Sparsity           : 100%
## Maximal term length: 25
## Weighting          : term frequency (tf)
## [1] "   Sparse freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 22)>>
## Non-/sparse entries: 8657/176187
## Sparsity           : 95%
## Maximal term length: 7
## Weighting          : term frequency (tf)
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTBlogs_txtfeat_files/figure-html/extract.features-4.png) ![](NYTBlogs_txtfeat_files/figure-html/extract.features-5.png) ![](NYTBlogs_txtfeat_files/figure-html/extract.features-6.png) 

```
## [1] "Building corpus for Abstract..."
## [1] "   Full freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 13866)>>
## Non-/sparse entries: 105900/116396232
## Sparsity           : 100%
## Maximal term length: 112
## Weighting          : term frequency (tf)
## [1] "   Sparse freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 22)>>
## Non-/sparse entries: 8672/176172
## Sparsity           : 95%
## Maximal term length: 7
## Weighting          : term frequency (tf)
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTBlogs_txtfeat_files/figure-html/extract.features-7.png) ![](NYTBlogs_txtfeat_files/figure-html/extract.features-8.png) ![](NYTBlogs_txtfeat_files/figure-html/extract.features-9.png) 

```
## [1] "Binding DTM for Headline..."
## [1] "Binding DTM for Snippet..."
## [1] "Binding DTM for Abstract..."
```

```r
# Re-partition
glb_trnent_df <- subset(glb_entity_df, .src == "Train")
glb_newent_df <- subset(glb_entity_df, .src == "Test")

# print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

# print(myplot_scatter(glb_trnent_df, "<col1_name>", "<col2_name>", smooth=TRUE))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](NYTBlogs_txtfeat_files/figure-html/extract.features-10.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 6 extract.features          3          0  36.591 147.378 110.787
## 7  select.features          4          0 147.378      NA      NA
```

## Step `4.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnent_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
## Warning in cor(data.matrix(entity_df[, sel_feats]), y =
## as.numeric(entity_df[, : the standard deviation is zero
```

```
##                                      id         cor.y exclude.as.feat
## Popular                         Popular  1.000000e+00               1
## A.nuppr.log                 A.nuppr.log -2.720962e-01               0
## S.nuppr.log                 S.nuppr.log -2.718459e-01               0
## WordCount.log             WordCount.log  2.656836e-01               0
## WordCount                     WordCount  2.575265e-01               1
## S.nwrds.unq.log         S.nwrds.unq.log -2.507969e-01               0
## A.nwrds.unq.log         A.nwrds.unq.log -2.506012e-01               0
## S.nwrds.log                 S.nwrds.log -2.453541e-01               0
## A.nwrds.log                 A.nwrds.log -2.450733e-01               0
## S.nchrs.log                 S.nchrs.log -2.246930e-01               0
## A.nchrs.log                 A.nchrs.log -2.245488e-01               0
## H.nwrds.unq.log         H.nwrds.unq.log -2.044964e-01               0
## H.nwrds.log                 H.nwrds.log -2.006864e-01               0
## H.nchrs.log                 H.nchrs.log -1.710624e-01               0
## PubDate.hour.fctr     PubDate.hour.fctr  1.354368e-01               0
## H.npnct21.log             H.npnct21.log  1.283641e-01               0
## H.nuppr.log                 H.nuppr.log -1.278085e-01               0
## A.ndgts.log                 A.ndgts.log -1.249484e-01               0
## S.ndgts.log                 S.ndgts.log -1.242046e-01               0
## H.ndgts.log                 H.ndgts.log -1.196633e-01               0
## PubDate.wkend             PubDate.wkend  1.067288e-01               0
## A.npnct12.log             A.npnct12.log -9.183870e-02               0
## S.npnct12.log             S.npnct12.log -9.158156e-02               0
## H.npnct30.log             H.npnct30.log -8.917338e-02               0
## S.week.log                   S.week.log -8.840293e-02               0
## A.week.log                   A.week.log -8.840293e-02               0
## S.fashion.log             S.fashion.log -8.724932e-02               0
## A.fashion.log             A.fashion.log -8.724932e-02               0
## H.npnct16.log             H.npnct16.log -8.273237e-02               0
## H.fashion.log             H.fashion.log -8.204998e-02               0
## H.has.year.colon       H.has.year.colon -7.842875e-02               0
## H.week.log                   H.week.log -7.510522e-02               0
## H.daili.log                 H.daili.log -6.919298e-02               0
## A.npnct16.log             A.npnct16.log -6.893301e-02               0
## S.intern.log               S.intern.log -6.864274e-02               0
## A.intern.log               A.intern.log -6.864274e-02               0
## S.npnct16.log             S.npnct16.log -6.770952e-02               0
## H.X2015.log                 H.X2015.log -6.658489e-02               0
## H.report.log               H.report.log -6.494810e-02               0
## H.today.log                 H.today.log -6.372306e-02               0
## S.npnct04.log             S.npnct04.log -6.294642e-02               0
## A.npnct04.log             A.npnct04.log -6.294642e-02               0
## H.day.log                     H.day.log -6.272898e-02               0
## S.newyork.log             S.newyork.log -6.219997e-02               0
## A.newyork.log             A.newyork.log -6.219997e-02               0
## H.npnct15.log             H.npnct15.log -6.158577e-02               0
## A.will.log                   A.will.log -6.147068e-02               0
## S.will.log                   S.will.log -6.103349e-02               0
## S.articl.log               S.articl.log -5.952055e-02               0
## A.articl.log               A.articl.log -5.952055e-02               0
## H.newyork.log             H.newyork.log -5.797009e-02               0
## A.time.log                   A.time.log -5.779371e-02               0
## S.time.log                   S.time.log -5.759227e-02               0
## S.npnct21.log             S.npnct21.log  5.503894e-02               0
## A.npnct21.log             A.npnct21.log  5.482747e-02               0
## PubDate.last10           PubDate.last10  5.398093e-02               1
## H.npnct08.log             H.npnct08.log  5.375262e-02               0
## H.npnct09.log             H.npnct09.log  5.375262e-02               0
## S.first.log                 S.first.log -5.345938e-02               0
## A.first.log                 A.first.log -5.345938e-02               0
## S.npnct14.log             S.npnct14.log -5.332519e-02               0
## H.new.log                     H.new.log -5.313316e-02               0
## A.compani.log             A.compani.log -5.268413e-02               0
## S.compani.log             S.compani.log -5.261812e-02               0
## S.share.log                 S.share.log -5.138139e-02               0
## A.share.log                 A.share.log -5.138139e-02               0
## H.npnct04.log             H.npnct04.log -5.126277e-02               0
## S.year.log                   S.year.log -5.094457e-02               0
## A.year.log                   A.year.log -5.094457e-02               0
## S.report.log               S.report.log -5.032801e-02               0
## A.report.log               A.report.log -5.032801e-02               0
## A.npnct14.log             A.npnct14.log -4.999563e-02               0
## PubDate.last10.log   PubDate.last10.log  4.931702e-02               0
## S.show.log                   S.show.log -4.897915e-02               0
## A.show.log                   A.show.log -4.897915e-02               0
## PubDate.last1.log     PubDate.last1.log  4.635751e-02               0
## H.X2014.log                 H.X2014.log -4.620638e-02               0
## A.day.log                     A.day.log -4.581783e-02               0
## S.day.log                     S.day.log -4.555421e-02               0
## A.npnct30.log             A.npnct30.log -4.373349e-02               0
## S.npnct30.log             S.npnct30.log -4.370037e-02               0
## PubDate.last100         PubDate.last100  3.989229e-02               1
## PubDate.wkday.fctr   PubDate.wkday.fctr -3.980129e-02               0
## A.npnct13.log             A.npnct13.log -3.760012e-02               0
## S.npnct13.log             S.npnct13.log -3.638891e-02               0
## PubDate.last1             PubDate.last1  3.592267e-02               1
## A.new.log                     A.new.log -3.524871e-02               0
## S.new.log                     S.new.log -3.483189e-02               0
## PubDate.minute.fctr PubDate.minute.fctr -3.407385e-02               0
## H.npnct06.log             H.npnct06.log  3.190718e-02               0
## A.can.log                     A.can.log  3.169296e-02               0
## S.npnct01.log             S.npnct01.log  3.093101e-02               0
## A.npnct01.log             A.npnct01.log  3.093101e-02               0
## S.can.log                     S.can.log  3.077833e-02               0
## H.npnct17.log             H.npnct17.log  3.039622e-02               0
## S.npnct23.log             S.npnct23.log  2.760321e-02               0
## S.npnct25.log             S.npnct25.log  2.760321e-02               0
## A.take.log                   A.take.log -2.601772e-02               0
## H.has.ebola                 H.has.ebola  2.588140e-02               0
## S.take.log                   S.take.log -2.569295e-02               0
## H.npnct14.log             H.npnct14.log -2.524770e-02               0
## A.npnct15.log             A.npnct15.log -2.407715e-02               0
## S.npnct06.log             S.npnct06.log -2.389145e-02               0
## A.npnct06.log             A.npnct06.log -2.389145e-02               0
## S.make.log                   S.make.log  2.334962e-02               0
## A.make.log                   A.make.log  2.334962e-02               0
## H.npnct01.log             H.npnct01.log  2.271577e-02               0
## S.npnct15.log             S.npnct15.log -2.121844e-02               0
## S.presid.log               S.presid.log -2.014404e-02               0
## A.presid.log               A.presid.log -2.014404e-02               0
## H.npnct02.log             H.npnct02.log -2.001851e-02               0
## S.npnct22.log             S.npnct22.log -1.923169e-02               0
## A.npnct22.log             A.npnct22.log -1.923169e-02               0
## PubDate.month.fctr   PubDate.month.fctr  1.914874e-02               1
## S.has.year.colon       S.has.year.colon -1.755336e-02               0
## A.has.year.colon       A.has.year.colon -1.755336e-02               0
## PubDate.POSIX             PubDate.POSIX  1.568326e-02               1
## PubDate.zoo                 PubDate.zoo  1.568326e-02               1
## A.npnct23.log             A.npnct23.log  1.537569e-02               0
## A.npnct25.log             A.npnct25.log  1.537569e-02               0
## A.npnct02.log             A.npnct02.log -1.451467e-02               0
## A.npnct18.log             A.npnct18.log -1.451467e-02               0
## A.npnct20.log             A.npnct20.log -1.451467e-02               0
## A.has.http                   A.has.http -1.359260e-02               0
## A.npnct03.log             A.npnct03.log -1.359260e-02               0
## H.npnct12.log             H.npnct12.log  1.333613e-02               0
## H.npnct13.log             H.npnct13.log -1.305305e-02               0
## A.npnct19.log             A.npnct19.log -1.271661e-02               0
## S.npnct03.log             S.npnct03.log -1.240734e-02               0
## myCategory.fctr         myCategory.fctr  1.234541e-02               0
## S.npnct07.log             S.npnct07.log -1.214357e-02               0
## A.npnct07.log             A.npnct07.log -1.214357e-02               0
## H.npnct07.log             H.npnct07.log -1.201741e-02               0
## PubDate.second.fctr PubDate.second.fctr -1.187946e-02               0
## UniqueID                       UniqueID  1.182492e-02               1
## PubDate.date.fctr     PubDate.date.fctr -1.164756e-02               0
## H.npnct05.log             H.npnct05.log -9.653967e-03               0
## H.npnct03.log             H.npnct03.log  9.533020e-03               0
## .rnorm                           .rnorm -8.244230e-03               0
## PubDate.last100.log PubDate.last100.log -7.663322e-03               0
## S.state.log                 S.state.log  7.050791e-03               0
## A.state.log                 A.state.log  6.668101e-03               0
## H.npnct11.log             H.npnct11.log -5.547032e-03               0
## H.npnct22.log             H.npnct22.log -5.547032e-03               0
## S.npnct02.log             S.npnct02.log -5.547032e-03               0
## S.npnct11.log             S.npnct11.log -5.547032e-03               0
## A.npnct11.log             A.npnct11.log -5.547032e-03               0
## A.npnct27.log             A.npnct27.log -5.547032e-03               0
## S.one.log                     S.one.log  4.891059e-03               0
## A.npnct09.log             A.npnct09.log -4.775988e-03               0
## A.one.log                     A.one.log  4.368856e-03               0
## S.npnct09.log             S.npnct09.log -3.986882e-03               0
## A.npnct08.log             A.npnct08.log -3.258100e-03               0
## S.npnct08.log             S.npnct08.log -2.413868e-03               0
## S.npnct17.log             S.npnct17.log -1.587454e-03               0
## A.npnct17.log             A.npnct17.log -1.587454e-03               0
## S.said.log                   S.said.log  3.735051e-04               0
## A.said.log                   A.said.log  3.735051e-04               0
## H.npnct26.log             H.npnct26.log -9.890046e-19               0
## S.npnct26.log             S.npnct26.log -9.890046e-19               0
## A.npnct26.log             A.npnct26.log -9.890046e-19               0
## H.has.http                   H.has.http            NA               0
## H.npnct10.log             H.npnct10.log            NA               0
## H.npnct18.log             H.npnct18.log            NA               0
## H.npnct19.log             H.npnct19.log            NA               0
## H.npnct20.log             H.npnct20.log            NA               0
## H.npnct23.log             H.npnct23.log            NA               0
## H.npnct24.log             H.npnct24.log            NA               0
## H.npnct25.log             H.npnct25.log            NA               0
## H.npnct27.log             H.npnct27.log            NA               0
## H.npnct28.log             H.npnct28.log            NA               0
## H.npnct29.log             H.npnct29.log            NA               0
## H.npnct31.log             H.npnct31.log            NA               0
## H.npnct32.log             H.npnct32.log            NA               0
## S.has.http                   S.has.http            NA               0
## S.npnct05.log             S.npnct05.log            NA               0
## S.npnct10.log             S.npnct10.log            NA               0
## S.npnct18.log             S.npnct18.log            NA               0
## S.npnct19.log             S.npnct19.log            NA               0
## S.npnct20.log             S.npnct20.log            NA               0
## S.npnct24.log             S.npnct24.log            NA               0
## S.npnct27.log             S.npnct27.log            NA               0
## S.npnct28.log             S.npnct28.log            NA               0
## S.npnct29.log             S.npnct29.log            NA               0
## S.npnct31.log             S.npnct31.log            NA               0
## S.npnct32.log             S.npnct32.log            NA               0
## A.npnct05.log             A.npnct05.log            NA               0
## A.npnct10.log             A.npnct10.log            NA               0
## A.npnct24.log             A.npnct24.log            NA               0
## A.npnct28.log             A.npnct28.log            NA               0
## A.npnct29.log             A.npnct29.log            NA               0
## A.npnct31.log             A.npnct31.log            NA               0
## A.npnct32.log             A.npnct32.log            NA               0
## PubDate.year.fctr     PubDate.year.fctr            NA               0
##                        cor.y.abs
## Popular             1.000000e+00
## A.nuppr.log         2.720962e-01
## S.nuppr.log         2.718459e-01
## WordCount.log       2.656836e-01
## WordCount           2.575265e-01
## S.nwrds.unq.log     2.507969e-01
## A.nwrds.unq.log     2.506012e-01
## S.nwrds.log         2.453541e-01
## A.nwrds.log         2.450733e-01
## S.nchrs.log         2.246930e-01
## A.nchrs.log         2.245488e-01
## H.nwrds.unq.log     2.044964e-01
## H.nwrds.log         2.006864e-01
## H.nchrs.log         1.710624e-01
## PubDate.hour.fctr   1.354368e-01
## H.npnct21.log       1.283641e-01
## H.nuppr.log         1.278085e-01
## A.ndgts.log         1.249484e-01
## S.ndgts.log         1.242046e-01
## H.ndgts.log         1.196633e-01
## PubDate.wkend       1.067288e-01
## A.npnct12.log       9.183870e-02
## S.npnct12.log       9.158156e-02
## H.npnct30.log       8.917338e-02
## S.week.log          8.840293e-02
## A.week.log          8.840293e-02
## S.fashion.log       8.724932e-02
## A.fashion.log       8.724932e-02
## H.npnct16.log       8.273237e-02
## H.fashion.log       8.204998e-02
## H.has.year.colon    7.842875e-02
## H.week.log          7.510522e-02
## H.daili.log         6.919298e-02
## A.npnct16.log       6.893301e-02
## S.intern.log        6.864274e-02
## A.intern.log        6.864274e-02
## S.npnct16.log       6.770952e-02
## H.X2015.log         6.658489e-02
## H.report.log        6.494810e-02
## H.today.log         6.372306e-02
## S.npnct04.log       6.294642e-02
## A.npnct04.log       6.294642e-02
## H.day.log           6.272898e-02
## S.newyork.log       6.219997e-02
## A.newyork.log       6.219997e-02
## H.npnct15.log       6.158577e-02
## A.will.log          6.147068e-02
## S.will.log          6.103349e-02
## S.articl.log        5.952055e-02
## A.articl.log        5.952055e-02
## H.newyork.log       5.797009e-02
## A.time.log          5.779371e-02
## S.time.log          5.759227e-02
## S.npnct21.log       5.503894e-02
## A.npnct21.log       5.482747e-02
## PubDate.last10      5.398093e-02
## H.npnct08.log       5.375262e-02
## H.npnct09.log       5.375262e-02
## S.first.log         5.345938e-02
## A.first.log         5.345938e-02
## S.npnct14.log       5.332519e-02
## H.new.log           5.313316e-02
## A.compani.log       5.268413e-02
## S.compani.log       5.261812e-02
## S.share.log         5.138139e-02
## A.share.log         5.138139e-02
## H.npnct04.log       5.126277e-02
## S.year.log          5.094457e-02
## A.year.log          5.094457e-02
## S.report.log        5.032801e-02
## A.report.log        5.032801e-02
## A.npnct14.log       4.999563e-02
## PubDate.last10.log  4.931702e-02
## S.show.log          4.897915e-02
## A.show.log          4.897915e-02
## PubDate.last1.log   4.635751e-02
## H.X2014.log         4.620638e-02
## A.day.log           4.581783e-02
## S.day.log           4.555421e-02
## A.npnct30.log       4.373349e-02
## S.npnct30.log       4.370037e-02
## PubDate.last100     3.989229e-02
## PubDate.wkday.fctr  3.980129e-02
## A.npnct13.log       3.760012e-02
## S.npnct13.log       3.638891e-02
## PubDate.last1       3.592267e-02
## A.new.log           3.524871e-02
## S.new.log           3.483189e-02
## PubDate.minute.fctr 3.407385e-02
## H.npnct06.log       3.190718e-02
## A.can.log           3.169296e-02
## S.npnct01.log       3.093101e-02
## A.npnct01.log       3.093101e-02
## S.can.log           3.077833e-02
## H.npnct17.log       3.039622e-02
## S.npnct23.log       2.760321e-02
## S.npnct25.log       2.760321e-02
## A.take.log          2.601772e-02
## H.has.ebola         2.588140e-02
## S.take.log          2.569295e-02
## H.npnct14.log       2.524770e-02
## A.npnct15.log       2.407715e-02
## S.npnct06.log       2.389145e-02
## A.npnct06.log       2.389145e-02
## S.make.log          2.334962e-02
## A.make.log          2.334962e-02
## H.npnct01.log       2.271577e-02
## S.npnct15.log       2.121844e-02
## S.presid.log        2.014404e-02
## A.presid.log        2.014404e-02
## H.npnct02.log       2.001851e-02
## S.npnct22.log       1.923169e-02
## A.npnct22.log       1.923169e-02
## PubDate.month.fctr  1.914874e-02
## S.has.year.colon    1.755336e-02
## A.has.year.colon    1.755336e-02
## PubDate.POSIX       1.568326e-02
## PubDate.zoo         1.568326e-02
## A.npnct23.log       1.537569e-02
## A.npnct25.log       1.537569e-02
## A.npnct02.log       1.451467e-02
## A.npnct18.log       1.451467e-02
## A.npnct20.log       1.451467e-02
## A.has.http          1.359260e-02
## A.npnct03.log       1.359260e-02
## H.npnct12.log       1.333613e-02
## H.npnct13.log       1.305305e-02
## A.npnct19.log       1.271661e-02
## S.npnct03.log       1.240734e-02
## myCategory.fctr     1.234541e-02
## S.npnct07.log       1.214357e-02
## A.npnct07.log       1.214357e-02
## H.npnct07.log       1.201741e-02
## PubDate.second.fctr 1.187946e-02
## UniqueID            1.182492e-02
## PubDate.date.fctr   1.164756e-02
## H.npnct05.log       9.653967e-03
## H.npnct03.log       9.533020e-03
## .rnorm              8.244230e-03
## PubDate.last100.log 7.663322e-03
## S.state.log         7.050791e-03
## A.state.log         6.668101e-03
## H.npnct11.log       5.547032e-03
## H.npnct22.log       5.547032e-03
## S.npnct02.log       5.547032e-03
## S.npnct11.log       5.547032e-03
## A.npnct11.log       5.547032e-03
## A.npnct27.log       5.547032e-03
## S.one.log           4.891059e-03
## A.npnct09.log       4.775988e-03
## A.one.log           4.368856e-03
## S.npnct09.log       3.986882e-03
## A.npnct08.log       3.258100e-03
## S.npnct08.log       2.413868e-03
## S.npnct17.log       1.587454e-03
## A.npnct17.log       1.587454e-03
## S.said.log          3.735051e-04
## A.said.log          3.735051e-04
## H.npnct26.log       9.890046e-19
## S.npnct26.log       9.890046e-19
## A.npnct26.log       9.890046e-19
## H.has.http                    NA
## H.npnct10.log                 NA
## H.npnct18.log                 NA
## H.npnct19.log                 NA
## H.npnct20.log                 NA
## H.npnct23.log                 NA
## H.npnct24.log                 NA
## H.npnct25.log                 NA
## H.npnct27.log                 NA
## H.npnct28.log                 NA
## H.npnct29.log                 NA
## H.npnct31.log                 NA
## H.npnct32.log                 NA
## S.has.http                    NA
## S.npnct05.log                 NA
## S.npnct10.log                 NA
## S.npnct18.log                 NA
## S.npnct19.log                 NA
## S.npnct20.log                 NA
## S.npnct24.log                 NA
## S.npnct27.log                 NA
## S.npnct28.log                 NA
## S.npnct29.log                 NA
## S.npnct31.log                 NA
## S.npnct32.log                 NA
## A.npnct05.log                 NA
## A.npnct10.log                 NA
## A.npnct24.log                 NA
## A.npnct28.log                 NA
## A.npnct29.log                 NA
## A.npnct31.log                 NA
## A.npnct32.log                 NA
## PubDate.year.fctr             NA
```

```r
# stop("here")
# sav_feats_df <- glb_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, entity_df=glb_trnent_df, 
                              rsp_var=glb_rsp_var)))
```

```
## Loading required package: caret
## 
## Attaching package: 'caret'
## 
## The following object is masked from 'package:survival':
## 
##     cluster
```

```
## [1] "cor(A.articl.log, S.articl.log)=1.0000"
## [1] "cor(Popular.fctr, A.articl.log)=-0.0595"
## [1] "cor(Popular.fctr, S.articl.log)=-0.0595"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.articl.log as highly correlated with
## A.articl.log
```

```
## [1] "cor(A.fashion.log, S.fashion.log)=1.0000"
## [1] "cor(Popular.fctr, A.fashion.log)=-0.0872"
## [1] "cor(Popular.fctr, S.fashion.log)=-0.0872"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.fashion.log as highly correlated with
## A.fashion.log
```

```
## [1] "cor(A.first.log, S.first.log)=1.0000"
## [1] "cor(Popular.fctr, A.first.log)=-0.0535"
## [1] "cor(Popular.fctr, S.first.log)=-0.0535"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.first.log as highly correlated with
## A.first.log
```

```
## [1] "cor(A.has.year.colon, S.has.year.colon)=1.0000"
## [1] "cor(Popular.fctr, A.has.year.colon)=-0.0176"
## [1] "cor(Popular.fctr, S.has.year.colon)=-0.0176"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.has.year.colon as highly correlated with
## A.has.year.colon
```

```
## [1] "cor(A.intern.log, S.intern.log)=1.0000"
## [1] "cor(Popular.fctr, A.intern.log)=-0.0686"
## [1] "cor(Popular.fctr, S.intern.log)=-0.0686"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.intern.log as highly correlated with
## A.intern.log
```

```
## [1] "cor(A.make.log, S.make.log)=1.0000"
## [1] "cor(Popular.fctr, A.make.log)=0.0233"
## [1] "cor(Popular.fctr, S.make.log)=0.0233"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.make.log as highly correlated with
## A.make.log
```

```
## [1] "cor(A.newyork.log, S.newyork.log)=1.0000"
## [1] "cor(Popular.fctr, A.newyork.log)=-0.0622"
## [1] "cor(Popular.fctr, S.newyork.log)=-0.0622"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.newyork.log as highly correlated with
## A.newyork.log
```

```
## [1] "cor(A.npnct01.log, S.npnct01.log)=1.0000"
## [1] "cor(Popular.fctr, A.npnct01.log)=0.0309"
## [1] "cor(Popular.fctr, S.npnct01.log)=0.0309"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct01.log as highly correlated with
## A.npnct01.log
```

```
## [1] "cor(A.npnct04.log, S.npnct04.log)=1.0000"
## [1] "cor(Popular.fctr, A.npnct04.log)=-0.0629"
## [1] "cor(Popular.fctr, S.npnct04.log)=-0.0629"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct04.log as highly correlated with
## A.npnct04.log
```

```
## [1] "cor(A.npnct06.log, S.npnct06.log)=1.0000"
## [1] "cor(Popular.fctr, A.npnct06.log)=-0.0239"
## [1] "cor(Popular.fctr, S.npnct06.log)=-0.0239"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct06.log as highly correlated with
## A.npnct06.log
```

```
## [1] "cor(A.npnct07.log, S.npnct07.log)=1.0000"
## [1] "cor(Popular.fctr, A.npnct07.log)=-0.0121"
## [1] "cor(Popular.fctr, S.npnct07.log)=-0.0121"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct07.log as highly correlated with
## A.npnct07.log
```

```
## [1] "cor(A.npnct18.log, A.npnct20.log)=1.0000"
## [1] "cor(Popular.fctr, A.npnct18.log)=-0.0145"
## [1] "cor(Popular.fctr, A.npnct20.log)=-0.0145"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct20.log as highly correlated with
## A.npnct18.log
```

```
## [1] "cor(A.npnct22.log, S.npnct22.log)=1.0000"
## [1] "cor(Popular.fctr, A.npnct22.log)=-0.0192"
## [1] "cor(Popular.fctr, S.npnct22.log)=-0.0192"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct22.log as highly correlated with
## A.npnct22.log
```

```
## [1] "cor(A.npnct23.log, A.npnct25.log)=1.0000"
## [1] "cor(Popular.fctr, A.npnct23.log)=0.0154"
## [1] "cor(Popular.fctr, A.npnct25.log)=0.0154"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct25.log as highly correlated with
## A.npnct23.log
```

```
## [1] "cor(A.presid.log, S.presid.log)=1.0000"
## [1] "cor(Popular.fctr, A.presid.log)=-0.0201"
## [1] "cor(Popular.fctr, S.presid.log)=-0.0201"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.presid.log as highly correlated with
## A.presid.log
```

```
## [1] "cor(A.report.log, S.report.log)=1.0000"
## [1] "cor(Popular.fctr, A.report.log)=-0.0503"
## [1] "cor(Popular.fctr, S.report.log)=-0.0503"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.report.log as highly correlated with
## A.report.log
```

```
## [1] "cor(A.share.log, S.share.log)=1.0000"
## [1] "cor(Popular.fctr, A.share.log)=-0.0514"
## [1] "cor(Popular.fctr, S.share.log)=-0.0514"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.share.log as highly correlated with
## A.share.log
```

```
## [1] "cor(A.show.log, S.show.log)=1.0000"
## [1] "cor(Popular.fctr, A.show.log)=-0.0490"
## [1] "cor(Popular.fctr, S.show.log)=-0.0490"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.show.log as highly correlated with
## A.show.log
```

```
## [1] "cor(A.week.log, S.week.log)=1.0000"
## [1] "cor(Popular.fctr, A.week.log)=-0.0884"
## [1] "cor(Popular.fctr, S.week.log)=-0.0884"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.week.log as highly correlated with
## A.week.log
```

```
## [1] "cor(A.year.log, S.year.log)=1.0000"
## [1] "cor(Popular.fctr, A.year.log)=-0.0509"
## [1] "cor(Popular.fctr, S.year.log)=-0.0509"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.year.log as highly correlated with
## A.year.log
```

```
## [1] "cor(H.npnct08.log, H.npnct09.log)=1.0000"
## [1] "cor(Popular.fctr, H.npnct08.log)=0.0538"
## [1] "cor(Popular.fctr, H.npnct09.log)=0.0538"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.npnct09.log as highly correlated with
## H.npnct08.log
```

```
## [1] "cor(S.npnct23.log, S.npnct25.log)=1.0000"
## [1] "cor(Popular.fctr, S.npnct23.log)=0.0276"
## [1] "cor(Popular.fctr, S.npnct25.log)=0.0276"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct25.log as highly correlated with
## S.npnct23.log
```

```
## [1] "cor(A.npnct12.log, S.npnct12.log)=0.9997"
## [1] "cor(Popular.fctr, A.npnct12.log)=-0.0918"
## [1] "cor(Popular.fctr, S.npnct12.log)=-0.0916"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct12.log as highly correlated with
## A.npnct12.log
```

```
## [1] "cor(A.compani.log, S.compani.log)=0.9995"
## [1] "cor(Popular.fctr, A.compani.log)=-0.0527"
## [1] "cor(Popular.fctr, S.compani.log)=-0.0526"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.compani.log as highly correlated with
## A.compani.log
```

```
## [1] "cor(A.can.log, S.can.log)=0.9993"
## [1] "cor(Popular.fctr, A.can.log)=0.0317"
## [1] "cor(Popular.fctr, S.can.log)=0.0308"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.can.log as highly correlated with A.can.log
```

```
## [1] "cor(A.nuppr.log, S.nuppr.log)=0.9991"
## [1] "cor(Popular.fctr, A.nuppr.log)=-0.2721"
## [1] "cor(Popular.fctr, S.nuppr.log)=-0.2718"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.nuppr.log as highly correlated with
## A.nuppr.log
```

```
## [1] "cor(A.time.log, S.time.log)=0.9990"
## [1] "cor(Popular.fctr, A.time.log)=-0.0578"
## [1] "cor(Popular.fctr, S.time.log)=-0.0576"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.time.log as highly correlated with
## A.time.log
```

```
## [1] "cor(A.npnct30.log, S.npnct30.log)=0.9989"
## [1] "cor(Popular.fctr, A.npnct30.log)=-0.0437"
## [1] "cor(Popular.fctr, S.npnct30.log)=-0.0437"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct30.log as highly correlated with
## A.npnct30.log
```

```
## [1] "cor(A.nwrds.unq.log, S.nwrds.unq.log)=0.9989"
## [1] "cor(Popular.fctr, A.nwrds.unq.log)=-0.2506"
## [1] "cor(Popular.fctr, S.nwrds.unq.log)=-0.2508"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.nwrds.unq.log as highly correlated with
## S.nwrds.unq.log
```

```
## [1] "cor(A.nwrds.log, S.nwrds.log)=0.9988"
## [1] "cor(Popular.fctr, A.nwrds.log)=-0.2451"
## [1] "cor(Popular.fctr, S.nwrds.log)=-0.2454"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.nwrds.log as highly correlated with
## S.nwrds.log
```

```
## [1] "cor(A.nchrs.log, S.nchrs.log)=0.9986"
## [1] "cor(Popular.fctr, A.nchrs.log)=-0.2245"
## [1] "cor(Popular.fctr, S.nchrs.log)=-0.2247"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.nchrs.log as highly correlated with
## S.nchrs.log
```

```
## [1] "cor(A.new.log, S.new.log)=0.9982"
## [1] "cor(Popular.fctr, A.new.log)=-0.0352"
## [1] "cor(Popular.fctr, S.new.log)=-0.0348"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.new.log as highly correlated with A.new.log
```

```
## [1] "cor(A.day.log, S.day.log)=0.9981"
## [1] "cor(Popular.fctr, A.day.log)=-0.0458"
## [1] "cor(Popular.fctr, S.day.log)=-0.0456"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.day.log as highly correlated with A.day.log
```

```
## [1] "cor(A.will.log, S.will.log)=0.9979"
## [1] "cor(Popular.fctr, A.will.log)=-0.0615"
## [1] "cor(Popular.fctr, S.will.log)=-0.0610"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.will.log as highly correlated with
## A.will.log
```

```
## [1] "cor(A.take.log, S.take.log)=0.9976"
## [1] "cor(Popular.fctr, A.take.log)=-0.0260"
## [1] "cor(Popular.fctr, S.take.log)=-0.0257"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.take.log as highly correlated with
## A.take.log
```

```
## [1] "cor(H.nwrds.log, H.nwrds.unq.log)=0.9967"
## [1] "cor(Popular.fctr, H.nwrds.log)=-0.2007"
## [1] "cor(Popular.fctr, H.nwrds.unq.log)=-0.2045"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.nwrds.log as highly correlated with
## H.nwrds.unq.log
```

```
## [1] "cor(A.npnct21.log, S.npnct21.log)=0.9957"
## [1] "cor(Popular.fctr, A.npnct21.log)=0.0548"
## [1] "cor(Popular.fctr, S.npnct21.log)=0.0550"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct21.log as highly correlated with
## S.npnct21.log
```

```
## [1] "cor(A.ndgts.log, S.ndgts.log)=0.9955"
## [1] "cor(Popular.fctr, A.ndgts.log)=-0.1249"
## [1] "cor(Popular.fctr, S.ndgts.log)=-0.1242"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.ndgts.log as highly correlated with
## A.ndgts.log
```

```
## [1] "cor(S.nwrds.log, S.nwrds.unq.log)=0.9954"
## [1] "cor(Popular.fctr, S.nwrds.log)=-0.2454"
## [1] "cor(Popular.fctr, S.nwrds.unq.log)=-0.2508"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.nwrds.log as highly correlated with
## S.nwrds.unq.log
```

```
## [1] "cor(A.npnct13.log, S.npnct13.log)=0.9935"
## [1] "cor(Popular.fctr, A.npnct13.log)=-0.0376"
## [1] "cor(Popular.fctr, S.npnct13.log)=-0.0364"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct13.log as highly correlated with
## A.npnct13.log
```

```
## [1] "cor(A.npnct16.log, S.npnct16.log)=0.9917"
## [1] "cor(Popular.fctr, A.npnct16.log)=-0.0689"
## [1] "cor(Popular.fctr, S.npnct16.log)=-0.0677"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct16.log as highly correlated with
## A.npnct16.log
```

```
## [1] "cor(A.npnct14.log, S.npnct14.log)=0.9795"
## [1] "cor(Popular.fctr, A.npnct14.log)=-0.0500"
## [1] "cor(Popular.fctr, S.npnct14.log)=-0.0533"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct14.log as highly correlated with
## S.npnct14.log
```

```
## [1] "cor(S.nchrs.log, S.nwrds.unq.log)=0.9543"
## [1] "cor(Popular.fctr, S.nchrs.log)=-0.2247"
## [1] "cor(Popular.fctr, S.nwrds.unq.log)=-0.2508"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.nchrs.log as highly correlated with
## S.nwrds.unq.log
```

```
## [1] "cor(A.has.http, A.npnct19.log)=0.9356"
## [1] "cor(Popular.fctr, A.has.http)=-0.0136"
## [1] "cor(Popular.fctr, A.npnct19.log)=-0.0127"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct19.log as highly correlated with
## A.has.http
```

```
## [1] "cor(A.has.http, A.npnct02.log)=0.9247"
## [1] "cor(Popular.fctr, A.has.http)=-0.0136"
## [1] "cor(Popular.fctr, A.npnct02.log)=-0.0145"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.has.http as highly correlated with
## A.npnct02.log
```

```
## [1] "cor(A.npnct03.log, S.npnct03.log)=0.9128"
## [1] "cor(Popular.fctr, A.npnct03.log)=-0.0136"
## [1] "cor(Popular.fctr, S.npnct03.log)=-0.0124"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct03.log as highly correlated with
## A.npnct03.log
```

```
## [1] "cor(H.nchrs.log, H.nwrds.unq.log)=0.8881"
## [1] "cor(Popular.fctr, H.nchrs.log)=-0.1711"
## [1] "cor(Popular.fctr, H.nwrds.unq.log)=-0.2045"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.nchrs.log as highly correlated with
## H.nwrds.unq.log
```

```
## [1] "cor(H.npnct15.log, H.X2015.log)=0.8780"
## [1] "cor(Popular.fctr, H.npnct15.log)=-0.0616"
## [1] "cor(Popular.fctr, H.X2015.log)=-0.0666"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.npnct15.log as highly correlated with
## H.X2015.log
```

```
## [1] "cor(A.npnct02.log, A.npnct18.log)=0.8771"
## [1] "cor(Popular.fctr, A.npnct02.log)=-0.0145"
## [1] "cor(Popular.fctr, A.npnct18.log)=-0.0145"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct18.log as highly correlated with
## A.npnct02.log
```

```
## [1] "cor(H.nuppr.log, H.nwrds.unq.log)=0.8288"
## [1] "cor(Popular.fctr, H.nuppr.log)=-0.1278"
## [1] "cor(Popular.fctr, H.nwrds.unq.log)=-0.2045"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.nuppr.log as highly correlated with
## H.nwrds.unq.log
```

```
## [1] "cor(H.npnct06.log, H.npnct17.log)=0.8106"
## [1] "cor(Popular.fctr, H.npnct06.log)=0.0319"
## [1] "cor(Popular.fctr, H.npnct17.log)=0.0304"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.npnct17.log as highly correlated with
## H.npnct06.log
```

```
## [1] "cor(A.intern.log, H.has.year.colon)=0.7757"
## [1] "cor(Popular.fctr, A.intern.log)=-0.0686"
## [1] "cor(Popular.fctr, H.has.year.colon)=-0.0784"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.intern.log as highly correlated with
## H.has.year.colon
```

```
## [1] "cor(H.fashion.log, H.week.log)=0.7658"
## [1] "cor(Popular.fctr, H.fashion.log)=-0.0820"
## [1] "cor(Popular.fctr, H.week.log)=-0.0751"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.week.log as highly correlated with
## H.fashion.log
```

```
## [1] "cor(A.npnct23.log, S.npnct23.log)=0.7461"
## [1] "cor(Popular.fctr, A.npnct23.log)=0.0154"
## [1] "cor(Popular.fctr, S.npnct23.log)=0.0276"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct23.log as highly correlated with
## S.npnct23.log
```

```
## [1] "cor(A.npnct02.log, A.npnct15.log)=0.7324"
## [1] "cor(Popular.fctr, A.npnct02.log)=-0.0145"
## [1] "cor(Popular.fctr, A.npnct15.log)=-0.0241"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct02.log as highly correlated with
## A.npnct15.log
```

```
##                                      id         cor.y exclude.as.feat
## Popular                         Popular  1.000000e+00               1
## WordCount.log             WordCount.log  2.656836e-01               0
## WordCount                     WordCount  2.575265e-01               1
## PubDate.hour.fctr     PubDate.hour.fctr  1.354368e-01               0
## H.npnct21.log             H.npnct21.log  1.283641e-01               0
## PubDate.wkend             PubDate.wkend  1.067288e-01               0
## S.npnct21.log             S.npnct21.log  5.503894e-02               0
## A.npnct21.log             A.npnct21.log  5.482747e-02               0
## PubDate.last10           PubDate.last10  5.398093e-02               1
## H.npnct08.log             H.npnct08.log  5.375262e-02               0
## H.npnct09.log             H.npnct09.log  5.375262e-02               0
## PubDate.last10.log   PubDate.last10.log  4.931702e-02               0
## PubDate.last1.log     PubDate.last1.log  4.635751e-02               0
## PubDate.last100         PubDate.last100  3.989229e-02               1
## PubDate.last1             PubDate.last1  3.592267e-02               1
## H.npnct06.log             H.npnct06.log  3.190718e-02               0
## A.can.log                     A.can.log  3.169296e-02               0
## A.npnct01.log             A.npnct01.log  3.093101e-02               0
## S.npnct01.log             S.npnct01.log  3.093101e-02               0
## S.can.log                     S.can.log  3.077833e-02               0
## H.npnct17.log             H.npnct17.log  3.039622e-02               0
## S.npnct23.log             S.npnct23.log  2.760321e-02               0
## S.npnct25.log             S.npnct25.log  2.760321e-02               0
## H.has.ebola                 H.has.ebola  2.588140e-02               0
## A.make.log                   A.make.log  2.334962e-02               0
## S.make.log                   S.make.log  2.334962e-02               0
## H.npnct01.log             H.npnct01.log  2.271577e-02               0
## PubDate.month.fctr   PubDate.month.fctr  1.914874e-02               1
## PubDate.POSIX             PubDate.POSIX  1.568326e-02               1
## PubDate.zoo                 PubDate.zoo  1.568326e-02               1
## A.npnct23.log             A.npnct23.log  1.537569e-02               0
## A.npnct25.log             A.npnct25.log  1.537569e-02               0
## H.npnct12.log             H.npnct12.log  1.333613e-02               0
## myCategory.fctr         myCategory.fctr  1.234541e-02               0
## UniqueID                       UniqueID  1.182492e-02               1
## H.npnct03.log             H.npnct03.log  9.533020e-03               0
## S.state.log                 S.state.log  7.050791e-03               0
## A.state.log                 A.state.log  6.668101e-03               0
## S.one.log                     S.one.log  4.891059e-03               0
## A.one.log                     A.one.log  4.368856e-03               0
## A.said.log                   A.said.log  3.735051e-04               0
## S.said.log                   S.said.log  3.735051e-04               0
## A.npnct26.log             A.npnct26.log -9.890046e-19               0
## H.npnct26.log             H.npnct26.log -9.890046e-19               0
## S.npnct26.log             S.npnct26.log -9.890046e-19               0
## A.npnct17.log             A.npnct17.log -1.587454e-03               0
## S.npnct17.log             S.npnct17.log -1.587454e-03               0
## S.npnct08.log             S.npnct08.log -2.413868e-03               0
## A.npnct08.log             A.npnct08.log -3.258100e-03               0
## S.npnct09.log             S.npnct09.log -3.986882e-03               0
## A.npnct09.log             A.npnct09.log -4.775988e-03               0
## A.npnct27.log             A.npnct27.log -5.547032e-03               0
## A.npnct11.log             A.npnct11.log -5.547032e-03               0
## H.npnct11.log             H.npnct11.log -5.547032e-03               0
## H.npnct22.log             H.npnct22.log -5.547032e-03               0
## S.npnct02.log             S.npnct02.log -5.547032e-03               0
## S.npnct11.log             S.npnct11.log -5.547032e-03               0
## PubDate.last100.log PubDate.last100.log -7.663322e-03               0
## .rnorm                           .rnorm -8.244230e-03               0
## H.npnct05.log             H.npnct05.log -9.653967e-03               0
## PubDate.date.fctr     PubDate.date.fctr -1.164756e-02               0
## PubDate.second.fctr PubDate.second.fctr -1.187946e-02               0
## H.npnct07.log             H.npnct07.log -1.201741e-02               0
## A.npnct07.log             A.npnct07.log -1.214357e-02               0
## S.npnct07.log             S.npnct07.log -1.214357e-02               0
## S.npnct03.log             S.npnct03.log -1.240734e-02               0
## A.npnct19.log             A.npnct19.log -1.271661e-02               0
## H.npnct13.log             H.npnct13.log -1.305305e-02               0
## A.has.http                   A.has.http -1.359260e-02               0
## A.npnct03.log             A.npnct03.log -1.359260e-02               0
## A.npnct02.log             A.npnct02.log -1.451467e-02               0
## A.npnct18.log             A.npnct18.log -1.451467e-02               0
## A.npnct20.log             A.npnct20.log -1.451467e-02               0
## A.has.year.colon       A.has.year.colon -1.755336e-02               0
## S.has.year.colon       S.has.year.colon -1.755336e-02               0
## A.npnct22.log             A.npnct22.log -1.923169e-02               0
## S.npnct22.log             S.npnct22.log -1.923169e-02               0
## H.npnct02.log             H.npnct02.log -2.001851e-02               0
## A.presid.log               A.presid.log -2.014404e-02               0
## S.presid.log               S.presid.log -2.014404e-02               0
## S.npnct15.log             S.npnct15.log -2.121844e-02               0
## A.npnct06.log             A.npnct06.log -2.389145e-02               0
## S.npnct06.log             S.npnct06.log -2.389145e-02               0
## A.npnct15.log             A.npnct15.log -2.407715e-02               0
## H.npnct14.log             H.npnct14.log -2.524770e-02               0
## S.take.log                   S.take.log -2.569295e-02               0
## A.take.log                   A.take.log -2.601772e-02               0
## PubDate.minute.fctr PubDate.minute.fctr -3.407385e-02               0
## S.new.log                     S.new.log -3.483189e-02               0
## A.new.log                     A.new.log -3.524871e-02               0
## S.npnct13.log             S.npnct13.log -3.638891e-02               0
## A.npnct13.log             A.npnct13.log -3.760012e-02               0
## PubDate.wkday.fctr   PubDate.wkday.fctr -3.980129e-02               0
## S.npnct30.log             S.npnct30.log -4.370037e-02               0
## A.npnct30.log             A.npnct30.log -4.373349e-02               0
## S.day.log                     S.day.log -4.555421e-02               0
## A.day.log                     A.day.log -4.581783e-02               0
## H.X2014.log                 H.X2014.log -4.620638e-02               0
## A.show.log                   A.show.log -4.897915e-02               0
## S.show.log                   S.show.log -4.897915e-02               0
## A.npnct14.log             A.npnct14.log -4.999563e-02               0
## A.report.log               A.report.log -5.032801e-02               0
## S.report.log               S.report.log -5.032801e-02               0
## A.year.log                   A.year.log -5.094457e-02               0
## S.year.log                   S.year.log -5.094457e-02               0
## H.npnct04.log             H.npnct04.log -5.126277e-02               0
## A.share.log                 A.share.log -5.138139e-02               0
## S.share.log                 S.share.log -5.138139e-02               0
## S.compani.log             S.compani.log -5.261812e-02               0
## A.compani.log             A.compani.log -5.268413e-02               0
## H.new.log                     H.new.log -5.313316e-02               0
## S.npnct14.log             S.npnct14.log -5.332519e-02               0
## A.first.log                 A.first.log -5.345938e-02               0
## S.first.log                 S.first.log -5.345938e-02               0
## S.time.log                   S.time.log -5.759227e-02               0
## A.time.log                   A.time.log -5.779371e-02               0
## H.newyork.log             H.newyork.log -5.797009e-02               0
## A.articl.log               A.articl.log -5.952055e-02               0
## S.articl.log               S.articl.log -5.952055e-02               0
## S.will.log                   S.will.log -6.103349e-02               0
## A.will.log                   A.will.log -6.147068e-02               0
## H.npnct15.log             H.npnct15.log -6.158577e-02               0
## A.newyork.log             A.newyork.log -6.219997e-02               0
## S.newyork.log             S.newyork.log -6.219997e-02               0
## H.day.log                     H.day.log -6.272898e-02               0
## A.npnct04.log             A.npnct04.log -6.294642e-02               0
## S.npnct04.log             S.npnct04.log -6.294642e-02               0
## H.today.log                 H.today.log -6.372306e-02               0
## H.report.log               H.report.log -6.494810e-02               0
## H.X2015.log                 H.X2015.log -6.658489e-02               0
## S.npnct16.log             S.npnct16.log -6.770952e-02               0
## A.intern.log               A.intern.log -6.864274e-02               0
## S.intern.log               S.intern.log -6.864274e-02               0
## A.npnct16.log             A.npnct16.log -6.893301e-02               0
## H.daili.log                 H.daili.log -6.919298e-02               0
## H.week.log                   H.week.log -7.510522e-02               0
## H.has.year.colon       H.has.year.colon -7.842875e-02               0
## H.fashion.log             H.fashion.log -8.204998e-02               0
## H.npnct16.log             H.npnct16.log -8.273237e-02               0
## A.fashion.log             A.fashion.log -8.724932e-02               0
## S.fashion.log             S.fashion.log -8.724932e-02               0
## A.week.log                   A.week.log -8.840293e-02               0
## S.week.log                   S.week.log -8.840293e-02               0
## H.npnct30.log             H.npnct30.log -8.917338e-02               0
## S.npnct12.log             S.npnct12.log -9.158156e-02               0
## A.npnct12.log             A.npnct12.log -9.183870e-02               0
## H.ndgts.log                 H.ndgts.log -1.196633e-01               0
## S.ndgts.log                 S.ndgts.log -1.242046e-01               0
## A.ndgts.log                 A.ndgts.log -1.249484e-01               0
## H.nuppr.log                 H.nuppr.log -1.278085e-01               0
## H.nchrs.log                 H.nchrs.log -1.710624e-01               0
## H.nwrds.log                 H.nwrds.log -2.006864e-01               0
## H.nwrds.unq.log         H.nwrds.unq.log -2.044964e-01               0
## A.nchrs.log                 A.nchrs.log -2.245488e-01               0
## S.nchrs.log                 S.nchrs.log -2.246930e-01               0
## A.nwrds.log                 A.nwrds.log -2.450733e-01               0
## S.nwrds.log                 S.nwrds.log -2.453541e-01               0
## A.nwrds.unq.log         A.nwrds.unq.log -2.506012e-01               0
## S.nwrds.unq.log         S.nwrds.unq.log -2.507969e-01               0
## S.nuppr.log                 S.nuppr.log -2.718459e-01               0
## A.nuppr.log                 A.nuppr.log -2.720962e-01               0
## A.npnct05.log             A.npnct05.log            NA               0
## A.npnct10.log             A.npnct10.log            NA               0
## A.npnct24.log             A.npnct24.log            NA               0
## A.npnct28.log             A.npnct28.log            NA               0
## A.npnct29.log             A.npnct29.log            NA               0
## A.npnct31.log             A.npnct31.log            NA               0
## A.npnct32.log             A.npnct32.log            NA               0
## H.has.http                   H.has.http            NA               0
## H.npnct10.log             H.npnct10.log            NA               0
## H.npnct18.log             H.npnct18.log            NA               0
## H.npnct19.log             H.npnct19.log            NA               0
## H.npnct20.log             H.npnct20.log            NA               0
## H.npnct23.log             H.npnct23.log            NA               0
## H.npnct24.log             H.npnct24.log            NA               0
## H.npnct25.log             H.npnct25.log            NA               0
## H.npnct27.log             H.npnct27.log            NA               0
## H.npnct28.log             H.npnct28.log            NA               0
## H.npnct29.log             H.npnct29.log            NA               0
## H.npnct31.log             H.npnct31.log            NA               0
## H.npnct32.log             H.npnct32.log            NA               0
## PubDate.year.fctr     PubDate.year.fctr            NA               0
## S.has.http                   S.has.http            NA               0
## S.npnct05.log             S.npnct05.log            NA               0
## S.npnct10.log             S.npnct10.log            NA               0
## S.npnct18.log             S.npnct18.log            NA               0
## S.npnct19.log             S.npnct19.log            NA               0
## S.npnct20.log             S.npnct20.log            NA               0
## S.npnct24.log             S.npnct24.log            NA               0
## S.npnct27.log             S.npnct27.log            NA               0
## S.npnct28.log             S.npnct28.log            NA               0
## S.npnct29.log             S.npnct29.log            NA               0
## S.npnct31.log             S.npnct31.log            NA               0
## S.npnct32.log             S.npnct32.log            NA               0
##                        cor.y.abs       cor.high.X   freqRatio
## Popular             1.000000e+00             <NA>    4.976212
## WordCount.log       2.656836e-01             <NA>    1.300000
## WordCount           2.575265e-01             <NA>    2.315789
## PubDate.hour.fctr   1.354368e-01             <NA>    1.835040
## H.npnct21.log       1.283641e-01             <NA>   14.995098
## PubDate.wkend       1.067288e-01             <NA>    9.095827
## S.npnct21.log       5.503894e-02    A.npnct21.log   12.862366
## A.npnct21.log       5.482747e-02             <NA>   12.798715
## PubDate.last10      5.398093e-02             <NA>    1.666667
## H.npnct08.log       5.375262e-02    H.npnct09.log  111.620690
## H.npnct09.log       5.375262e-02             <NA>  111.620690
## PubDate.last10.log  4.931702e-02             <NA>    1.666667
## PubDate.last1.log   4.635751e-02             <NA>    1.142857
## PubDate.last100     3.989229e-02             <NA>   25.000000
## PubDate.last1       3.592267e-02             <NA>    1.142857
## H.npnct06.log       3.190718e-02    H.npnct17.log   68.935484
## A.can.log           3.169296e-02        S.can.log   26.166667
## A.npnct01.log       3.093101e-02    S.npnct01.log  309.952381
## S.npnct01.log       3.093101e-02             <NA>  309.952381
## S.can.log           3.077833e-02             <NA>   26.058091
## H.npnct17.log       3.039622e-02             <NA>   96.104478
## S.npnct23.log       2.760321e-02    A.npnct23.log 6531.000000
## S.npnct25.log       2.760321e-02             <NA> 6531.000000
## H.has.ebola         2.588140e-02             <NA>   73.227273
## A.make.log          2.334962e-02       S.make.log   27.378261
## S.make.log          2.334962e-02             <NA>   27.378261
## H.npnct01.log       2.271577e-02             <NA>  282.913043
## PubDate.month.fctr  1.914874e-02             <NA>    1.017514
## PubDate.POSIX       1.568326e-02             <NA>    1.000000
## PubDate.zoo         1.568326e-02             <NA>    1.000000
## A.npnct23.log       1.537569e-02    A.npnct25.log 3264.500000
## A.npnct25.log       1.537569e-02             <NA> 3264.500000
## H.npnct12.log       1.333613e-02             <NA>    4.937442
## myCategory.fctr     1.234541e-02             <NA>    1.337185
## UniqueID            1.182492e-02             <NA>    1.000000
## H.npnct03.log       9.533020e-03             <NA> 2176.333333
## S.state.log         7.050791e-03             <NA>   30.655340
## A.state.log         6.668101e-03             <NA>   30.502415
## S.one.log           4.891059e-03             <NA>   22.777372
## A.one.log           4.368856e-03             <NA>   22.773723
## A.said.log          3.735051e-04             <NA>   25.212851
## S.said.log          3.735051e-04             <NA>   25.212851
## A.npnct26.log       9.890046e-19             <NA>    0.000000
## H.npnct26.log       9.890046e-19             <NA>    0.000000
## S.npnct26.log       9.890046e-19             <NA>    0.000000
## A.npnct17.log       1.587454e-03             <NA>  434.133333
## S.npnct17.log       1.587454e-03             <NA>  434.133333
## S.npnct08.log       2.413868e-03             <NA>  175.513514
## A.npnct08.log       3.258100e-03             <NA>  170.868421
## S.npnct09.log       3.986882e-03             <NA>  175.486486
## A.npnct09.log       4.775988e-03             <NA>  170.842105
## A.npnct27.log       5.547032e-03             <NA> 6531.000000
## A.npnct11.log       5.547032e-03             <NA> 6531.000000
## H.npnct11.log       5.547032e-03             <NA> 6531.000000
## H.npnct22.log       5.547032e-03             <NA> 6531.000000
## S.npnct02.log       5.547032e-03             <NA> 6531.000000
## S.npnct11.log       5.547032e-03             <NA> 6531.000000
## PubDate.last100.log 7.663322e-03             <NA>   25.000000
## .rnorm              8.244230e-03             <NA>    2.000000
## H.npnct05.log       9.653967e-03             <NA>  543.333333
## PubDate.date.fctr   1.164756e-02             <NA>    1.021394
## PubDate.second.fctr 1.187946e-02             <NA>    1.018204
## H.npnct07.log       1.201741e-02             <NA>    5.437234
## A.npnct07.log       1.214357e-02    S.npnct07.log 1631.750000
## S.npnct07.log       1.214357e-02             <NA> 1631.750000
## S.npnct03.log       1.240734e-02             <NA> 1305.400000
## A.npnct19.log       1.271661e-02             <NA> 1631.500000
## H.npnct13.log       1.305305e-02             <NA>   13.126638
## A.has.http          1.359260e-02    A.npnct19.log 1087.666667
## A.npnct03.log       1.359260e-02    S.npnct03.log 1087.666667
## A.npnct02.log       1.451467e-02    A.npnct18.log 1087.500000
## A.npnct18.log       1.451467e-02    A.npnct20.log 1087.500000
## A.npnct20.log       1.451467e-02             <NA> 1087.500000
## A.has.year.colon    1.755336e-02 S.has.year.colon  652.200000
## S.has.year.colon    1.755336e-02             <NA>  652.200000
## A.npnct22.log       1.923169e-02    S.npnct22.log  543.333333
## S.npnct22.log       1.923169e-02             <NA>  543.333333
## H.npnct02.log       2.001851e-02             <NA>  501.461538
## A.presid.log        2.014404e-02     S.presid.log   26.854701
## S.presid.log        2.014404e-02             <NA>   26.854701
## S.npnct15.log       2.121844e-02             <NA>  203.062500
## A.npnct06.log       2.389145e-02    S.npnct06.log  115.642857
## S.npnct06.log       2.389145e-02             <NA>  115.642857
## A.npnct15.log       2.407715e-02    A.npnct02.log  196.696970
## H.npnct14.log       2.524770e-02             <NA>   22.802326
## S.take.log          2.569295e-02             <NA>   29.376744
## A.take.log          2.601772e-02       S.take.log   29.236111
## PubDate.minute.fctr 3.407385e-02             <NA>    1.483365
## S.new.log           3.483189e-02             <NA>   10.124573
## A.new.log           3.524871e-02        S.new.log   10.086735
## S.npnct13.log       3.638891e-02             <NA>    5.706263
## A.npnct13.log       3.760012e-02    S.npnct13.log    5.715368
## PubDate.wkday.fctr  3.980129e-02             <NA>    1.003268
## S.npnct30.log       4.370037e-02             <NA>  134.791667
## A.npnct30.log       4.373349e-02    S.npnct30.log  126.862745
## S.day.log           4.555421e-02             <NA>   24.692913
## A.day.log           4.581783e-02        S.day.log   24.592157
## H.X2014.log         4.620638e-02             <NA>   63.673267
## A.show.log          4.897915e-02       S.show.log   30.512077
## S.show.log          4.897915e-02             <NA>   30.512077
## A.npnct14.log       4.999563e-02             <NA>    4.603330
## A.report.log        5.032801e-02     S.report.log   24.204633
## S.report.log        5.032801e-02             <NA>   24.204633
## A.year.log          5.094457e-02       S.year.log   18.456716
## S.year.log          5.094457e-02             <NA>   18.456716
## H.npnct04.log       5.126277e-02             <NA>   38.325301
## A.share.log         5.138139e-02      S.share.log   32.654639
## S.share.log         5.138139e-02             <NA>   32.654639
## S.compani.log       5.261812e-02             <NA>   18.093842
## A.compani.log       5.268413e-02    S.compani.log   18.147059
## H.new.log           5.313316e-02             <NA>   25.228916
## S.npnct14.log       5.332519e-02    A.npnct14.log    4.672000
## A.first.log         5.345938e-02      S.first.log   29.509346
## S.first.log         5.345938e-02             <NA>   29.509346
## S.time.log          5.759227e-02             <NA>   13.483296
## A.time.log          5.779371e-02       S.time.log   13.451111
## H.newyork.log       5.797009e-02             <NA>   26.795745
## A.articl.log        5.952055e-02     S.articl.log   30.863415
## S.articl.log        5.952055e-02             <NA>   30.863415
## S.will.log          6.103349e-02             <NA>   11.237288
## A.will.log          6.147068e-02       S.will.log   11.212406
## H.npnct15.log       6.158577e-02             <NA>   52.983471
## A.newyork.log       6.219997e-02    S.newyork.log   15.153465
## S.newyork.log       6.219997e-02             <NA>   15.153465
## H.day.log           6.272898e-02             <NA>   29.801887
## A.npnct04.log       6.294642e-02    S.npnct04.log   28.536364
## S.npnct04.log       6.294642e-02             <NA>   28.536364
## H.today.log         6.372306e-02             <NA>   36.757225
## H.report.log        6.494810e-02             <NA>   30.403846
## H.X2015.log         6.658489e-02    H.npnct15.log   45.326241
## S.npnct16.log       6.770952e-02             <NA>   13.647191
## A.intern.log        6.864274e-02     S.intern.log   29.801887
## S.intern.log        6.864274e-02             <NA>   29.801887
## A.npnct16.log       6.893301e-02    S.npnct16.log   13.482222
## H.daili.log         6.919298e-02             <NA>   41.973684
## H.week.log          7.510522e-02             <NA>   24.818182
## H.has.year.colon    7.842875e-02     A.intern.log   32.670103
## H.fashion.log       8.204998e-02       H.week.log   28.542986
## H.npnct16.log       8.273237e-02             <NA>    3.914910
## A.fashion.log       8.724932e-02    S.fashion.log   25.737705
## S.fashion.log       8.724932e-02             <NA>   25.737705
## A.week.log          8.840293e-02       S.week.log   13.278509
## S.week.log          8.840293e-02             <NA>   13.278509
## H.npnct30.log       8.917338e-02             <NA>   24.123077
## S.npnct12.log       9.158156e-02             <NA>    1.660473
## A.npnct12.log       9.183870e-02    S.npnct12.log    1.660473
## H.ndgts.log         1.196633e-01             <NA>   13.616137
## S.ndgts.log         1.242046e-01             <NA>   10.511247
## A.ndgts.log         1.249484e-01      S.ndgts.log   10.501022
## H.nuppr.log         1.278085e-01             <NA>    1.033930
## H.nchrs.log         1.710624e-01             <NA>    1.023810
## H.nwrds.log         2.006864e-01             <NA>    1.019119
## H.nwrds.unq.log     2.044964e-01      H.nuppr.log    1.019071
## A.nchrs.log         2.245488e-01             <NA>    1.328571
## S.nchrs.log         2.246930e-01      A.nchrs.log    1.328571
## A.nwrds.log         2.450733e-01             <NA>    1.029183
## S.nwrds.log         2.453541e-01      A.nwrds.log    1.029183
## A.nwrds.unq.log     2.506012e-01             <NA>    1.061567
## S.nwrds.unq.log     2.507969e-01      S.nchrs.log    1.061567
## S.nuppr.log         2.718459e-01             <NA>    1.152620
## A.nuppr.log         2.720962e-01      S.nuppr.log    1.151308
## A.npnct05.log                 NA             <NA>    0.000000
## A.npnct10.log                 NA             <NA>    0.000000
## A.npnct24.log                 NA             <NA>    0.000000
## A.npnct28.log                 NA             <NA>    0.000000
## A.npnct29.log                 NA             <NA>    0.000000
## A.npnct31.log                 NA             <NA>    0.000000
## A.npnct32.log                 NA             <NA>    0.000000
## H.has.http                    NA             <NA>    0.000000
## H.npnct10.log                 NA             <NA>    0.000000
## H.npnct18.log                 NA             <NA>    0.000000
## H.npnct19.log                 NA             <NA>    0.000000
## H.npnct20.log                 NA             <NA>    0.000000
## H.npnct23.log                 NA             <NA>    0.000000
## H.npnct24.log                 NA             <NA>    0.000000
## H.npnct25.log                 NA             <NA>    0.000000
## H.npnct27.log                 NA             <NA>    0.000000
## H.npnct28.log                 NA             <NA>    0.000000
## H.npnct29.log                 NA             <NA>    0.000000
## H.npnct31.log                 NA             <NA>    0.000000
## H.npnct32.log                 NA             <NA>    0.000000
## PubDate.year.fctr             NA             <NA>    0.000000
## S.has.http                    NA             <NA>    0.000000
## S.npnct05.log                 NA             <NA>    0.000000
## S.npnct10.log                 NA             <NA>    0.000000
## S.npnct18.log                 NA             <NA>    0.000000
## S.npnct19.log                 NA             <NA>    0.000000
## S.npnct20.log                 NA             <NA>    0.000000
## S.npnct24.log                 NA             <NA>    0.000000
## S.npnct27.log                 NA             <NA>    0.000000
## S.npnct28.log                 NA             <NA>    0.000000
## S.npnct29.log                 NA             <NA>    0.000000
## S.npnct31.log                 NA             <NA>    0.000000
## S.npnct32.log                 NA             <NA>    0.000000
##                     percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## Popular                0.03061849   FALSE FALSE    FALSE            FALSE
## WordCount.log         24.14268218   FALSE FALSE    FALSE            FALSE
## WordCount             24.15799143   FALSE FALSE    FALSE            FALSE
## PubDate.hour.fctr      0.04592774   FALSE FALSE    FALSE            FALSE
## H.npnct21.log          0.06123699   FALSE FALSE    FALSE            FALSE
## PubDate.wkend          0.03061849   FALSE FALSE    FALSE            FALSE
## S.npnct21.log          0.07654623   FALSE FALSE    FALSE            FALSE
## A.npnct21.log          0.07654623   FALSE FALSE    FALSE            FALSE
## PubDate.last10        79.05695040   FALSE FALSE    FALSE            FALSE
## H.npnct08.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## H.npnct09.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## PubDate.last10.log    79.05695040   FALSE FALSE    FALSE            FALSE
## PubDate.last1.log     36.49724434   FALSE FALSE    FALSE            FALSE
## PubDate.last100       92.52908757   FALSE FALSE    FALSE            FALSE
## PubDate.last1         36.49724434   FALSE FALSE    FALSE            FALSE
## H.npnct06.log          0.06123699   FALSE  TRUE    FALSE            FALSE
## A.can.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## A.npnct01.log          0.06123699   FALSE  TRUE    FALSE            FALSE
## S.npnct01.log          0.06123699   FALSE  TRUE    FALSE            FALSE
## S.can.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## H.npnct17.log          0.06123699   FALSE  TRUE    FALSE            FALSE
## S.npnct23.log          0.03061849   FALSE  TRUE     TRUE            FALSE
## S.npnct25.log          0.03061849   FALSE  TRUE     TRUE            FALSE
## H.has.ebola            0.03061849   FALSE  TRUE    FALSE            FALSE
## A.make.log             0.04592774   FALSE  TRUE    FALSE            FALSE
## S.make.log             0.04592774   FALSE  TRUE    FALSE            FALSE
## H.npnct01.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## PubDate.month.fctr     0.04592774   FALSE FALSE    FALSE            FALSE
## PubDate.POSIX         99.86221678   FALSE FALSE    FALSE            FALSE
## PubDate.zoo           99.86221678   FALSE FALSE    FALSE            FALSE
## A.npnct23.log          0.04592774   FALSE  TRUE     TRUE            FALSE
## A.npnct25.log          0.04592774   FALSE  TRUE     TRUE            FALSE
## H.npnct12.log          0.07654623   FALSE FALSE    FALSE            FALSE
## myCategory.fctr        0.30618494   FALSE FALSE    FALSE            FALSE
## UniqueID             100.00000000   FALSE FALSE    FALSE            FALSE
## H.npnct03.log          0.03061849   FALSE  TRUE     TRUE            FALSE
## S.state.log            0.04592774   FALSE  TRUE    FALSE             TRUE
## A.state.log            0.04592774   FALSE  TRUE    FALSE             TRUE
## S.one.log              0.04592774   FALSE  TRUE    FALSE             TRUE
## A.one.log              0.04592774   FALSE  TRUE    FALSE             TRUE
## A.said.log             0.04592774   FALSE  TRUE    FALSE             TRUE
## S.said.log             0.04592774   FALSE  TRUE    FALSE             TRUE
## A.npnct26.log          0.01530925    TRUE  TRUE     TRUE             TRUE
## H.npnct26.log          0.01530925    TRUE  TRUE     TRUE             TRUE
## S.npnct26.log          0.01530925    TRUE  TRUE     TRUE             TRUE
## A.npnct17.log          0.04592774   FALSE  TRUE    FALSE             TRUE
## S.npnct17.log          0.04592774   FALSE  TRUE    FALSE             TRUE
## S.npnct08.log          0.04592774   FALSE  TRUE    FALSE             TRUE
## A.npnct08.log          0.04592774   FALSE  TRUE    FALSE             TRUE
## S.npnct09.log          0.06123699   FALSE  TRUE    FALSE             TRUE
## A.npnct09.log          0.06123699   FALSE  TRUE    FALSE             TRUE
## A.npnct27.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## A.npnct11.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## H.npnct11.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## H.npnct22.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## S.npnct02.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## S.npnct11.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## PubDate.last100.log   92.19228414   FALSE FALSE    FALSE             TRUE
## .rnorm                99.98469075   FALSE FALSE    FALSE            FALSE
## H.npnct05.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## PubDate.date.fctr      0.07654623   FALSE FALSE    FALSE            FALSE
## PubDate.second.fctr    0.06123699   FALSE FALSE    FALSE            FALSE
## H.npnct07.log          0.12247397   FALSE FALSE    FALSE            FALSE
## A.npnct07.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## S.npnct07.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## S.npnct03.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct19.log          0.06123699   FALSE  TRUE    FALSE            FALSE
## H.npnct13.log          0.09185548   FALSE FALSE    FALSE            FALSE
## A.has.http             0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct03.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct02.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## A.npnct18.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## A.npnct20.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## A.has.year.colon       0.03061849   FALSE  TRUE    FALSE            FALSE
## S.has.year.colon       0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct22.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## S.npnct22.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## H.npnct02.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.presid.log           0.06123699   FALSE  TRUE    FALSE            FALSE
## S.presid.log           0.06123699   FALSE  TRUE    FALSE            FALSE
## S.npnct15.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## A.npnct06.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## S.npnct06.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct15.log          0.10716473   FALSE  TRUE    FALSE            FALSE
## H.npnct14.log          0.12247397   FALSE  TRUE    FALSE            FALSE
## S.take.log             0.04592774   FALSE  TRUE    FALSE            FALSE
## A.take.log             0.04592774   FALSE  TRUE    FALSE            FALSE
## PubDate.minute.fctr    0.06123699   FALSE FALSE    FALSE            FALSE
## S.new.log              0.04592774   FALSE FALSE    FALSE            FALSE
## A.new.log              0.04592774   FALSE FALSE    FALSE            FALSE
## S.npnct13.log          0.09185548   FALSE FALSE    FALSE            FALSE
## A.npnct13.log          0.12247397   FALSE FALSE    FALSE            FALSE
## PubDate.wkday.fctr     0.10716473   FALSE FALSE    FALSE            FALSE
## S.npnct30.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## A.npnct30.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## S.day.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## A.day.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## H.X2014.log            0.03061849   FALSE  TRUE    FALSE            FALSE
## A.show.log             0.06123699   FALSE  TRUE    FALSE            FALSE
## S.show.log             0.06123699   FALSE  TRUE    FALSE            FALSE
## A.npnct14.log          0.16840171   FALSE FALSE    FALSE            FALSE
## A.report.log           0.06123699   FALSE  TRUE    FALSE            FALSE
## S.report.log           0.06123699   FALSE  TRUE    FALSE            FALSE
## A.year.log             0.06123699   FALSE FALSE    FALSE            FALSE
## S.year.log             0.06123699   FALSE FALSE    FALSE            FALSE
## H.npnct04.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## A.share.log            0.04592774   FALSE  TRUE    FALSE            FALSE
## S.share.log            0.04592774   FALSE  TRUE    FALSE            FALSE
## S.compani.log          0.04592774   FALSE FALSE    FALSE            FALSE
## A.compani.log          0.04592774   FALSE FALSE    FALSE            FALSE
## H.new.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## S.npnct14.log          0.16840171   FALSE FALSE    FALSE            FALSE
## A.first.log            0.04592774   FALSE  TRUE    FALSE            FALSE
## S.first.log            0.04592774   FALSE  TRUE    FALSE            FALSE
## S.time.log             0.04592774   FALSE FALSE    FALSE            FALSE
## A.time.log             0.04592774   FALSE FALSE    FALSE            FALSE
## H.newyork.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.articl.log           0.03061849   FALSE  TRUE    FALSE            FALSE
## S.articl.log           0.03061849   FALSE  TRUE    FALSE            FALSE
## S.will.log             0.06123699   FALSE FALSE    FALSE            FALSE
## A.will.log             0.06123699   FALSE FALSE    FALSE            FALSE
## H.npnct15.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.newyork.log          0.06123699   FALSE FALSE    FALSE            FALSE
## S.newyork.log          0.06123699   FALSE FALSE    FALSE            FALSE
## H.day.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## A.npnct04.log          0.07654623   FALSE  TRUE    FALSE            FALSE
## S.npnct04.log          0.07654623   FALSE  TRUE    FALSE            FALSE
## H.today.log            0.03061849   FALSE  TRUE    FALSE            FALSE
## H.report.log           0.03061849   FALSE  TRUE    FALSE            FALSE
## H.X2015.log            0.03061849   FALSE  TRUE    FALSE            FALSE
## S.npnct16.log          0.04592774   FALSE FALSE    FALSE            FALSE
## A.intern.log           0.04592774   FALSE  TRUE    FALSE            FALSE
## S.intern.log           0.04592774   FALSE  TRUE    FALSE            FALSE
## A.npnct16.log          0.04592774   FALSE FALSE    FALSE            FALSE
## H.daili.log            0.03061849   FALSE  TRUE    FALSE            FALSE
## H.week.log             0.03061849   FALSE  TRUE    FALSE            FALSE
## H.has.year.colon       0.03061849   FALSE  TRUE    FALSE            FALSE
## H.fashion.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## H.npnct16.log          0.04592774   FALSE FALSE    FALSE            FALSE
## A.fashion.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## S.fashion.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## A.week.log             0.04592774   FALSE FALSE    FALSE            FALSE
## S.week.log             0.04592774   FALSE FALSE    FALSE            FALSE
## H.npnct30.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## S.npnct12.log          0.13778322   FALSE FALSE    FALSE            FALSE
## A.npnct12.log          0.13778322   FALSE FALSE    FALSE            FALSE
## H.ndgts.log            0.18371096   FALSE FALSE    FALSE            FALSE
## S.ndgts.log            0.26025720   FALSE FALSE    FALSE            FALSE
## A.ndgts.log            0.29087569   FALSE FALSE    FALSE            FALSE
## H.nuppr.log            0.29087569   FALSE FALSE    FALSE            FALSE
## H.nchrs.log            1.57685242   FALSE FALSE    FALSE            FALSE
## H.nwrds.log            0.21432945   FALSE FALSE    FALSE            FALSE
## H.nwrds.unq.log        0.21432945   FALSE FALSE    FALSE            FALSE
## A.nchrs.log            4.39375383   FALSE FALSE    FALSE            FALSE
## S.nchrs.log            3.72014697   FALSE FALSE    FALSE            FALSE
## A.nwrds.log            0.59706062   FALSE FALSE    FALSE            FALSE
## S.nwrds.log            0.45927740   FALSE FALSE    FALSE            FALSE
## A.nwrds.unq.log        0.55113288   FALSE FALSE    FALSE            FALSE
## S.nwrds.unq.log        0.44396816   FALSE FALSE    FALSE            FALSE
## S.nuppr.log            0.33680343   FALSE FALSE    FALSE            FALSE
## A.nuppr.log            0.33680343   FALSE FALSE    FALSE            FALSE
## A.npnct05.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct10.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct24.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct28.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct29.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct31.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct32.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.has.http             0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct10.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct18.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct19.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct20.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct23.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct24.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct25.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct27.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct28.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct29.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct31.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct32.log          0.01530925    TRUE  TRUE     TRUE               NA
## PubDate.year.fctr      0.01530925    TRUE  TRUE     TRUE               NA
## S.has.http             0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct05.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct10.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct18.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct19.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct20.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct24.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct27.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct28.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct29.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct31.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct32.log          0.01530925    TRUE  TRUE     TRUE               NA
```

```r
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning: Removed 10 rows containing missing values (geom_point).
```

```
## Warning: Removed 10 rows containing missing values (geom_point).
```

```
## Warning: Removed 10 rows containing missing values (geom_point).
```

![](NYTBlogs_txtfeat_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##                                  id         cor.y exclude.as.feat
## S.npnct23.log         S.npnct23.log  2.760321e-02               0
## S.npnct25.log         S.npnct25.log  2.760321e-02               0
## A.npnct23.log         A.npnct23.log  1.537569e-02               0
## A.npnct25.log         A.npnct25.log  1.537569e-02               0
## H.npnct03.log         H.npnct03.log  9.533020e-03               0
## A.npnct26.log         A.npnct26.log -9.890046e-19               0
## H.npnct26.log         H.npnct26.log -9.890046e-19               0
## S.npnct26.log         S.npnct26.log -9.890046e-19               0
## A.npnct27.log         A.npnct27.log -5.547032e-03               0
## A.npnct11.log         A.npnct11.log -5.547032e-03               0
## H.npnct11.log         H.npnct11.log -5.547032e-03               0
## H.npnct22.log         H.npnct22.log -5.547032e-03               0
## S.npnct02.log         S.npnct02.log -5.547032e-03               0
## S.npnct11.log         S.npnct11.log -5.547032e-03               0
## A.npnct05.log         A.npnct05.log            NA               0
## A.npnct10.log         A.npnct10.log            NA               0
## A.npnct24.log         A.npnct24.log            NA               0
## A.npnct28.log         A.npnct28.log            NA               0
## A.npnct29.log         A.npnct29.log            NA               0
## A.npnct31.log         A.npnct31.log            NA               0
## A.npnct32.log         A.npnct32.log            NA               0
## H.has.http               H.has.http            NA               0
## H.npnct10.log         H.npnct10.log            NA               0
## H.npnct18.log         H.npnct18.log            NA               0
## H.npnct19.log         H.npnct19.log            NA               0
## H.npnct20.log         H.npnct20.log            NA               0
## H.npnct23.log         H.npnct23.log            NA               0
## H.npnct24.log         H.npnct24.log            NA               0
## H.npnct25.log         H.npnct25.log            NA               0
## H.npnct27.log         H.npnct27.log            NA               0
## H.npnct28.log         H.npnct28.log            NA               0
## H.npnct29.log         H.npnct29.log            NA               0
## H.npnct31.log         H.npnct31.log            NA               0
## H.npnct32.log         H.npnct32.log            NA               0
## PubDate.year.fctr PubDate.year.fctr            NA               0
## S.has.http               S.has.http            NA               0
## S.npnct05.log         S.npnct05.log            NA               0
## S.npnct10.log         S.npnct10.log            NA               0
## S.npnct18.log         S.npnct18.log            NA               0
## S.npnct19.log         S.npnct19.log            NA               0
## S.npnct20.log         S.npnct20.log            NA               0
## S.npnct24.log         S.npnct24.log            NA               0
## S.npnct27.log         S.npnct27.log            NA               0
## S.npnct28.log         S.npnct28.log            NA               0
## S.npnct29.log         S.npnct29.log            NA               0
## S.npnct31.log         S.npnct31.log            NA               0
## S.npnct32.log         S.npnct32.log            NA               0
##                      cor.y.abs    cor.high.X freqRatio percentUnique
## S.npnct23.log     2.760321e-02 A.npnct23.log  6531.000    0.03061849
## S.npnct25.log     2.760321e-02          <NA>  6531.000    0.03061849
## A.npnct23.log     1.537569e-02 A.npnct25.log  3264.500    0.04592774
## A.npnct25.log     1.537569e-02          <NA>  3264.500    0.04592774
## H.npnct03.log     9.533020e-03          <NA>  2176.333    0.03061849
## A.npnct26.log     9.890046e-19          <NA>     0.000    0.01530925
## H.npnct26.log     9.890046e-19          <NA>     0.000    0.01530925
## S.npnct26.log     9.890046e-19          <NA>     0.000    0.01530925
## A.npnct27.log     5.547032e-03          <NA>  6531.000    0.03061849
## A.npnct11.log     5.547032e-03          <NA>  6531.000    0.03061849
## H.npnct11.log     5.547032e-03          <NA>  6531.000    0.03061849
## H.npnct22.log     5.547032e-03          <NA>  6531.000    0.03061849
## S.npnct02.log     5.547032e-03          <NA>  6531.000    0.03061849
## S.npnct11.log     5.547032e-03          <NA>  6531.000    0.03061849
## A.npnct05.log               NA          <NA>     0.000    0.01530925
## A.npnct10.log               NA          <NA>     0.000    0.01530925
## A.npnct24.log               NA          <NA>     0.000    0.01530925
## A.npnct28.log               NA          <NA>     0.000    0.01530925
## A.npnct29.log               NA          <NA>     0.000    0.01530925
## A.npnct31.log               NA          <NA>     0.000    0.01530925
## A.npnct32.log               NA          <NA>     0.000    0.01530925
## H.has.http                  NA          <NA>     0.000    0.01530925
## H.npnct10.log               NA          <NA>     0.000    0.01530925
## H.npnct18.log               NA          <NA>     0.000    0.01530925
## H.npnct19.log               NA          <NA>     0.000    0.01530925
## H.npnct20.log               NA          <NA>     0.000    0.01530925
## H.npnct23.log               NA          <NA>     0.000    0.01530925
## H.npnct24.log               NA          <NA>     0.000    0.01530925
## H.npnct25.log               NA          <NA>     0.000    0.01530925
## H.npnct27.log               NA          <NA>     0.000    0.01530925
## H.npnct28.log               NA          <NA>     0.000    0.01530925
## H.npnct29.log               NA          <NA>     0.000    0.01530925
## H.npnct31.log               NA          <NA>     0.000    0.01530925
## H.npnct32.log               NA          <NA>     0.000    0.01530925
## PubDate.year.fctr           NA          <NA>     0.000    0.01530925
## S.has.http                  NA          <NA>     0.000    0.01530925
## S.npnct05.log               NA          <NA>     0.000    0.01530925
## S.npnct10.log               NA          <NA>     0.000    0.01530925
## S.npnct18.log               NA          <NA>     0.000    0.01530925
## S.npnct19.log               NA          <NA>     0.000    0.01530925
## S.npnct20.log               NA          <NA>     0.000    0.01530925
## S.npnct24.log               NA          <NA>     0.000    0.01530925
## S.npnct27.log               NA          <NA>     0.000    0.01530925
## S.npnct28.log               NA          <NA>     0.000    0.01530925
## S.npnct29.log               NA          <NA>     0.000    0.01530925
## S.npnct31.log               NA          <NA>     0.000    0.01530925
## S.npnct32.log               NA          <NA>     0.000    0.01530925
##                   zeroVar  nzv myNearZV is.cor.y.abs.low
## S.npnct23.log       FALSE TRUE     TRUE            FALSE
## S.npnct25.log       FALSE TRUE     TRUE            FALSE
## A.npnct23.log       FALSE TRUE     TRUE            FALSE
## A.npnct25.log       FALSE TRUE     TRUE            FALSE
## H.npnct03.log       FALSE TRUE     TRUE            FALSE
## A.npnct26.log        TRUE TRUE     TRUE             TRUE
## H.npnct26.log        TRUE TRUE     TRUE             TRUE
## S.npnct26.log        TRUE TRUE     TRUE             TRUE
## A.npnct27.log       FALSE TRUE     TRUE             TRUE
## A.npnct11.log       FALSE TRUE     TRUE             TRUE
## H.npnct11.log       FALSE TRUE     TRUE             TRUE
## H.npnct22.log       FALSE TRUE     TRUE             TRUE
## S.npnct02.log       FALSE TRUE     TRUE             TRUE
## S.npnct11.log       FALSE TRUE     TRUE             TRUE
## A.npnct05.log        TRUE TRUE     TRUE               NA
## A.npnct10.log        TRUE TRUE     TRUE               NA
## A.npnct24.log        TRUE TRUE     TRUE               NA
## A.npnct28.log        TRUE TRUE     TRUE               NA
## A.npnct29.log        TRUE TRUE     TRUE               NA
## A.npnct31.log        TRUE TRUE     TRUE               NA
## A.npnct32.log        TRUE TRUE     TRUE               NA
## H.has.http           TRUE TRUE     TRUE               NA
## H.npnct10.log        TRUE TRUE     TRUE               NA
## H.npnct18.log        TRUE TRUE     TRUE               NA
## H.npnct19.log        TRUE TRUE     TRUE               NA
## H.npnct20.log        TRUE TRUE     TRUE               NA
## H.npnct23.log        TRUE TRUE     TRUE               NA
## H.npnct24.log        TRUE TRUE     TRUE               NA
## H.npnct25.log        TRUE TRUE     TRUE               NA
## H.npnct27.log        TRUE TRUE     TRUE               NA
## H.npnct28.log        TRUE TRUE     TRUE               NA
## H.npnct29.log        TRUE TRUE     TRUE               NA
## H.npnct31.log        TRUE TRUE     TRUE               NA
## H.npnct32.log        TRUE TRUE     TRUE               NA
## PubDate.year.fctr    TRUE TRUE     TRUE               NA
## S.has.http           TRUE TRUE     TRUE               NA
## S.npnct05.log        TRUE TRUE     TRUE               NA
## S.npnct10.log        TRUE TRUE     TRUE               NA
## S.npnct18.log        TRUE TRUE     TRUE               NA
## S.npnct19.log        TRUE TRUE     TRUE               NA
## S.npnct20.log        TRUE TRUE     TRUE               NA
## S.npnct24.log        TRUE TRUE     TRUE               NA
## S.npnct27.log        TRUE TRUE     TRUE               NA
## S.npnct28.log        TRUE TRUE     TRUE               NA
## S.npnct29.log        TRUE TRUE     TRUE               NA
## S.npnct31.log        TRUE TRUE     TRUE               NA
## S.npnct32.log        TRUE TRUE     TRUE               NA
```

```r
glb_entity_df <- glb_entity_df[, setdiff(names(glb_entity_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor     bgn     end elapsed
## 7         select.features          4          0 147.378 188.221  40.843
## 8 partition.data.training          5          0 188.222      NA      NA
```

## Step `5.0: partition data training`

```r
if (all(is.na(glb_newent_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnent_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newent_df) * 1.1 / nrow(glb_trnent_df)))
    glb_fitent_df <- glb_trnent_df[split, ] 
    glb_OOBent_df <- glb_trnent_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitent_df <- glb_trnent_df; glb_OOBent_df <- glb_newent_df
}
```

```
## Loading required package: caTools
```

```r
if (!is.null(glb_max_fitent_obs) && (nrow(glb_fitent_df) > glb_max_fitent_obs)) {
    warning("glb_fitent_df restricted to glb_max_fitent_obs: ", 
            format(glb_max_fitent_obs, big.mark=","))
    org_fitent_df <- glb_fitent_df
    glb_fitent_df <- 
        org_fitent_df[split <- sample.split(org_fitent_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitent_obs), ]
    org_fitent_df <- NULL
}

sav_entity_df <- glb_entity_df
glb_entity_df$.lcn <- ""
glb_entity_df[glb_entity_df[, glb_id_vars] %in% 
              glb_fitent_df[, glb_id_vars], ".lcn"] <- "Fit"
glb_entity_df[glb_entity_df[, glb_id_vars] %in% 
              glb_OOBent_df[, glb_id_vars], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
dsp_class_dstrb(glb_entity_df, ".lcn", glb_rsp_var_raw)
```

```
##     Popular.0 Popular.1 Popular.NA
##            NA        NA       1870
## Fit      3726       749         NA
## OOB      1713       344         NA
##     Popular.0 Popular.1 Popular.NA
##            NA        NA          1
## Fit 0.8326257 0.1673743         NA
## OOB 0.8327662 0.1672338         NA
```

```r
newent_ctgry_df <- mycreate_sqlxtab_df(subset(glb_entity_df, .src == "Test"), 
                                       "myCategory")
OOBent_ctgry_df <- mycreate_sqlxtab_df(subset(glb_entity_df, .lcn == "OOB"), 
                                       "myCategory")
glb_ctgry_df <- merge(newent_ctgry_df, OOBent_ctgry_df, by="myCategory", all=TRUE, 
                      suffixes=c(".Tst", ".OOB"))
glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
```

```
##                              myCategory .n.Tst .n.OOB .freqRatio.Tst
## 1                                    ##    338    407    0.180748663
## 6        Business#Business Day#Dealbook    304    312    0.162566845
## 10                        Culture#Arts#    244    225    0.130481283
## 15                        OpEd#Opinion#    164    154    0.087700535
## 9                  Business#Technology#    113    114    0.060427807
## 20                             TStyle##    105    221    0.056149733
## 5                       #U.S.#Education     90     93    0.048128342
## 13                 Metro#N.Y. / Region#     67     60    0.035828877
## 18                         Styles#U.S.#     62     54    0.033155080
## 16                      Science#Health#     57     66    0.030481283
## 12           Foreign#World#Asia Pacific     56     61    0.029946524
## 2                          #Multimedia#     52     42    0.027807487
## 11                       Foreign#World#     47     47    0.025133690
## 7  Business#Business Day#Small Business     42     45    0.022459893
## 8            Business#Crosswords/Games#     42     40    0.022459893
## 19                       Travel#Travel#     35     31    0.018716578
## 3              #Opinion#Room For Debate     24     21    0.012834225
## 17                      Styles##Fashion     15     41    0.008021390
## 4            #Opinion#The Public Editor     10     10    0.005347594
## 14                              myOther      3     13    0.001604278
##    .freqRatio.OOB
## 1     0.197860963
## 6     0.151677200
## 10    0.109382596
## 15    0.074866310
## 9     0.055420515
## 20    0.107438017
## 5     0.045211473
## 13    0.029168692
## 18    0.026251823
## 16    0.032085561
## 12    0.029654837
## 2     0.020418085
## 11    0.022848809
## 7     0.021876519
## 8     0.019445795
## 19    0.015070491
## 3     0.010209042
## 17    0.019931940
## 4     0.004861449
## 14    0.006319883
```

```r
# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 194  11
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_vars) && glb_id_vars != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_vars, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                        id exclude.as.feat rsp_var
## Popular.fctr Popular.fctr            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var))
```

```
##                        id      cor.y exclude.as.feat  cor.y.abs cor.high.X
## Popular           Popular 1.00000000            TRUE 1.00000000       <NA>
## UniqueID         UniqueID 0.01182492            TRUE 0.01182492       <NA>
## Popular.fctr Popular.fctr         NA            TRUE         NA       <NA>
##              freqRatio percentUnique zeroVar   nzv myNearZV
## Popular       4.976212    0.03061849   FALSE FALSE    FALSE
## UniqueID      1.000000  100.00000000   FALSE FALSE    FALSE
## Popular.fctr        NA            NA      NA    NA       NA
##              is.cor.y.abs.low rsp_var_raw id_var rsp_var
## Popular                 FALSE        TRUE     NA      NA
## UniqueID                FALSE       FALSE   TRUE      NA
## Popular.fctr               NA          NA     NA    TRUE
```

```r
print("glb_feats_df vs. glb_entity_df: "); 
```

```
## [1] "glb_feats_df vs. glb_entity_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_entity_df)))
```

```
##  [1] "S.npnct23.log"     "S.npnct25.log"     "A.npnct23.log"    
##  [4] "A.npnct25.log"     "H.npnct03.log"     "A.npnct26.log"    
##  [7] "H.npnct26.log"     "S.npnct26.log"     "A.npnct27.log"    
## [10] "A.npnct11.log"     "H.npnct11.log"     "H.npnct22.log"    
## [13] "S.npnct02.log"     "S.npnct11.log"     "A.npnct05.log"    
## [16] "A.npnct10.log"     "A.npnct24.log"     "A.npnct28.log"    
## [19] "A.npnct29.log"     "A.npnct31.log"     "A.npnct32.log"    
## [22] "H.has.http"        "H.npnct10.log"     "H.npnct18.log"    
## [25] "H.npnct19.log"     "H.npnct20.log"     "H.npnct23.log"    
## [28] "H.npnct24.log"     "H.npnct25.log"     "H.npnct27.log"    
## [31] "H.npnct28.log"     "H.npnct29.log"     "H.npnct31.log"    
## [34] "H.npnct32.log"     "PubDate.year.fctr" "S.has.http"       
## [37] "S.npnct05.log"     "S.npnct10.log"     "S.npnct18.log"    
## [40] "S.npnct19.log"     "S.npnct20.log"     "S.npnct24.log"    
## [43] "S.npnct27.log"     "S.npnct28.log"     "S.npnct29.log"    
## [46] "S.npnct31.log"     "S.npnct32.log"
```

```r
print("glb_entity_df vs. glb_feats_df: "); 
```

```
## [1] "glb_entity_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_entity_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_entity_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_entity_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_entity_df: "); print(dim(glb_entity_df))
```

```
## [1] "glb_entity_df: "
```

```
## [1] 8402  158
```

```r
print("glb_trnent_df: "); print(dim(glb_trnent_df))
```

```
## [1] "glb_trnent_df: "
```

```
## [1] 6532  204
```

```r
print("glb_fitent_df: "); print(dim(glb_fitent_df))
```

```
## [1] "glb_fitent_df: "
```

```
## [1] 4475  204
```

```r
print("glb_OOBent_df: "); print(dim(glb_OOBent_df))
```

```
## [1] "glb_OOBent_df: "
```

```
## [1] 2057  204
```

```r
print("glb_newent_df: "); print(dim(glb_newent_df))
```

```
## [1] "glb_newent_df: "
```

```
## [1] 1870  204
```

```r
# sav_entity_df <- glb_entity_df
# glb_entity_df <- sav_entity_df
# # Does not handle NULL or length(glb_id_vars) > 1
# glb_entity_df$.src.trn <- 0
# glb_entity_df[glb_entity_df[, glb_id_vars] %in% glb_trnent_df[, glb_id_vars], 
#                 ".src.trn"] <- 1 
# glb_entity_df$.src.fit <- 0
# glb_entity_df[glb_entity_df[, glb_id_vars] %in% glb_fitent_df[, glb_id_vars], 
#                 ".src.fit"] <- 1 
# glb_entity_df$.src.OOB <- 0
# glb_entity_df[glb_entity_df[, glb_id_vars] %in% glb_OOBent_df[, glb_id_vars], 
#                 ".src.OOB"] <- 1 
# glb_entity_df$.src.new <- 0
# glb_entity_df[glb_entity_df[, glb_id_vars] %in% glb_newent_df[, glb_id_vars], 
#                 ".src.new"] <- 1 
# #print(unique(glb_entity_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_entity_df <- glb_entity_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_entity_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_entity_df, #glb_trnent_df, glb_fitent_df, glb_OOBent_df, glb_newent_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_entity_df))
#     stop("glb_entity_df r/w not working")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                     label step_major step_minor     bgn     end elapsed
## 8 partition.data.training          5          0 188.222 189.589   1.367
## 9              fit.models          6          0 189.590      NA      NA
```

## Step `6.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_entity_df), 
#                      grep("^.src", names(glb_entity_df), value=TRUE))
# glb_trnent_df <- glb_entity_df[glb_entity_df$.src.trn == 1, keep_cols]
# glb_fitent_df <- glb_entity_df[glb_entity_df$.src.fit == 1, keep_cols]
# glb_OOBent_df <- glb_entity_df[glb_entity_df$.src.OOB == 1, keep_cols]
# glb_newent_df <- glb_entity_df[glb_entity_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitent_df[, glb_rsp_var])) < 2))
    stop("glb_fitent_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitent_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_var <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[1, "id"]
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_var != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_var, "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_var, " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitent_df, OOB_df=glb_OOBent_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df)
```

```
## [1] "fitting model: MFO.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] N Y
## Levels: N Y
## [1] "unique.prob:"
## y
##         N         Y 
## 0.8326257 0.1673743 
## [1] "MFO.val:"
## [1] "N"
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## [1] "entr MFO.Classifier$predict"
## [1] "exit MFO.Classifier$predict"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8326257 0.1673743
## 2 0.8326257 0.1673743
## 3 0.8326257 0.1673743
## 4 0.8326257 0.1673743
## 5 0.8326257 0.1673743
## 6 0.8326257 0.1673743
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.MFO.myMFO_classfr.N
## 1            N                                     3726
## 2            Y                                      749
##          Prediction
## Reference    N    Y
##         N 3726    0
##         Y  749    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.326257e-01   0.000000e+00   8.213602e-01   8.434553e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   5.097571e-01  1.800616e-164 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "entr MFO.Classifier$predict"
## [1] "exit MFO.Classifier$predict"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8326257 0.1673743
## 2 0.8326257 0.1673743
## 3 0.8326257 0.1673743
## 4 0.8326257 0.1673743
## 5 0.8326257 0.1673743
## 6 0.8326257 0.1673743
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.MFO.myMFO_classfr.N
## 1            N                                     1713
## 2            Y                                      344
##          Prediction
## Reference    N    Y
##         N 1713    0
##         Y  344    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.327662e-01   0.000000e+00   8.159247e-01   8.486533e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.143944e-01   2.337097e-76 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                       0.87                 0.003         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8326257
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitent_df, OOB_df=glb_OOBent_df)
```

```
## [1] "fitting model: Random.myrandom_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      table      numeric  
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "in Random.Classifier$prob"
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.2867534
## 3        0.2 0.1735751
## 4        0.3 0.1735751
## 5        0.4 0.1735751
## 6        0.5 0.1735751
## 7        0.6 0.1735751
## 8        0.7 0.1735751
## 9        0.8 0.1735751
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.Y
## 1            N                                           3726
## 2            Y                                            749
##          Prediction
## Reference    N    Y
##         N    0 3726
##         Y    0  749
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.1673743      0.0000000      0.1565447      0.1786398      0.8326257 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.0000000 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in Random.Classifier$prob"
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.2865473
## 3        0.2 0.1547278
## 4        0.3 0.1547278
## 5        0.4 0.1547278
## 6        0.5 0.1547278
## 7        0.6 0.1547278
## 8        0.7 0.1547278
## 9        0.8 0.1547278
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.Y
## 1            N                                           1713
## 2            Y                                            344
##          Prediction
## Reference    N    Y
##         N    0 1713
##         Y    0  344
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.1672338      0.0000000      0.1513467      0.1840753      0.8327662 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.0000000 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.341                 0.002   0.5007516
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.1       0.2867534        0.1673743
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.1565447             0.1786398             0   0.4909227
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.2865473        0.1672338
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.1513467             0.1840753             0
```

```r
# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: A.nuppr.log"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0 on full training set
```

```
## Loading required package: rpart.plot
```

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##   CP nsplit rel error
## 1  0      0         1
## 
## Node number 1: 4475 observations
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 4475 749 N (0.8326257 0.1673743) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1            N                                        3726
## 2            Y                                         749
##          Prediction
## Reference    N    Y
##         N 3726    0
##         Y  749    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.326257e-01   0.000000e+00   8.213602e-01   8.434553e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   5.097571e-01  1.800616e-164 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1            N                                        1713
## 2            Y                                         344
##          Prediction
## Reference    N    Y
##         N 1713    0
##         Y  344    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.327662e-01   0.000000e+00   8.159247e-01   8.486533e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.143944e-01   2.337097e-76 
##               model_id model_method       feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart A.nuppr.log               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.688                 0.053         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8326257
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: A.nuppr.log"
## Fitting cp = 0 on full training set
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##   CP nsplit rel error
## 1  0      0         1
## 
## Node number 1: 4475 observations
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 4475 749 N (0.8326257 0.1673743) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3726
## 2            Y                                              749
##          Prediction
## Reference    N    Y
##         N 3726    0
##         Y  749    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.326257e-01   0.000000e+00   8.213602e-01   8.434553e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   5.097571e-01  1.800616e-164 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1713
## 2            Y                                              344
##          Prediction
## Reference    N    Y
##         N 1713    0
##         Y  344    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.327662e-01   0.000000e+00   8.159247e-01   8.486533e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.143944e-01   2.337097e-76 
##                    model_id model_method       feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart A.nuppr.log               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.607                 0.056         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8326257
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: A.nuppr.log"
## Aggregating results
## Fitting final model on full training set
```

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##   CP nsplit rel error
## 1  0      0         1
## 
## Node number 1: 4475 observations
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 4475 749 N (0.8326257 0.1673743) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.rpart.N
## 1            N                                   3726
## 2            Y                                    749
##          Prediction
## Reference    N    Y
##         N 3726    0
##         Y  749    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.326257e-01   0.000000e+00   8.213602e-01   8.434553e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   5.097571e-01  1.800616e-164 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.rpart.N
## 1            N                                   1713
## 2            Y                                    344
##          Prediction
## Reference    N    Y
##         N 1713    0
##         Y  344    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.327662e-01   0.000000e+00   8.159247e-01   8.486533e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.143944e-01   2.337097e-76 
##          model_id model_method       feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart A.nuppr.log               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.268                 0.056         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8326258
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0002791548               0
```

```r
# Used to compare vs. Interactions.High.cor.Y 
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.glm"
## [1] "    indep_vars: A.nuppr.log"
## Aggregating results
## Fitting final model on full training set
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-6.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-7.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-8.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-9.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.3585  -0.6318  -0.4867  -0.3464   2.6336  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  0.41620    0.11470   3.628 0.000285 ***
## A.nuppr.log -1.38947    0.08027 -17.310  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 3710.6  on 4473  degrees of freedom
## AIC: 3714.6
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-10.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.3499729
## 3        0.2 0.3986014
## 4        0.3 0.3121547
## 5        0.4 0.0000000
## 6        0.5 0.0000000
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-11.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 2872
## 2            Y                                  350
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  854
## 2                                  399
##          Prediction
## Reference    N    Y
##         N 2872  854
##         Y  350  399
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.309497e-01   2.392074e-01   7.176970e-01   7.439004e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.280095e-47 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-12.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.3485577
## 3        0.2 0.3880266
## 4        0.3 0.3465046
## 5        0.4 0.0000000
## 6        0.5 0.0000000
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-13.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1330
## 2            Y                                  169
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  383
## 2                                  175
##          Prediction
## Reference    N    Y
##         N 1330  383
##         Y  169  175
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.316480e-01   2.283681e-01   7.119353e-01   7.506985e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.236001e-19 
##        model_id model_method       feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm A.nuppr.log               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.224                 0.079   0.7073742
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.3986014        0.8324022
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.717697             0.7439004 -0.0004459345    0.710206
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3880266         0.731648
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7119353             0.7506985     0.2283681    3714.601
##   max.AccuracySD.fit max.KappaSD.fit
## 1        6.48833e-05    0.0007723812
```

```r
# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_var, paste(max_cor_y_x_var, int_feats, sep=":"))       
    } else { indep_vars_vctr <- union(max_cor_y_x_var, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.glm"
## [1] "    indep_vars: A.nuppr.log, A.nuppr.log:A.npnct21.log, A.nuppr.log:H.npnct09.log, A.nuppr.log:H.npnct17.log, A.nuppr.log:S.can.log, A.nuppr.log:S.npnct01.log, A.nuppr.log:A.npnct23.log, A.nuppr.log:S.make.log, A.nuppr.log:A.npnct25.log, A.nuppr.log:S.npnct07.log, A.nuppr.log:A.npnct19.log, A.nuppr.log:S.npnct03.log, A.nuppr.log:A.npnct18.log, A.nuppr.log:A.npnct20.log, A.nuppr.log:S.has.year.colon, A.nuppr.log:S.npnct22.log, A.nuppr.log:S.presid.log, A.nuppr.log:S.npnct06.log, A.nuppr.log:A.npnct02.log, A.nuppr.log:S.take.log, A.nuppr.log:S.new.log, A.nuppr.log:S.npnct13.log, A.nuppr.log:S.npnct30.log, A.nuppr.log:S.day.log, A.nuppr.log:S.show.log, A.nuppr.log:S.report.log, A.nuppr.log:S.year.log, A.nuppr.log:S.share.log, A.nuppr.log:S.compani.log, A.nuppr.log:A.npnct14.log, A.nuppr.log:S.first.log, A.nuppr.log:S.time.log, A.nuppr.log:S.articl.log, A.nuppr.log:S.will.log, A.nuppr.log:S.newyork.log, A.nuppr.log:S.npnct04.log, A.nuppr.log:H.npnct15.log, A.nuppr.log:S.intern.log, A.nuppr.log:S.npnct16.log, A.nuppr.log:A.intern.log, A.nuppr.log:H.week.log, A.nuppr.log:S.fashion.log, A.nuppr.log:S.week.log, A.nuppr.log:S.npnct12.log, A.nuppr.log:S.ndgts.log, A.nuppr.log:H.nuppr.log, A.nuppr.log:A.nchrs.log, A.nuppr.log:A.nwrds.log, A.nuppr.log:S.nchrs.log, A.nuppr.log:S.nuppr.log"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-14.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-15.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-16.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7101  -0.6726  -0.3775  -0.0964   3.1776  
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                    -5.117e-01  3.132e-01  -1.634 0.102323    
## A.nuppr.log                     1.619e+00  7.353e-01   2.202 0.027681 *  
## `A.nuppr.log:A.npnct21.log`     5.276e-01  1.410e-01   3.741 0.000183 ***
## `A.nuppr.log:H.npnct09.log`     6.130e-01  3.533e-01   1.735 0.082723 .  
## `A.nuppr.log:H.npnct17.log`     7.557e-01  2.601e-01   2.905 0.003674 ** 
## `A.nuppr.log:S.can.log`         1.246e-01  2.420e-01   0.515 0.606429    
## `A.nuppr.log:S.npnct01.log`     8.649e-01  4.669e-01   1.852 0.063960 .  
## `A.nuppr.log:A.npnct23.log`    -9.126e+00  3.927e+03  -0.002 0.998146    
## `A.nuppr.log:S.make.log`        1.855e-01  2.185e-01   0.849 0.396110    
## `A.nuppr.log:A.npnct25.log`            NA         NA      NA       NA    
## `A.nuppr.log:S.npnct07.log`    -3.439e+01  5.941e+03  -0.006 0.995382    
## `A.nuppr.log:A.npnct19.log`     9.215e+00  1.801e+05   0.000 0.999959    
## `A.nuppr.log:S.npnct03.log`    -1.172e+01  2.974e+03  -0.004 0.996855    
## `A.nuppr.log:A.npnct18.log`    -1.628e+01  1.126e+05   0.000 0.999885    
## `A.nuppr.log:A.npnct20.log`            NA         NA      NA       NA    
## `A.nuppr.log:S.has.year.colon` -1.105e+01  1.621e+03  -0.007 0.994559    
## `A.nuppr.log:S.npnct22.log`    -1.522e+01  2.877e+03  -0.005 0.995779    
## `A.nuppr.log:S.presid.log`     -8.079e-02  2.072e-01  -0.390 0.696686    
## `A.nuppr.log:S.npnct06.log`    -6.995e-01  8.398e-01  -0.833 0.404904    
## `A.nuppr.log:A.npnct02.log`    -1.135e+01  1.495e+04  -0.001 0.999394    
## `A.nuppr.log:S.take.log`       -4.029e-01  3.136e-01  -1.285 0.198814    
## `A.nuppr.log:S.new.log`        -1.961e-01  1.713e-01  -1.145 0.252311    
## `A.nuppr.log:S.npnct13.log`    -4.416e-02  1.006e-01  -0.439 0.660554    
## `A.nuppr.log:S.npnct30.log`    -8.502e+00  6.212e+02  -0.014 0.989081    
## `A.nuppr.log:S.day.log`        -5.125e-01  3.470e-01  -1.477 0.139701    
## `A.nuppr.log:S.show.log`       -7.227e-01  3.737e-01  -1.934 0.053096 .  
## `A.nuppr.log:S.report.log`     -8.363e-01  3.485e-01  -2.400 0.016414 *  
## `A.nuppr.log:S.year.log`       -1.535e-01  2.436e-01  -0.630 0.528568    
## `A.nuppr.log:S.share.log`      -8.884e-01  3.811e-01  -2.331 0.019755 *  
## `A.nuppr.log:S.compani.log`    -6.194e-01  2.586e-01  -2.396 0.016594 *  
## `A.nuppr.log:A.npnct14.log`     7.406e-01  1.082e-01   6.846 7.59e-12 ***
## `A.nuppr.log:S.first.log`      -3.713e-01  3.040e-01  -1.221 0.221974    
## `A.nuppr.log:S.time.log`       -2.135e-01  1.994e-01  -1.071 0.284092    
## `A.nuppr.log:S.articl.log`     -1.510e+00  4.841e-01  -3.120 0.001808 ** 
## `A.nuppr.log:S.will.log`       -5.920e-01  1.946e-01  -3.042 0.002354 ** 
## `A.nuppr.log:S.newyork.log`     4.326e-01  1.904e-01   2.272 0.023084 *  
## `A.nuppr.log:S.npnct04.log`    -1.062e+00  4.332e-01  -2.451 0.014232 *  
## `A.nuppr.log:H.npnct15.log`    -2.727e+01  8.975e+02  -0.030 0.975759    
## `A.nuppr.log:S.intern.log`     -1.226e+00  5.691e-01  -2.153 0.031280 *  
## `A.nuppr.log:S.npnct16.log`    -2.997e-01  2.168e-01  -1.382 0.166881    
## `A.nuppr.log:A.intern.log`             NA         NA      NA       NA    
## `A.nuppr.log:H.week.log`       -1.488e+00  6.674e-01  -2.230 0.025728 *  
## `A.nuppr.log:S.fashion.log`    -3.015e+01  7.075e+02  -0.043 0.966006    
## `A.nuppr.log:S.week.log`       -6.176e-01  2.725e-01  -2.266 0.023421 *  
## `A.nuppr.log:S.npnct12.log`    -2.600e-02  6.739e-02  -0.386 0.699632    
## `A.nuppr.log:S.ndgts.log`      -2.579e-01  7.543e-02  -3.419 0.000629 ***
## `A.nuppr.log:H.nuppr.log`      -4.571e-01  9.393e-02  -4.866 1.14e-06 ***
## `A.nuppr.log:A.nchrs.log`       5.276e-02  2.794e+00   0.019 0.984933    
## `A.nuppr.log:A.nwrds.log`      -5.774e-01  2.906e-01  -1.987 0.046950 *  
## `A.nuppr.log:S.nchrs.log`       4.078e-02  2.787e+00   0.015 0.988328    
## `A.nuppr.log:S.nuppr.log`      -4.910e-01  1.868e-01  -2.628 0.008587 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 3328.7  on 4427  degrees of freedom
## AIC: 3424.7
## 
## Number of Fisher Scoring iterations: 18
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-17.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-18.png) 

```
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.39842567
## 3        0.2 0.45857260
## 4        0.3 0.46666667
## 5        0.4 0.36278195
## 6        0.5 0.13480392
## 7        0.6 0.01055409
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3164
## 2            Y                                            350
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            562
## 2                                            399
##          Prediction
## Reference    N    Y
##         N 3164  562
##         Y  350  399
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.962011e-01   3.430846e-01   7.840956e-01   8.079188e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.810100e-12 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-19.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-20.png) 

```
##    threshold    f.score
## 1        0.0 0.28654727
## 2        0.1 0.39014647
## 3        0.2 0.44770642
## 4        0.3 0.45210728
## 5        0.4 0.33877551
## 6        0.5 0.11320755
## 7        0.6 0.03418803
## 8        0.7 0.01156069
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-21.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1451
## 2            Y                                            167
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            262
## 2                                            177
##          Prediction
## Reference    N    Y
##         N 1451  262
##         Y  167  177
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.914439e-01   3.256506e-01   7.732343e-01   8.088189e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   9.999995e-01   5.669267e-06 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              feats
## 1 A.nuppr.log, A.nuppr.log:A.npnct21.log, A.nuppr.log:H.npnct09.log, A.nuppr.log:H.npnct17.log, A.nuppr.log:S.can.log, A.nuppr.log:S.npnct01.log, A.nuppr.log:A.npnct23.log, A.nuppr.log:S.make.log, A.nuppr.log:A.npnct25.log, A.nuppr.log:S.npnct07.log, A.nuppr.log:A.npnct19.log, A.nuppr.log:S.npnct03.log, A.nuppr.log:A.npnct18.log, A.nuppr.log:A.npnct20.log, A.nuppr.log:S.has.year.colon, A.nuppr.log:S.npnct22.log, A.nuppr.log:S.presid.log, A.nuppr.log:S.npnct06.log, A.nuppr.log:A.npnct02.log, A.nuppr.log:S.take.log, A.nuppr.log:S.new.log, A.nuppr.log:S.npnct13.log, A.nuppr.log:S.npnct30.log, A.nuppr.log:S.day.log, A.nuppr.log:S.show.log, A.nuppr.log:S.report.log, A.nuppr.log:S.year.log, A.nuppr.log:S.share.log, A.nuppr.log:S.compani.log, A.nuppr.log:A.npnct14.log, A.nuppr.log:S.first.log, A.nuppr.log:S.time.log, A.nuppr.log:S.articl.log, A.nuppr.log:S.will.log, A.nuppr.log:S.newyork.log, A.nuppr.log:S.npnct04.log, A.nuppr.log:H.npnct15.log, A.nuppr.log:S.intern.log, A.nuppr.log:S.npnct16.log, A.nuppr.log:A.intern.log, A.nuppr.log:H.week.log, A.nuppr.log:S.fashion.log, A.nuppr.log:S.week.log, A.nuppr.log:S.npnct12.log, A.nuppr.log:S.ndgts.log, A.nuppr.log:H.nuppr.log, A.nuppr.log:A.nchrs.log, A.nuppr.log:A.nwrds.log, A.nuppr.log:S.nchrs.log, A.nuppr.log:S.nuppr.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      3.036                 1.064
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7936512                    0.3       0.4666667        0.8420117
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7840956             0.8079188     0.1154034   0.7739949
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.4521073        0.7914439
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7732343             0.8088189     0.3256506    3424.717
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002095066      0.02849274
```

```r
# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.glm"
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, A.npnct21.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, .rnorm, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.npnct20.log, S.has.year.colon, S.npnct22.log, H.npnct02.log, S.presid.log, S.npnct15.log, S.npnct06.log, H.npnct14.log, S.take.log, PubDate.minute.fctr, S.new.log, S.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, S.day.log, H.X2014.log, S.show.log, A.npnct14.log, S.report.log, S.year.log, H.npnct04.log, S.share.log, S.compani.log, H.new.log, S.first.log, S.time.log, H.newyork.log, S.articl.log, S.will.log, H.npnct15.log, S.newyork.log, H.day.log, S.npnct04.log, H.today.log, H.report.log, S.npnct16.log, S.intern.log, H.daili.log, H.week.log, H.npnct16.log, S.fashion.log, S.week.log, H.npnct30.log, S.npnct12.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, A.nchrs.log, A.nwrds.log, A.nwrds.unq.log, S.nuppr.log"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: not plotting observations with leverage one:
##   1143, 2501, 4105, 4408
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-22.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-23.png) 

```
## Warning: not plotting observations with leverage one:
##   1143, 2501, 4105, 4408
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-24.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.7423  -0.3140  -0.1356   0.0000   3.5693  
## 
## Coefficients: (4 not defined because of singularities)
##                                                         Estimate
## (Intercept)                                           -4.162e+00
## WordCount.log                                          1.111e+00
## `PubDate.hour.fctr(7.67,15.3]`                         7.729e-02
## `PubDate.hour.fctr(15.3,23]`                           2.345e-01
## H.npnct21.log                                          1.495e+00
## PubDate.wkend                                         -2.749e-01
## A.npnct21.log                                          1.419e+00
## H.npnct09.log                                          2.084e+00
## PubDate.last10.log                                     2.334e-01
## PubDate.last1.log                                     -4.732e-02
## S.npnct01.log                                          1.839e+00
## S.can.log                                             -7.366e-01
## H.npnct17.log                                          1.009e+00
## H.has.ebola                                           -3.894e-01
## S.make.log                                            -3.826e-01
## H.npnct01.log                                         -1.295e+00
## H.npnct12.log                                          4.241e-01
## `myCategory.fctrForeign#World#Asia Pacific`           -4.121e+00
## `myCategory.fctr#Multimedia#`                         -4.377e+00
## `myCategory.fctrCulture#Arts#`                        -2.823e+00
## `myCategory.fctrBusiness#Business Day#Dealbook`       -2.412e+00
## myCategory.fctrmyOther                                -2.022e+01
## `myCategory.fctrBusiness#Technology#`                 -1.841e+00
## `myCategory.fctrBusiness#Crosswords/Games#`            8.494e-01
## `myCategory.fctrTStyle##`                             -4.201e+00
## `myCategory.fctrForeign#World#`                       -1.826e+01
## `myCategory.fctrOpEd#Opinion#`                         6.859e-01
## `myCategory.fctrStyles##Fashion`                      -1.970e+01
## `myCategory.fctr#Opinion#Room For Debate`             -5.673e+00
## `myCategory.fctr#U.S.#Education`                      -2.090e+01
## `myCategory.fctr##`                                   -2.722e+00
## `myCategory.fctrMetro#N.Y. / Region#`                 -1.816e+00
## `myCategory.fctrBusiness#Business Day#Small Business` -4.537e+00
## `myCategory.fctrStyles#U.S.#`                         -4.729e-01
## `myCategory.fctrTravel#Travel#`                       -4.027e+00
## `myCategory.fctr#Opinion#The Public Editor`            1.166e+00
## S.state.log                                           -1.440e+01
## A.state.log                                            1.559e+01
## S.one.log                                              2.290e+01
## A.one.log                                             -2.329e+01
## A.said.log                                             8.864e-01
## S.said.log                                                    NA
## A.npnct17.log                                         -3.040e-01
## S.npnct17.log                                                 NA
## S.npnct08.log                                          1.321e+01
## A.npnct08.log                                                 NA
## S.npnct09.log                                         -1.164e+01
## A.npnct09.log                                                 NA
## PubDate.last100.log                                    1.464e-02
## .rnorm                                                -8.169e-02
## H.npnct05.log                                         -2.430e+01
## `PubDate.date.fctr(7,13]`                             -4.525e-02
## `PubDate.date.fctr(13,19]`                            -1.459e-01
## `PubDate.date.fctr(19,25]`                            -9.055e-02
## `PubDate.date.fctr(25,31]`                             1.354e-01
## `PubDate.second.fctr(14.8,29.5]`                       9.117e-02
## `PubDate.second.fctr(29.5,44.2]`                      -1.879e-02
## `PubDate.second.fctr(44.2,59.1]`                      -2.889e-01
## H.npnct07.log                                          1.974e-01
## S.npnct07.log                                         -2.552e+01
## S.npnct03.log                                         -2.763e+01
## A.npnct19.log                                         -2.245e+01
## H.npnct13.log                                          3.489e-01
## A.npnct20.log                                         -2.505e+00
## S.has.year.colon                                      -1.268e+01
## S.npnct22.log                                         -2.393e+01
## H.npnct02.log                                         -1.736e+01
## S.presid.log                                           4.844e-01
## S.npnct15.log                                          9.482e-01
## S.npnct06.log                                          1.060e-01
## H.npnct14.log                                         -2.168e-01
## S.take.log                                            -6.489e-01
## `PubDate.minute.fctr(14.8,29.5]`                      -1.479e-01
## `PubDate.minute.fctr(29.5,44.2]`                      -2.149e-01
## `PubDate.minute.fctr(44.2,59.1]`                       5.112e-02
## S.new.log                                              2.066e-02
## S.npnct13.log                                         -1.672e-01
## PubDate.wkday.fctr1                                   -5.211e-01
## PubDate.wkday.fctr2                                   -1.131e+00
## PubDate.wkday.fctr3                                   -7.598e-01
## PubDate.wkday.fctr4                                   -9.757e-01
## PubDate.wkday.fctr5                                   -8.554e-01
## PubDate.wkday.fctr6                                   -1.271e+00
## S.npnct30.log                                         -1.448e+01
## S.day.log                                             -1.962e-01
## H.X2014.log                                           -1.043e+00
## S.show.log                                            -6.364e-01
## A.npnct14.log                                          9.378e-01
## S.report.log                                          -1.302e+00
## S.year.log                                            -3.860e-01
## H.npnct04.log                                         -1.891e+00
## S.share.log                                           -9.829e-01
## S.compani.log                                         -4.270e-01
## H.new.log                                             -8.796e-01
## S.first.log                                           -1.557e-01
## S.time.log                                            -2.970e-01
## H.newyork.log                                          6.233e-02
## S.articl.log                                          -1.780e-01
## S.will.log                                            -5.102e-01
## H.npnct15.log                                         -2.035e+01
## S.newyork.log                                          1.120e+00
## H.day.log                                             -1.145e+00
## S.npnct04.log                                         -1.109e+00
## H.today.log                                           -3.482e+00
## H.report.log                                          -7.512e-01
## S.npnct16.log                                          4.426e-01
## S.intern.log                                          -9.754e-01
## H.daili.log                                           -2.152e+01
## H.week.log                                            -6.268e-01
## H.npnct16.log                                         -2.383e-01
## S.fashion.log                                         -2.097e+01
## S.week.log                                            -2.515e-01
## H.npnct30.log                                         -1.406e-01
## S.npnct12.log                                         -1.611e-01
## H.ndgts.log                                            4.666e-01
## S.ndgts.log                                           -3.408e-01
## H.nuppr.log                                            1.244e+00
## H.nchrs.log                                           -9.101e-01
## H.nwrds.log                                           -8.080e-01
## A.nchrs.log                                            2.336e-01
## A.nwrds.log                                            8.562e-01
## A.nwrds.unq.log                                       -1.601e+00
## S.nuppr.log                                           -6.715e-01
##                                                       Std. Error z value
## (Intercept)                                            2.122e+00  -1.962
## WordCount.log                                          9.018e-02  12.319
## `PubDate.hour.fctr(7.67,15.3]`                         2.472e-01   0.313
## `PubDate.hour.fctr(15.3,23]`                           2.519e-01   0.931
## H.npnct21.log                                          3.165e-01   4.725
## PubDate.wkend                                          4.481e-01  -0.613
## A.npnct21.log                                          3.286e-01   4.317
## H.npnct09.log                                          7.224e-01   2.885
## PubDate.last10.log                                     1.255e-01   1.860
## PubDate.last1.log                                      4.384e-02  -1.080
## S.npnct01.log                                          1.745e+00   1.054
## S.can.log                                              4.622e-01  -1.594
## H.npnct17.log                                          5.692e-01   1.773
## H.has.ebola                                            4.424e-01  -0.880
## S.make.log                                             4.236e-01  -0.903
## H.npnct01.log                                          1.256e+00  -1.031
## H.npnct12.log                                          2.096e-01   2.024
## `myCategory.fctrForeign#World#Asia Pacific`            6.660e-01  -6.189
## `myCategory.fctr#Multimedia#`                          7.986e-01  -5.481
## `myCategory.fctrCulture#Arts#`                         3.662e-01  -7.708
## `myCategory.fctrBusiness#Business Day#Dealbook`        3.042e-01  -7.930
## myCategory.fctrmyOther                                 1.818e+03  -0.011
## `myCategory.fctrBusiness#Technology#`                  3.220e-01  -5.719
## `myCategory.fctrBusiness#Crosswords/Games#`            4.981e-01   1.705
## `myCategory.fctrTStyle##`                              4.915e-01  -8.547
## `myCategory.fctrForeign#World#`                        8.774e+02  -0.021
## `myCategory.fctrOpEd#Opinion#`                         2.934e-01   2.338
## `myCategory.fctrStyles##Fashion`                       1.017e+03  -0.019
## `myCategory.fctr#Opinion#Room For Debate`              6.277e-01  -9.039
## `myCategory.fctr#U.S.#Education`                       5.997e+02  -0.035
## `myCategory.fctr##`                                    2.863e-01  -9.508
## `myCategory.fctrMetro#N.Y. / Region#`                  4.654e-01  -3.901
## `myCategory.fctrBusiness#Business Day#Small Business`  6.841e-01  -6.632
## `myCategory.fctrStyles#U.S.#`                          3.336e-01  -1.417
## `myCategory.fctrTravel#Travel#`                        1.048e+00  -3.842
## `myCategory.fctr#Opinion#The Public Editor`            1.203e+00   0.969
## S.state.log                                            2.143e+04  -0.001
## A.state.log                                            2.143e+04   0.001
## S.one.log                                              1.551e+04   0.001
## A.one.log                                              1.551e+04  -0.002
## A.said.log                                             4.126e-01   2.148
## S.said.log                                                    NA      NA
## A.npnct17.log                                          1.287e+00  -0.236
## S.npnct17.log                                                 NA      NA
## S.npnct08.log                                          7.757e+03   0.002
## A.npnct08.log                                                 NA      NA
## S.npnct09.log                                          7.757e+03  -0.002
## A.npnct09.log                                                 NA      NA
## PubDate.last100.log                                    4.493e-02   0.326
## .rnorm                                                 6.266e-02  -1.304
## H.npnct05.log                                          6.194e+03  -0.004
## `PubDate.date.fctr(7,13]`                              1.957e-01  -0.231
## `PubDate.date.fctr(13,19]`                             1.933e-01  -0.754
## `PubDate.date.fctr(19,25]`                             1.903e-01  -0.476
## `PubDate.date.fctr(25,31]`                             2.039e-01   0.664
## `PubDate.second.fctr(14.8,29.5]`                       1.731e-01   0.527
## `PubDate.second.fctr(29.5,44.2]`                       1.700e-01  -0.110
## `PubDate.second.fctr(44.2,59.1]`                       1.769e-01  -1.633
## H.npnct07.log                                          1.850e-01   1.067
## S.npnct07.log                                          6.740e+03  -0.004
## S.npnct03.log                                          5.275e+03  -0.005
## A.npnct19.log                                          1.719e+04  -0.001
## H.npnct13.log                                          3.094e-01   1.128
## A.npnct20.log                                          9.804e+03   0.000
## S.has.year.colon                                       2.979e+03  -0.004
## S.npnct22.log                                          4.665e+03  -0.005
## H.npnct02.log                                          3.057e+03  -0.006
## S.presid.log                                           4.689e-01   1.033
## S.npnct15.log                                          1.521e+00   0.623
## S.npnct06.log                                          1.527e+00   0.069
## H.npnct14.log                                          1.967e-01  -1.102
## S.take.log                                             5.528e-01  -1.174
## `PubDate.minute.fctr(14.8,29.5]`                       1.807e-01  -0.819
## `PubDate.minute.fctr(29.5,44.2]`                       1.750e-01  -1.228
## `PubDate.minute.fctr(44.2,59.1]`                       1.816e-01   0.281
## S.new.log                                              3.091e-01   0.067
## S.npnct13.log                                          1.988e-01  -0.841
## PubDate.wkday.fctr1                                    5.244e-01  -0.994
## PubDate.wkday.fctr2                                    5.709e-01  -1.981
## PubDate.wkday.fctr3                                    5.632e-01  -1.349
## PubDate.wkday.fctr4                                    5.574e-01  -1.750
## PubDate.wkday.fctr5                                    5.621e-01  -1.522
## PubDate.wkday.fctr6                                    4.656e-01  -2.730
## S.npnct30.log                                          1.278e+03  -0.011
## S.day.log                                              6.227e-01  -0.315
## H.X2014.log                                            1.431e+00  -0.729
## S.show.log                                             6.102e-01  -1.043
## A.npnct14.log                                          2.591e-01   3.620
## S.report.log                                           6.050e-01  -2.152
## S.year.log                                             4.560e-01  -0.847
## H.npnct04.log                                          9.471e-01  -1.997
## S.share.log                                            6.595e-01  -1.491
## S.compani.log                                          4.143e-01  -1.031
## H.new.log                                              6.206e-01  -1.417
## S.first.log                                            6.259e-01  -0.249
## S.time.log                                             4.621e-01  -0.643
## H.newyork.log                                          6.991e-01   0.089
## S.articl.log                                           1.154e+00  -0.154
## S.will.log                                             3.715e-01  -1.373
## H.npnct15.log                                          1.298e+03  -0.016
## S.newyork.log                                          5.112e-01   2.191
## H.day.log                                              1.043e+00  -1.098
## S.npnct04.log                                          6.872e-01  -1.614
## H.today.log                                            9.770e-01  -3.564
## H.report.log                                           9.984e-01  -0.752
## S.npnct16.log                                          4.763e-01   0.929
## S.intern.log                                           1.206e+00  -0.809
## H.daili.log                                            1.439e+03  -0.015
## H.week.log                                             9.382e-01  -0.668
## H.npnct16.log                                          2.850e-01  -0.836
## S.fashion.log                                          9.283e+02  -0.023
## S.week.log                                             4.770e-01  -0.527
## H.npnct30.log                                          1.704e+00  -0.083
## S.npnct12.log                                          1.431e-01  -1.126
## H.ndgts.log                                            2.468e-01   1.890
## S.ndgts.log                                            1.547e-01  -2.202
## H.nuppr.log                                            4.193e-01   2.967
## H.nchrs.log                                            4.340e-01  -2.097
## H.nwrds.log                                            4.448e-01  -1.817
## A.nchrs.log                                            5.068e-01   0.461
## A.nwrds.log                                            1.652e+00   0.518
## A.nwrds.unq.log                                        1.591e+00  -1.006
## S.nuppr.log                                            1.563e-01  -4.297
##                                                       Pr(>|z|)    
## (Intercept)                                           0.049784 *  
## WordCount.log                                          < 2e-16 ***
## `PubDate.hour.fctr(7.67,15.3]`                        0.754532    
## `PubDate.hour.fctr(15.3,23]`                          0.351877    
## H.npnct21.log                                         2.30e-06 ***
## PubDate.wkend                                         0.539548    
## A.npnct21.log                                         1.58e-05 ***
## H.npnct09.log                                         0.003920 ** 
## PubDate.last10.log                                    0.062838 .  
## PubDate.last1.log                                     0.280342    
## S.npnct01.log                                         0.291976    
## S.can.log                                             0.110955    
## H.npnct17.log                                         0.076252 .  
## H.has.ebola                                           0.378717    
## S.make.log                                            0.366493    
## H.npnct01.log                                         0.302512    
## H.npnct12.log                                         0.043017 *  
## `myCategory.fctrForeign#World#Asia Pacific`           6.07e-10 ***
## `myCategory.fctr#Multimedia#`                         4.24e-08 ***
## `myCategory.fctrCulture#Arts#`                        1.28e-14 ***
## `myCategory.fctrBusiness#Business Day#Dealbook`       2.19e-15 ***
## myCategory.fctrmyOther                                0.991127    
## `myCategory.fctrBusiness#Technology#`                 1.07e-08 ***
## `myCategory.fctrBusiness#Crosswords/Games#`           0.088141 .  
## `myCategory.fctrTStyle##`                              < 2e-16 ***
## `myCategory.fctrForeign#World#`                       0.983399    
## `myCategory.fctrOpEd#Opinion#`                        0.019399 *  
## `myCategory.fctrStyles##Fashion`                      0.984554    
## `myCategory.fctr#Opinion#Room For Debate`              < 2e-16 ***
## `myCategory.fctr#U.S.#Education`                      0.972195    
## `myCategory.fctr##`                                    < 2e-16 ***
## `myCategory.fctrMetro#N.Y. / Region#`                 9.56e-05 ***
## `myCategory.fctrBusiness#Business Day#Small Business` 3.31e-11 ***
## `myCategory.fctrStyles#U.S.#`                         0.156348    
## `myCategory.fctrTravel#Travel#`                       0.000122 ***
## `myCategory.fctr#Opinion#The Public Editor`           0.332379    
## S.state.log                                           0.999464    
## A.state.log                                           0.999419    
## S.one.log                                             0.998822    
## A.one.log                                             0.998802    
## A.said.log                                            0.031690 *  
## S.said.log                                                  NA    
## A.npnct17.log                                         0.813240    
## S.npnct17.log                                               NA    
## S.npnct08.log                                         0.998642    
## A.npnct08.log                                               NA    
## S.npnct09.log                                         0.998803    
## A.npnct09.log                                               NA    
## PubDate.last100.log                                   0.744618    
## .rnorm                                                0.192341    
## H.npnct05.log                                         0.996870    
## `PubDate.date.fctr(7,13]`                             0.817151    
## `PubDate.date.fctr(13,19]`                            0.450572    
## `PubDate.date.fctr(19,25]`                            0.634202    
## `PubDate.date.fctr(25,31]`                            0.506891    
## `PubDate.second.fctr(14.8,29.5]`                      0.598536    
## `PubDate.second.fctr(29.5,44.2]`                      0.912018    
## `PubDate.second.fctr(44.2,59.1]`                      0.102401    
## H.npnct07.log                                         0.285972    
## S.npnct07.log                                         0.996979    
## S.npnct03.log                                         0.995821    
## A.npnct19.log                                         0.998958    
## H.npnct13.log                                         0.259466    
## A.npnct20.log                                         0.999796    
## S.has.year.colon                                      0.996602    
## S.npnct22.log                                         0.995907    
## H.npnct02.log                                         0.995470    
## S.presid.log                                          0.301545    
## S.npnct15.log                                         0.533108    
## S.npnct06.log                                         0.944691    
## H.npnct14.log                                         0.270322    
## S.take.log                                            0.240453    
## `PubDate.minute.fctr(14.8,29.5]`                      0.412919    
## `PubDate.minute.fctr(29.5,44.2]`                      0.219295    
## `PubDate.minute.fctr(44.2,59.1]`                      0.778381    
## S.new.log                                             0.946707    
## S.npnct13.log                                         0.400340    
## PubDate.wkday.fctr1                                   0.320325    
## PubDate.wkday.fctr2                                   0.047621 *  
## PubDate.wkday.fctr3                                   0.177272    
## PubDate.wkday.fctr4                                   0.080051 .  
## PubDate.wkday.fctr5                                   0.128106    
## PubDate.wkday.fctr6                                   0.006332 ** 
## S.npnct30.log                                         0.990966    
## S.day.log                                             0.752696    
## H.X2014.log                                           0.466137    
## S.show.log                                            0.297002    
## A.npnct14.log                                         0.000295 ***
## S.report.log                                          0.031395 *  
## S.year.log                                            0.397220    
## H.npnct04.log                                         0.045839 *  
## S.share.log                                           0.136093    
## S.compani.log                                         0.302760    
## H.new.log                                             0.156403    
## S.first.log                                           0.803577    
## S.time.log                                            0.520425    
## H.newyork.log                                         0.928951    
## S.articl.log                                          0.877434    
## S.will.log                                            0.169631    
## H.npnct15.log                                         0.987496    
## S.newyork.log                                         0.028476 *  
## H.day.log                                             0.272099    
## S.npnct04.log                                         0.106537    
## H.today.log                                           0.000366 ***
## H.report.log                                          0.451830    
## S.npnct16.log                                         0.352844    
## S.intern.log                                          0.418790    
## H.daili.log                                           0.988067    
## H.week.log                                            0.504070    
## H.npnct16.log                                         0.402997    
## S.fashion.log                                         0.981973    
## S.week.log                                            0.598083    
## H.npnct30.log                                         0.934245    
## S.npnct12.log                                         0.260270    
## H.ndgts.log                                           0.058737 .  
## S.ndgts.log                                           0.027647 *  
## H.nuppr.log                                           0.003005 ** 
## H.nchrs.log                                           0.035976 *  
## H.nwrds.log                                           0.069265 .  
## A.nchrs.log                                           0.644811    
## A.nwrds.log                                           0.604257    
## A.nwrds.unq.log                                       0.314356    
## S.nuppr.log                                           1.73e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 1841.2  on 4356  degrees of freedom
## AIC: 2079.2
## 
## Number of Fisher Scoring iterations: 18
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-25.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-26.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6600660
## 3        0.2 0.7335601
## 4        0.3 0.7415307
## 5        0.4 0.7444668
## 6        0.5 0.7324750
## 7        0.6 0.7080182
## 8        0.7 0.6540050
## 9        0.8 0.5776173
## 10       0.9 0.3779193
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3539
## 2            Y                                  194
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  187
## 2                                  555
##          Prediction
## Reference    N    Y
##         N 3539  187
##         Y  194  555
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.148603e-01   6.933889e-01   9.062982e-01   9.228764e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   9.523435e-58   7.585471e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-27.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-28.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6397516
## 3        0.2 0.7058824
## 4        0.3 0.7257618
## 5        0.4 0.7010309
## 6        0.5 0.7021944
## 7        0.6 0.6689189
## 8        0.7 0.6198198
## 9        0.8 0.5415020
## 10       0.9 0.3190476
## 11       1.0 0.0000000
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_0-29.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1597
## 2            Y                                   82
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  116
## 2                                  262
##          Prediction
## Reference    N    Y
##         N 1597  116
##         Y   82  262
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.037433e-01   6.675461e-01   8.901728e-01   9.161500e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.831382e-20   1.901647e-02 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, A.npnct21.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, .rnorm, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.npnct20.log, S.has.year.colon, S.npnct22.log, H.npnct02.log, S.presid.log, S.npnct15.log, S.npnct06.log, H.npnct14.log, S.take.log, PubDate.minute.fctr, S.new.log, S.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, S.day.log, H.X2014.log, S.show.log, A.npnct14.log, S.report.log, S.year.log, H.npnct04.log, S.share.log, S.compani.log, H.new.log, S.first.log, S.time.log, H.newyork.log, S.articl.log, S.will.log, H.npnct15.log, S.newyork.log, H.day.log, S.npnct04.log, H.today.log, H.report.log, S.npnct16.log, S.intern.log, H.daili.log, H.week.log, H.npnct16.log, S.fashion.log, S.week.log, H.npnct30.log, S.npnct12.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, A.nchrs.log, A.nwrds.log, A.nwrds.unq.log, S.nuppr.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      6.939                 3.268
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9486508                    0.4       0.7444668        0.9088264
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9062982             0.9228764     0.6544786   0.9262548
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.7257618        0.9037433
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8901728               0.91615     0.6675461    2079.176
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.00180873      0.00412111
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 9  fit.models          6          0 189.590 228.568  38.978
## 10 fit.models          6          1 228.568      NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor     bgn end elapsed
## 1 fit.models_1_bgn          1          0 232.714  NA      NA
```

```r
# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitent_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
        
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor     bgn     end elapsed
## 1 fit.models_1_bgn          1          0 232.714 232.725   0.012
## 2 fit.models_1_glm          2          0 232.726      NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, .rnorm, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: not plotting observations with leverage one:
##   1143, 2501, 3625, 3637, 3799, 4105, 4408
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-1.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-2.png) 

```
## Warning: not plotting observations with leverage one:
##   1143, 2501, 3625, 3637, 3799, 4105, 4408
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-3.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -8.49    0.00    0.00    0.00    8.49  
## 
## Coefficients: (31 not defined because of singularities)
##                                                         Estimate
## (Intercept)                                           -9.447e+14
## WordCount.log                                          4.908e+14
## `PubDate.hour.fctr(7.67,15.3]`                         1.748e+14
## `PubDate.hour.fctr(15.3,23]`                           6.615e+14
## H.npnct21.log                                          1.242e+15
## PubDate.wkend                                         -3.716e+13
## S.npnct21.log                                         -2.188e+15
## A.npnct21.log                                          3.821e+15
## H.npnct08.log                                          1.041e+15
## H.npnct09.log                                                 NA
## PubDate.last10.log                                     1.468e+14
## PubDate.last1.log                                     -5.579e+12
## H.npnct06.log                                          8.977e+14
## A.can.log                                              9.731e+15
## A.npnct01.log                                          9.868e+14
## S.npnct01.log                                                 NA
## S.can.log                                             -8.487e+15
## H.npnct17.log                                         -2.481e+14
## H.has.ebola                                            9.863e+13
## A.make.log                                             1.094e+15
## S.make.log                                                    NA
## H.npnct01.log                                          2.972e+14
## H.npnct12.log                                         -8.269e+13
## `myCategory.fctrForeign#World#Asia Pacific`           -3.868e+15
## `myCategory.fctr#Multimedia#`                         -3.856e+15
## `myCategory.fctrCulture#Arts#`                        -2.956e+15
## `myCategory.fctrBusiness#Business Day#Dealbook`       -3.209e+15
## myCategory.fctrmyOther                                -3.205e+15
## `myCategory.fctrBusiness#Technology#`                 -2.871e+15
## `myCategory.fctrBusiness#Crosswords/Games#`           -1.662e+14
## `myCategory.fctrTStyle##`                             -2.835e+15
## `myCategory.fctrForeign#World#`                       -2.009e+15
## `myCategory.fctrOpEd#Opinion#`                         9.667e+14
## `myCategory.fctrStyles##Fashion`                      -4.023e+15
## `myCategory.fctr#Opinion#Room For Debate`             -5.282e+15
## `myCategory.fctr#U.S.#Education`                      -6.503e+15
## `myCategory.fctr##`                                   -3.413e+15
## `myCategory.fctrMetro#N.Y. / Region#`                 -3.186e+15
## `myCategory.fctrBusiness#Business Day#Small Business` -3.525e+15
## `myCategory.fctrStyles#U.S.#`                         -8.377e+13
## `myCategory.fctrTravel#Travel#`                       -2.945e+15
## `myCategory.fctr#Opinion#The Public Editor`            1.634e+15
## S.state.log                                            1.774e+16
## A.state.log                                           -1.452e+16
## S.one.log                                             -5.269e+15
## A.one.log                                              5.178e+15
## A.said.log                                             6.214e+14
## S.said.log                                                    NA
## A.npnct17.log                                         -8.489e+14
## S.npnct17.log                                                 NA
## S.npnct08.log                                          2.157e+15
## A.npnct08.log                                                 NA
## S.npnct09.log                                         -1.195e+15
## A.npnct09.log                                                 NA
## PubDate.last100.log                                    2.795e+13
## .rnorm                                                -6.296e+13
## H.npnct05.log                                         -1.567e+15
## `PubDate.date.fctr(7,13]`                              9.714e+13
## `PubDate.date.fctr(13,19]`                            -1.235e+13
## `PubDate.date.fctr(19,25]`                             1.877e+13
## `PubDate.date.fctr(25,31]`                             1.610e+14
## `PubDate.second.fctr(14.8,29.5]`                       2.563e+13
## `PubDate.second.fctr(29.5,44.2]`                       5.194e+13
## `PubDate.second.fctr(44.2,59.1]`                      -7.845e+13
## H.npnct07.log                                          1.655e+14
## A.npnct07.log                                         -6.395e+14
## S.npnct07.log                                                 NA
## S.npnct03.log                                          3.179e+14
## A.npnct19.log                                          3.597e+16
## H.npnct13.log                                          2.953e+14
## A.has.http                                                    NA
## A.npnct03.log                                                 NA
## A.npnct02.log                                          4.811e+14
## A.npnct18.log                                          1.205e+16
## A.npnct20.log                                                 NA
## A.has.year.colon                                       1.478e+14
## S.has.year.colon                                              NA
## A.npnct22.log                                          1.087e+15
## S.npnct22.log                                                 NA
## H.npnct02.log                                         -3.379e+13
## A.presid.log                                           3.207e+14
## S.presid.log                                                  NA
## S.npnct15.log                                          2.152e+16
## A.npnct06.log                                         -2.430e+14
## S.npnct06.log                                                 NA
## A.npnct15.log                                         -2.113e+16
## H.npnct14.log                                         -1.404e+13
## S.take.log                                            -2.134e+14
## A.take.log                                                    NA
## `PubDate.minute.fctr(14.8,29.5]`                      -1.461e+14
## `PubDate.minute.fctr(29.5,44.2]`                      -9.707e+13
## `PubDate.minute.fctr(44.2,59.1]`                      -1.399e+13
## S.new.log                                              3.170e+15
## A.new.log                                             -3.070e+15
## S.npnct13.log                                         -2.045e+15
## A.npnct13.log                                          2.039e+15
## PubDate.wkday.fctr1                                   -7.566e+13
## PubDate.wkday.fctr2                                   -2.884e+14
## PubDate.wkday.fctr3                                   -1.565e+14
## PubDate.wkday.fctr4                                   -1.658e+14
## PubDate.wkday.fctr5                                   -2.091e+14
## PubDate.wkday.fctr6                                   -4.855e+14
## S.npnct30.log                                         -3.633e+15
## A.npnct30.log                                          2.711e+15
## S.day.log                                             -2.921e+14
## A.day.log                                                     NA
## H.X2014.log                                            4.301e+14
## A.show.log                                            -4.676e+14
## S.show.log                                                    NA
## A.npnct14.log                                          2.796e+14
## A.report.log                                          -9.455e+14
## S.report.log                                                  NA
## A.year.log                                            -2.529e+14
## S.year.log                                                    NA
## H.npnct04.log                                         -8.797e+14
## A.share.log                                           -8.119e+14
## S.share.log                                                   NA
## S.compani.log                                         -2.613e+14
## A.compani.log                                                 NA
## H.new.log                                             -6.768e+14
## S.npnct14.log                                          2.368e+14
## A.first.log                                           -6.177e+13
## S.first.log                                                   NA
## S.time.log                                            -3.788e+14
## A.time.log                                                    NA
## H.newyork.log                                         -6.853e+13
## A.articl.log                                           1.199e+15
## S.articl.log                                                  NA
## S.will.log                                             3.765e+15
## A.will.log                                            -4.114e+15
## H.npnct15.log                                         -4.845e+14
## A.newyork.log                                          4.852e+14
## S.newyork.log                                                 NA
## H.day.log                                              7.724e+13
## A.npnct04.log                                         -7.641e+14
## S.npnct04.log                                                 NA
## H.today.log                                           -1.053e+15
## H.report.log                                          -3.294e+14
## H.X2015.log                                            2.301e+15
## S.npnct16.log                                          3.428e+14
## A.intern.log                                          -8.385e+14
## S.intern.log                                                  NA
## A.npnct16.log                                                 NA
## H.daili.log                                            1.739e+13
## H.week.log                                            -6.011e+14
## H.has.year.colon                                       1.197e+15
## H.fashion.log                                          3.386e+14
## H.npnct16.log                                         -2.156e+13
## A.fashion.log                                         -1.494e+15
## S.fashion.log                                                 NA
## A.week.log                                            -3.959e+14
## S.week.log                                                    NA
## H.npnct30.log                                          1.581e+15
## S.npnct12.log                                         -8.402e+15
## A.npnct12.log                                          8.350e+15
## H.ndgts.log                                            3.205e+14
## S.ndgts.log                                           -1.457e+15
## A.ndgts.log                                            1.320e+15
## H.nuppr.log                                            3.456e+14
## H.nchrs.log                                           -2.818e+14
## H.nwrds.log                                           -1.434e+14
## H.nwrds.unq.log                                       -4.929e+14
## A.nchrs.log                                           -8.539e+15
## S.nchrs.log                                            8.168e+15
## A.nwrds.log                                           -8.951e+15
## S.nwrds.log                                            9.525e+15
## A.nwrds.unq.log                                        1.404e+16
## S.nwrds.unq.log                                       -1.464e+16
## S.nuppr.log                                           -4.014e+15
## A.nuppr.log                                            3.559e+15
##                                                       Std. Error
## (Intercept)                                            3.559e+07
## WordCount.log                                          1.219e+06
## `PubDate.hour.fctr(7.67,15.3]`                         3.797e+06
## `PubDate.hour.fctr(15.3,23]`                           4.044e+06
## H.npnct21.log                                          6.619e+06
## PubDate.wkend                                          7.776e+06
## S.npnct21.log                                          9.958e+07
## A.npnct21.log                                          9.953e+07
## H.npnct08.log                                          1.489e+07
## H.npnct09.log                                                 NA
## PubDate.last10.log                                     1.954e+06
## PubDate.last1.log                                      7.091e+05
## H.npnct06.log                                          1.670e+07
## A.can.log                                              2.057e+08
## A.npnct01.log                                          2.679e+07
## S.npnct01.log                                                 NA
## S.can.log                                              2.062e+08
## H.npnct17.log                                          1.784e+07
## H.has.ebola                                            8.881e+06
## A.make.log                                             7.668e+06
## S.make.log                                                    NA
## H.npnct01.log                                          2.340e+07
## H.npnct12.log                                          3.661e+06
## `myCategory.fctrForeign#World#Asia Pacific`            8.774e+06
## `myCategory.fctr#Multimedia#`                          1.018e+07
## `myCategory.fctrCulture#Arts#`                         7.372e+06
## `myCategory.fctrBusiness#Business Day#Dealbook`        7.183e+06
## myCategory.fctrmyOther                                 1.513e+07
## `myCategory.fctrBusiness#Technology#`                  7.949e+06
## `myCategory.fctrBusiness#Crosswords/Games#`            1.047e+07
## `myCategory.fctrTStyle##`                              7.317e+06
## `myCategory.fctrForeign#World#`                        1.735e+07
## `myCategory.fctrOpEd#Opinion#`                         7.377e+06
## `myCategory.fctrStyles##Fashion`                       1.128e+07
## `myCategory.fctr#Opinion#Room For Debate`              1.181e+07
## `myCategory.fctr#U.S.#Education`                       1.138e+07
## `myCategory.fctr##`                                    6.801e+06
## `myCategory.fctrMetro#N.Y. / Region#`                  9.712e+06
## `myCategory.fctrBusiness#Business Day#Small Business`  9.909e+06
## `myCategory.fctrStyles#U.S.#`                          8.958e+06
## `myCategory.fctrTravel#Travel#`                        9.798e+06
## `myCategory.fctr#Opinion#The Public Editor`            2.252e+07
## S.state.log                                            3.414e+08
## A.state.log                                            3.414e+08
## S.one.log                                              1.684e+08
## A.one.log                                              1.686e+08
## A.said.log                                             7.938e+06
## S.said.log                                                    NA
## A.npnct17.log                                          2.100e+07
## S.npnct17.log                                                 NA
## S.npnct08.log                                          5.207e+07
## A.npnct08.log                                                 NA
## S.npnct09.log                                          4.868e+07
## A.npnct09.log                                                 NA
## PubDate.last100.log                                    7.782e+05
## .rnorm                                                 1.028e+06
## H.npnct05.log                                          4.048e+07
## `PubDate.date.fctr(7,13]`                              3.232e+06
## `PubDate.date.fctr(13,19]`                             3.186e+06
## `PubDate.date.fctr(19,25]`                             3.079e+06
## `PubDate.date.fctr(25,31]`                             3.432e+06
## `PubDate.second.fctr(14.8,29.5]`                       2.860e+06
## `PubDate.second.fctr(29.5,44.2]`                       2.823e+06
## `PubDate.second.fctr(44.2,59.1]`                       2.883e+06
## H.npnct07.log                                          2.982e+06
## A.npnct07.log                                          5.011e+07
## S.npnct07.log                                                 NA
## S.npnct03.log                                          4.404e+07
## A.npnct19.log                                          7.276e+08
## H.npnct13.log                                          5.228e+06
## A.has.http                                                    NA
## A.npnct03.log                                                 NA
## A.npnct02.log                                          6.300e+07
## A.npnct18.log                                          2.198e+08
## A.npnct20.log                                                 NA
## A.has.year.colon                                       2.385e+07
## S.has.year.colon                                              NA
## A.npnct22.log                                          3.172e+07
## S.npnct22.log                                                 NA
## H.npnct02.log                                          2.532e+07
## A.presid.log                                           7.645e+06
## S.presid.log                                                  NA
## S.npnct15.log                                          3.378e+08
## A.npnct06.log                                          1.791e+07
## S.npnct06.log                                                 NA
## A.npnct15.log                                          3.373e+08
## H.npnct14.log                                          3.444e+06
## S.take.log                                             8.391e+06
## A.take.log                                                    NA
## `PubDate.minute.fctr(14.8,29.5]`                       2.937e+06
## `PubDate.minute.fctr(29.5,44.2]`                       2.776e+06
## `PubDate.minute.fctr(44.2,59.1]`                       2.998e+06
## S.new.log                                              1.496e+08
## A.new.log                                              1.494e+08
## S.npnct13.log                                          4.866e+07
## A.npnct13.log                                          4.851e+07
## PubDate.wkday.fctr1                                    9.451e+06
## PubDate.wkday.fctr2                                    1.006e+07
## PubDate.wkday.fctr3                                    1.001e+07
## PubDate.wkday.fctr4                                    9.860e+06
## PubDate.wkday.fctr5                                    9.986e+06
## PubDate.wkday.fctr6                                    7.826e+06
## S.npnct30.log                                          2.072e+08
## A.npnct30.log                                          2.058e+08
## S.day.log                                              8.907e+06
## A.day.log                                                     NA
## H.X2014.log                                            1.620e+07
## A.show.log                                             8.303e+06
## S.show.log                                                    NA
## A.npnct14.log                                          2.831e+07
## A.report.log                                           9.142e+06
## S.report.log                                                  NA
## A.year.log                                             6.828e+06
## S.year.log                                                    NA
## H.npnct04.log                                          1.190e+07
## A.share.log                                            8.761e+06
## S.share.log                                                   NA
## S.compani.log                                          6.664e+06
## A.compani.log                                                 NA
## H.new.log                                              8.030e+06
## S.npnct14.log                                          2.810e+07
## A.first.log                                            8.267e+06
## S.first.log                                                   NA
## S.time.log                                             6.816e+06
## A.time.log                                                    NA
## H.newyork.log                                          9.799e+06
## A.articl.log                                           1.201e+07
## S.articl.log                                                  NA
## S.will.log                                             1.030e+08
## A.will.log                                             1.029e+08
## H.npnct15.log                                          2.330e+07
## A.newyork.log                                          7.217e+06
## S.newyork.log                                                 NA
## H.day.log                                              1.027e+07
## A.npnct04.log                                          8.063e+06
## S.npnct04.log                                                 NA
## H.today.log                                            1.208e+07
## H.report.log                                           1.258e+07
## H.X2015.log                                            2.464e+07
## S.npnct16.log                                          8.188e+06
## A.intern.log                                           1.491e+07
## S.intern.log                                                  NA
## A.npnct16.log                                                 NA
## H.daili.log                                            1.584e+07
## H.week.log                                             1.319e+07
## H.has.year.colon                                       1.430e+07
## H.fashion.log                                          1.501e+07
## H.npnct16.log                                          4.811e+06
## A.fashion.log                                          1.136e+07
## S.fashion.log                                                 NA
## A.week.log                                             6.934e+06
## S.week.log                                                    NA
## H.npnct30.log                                          1.469e+07
## S.npnct12.log                                          1.791e+08
## A.npnct12.log                                          1.790e+08
## H.ndgts.log                                            4.098e+06
## S.ndgts.log                                            3.677e+07
## A.ndgts.log                                            3.667e+07
## H.nuppr.log                                            7.529e+06
## H.nchrs.log                                            7.535e+06
## H.nwrds.log                                            3.854e+07
## H.nwrds.unq.log                                        3.787e+07
## A.nchrs.log                                            4.519e+08
## S.nchrs.log                                            4.518e+08
## A.nwrds.log                                            7.378e+08
## S.nwrds.log                                            7.379e+08
## A.nwrds.unq.log                                        5.933e+08
## S.nwrds.unq.log                                        5.932e+08
## S.nuppr.log                                            1.054e+08
## A.nuppr.log                                            1.054e+08
##                                                          z value Pr(>|z|)
## (Intercept)                                            -26544426   <2e-16
## WordCount.log                                          402748115   <2e-16
## `PubDate.hour.fctr(7.67,15.3]`                          46035233   <2e-16
## `PubDate.hour.fctr(15.3,23]`                           163594019   <2e-16
## H.npnct21.log                                          187659261   <2e-16
## PubDate.wkend                                           -4778830   <2e-16
## S.npnct21.log                                          -21970579   <2e-16
## A.npnct21.log                                           38389620   <2e-16
## H.npnct08.log                                           69905112   <2e-16
## H.npnct09.log                                                 NA       NA
## PubDate.last10.log                                      75105174   <2e-16
## PubDate.last1.log                                       -7868539   <2e-16
## H.npnct06.log                                           53768602   <2e-16
## A.can.log                                               47310920   <2e-16
## A.npnct01.log                                           36827222   <2e-16
## S.npnct01.log                                                 NA       NA
## S.can.log                                              -41149445   <2e-16
## H.npnct17.log                                          -13906966   <2e-16
## H.has.ebola                                             11105687   <2e-16
## A.make.log                                             142663085   <2e-16
## S.make.log                                                    NA       NA
## H.npnct01.log                                           12702040   <2e-16
## H.npnct12.log                                          -22585094   <2e-16
## `myCategory.fctrForeign#World#Asia Pacific`           -440918229   <2e-16
## `myCategory.fctr#Multimedia#`                         -378666617   <2e-16
## `myCategory.fctrCulture#Arts#`                        -400964516   <2e-16
## `myCategory.fctrBusiness#Business Day#Dealbook`       -446802417   <2e-16
## myCategory.fctrmyOther                                -211785090   <2e-16
## `myCategory.fctrBusiness#Technology#`                 -361145227   <2e-16
## `myCategory.fctrBusiness#Crosswords/Games#`            -15869663   <2e-16
## `myCategory.fctrTStyle##`                             -387523258   <2e-16
## `myCategory.fctrForeign#World#`                       -115739228   <2e-16
## `myCategory.fctrOpEd#Opinion#`                         131038302   <2e-16
## `myCategory.fctrStyles##Fashion`                      -356671576   <2e-16
## `myCategory.fctr#Opinion#Room For Debate`             -447081085   <2e-16
## `myCategory.fctr#U.S.#Education`                      -571361362   <2e-16
## `myCategory.fctr##`                                   -501870028   <2e-16
## `myCategory.fctrMetro#N.Y. / Region#`                 -328006158   <2e-16
## `myCategory.fctrBusiness#Business Day#Small Business` -355738103   <2e-16
## `myCategory.fctrStyles#U.S.#`                           -9351322   <2e-16
## `myCategory.fctrTravel#Travel#`                       -300556630   <2e-16
## `myCategory.fctr#Opinion#The Public Editor`             72559625   <2e-16
## S.state.log                                             51970619   <2e-16
## A.state.log                                            -42516911   <2e-16
## S.one.log                                              -31289229   <2e-16
## A.one.log                                               30714676   <2e-16
## A.said.log                                              78280670   <2e-16
## S.said.log                                                    NA       NA
## A.npnct17.log                                          -40431083   <2e-16
## S.npnct17.log                                                 NA       NA
## S.npnct08.log                                           41419696   <2e-16
## A.npnct08.log                                                 NA       NA
## S.npnct09.log                                          -24549282   <2e-16
## A.npnct09.log                                                 NA       NA
## PubDate.last100.log                                     35917907   <2e-16
## .rnorm                                                 -61275955   <2e-16
## H.npnct05.log                                          -38720733   <2e-16
## `PubDate.date.fctr(7,13]`                               30052149   <2e-16
## `PubDate.date.fctr(13,19]`                              -3875972   <2e-16
## `PubDate.date.fctr(19,25]`                               6096550   <2e-16
## `PubDate.date.fctr(25,31]`                              46922910   <2e-16
## `PubDate.second.fctr(14.8,29.5]`                         8962778   <2e-16
## `PubDate.second.fctr(29.5,44.2]`                        18402512   <2e-16
## `PubDate.second.fctr(44.2,59.1]`                       -27213882   <2e-16
## H.npnct07.log                                           55482694   <2e-16
## A.npnct07.log                                          -12761386   <2e-16
## S.npnct07.log                                                 NA       NA
## S.npnct03.log                                            7218966   <2e-16
## A.npnct19.log                                           49439608   <2e-16
## H.npnct13.log                                           56482498   <2e-16
## A.has.http                                                    NA       NA
## A.npnct03.log                                                 NA       NA
## A.npnct02.log                                            7636804   <2e-16
## A.npnct18.log                                           54815431   <2e-16
## A.npnct20.log                                                 NA       NA
## A.has.year.colon                                         6196380   <2e-16
## S.has.year.colon                                              NA       NA
## A.npnct22.log                                           34281885   <2e-16
## S.npnct22.log                                                 NA       NA
## H.npnct02.log                                           -1334378   <2e-16
## A.presid.log                                            41945768   <2e-16
## S.presid.log                                                  NA       NA
## S.npnct15.log                                           63704223   <2e-16
## A.npnct06.log                                          -13567638   <2e-16
## S.npnct06.log                                                 NA       NA
## A.npnct15.log                                          -62646168   <2e-16
## H.npnct14.log                                           -4076244   <2e-16
## S.take.log                                             -25432453   <2e-16
## A.take.log                                                    NA       NA
## `PubDate.minute.fctr(14.8,29.5]`                       -49746763   <2e-16
## `PubDate.minute.fctr(29.5,44.2]`                       -34971274   <2e-16
## `PubDate.minute.fctr(44.2,59.1]`                        -4664975   <2e-16
## S.new.log                                               21186768   <2e-16
## A.new.log                                              -20549279   <2e-16
## S.npnct13.log                                          -42037279   <2e-16
## A.npnct13.log                                           42030083   <2e-16
## PubDate.wkday.fctr1                                     -8005232   <2e-16
## PubDate.wkday.fctr2                                    -28661306   <2e-16
## PubDate.wkday.fctr3                                    -15634792   <2e-16
## PubDate.wkday.fctr4                                    -16811492   <2e-16
## PubDate.wkday.fctr5                                    -20935621   <2e-16
## PubDate.wkday.fctr6                                    -62035818   <2e-16
## S.npnct30.log                                          -17529790   <2e-16
## A.npnct30.log                                           13176029   <2e-16
## S.day.log                                              -32790424   <2e-16
## A.day.log                                                     NA       NA
## H.X2014.log                                             26558128   <2e-16
## A.show.log                                             -56322215   <2e-16
## S.show.log                                                    NA       NA
## A.npnct14.log                                            9876652   <2e-16
## A.report.log                                          -103425376   <2e-16
## S.report.log                                                  NA       NA
## A.year.log                                             -37043182   <2e-16
## S.year.log                                                    NA       NA
## H.npnct04.log                                          -73915998   <2e-16
## A.share.log                                            -92668869   <2e-16
## S.share.log                                                   NA       NA
## S.compani.log                                          -39209719   <2e-16
## A.compani.log                                                 NA       NA
## H.new.log                                              -84292376   <2e-16
## S.npnct14.log                                            8426715   <2e-16
## A.first.log                                             -7471487   <2e-16
## S.first.log                                                   NA       NA
## S.time.log                                             -55576089   <2e-16
## A.time.log                                                    NA       NA
## H.newyork.log                                           -6993368   <2e-16
## A.articl.log                                            99843475   <2e-16
## S.articl.log                                                  NA       NA
## S.will.log                                              36567174   <2e-16
## A.will.log                                             -39972218   <2e-16
## H.npnct15.log                                          -20793499   <2e-16
## A.newyork.log                                           67232099   <2e-16
## S.newyork.log                                                 NA       NA
## H.day.log                                                7522782   <2e-16
## A.npnct04.log                                          -94776214   <2e-16
## S.npnct04.log                                                 NA       NA
## H.today.log                                            -87153602   <2e-16
## H.report.log                                           -26190720   <2e-16
## H.X2015.log                                             93410695   <2e-16
## S.npnct16.log                                           41868159   <2e-16
## A.intern.log                                           -56220975   <2e-16
## S.intern.log                                                  NA       NA
## A.npnct16.log                                                 NA       NA
## H.daili.log                                              1097436   <2e-16
## H.week.log                                             -45556770   <2e-16
## H.has.year.colon                                        83708330   <2e-16
## H.fashion.log                                           22561643   <2e-16
## H.npnct16.log                                           -4481965   <2e-16
## A.fashion.log                                         -131505345   <2e-16
## S.fashion.log                                                 NA       NA
## A.week.log                                             -57095008   <2e-16
## S.week.log                                                    NA       NA
## H.npnct30.log                                          107559412   <2e-16
## S.npnct12.log                                          -46916170   <2e-16
## A.npnct12.log                                           46639912   <2e-16
## H.ndgts.log                                             78211121   <2e-16
## S.ndgts.log                                            -39634064   <2e-16
## A.ndgts.log                                             36010722   <2e-16
## H.nuppr.log                                             45899612   <2e-16
## H.nchrs.log                                            -37398480   <2e-16
## H.nwrds.log                                             -3721869   <2e-16
## H.nwrds.unq.log                                        -13016909   <2e-16
## A.nchrs.log                                            -18894333   <2e-16
## S.nchrs.log                                             18080056   <2e-16
## A.nwrds.log                                            -12132798   <2e-16
## S.nwrds.log                                             12909056   <2e-16
## A.nwrds.unq.log                                         23662101   <2e-16
## S.nwrds.unq.log                                        -24676382   <2e-16
## S.nuppr.log                                            -38091413   <2e-16
## A.nuppr.log                                             33776771   <2e-16
##                                                          
## (Intercept)                                           ***
## WordCount.log                                         ***
## `PubDate.hour.fctr(7.67,15.3]`                        ***
## `PubDate.hour.fctr(15.3,23]`                          ***
## H.npnct21.log                                         ***
## PubDate.wkend                                         ***
## S.npnct21.log                                         ***
## A.npnct21.log                                         ***
## H.npnct08.log                                         ***
## H.npnct09.log                                            
## PubDate.last10.log                                    ***
## PubDate.last1.log                                     ***
## H.npnct06.log                                         ***
## A.can.log                                             ***
## A.npnct01.log                                         ***
## S.npnct01.log                                            
## S.can.log                                             ***
## H.npnct17.log                                         ***
## H.has.ebola                                           ***
## A.make.log                                            ***
## S.make.log                                               
## H.npnct01.log                                         ***
## H.npnct12.log                                         ***
## `myCategory.fctrForeign#World#Asia Pacific`           ***
## `myCategory.fctr#Multimedia#`                         ***
## `myCategory.fctrCulture#Arts#`                        ***
## `myCategory.fctrBusiness#Business Day#Dealbook`       ***
## myCategory.fctrmyOther                                ***
## `myCategory.fctrBusiness#Technology#`                 ***
## `myCategory.fctrBusiness#Crosswords/Games#`           ***
## `myCategory.fctrTStyle##`                             ***
## `myCategory.fctrForeign#World#`                       ***
## `myCategory.fctrOpEd#Opinion#`                        ***
## `myCategory.fctrStyles##Fashion`                      ***
## `myCategory.fctr#Opinion#Room For Debate`             ***
## `myCategory.fctr#U.S.#Education`                      ***
## `myCategory.fctr##`                                   ***
## `myCategory.fctrMetro#N.Y. / Region#`                 ***
## `myCategory.fctrBusiness#Business Day#Small Business` ***
## `myCategory.fctrStyles#U.S.#`                         ***
## `myCategory.fctrTravel#Travel#`                       ***
## `myCategory.fctr#Opinion#The Public Editor`           ***
## S.state.log                                           ***
## A.state.log                                           ***
## S.one.log                                             ***
## A.one.log                                             ***
## A.said.log                                            ***
## S.said.log                                               
## A.npnct17.log                                         ***
## S.npnct17.log                                            
## S.npnct08.log                                         ***
## A.npnct08.log                                            
## S.npnct09.log                                         ***
## A.npnct09.log                                            
## PubDate.last100.log                                   ***
## .rnorm                                                ***
## H.npnct05.log                                         ***
## `PubDate.date.fctr(7,13]`                             ***
## `PubDate.date.fctr(13,19]`                            ***
## `PubDate.date.fctr(19,25]`                            ***
## `PubDate.date.fctr(25,31]`                            ***
## `PubDate.second.fctr(14.8,29.5]`                      ***
## `PubDate.second.fctr(29.5,44.2]`                      ***
## `PubDate.second.fctr(44.2,59.1]`                      ***
## H.npnct07.log                                         ***
## A.npnct07.log                                         ***
## S.npnct07.log                                            
## S.npnct03.log                                         ***
## A.npnct19.log                                         ***
## H.npnct13.log                                         ***
## A.has.http                                               
## A.npnct03.log                                            
## A.npnct02.log                                         ***
## A.npnct18.log                                         ***
## A.npnct20.log                                            
## A.has.year.colon                                      ***
## S.has.year.colon                                         
## A.npnct22.log                                         ***
## S.npnct22.log                                            
## H.npnct02.log                                         ***
## A.presid.log                                          ***
## S.presid.log                                             
## S.npnct15.log                                         ***
## A.npnct06.log                                         ***
## S.npnct06.log                                            
## A.npnct15.log                                         ***
## H.npnct14.log                                         ***
## S.take.log                                            ***
## A.take.log                                               
## `PubDate.minute.fctr(14.8,29.5]`                      ***
## `PubDate.minute.fctr(29.5,44.2]`                      ***
## `PubDate.minute.fctr(44.2,59.1]`                      ***
## S.new.log                                             ***
## A.new.log                                             ***
## S.npnct13.log                                         ***
## A.npnct13.log                                         ***
## PubDate.wkday.fctr1                                   ***
## PubDate.wkday.fctr2                                   ***
## PubDate.wkday.fctr3                                   ***
## PubDate.wkday.fctr4                                   ***
## PubDate.wkday.fctr5                                   ***
## PubDate.wkday.fctr6                                   ***
## S.npnct30.log                                         ***
## A.npnct30.log                                         ***
## S.day.log                                             ***
## A.day.log                                                
## H.X2014.log                                           ***
## A.show.log                                            ***
## S.show.log                                               
## A.npnct14.log                                         ***
## A.report.log                                          ***
## S.report.log                                             
## A.year.log                                            ***
## S.year.log                                               
## H.npnct04.log                                         ***
## A.share.log                                           ***
## S.share.log                                              
## S.compani.log                                         ***
## A.compani.log                                            
## H.new.log                                             ***
## S.npnct14.log                                         ***
## A.first.log                                           ***
## S.first.log                                              
## S.time.log                                            ***
## A.time.log                                               
## H.newyork.log                                         ***
## A.articl.log                                          ***
## S.articl.log                                             
## S.will.log                                            ***
## A.will.log                                            ***
## H.npnct15.log                                         ***
## A.newyork.log                                         ***
## S.newyork.log                                            
## H.day.log                                             ***
## A.npnct04.log                                         ***
## S.npnct04.log                                            
## H.today.log                                           ***
## H.report.log                                          ***
## H.X2015.log                                           ***
## S.npnct16.log                                         ***
## A.intern.log                                          ***
## S.intern.log                                             
## A.npnct16.log                                            
## H.daili.log                                           ***
## H.week.log                                            ***
## H.has.year.colon                                      ***
## H.fashion.log                                         ***
## H.npnct16.log                                         ***
## A.fashion.log                                         ***
## S.fashion.log                                            
## A.week.log                                            ***
## S.week.log                                               
## H.npnct30.log                                         ***
## S.npnct12.log                                         ***
## A.npnct12.log                                         ***
## H.ndgts.log                                           ***
## S.ndgts.log                                           ***
## A.ndgts.log                                           ***
## H.nuppr.log                                           ***
## H.nchrs.log                                           ***
## H.nwrds.log                                           ***
## H.nwrds.unq.log                                       ***
## A.nchrs.log                                           ***
## S.nchrs.log                                           ***
## A.nwrds.log                                           ***
## S.nwrds.log                                           ***
## A.nwrds.unq.log                                       ***
## S.nwrds.unq.log                                       ***
## S.nuppr.log                                           ***
## A.nuppr.log                                           ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance:  4042.7  on 4474  degrees of freedom
## Residual deviance: 29844.1  on 4336  degrees of freedom
## AIC: 30122
## 
## Number of Fisher Scoring iterations: 25
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-4.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.7059659
## 3        0.2 0.7059659
## 4        0.3 0.7059659
## 5        0.4 0.7059659
## 6        0.5 0.7059659
## 7        0.6 0.7059659
## 8        0.7 0.7059659
## 9        0.8 0.7059659
## 10       0.9 0.7059659
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.All.X.glm.N
## 1            N                             3564
## 2            Y                              252
##   Popular.fctr.predict.All.X.glm.Y
## 1                              162
## 2                              497
##          Prediction
## Reference    N    Y
##         N 3564  162
##         Y  252  497
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.074860e-01   6.513393e-01   8.986168e-01   9.158192e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   2.373225e-47   1.219281e-05 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-6.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6897638
## 3        0.2 0.6897638
## 4        0.3 0.6897638
## 5        0.4 0.6897638
## 6        0.5 0.6897638
## 7        0.6 0.6897638
## 8        0.7 0.6897638
## 9        0.8 0.6897638
## 10       0.9 0.6897638
## 11       1.0 0.0000000
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-8.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.All.X.glm.N
## 1            N                             1641
## 2            Y                              125
##   Popular.fctr.predict.All.X.glm.Y
## 1                               72
## 2                              219
##          Prediction
## Reference    N    Y
##         N 1641   72
##         Y  125  219
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.042295e-01   6.336042e-01   8.906870e-01   9.166067e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   9.648468e-21   2.115187e-04 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## 1 WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, .rnorm, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     13.409                  6.87
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8100366                    0.9       0.7059659        0.9021193
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8986168             0.9158192     0.6295388   0.7972982
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.6897638        0.9042295
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1              0.890687             0.9166067     0.6336042    30122.15
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01403897      0.05217796
##                label step_major step_minor     bgn     end elapsed
## 2   fit.models_1_glm          2          0 232.726 250.408  17.682
## 3 fit.models_1_rpart          3          0 250.408      NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0113 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-9.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-10.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##           CP nsplit rel error
## 1 0.27102804      0 1.0000000
## 2 0.08411215      1 0.7289720
## 3 0.01134846      2 0.6448598
## 
## Variable importance
##              myCategory.fctrOpEd#Opinion# 
##                                        51 
## myCategory.fctrBusiness#Crosswords/Games# 
##                                        17 
##                           A.nwrds.unq.log 
##                                         6 
##                               A.nwrds.log 
##                                         6 
##                           S.nwrds.unq.log 
##                                         6 
##                               S.nwrds.log 
##                                         6 
##                               A.nchrs.log 
##                                         6 
##                               H.nchrs.log 
##                                         1 
## 
## Node number 1: 4475 observations,    complexity param=0.271028
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (4106 obs) right son=3 (369 obs)
##   Primary splits:
##       myCategory.fctrOpEd#Opinion#              < 0.5       to the left,  improve=297.02950, (0 missing)
##       WordCount.log                             < 6.524296  to the left,  improve=105.72630, (0 missing)
##       S.nuppr.log                               < 1.497866  to the right, improve= 86.35796, (0 missing)
##       A.nuppr.log                               < 1.497866  to the right, improve= 86.35796, (0 missing)
##       myCategory.fctrBusiness#Crosswords/Games# < 0.5       to the left,  improve= 85.77765, (0 missing)
##   Surrogate splits:
##       A.nwrds.unq.log < 1.497866  to the right, agree=0.928, adj=0.127, (0 split)
##       A.nwrds.log     < 1.497866  to the right, agree=0.928, adj=0.125, (0 split)
##       S.nwrds.unq.log < 1.497866  to the right, agree=0.928, adj=0.125, (0 split)
##       S.nwrds.log     < 1.497866  to the right, agree=0.928, adj=0.122, (0 split)
##       A.nchrs.log     < 3.725621  to the right, agree=0.927, adj=0.117, (0 split)
## 
## Node number 2: 4106 observations,    complexity param=0.08411215
##   predicted class=N  expected loss=0.1127618  P(node) =0.9175419
##     class counts:  3643   463
##    probabilities: 0.887 0.113 
##   left son=4 (4023 obs) right son=5 (83 obs)
##   Primary splits:
##       myCategory.fctrBusiness#Crosswords/Games# < 0.5       to the left,  improve=99.60741, (0 missing)
##       WordCount.log                             < 6.485398  to the left,  improve=94.68604, (0 missing)
##       myCategory.fctrStyles#U.S.#               < 0.5       to the left,  improve=50.94648, (0 missing)
##       S.nuppr.log                               < 1.497866  to the right, improve=31.44556, (0 missing)
##       A.nuppr.log                               < 1.497866  to the right, improve=31.44556, (0 missing)
##   Surrogate splits:
##       H.nchrs.log < 2.35024   to the right, agree=0.981, adj=0.060, (0 split)
##       H.nuppr.log < 0.8958797 to the right, agree=0.980, adj=0.024, (0 split)
## 
## Node number 3: 369 observations
##   predicted class=Y  expected loss=0.2249322  P(node) =0.0824581
##     class counts:    83   286
##    probabilities: 0.225 0.775 
## 
## Node number 4: 4023 observations
##   predicted class=N  expected loss=0.09694258  P(node) =0.8989944
##     class counts:  3633   390
##    probabilities: 0.903 0.097 
## 
## Node number 5: 83 observations
##   predicted class=Y  expected loss=0.1204819  P(node) =0.01854749
##     class counts:    10    73
##    probabilities: 0.120 0.880 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 4475 749 N (0.83262570 0.16737430)  
##   2) myCategory.fctrOpEd#Opinion#< 0.5 4106 463 N (0.88723819 0.11276181)  
##     4) myCategory.fctrBusiness#Crosswords/Games#< 0.5 4023 390 N (0.90305742 0.09694258) *
##     5) myCategory.fctrBusiness#Crosswords/Games#>=0.5 83  10 Y (0.12048193 0.87951807) *
##   3) myCategory.fctrOpEd#Opinion#>=0.5 369  83 Y (0.22493225 0.77506775) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.5978351
## 3        0.2 0.5978351
## 4        0.3 0.5978351
## 5        0.4 0.5978351
## 6        0.5 0.5978351
## 7        0.6 0.5978351
## 8        0.7 0.5978351
## 9        0.8 0.1754808
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.All.X.no.rnorm.rpart.N
## 1            N                                        3633
## 2            Y                                         390
##   Popular.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                          93
## 2                                         359
##          Prediction
## Reference    N    Y
##         N 3633   93
##         Y  390  359
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.920670e-01   5.398657e-01   8.826068e-01   9.010121e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.439953e-29   2.397951e-41 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-13.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.5650558
## 3        0.2 0.5650558
## 4        0.3 0.5650558
## 5        0.4 0.5650558
## 6        0.5 0.5650558
## 7        0.6 0.5650558
## 8        0.7 0.5650558
## 9        0.8 0.1562500
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.All.X.no.rnorm.rpart.N
## 1            N                                        1671
## 2            Y                                         192
##   Popular.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                          42
## 2                                         152
##          Prediction
## Reference    N    Y
##         N 1671   42
##         Y  192  152
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.862421e-01   5.054039e-01   8.717239e-01   8.996488e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.783557e-12   2.026854e-22 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       feats
## 1 WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      8.419                 1.976
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7277461                    0.7       0.5978351        0.8934084
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8826068             0.9010121     0.5566659   0.7084504
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.5650558        0.8862421
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8717239             0.8996488     0.5054039
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.003041136      0.02922293
##                label step_major step_minor     bgn     end elapsed
## 3 fit.models_1_rpart          3          0 250.408 263.087  12.679
## 4    fit.models_1_rf          4          0 263.087      NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log"
## [1] "no pca pre-processing for rf"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-14.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 85 on full training set
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-15.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-16.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted       4475   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           8950   matrix     numeric  
## oob.times       4475   -none-     numeric  
## classes            2   -none-     character
## importance       168   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           168   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-17.png) 

```
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.78759201
## 3        0.2 0.93217175
## 4        0.3 0.98358503
## 5        0.4 1.00000000
## 6        0.5 1.00000000
## 7        0.6 0.99866310
## 8        0.7 0.91997116
## 9        0.8 0.78704453
## 10       0.9 0.54280156
## 11       1.0 0.01062417
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.All.X.no.rnorm.rf.N
## 1            N                                     3726
## 2            Y                                       NA
##   Popular.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                       NA
## 2                                      749
##          Prediction
## Reference    N    Y
##         N 3726    0
##         Y    0  749
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      1.0000000      1.0000000      0.9991760      1.0000000      0.8326257 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-19.png) 

```
##    threshold     f.score
## 1        0.0 0.286547272
## 2        0.1 0.560632689
## 3        0.2 0.653802497
## 4        0.3 0.678331090
## 5        0.4 0.676384840
## 6        0.5 0.654723127
## 7        0.6 0.618962433
## 8        0.7 0.563600783
## 9        0.8 0.458515284
## 10       0.9 0.230179028
## 11       1.0 0.005797101
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_1-20.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.All.X.no.rnorm.rf.N
## 1            N                                     1566
## 2            Y                                       92
##   Popular.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                      147
## 2                                      252
##          Prediction
## Reference    N    Y
##         N 1566  147
##         Y   92  252
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.838114e-01   6.079058e-01   8.691702e-01   8.973483e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.275832e-11   4.776808e-04 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       feats
## 1 WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                    163.466                66.018
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.9065922
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.999176                     1     0.6331733   0.9217314
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.6783311        0.8838114
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8691702             0.8973483     0.6079058
```

```r
# User specified
    # easier to exclude features
#model_id_pfx <- "";
# indep_vars_vctr <- setdiff(names(glb_fitent_df), 
#                         union(union(glb_rsp_var, glb_exclude_vars_as_features), 
#                                 c("<feat1_name>", "<feat2_name>")))
# method <- ""                                

    # easier to include features
#model_id_pfx <- ""; indep_vars_vctr <- c("<feat1_name>", "<feat1_name>"); method <- ""

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitent_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitent_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitent_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                     model_id     model_method
## 1          MFO.myMFO_classfr    myMFO_classfr
## 2    Random.myrandom_classfr myrandom_classfr
## 3       Max.cor.Y.cv.0.rpart            rpart
## 4  Max.cor.Y.cv.0.cp.0.rpart            rpart
## 5            Max.cor.Y.rpart            rpart
## 6              Max.cor.Y.glm              glm
## 7    Interact.High.cor.Y.glm              glm
## 8              Low.cor.X.glm              glm
## 9                  All.X.glm              glm
## 10      All.X.no.rnorm.rpart            rpart
## 11         All.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A.nuppr.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A.nuppr.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A.nuppr.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A.nuppr.log
## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   A.nuppr.log, A.nuppr.log:A.npnct21.log, A.nuppr.log:H.npnct09.log, A.nuppr.log:H.npnct17.log, A.nuppr.log:S.can.log, A.nuppr.log:S.npnct01.log, A.nuppr.log:A.npnct23.log, A.nuppr.log:S.make.log, A.nuppr.log:A.npnct25.log, A.nuppr.log:S.npnct07.log, A.nuppr.log:A.npnct19.log, A.nuppr.log:S.npnct03.log, A.nuppr.log:A.npnct18.log, A.nuppr.log:A.npnct20.log, A.nuppr.log:S.has.year.colon, A.nuppr.log:S.npnct22.log, A.nuppr.log:S.presid.log, A.nuppr.log:S.npnct06.log, A.nuppr.log:A.npnct02.log, A.nuppr.log:S.take.log, A.nuppr.log:S.new.log, A.nuppr.log:S.npnct13.log, A.nuppr.log:S.npnct30.log, A.nuppr.log:S.day.log, A.nuppr.log:S.show.log, A.nuppr.log:S.report.log, A.nuppr.log:S.year.log, A.nuppr.log:S.share.log, A.nuppr.log:S.compani.log, A.nuppr.log:A.npnct14.log, A.nuppr.log:S.first.log, A.nuppr.log:S.time.log, A.nuppr.log:S.articl.log, A.nuppr.log:S.will.log, A.nuppr.log:S.newyork.log, A.nuppr.log:S.npnct04.log, A.nuppr.log:H.npnct15.log, A.nuppr.log:S.intern.log, A.nuppr.log:S.npnct16.log, A.nuppr.log:A.intern.log, A.nuppr.log:H.week.log, A.nuppr.log:S.fashion.log, A.nuppr.log:S.week.log, A.nuppr.log:S.npnct12.log, A.nuppr.log:S.ndgts.log, A.nuppr.log:H.nuppr.log, A.nuppr.log:A.nchrs.log, A.nuppr.log:A.nwrds.log, A.nuppr.log:S.nchrs.log, A.nuppr.log:S.nuppr.log
## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, A.npnct21.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, .rnorm, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.npnct20.log, S.has.year.colon, S.npnct22.log, H.npnct02.log, S.presid.log, S.npnct15.log, S.npnct06.log, H.npnct14.log, S.take.log, PubDate.minute.fctr, S.new.log, S.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, S.day.log, H.X2014.log, S.show.log, A.npnct14.log, S.report.log, S.year.log, H.npnct04.log, S.share.log, S.compani.log, H.new.log, S.first.log, S.time.log, H.newyork.log, S.articl.log, S.will.log, H.npnct15.log, S.newyork.log, H.day.log, S.npnct04.log, H.today.log, H.report.log, S.npnct16.log, S.intern.log, H.daili.log, H.week.log, H.npnct16.log, S.fashion.log, S.week.log, H.npnct30.log, S.npnct12.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, A.nchrs.log, A.nwrds.log, A.nwrds.unq.log, S.nuppr.log
## 9  WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, .rnorm, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log
## 10         WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log
## 11         WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log
##    max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1                0                      0.870                 0.003
## 2                0                      0.341                 0.002
## 3                0                      0.688                 0.053
## 4                0                      0.607                 0.056
## 5                1                      1.268                 0.056
## 6                1                      1.224                 0.079
## 7                1                      3.036                 1.064
## 8                1                      6.939                 3.268
## 9                1                     13.409                 6.870
## 10               3                      8.419                 1.976
## 11               3                    163.466                66.018
##    max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.5000000                    0.5       0.0000000        0.8326257
## 2    0.5007516                    0.1       0.2867534        0.1673743
## 3    0.5000000                    0.5       0.0000000        0.8326257
## 4    0.5000000                    0.5       0.0000000        0.8326257
## 5    0.5000000                    0.5       0.0000000        0.8326258
## 6    0.7073742                    0.2       0.3986014        0.8324022
## 7    0.7936512                    0.3       0.4666667        0.8420117
## 8    0.9486508                    0.4       0.7444668        0.9088264
## 9    0.8100366                    0.9       0.7059659        0.9021193
## 10   0.7277461                    0.7       0.5978351        0.8934084
## 11   1.0000000                    0.5       1.0000000        0.9065922
##    max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.8213602             0.8434553  0.0000000000   0.5000000
## 2              0.1565447             0.1786398  0.0000000000   0.4909227
## 3              0.8213602             0.8434553  0.0000000000   0.5000000
## 4              0.8213602             0.8434553  0.0000000000   0.5000000
## 5              0.8213602             0.8434553  0.0000000000   0.5000000
## 6              0.7176970             0.7439004 -0.0004459345   0.7102060
## 7              0.7840956             0.8079188  0.1154034120   0.7739949
## 8              0.9062982             0.9228764  0.6544785876   0.9262548
## 9              0.8986168             0.9158192  0.6295388414   0.7972982
## 10             0.8826068             0.9010121  0.5566658958   0.7084504
## 11             0.9991760             1.0000000  0.6331732713   0.9217314
##    opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                     0.5       0.0000000        0.8327662
## 2                     0.1       0.2865473        0.1672338
## 3                     0.5       0.0000000        0.8327662
## 4                     0.5       0.0000000        0.8327662
## 5                     0.5       0.0000000        0.8327662
## 6                     0.2       0.3880266        0.7316480
## 7                     0.3       0.4521073        0.7914439
## 8                     0.3       0.7257618        0.9037433
## 9                     0.9       0.6897638        0.9042295
## 10                    0.7       0.5650558        0.8862421
## 11                    0.3       0.6783311        0.8838114
##    max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.8159247             0.8486533     0.0000000
## 2              0.1513467             0.1840753     0.0000000
## 3              0.8159247             0.8486533     0.0000000
## 4              0.8159247             0.8486533     0.0000000
## 5              0.8159247             0.8486533     0.0000000
## 6              0.7119353             0.7506985     0.2283681
## 7              0.7732343             0.8088189     0.3256506
## 8              0.8901728             0.9161500     0.6675461
## 9              0.8906870             0.9166067     0.6336042
## 10             0.8717239             0.8996488     0.5054039
## 11             0.8691702             0.8973483     0.6079058
##    max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## 1                  NA              NA          NA
## 2                  NA              NA          NA
## 3                  NA              NA          NA
## 4                  NA              NA          NA
## 5        0.0002791548    0.0000000000          NA
## 6        0.0000648833    0.0007723812    3714.601
## 7        0.0020950664    0.0284927425    3424.717
## 8        0.0018087297    0.0041211095    2079.176
## 9        0.0140389674    0.0521779604   30122.145
## 10       0.0030411362    0.0292229339          NA
## 11                 NA              NA          NA
```

```r
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 4  fit.models_1_rf          4          0 263.087 432.436 169.349
## 5 fit.models_1_end          5          0 432.437      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          6          1 228.568 432.444 203.876
## 11 fit.models          6          2 432.444      NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitent_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBent_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
#     tmp_models_df <- orderBy(~model_id, glb_models_df)
#     rownames(tmp_models_df) <- seq(1, nrow(tmp_models_df))
#     all.equal(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr"),
#               subset(stats_df, model_id != "Random.myrandom_classfr"))
#     print(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])
#     print(subset(stats_df, model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])

    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id", grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df), grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                     model_id     model_method
## 1          MFO.myMFO_classfr    myMFO_classfr
## 2    Random.myrandom_classfr myrandom_classfr
## 3       Max.cor.Y.cv.0.rpart            rpart
## 4  Max.cor.Y.cv.0.cp.0.rpart            rpart
## 5            Max.cor.Y.rpart            rpart
## 6              Max.cor.Y.glm              glm
## 7    Interact.High.cor.Y.glm              glm
## 8              Low.cor.X.glm              glm
## 9                  All.X.glm              glm
## 10      All.X.no.rnorm.rpart            rpart
## 11         All.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A.nuppr.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A.nuppr.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A.nuppr.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A.nuppr.log
## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   A.nuppr.log, A.nuppr.log:A.npnct21.log, A.nuppr.log:H.npnct09.log, A.nuppr.log:H.npnct17.log, A.nuppr.log:S.can.log, A.nuppr.log:S.npnct01.log, A.nuppr.log:A.npnct23.log, A.nuppr.log:S.make.log, A.nuppr.log:A.npnct25.log, A.nuppr.log:S.npnct07.log, A.nuppr.log:A.npnct19.log, A.nuppr.log:S.npnct03.log, A.nuppr.log:A.npnct18.log, A.nuppr.log:A.npnct20.log, A.nuppr.log:S.has.year.colon, A.nuppr.log:S.npnct22.log, A.nuppr.log:S.presid.log, A.nuppr.log:S.npnct06.log, A.nuppr.log:A.npnct02.log, A.nuppr.log:S.take.log, A.nuppr.log:S.new.log, A.nuppr.log:S.npnct13.log, A.nuppr.log:S.npnct30.log, A.nuppr.log:S.day.log, A.nuppr.log:S.show.log, A.nuppr.log:S.report.log, A.nuppr.log:S.year.log, A.nuppr.log:S.share.log, A.nuppr.log:S.compani.log, A.nuppr.log:A.npnct14.log, A.nuppr.log:S.first.log, A.nuppr.log:S.time.log, A.nuppr.log:S.articl.log, A.nuppr.log:S.will.log, A.nuppr.log:S.newyork.log, A.nuppr.log:S.npnct04.log, A.nuppr.log:H.npnct15.log, A.nuppr.log:S.intern.log, A.nuppr.log:S.npnct16.log, A.nuppr.log:A.intern.log, A.nuppr.log:H.week.log, A.nuppr.log:S.fashion.log, A.nuppr.log:S.week.log, A.nuppr.log:S.npnct12.log, A.nuppr.log:S.ndgts.log, A.nuppr.log:H.nuppr.log, A.nuppr.log:A.nchrs.log, A.nuppr.log:A.nwrds.log, A.nuppr.log:S.nchrs.log, A.nuppr.log:S.nuppr.log
## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, A.npnct21.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, .rnorm, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.npnct20.log, S.has.year.colon, S.npnct22.log, H.npnct02.log, S.presid.log, S.npnct15.log, S.npnct06.log, H.npnct14.log, S.take.log, PubDate.minute.fctr, S.new.log, S.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, S.day.log, H.X2014.log, S.show.log, A.npnct14.log, S.report.log, S.year.log, H.npnct04.log, S.share.log, S.compani.log, H.new.log, S.first.log, S.time.log, H.newyork.log, S.articl.log, S.will.log, H.npnct15.log, S.newyork.log, H.day.log, S.npnct04.log, H.today.log, H.report.log, S.npnct16.log, S.intern.log, H.daili.log, H.week.log, H.npnct16.log, S.fashion.log, S.week.log, H.npnct30.log, S.npnct12.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, A.nchrs.log, A.nwrds.log, A.nwrds.unq.log, S.nuppr.log
## 9  WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, .rnorm, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log
## 10         WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log
## 11         WordCount.log, PubDate.hour.fctr, H.npnct21.log, PubDate.wkend, S.npnct21.log, A.npnct21.log, H.npnct08.log, H.npnct09.log, PubDate.last10.log, PubDate.last1.log, H.npnct06.log, A.can.log, A.npnct01.log, S.npnct01.log, S.can.log, H.npnct17.log, H.has.ebola, A.make.log, S.make.log, H.npnct01.log, H.npnct12.log, myCategory.fctr, S.state.log, A.state.log, S.one.log, A.one.log, A.said.log, S.said.log, A.npnct17.log, S.npnct17.log, S.npnct08.log, A.npnct08.log, S.npnct09.log, A.npnct09.log, PubDate.last100.log, H.npnct05.log, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, A.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct19.log, H.npnct13.log, A.has.http, A.npnct03.log, A.npnct02.log, A.npnct18.log, A.npnct20.log, A.has.year.colon, S.has.year.colon, A.npnct22.log, S.npnct22.log, H.npnct02.log, A.presid.log, S.presid.log, S.npnct15.log, A.npnct06.log, S.npnct06.log, A.npnct15.log, H.npnct14.log, S.take.log, A.take.log, PubDate.minute.fctr, S.new.log, A.new.log, S.npnct13.log, A.npnct13.log, PubDate.wkday.fctr, S.npnct30.log, A.npnct30.log, S.day.log, A.day.log, H.X2014.log, A.show.log, S.show.log, A.npnct14.log, A.report.log, S.report.log, A.year.log, S.year.log, H.npnct04.log, A.share.log, S.share.log, S.compani.log, A.compani.log, H.new.log, S.npnct14.log, A.first.log, S.first.log, S.time.log, A.time.log, H.newyork.log, A.articl.log, S.articl.log, S.will.log, A.will.log, H.npnct15.log, A.newyork.log, S.newyork.log, H.day.log, A.npnct04.log, S.npnct04.log, H.today.log, H.report.log, H.X2015.log, S.npnct16.log, A.intern.log, S.intern.log, A.npnct16.log, H.daili.log, H.week.log, H.has.year.colon, H.fashion.log, H.npnct16.log, A.fashion.log, S.fashion.log, A.week.log, S.week.log, H.npnct30.log, S.npnct12.log, A.npnct12.log, H.ndgts.log, S.ndgts.log, A.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.log, H.nwrds.unq.log, A.nchrs.log, S.nchrs.log, A.nwrds.log, S.nwrds.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log, A.nuppr.log
##    max.nTuningRuns max.auc.fit opt.prob.threshold.fit max.f.score.fit
## 1                0   0.5000000                    0.5       0.0000000
## 2                0   0.5007516                    0.1       0.2867534
## 3                0   0.5000000                    0.5       0.0000000
## 4                0   0.5000000                    0.5       0.0000000
## 5                1   0.5000000                    0.5       0.0000000
## 6                1   0.7073742                    0.2       0.3986014
## 7                1   0.7936512                    0.3       0.4666667
## 8                1   0.9486508                    0.4       0.7444668
## 9                1   0.8100366                    0.9       0.7059659
## 10               3   0.7277461                    0.7       0.5978351
## 11               3   1.0000000                    0.5       1.0000000
##    max.Accuracy.fit max.Kappa.fit max.auc.OOB opt.prob.threshold.OOB
## 1         0.8326257  0.0000000000   0.5000000                    0.5
## 2         0.1673743  0.0000000000   0.4909227                    0.1
## 3         0.8326257  0.0000000000   0.5000000                    0.5
## 4         0.8326257  0.0000000000   0.5000000                    0.5
## 5         0.8326258  0.0000000000   0.5000000                    0.5
## 6         0.8324022 -0.0004459345   0.7102060                    0.2
## 7         0.8420117  0.1154034120   0.7739949                    0.3
## 8         0.9088264  0.6544785876   0.9262548                    0.3
## 9         0.9021193  0.6295388414   0.7972982                    0.9
## 10        0.8934084  0.5566658958   0.7084504                    0.7
## 11        0.9065922  0.6331732713   0.9217314                    0.3
##    max.f.score.OOB max.Accuracy.OOB max.Kappa.OOB
## 1        0.0000000        0.8327662     0.0000000
## 2        0.2865473        0.1672338     0.0000000
## 3        0.0000000        0.8327662     0.0000000
## 4        0.0000000        0.8327662     0.0000000
## 5        0.0000000        0.8327662     0.0000000
## 6        0.3880266        0.7316480     0.2283681
## 7        0.4521073        0.7914439     0.3256506
## 8        0.7257618        0.9037433     0.6675461
## 9        0.6897638        0.9042295     0.6336042
## 10       0.5650558        0.8862421     0.5054039
## 11       0.6783311        0.8838114     0.6079058
##    inv.elapsedtime.everything inv.elapsedtime.final  inv.aic.fit
## 1                  1.14942529          333.33333333           NA
## 2                  2.93255132          500.00000000           NA
## 3                  1.45348837           18.86792453           NA
## 4                  1.64744646           17.85714286           NA
## 5                  0.78864353           17.85714286           NA
## 6                  0.81699346           12.65822785 2.692079e-04
## 7                  0.32938076            0.93984962 2.919949e-04
## 8                  0.14411298            0.30599755 4.809597e-04
## 9                  0.07457678            0.14556041 3.319817e-05
## 10                 0.11877895            0.50607287           NA
## 11                 0.00611748            0.01514738           NA
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

```
## Warning: Removed 5 rows containing missing values (geom_path).
```

```
## Warning: Removed 74 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## Warning: max.AccuracyUpper.fit already exists in glb_models_df
```

```
## [1] "var:max.KappaSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
#print(mltdCI_models_df)
# castCI_models_df <- dcast(mltdCI_models_df, value ~ type, fun.aggregate=sum)
# print(castCI_models_df)
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
```

```
## Warning: Stacking not well defined when ymin != 0
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

```
## Warning: Stacking not well defined when ymin != 0
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)
      [, c("model_id", glb_model_evl_criteria, 
           ifelse(glb_is_classification && glb_is_binomial, 
                  "opt.prob.threshold.OOB", NULL))])
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 9                  All.X.glm        0.9042295   0.7972982     0.6336042
## 8              Low.cor.X.glm        0.9037433   0.9262548     0.6675461
## 10      All.X.no.rnorm.rpart        0.8862421   0.7084504     0.5054039
## 11         All.X.no.rnorm.rf        0.8838114   0.9217314     0.6079058
## 1          MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.8327662   0.5000000     0.0000000
## 5            Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 7    Interact.High.cor.Y.glm        0.7914439   0.7739949     0.3256506
## 6              Max.cor.Y.glm        0.7316480   0.7102060     0.2283681
## 2    Random.myrandom_classfr        0.1672338   0.4909227     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 9    30122.145                    0.9
## 8     2079.176                    0.3
## 10          NA                    0.7
## 11          NA                    0.3
## 1           NA                    0.5
## 3           NA                    0.5
## 4           NA                    0.5
## 5           NA                    0.5
## 7     3424.717                    0.3
## 6     3714.601                    0.2
## 2           NA                    0.1
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

```
## Warning: Removed 33 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~-max.Accuracy.OOB - max.auc.OOB - max.Kappa.OOB + min.aic.fit - 
##     opt.prob.threshold.OOB
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: All.X.glm"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
    if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
        warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
        glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
    }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

```
## Warning: not plotting observations with leverage one:
##   1143, 2501, 3625, 3637, 3799, 4105, 4408
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-4.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-5.png) 

```
## Warning: not plotting observations with leverage one:
##   1143, 2501, 3625, 3637, 3799, 4105, 4408
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-6.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -8.49    0.00    0.00    0.00    8.49  
## 
## Coefficients: (31 not defined because of singularities)
##                                                         Estimate
## (Intercept)                                           -9.447e+14
## WordCount.log                                          4.908e+14
## `PubDate.hour.fctr(7.67,15.3]`                         1.748e+14
## `PubDate.hour.fctr(15.3,23]`                           6.615e+14
## H.npnct21.log                                          1.242e+15
## PubDate.wkend                                         -3.716e+13
## S.npnct21.log                                         -2.188e+15
## A.npnct21.log                                          3.821e+15
## H.npnct08.log                                          1.041e+15
## H.npnct09.log                                                 NA
## PubDate.last10.log                                     1.468e+14
## PubDate.last1.log                                     -5.579e+12
## H.npnct06.log                                          8.977e+14
## A.can.log                                              9.731e+15
## A.npnct01.log                                          9.868e+14
## S.npnct01.log                                                 NA
## S.can.log                                             -8.487e+15
## H.npnct17.log                                         -2.481e+14
## H.has.ebola                                            9.863e+13
## A.make.log                                             1.094e+15
## S.make.log                                                    NA
## H.npnct01.log                                          2.972e+14
## H.npnct12.log                                         -8.269e+13
## `myCategory.fctrForeign#World#Asia Pacific`           -3.868e+15
## `myCategory.fctr#Multimedia#`                         -3.856e+15
## `myCategory.fctrCulture#Arts#`                        -2.956e+15
## `myCategory.fctrBusiness#Business Day#Dealbook`       -3.209e+15
## myCategory.fctrmyOther                                -3.205e+15
## `myCategory.fctrBusiness#Technology#`                 -2.871e+15
## `myCategory.fctrBusiness#Crosswords/Games#`           -1.662e+14
## `myCategory.fctrTStyle##`                             -2.835e+15
## `myCategory.fctrForeign#World#`                       -2.009e+15
## `myCategory.fctrOpEd#Opinion#`                         9.667e+14
## `myCategory.fctrStyles##Fashion`                      -4.023e+15
## `myCategory.fctr#Opinion#Room For Debate`             -5.282e+15
## `myCategory.fctr#U.S.#Education`                      -6.503e+15
## `myCategory.fctr##`                                   -3.413e+15
## `myCategory.fctrMetro#N.Y. / Region#`                 -3.186e+15
## `myCategory.fctrBusiness#Business Day#Small Business` -3.525e+15
## `myCategory.fctrStyles#U.S.#`                         -8.377e+13
## `myCategory.fctrTravel#Travel#`                       -2.945e+15
## `myCategory.fctr#Opinion#The Public Editor`            1.634e+15
## S.state.log                                            1.774e+16
## A.state.log                                           -1.452e+16
## S.one.log                                             -5.269e+15
## A.one.log                                              5.178e+15
## A.said.log                                             6.214e+14
## S.said.log                                                    NA
## A.npnct17.log                                         -8.489e+14
## S.npnct17.log                                                 NA
## S.npnct08.log                                          2.157e+15
## A.npnct08.log                                                 NA
## S.npnct09.log                                         -1.195e+15
## A.npnct09.log                                                 NA
## PubDate.last100.log                                    2.795e+13
## .rnorm                                                -6.296e+13
## H.npnct05.log                                         -1.567e+15
## `PubDate.date.fctr(7,13]`                              9.714e+13
## `PubDate.date.fctr(13,19]`                            -1.235e+13
## `PubDate.date.fctr(19,25]`                             1.877e+13
## `PubDate.date.fctr(25,31]`                             1.610e+14
## `PubDate.second.fctr(14.8,29.5]`                       2.563e+13
## `PubDate.second.fctr(29.5,44.2]`                       5.194e+13
## `PubDate.second.fctr(44.2,59.1]`                      -7.845e+13
## H.npnct07.log                                          1.655e+14
## A.npnct07.log                                         -6.395e+14
## S.npnct07.log                                                 NA
## S.npnct03.log                                          3.179e+14
## A.npnct19.log                                          3.597e+16
## H.npnct13.log                                          2.953e+14
## A.has.http                                                    NA
## A.npnct03.log                                                 NA
## A.npnct02.log                                          4.811e+14
## A.npnct18.log                                          1.205e+16
## A.npnct20.log                                                 NA
## A.has.year.colon                                       1.478e+14
## S.has.year.colon                                              NA
## A.npnct22.log                                          1.087e+15
## S.npnct22.log                                                 NA
## H.npnct02.log                                         -3.379e+13
## A.presid.log                                           3.207e+14
## S.presid.log                                                  NA
## S.npnct15.log                                          2.152e+16
## A.npnct06.log                                         -2.430e+14
## S.npnct06.log                                                 NA
## A.npnct15.log                                         -2.113e+16
## H.npnct14.log                                         -1.404e+13
## S.take.log                                            -2.134e+14
## A.take.log                                                    NA
## `PubDate.minute.fctr(14.8,29.5]`                      -1.461e+14
## `PubDate.minute.fctr(29.5,44.2]`                      -9.707e+13
## `PubDate.minute.fctr(44.2,59.1]`                      -1.399e+13
## S.new.log                                              3.170e+15
## A.new.log                                             -3.070e+15
## S.npnct13.log                                         -2.045e+15
## A.npnct13.log                                          2.039e+15
## PubDate.wkday.fctr1                                   -7.566e+13
## PubDate.wkday.fctr2                                   -2.884e+14
## PubDate.wkday.fctr3                                   -1.565e+14
## PubDate.wkday.fctr4                                   -1.658e+14
## PubDate.wkday.fctr5                                   -2.091e+14
## PubDate.wkday.fctr6                                   -4.855e+14
## S.npnct30.log                                         -3.633e+15
## A.npnct30.log                                          2.711e+15
## S.day.log                                             -2.921e+14
## A.day.log                                                     NA
## H.X2014.log                                            4.301e+14
## A.show.log                                            -4.676e+14
## S.show.log                                                    NA
## A.npnct14.log                                          2.796e+14
## A.report.log                                          -9.455e+14
## S.report.log                                                  NA
## A.year.log                                            -2.529e+14
## S.year.log                                                    NA
## H.npnct04.log                                         -8.797e+14
## A.share.log                                           -8.119e+14
## S.share.log                                                   NA
## S.compani.log                                         -2.613e+14
## A.compani.log                                                 NA
## H.new.log                                             -6.768e+14
## S.npnct14.log                                          2.368e+14
## A.first.log                                           -6.177e+13
## S.first.log                                                   NA
## S.time.log                                            -3.788e+14
## A.time.log                                                    NA
## H.newyork.log                                         -6.853e+13
## A.articl.log                                           1.199e+15
## S.articl.log                                                  NA
## S.will.log                                             3.765e+15
## A.will.log                                            -4.114e+15
## H.npnct15.log                                         -4.845e+14
## A.newyork.log                                          4.852e+14
## S.newyork.log                                                 NA
## H.day.log                                              7.724e+13
## A.npnct04.log                                         -7.641e+14
## S.npnct04.log                                                 NA
## H.today.log                                           -1.053e+15
## H.report.log                                          -3.294e+14
## H.X2015.log                                            2.301e+15
## S.npnct16.log                                          3.428e+14
## A.intern.log                                          -8.385e+14
## S.intern.log                                                  NA
## A.npnct16.log                                                 NA
## H.daili.log                                            1.739e+13
## H.week.log                                            -6.011e+14
## H.has.year.colon                                       1.197e+15
## H.fashion.log                                          3.386e+14
## H.npnct16.log                                         -2.156e+13
## A.fashion.log                                         -1.494e+15
## S.fashion.log                                                 NA
## A.week.log                                            -3.959e+14
## S.week.log                                                    NA
## H.npnct30.log                                          1.581e+15
## S.npnct12.log                                         -8.402e+15
## A.npnct12.log                                          8.350e+15
## H.ndgts.log                                            3.205e+14
## S.ndgts.log                                           -1.457e+15
## A.ndgts.log                                            1.320e+15
## H.nuppr.log                                            3.456e+14
## H.nchrs.log                                           -2.818e+14
## H.nwrds.log                                           -1.434e+14
## H.nwrds.unq.log                                       -4.929e+14
## A.nchrs.log                                           -8.539e+15
## S.nchrs.log                                            8.168e+15
## A.nwrds.log                                           -8.951e+15
## S.nwrds.log                                            9.525e+15
## A.nwrds.unq.log                                        1.404e+16
## S.nwrds.unq.log                                       -1.464e+16
## S.nuppr.log                                           -4.014e+15
## A.nuppr.log                                            3.559e+15
##                                                       Std. Error
## (Intercept)                                            3.559e+07
## WordCount.log                                          1.219e+06
## `PubDate.hour.fctr(7.67,15.3]`                         3.797e+06
## `PubDate.hour.fctr(15.3,23]`                           4.044e+06
## H.npnct21.log                                          6.619e+06
## PubDate.wkend                                          7.776e+06
## S.npnct21.log                                          9.958e+07
## A.npnct21.log                                          9.953e+07
## H.npnct08.log                                          1.489e+07
## H.npnct09.log                                                 NA
## PubDate.last10.log                                     1.954e+06
## PubDate.last1.log                                      7.091e+05
## H.npnct06.log                                          1.670e+07
## A.can.log                                              2.057e+08
## A.npnct01.log                                          2.679e+07
## S.npnct01.log                                                 NA
## S.can.log                                              2.062e+08
## H.npnct17.log                                          1.784e+07
## H.has.ebola                                            8.881e+06
## A.make.log                                             7.668e+06
## S.make.log                                                    NA
## H.npnct01.log                                          2.340e+07
## H.npnct12.log                                          3.661e+06
## `myCategory.fctrForeign#World#Asia Pacific`            8.774e+06
## `myCategory.fctr#Multimedia#`                          1.018e+07
## `myCategory.fctrCulture#Arts#`                         7.372e+06
## `myCategory.fctrBusiness#Business Day#Dealbook`        7.183e+06
## myCategory.fctrmyOther                                 1.513e+07
## `myCategory.fctrBusiness#Technology#`                  7.949e+06
## `myCategory.fctrBusiness#Crosswords/Games#`            1.047e+07
## `myCategory.fctrTStyle##`                              7.317e+06
## `myCategory.fctrForeign#World#`                        1.735e+07
## `myCategory.fctrOpEd#Opinion#`                         7.377e+06
## `myCategory.fctrStyles##Fashion`                       1.128e+07
## `myCategory.fctr#Opinion#Room For Debate`              1.181e+07
## `myCategory.fctr#U.S.#Education`                       1.138e+07
## `myCategory.fctr##`                                    6.801e+06
## `myCategory.fctrMetro#N.Y. / Region#`                  9.712e+06
## `myCategory.fctrBusiness#Business Day#Small Business`  9.909e+06
## `myCategory.fctrStyles#U.S.#`                          8.958e+06
## `myCategory.fctrTravel#Travel#`                        9.798e+06
## `myCategory.fctr#Opinion#The Public Editor`            2.252e+07
## S.state.log                                            3.414e+08
## A.state.log                                            3.414e+08
## S.one.log                                              1.684e+08
## A.one.log                                              1.686e+08
## A.said.log                                             7.938e+06
## S.said.log                                                    NA
## A.npnct17.log                                          2.100e+07
## S.npnct17.log                                                 NA
## S.npnct08.log                                          5.207e+07
## A.npnct08.log                                                 NA
## S.npnct09.log                                          4.868e+07
## A.npnct09.log                                                 NA
## PubDate.last100.log                                    7.782e+05
## .rnorm                                                 1.028e+06
## H.npnct05.log                                          4.048e+07
## `PubDate.date.fctr(7,13]`                              3.232e+06
## `PubDate.date.fctr(13,19]`                             3.186e+06
## `PubDate.date.fctr(19,25]`                             3.079e+06
## `PubDate.date.fctr(25,31]`                             3.432e+06
## `PubDate.second.fctr(14.8,29.5]`                       2.860e+06
## `PubDate.second.fctr(29.5,44.2]`                       2.823e+06
## `PubDate.second.fctr(44.2,59.1]`                       2.883e+06
## H.npnct07.log                                          2.982e+06
## A.npnct07.log                                          5.011e+07
## S.npnct07.log                                                 NA
## S.npnct03.log                                          4.404e+07
## A.npnct19.log                                          7.276e+08
## H.npnct13.log                                          5.228e+06
## A.has.http                                                    NA
## A.npnct03.log                                                 NA
## A.npnct02.log                                          6.300e+07
## A.npnct18.log                                          2.198e+08
## A.npnct20.log                                                 NA
## A.has.year.colon                                       2.385e+07
## S.has.year.colon                                              NA
## A.npnct22.log                                          3.172e+07
## S.npnct22.log                                                 NA
## H.npnct02.log                                          2.532e+07
## A.presid.log                                           7.645e+06
## S.presid.log                                                  NA
## S.npnct15.log                                          3.378e+08
## A.npnct06.log                                          1.791e+07
## S.npnct06.log                                                 NA
## A.npnct15.log                                          3.373e+08
## H.npnct14.log                                          3.444e+06
## S.take.log                                             8.391e+06
## A.take.log                                                    NA
## `PubDate.minute.fctr(14.8,29.5]`                       2.937e+06
## `PubDate.minute.fctr(29.5,44.2]`                       2.776e+06
## `PubDate.minute.fctr(44.2,59.1]`                       2.998e+06
## S.new.log                                              1.496e+08
## A.new.log                                              1.494e+08
## S.npnct13.log                                          4.866e+07
## A.npnct13.log                                          4.851e+07
## PubDate.wkday.fctr1                                    9.451e+06
## PubDate.wkday.fctr2                                    1.006e+07
## PubDate.wkday.fctr3                                    1.001e+07
## PubDate.wkday.fctr4                                    9.860e+06
## PubDate.wkday.fctr5                                    9.986e+06
## PubDate.wkday.fctr6                                    7.826e+06
## S.npnct30.log                                          2.072e+08
## A.npnct30.log                                          2.058e+08
## S.day.log                                              8.907e+06
## A.day.log                                                     NA
## H.X2014.log                                            1.620e+07
## A.show.log                                             8.303e+06
## S.show.log                                                    NA
## A.npnct14.log                                          2.831e+07
## A.report.log                                           9.142e+06
## S.report.log                                                  NA
## A.year.log                                             6.828e+06
## S.year.log                                                    NA
## H.npnct04.log                                          1.190e+07
## A.share.log                                            8.761e+06
## S.share.log                                                   NA
## S.compani.log                                          6.664e+06
## A.compani.log                                                 NA
## H.new.log                                              8.030e+06
## S.npnct14.log                                          2.810e+07
## A.first.log                                            8.267e+06
## S.first.log                                                   NA
## S.time.log                                             6.816e+06
## A.time.log                                                    NA
## H.newyork.log                                          9.799e+06
## A.articl.log                                           1.201e+07
## S.articl.log                                                  NA
## S.will.log                                             1.030e+08
## A.will.log                                             1.029e+08
## H.npnct15.log                                          2.330e+07
## A.newyork.log                                          7.217e+06
## S.newyork.log                                                 NA
## H.day.log                                              1.027e+07
## A.npnct04.log                                          8.063e+06
## S.npnct04.log                                                 NA
## H.today.log                                            1.208e+07
## H.report.log                                           1.258e+07
## H.X2015.log                                            2.464e+07
## S.npnct16.log                                          8.188e+06
## A.intern.log                                           1.491e+07
## S.intern.log                                                  NA
## A.npnct16.log                                                 NA
## H.daili.log                                            1.584e+07
## H.week.log                                             1.319e+07
## H.has.year.colon                                       1.430e+07
## H.fashion.log                                          1.501e+07
## H.npnct16.log                                          4.811e+06
## A.fashion.log                                          1.136e+07
## S.fashion.log                                                 NA
## A.week.log                                             6.934e+06
## S.week.log                                                    NA
## H.npnct30.log                                          1.469e+07
## S.npnct12.log                                          1.791e+08
## A.npnct12.log                                          1.790e+08
## H.ndgts.log                                            4.098e+06
## S.ndgts.log                                            3.677e+07
## A.ndgts.log                                            3.667e+07
## H.nuppr.log                                            7.529e+06
## H.nchrs.log                                            7.535e+06
## H.nwrds.log                                            3.854e+07
## H.nwrds.unq.log                                        3.787e+07
## A.nchrs.log                                            4.519e+08
## S.nchrs.log                                            4.518e+08
## A.nwrds.log                                            7.378e+08
## S.nwrds.log                                            7.379e+08
## A.nwrds.unq.log                                        5.933e+08
## S.nwrds.unq.log                                        5.932e+08
## S.nuppr.log                                            1.054e+08
## A.nuppr.log                                            1.054e+08
##                                                          z value Pr(>|z|)
## (Intercept)                                            -26544426   <2e-16
## WordCount.log                                          402748115   <2e-16
## `PubDate.hour.fctr(7.67,15.3]`                          46035233   <2e-16
## `PubDate.hour.fctr(15.3,23]`                           163594019   <2e-16
## H.npnct21.log                                          187659261   <2e-16
## PubDate.wkend                                           -4778830   <2e-16
## S.npnct21.log                                          -21970579   <2e-16
## A.npnct21.log                                           38389620   <2e-16
## H.npnct08.log                                           69905112   <2e-16
## H.npnct09.log                                                 NA       NA
## PubDate.last10.log                                      75105174   <2e-16
## PubDate.last1.log                                       -7868539   <2e-16
## H.npnct06.log                                           53768602   <2e-16
## A.can.log                                               47310920   <2e-16
## A.npnct01.log                                           36827222   <2e-16
## S.npnct01.log                                                 NA       NA
## S.can.log                                              -41149445   <2e-16
## H.npnct17.log                                          -13906966   <2e-16
## H.has.ebola                                             11105687   <2e-16
## A.make.log                                             142663085   <2e-16
## S.make.log                                                    NA       NA
## H.npnct01.log                                           12702040   <2e-16
## H.npnct12.log                                          -22585094   <2e-16
## `myCategory.fctrForeign#World#Asia Pacific`           -440918229   <2e-16
## `myCategory.fctr#Multimedia#`                         -378666617   <2e-16
## `myCategory.fctrCulture#Arts#`                        -400964516   <2e-16
## `myCategory.fctrBusiness#Business Day#Dealbook`       -446802417   <2e-16
## myCategory.fctrmyOther                                -211785090   <2e-16
## `myCategory.fctrBusiness#Technology#`                 -361145227   <2e-16
## `myCategory.fctrBusiness#Crosswords/Games#`            -15869663   <2e-16
## `myCategory.fctrTStyle##`                             -387523258   <2e-16
## `myCategory.fctrForeign#World#`                       -115739228   <2e-16
## `myCategory.fctrOpEd#Opinion#`                         131038302   <2e-16
## `myCategory.fctrStyles##Fashion`                      -356671576   <2e-16
## `myCategory.fctr#Opinion#Room For Debate`             -447081085   <2e-16
## `myCategory.fctr#U.S.#Education`                      -571361362   <2e-16
## `myCategory.fctr##`                                   -501870028   <2e-16
## `myCategory.fctrMetro#N.Y. / Region#`                 -328006158   <2e-16
## `myCategory.fctrBusiness#Business Day#Small Business` -355738103   <2e-16
## `myCategory.fctrStyles#U.S.#`                           -9351322   <2e-16
## `myCategory.fctrTravel#Travel#`                       -300556630   <2e-16
## `myCategory.fctr#Opinion#The Public Editor`             72559625   <2e-16
## S.state.log                                             51970619   <2e-16
## A.state.log                                            -42516911   <2e-16
## S.one.log                                              -31289229   <2e-16
## A.one.log                                               30714676   <2e-16
## A.said.log                                              78280670   <2e-16
## S.said.log                                                    NA       NA
## A.npnct17.log                                          -40431083   <2e-16
## S.npnct17.log                                                 NA       NA
## S.npnct08.log                                           41419696   <2e-16
## A.npnct08.log                                                 NA       NA
## S.npnct09.log                                          -24549282   <2e-16
## A.npnct09.log                                                 NA       NA
## PubDate.last100.log                                     35917907   <2e-16
## .rnorm                                                 -61275955   <2e-16
## H.npnct05.log                                          -38720733   <2e-16
## `PubDate.date.fctr(7,13]`                               30052149   <2e-16
## `PubDate.date.fctr(13,19]`                              -3875972   <2e-16
## `PubDate.date.fctr(19,25]`                               6096550   <2e-16
## `PubDate.date.fctr(25,31]`                              46922910   <2e-16
## `PubDate.second.fctr(14.8,29.5]`                         8962778   <2e-16
## `PubDate.second.fctr(29.5,44.2]`                        18402512   <2e-16
## `PubDate.second.fctr(44.2,59.1]`                       -27213882   <2e-16
## H.npnct07.log                                           55482694   <2e-16
## A.npnct07.log                                          -12761386   <2e-16
## S.npnct07.log                                                 NA       NA
## S.npnct03.log                                            7218966   <2e-16
## A.npnct19.log                                           49439608   <2e-16
## H.npnct13.log                                           56482498   <2e-16
## A.has.http                                                    NA       NA
## A.npnct03.log                                                 NA       NA
## A.npnct02.log                                            7636804   <2e-16
## A.npnct18.log                                           54815431   <2e-16
## A.npnct20.log                                                 NA       NA
## A.has.year.colon                                         6196380   <2e-16
## S.has.year.colon                                              NA       NA
## A.npnct22.log                                           34281885   <2e-16
## S.npnct22.log                                                 NA       NA
## H.npnct02.log                                           -1334378   <2e-16
## A.presid.log                                            41945768   <2e-16
## S.presid.log                                                  NA       NA
## S.npnct15.log                                           63704223   <2e-16
## A.npnct06.log                                          -13567638   <2e-16
## S.npnct06.log                                                 NA       NA
## A.npnct15.log                                          -62646168   <2e-16
## H.npnct14.log                                           -4076244   <2e-16
## S.take.log                                             -25432453   <2e-16
## A.take.log                                                    NA       NA
## `PubDate.minute.fctr(14.8,29.5]`                       -49746763   <2e-16
## `PubDate.minute.fctr(29.5,44.2]`                       -34971274   <2e-16
## `PubDate.minute.fctr(44.2,59.1]`                        -4664975   <2e-16
## S.new.log                                               21186768   <2e-16
## A.new.log                                              -20549279   <2e-16
## S.npnct13.log                                          -42037279   <2e-16
## A.npnct13.log                                           42030083   <2e-16
## PubDate.wkday.fctr1                                     -8005232   <2e-16
## PubDate.wkday.fctr2                                    -28661306   <2e-16
## PubDate.wkday.fctr3                                    -15634792   <2e-16
## PubDate.wkday.fctr4                                    -16811492   <2e-16
## PubDate.wkday.fctr5                                    -20935621   <2e-16
## PubDate.wkday.fctr6                                    -62035818   <2e-16
## S.npnct30.log                                          -17529790   <2e-16
## A.npnct30.log                                           13176029   <2e-16
## S.day.log                                              -32790424   <2e-16
## A.day.log                                                     NA       NA
## H.X2014.log                                             26558128   <2e-16
## A.show.log                                             -56322215   <2e-16
## S.show.log                                                    NA       NA
## A.npnct14.log                                            9876652   <2e-16
## A.report.log                                          -103425376   <2e-16
## S.report.log                                                  NA       NA
## A.year.log                                             -37043182   <2e-16
## S.year.log                                                    NA       NA
## H.npnct04.log                                          -73915998   <2e-16
## A.share.log                                            -92668869   <2e-16
## S.share.log                                                   NA       NA
## S.compani.log                                          -39209719   <2e-16
## A.compani.log                                                 NA       NA
## H.new.log                                              -84292376   <2e-16
## S.npnct14.log                                            8426715   <2e-16
## A.first.log                                             -7471487   <2e-16
## S.first.log                                                   NA       NA
## S.time.log                                             -55576089   <2e-16
## A.time.log                                                    NA       NA
## H.newyork.log                                           -6993368   <2e-16
## A.articl.log                                            99843475   <2e-16
## S.articl.log                                                  NA       NA
## S.will.log                                              36567174   <2e-16
## A.will.log                                             -39972218   <2e-16
## H.npnct15.log                                          -20793499   <2e-16
## A.newyork.log                                           67232099   <2e-16
## S.newyork.log                                                 NA       NA
## H.day.log                                                7522782   <2e-16
## A.npnct04.log                                          -94776214   <2e-16
## S.npnct04.log                                                 NA       NA
## H.today.log                                            -87153602   <2e-16
## H.report.log                                           -26190720   <2e-16
## H.X2015.log                                             93410695   <2e-16
## S.npnct16.log                                           41868159   <2e-16
## A.intern.log                                           -56220975   <2e-16
## S.intern.log                                                  NA       NA
## A.npnct16.log                                                 NA       NA
## H.daili.log                                              1097436   <2e-16
## H.week.log                                             -45556770   <2e-16
## H.has.year.colon                                        83708330   <2e-16
## H.fashion.log                                           22561643   <2e-16
## H.npnct16.log                                           -4481965   <2e-16
## A.fashion.log                                         -131505345   <2e-16
## S.fashion.log                                                 NA       NA
## A.week.log                                             -57095008   <2e-16
## S.week.log                                                    NA       NA
## H.npnct30.log                                          107559412   <2e-16
## S.npnct12.log                                          -46916170   <2e-16
## A.npnct12.log                                           46639912   <2e-16
## H.ndgts.log                                             78211121   <2e-16
## S.ndgts.log                                            -39634064   <2e-16
## A.ndgts.log                                             36010722   <2e-16
## H.nuppr.log                                             45899612   <2e-16
## H.nchrs.log                                            -37398480   <2e-16
## H.nwrds.log                                             -3721869   <2e-16
## H.nwrds.unq.log                                        -13016909   <2e-16
## A.nchrs.log                                            -18894333   <2e-16
## S.nchrs.log                                             18080056   <2e-16
## A.nwrds.log                                            -12132798   <2e-16
## S.nwrds.log                                             12909056   <2e-16
## A.nwrds.unq.log                                         23662101   <2e-16
## S.nwrds.unq.log                                        -24676382   <2e-16
## S.nuppr.log                                            -38091413   <2e-16
## A.nuppr.log                                             33776771   <2e-16
##                                                          
## (Intercept)                                           ***
## WordCount.log                                         ***
## `PubDate.hour.fctr(7.67,15.3]`                        ***
## `PubDate.hour.fctr(15.3,23]`                          ***
## H.npnct21.log                                         ***
## PubDate.wkend                                         ***
## S.npnct21.log                                         ***
## A.npnct21.log                                         ***
## H.npnct08.log                                         ***
## H.npnct09.log                                            
## PubDate.last10.log                                    ***
## PubDate.last1.log                                     ***
## H.npnct06.log                                         ***
## A.can.log                                             ***
## A.npnct01.log                                         ***
## S.npnct01.log                                            
## S.can.log                                             ***
## H.npnct17.log                                         ***
## H.has.ebola                                           ***
## A.make.log                                            ***
## S.make.log                                               
## H.npnct01.log                                         ***
## H.npnct12.log                                         ***
## `myCategory.fctrForeign#World#Asia Pacific`           ***
## `myCategory.fctr#Multimedia#`                         ***
## `myCategory.fctrCulture#Arts#`                        ***
## `myCategory.fctrBusiness#Business Day#Dealbook`       ***
## myCategory.fctrmyOther                                ***
## `myCategory.fctrBusiness#Technology#`                 ***
## `myCategory.fctrBusiness#Crosswords/Games#`           ***
## `myCategory.fctrTStyle##`                             ***
## `myCategory.fctrForeign#World#`                       ***
## `myCategory.fctrOpEd#Opinion#`                        ***
## `myCategory.fctrStyles##Fashion`                      ***
## `myCategory.fctr#Opinion#Room For Debate`             ***
## `myCategory.fctr#U.S.#Education`                      ***
## `myCategory.fctr##`                                   ***
## `myCategory.fctrMetro#N.Y. / Region#`                 ***
## `myCategory.fctrBusiness#Business Day#Small Business` ***
## `myCategory.fctrStyles#U.S.#`                         ***
## `myCategory.fctrTravel#Travel#`                       ***
## `myCategory.fctr#Opinion#The Public Editor`           ***
## S.state.log                                           ***
## A.state.log                                           ***
## S.one.log                                             ***
## A.one.log                                             ***
## A.said.log                                            ***
## S.said.log                                               
## A.npnct17.log                                         ***
## S.npnct17.log                                            
## S.npnct08.log                                         ***
## A.npnct08.log                                            
## S.npnct09.log                                         ***
## A.npnct09.log                                            
## PubDate.last100.log                                   ***
## .rnorm                                                ***
## H.npnct05.log                                         ***
## `PubDate.date.fctr(7,13]`                             ***
## `PubDate.date.fctr(13,19]`                            ***
## `PubDate.date.fctr(19,25]`                            ***
## `PubDate.date.fctr(25,31]`                            ***
## `PubDate.second.fctr(14.8,29.5]`                      ***
## `PubDate.second.fctr(29.5,44.2]`                      ***
## `PubDate.second.fctr(44.2,59.1]`                      ***
## H.npnct07.log                                         ***
## A.npnct07.log                                         ***
## S.npnct07.log                                            
## S.npnct03.log                                         ***
## A.npnct19.log                                         ***
## H.npnct13.log                                         ***
## A.has.http                                               
## A.npnct03.log                                            
## A.npnct02.log                                         ***
## A.npnct18.log                                         ***
## A.npnct20.log                                            
## A.has.year.colon                                      ***
## S.has.year.colon                                         
## A.npnct22.log                                         ***
## S.npnct22.log                                            
## H.npnct02.log                                         ***
## A.presid.log                                          ***
## S.presid.log                                             
## S.npnct15.log                                         ***
## A.npnct06.log                                         ***
## S.npnct06.log                                            
## A.npnct15.log                                         ***
## H.npnct14.log                                         ***
## S.take.log                                            ***
## A.take.log                                               
## `PubDate.minute.fctr(14.8,29.5]`                      ***
## `PubDate.minute.fctr(29.5,44.2]`                      ***
## `PubDate.minute.fctr(44.2,59.1]`                      ***
## S.new.log                                             ***
## A.new.log                                             ***
## S.npnct13.log                                         ***
## A.npnct13.log                                         ***
## PubDate.wkday.fctr1                                   ***
## PubDate.wkday.fctr2                                   ***
## PubDate.wkday.fctr3                                   ***
## PubDate.wkday.fctr4                                   ***
## PubDate.wkday.fctr5                                   ***
## PubDate.wkday.fctr6                                   ***
## S.npnct30.log                                         ***
## A.npnct30.log                                         ***
## S.day.log                                             ***
## A.day.log                                                
## H.X2014.log                                           ***
## A.show.log                                            ***
## S.show.log                                               
## A.npnct14.log                                         ***
## A.report.log                                          ***
## S.report.log                                             
## A.year.log                                            ***
## S.year.log                                               
## H.npnct04.log                                         ***
## A.share.log                                           ***
## S.share.log                                              
## S.compani.log                                         ***
## A.compani.log                                            
## H.new.log                                             ***
## S.npnct14.log                                         ***
## A.first.log                                           ***
## S.first.log                                              
## S.time.log                                            ***
## A.time.log                                               
## H.newyork.log                                         ***
## A.articl.log                                          ***
## S.articl.log                                             
## S.will.log                                            ***
## A.will.log                                            ***
## H.npnct15.log                                         ***
## A.newyork.log                                         ***
## S.newyork.log                                            
## H.day.log                                             ***
## A.npnct04.log                                         ***
## S.npnct04.log                                            
## H.today.log                                           ***
## H.report.log                                          ***
## H.X2015.log                                           ***
## S.npnct16.log                                         ***
## A.intern.log                                          ***
## S.intern.log                                             
## A.npnct16.log                                            
## H.daili.log                                           ***
## H.week.log                                            ***
## H.has.year.colon                                      ***
## H.fashion.log                                         ***
## H.npnct16.log                                         ***
## A.fashion.log                                         ***
## S.fashion.log                                            
## A.week.log                                            ***
## S.week.log                                               
## H.npnct30.log                                         ***
## S.npnct12.log                                         ***
## A.npnct12.log                                         ***
## H.ndgts.log                                           ***
## S.ndgts.log                                           ***
## A.ndgts.log                                           ***
## H.nuppr.log                                           ***
## H.nchrs.log                                           ***
## H.nwrds.log                                           ***
## H.nwrds.unq.log                                       ***
## A.nchrs.log                                           ***
## S.nchrs.log                                           ***
## A.nwrds.log                                           ***
## S.nwrds.log                                           ***
## A.nwrds.unq.log                                       ***
## S.nwrds.unq.log                                       ***
## S.nuppr.log                                           ***
## A.nuppr.log                                           ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance:  4042.7  on 4474  degrees of freedom
## Residual deviance: 29844.1  on 4336  degrees of freedom
## AIC: 30122
## 
## Number of Fisher Scoring iterations: 25
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(glb_rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
    }

    return(df)
}    
glb_OOBent_df <- glb_get_predictions(df=glb_OOBent_df, glb_sel_mdl_id, glb_rsp_var_out)
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```r
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBent_df[, predct_accurate_var_name] <-
                    (glb_OOBent_df[, glb_rsp_var] == 
                     glb_OOBent_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

glb_feats_df <- 
    mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_sel_mdl, glb_fitent_df)
glb_feats_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_feats_df$importance
print(glb_feats_df)
```

```
##                                      id         cor.y exclude.as.feat
## myCategory.fctr         myCategory.fctr  1.234541e-02           FALSE
## WordCount.log             WordCount.log  2.656836e-01           FALSE
## H.npnct21.log             H.npnct21.log  1.283641e-01           FALSE
## PubDate.hour.fctr     PubDate.hour.fctr  1.354368e-01           FALSE
## A.make.log                   A.make.log  2.334962e-02           FALSE
## A.fashion.log             A.fashion.log -8.724932e-02           FALSE
## H.npnct30.log             H.npnct30.log -8.917338e-02           FALSE
## A.report.log               A.report.log -5.032801e-02           FALSE
## A.articl.log               A.articl.log -5.952055e-02           FALSE
## A.npnct04.log             A.npnct04.log -6.294642e-02           FALSE
## H.X2015.log                 H.X2015.log -6.658489e-02           FALSE
## A.share.log                 A.share.log -5.138139e-02           FALSE
## H.today.log                 H.today.log -6.372306e-02           FALSE
## H.new.log                     H.new.log -5.313316e-02           FALSE
## H.has.year.colon       H.has.year.colon -7.842875e-02           FALSE
## A.said.log                   A.said.log  3.735051e-04           FALSE
## H.ndgts.log                 H.ndgts.log -1.196633e-01           FALSE
## PubDate.last10.log   PubDate.last10.log  4.931702e-02           FALSE
## H.npnct04.log             H.npnct04.log -5.126277e-02           FALSE
## H.npnct08.log             H.npnct08.log  5.375262e-02           FALSE
## A.newyork.log             A.newyork.log -6.219997e-02           FALSE
## S.npnct15.log             S.npnct15.log -2.121844e-02           FALSE
## A.npnct15.log             A.npnct15.log -2.407715e-02           FALSE
## PubDate.wkday.fctr   PubDate.wkday.fctr -3.980129e-02           FALSE
## .rnorm                           .rnorm -8.244230e-03           FALSE
## A.week.log                   A.week.log -8.840293e-02           FALSE
## H.npnct13.log             H.npnct13.log -1.305305e-02           FALSE
## A.show.log                   A.show.log -4.897915e-02           FALSE
## A.intern.log               A.intern.log -6.864274e-02           FALSE
## S.time.log                   S.time.log -5.759227e-02           FALSE
## H.npnct07.log             H.npnct07.log -1.201741e-02           FALSE
## A.npnct18.log             A.npnct18.log -1.451467e-02           FALSE
## H.npnct06.log             H.npnct06.log  3.190718e-02           FALSE
## S.state.log                 S.state.log  7.050791e-03           FALSE
## PubDate.minute.fctr PubDate.minute.fctr -3.407385e-02           FALSE
## A.npnct19.log             A.npnct19.log -1.271661e-02           FALSE
## A.can.log                     A.can.log  3.169296e-02           FALSE
## PubDate.date.fctr     PubDate.date.fctr -1.164756e-02           FALSE
## S.npnct12.log             S.npnct12.log -9.158156e-02           FALSE
## A.npnct12.log             A.npnct12.log -9.183870e-02           FALSE
## H.nuppr.log                 H.nuppr.log -1.278085e-01           FALSE
## H.week.log                   H.week.log -7.510522e-02           FALSE
## A.state.log                 A.state.log  6.668101e-03           FALSE
## S.npnct13.log             S.npnct13.log -3.638891e-02           FALSE
## A.npnct13.log             A.npnct13.log -3.760012e-02           FALSE
## A.presid.log               A.presid.log -2.014404e-02           FALSE
## S.npnct16.log             S.npnct16.log -6.770952e-02           FALSE
## S.npnct08.log             S.npnct08.log -2.413868e-03           FALSE
## S.can.log                     S.can.log  3.077833e-02           FALSE
## A.npnct17.log             A.npnct17.log -1.587454e-03           FALSE
## A.will.log                   A.will.log -6.147068e-02           FALSE
## S.ndgts.log                 S.ndgts.log -1.242046e-01           FALSE
## S.compani.log             S.compani.log -5.261812e-02           FALSE
## H.npnct05.log             H.npnct05.log -9.653967e-03           FALSE
## A.npnct21.log             A.npnct21.log  5.482747e-02           FALSE
## S.nuppr.log                 S.nuppr.log -2.718459e-01           FALSE
## H.nchrs.log                 H.nchrs.log -1.710624e-01           FALSE
## A.year.log                   A.year.log -5.094457e-02           FALSE
## A.npnct01.log             A.npnct01.log  3.093101e-02           FALSE
## S.will.log                   S.will.log -6.103349e-02           FALSE
## A.ndgts.log                 A.ndgts.log -1.249484e-01           FALSE
## PubDate.last100.log PubDate.last100.log -7.663322e-03           FALSE
## A.npnct22.log             A.npnct22.log -1.923169e-02           FALSE
## A.nuppr.log                 A.nuppr.log -2.720962e-01           FALSE
## S.day.log                     S.day.log -4.555421e-02           FALSE
## S.one.log                     S.one.log  4.891059e-03           FALSE
## A.one.log                     A.one.log  4.368856e-03           FALSE
## PubDate.second.fctr PubDate.second.fctr -1.187946e-02           FALSE
## H.X2014.log                 H.X2014.log -4.620638e-02           FALSE
## H.report.log               H.report.log -6.494810e-02           FALSE
## S.take.log                   S.take.log -2.569295e-02           FALSE
## S.nwrds.unq.log         S.nwrds.unq.log -2.507969e-01           FALSE
## S.npnct09.log             S.npnct09.log -3.986882e-03           FALSE
## A.nwrds.unq.log         A.nwrds.unq.log -2.506012e-01           FALSE
## H.npnct12.log             H.npnct12.log  1.333613e-02           FALSE
## H.fashion.log             H.fashion.log -8.204998e-02           FALSE
## S.npnct21.log             S.npnct21.log  5.503894e-02           FALSE
## S.new.log                     S.new.log -3.483189e-02           FALSE
## H.npnct15.log             H.npnct15.log -6.158577e-02           FALSE
## A.new.log                     A.new.log -3.524871e-02           FALSE
## A.nchrs.log                 A.nchrs.log -2.245488e-01           FALSE
## S.nchrs.log                 S.nchrs.log -2.246930e-01           FALSE
## S.npnct30.log             S.npnct30.log -4.370037e-02           FALSE
## H.npnct17.log             H.npnct17.log  3.039622e-02           FALSE
## A.npnct06.log             A.npnct06.log -2.389145e-02           FALSE
## A.npnct30.log             A.npnct30.log -4.373349e-02           FALSE
## H.nwrds.unq.log         H.nwrds.unq.log -2.044964e-01           FALSE
## S.nwrds.log                 S.nwrds.log -2.453541e-01           FALSE
## A.npnct07.log             A.npnct07.log -1.214357e-02           FALSE
## H.npnct01.log             H.npnct01.log  2.271577e-02           FALSE
## A.nwrds.log                 A.nwrds.log -2.450733e-01           FALSE
## H.has.ebola                 H.has.ebola  2.588140e-02           FALSE
## A.npnct14.log             A.npnct14.log -4.999563e-02           FALSE
## S.npnct14.log             S.npnct14.log -5.332519e-02           FALSE
## PubDate.last1.log     PubDate.last1.log  4.635751e-02           FALSE
## A.npnct02.log             A.npnct02.log -1.451467e-02           FALSE
## H.day.log                     H.day.log -6.272898e-02           FALSE
## A.first.log                 A.first.log -5.345938e-02           FALSE
## S.npnct03.log             S.npnct03.log -1.240734e-02           FALSE
## H.newyork.log             H.newyork.log -5.797009e-02           FALSE
## A.has.year.colon       A.has.year.colon -1.755336e-02           FALSE
## PubDate.wkend             PubDate.wkend  1.067288e-01           FALSE
## H.npnct16.log             H.npnct16.log -8.273237e-02           FALSE
## H.npnct14.log             H.npnct14.log -2.524770e-02           FALSE
## H.nwrds.log                 H.nwrds.log -2.006864e-01           FALSE
## H.npnct02.log             H.npnct02.log -2.001851e-02           FALSE
## H.daili.log                 H.daili.log -6.919298e-02           FALSE
## A.compani.log             A.compani.log -5.268413e-02           FALSE
## A.day.log                     A.day.log -4.581783e-02           FALSE
## A.has.http                   A.has.http -1.359260e-02           FALSE
## A.npnct03.log             A.npnct03.log -1.359260e-02           FALSE
## A.npnct05.log             A.npnct05.log            NA           FALSE
## A.npnct08.log             A.npnct08.log -3.258100e-03           FALSE
## A.npnct09.log             A.npnct09.log -4.775988e-03           FALSE
## A.npnct10.log             A.npnct10.log            NA           FALSE
## A.npnct11.log             A.npnct11.log -5.547032e-03           FALSE
## A.npnct16.log             A.npnct16.log -6.893301e-02           FALSE
## A.npnct20.log             A.npnct20.log -1.451467e-02           FALSE
## A.npnct23.log             A.npnct23.log  1.537569e-02           FALSE
## A.npnct24.log             A.npnct24.log            NA           FALSE
## A.npnct25.log             A.npnct25.log  1.537569e-02           FALSE
## A.npnct26.log             A.npnct26.log -9.890046e-19           FALSE
## A.npnct27.log             A.npnct27.log -5.547032e-03           FALSE
## A.npnct28.log             A.npnct28.log            NA           FALSE
## A.npnct29.log             A.npnct29.log            NA           FALSE
## A.npnct31.log             A.npnct31.log            NA           FALSE
## A.npnct32.log             A.npnct32.log            NA           FALSE
## A.take.log                   A.take.log -2.601772e-02           FALSE
## A.time.log                   A.time.log -5.779371e-02           FALSE
## H.has.http                   H.has.http            NA           FALSE
## H.npnct03.log             H.npnct03.log  9.533020e-03           FALSE
## H.npnct09.log             H.npnct09.log  5.375262e-02           FALSE
## H.npnct10.log             H.npnct10.log            NA           FALSE
## H.npnct11.log             H.npnct11.log -5.547032e-03           FALSE
## H.npnct18.log             H.npnct18.log            NA           FALSE
## H.npnct19.log             H.npnct19.log            NA           FALSE
## H.npnct20.log             H.npnct20.log            NA           FALSE
## H.npnct22.log             H.npnct22.log -5.547032e-03           FALSE
## H.npnct23.log             H.npnct23.log            NA           FALSE
## H.npnct24.log             H.npnct24.log            NA           FALSE
## H.npnct25.log             H.npnct25.log            NA           FALSE
## H.npnct26.log             H.npnct26.log -9.890046e-19           FALSE
## H.npnct27.log             H.npnct27.log            NA           FALSE
## H.npnct28.log             H.npnct28.log            NA           FALSE
## H.npnct29.log             H.npnct29.log            NA           FALSE
## H.npnct31.log             H.npnct31.log            NA           FALSE
## H.npnct32.log             H.npnct32.log            NA           FALSE
## Popular                         Popular  1.000000e+00            TRUE
## Popular.fctr               Popular.fctr            NA            TRUE
## PubDate.last1             PubDate.last1  3.592267e-02            TRUE
## PubDate.last10           PubDate.last10  5.398093e-02            TRUE
## PubDate.last100         PubDate.last100  3.989229e-02            TRUE
## PubDate.month.fctr   PubDate.month.fctr  1.914874e-02            TRUE
## PubDate.POSIX             PubDate.POSIX  1.568326e-02            TRUE
## PubDate.year.fctr     PubDate.year.fctr            NA           FALSE
## PubDate.zoo                 PubDate.zoo  1.568326e-02            TRUE
## S.articl.log               S.articl.log -5.952055e-02           FALSE
## S.fashion.log             S.fashion.log -8.724932e-02           FALSE
## S.first.log                 S.first.log -5.345938e-02           FALSE
## S.has.http                   S.has.http            NA           FALSE
## S.has.year.colon       S.has.year.colon -1.755336e-02           FALSE
## S.intern.log               S.intern.log -6.864274e-02           FALSE
## S.make.log                   S.make.log  2.334962e-02           FALSE
## S.newyork.log             S.newyork.log -6.219997e-02           FALSE
## S.npnct01.log             S.npnct01.log  3.093101e-02           FALSE
## S.npnct02.log             S.npnct02.log -5.547032e-03           FALSE
## S.npnct04.log             S.npnct04.log -6.294642e-02           FALSE
## S.npnct05.log             S.npnct05.log            NA           FALSE
## S.npnct06.log             S.npnct06.log -2.389145e-02           FALSE
## S.npnct07.log             S.npnct07.log -1.214357e-02           FALSE
## S.npnct10.log             S.npnct10.log            NA           FALSE
## S.npnct11.log             S.npnct11.log -5.547032e-03           FALSE
## S.npnct17.log             S.npnct17.log -1.587454e-03           FALSE
## S.npnct18.log             S.npnct18.log            NA           FALSE
## S.npnct19.log             S.npnct19.log            NA           FALSE
## S.npnct20.log             S.npnct20.log            NA           FALSE
## S.npnct22.log             S.npnct22.log -1.923169e-02           FALSE
## S.npnct23.log             S.npnct23.log  2.760321e-02           FALSE
## S.npnct24.log             S.npnct24.log            NA           FALSE
## S.npnct25.log             S.npnct25.log  2.760321e-02           FALSE
## S.npnct26.log             S.npnct26.log -9.890046e-19           FALSE
## S.npnct27.log             S.npnct27.log            NA           FALSE
## S.npnct28.log             S.npnct28.log            NA           FALSE
## S.npnct29.log             S.npnct29.log            NA           FALSE
## S.npnct31.log             S.npnct31.log            NA           FALSE
## S.npnct32.log             S.npnct32.log            NA           FALSE
## S.presid.log               S.presid.log -2.014404e-02           FALSE
## S.report.log               S.report.log -5.032801e-02           FALSE
## S.said.log                   S.said.log  3.735051e-04           FALSE
## S.share.log                 S.share.log -5.138139e-02           FALSE
## S.show.log                   S.show.log -4.897915e-02           FALSE
## S.week.log                   S.week.log -8.840293e-02           FALSE
## S.year.log                   S.year.log -5.094457e-02           FALSE
## UniqueID                       UniqueID  1.182492e-02            TRUE
## WordCount                     WordCount  2.575265e-01            TRUE
##                        cor.y.abs       cor.high.X   freqRatio
## myCategory.fctr     1.234541e-02             <NA>    1.337185
## WordCount.log       2.656836e-01             <NA>    1.300000
## H.npnct21.log       1.283641e-01             <NA>   14.995098
## PubDate.hour.fctr   1.354368e-01             <NA>    1.835040
## A.make.log          2.334962e-02       S.make.log   27.378261
## A.fashion.log       8.724932e-02    S.fashion.log   25.737705
## H.npnct30.log       8.917338e-02             <NA>   24.123077
## A.report.log        5.032801e-02     S.report.log   24.204633
## A.articl.log        5.952055e-02     S.articl.log   30.863415
## A.npnct04.log       6.294642e-02    S.npnct04.log   28.536364
## H.X2015.log         6.658489e-02    H.npnct15.log   45.326241
## A.share.log         5.138139e-02      S.share.log   32.654639
## H.today.log         6.372306e-02             <NA>   36.757225
## H.new.log           5.313316e-02             <NA>   25.228916
## H.has.year.colon    7.842875e-02     A.intern.log   32.670103
## A.said.log          3.735051e-04             <NA>   25.212851
## H.ndgts.log         1.196633e-01             <NA>   13.616137
## PubDate.last10.log  4.931702e-02             <NA>    1.666667
## H.npnct04.log       5.126277e-02             <NA>   38.325301
## H.npnct08.log       5.375262e-02    H.npnct09.log  111.620690
## A.newyork.log       6.219997e-02    S.newyork.log   15.153465
## S.npnct15.log       2.121844e-02             <NA>  203.062500
## A.npnct15.log       2.407715e-02    A.npnct02.log  196.696970
## PubDate.wkday.fctr  3.980129e-02             <NA>    1.003268
## .rnorm              8.244230e-03             <NA>    2.000000
## A.week.log          8.840293e-02       S.week.log   13.278509
## H.npnct13.log       1.305305e-02             <NA>   13.126638
## A.show.log          4.897915e-02       S.show.log   30.512077
## A.intern.log        6.864274e-02     S.intern.log   29.801887
## S.time.log          5.759227e-02             <NA>   13.483296
## H.npnct07.log       1.201741e-02             <NA>    5.437234
## A.npnct18.log       1.451467e-02    A.npnct20.log 1087.500000
## H.npnct06.log       3.190718e-02    H.npnct17.log   68.935484
## S.state.log         7.050791e-03             <NA>   30.655340
## PubDate.minute.fctr 3.407385e-02             <NA>    1.483365
## A.npnct19.log       1.271661e-02             <NA> 1631.500000
## A.can.log           3.169296e-02        S.can.log   26.166667
## PubDate.date.fctr   1.164756e-02             <NA>    1.021394
## S.npnct12.log       9.158156e-02             <NA>    1.660473
## A.npnct12.log       9.183870e-02    S.npnct12.log    1.660473
## H.nuppr.log         1.278085e-01             <NA>    1.033930
## H.week.log          7.510522e-02             <NA>   24.818182
## A.state.log         6.668101e-03             <NA>   30.502415
## S.npnct13.log       3.638891e-02             <NA>    5.706263
## A.npnct13.log       3.760012e-02    S.npnct13.log    5.715368
## A.presid.log        2.014404e-02     S.presid.log   26.854701
## S.npnct16.log       6.770952e-02             <NA>   13.647191
## S.npnct08.log       2.413868e-03             <NA>  175.513514
## S.can.log           3.077833e-02             <NA>   26.058091
## A.npnct17.log       1.587454e-03             <NA>  434.133333
## A.will.log          6.147068e-02       S.will.log   11.212406
## S.ndgts.log         1.242046e-01             <NA>   10.511247
## S.compani.log       5.261812e-02             <NA>   18.093842
## H.npnct05.log       9.653967e-03             <NA>  543.333333
## A.npnct21.log       5.482747e-02             <NA>   12.798715
## S.nuppr.log         2.718459e-01             <NA>    1.152620
## H.nchrs.log         1.710624e-01             <NA>    1.023810
## A.year.log          5.094457e-02       S.year.log   18.456716
## A.npnct01.log       3.093101e-02    S.npnct01.log  309.952381
## S.will.log          6.103349e-02             <NA>   11.237288
## A.ndgts.log         1.249484e-01      S.ndgts.log   10.501022
## PubDate.last100.log 7.663322e-03             <NA>   25.000000
## A.npnct22.log       1.923169e-02    S.npnct22.log  543.333333
## A.nuppr.log         2.720962e-01      S.nuppr.log    1.151308
## S.day.log           4.555421e-02             <NA>   24.692913
## S.one.log           4.891059e-03             <NA>   22.777372
## A.one.log           4.368856e-03             <NA>   22.773723
## PubDate.second.fctr 1.187946e-02             <NA>    1.018204
## H.X2014.log         4.620638e-02             <NA>   63.673267
## H.report.log        6.494810e-02             <NA>   30.403846
## S.take.log          2.569295e-02             <NA>   29.376744
## S.nwrds.unq.log     2.507969e-01      S.nchrs.log    1.061567
## S.npnct09.log       3.986882e-03             <NA>  175.486486
## A.nwrds.unq.log     2.506012e-01             <NA>    1.061567
## H.npnct12.log       1.333613e-02             <NA>    4.937442
## H.fashion.log       8.204998e-02       H.week.log   28.542986
## S.npnct21.log       5.503894e-02    A.npnct21.log   12.862366
## S.new.log           3.483189e-02             <NA>   10.124573
## H.npnct15.log       6.158577e-02             <NA>   52.983471
## A.new.log           3.524871e-02        S.new.log   10.086735
## A.nchrs.log         2.245488e-01             <NA>    1.328571
## S.nchrs.log         2.246930e-01      A.nchrs.log    1.328571
## S.npnct30.log       4.370037e-02             <NA>  134.791667
## H.npnct17.log       3.039622e-02             <NA>   96.104478
## A.npnct06.log       2.389145e-02    S.npnct06.log  115.642857
## A.npnct30.log       4.373349e-02    S.npnct30.log  126.862745
## H.nwrds.unq.log     2.044964e-01      H.nuppr.log    1.019071
## S.nwrds.log         2.453541e-01      A.nwrds.log    1.029183
## A.npnct07.log       1.214357e-02    S.npnct07.log 1631.750000
## H.npnct01.log       2.271577e-02             <NA>  282.913043
## A.nwrds.log         2.450733e-01             <NA>    1.029183
## H.has.ebola         2.588140e-02             <NA>   73.227273
## A.npnct14.log       4.999563e-02             <NA>    4.603330
## S.npnct14.log       5.332519e-02    A.npnct14.log    4.672000
## PubDate.last1.log   4.635751e-02             <NA>    1.142857
## A.npnct02.log       1.451467e-02    A.npnct18.log 1087.500000
## H.day.log           6.272898e-02             <NA>   29.801887
## A.first.log         5.345938e-02      S.first.log   29.509346
## S.npnct03.log       1.240734e-02             <NA> 1305.400000
## H.newyork.log       5.797009e-02             <NA>   26.795745
## A.has.year.colon    1.755336e-02 S.has.year.colon  652.200000
## PubDate.wkend       1.067288e-01             <NA>    9.095827
## H.npnct16.log       8.273237e-02             <NA>    3.914910
## H.npnct14.log       2.524770e-02             <NA>   22.802326
## H.nwrds.log         2.006864e-01             <NA>    1.019119
## H.npnct02.log       2.001851e-02             <NA>  501.461538
## H.daili.log         6.919298e-02             <NA>   41.973684
## A.compani.log       5.268413e-02    S.compani.log   18.147059
## A.day.log           4.581783e-02        S.day.log   24.592157
## A.has.http          1.359260e-02    A.npnct19.log 1087.666667
## A.npnct03.log       1.359260e-02    S.npnct03.log 1087.666667
## A.npnct05.log                 NA             <NA>    0.000000
## A.npnct08.log       3.258100e-03             <NA>  170.868421
## A.npnct09.log       4.775988e-03             <NA>  170.842105
## A.npnct10.log                 NA             <NA>    0.000000
## A.npnct11.log       5.547032e-03             <NA> 6531.000000
## A.npnct16.log       6.893301e-02    S.npnct16.log   13.482222
## A.npnct20.log       1.451467e-02             <NA> 1087.500000
## A.npnct23.log       1.537569e-02    A.npnct25.log 3264.500000
## A.npnct24.log                 NA             <NA>    0.000000
## A.npnct25.log       1.537569e-02             <NA> 3264.500000
## A.npnct26.log       9.890046e-19             <NA>    0.000000
## A.npnct27.log       5.547032e-03             <NA> 6531.000000
## A.npnct28.log                 NA             <NA>    0.000000
## A.npnct29.log                 NA             <NA>    0.000000
## A.npnct31.log                 NA             <NA>    0.000000
## A.npnct32.log                 NA             <NA>    0.000000
## A.take.log          2.601772e-02       S.take.log   29.236111
## A.time.log          5.779371e-02       S.time.log   13.451111
## H.has.http                    NA             <NA>    0.000000
## H.npnct03.log       9.533020e-03             <NA> 2176.333333
## H.npnct09.log       5.375262e-02             <NA>  111.620690
## H.npnct10.log                 NA             <NA>    0.000000
## H.npnct11.log       5.547032e-03             <NA> 6531.000000
## H.npnct18.log                 NA             <NA>    0.000000
## H.npnct19.log                 NA             <NA>    0.000000
## H.npnct20.log                 NA             <NA>    0.000000
## H.npnct22.log       5.547032e-03             <NA> 6531.000000
## H.npnct23.log                 NA             <NA>    0.000000
## H.npnct24.log                 NA             <NA>    0.000000
## H.npnct25.log                 NA             <NA>    0.000000
## H.npnct26.log       9.890046e-19             <NA>    0.000000
## H.npnct27.log                 NA             <NA>    0.000000
## H.npnct28.log                 NA             <NA>    0.000000
## H.npnct29.log                 NA             <NA>    0.000000
## H.npnct31.log                 NA             <NA>    0.000000
## H.npnct32.log                 NA             <NA>    0.000000
## Popular             1.000000e+00             <NA>    4.976212
## Popular.fctr                  NA             <NA>          NA
## PubDate.last1       3.592267e-02             <NA>    1.142857
## PubDate.last10      5.398093e-02             <NA>    1.666667
## PubDate.last100     3.989229e-02             <NA>   25.000000
## PubDate.month.fctr  1.914874e-02             <NA>    1.017514
## PubDate.POSIX       1.568326e-02             <NA>    1.000000
## PubDate.year.fctr             NA             <NA>    0.000000
## PubDate.zoo         1.568326e-02             <NA>    1.000000
## S.articl.log        5.952055e-02             <NA>   30.863415
## S.fashion.log       8.724932e-02             <NA>   25.737705
## S.first.log         5.345938e-02             <NA>   29.509346
## S.has.http                    NA             <NA>    0.000000
## S.has.year.colon    1.755336e-02             <NA>  652.200000
## S.intern.log        6.864274e-02             <NA>   29.801887
## S.make.log          2.334962e-02             <NA>   27.378261
## S.newyork.log       6.219997e-02             <NA>   15.153465
## S.npnct01.log       3.093101e-02             <NA>  309.952381
## S.npnct02.log       5.547032e-03             <NA> 6531.000000
## S.npnct04.log       6.294642e-02             <NA>   28.536364
## S.npnct05.log                 NA             <NA>    0.000000
## S.npnct06.log       2.389145e-02             <NA>  115.642857
## S.npnct07.log       1.214357e-02             <NA> 1631.750000
## S.npnct10.log                 NA             <NA>    0.000000
## S.npnct11.log       5.547032e-03             <NA> 6531.000000
## S.npnct17.log       1.587454e-03             <NA>  434.133333
## S.npnct18.log                 NA             <NA>    0.000000
## S.npnct19.log                 NA             <NA>    0.000000
## S.npnct20.log                 NA             <NA>    0.000000
## S.npnct22.log       1.923169e-02             <NA>  543.333333
## S.npnct23.log       2.760321e-02    A.npnct23.log 6531.000000
## S.npnct24.log                 NA             <NA>    0.000000
## S.npnct25.log       2.760321e-02             <NA> 6531.000000
## S.npnct26.log       9.890046e-19             <NA>    0.000000
## S.npnct27.log                 NA             <NA>    0.000000
## S.npnct28.log                 NA             <NA>    0.000000
## S.npnct29.log                 NA             <NA>    0.000000
## S.npnct31.log                 NA             <NA>    0.000000
## S.npnct32.log                 NA             <NA>    0.000000
## S.presid.log        2.014404e-02             <NA>   26.854701
## S.report.log        5.032801e-02             <NA>   24.204633
## S.said.log          3.735051e-04             <NA>   25.212851
## S.share.log         5.138139e-02             <NA>   32.654639
## S.show.log          4.897915e-02             <NA>   30.512077
## S.week.log          8.840293e-02             <NA>   13.278509
## S.year.log          5.094457e-02             <NA>   18.456716
## UniqueID            1.182492e-02             <NA>    1.000000
## WordCount           2.575265e-01             <NA>    2.315789
##                     percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## myCategory.fctr        0.30618494   FALSE FALSE    FALSE            FALSE
## WordCount.log         24.14268218   FALSE FALSE    FALSE            FALSE
## H.npnct21.log          0.06123699   FALSE FALSE    FALSE            FALSE
## PubDate.hour.fctr      0.04592774   FALSE FALSE    FALSE            FALSE
## A.make.log             0.04592774   FALSE  TRUE    FALSE            FALSE
## A.fashion.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## H.npnct30.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.report.log           0.06123699   FALSE  TRUE    FALSE            FALSE
## A.articl.log           0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct04.log          0.07654623   FALSE  TRUE    FALSE            FALSE
## H.X2015.log            0.03061849   FALSE  TRUE    FALSE            FALSE
## A.share.log            0.04592774   FALSE  TRUE    FALSE            FALSE
## H.today.log            0.03061849   FALSE  TRUE    FALSE            FALSE
## H.new.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## H.has.year.colon       0.03061849   FALSE  TRUE    FALSE            FALSE
## A.said.log             0.04592774   FALSE  TRUE    FALSE             TRUE
## H.ndgts.log            0.18371096   FALSE FALSE    FALSE            FALSE
## PubDate.last10.log    79.05695040   FALSE FALSE    FALSE            FALSE
## H.npnct04.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## H.npnct08.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.newyork.log          0.06123699   FALSE FALSE    FALSE            FALSE
## S.npnct15.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## A.npnct15.log          0.10716473   FALSE  TRUE    FALSE            FALSE
## PubDate.wkday.fctr     0.10716473   FALSE FALSE    FALSE            FALSE
## .rnorm                99.98469075   FALSE FALSE    FALSE            FALSE
## A.week.log             0.04592774   FALSE FALSE    FALSE            FALSE
## H.npnct13.log          0.09185548   FALSE FALSE    FALSE            FALSE
## A.show.log             0.06123699   FALSE  TRUE    FALSE            FALSE
## A.intern.log           0.04592774   FALSE  TRUE    FALSE            FALSE
## S.time.log             0.04592774   FALSE FALSE    FALSE            FALSE
## H.npnct07.log          0.12247397   FALSE FALSE    FALSE            FALSE
## A.npnct18.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## H.npnct06.log          0.06123699   FALSE  TRUE    FALSE            FALSE
## S.state.log            0.04592774   FALSE  TRUE    FALSE             TRUE
## PubDate.minute.fctr    0.06123699   FALSE FALSE    FALSE            FALSE
## A.npnct19.log          0.06123699   FALSE  TRUE    FALSE            FALSE
## A.can.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## PubDate.date.fctr      0.07654623   FALSE FALSE    FALSE            FALSE
## S.npnct12.log          0.13778322   FALSE FALSE    FALSE            FALSE
## A.npnct12.log          0.13778322   FALSE FALSE    FALSE            FALSE
## H.nuppr.log            0.29087569   FALSE FALSE    FALSE            FALSE
## H.week.log             0.03061849   FALSE  TRUE    FALSE            FALSE
## A.state.log            0.04592774   FALSE  TRUE    FALSE             TRUE
## S.npnct13.log          0.09185548   FALSE FALSE    FALSE            FALSE
## A.npnct13.log          0.12247397   FALSE FALSE    FALSE            FALSE
## A.presid.log           0.06123699   FALSE  TRUE    FALSE            FALSE
## S.npnct16.log          0.04592774   FALSE FALSE    FALSE            FALSE
## S.npnct08.log          0.04592774   FALSE  TRUE    FALSE             TRUE
## S.can.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## A.npnct17.log          0.04592774   FALSE  TRUE    FALSE             TRUE
## A.will.log             0.06123699   FALSE FALSE    FALSE            FALSE
## S.ndgts.log            0.26025720   FALSE FALSE    FALSE            FALSE
## S.compani.log          0.04592774   FALSE FALSE    FALSE            FALSE
## H.npnct05.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct21.log          0.07654623   FALSE FALSE    FALSE            FALSE
## S.nuppr.log            0.33680343   FALSE FALSE    FALSE            FALSE
## H.nchrs.log            1.57685242   FALSE FALSE    FALSE            FALSE
## A.year.log             0.06123699   FALSE FALSE    FALSE            FALSE
## A.npnct01.log          0.06123699   FALSE  TRUE    FALSE            FALSE
## S.will.log             0.06123699   FALSE FALSE    FALSE            FALSE
## A.ndgts.log            0.29087569   FALSE FALSE    FALSE            FALSE
## PubDate.last100.log   92.19228414   FALSE FALSE    FALSE             TRUE
## A.npnct22.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.nuppr.log            0.33680343   FALSE FALSE    FALSE            FALSE
## S.day.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## S.one.log              0.04592774   FALSE  TRUE    FALSE             TRUE
## A.one.log              0.04592774   FALSE  TRUE    FALSE             TRUE
## PubDate.second.fctr    0.06123699   FALSE FALSE    FALSE            FALSE
## H.X2014.log            0.03061849   FALSE  TRUE    FALSE            FALSE
## H.report.log           0.03061849   FALSE  TRUE    FALSE            FALSE
## S.take.log             0.04592774   FALSE  TRUE    FALSE            FALSE
## S.nwrds.unq.log        0.44396816   FALSE FALSE    FALSE            FALSE
## S.npnct09.log          0.06123699   FALSE  TRUE    FALSE             TRUE
## A.nwrds.unq.log        0.55113288   FALSE FALSE    FALSE            FALSE
## H.npnct12.log          0.07654623   FALSE FALSE    FALSE            FALSE
## H.fashion.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## S.npnct21.log          0.07654623   FALSE FALSE    FALSE            FALSE
## S.new.log              0.04592774   FALSE FALSE    FALSE            FALSE
## H.npnct15.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.new.log              0.04592774   FALSE FALSE    FALSE            FALSE
## A.nchrs.log            4.39375383   FALSE FALSE    FALSE            FALSE
## S.nchrs.log            3.72014697   FALSE FALSE    FALSE            FALSE
## S.npnct30.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## H.npnct17.log          0.06123699   FALSE  TRUE    FALSE            FALSE
## A.npnct06.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct30.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## H.nwrds.unq.log        0.21432945   FALSE FALSE    FALSE            FALSE
## S.nwrds.log            0.45927740   FALSE FALSE    FALSE            FALSE
## A.npnct07.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## H.npnct01.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## A.nwrds.log            0.59706062   FALSE FALSE    FALSE            FALSE
## H.has.ebola            0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct14.log          0.16840171   FALSE FALSE    FALSE            FALSE
## S.npnct14.log          0.16840171   FALSE FALSE    FALSE            FALSE
## PubDate.last1.log     36.49724434   FALSE FALSE    FALSE            FALSE
## A.npnct02.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## H.day.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## A.first.log            0.04592774   FALSE  TRUE    FALSE            FALSE
## S.npnct03.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## H.newyork.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.has.year.colon       0.03061849   FALSE  TRUE    FALSE            FALSE
## PubDate.wkend          0.03061849   FALSE FALSE    FALSE            FALSE
## H.npnct16.log          0.04592774   FALSE FALSE    FALSE            FALSE
## H.npnct14.log          0.12247397   FALSE  TRUE    FALSE            FALSE
## H.nwrds.log            0.21432945   FALSE FALSE    FALSE            FALSE
## H.npnct02.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## H.daili.log            0.03061849   FALSE  TRUE    FALSE            FALSE
## A.compani.log          0.04592774   FALSE FALSE    FALSE            FALSE
## A.day.log              0.04592774   FALSE  TRUE    FALSE            FALSE
## A.has.http             0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct03.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## A.npnct05.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct08.log          0.04592774   FALSE  TRUE    FALSE             TRUE
## A.npnct09.log          0.06123699   FALSE  TRUE    FALSE             TRUE
## A.npnct10.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct11.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## A.npnct16.log          0.04592774   FALSE FALSE    FALSE            FALSE
## A.npnct20.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## A.npnct23.log          0.04592774   FALSE  TRUE     TRUE            FALSE
## A.npnct24.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct25.log          0.04592774   FALSE  TRUE     TRUE            FALSE
## A.npnct26.log          0.01530925    TRUE  TRUE     TRUE             TRUE
## A.npnct27.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## A.npnct28.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct29.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct31.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.npnct32.log          0.01530925    TRUE  TRUE     TRUE               NA
## A.take.log             0.04592774   FALSE  TRUE    FALSE            FALSE
## A.time.log             0.04592774   FALSE FALSE    FALSE            FALSE
## H.has.http             0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct03.log          0.03061849   FALSE  TRUE     TRUE            FALSE
## H.npnct09.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## H.npnct10.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct11.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## H.npnct18.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct19.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct20.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct22.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## H.npnct23.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct24.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct25.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct26.log          0.01530925    TRUE  TRUE     TRUE             TRUE
## H.npnct27.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct28.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct29.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct31.log          0.01530925    TRUE  TRUE     TRUE               NA
## H.npnct32.log          0.01530925    TRUE  TRUE     TRUE               NA
## Popular                0.03061849   FALSE FALSE    FALSE            FALSE
## Popular.fctr                   NA      NA    NA       NA               NA
## PubDate.last1         36.49724434   FALSE FALSE    FALSE            FALSE
## PubDate.last10        79.05695040   FALSE FALSE    FALSE            FALSE
## PubDate.last100       92.52908757   FALSE FALSE    FALSE            FALSE
## PubDate.month.fctr     0.04592774   FALSE FALSE    FALSE            FALSE
## PubDate.POSIX         99.86221678   FALSE FALSE    FALSE            FALSE
## PubDate.year.fctr      0.01530925    TRUE  TRUE     TRUE               NA
## PubDate.zoo           99.86221678   FALSE FALSE    FALSE            FALSE
## S.articl.log           0.03061849   FALSE  TRUE    FALSE            FALSE
## S.fashion.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## S.first.log            0.04592774   FALSE  TRUE    FALSE            FALSE
## S.has.http             0.01530925    TRUE  TRUE     TRUE               NA
## S.has.year.colon       0.03061849   FALSE  TRUE    FALSE            FALSE
## S.intern.log           0.04592774   FALSE  TRUE    FALSE            FALSE
## S.make.log             0.04592774   FALSE  TRUE    FALSE            FALSE
## S.newyork.log          0.06123699   FALSE FALSE    FALSE            FALSE
## S.npnct01.log          0.06123699   FALSE  TRUE    FALSE            FALSE
## S.npnct02.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## S.npnct04.log          0.07654623   FALSE  TRUE    FALSE            FALSE
## S.npnct05.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct06.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## S.npnct07.log          0.04592774   FALSE  TRUE    FALSE            FALSE
## S.npnct10.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct11.log          0.03061849   FALSE  TRUE     TRUE             TRUE
## S.npnct17.log          0.04592774   FALSE  TRUE    FALSE             TRUE
## S.npnct18.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct19.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct20.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct22.log          0.03061849   FALSE  TRUE    FALSE            FALSE
## S.npnct23.log          0.03061849   FALSE  TRUE     TRUE            FALSE
## S.npnct24.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct25.log          0.03061849   FALSE  TRUE     TRUE            FALSE
## S.npnct26.log          0.01530925    TRUE  TRUE     TRUE             TRUE
## S.npnct27.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct28.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct29.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct31.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.npnct32.log          0.01530925    TRUE  TRUE     TRUE               NA
## S.presid.log           0.06123699   FALSE  TRUE    FALSE            FALSE
## S.report.log           0.06123699   FALSE  TRUE    FALSE            FALSE
## S.said.log             0.04592774   FALSE  TRUE    FALSE             TRUE
## S.share.log            0.04592774   FALSE  TRUE    FALSE            FALSE
## S.show.log             0.06123699   FALSE  TRUE    FALSE            FALSE
## S.week.log             0.04592774   FALSE FALSE    FALSE            FALSE
## S.year.log             0.06123699   FALSE FALSE    FALSE            FALSE
## UniqueID             100.00000000   FALSE FALSE    FALSE            FALSE
## WordCount             24.15799143   FALSE FALSE    FALSE            FALSE
##                     rsp_var_raw id_var rsp_var   importance
## myCategory.fctr           FALSE     NA      NA 100.00000000
## WordCount.log             FALSE     NA      NA  70.43241920
## H.npnct21.log             FALSE     NA      NA  32.71499686
## PubDate.hour.fctr         FALSE     NA      NA  28.49497858
## A.make.log                FALSE     NA      NA  24.82458430
## A.fashion.log             FALSE     NA      NA  22.86799201
## H.npnct30.log             FALSE     NA      NA  18.66889549
## A.report.log              FALSE     NA      NA  17.94396169
## A.articl.log              FALSE     NA      NA  17.31584878
## A.npnct04.log             FALSE     NA      NA  16.42726704
## H.X2015.log               FALSE     NA      NA  16.18781324
## A.share.log               FALSE     NA      NA  16.05772852
## H.today.log               FALSE     NA      NA  15.09058570
## H.new.log                 FALSE     NA      NA  14.58884852
## H.has.year.colon          FALSE     NA      NA  14.48643170
## A.said.log                FALSE     NA      NA  13.53465142
## H.ndgts.log               FALSE     NA      NA  13.52245535
## PubDate.last10.log        FALSE     NA      NA  12.97780472
## H.npnct04.log             FALSE     NA      NA  12.76927377
## H.npnct08.log             FALSE     NA      NA  12.06593521
## A.newyork.log             FALSE     NA      NA  11.59720267
## S.npnct15.log             FALSE     NA      NA  10.97856358
## A.npnct15.log             FALSE     NA      NA  10.79302566
## PubDate.wkday.fctr        FALSE     NA      NA  10.68599632
## .rnorm                    FALSE     NA      NA  10.55274877
## A.week.log                FALSE     NA      NA   9.81958866
## H.npnct13.log             FALSE     NA      NA   9.71218057
## A.show.log                FALSE     NA      NA   9.68407375
## A.intern.log              FALSE     NA      NA   9.66632050
## S.time.log                FALSE     NA      NA   9.55323497
## H.npnct07.log             FALSE     NA      NA   9.53685753
## A.npnct18.log             FALSE     NA      NA   9.41984800
## H.npnct06.log             FALSE     NA      NA   9.23627876
## S.state.log               FALSE     NA      NA   8.92098916
## PubDate.minute.fctr       FALSE     NA      NA   8.53101958
## A.npnct19.log             FALSE     NA      NA   8.47715771
## A.can.log                 FALSE     NA      NA   8.10387656
## PubDate.date.fctr         FALSE     NA      NA   8.03583604
## S.npnct12.log             FALSE     NA      NA   8.03465413
## A.npnct12.log             FALSE     NA      NA   7.98621020
## H.nuppr.log               FALSE     NA      NA   7.85639322
## H.week.log                FALSE     NA      NA   7.79627328
## A.state.log               FALSE     NA      NA   7.26321159
## S.npnct13.log             FALSE     NA      NA   7.17910453
## A.npnct13.log             FALSE     NA      NA   7.17784272
## A.presid.log              FALSE     NA      NA   7.16305732
## S.npnct16.log             FALSE     NA      NA   7.14944805
## S.npnct08.log             FALSE     NA      NA   7.07080676
## S.can.log                 FALSE     NA      NA   7.02341628
## A.npnct17.log             FALSE     NA      NA   6.89744623
## A.will.log                FALSE     NA      NA   6.81698075
## S.ndgts.log               FALSE     NA      NA   6.75768296
## S.compani.log             FALSE     NA      NA   6.68327086
## H.npnct05.log             FALSE     NA      NA   6.59752354
## A.npnct21.log             FALSE     NA      NA   6.53946049
## S.nuppr.log               FALSE     NA      NA   6.48716764
## H.nchrs.log               FALSE     NA      NA   6.36565681
## A.year.log                FALSE     NA      NA   6.30335271
## A.npnct01.log             FALSE     NA      NA   6.26548239
## S.will.log                FALSE     NA      NA   6.21988118
## A.ndgts.log               FALSE     NA      NA   6.12230312
## PubDate.last100.log       FALSE     NA      NA   6.10602730
## A.npnct22.log             FALSE     NA      NA   5.81913883
## A.nuppr.log               FALSE     NA      NA   5.73056332
## S.day.log                 FALSE     NA      NA   5.55760002
## S.one.log                 FALSE     NA      NA   5.29435441
## A.one.log                 FALSE     NA      NA   5.19360228
## PubDate.second.fctr       FALSE     NA      NA   4.57971207
## H.X2014.log               FALSE     NA      NA   4.46472089
## H.report.log              FALSE     NA      NA   4.40029310
## S.take.log                FALSE     NA      NA   4.26732531
## S.nwrds.unq.log           FALSE     NA      NA   4.13474268
## S.npnct09.log             FALSE     NA      NA   4.11245488
## A.nwrds.unq.log           FALSE     NA      NA   3.95688107
## H.npnct12.log             FALSE     NA      NA   3.76801998
## H.fashion.log             FALSE     NA      NA   3.76390763
## S.npnct21.log             FALSE     NA      NA   3.66026012
## S.new.log                 FALSE     NA      NA   3.52281315
## H.npnct15.log             FALSE     NA      NA   3.45385040
## A.new.log                 FALSE     NA      NA   3.41102464
## A.nchrs.log               FALSE     NA      NA   3.12081764
## S.nchrs.log               FALSE     NA      NA   2.97802811
## S.npnct30.log             FALSE     NA      NA   2.88153488
## H.npnct17.log             FALSE     NA      NA   2.24624596
## A.npnct06.log             FALSE     NA      NA   2.18674221
## A.npnct30.log             FALSE     NA      NA   2.11807078
## H.nwrds.unq.log           FALSE     NA      NA   2.09016785
## S.nwrds.log               FALSE     NA      NA   2.07125498
## A.npnct07.log             FALSE     NA      NA   2.04536000
## H.npnct01.log             FALSE     NA      NA   2.03495324
## A.nwrds.log               FALSE     NA      NA   1.93513242
## H.has.ebola               FALSE     NA      NA   1.75502087
## A.npnct14.log             FALSE     NA      NA   1.53950062
## S.npnct14.log             FALSE     NA      NA   1.28524340
## PubDate.last1.log         FALSE     NA      NA   1.18736302
## A.npnct02.log             FALSE     NA      NA   1.14672657
## H.day.log                 FALSE     NA      NA   1.12673197
## A.first.log               FALSE     NA      NA   1.11773697
## S.npnct03.log             FALSE     NA      NA   1.07345568
## H.newyork.log             FALSE     NA      NA   1.03389528
## A.has.year.colon          FALSE     NA      NA   0.89413765
## PubDate.wkend             FALSE     NA      NA   0.64555965
## H.npnct16.log             FALSE     NA      NA   0.59350225
## H.npnct14.log             FALSE     NA      NA   0.52235605
## H.nwrds.log               FALSE     NA      NA   0.46021386
## H.npnct02.log             FALSE     NA      NA   0.04154952
## H.daili.log               FALSE     NA      NA   0.00000000
## A.compani.log             FALSE     NA      NA           NA
## A.day.log                 FALSE     NA      NA           NA
## A.has.http                FALSE     NA      NA           NA
## A.npnct03.log             FALSE     NA      NA           NA
## A.npnct05.log             FALSE     NA      NA           NA
## A.npnct08.log             FALSE     NA      NA           NA
## A.npnct09.log             FALSE     NA      NA           NA
## A.npnct10.log             FALSE     NA      NA           NA
## A.npnct11.log             FALSE     NA      NA           NA
## A.npnct16.log             FALSE     NA      NA           NA
## A.npnct20.log             FALSE     NA      NA           NA
## A.npnct23.log             FALSE     NA      NA           NA
## A.npnct24.log             FALSE     NA      NA           NA
## A.npnct25.log             FALSE     NA      NA           NA
## A.npnct26.log             FALSE     NA      NA           NA
## A.npnct27.log             FALSE     NA      NA           NA
## A.npnct28.log             FALSE     NA      NA           NA
## A.npnct29.log             FALSE     NA      NA           NA
## A.npnct31.log             FALSE     NA      NA           NA
## A.npnct32.log             FALSE     NA      NA           NA
## A.take.log                FALSE     NA      NA           NA
## A.time.log                FALSE     NA      NA           NA
## H.has.http                FALSE     NA      NA           NA
## H.npnct03.log             FALSE     NA      NA           NA
## H.npnct09.log             FALSE     NA      NA           NA
## H.npnct10.log             FALSE     NA      NA           NA
## H.npnct11.log             FALSE     NA      NA           NA
## H.npnct18.log             FALSE     NA      NA           NA
## H.npnct19.log             FALSE     NA      NA           NA
## H.npnct20.log             FALSE     NA      NA           NA
## H.npnct22.log             FALSE     NA      NA           NA
## H.npnct23.log             FALSE     NA      NA           NA
## H.npnct24.log             FALSE     NA      NA           NA
## H.npnct25.log             FALSE     NA      NA           NA
## H.npnct26.log             FALSE     NA      NA           NA
## H.npnct27.log             FALSE     NA      NA           NA
## H.npnct28.log             FALSE     NA      NA           NA
## H.npnct29.log             FALSE     NA      NA           NA
## H.npnct31.log             FALSE     NA      NA           NA
## H.npnct32.log             FALSE     NA      NA           NA
## Popular                    TRUE     NA      NA           NA
## Popular.fctr                 NA     NA    TRUE           NA
## PubDate.last1             FALSE     NA      NA           NA
## PubDate.last10            FALSE     NA      NA           NA
## PubDate.last100           FALSE     NA      NA           NA
## PubDate.month.fctr        FALSE     NA      NA           NA
## PubDate.POSIX             FALSE     NA      NA           NA
## PubDate.year.fctr         FALSE     NA      NA           NA
## PubDate.zoo               FALSE     NA      NA           NA
## S.articl.log              FALSE     NA      NA           NA
## S.fashion.log             FALSE     NA      NA           NA
## S.first.log               FALSE     NA      NA           NA
## S.has.http                FALSE     NA      NA           NA
## S.has.year.colon          FALSE     NA      NA           NA
## S.intern.log              FALSE     NA      NA           NA
## S.make.log                FALSE     NA      NA           NA
## S.newyork.log             FALSE     NA      NA           NA
## S.npnct01.log             FALSE     NA      NA           NA
## S.npnct02.log             FALSE     NA      NA           NA
## S.npnct04.log             FALSE     NA      NA           NA
## S.npnct05.log             FALSE     NA      NA           NA
## S.npnct06.log             FALSE     NA      NA           NA
## S.npnct07.log             FALSE     NA      NA           NA
## S.npnct10.log             FALSE     NA      NA           NA
## S.npnct11.log             FALSE     NA      NA           NA
## S.npnct17.log             FALSE     NA      NA           NA
## S.npnct18.log             FALSE     NA      NA           NA
## S.npnct19.log             FALSE     NA      NA           NA
## S.npnct20.log             FALSE     NA      NA           NA
## S.npnct22.log             FALSE     NA      NA           NA
## S.npnct23.log             FALSE     NA      NA           NA
## S.npnct24.log             FALSE     NA      NA           NA
## S.npnct25.log             FALSE     NA      NA           NA
## S.npnct26.log             FALSE     NA      NA           NA
## S.npnct27.log             FALSE     NA      NA           NA
## S.npnct28.log             FALSE     NA      NA           NA
## S.npnct29.log             FALSE     NA      NA           NA
## S.npnct31.log             FALSE     NA      NA           NA
## S.npnct32.log             FALSE     NA      NA           NA
## S.presid.log              FALSE     NA      NA           NA
## S.report.log              FALSE     NA      NA           NA
## S.said.log                FALSE     NA      NA           NA
## S.share.log               FALSE     NA      NA           NA
## S.show.log                FALSE     NA      NA           NA
## S.week.log                FALSE     NA      NA           NA
## S.year.log                FALSE     NA      NA           NA
## UniqueID                  FALSE   TRUE      NA           NA
## WordCount                 FALSE     NA      NA           NA
##                     All.X.glm.importance
## myCategory.fctr             100.00000000
## WordCount.log                70.43241920
## H.npnct21.log                32.71499686
## PubDate.hour.fctr            28.49497858
## A.make.log                   24.82458430
## A.fashion.log                22.86799201
## H.npnct30.log                18.66889549
## A.report.log                 17.94396169
## A.articl.log                 17.31584878
## A.npnct04.log                16.42726704
## H.X2015.log                  16.18781324
## A.share.log                  16.05772852
## H.today.log                  15.09058570
## H.new.log                    14.58884852
## H.has.year.colon             14.48643170
## A.said.log                   13.53465142
## H.ndgts.log                  13.52245535
## PubDate.last10.log           12.97780472
## H.npnct04.log                12.76927377
## H.npnct08.log                12.06593521
## A.newyork.log                11.59720267
## S.npnct15.log                10.97856358
## A.npnct15.log                10.79302566
## PubDate.wkday.fctr           10.68599632
## .rnorm                       10.55274877
## A.week.log                    9.81958866
## H.npnct13.log                 9.71218057
## A.show.log                    9.68407375
## A.intern.log                  9.66632050
## S.time.log                    9.55323497
## H.npnct07.log                 9.53685753
## A.npnct18.log                 9.41984800
## H.npnct06.log                 9.23627876
## S.state.log                   8.92098916
## PubDate.minute.fctr           8.53101958
## A.npnct19.log                 8.47715771
## A.can.log                     8.10387656
## PubDate.date.fctr             8.03583604
## S.npnct12.log                 8.03465413
## A.npnct12.log                 7.98621020
## H.nuppr.log                   7.85639322
## H.week.log                    7.79627328
## A.state.log                   7.26321159
## S.npnct13.log                 7.17910453
## A.npnct13.log                 7.17784272
## A.presid.log                  7.16305732
## S.npnct16.log                 7.14944805
## S.npnct08.log                 7.07080676
## S.can.log                     7.02341628
## A.npnct17.log                 6.89744623
## A.will.log                    6.81698075
## S.ndgts.log                   6.75768296
## S.compani.log                 6.68327086
## H.npnct05.log                 6.59752354
## A.npnct21.log                 6.53946049
## S.nuppr.log                   6.48716764
## H.nchrs.log                   6.36565681
## A.year.log                    6.30335271
## A.npnct01.log                 6.26548239
## S.will.log                    6.21988118
## A.ndgts.log                   6.12230312
## PubDate.last100.log           6.10602730
## A.npnct22.log                 5.81913883
## A.nuppr.log                   5.73056332
## S.day.log                     5.55760002
## S.one.log                     5.29435441
## A.one.log                     5.19360228
## PubDate.second.fctr           4.57971207
## H.X2014.log                   4.46472089
## H.report.log                  4.40029310
## S.take.log                    4.26732531
## S.nwrds.unq.log               4.13474268
## S.npnct09.log                 4.11245488
## A.nwrds.unq.log               3.95688107
## H.npnct12.log                 3.76801998
## H.fashion.log                 3.76390763
## S.npnct21.log                 3.66026012
## S.new.log                     3.52281315
## H.npnct15.log                 3.45385040
## A.new.log                     3.41102464
## A.nchrs.log                   3.12081764
## S.nchrs.log                   2.97802811
## S.npnct30.log                 2.88153488
## H.npnct17.log                 2.24624596
## A.npnct06.log                 2.18674221
## A.npnct30.log                 2.11807078
## H.nwrds.unq.log               2.09016785
## S.nwrds.log                   2.07125498
## A.npnct07.log                 2.04536000
## H.npnct01.log                 2.03495324
## A.nwrds.log                   1.93513242
## H.has.ebola                   1.75502087
## A.npnct14.log                 1.53950062
## S.npnct14.log                 1.28524340
## PubDate.last1.log             1.18736302
## A.npnct02.log                 1.14672657
## H.day.log                     1.12673197
## A.first.log                   1.11773697
## S.npnct03.log                 1.07345568
## H.newyork.log                 1.03389528
## A.has.year.colon              0.89413765
## PubDate.wkend                 0.64555965
## H.npnct16.log                 0.59350225
## H.npnct14.log                 0.52235605
## H.nwrds.log                   0.46021386
## H.npnct02.log                 0.04154952
## H.daili.log                   0.00000000
## A.compani.log                         NA
## A.day.log                             NA
## A.has.http                            NA
## A.npnct03.log                         NA
## A.npnct05.log                         NA
## A.npnct08.log                         NA
## A.npnct09.log                         NA
## A.npnct10.log                         NA
## A.npnct11.log                         NA
## A.npnct16.log                         NA
## A.npnct20.log                         NA
## A.npnct23.log                         NA
## A.npnct24.log                         NA
## A.npnct25.log                         NA
## A.npnct26.log                         NA
## A.npnct27.log                         NA
## A.npnct28.log                         NA
## A.npnct29.log                         NA
## A.npnct31.log                         NA
## A.npnct32.log                         NA
## A.take.log                            NA
## A.time.log                            NA
## H.has.http                            NA
## H.npnct03.log                         NA
## H.npnct09.log                         NA
## H.npnct10.log                         NA
## H.npnct11.log                         NA
## H.npnct18.log                         NA
## H.npnct19.log                         NA
## H.npnct20.log                         NA
## H.npnct22.log                         NA
## H.npnct23.log                         NA
## H.npnct24.log                         NA
## H.npnct25.log                         NA
## H.npnct26.log                         NA
## H.npnct27.log                         NA
## H.npnct28.log                         NA
## H.npnct29.log                         NA
## H.npnct31.log                         NA
## H.npnct32.log                         NA
## Popular                               NA
## Popular.fctr                          NA
## PubDate.last1                         NA
## PubDate.last10                        NA
## PubDate.last100                       NA
## PubDate.month.fctr                    NA
## PubDate.POSIX                         NA
## PubDate.year.fctr                     NA
## PubDate.zoo                           NA
## S.articl.log                          NA
## S.fashion.log                         NA
## S.first.log                           NA
## S.has.http                            NA
## S.has.year.colon                      NA
## S.intern.log                          NA
## S.make.log                            NA
## S.newyork.log                         NA
## S.npnct01.log                         NA
## S.npnct02.log                         NA
## S.npnct04.log                         NA
## S.npnct05.log                         NA
## S.npnct06.log                         NA
## S.npnct07.log                         NA
## S.npnct10.log                         NA
## S.npnct11.log                         NA
## S.npnct17.log                         NA
## S.npnct18.log                         NA
## S.npnct19.log                         NA
## S.npnct20.log                         NA
## S.npnct22.log                         NA
## S.npnct23.log                         NA
## S.npnct24.log                         NA
## S.npnct25.log                         NA
## S.npnct26.log                         NA
## S.npnct27.log                         NA
## S.npnct28.log                         NA
## S.npnct29.log                         NA
## S.npnct31.log                         NA
## S.npnct32.log                         NA
## S.presid.log                          NA
## S.report.log                          NA
## S.said.log                            NA
## S.share.log                           NA
## S.show.log                            NA
## S.week.log                            NA
## S.year.log                            NA
## UniqueID                              NA
## WordCount                             NA
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    if (length(vars <- subset(glb_feats_df, importance > 0)$id) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", length(vars))
        vars <- vars[1:5]
    }
    require(reshape2)
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in vars) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
#         plot_vars_df <- subset(glb_feats_df, importance > 
#                         glb_feats_df[glb_feats_df$id == ".rnorm", "importance"])
        plot_vars_df <- orderBy(~ -importance, glb_feats_df)
        if (nrow(plot_vars_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2],
                                      ".rownames"), 
                                               feat_y=plot_vars_df$id[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_vars)
    #               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1 or 2] is a factor                                                         
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(plot_vars_df <- subset(glb_feats_df, importance > 0)) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], 
                              ".rownames"),
                                               feat_y=plot_vars_df$id[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_vars,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(obs_df=glb_OOBent_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_OOBent_df, mdl_id =
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 106
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-8.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-9.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-10.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-11.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-12.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.All.X.glm.prob
## 6370     6370            Y                        2.220446e-16
## 6018     6018            N                        2.220446e-16
##      Popular.fctr.predict.All.X.glm
## 6370                              N
## 6018                              N
##      Popular.fctr.predict.All.X.glm.accurate
## 6370                                   FALSE
## 6018                                    TRUE
##      Popular.fctr.predict.All.X.glm.error .label
## 6370                                 -0.9   6370
## 6018                                  0.0   6018
## [1] "Inaccurate: "
##     UniqueID Popular.fctr Popular.fctr.predict.All.X.glm.prob
## 4          4            Y                        2.220446e-16
## 104      104            Y                        2.220446e-16
## 95        95            Y                        2.220446e-16
## 56        56            Y                        2.220446e-16
## 37        37            Y                        2.220446e-16
## 172      172            Y                        2.220446e-16
##     Popular.fctr.predict.All.X.glm Popular.fctr.predict.All.X.glm.accurate
## 4                                N                                   FALSE
## 104                              N                                   FALSE
## 95                               N                                   FALSE
## 56                               N                                   FALSE
## 37                               N                                   FALSE
## 172                              N                                   FALSE
##     Popular.fctr.predict.All.X.glm.error
## 4                                   -0.9
## 104                                 -0.9
## 95                                  -0.9
## 56                                  -0.9
## 37                                  -0.9
## 172                                 -0.9
##      UniqueID Popular.fctr Popular.fctr.predict.All.X.glm.prob
## 1588     1588            Y                        2.220446e-16
## 4632     4632            Y                        2.220446e-16
## 5058     5058            Y                        2.220446e-16
## 5573     5573            Y                        2.220446e-16
## 5701     5701            Y                        2.220446e-16
## 725       725            N                        1.000000e+00
##      Popular.fctr.predict.All.X.glm
## 1588                              N
## 4632                              N
## 5058                              N
## 5573                              N
## 5701                              N
## 725                               Y
##      Popular.fctr.predict.All.X.glm.accurate
## 1588                                   FALSE
## 4632                                   FALSE
## 5058                                   FALSE
## 5573                                   FALSE
## 5701                                   FALSE
## 725                                    FALSE
##      Popular.fctr.predict.All.X.glm.error
## 1588                                 -0.9
## 4632                                 -0.9
## 5058                                 -0.9
## 5573                                 -0.9
## 5701                                 -0.9
## 725                                   0.1
##      UniqueID Popular.fctr Popular.fctr.predict.All.X.glm.prob
## 5729     5729            N                                   1
## 6276     6276            N                                   1
## 6329     6329            N                                   1
## 6412     6412            N                                   1
## 6499     6499            N                                   1
## 6479     6479            N                                   1
##      Popular.fctr.predict.All.X.glm
## 5729                              Y
## 6276                              Y
## 6329                              Y
## 6412                              Y
## 6499                              Y
## 6479                              Y
##      Popular.fctr.predict.All.X.glm.accurate
## 5729                                   FALSE
## 6276                                   FALSE
## 6329                                   FALSE
## 6412                                   FALSE
## 6499                                   FALSE
## 6479                                   FALSE
##      Popular.fctr.predict.All.X.glm.error
## 5729                                  0.1
## 6276                                  0.1
## 6329                                  0.1
## 6412                                  0.1
## 6499                                  0.1
## 6479                                  0.1
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_2-13.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBent_df <- glb_get_predictions(df=glb_OOBent_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBent_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBent_df[, glb_rsp_var])$table))
FN_OOB_ids <- c(4721, 4020, 693, 92)
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_OOBent_df), value=TRUE)])
```

```
## [1] Popular.fctr                           
## [2] Popular.fctr.predict.All.X.glm.prob    
## [3] Popular.fctr.predict.All.X.glm         
## [4] Popular.fctr.predict.All.X.glm.accurate
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
## [1] myCategory.fctr   WordCount.log     H.npnct21.log     PubDate.hour.fctr
## [5] A.make.log       
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_txt_vars])
```

```
## [1] Headline Snippet  Abstract
## <0 rows> (or 0-length row.names)
```

```r
write.csv(glb_OOBent_df[, c("UniqueID", 
                grep(glb_rsp_var, names(glb_OOBent_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBent.csv"), row.names=FALSE)

# print(glb_entity_df[glb_entity_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          6          2 432.444 451.331  18.888
## 12 fit.models          6          3 451.332      NA      NA
```


```r
sav_entity_df <- glb_entity_df
print(setdiff(names(glb_trnent_df), names(glb_entity_df)))
```

```
##  [1] "PubDate.year.fctr" "H.has.http"        "H.npnct03.log"    
##  [4] "H.npnct10.log"     "H.npnct11.log"     "H.npnct18.log"    
##  [7] "H.npnct19.log"     "H.npnct20.log"     "H.npnct22.log"    
## [10] "H.npnct23.log"     "H.npnct24.log"     "H.npnct25.log"    
## [13] "H.npnct26.log"     "H.npnct27.log"     "H.npnct28.log"    
## [16] "H.npnct29.log"     "H.npnct31.log"     "H.npnct32.log"    
## [19] "S.has.http"        "S.npnct02.log"     "S.npnct05.log"    
## [22] "S.npnct10.log"     "S.npnct11.log"     "S.npnct18.log"    
## [25] "S.npnct19.log"     "S.npnct20.log"     "S.npnct23.log"    
## [28] "S.npnct24.log"     "S.npnct25.log"     "S.npnct26.log"    
## [31] "S.npnct27.log"     "S.npnct28.log"     "S.npnct29.log"    
## [34] "S.npnct31.log"     "S.npnct32.log"     "A.npnct05.log"    
## [37] "A.npnct10.log"     "A.npnct11.log"     "A.npnct23.log"    
## [40] "A.npnct24.log"     "A.npnct25.log"     "A.npnct26.log"    
## [43] "A.npnct27.log"     "A.npnct28.log"     "A.npnct29.log"    
## [46] "A.npnct31.log"     "A.npnct32.log"
```

```r
print(setdiff(names(glb_fitent_df), names(glb_entity_df)))
```

```
##  [1] "PubDate.year.fctr" "H.has.http"        "H.npnct03.log"    
##  [4] "H.npnct10.log"     "H.npnct11.log"     "H.npnct18.log"    
##  [7] "H.npnct19.log"     "H.npnct20.log"     "H.npnct22.log"    
## [10] "H.npnct23.log"     "H.npnct24.log"     "H.npnct25.log"    
## [13] "H.npnct26.log"     "H.npnct27.log"     "H.npnct28.log"    
## [16] "H.npnct29.log"     "H.npnct31.log"     "H.npnct32.log"    
## [19] "S.has.http"        "S.npnct02.log"     "S.npnct05.log"    
## [22] "S.npnct10.log"     "S.npnct11.log"     "S.npnct18.log"    
## [25] "S.npnct19.log"     "S.npnct20.log"     "S.npnct23.log"    
## [28] "S.npnct24.log"     "S.npnct25.log"     "S.npnct26.log"    
## [31] "S.npnct27.log"     "S.npnct28.log"     "S.npnct29.log"    
## [34] "S.npnct31.log"     "S.npnct32.log"     "A.npnct05.log"    
## [37] "A.npnct10.log"     "A.npnct11.log"     "A.npnct23.log"    
## [40] "A.npnct24.log"     "A.npnct25.log"     "A.npnct26.log"    
## [43] "A.npnct27.log"     "A.npnct28.log"     "A.npnct29.log"    
## [46] "A.npnct31.log"     "A.npnct32.log"
```

```r
print(setdiff(names(glb_OOBent_df), names(glb_entity_df)))
```

```
##  [1] "PubDate.year.fctr"                      
##  [2] "H.has.http"                             
##  [3] "H.npnct03.log"                          
##  [4] "H.npnct10.log"                          
##  [5] "H.npnct11.log"                          
##  [6] "H.npnct18.log"                          
##  [7] "H.npnct19.log"                          
##  [8] "H.npnct20.log"                          
##  [9] "H.npnct22.log"                          
## [10] "H.npnct23.log"                          
## [11] "H.npnct24.log"                          
## [12] "H.npnct25.log"                          
## [13] "H.npnct26.log"                          
## [14] "H.npnct27.log"                          
## [15] "H.npnct28.log"                          
## [16] "H.npnct29.log"                          
## [17] "H.npnct31.log"                          
## [18] "H.npnct32.log"                          
## [19] "S.has.http"                             
## [20] "S.npnct02.log"                          
## [21] "S.npnct05.log"                          
## [22] "S.npnct10.log"                          
## [23] "S.npnct11.log"                          
## [24] "S.npnct18.log"                          
## [25] "S.npnct19.log"                          
## [26] "S.npnct20.log"                          
## [27] "S.npnct23.log"                          
## [28] "S.npnct24.log"                          
## [29] "S.npnct25.log"                          
## [30] "S.npnct26.log"                          
## [31] "S.npnct27.log"                          
## [32] "S.npnct28.log"                          
## [33] "S.npnct29.log"                          
## [34] "S.npnct31.log"                          
## [35] "S.npnct32.log"                          
## [36] "A.npnct05.log"                          
## [37] "A.npnct10.log"                          
## [38] "A.npnct11.log"                          
## [39] "A.npnct23.log"                          
## [40] "A.npnct24.log"                          
## [41] "A.npnct25.log"                          
## [42] "A.npnct26.log"                          
## [43] "A.npnct27.log"                          
## [44] "A.npnct28.log"                          
## [45] "A.npnct29.log"                          
## [46] "A.npnct31.log"                          
## [47] "A.npnct32.log"                          
## [48] "Popular.fctr.predict.All.X.glm.prob"    
## [49] "Popular.fctr.predict.All.X.glm"         
## [50] "Popular.fctr.predict.All.X.glm.accurate"
```

```r
for (col in setdiff(names(glb_OOBent_df), names(glb_entity_df)))
    # Merge or cbind ?
    glb_entity_df[glb_entity_df$.lcn == "OOB", col] <- glb_OOBent_df[, col]
    
print(setdiff(names(glb_newent_df), names(glb_entity_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_entity_df, #glb_trnent_df, glb_fitent_df, glb_OOBent_df, glb_newent_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](NYTBlogs_txtfeat_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 12        fit.models          6          3 451.332 458.164   6.833
## 13 fit.data.training          7          0 458.165      NA      NA
```

## Step `7.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
    print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
                                              entity_df=glb_fitent_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
                            indep_vars_vctr=mdl_feats_df$id, model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnent_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
##                                      id   importance
## myCategory.fctr         myCategory.fctr 100.00000000
## WordCount.log             WordCount.log  70.43241920
## H.npnct21.log             H.npnct21.log  32.71499686
## PubDate.hour.fctr     PubDate.hour.fctr  28.49497858
## A.make.log                   A.make.log  24.82458430
## A.fashion.log             A.fashion.log  22.86799201
## H.npnct30.log             H.npnct30.log  18.66889549
## A.report.log               A.report.log  17.94396169
## A.articl.log               A.articl.log  17.31584878
## A.npnct04.log             A.npnct04.log  16.42726704
## H.X2015.log                 H.X2015.log  16.18781324
## A.share.log                 A.share.log  16.05772852
## H.today.log                 H.today.log  15.09058570
## H.new.log                     H.new.log  14.58884852
## H.has.year.colon       H.has.year.colon  14.48643170
## A.said.log                   A.said.log  13.53465142
## H.ndgts.log                 H.ndgts.log  13.52245535
## PubDate.last10.log   PubDate.last10.log  12.97780472
## H.npnct04.log             H.npnct04.log  12.76927377
## H.npnct08.log             H.npnct08.log  12.06593521
## A.newyork.log             A.newyork.log  11.59720267
## S.npnct15.log             S.npnct15.log  10.97856358
## A.npnct15.log             A.npnct15.log  10.79302566
## PubDate.wkday.fctr   PubDate.wkday.fctr  10.68599632
## .rnorm                           .rnorm  10.55274877
## A.week.log                   A.week.log   9.81958866
## H.npnct13.log             H.npnct13.log   9.71218057
## A.show.log                   A.show.log   9.68407375
## A.intern.log               A.intern.log   9.66632050
## S.time.log                   S.time.log   9.55323497
## H.npnct07.log             H.npnct07.log   9.53685753
## A.npnct18.log             A.npnct18.log   9.41984800
## H.npnct06.log             H.npnct06.log   9.23627876
## S.state.log                 S.state.log   8.92098916
## PubDate.minute.fctr PubDate.minute.fctr   8.53101958
## A.npnct19.log             A.npnct19.log   8.47715771
## A.can.log                     A.can.log   8.10387656
## PubDate.date.fctr     PubDate.date.fctr   8.03583604
## S.npnct12.log             S.npnct12.log   8.03465413
## A.npnct12.log             A.npnct12.log   7.98621020
## H.nuppr.log                 H.nuppr.log   7.85639322
## H.week.log                   H.week.log   7.79627328
## A.state.log                 A.state.log   7.26321159
## S.npnct13.log             S.npnct13.log   7.17910453
## A.npnct13.log             A.npnct13.log   7.17784272
## A.presid.log               A.presid.log   7.16305732
## S.npnct16.log             S.npnct16.log   7.14944805
## S.npnct08.log             S.npnct08.log   7.07080676
## S.can.log                     S.can.log   7.02341628
## A.npnct17.log             A.npnct17.log   6.89744623
## A.will.log                   A.will.log   6.81698075
## S.ndgts.log                 S.ndgts.log   6.75768296
## S.compani.log             S.compani.log   6.68327086
## H.npnct05.log             H.npnct05.log   6.59752354
## A.npnct21.log             A.npnct21.log   6.53946049
## S.nuppr.log                 S.nuppr.log   6.48716764
## H.nchrs.log                 H.nchrs.log   6.36565681
## A.year.log                   A.year.log   6.30335271
## A.npnct01.log             A.npnct01.log   6.26548239
## S.will.log                   S.will.log   6.21988118
## A.ndgts.log                 A.ndgts.log   6.12230312
## PubDate.last100.log PubDate.last100.log   6.10602730
## A.npnct22.log             A.npnct22.log   5.81913883
## A.nuppr.log                 A.nuppr.log   5.73056332
## S.day.log                     S.day.log   5.55760002
## S.one.log                     S.one.log   5.29435441
## A.one.log                     A.one.log   5.19360228
## PubDate.second.fctr PubDate.second.fctr   4.57971207
## H.X2014.log                 H.X2014.log   4.46472089
## H.report.log               H.report.log   4.40029310
## S.take.log                   S.take.log   4.26732531
## S.nwrds.unq.log         S.nwrds.unq.log   4.13474268
## S.npnct09.log             S.npnct09.log   4.11245488
## A.nwrds.unq.log         A.nwrds.unq.log   3.95688107
## H.npnct12.log             H.npnct12.log   3.76801998
## H.fashion.log             H.fashion.log   3.76390763
## S.npnct21.log             S.npnct21.log   3.66026012
## S.new.log                     S.new.log   3.52281315
## H.npnct15.log             H.npnct15.log   3.45385040
## A.new.log                     A.new.log   3.41102464
## A.nchrs.log                 A.nchrs.log   3.12081764
## S.nchrs.log                 S.nchrs.log   2.97802811
## S.npnct30.log             S.npnct30.log   2.88153488
## H.npnct17.log             H.npnct17.log   2.24624596
## A.npnct06.log             A.npnct06.log   2.18674221
## A.npnct30.log             A.npnct30.log   2.11807078
## H.nwrds.unq.log         H.nwrds.unq.log   2.09016785
## S.nwrds.log                 S.nwrds.log   2.07125498
## A.npnct07.log             A.npnct07.log   2.04536000
## H.npnct01.log             H.npnct01.log   2.03495324
## A.nwrds.log                 A.nwrds.log   1.93513242
## H.has.ebola                 H.has.ebola   1.75502087
## A.npnct14.log             A.npnct14.log   1.53950062
## S.npnct14.log             S.npnct14.log   1.28524340
## PubDate.last1.log     PubDate.last1.log   1.18736302
## A.npnct02.log             A.npnct02.log   1.14672657
## H.day.log                     H.day.log   1.12673197
## A.first.log                 A.first.log   1.11773697
## S.npnct03.log             S.npnct03.log   1.07345568
## H.newyork.log             H.newyork.log   1.03389528
## A.has.year.colon       A.has.year.colon   0.89413765
## PubDate.wkend             PubDate.wkend   0.64555965
## H.npnct16.log             H.npnct16.log   0.59350225
## H.npnct14.log             H.npnct14.log   0.52235605
## H.nwrds.log                 H.nwrds.log   0.46021386
## H.npnct02.log             H.npnct02.log   0.04154952
## H.daili.log                 H.daili.log   0.00000000
## [1] "fitting model: Final.glm"
## [1] "    indep_vars: myCategory.fctr, WordCount.log, H.npnct21.log, PubDate.hour.fctr, A.make.log, A.fashion.log, H.npnct30.log, A.report.log, A.articl.log, A.npnct04.log, H.X2015.log, A.share.log, H.today.log, H.new.log, H.has.year.colon, A.said.log, H.ndgts.log, PubDate.last10.log, H.npnct04.log, H.npnct08.log, A.newyork.log, S.npnct15.log, A.npnct15.log, PubDate.wkday.fctr, .rnorm, A.week.log, H.npnct13.log, A.show.log, A.intern.log, S.time.log, H.npnct07.log, A.npnct18.log, H.npnct06.log, S.state.log, PubDate.minute.fctr, A.npnct19.log, A.can.log, PubDate.date.fctr, S.npnct12.log, A.npnct12.log, H.nuppr.log, H.week.log, A.state.log, S.npnct13.log, A.npnct13.log, A.presid.log, S.npnct16.log, S.npnct08.log, S.can.log, A.npnct17.log, A.will.log, S.ndgts.log, S.compani.log, H.npnct05.log, A.npnct21.log, S.nuppr.log, H.nchrs.log, A.year.log, A.npnct01.log, S.will.log, A.ndgts.log, PubDate.last100.log, A.npnct22.log, A.nuppr.log, S.day.log, S.one.log, A.one.log, PubDate.second.fctr, H.X2014.log, H.report.log, S.take.log, S.nwrds.unq.log, S.npnct09.log, A.nwrds.unq.log, H.npnct12.log, H.fashion.log, S.npnct21.log, S.new.log, H.npnct15.log, A.new.log, A.nchrs.log, S.nchrs.log, S.npnct30.log, H.npnct17.log, A.npnct06.log, A.npnct30.log, H.nwrds.unq.log, S.nwrds.log, A.npnct07.log, H.npnct01.log, A.nwrds.log, H.has.ebola, A.npnct14.log, S.npnct14.log, PubDate.last1.log, A.npnct02.log, H.day.log, A.first.log, S.npnct03.log, H.newyork.log, A.has.year.colon, PubDate.wkend, H.npnct16.log, H.npnct14.log, H.nwrds.log, H.npnct02.log, H.daili.log"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: not plotting observations with leverage one:
##   1651, 3675, 5299, 5552
```

![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_0-1.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_0-2.png) 

```
## Warning: not plotting observations with leverage one:
##   1651, 3675, 5299, 5552
```

![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_0-3.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_0-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.7951  -0.3225  -0.1419   0.0000   3.4577  
## 
## Coefficients:
##                                                         Estimate
## (Intercept)                                           -4.703e+00
## `myCategory.fctrForeign#World#Asia Pacific`           -4.586e+00
## `myCategory.fctr#Multimedia#`                         -4.800e+00
## `myCategory.fctrCulture#Arts#`                        -2.581e+00
## `myCategory.fctrBusiness#Business Day#Dealbook`       -2.449e+00
## myCategory.fctrmyOther                                -2.007e+01
## `myCategory.fctrBusiness#Technology#`                 -1.907e+00
## `myCategory.fctrBusiness#Crosswords/Games#`            7.029e-01
## `myCategory.fctrTStyle##`                             -4.321e+00
## `myCategory.fctrForeign#World#`                       -1.726e+01
## `myCategory.fctrOpEd#Opinion#`                         8.941e-01
## `myCategory.fctrStyles##Fashion`                      -5.554e+00
## `myCategory.fctr#Opinion#Room For Debate`             -5.375e+00
## `myCategory.fctr#U.S.#Education`                      -2.057e+01
## `myCategory.fctr##`                                   -2.679e+00
## `myCategory.fctrMetro#N.Y. / Region#`                 -1.590e+00
## `myCategory.fctrBusiness#Business Day#Small Business` -4.242e+00
## `myCategory.fctrStyles#U.S.#`                         -4.162e-01
## `myCategory.fctrTravel#Travel#`                       -4.237e+00
## `myCategory.fctr#Opinion#The Public Editor`            3.172e-01
## WordCount.log                                          1.149e+00
## H.npnct21.log                                          1.440e+00
## `PubDate.hour.fctr(7.67,15.3]`                        -3.019e-03
## `PubDate.hour.fctr(15.3,23]`                           8.004e-02
## A.make.log                                            -2.977e-01
## A.fashion.log                                         -1.838e+00
## H.npnct30.log                                         -3.352e-01
## A.report.log                                          -5.740e-01
## A.articl.log                                           6.064e-01
## A.npnct04.log                                         -9.268e-01
## H.X2015.log                                           -1.967e+01
## A.share.log                                           -8.346e-01
## H.today.log                                           -4.180e+00
## H.new.log                                             -7.403e-01
## H.has.year.colon                                      -1.443e+01
## A.said.log                                             1.052e+00
## H.ndgts.log                                            3.582e-01
## PubDate.last10.log                                     2.094e-01
## H.npnct04.log                                         -1.131e+00
## H.npnct08.log                                          1.891e+00
## A.newyork.log                                          6.115e-01
## S.npnct15.log                                          1.003e+02
## A.npnct15.log                                         -9.961e+01
## PubDate.wkday.fctr1                                   -3.939e-01
## PubDate.wkday.fctr2                                   -8.760e-01
## PubDate.wkday.fctr3                                   -6.039e-01
## PubDate.wkday.fctr4                                   -7.006e-01
## PubDate.wkday.fctr5                                   -7.768e-01
## PubDate.wkday.fctr6                                   -9.077e-01
## .rnorm                                                -7.739e-02
## A.week.log                                            -2.767e-01
## H.npnct13.log                                          1.868e-01
## A.show.log                                            -4.664e-01
## A.intern.log                                           5.861e-01
## S.time.log                                            -4.407e-01
## H.npnct07.log                                          2.998e-02
## A.npnct18.log                                          6.192e+01
## H.npnct06.log                                          1.272e+00
## S.state.log                                           -3.173e+01
## `PubDate.minute.fctr(14.8,29.5]`                      -3.771e-02
## `PubDate.minute.fctr(29.5,44.2]`                      -2.182e-01
## `PubDate.minute.fctr(44.2,59.1]`                       1.489e-02
## A.npnct19.log                                          1.820e+02
## A.can.log                                              6.911e+01
## `PubDate.date.fctr(7,13]`                              1.065e-01
## `PubDate.date.fctr(13,19]`                            -1.008e-01
## `PubDate.date.fctr(19,25]`                             1.680e-02
## `PubDate.date.fctr(25,31]`                             2.584e-02
## S.npnct12.log                                          7.793e+01
## A.npnct12.log                                         -7.802e+01
## H.nuppr.log                                            9.664e-01
## H.week.log                                            -6.770e-01
## A.state.log                                            3.257e+01
## S.npnct13.log                                          1.102e+01
## A.npnct13.log                                         -1.116e+01
## A.presid.log                                           1.158e-01
## S.npnct16.log                                         -1.439e-01
## S.npnct08.log                                          1.232e+01
## S.can.log                                             -7.016e+01
## A.npnct17.log                                          3.489e-01
## A.will.log                                            -4.967e+00
## S.ndgts.log                                            3.571e+01
## S.compani.log                                         -4.420e-01
## H.npnct05.log                                          1.872e-01
## A.npnct21.log                                          9.161e+00
## S.nuppr.log                                            9.535e+00
## H.nchrs.log                                           -1.115e+00
## A.year.log                                            -6.910e-01
## A.npnct01.log                                          2.057e+00
## S.will.log                                             4.508e+00
## A.ndgts.log                                           -3.598e+01
## PubDate.last100.log                                   -1.260e-03
## A.npnct22.log                                         -2.343e+01
## A.nuppr.log                                           -1.015e+01
## S.day.log                                              2.442e-01
## S.one.log                                             -5.102e+01
## A.one.log                                              5.107e+01
## `PubDate.second.fctr(14.8,29.5]`                      -4.725e-02
## `PubDate.second.fctr(29.5,44.2]`                      -1.795e-02
## `PubDate.second.fctr(44.2,59.1]`                      -1.969e-01
## H.X2014.log                                           -4.651e-01
## H.report.log                                          -1.603e-01
## S.take.log                                            -3.684e-01
## S.nwrds.unq.log                                       -1.206e+02
## S.npnct09.log                                         -1.178e+01
## A.nwrds.unq.log                                        1.206e+02
## H.npnct12.log                                          3.538e-01
## H.fashion.log                                          5.895e-01
## S.npnct21.log                                         -7.673e+00
## S.new.log                                              7.216e+00
## H.npnct15.log                                         -1.888e+01
## A.new.log                                             -7.365e+00
## A.nchrs.log                                            2.128e+01
## S.nchrs.log                                           -2.084e+01
## S.npnct30.log                                         -2.817e+01
## H.npnct17.log                                         -6.238e-01
## A.npnct06.log                                          2.675e-01
## A.npnct30.log                                          1.347e+01
## H.nwrds.unq.log                                       -1.799e+00
## S.nwrds.log                                            1.426e+02
## A.npnct07.log                                         -2.529e+01
## H.npnct01.log                                         -1.421e+00
## A.nwrds.log                                           -1.435e+02
## H.has.ebola                                           -5.791e-01
## A.npnct14.log                                          1.125e+00
## S.npnct14.log                                         -1.404e-01
## PubDate.last1.log                                     -3.608e-02
## A.npnct02.log                                         -1.893e+01
## H.day.log                                             -7.032e-01
## A.first.log                                           -1.731e-01
## S.npnct03.log                                         -2.809e+01
## H.newyork.log                                         -1.026e-01
## A.has.year.colon                                       1.433e+00
## PubDate.wkend                                         -1.118e-01
## H.npnct16.log                                          2.932e-02
## H.npnct14.log                                         -1.322e-01
## H.nwrds.log                                            1.573e+00
## H.npnct02.log                                         -1.680e+01
## H.daili.log                                           -2.317e+01
##                                                       Std. Error z value
## (Intercept)                                            1.684e+00  -2.792
## `myCategory.fctrForeign#World#Asia Pacific`            6.322e-01  -7.254
## `myCategory.fctr#Multimedia#`                          7.710e-01  -6.226
## `myCategory.fctrCulture#Arts#`                         2.924e-01  -8.827
## `myCategory.fctrBusiness#Business Day#Dealbook`        2.473e-01  -9.903
## myCategory.fctrmyOther                                 1.509e+03  -0.013
## `myCategory.fctrBusiness#Technology#`                  2.632e-01  -7.245
## `myCategory.fctrBusiness#Crosswords/Games#`            3.756e-01   1.872
## `myCategory.fctrTStyle##`                              4.181e-01 -10.334
## `myCategory.fctrForeign#World#`                        6.641e+02  -0.026
## `myCategory.fctrOpEd#Opinion#`                         2.438e-01   3.667
## `myCategory.fctrStyles##Fashion`                       1.314e+00  -4.227
## `myCategory.fctr#Opinion#Room For Debate`              5.194e-01 -10.349
## `myCategory.fctr#U.S.#Education`                       5.051e+02  -0.041
## `myCategory.fctr##`                                    2.310e-01 -11.599
## `myCategory.fctrMetro#N.Y. / Region#`                  3.997e-01  -3.978
## `myCategory.fctrBusiness#Business Day#Small Business`  5.328e-01  -7.962
## `myCategory.fctrStyles#U.S.#`                          2.731e-01  -1.524
## `myCategory.fctrTravel#Travel#`                        1.030e+00  -4.112
## `myCategory.fctr#Opinion#The Public Editor`            6.612e-01   0.480
## WordCount.log                                          7.444e-02  15.434
## H.npnct21.log                                          2.589e-01   5.562
## `PubDate.hour.fctr(7.67,15.3]`                         1.964e-01  -0.015
## `PubDate.hour.fctr(15.3,23]`                           1.990e-01   0.402
## A.make.log                                             3.435e-01  -0.867
## A.fashion.log                                          1.607e+00  -1.144
## H.npnct30.log                                          1.601e+00  -0.209
## A.report.log                                           4.597e-01  -1.249
## A.articl.log                                           8.658e-01   0.700
## A.npnct04.log                                          5.155e-01  -1.798
## H.X2015.log                                            9.465e+02  -0.021
## A.share.log                                            5.332e-01  -1.565
## H.today.log                                            9.307e-01  -4.491
## H.new.log                                              4.833e-01  -1.532
## H.has.year.colon                                       6.434e+02  -0.022
## A.said.log                                             3.247e-01   3.239
## H.ndgts.log                                            2.139e-01   1.675
## PubDate.last10.log                                     9.792e-02   2.138
## H.npnct04.log                                          6.965e-01  -1.624
## H.npnct08.log                                          6.189e-01   3.055
## A.newyork.log                                          4.543e-01   1.346
## S.npnct15.log                                          2.347e+04   0.004
## A.npnct15.log                                          2.347e+04  -0.004
## PubDate.wkday.fctr1                                    4.216e-01  -0.934
## PubDate.wkday.fctr2                                    4.594e-01  -1.907
## PubDate.wkday.fctr3                                    4.545e-01  -1.329
## PubDate.wkday.fctr4                                    4.481e-01  -1.563
## PubDate.wkday.fctr5                                    4.541e-01  -1.711
## PubDate.wkday.fctr6                                    3.743e-01  -2.425
## .rnorm                                                 5.061e-02  -1.529
## A.week.log                                             4.013e-01  -0.689
## H.npnct13.log                                          2.499e-01   0.748
## A.show.log                                             4.850e-01  -0.962
## A.intern.log                                           7.392e-01   0.793
## S.time.log                                             3.685e-01  -1.196
## H.npnct07.log                                          1.540e-01   0.195
## A.npnct18.log                                          2.256e+04   0.003
## H.npnct06.log                                          7.949e-01   1.601
## S.state.log                                            1.901e+04  -0.002
## `PubDate.minute.fctr(14.8,29.5]`                       1.462e-01  -0.258
## `PubDate.minute.fctr(29.5,44.2]`                       1.427e-01  -1.529
## `PubDate.minute.fctr(44.2,59.1]`                       1.495e-01   0.100
## A.npnct19.log                                          3.052e+04   0.006
## A.can.log                                              2.652e+04   0.003
## `PubDate.date.fctr(7,13]`                              1.587e-01   0.671
## `PubDate.date.fctr(13,19]`                             1.579e-01  -0.638
## `PubDate.date.fctr(19,25]`                             1.544e-01   0.109
## `PubDate.date.fctr(25,31]`                             1.679e-01   0.154
## S.npnct12.log                                          9.819e+03   0.008
## A.npnct12.log                                          9.819e+03  -0.008
## H.nuppr.log                                            3.414e-01   2.831
## H.week.log                                             7.845e-01  -0.863
## A.state.log                                            1.901e+04   0.002
## S.npnct13.log                                          3.317e+03   0.003
## A.npnct13.log                                          3.317e+03  -0.003
## A.presid.log                                           4.249e-01   0.273
## S.npnct16.log                                          3.982e-01  -0.361
## S.npnct08.log                                          7.757e+03   0.002
## S.can.log                                              2.652e+04  -0.003
## A.npnct17.log                                          1.006e+00   0.347
## A.will.log                                             1.554e+04   0.000
## S.ndgts.log                                            3.118e+03   0.011
## S.compani.log                                          3.290e-01  -1.343
## H.npnct05.log                                          1.610e+00   0.116
## A.npnct21.log                                          5.403e+00   1.696
## S.nuppr.log                                            2.509e+01   0.380
## H.nchrs.log                                            3.462e-01  -3.222
## A.year.log                                             3.813e-01  -1.812
## A.npnct01.log                                          1.095e+00   1.879
## S.will.log                                             1.554e+04   0.000
## A.ndgts.log                                            3.118e+03  -0.012
## PubDate.last100.log                                    3.599e-02  -0.035
## A.npnct22.log                                          4.047e+03  -0.006
## A.nuppr.log                                            2.509e+01  -0.405
## S.day.log                                              4.805e-01   0.508
## S.one.log                                              1.913e+04  -0.003
## A.one.log                                              1.913e+04   0.003
## `PubDate.second.fctr(14.8,29.5]`                       1.424e-01  -0.332
## `PubDate.second.fctr(29.5,44.2]`                       1.406e-01  -0.128
## `PubDate.second.fctr(44.2,59.1]`                       1.429e-01  -1.378
## H.X2014.log                                            1.450e+00  -0.321
## H.report.log                                           7.278e-01  -0.220
## S.take.log                                             4.545e-01  -0.811
## S.nwrds.unq.log                                        1.079e+02  -1.117
## S.npnct09.log                                          7.757e+03  -0.002
## A.nwrds.unq.log                                        1.079e+02   1.117
## H.npnct12.log                                          1.712e-01   2.067
## H.fashion.log                                          1.895e+00   0.311
## S.npnct21.log                                          5.401e+00  -1.421
## S.new.log                                              6.984e+03   0.001
## H.npnct15.log                                          9.999e+02  -0.019
## A.new.log                                              6.984e+03  -0.001
## A.nchrs.log                                            4.605e+01   0.462
## S.nchrs.log                                            4.605e+01  -0.453
## S.npnct30.log                                          1.689e+04  -0.002
## H.npnct17.log                                          8.741e-01  -0.714
## A.npnct06.log                                          1.002e+00   0.267
## A.npnct30.log                                          1.661e+04   0.001
## H.nwrds.unq.log                                        1.652e+00  -1.089
## S.nwrds.log                                            1.163e+02   1.226
## A.npnct07.log                                          5.545e+03  -0.005
## H.npnct01.log                                          9.521e-01  -1.492
## A.nwrds.log                                            1.163e+02  -1.234
## H.has.ebola                                            3.775e-01  -1.534
## A.npnct14.log                                          2.412e+00   0.467
## S.npnct14.log                                          2.397e+00  -0.059
## PubDate.last1.log                                      3.568e-02  -1.011
## A.npnct02.log                                          1.144e+04  -0.002
## H.day.log                                              7.466e-01  -0.942
## A.first.log                                            5.389e-01  -0.321
## S.npnct03.log                                          5.276e+03  -0.005
## H.newyork.log                                          6.534e-01  -0.157
## A.has.year.colon                                       2.902e+03   0.000
## PubDate.wkend                                          3.559e-01  -0.314
## H.npnct16.log                                          2.291e-01   0.128
## H.npnct14.log                                          1.633e-01  -0.810
## H.nwrds.log                                            1.691e+00   0.930
## H.npnct02.log                                          2.115e+03  -0.008
## H.daili.log                                            1.183e+03  -0.020
##                                                       Pr(>|z|)    
## (Intercept)                                           0.005238 ** 
## `myCategory.fctrForeign#World#Asia Pacific`           4.04e-13 ***
## `myCategory.fctr#Multimedia#`                         4.79e-10 ***
## `myCategory.fctrCulture#Arts#`                         < 2e-16 ***
## `myCategory.fctrBusiness#Business Day#Dealbook`        < 2e-16 ***
## myCategory.fctrmyOther                                0.989390    
## `myCategory.fctrBusiness#Technology#`                 4.33e-13 ***
## `myCategory.fctrBusiness#Crosswords/Games#`           0.061258 .  
## `myCategory.fctrTStyle##`                              < 2e-16 ***
## `myCategory.fctrForeign#World#`                       0.979269    
## `myCategory.fctrOpEd#Opinion#`                        0.000246 ***
## `myCategory.fctrStyles##Fashion`                      2.37e-05 ***
## `myCategory.fctr#Opinion#Room For Debate`              < 2e-16 ***
## `myCategory.fctr#U.S.#Education`                      0.967513    
## `myCategory.fctr##`                                    < 2e-16 ***
## `myCategory.fctrMetro#N.Y. / Region#`                 6.96e-05 ***
## `myCategory.fctrBusiness#Business Day#Small Business` 1.70e-15 ***
## `myCategory.fctrStyles#U.S.#`                         0.127494    
## `myCategory.fctrTravel#Travel#`                       3.92e-05 ***
## `myCategory.fctr#Opinion#The Public Editor`           0.631423    
## WordCount.log                                          < 2e-16 ***
## H.npnct21.log                                         2.67e-08 ***
## `PubDate.hour.fctr(7.67,15.3]`                        0.987735    
## `PubDate.hour.fctr(15.3,23]`                          0.687506    
## A.make.log                                            0.386076    
## A.fashion.log                                         0.252622    
## H.npnct30.log                                         0.834192    
## A.report.log                                          0.211815    
## A.articl.log                                          0.483682    
## A.npnct04.log                                         0.072233 .  
## H.X2015.log                                           0.983420    
## A.share.log                                           0.117535    
## H.today.log                                           7.08e-06 ***
## H.new.log                                             0.125614    
## H.has.year.colon                                      0.982112    
## A.said.log                                            0.001201 ** 
## H.ndgts.log                                           0.093970 .  
## PubDate.last10.log                                    0.032500 *  
## H.npnct04.log                                         0.104475    
## H.npnct08.log                                         0.002252 ** 
## A.newyork.log                                         0.178243    
## S.npnct15.log                                         0.996591    
## A.npnct15.log                                         0.996614    
## PubDate.wkday.fctr1                                   0.350157    
## PubDate.wkday.fctr2                                   0.056542 .  
## PubDate.wkday.fctr3                                   0.183992    
## PubDate.wkday.fctr4                                   0.117942    
## PubDate.wkday.fctr5                                   0.087132 .  
## PubDate.wkday.fctr6                                   0.015304 *  
## .rnorm                                                0.126256    
## A.week.log                                            0.490578    
## H.npnct13.log                                         0.454737    
## A.show.log                                            0.336240    
## A.intern.log                                          0.427827    
## S.time.log                                            0.231664    
## H.npnct07.log                                         0.845600    
## A.npnct18.log                                         0.997810    
## H.npnct06.log                                         0.109454    
## S.state.log                                           0.998668    
## `PubDate.minute.fctr(14.8,29.5]`                      0.796534    
## `PubDate.minute.fctr(29.5,44.2]`                      0.126146    
## `PubDate.minute.fctr(44.2,59.1]`                      0.920639    
## A.npnct19.log                                         0.995242    
## A.can.log                                             0.997921    
## `PubDate.date.fctr(7,13]`                             0.502114    
## `PubDate.date.fctr(13,19]`                            0.523288    
## `PubDate.date.fctr(19,25]`                            0.913347    
## `PubDate.date.fctr(25,31]`                            0.877650    
## S.npnct12.log                                         0.993668    
## A.npnct12.log                                         0.993660    
## H.nuppr.log                                           0.004646 ** 
## H.week.log                                            0.388134    
## A.state.log                                           0.998633    
## S.npnct13.log                                         0.997350    
## A.npnct13.log                                         0.997316    
## A.presid.log                                          0.785194    
## S.npnct16.log                                         0.717764    
## S.npnct08.log                                         0.998733    
## S.can.log                                             0.997889    
## A.npnct17.log                                         0.728605    
## A.will.log                                            0.999745    
## S.ndgts.log                                           0.990863    
## S.compani.log                                         0.179128    
## H.npnct05.log                                         0.907448    
## A.npnct21.log                                         0.089977 .  
## S.nuppr.log                                           0.703948    
## H.nchrs.log                                           0.001272 ** 
## A.year.log                                            0.069954 .  
## A.npnct01.log                                         0.060251 .  
## S.will.log                                            0.999769    
## A.ndgts.log                                           0.990792    
## PubDate.last100.log                                   0.972073    
## A.npnct22.log                                         0.995380    
## A.nuppr.log                                           0.685809    
## S.day.log                                             0.611375    
## S.one.log                                             0.997872    
## A.one.log                                             0.997870    
## `PubDate.second.fctr(14.8,29.5]`                      0.740007    
## `PubDate.second.fctr(29.5,44.2]`                      0.898388    
## `PubDate.second.fctr(44.2,59.1]`                      0.168256    
## H.X2014.log                                           0.748419    
## H.report.log                                          0.825737    
## S.take.log                                            0.417575    
## S.nwrds.unq.log                                       0.264019    
## S.npnct09.log                                         0.998788    
## A.nwrds.unq.log                                       0.263817    
## H.npnct12.log                                         0.038756 *  
## H.fashion.log                                         0.755699    
## S.npnct21.log                                         0.155430    
## S.new.log                                             0.999176    
## H.npnct15.log                                         0.984939    
## A.new.log                                             0.999159    
## A.nchrs.log                                           0.644109    
## S.nchrs.log                                           0.650888    
## S.npnct30.log                                         0.998669    
## H.npnct17.log                                         0.475467    
## A.npnct06.log                                         0.789539    
## A.npnct30.log                                         0.999353    
## H.nwrds.unq.log                                       0.276153    
## S.nwrds.log                                           0.220013    
## A.npnct07.log                                         0.996362    
## H.npnct01.log                                         0.135659    
## A.nwrds.log                                           0.217097    
## H.has.ebola                                           0.125082    
## A.npnct14.log                                         0.640829    
## S.npnct14.log                                         0.953299    
## PubDate.last1.log                                     0.311935    
## A.npnct02.log                                         0.998679    
## H.day.log                                             0.346247    
## A.first.log                                           0.748119    
## S.npnct03.log                                         0.995753    
## H.newyork.log                                         0.875286    
## A.has.year.colon                                      0.999606    
## PubDate.wkend                                         0.753421    
## H.npnct16.log                                         0.898176    
## H.npnct14.log                                         0.418203    
## H.nwrds.log                                           0.352283    
## H.npnct02.log                                         0.993661    
## H.daili.log                                           0.984380    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5900.1  on 6531  degrees of freedom
## Residual deviance: 2737.4  on 6393  degrees of freedom
## AIC: 3015.4
## 
## Number of Fisher Scoring iterations: 18
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_0-5.png) 

```
##    threshold   f.score
## 1        0.0 0.2866885
## 2        0.1 0.6546275
## 3        0.2 0.7282651
## 4        0.3 0.7443255
## 5        0.4 0.7414903
## 6        0.5 0.7327121
## 7        0.6 0.7077244
## 8        0.7 0.6603032
## 9        0.8 0.5538847
## 10       0.9 0.3553598
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Final.glm.N
## 1            N                             5066
## 2            Y                              224
##   Popular.fctr.predict.Final.glm.Y
## 1                              373
## 2                              869
##          Prediction
## Reference    N    Y
##         N 5066  373
##         Y  224  869
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.086038e-01   6.889575e-01   9.013552e-01   9.154852e-01   8.326699e-01 
## AccuracyPValue  McnemarPValue 
##   2.440618e-70   1.384797e-09
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_0-6.png) 

```
##    model_id model_method
## 1 Final.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## 1 myCategory.fctr, WordCount.log, H.npnct21.log, PubDate.hour.fctr, A.make.log, A.fashion.log, H.npnct30.log, A.report.log, A.articl.log, A.npnct04.log, H.X2015.log, A.share.log, H.today.log, H.new.log, H.has.year.colon, A.said.log, H.ndgts.log, PubDate.last10.log, H.npnct04.log, H.npnct08.log, A.newyork.log, S.npnct15.log, A.npnct15.log, PubDate.wkday.fctr, .rnorm, A.week.log, H.npnct13.log, A.show.log, A.intern.log, S.time.log, H.npnct07.log, A.npnct18.log, H.npnct06.log, S.state.log, PubDate.minute.fctr, A.npnct19.log, A.can.log, PubDate.date.fctr, S.npnct12.log, A.npnct12.log, H.nuppr.log, H.week.log, A.state.log, S.npnct13.log, A.npnct13.log, A.presid.log, S.npnct16.log, S.npnct08.log, S.can.log, A.npnct17.log, A.will.log, S.ndgts.log, S.compani.log, H.npnct05.log, A.npnct21.log, S.nuppr.log, H.nchrs.log, A.year.log, A.npnct01.log, S.will.log, A.ndgts.log, PubDate.last100.log, A.npnct22.log, A.nuppr.log, S.day.log, S.one.log, A.one.log, PubDate.second.fctr, H.X2014.log, H.report.log, S.take.log, S.nwrds.unq.log, S.npnct09.log, A.nwrds.unq.log, H.npnct12.log, H.fashion.log, S.npnct21.log, S.new.log, H.npnct15.log, A.new.log, A.nchrs.log, S.nchrs.log, S.npnct30.log, H.npnct17.log, A.npnct06.log, A.npnct30.log, H.nwrds.unq.log, S.nwrds.log, A.npnct07.log, H.npnct01.log, A.nwrds.log, H.has.ebola, A.npnct14.log, S.npnct14.log, PubDate.last1.log, A.npnct02.log, H.day.log, A.first.log, S.npnct03.log, H.newyork.log, A.has.year.colon, PubDate.wkend, H.npnct16.log, H.npnct14.log, H.nwrds.log, H.npnct02.log, H.daili.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     11.678                 5.617
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9463101                    0.3       0.7443255        0.9067654
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit min.aic.fit
## 1             0.9013552             0.9154852     0.6451275    3015.365
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01061015      0.04606138
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13 fit.data.training          7          0 458.165 476.847  18.682
## 14 fit.data.training          7          1 476.848      NA      NA
```


```r
glb_trnent_df <- glb_get_predictions(df=glb_trnent_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(df = glb_trnent_df, mdl_id =
## glb_fin_mdl_id, : Using default probability threshold: 0.9
```

```r
glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
                                               entity_df=glb_trnent_df)
glb_feats_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_feats_df$importance
print(glb_feats_df)
```

```
##                                      id   importance         cor.y
## myCategory.fctr         myCategory.fctr 100.00000000  1.234541e-02
## WordCount.log             WordCount.log  70.43241920  2.656836e-01
## H.npnct21.log             H.npnct21.log  32.71499686  1.283641e-01
## PubDate.hour.fctr     PubDate.hour.fctr  28.49497858  1.354368e-01
## A.make.log                   A.make.log  24.82458430  2.334962e-02
## A.fashion.log             A.fashion.log  22.86799201 -8.724932e-02
## H.npnct30.log             H.npnct30.log  18.66889549 -8.917338e-02
## A.report.log               A.report.log  17.94396169 -5.032801e-02
## A.articl.log               A.articl.log  17.31584878 -5.952055e-02
## A.npnct04.log             A.npnct04.log  16.42726704 -6.294642e-02
## H.X2015.log                 H.X2015.log  16.18781324 -6.658489e-02
## A.share.log                 A.share.log  16.05772852 -5.138139e-02
## H.today.log                 H.today.log  15.09058570 -6.372306e-02
## H.new.log                     H.new.log  14.58884852 -5.313316e-02
## H.has.year.colon       H.has.year.colon  14.48643170 -7.842875e-02
## A.said.log                   A.said.log  13.53465142  3.735051e-04
## H.ndgts.log                 H.ndgts.log  13.52245535 -1.196633e-01
## PubDate.last10.log   PubDate.last10.log  12.97780472  4.931702e-02
## H.npnct04.log             H.npnct04.log  12.76927377 -5.126277e-02
## H.npnct08.log             H.npnct08.log  12.06593521  5.375262e-02
## A.newyork.log             A.newyork.log  11.59720267 -6.219997e-02
## S.npnct15.log             S.npnct15.log  10.97856358 -2.121844e-02
## A.npnct15.log             A.npnct15.log  10.79302566 -2.407715e-02
## PubDate.wkday.fctr   PubDate.wkday.fctr  10.68599632 -3.980129e-02
## .rnorm                           .rnorm  10.55274877 -8.244230e-03
## A.week.log                   A.week.log   9.81958866 -8.840293e-02
## H.npnct13.log             H.npnct13.log   9.71218057 -1.305305e-02
## A.show.log                   A.show.log   9.68407375 -4.897915e-02
## A.intern.log               A.intern.log   9.66632050 -6.864274e-02
## S.time.log                   S.time.log   9.55323497 -5.759227e-02
## H.npnct07.log             H.npnct07.log   9.53685753 -1.201741e-02
## A.npnct18.log             A.npnct18.log   9.41984800 -1.451467e-02
## H.npnct06.log             H.npnct06.log   9.23627876  3.190718e-02
## S.state.log                 S.state.log   8.92098916  7.050791e-03
## PubDate.minute.fctr PubDate.minute.fctr   8.53101958 -3.407385e-02
## A.npnct19.log             A.npnct19.log   8.47715771 -1.271661e-02
## A.can.log                     A.can.log   8.10387656  3.169296e-02
## PubDate.date.fctr     PubDate.date.fctr   8.03583604 -1.164756e-02
## S.npnct12.log             S.npnct12.log   8.03465413 -9.158156e-02
## A.npnct12.log             A.npnct12.log   7.98621020 -9.183870e-02
## H.nuppr.log                 H.nuppr.log   7.85639322 -1.278085e-01
## H.week.log                   H.week.log   7.79627328 -7.510522e-02
## A.state.log                 A.state.log   7.26321159  6.668101e-03
## S.npnct13.log             S.npnct13.log   7.17910453 -3.638891e-02
## A.npnct13.log             A.npnct13.log   7.17784272 -3.760012e-02
## A.presid.log               A.presid.log   7.16305732 -2.014404e-02
## S.npnct16.log             S.npnct16.log   7.14944805 -6.770952e-02
## S.npnct08.log             S.npnct08.log   7.07080676 -2.413868e-03
## S.can.log                     S.can.log   7.02341628  3.077833e-02
## A.npnct17.log             A.npnct17.log   6.89744623 -1.587454e-03
## A.will.log                   A.will.log   6.81698075 -6.147068e-02
## S.ndgts.log                 S.ndgts.log   6.75768296 -1.242046e-01
## S.compani.log             S.compani.log   6.68327086 -5.261812e-02
## H.npnct05.log             H.npnct05.log   6.59752354 -9.653967e-03
## A.npnct21.log             A.npnct21.log   6.53946049  5.482747e-02
## S.nuppr.log                 S.nuppr.log   6.48716764 -2.718459e-01
## H.nchrs.log                 H.nchrs.log   6.36565681 -1.710624e-01
## A.year.log                   A.year.log   6.30335271 -5.094457e-02
## A.npnct01.log             A.npnct01.log   6.26548239  3.093101e-02
## S.will.log                   S.will.log   6.21988118 -6.103349e-02
## A.ndgts.log                 A.ndgts.log   6.12230312 -1.249484e-01
## PubDate.last100.log PubDate.last100.log   6.10602730 -7.663322e-03
## A.npnct22.log             A.npnct22.log   5.81913883 -1.923169e-02
## A.nuppr.log                 A.nuppr.log   5.73056332 -2.720962e-01
## S.day.log                     S.day.log   5.55760002 -4.555421e-02
## S.one.log                     S.one.log   5.29435441  4.891059e-03
## A.one.log                     A.one.log   5.19360228  4.368856e-03
## PubDate.second.fctr PubDate.second.fctr   4.57971207 -1.187946e-02
## H.X2014.log                 H.X2014.log   4.46472089 -4.620638e-02
## H.report.log               H.report.log   4.40029310 -6.494810e-02
## S.take.log                   S.take.log   4.26732531 -2.569295e-02
## S.nwrds.unq.log         S.nwrds.unq.log   4.13474268 -2.507969e-01
## S.npnct09.log             S.npnct09.log   4.11245488 -3.986882e-03
## A.nwrds.unq.log         A.nwrds.unq.log   3.95688107 -2.506012e-01
## H.npnct12.log             H.npnct12.log   3.76801998  1.333613e-02
## H.fashion.log             H.fashion.log   3.76390763 -8.204998e-02
## S.npnct21.log             S.npnct21.log   3.66026012  5.503894e-02
## S.new.log                     S.new.log   3.52281315 -3.483189e-02
## H.npnct15.log             H.npnct15.log   3.45385040 -6.158577e-02
## A.new.log                     A.new.log   3.41102464 -3.524871e-02
## A.nchrs.log                 A.nchrs.log   3.12081764 -2.245488e-01
## S.nchrs.log                 S.nchrs.log   2.97802811 -2.246930e-01
## S.npnct30.log             S.npnct30.log   2.88153488 -4.370037e-02
## H.npnct17.log             H.npnct17.log   2.24624596  3.039622e-02
## A.npnct06.log             A.npnct06.log   2.18674221 -2.389145e-02
## A.npnct30.log             A.npnct30.log   2.11807078 -4.373349e-02
## H.nwrds.unq.log         H.nwrds.unq.log   2.09016785 -2.044964e-01
## S.nwrds.log                 S.nwrds.log   2.07125498 -2.453541e-01
## A.npnct07.log             A.npnct07.log   2.04536000 -1.214357e-02
## H.npnct01.log             H.npnct01.log   2.03495324  2.271577e-02
## A.nwrds.log                 A.nwrds.log   1.93513242 -2.450733e-01
## H.has.ebola                 H.has.ebola   1.75502087  2.588140e-02
## A.npnct14.log             A.npnct14.log   1.53950062 -4.999563e-02
## S.npnct14.log             S.npnct14.log   1.28524340 -5.332519e-02
## PubDate.last1.log     PubDate.last1.log   1.18736302  4.635751e-02
## A.npnct02.log             A.npnct02.log   1.14672657 -1.451467e-02
## H.day.log                     H.day.log   1.12673197 -6.272898e-02
## A.first.log                 A.first.log   1.11773697 -5.345938e-02
## S.npnct03.log             S.npnct03.log   1.07345568 -1.240734e-02
## H.newyork.log             H.newyork.log   1.03389528 -5.797009e-02
## A.has.year.colon       A.has.year.colon   0.89413765 -1.755336e-02
## PubDate.wkend             PubDate.wkend   0.64555965  1.067288e-01
## H.npnct16.log             H.npnct16.log   0.59350225 -8.273237e-02
## H.npnct14.log             H.npnct14.log   0.52235605 -2.524770e-02
## H.nwrds.log                 H.nwrds.log   0.46021386 -2.006864e-01
## H.npnct02.log             H.npnct02.log   0.04154952 -2.001851e-02
## H.daili.log                 H.daili.log   0.00000000 -6.919298e-02
## A.compani.log             A.compani.log           NA -5.268413e-02
## A.day.log                     A.day.log           NA -4.581783e-02
## A.has.http                   A.has.http           NA -1.359260e-02
## A.npnct03.log             A.npnct03.log           NA -1.359260e-02
## A.npnct05.log             A.npnct05.log           NA            NA
## A.npnct08.log             A.npnct08.log           NA -3.258100e-03
## A.npnct09.log             A.npnct09.log           NA -4.775988e-03
## A.npnct10.log             A.npnct10.log           NA            NA
## A.npnct11.log             A.npnct11.log           NA -5.547032e-03
## A.npnct16.log             A.npnct16.log           NA -6.893301e-02
## A.npnct20.log             A.npnct20.log           NA -1.451467e-02
## A.npnct23.log             A.npnct23.log           NA  1.537569e-02
## A.npnct24.log             A.npnct24.log           NA            NA
## A.npnct25.log             A.npnct25.log           NA  1.537569e-02
## A.npnct26.log             A.npnct26.log           NA -9.890046e-19
## A.npnct27.log             A.npnct27.log           NA -5.547032e-03
## A.npnct28.log             A.npnct28.log           NA            NA
## A.npnct29.log             A.npnct29.log           NA            NA
## A.npnct31.log             A.npnct31.log           NA            NA
## A.npnct32.log             A.npnct32.log           NA            NA
## A.take.log                   A.take.log           NA -2.601772e-02
## A.time.log                   A.time.log           NA -5.779371e-02
## H.has.http                   H.has.http           NA            NA
## H.npnct03.log             H.npnct03.log           NA  9.533020e-03
## H.npnct09.log             H.npnct09.log           NA  5.375262e-02
## H.npnct10.log             H.npnct10.log           NA            NA
## H.npnct11.log             H.npnct11.log           NA -5.547032e-03
## H.npnct18.log             H.npnct18.log           NA            NA
## H.npnct19.log             H.npnct19.log           NA            NA
## H.npnct20.log             H.npnct20.log           NA            NA
## H.npnct22.log             H.npnct22.log           NA -5.547032e-03
## H.npnct23.log             H.npnct23.log           NA            NA
## H.npnct24.log             H.npnct24.log           NA            NA
## H.npnct25.log             H.npnct25.log           NA            NA
## H.npnct26.log             H.npnct26.log           NA -9.890046e-19
## H.npnct27.log             H.npnct27.log           NA            NA
## H.npnct28.log             H.npnct28.log           NA            NA
## H.npnct29.log             H.npnct29.log           NA            NA
## H.npnct31.log             H.npnct31.log           NA            NA
## H.npnct32.log             H.npnct32.log           NA            NA
## Popular                         Popular           NA  1.000000e+00
## Popular.fctr               Popular.fctr           NA            NA
## PubDate.last1             PubDate.last1           NA  3.592267e-02
## PubDate.last10           PubDate.last10           NA  5.398093e-02
## PubDate.last100         PubDate.last100           NA  3.989229e-02
## PubDate.month.fctr   PubDate.month.fctr           NA  1.914874e-02
## PubDate.POSIX             PubDate.POSIX           NA  1.568326e-02
## PubDate.year.fctr     PubDate.year.fctr           NA            NA
## PubDate.zoo                 PubDate.zoo           NA  1.568326e-02
## S.articl.log               S.articl.log           NA -5.952055e-02
## S.fashion.log             S.fashion.log           NA -8.724932e-02
## S.first.log                 S.first.log           NA -5.345938e-02
## S.has.http                   S.has.http           NA            NA
## S.has.year.colon       S.has.year.colon           NA -1.755336e-02
## S.intern.log               S.intern.log           NA -6.864274e-02
## S.make.log                   S.make.log           NA  2.334962e-02
## S.newyork.log             S.newyork.log           NA -6.219997e-02
## S.npnct01.log             S.npnct01.log           NA  3.093101e-02
## S.npnct02.log             S.npnct02.log           NA -5.547032e-03
## S.npnct04.log             S.npnct04.log           NA -6.294642e-02
## S.npnct05.log             S.npnct05.log           NA            NA
## S.npnct06.log             S.npnct06.log           NA -2.389145e-02
## S.npnct07.log             S.npnct07.log           NA -1.214357e-02
## S.npnct10.log             S.npnct10.log           NA            NA
## S.npnct11.log             S.npnct11.log           NA -5.547032e-03
## S.npnct17.log             S.npnct17.log           NA -1.587454e-03
## S.npnct18.log             S.npnct18.log           NA            NA
## S.npnct19.log             S.npnct19.log           NA            NA
## S.npnct20.log             S.npnct20.log           NA            NA
## S.npnct22.log             S.npnct22.log           NA -1.923169e-02
## S.npnct23.log             S.npnct23.log           NA  2.760321e-02
## S.npnct24.log             S.npnct24.log           NA            NA
## S.npnct25.log             S.npnct25.log           NA  2.760321e-02
## S.npnct26.log             S.npnct26.log           NA -9.890046e-19
## S.npnct27.log             S.npnct27.log           NA            NA
## S.npnct28.log             S.npnct28.log           NA            NA
## S.npnct29.log             S.npnct29.log           NA            NA
## S.npnct31.log             S.npnct31.log           NA            NA
## S.npnct32.log             S.npnct32.log           NA            NA
## S.presid.log               S.presid.log           NA -2.014404e-02
## S.report.log               S.report.log           NA -5.032801e-02
## S.said.log                   S.said.log           NA  3.735051e-04
## S.share.log                 S.share.log           NA -5.138139e-02
## S.show.log                   S.show.log           NA -4.897915e-02
## S.week.log                   S.week.log           NA -8.840293e-02
## S.year.log                   S.year.log           NA -5.094457e-02
## UniqueID                       UniqueID           NA  1.182492e-02
## WordCount                     WordCount           NA  2.575265e-01
##                     exclude.as.feat    cor.y.abs       cor.high.X
## myCategory.fctr               FALSE 1.234541e-02             <NA>
## WordCount.log                 FALSE 2.656836e-01             <NA>
## H.npnct21.log                 FALSE 1.283641e-01             <NA>
## PubDate.hour.fctr             FALSE 1.354368e-01             <NA>
## A.make.log                    FALSE 2.334962e-02       S.make.log
## A.fashion.log                 FALSE 8.724932e-02    S.fashion.log
## H.npnct30.log                 FALSE 8.917338e-02             <NA>
## A.report.log                  FALSE 5.032801e-02     S.report.log
## A.articl.log                  FALSE 5.952055e-02     S.articl.log
## A.npnct04.log                 FALSE 6.294642e-02    S.npnct04.log
## H.X2015.log                   FALSE 6.658489e-02    H.npnct15.log
## A.share.log                   FALSE 5.138139e-02      S.share.log
## H.today.log                   FALSE 6.372306e-02             <NA>
## H.new.log                     FALSE 5.313316e-02             <NA>
## H.has.year.colon              FALSE 7.842875e-02     A.intern.log
## A.said.log                    FALSE 3.735051e-04             <NA>
## H.ndgts.log                   FALSE 1.196633e-01             <NA>
## PubDate.last10.log            FALSE 4.931702e-02             <NA>
## H.npnct04.log                 FALSE 5.126277e-02             <NA>
## H.npnct08.log                 FALSE 5.375262e-02    H.npnct09.log
## A.newyork.log                 FALSE 6.219997e-02    S.newyork.log
## S.npnct15.log                 FALSE 2.121844e-02             <NA>
## A.npnct15.log                 FALSE 2.407715e-02    A.npnct02.log
## PubDate.wkday.fctr            FALSE 3.980129e-02             <NA>
## .rnorm                        FALSE 8.244230e-03             <NA>
## A.week.log                    FALSE 8.840293e-02       S.week.log
## H.npnct13.log                 FALSE 1.305305e-02             <NA>
## A.show.log                    FALSE 4.897915e-02       S.show.log
## A.intern.log                  FALSE 6.864274e-02     S.intern.log
## S.time.log                    FALSE 5.759227e-02             <NA>
## H.npnct07.log                 FALSE 1.201741e-02             <NA>
## A.npnct18.log                 FALSE 1.451467e-02    A.npnct20.log
## H.npnct06.log                 FALSE 3.190718e-02    H.npnct17.log
## S.state.log                   FALSE 7.050791e-03             <NA>
## PubDate.minute.fctr           FALSE 3.407385e-02             <NA>
## A.npnct19.log                 FALSE 1.271661e-02             <NA>
## A.can.log                     FALSE 3.169296e-02        S.can.log
## PubDate.date.fctr             FALSE 1.164756e-02             <NA>
## S.npnct12.log                 FALSE 9.158156e-02             <NA>
## A.npnct12.log                 FALSE 9.183870e-02    S.npnct12.log
## H.nuppr.log                   FALSE 1.278085e-01             <NA>
## H.week.log                    FALSE 7.510522e-02             <NA>
## A.state.log                   FALSE 6.668101e-03             <NA>
## S.npnct13.log                 FALSE 3.638891e-02             <NA>
## A.npnct13.log                 FALSE 3.760012e-02    S.npnct13.log
## A.presid.log                  FALSE 2.014404e-02     S.presid.log
## S.npnct16.log                 FALSE 6.770952e-02             <NA>
## S.npnct08.log                 FALSE 2.413868e-03             <NA>
## S.can.log                     FALSE 3.077833e-02             <NA>
## A.npnct17.log                 FALSE 1.587454e-03             <NA>
## A.will.log                    FALSE 6.147068e-02       S.will.log
## S.ndgts.log                   FALSE 1.242046e-01             <NA>
## S.compani.log                 FALSE 5.261812e-02             <NA>
## H.npnct05.log                 FALSE 9.653967e-03             <NA>
## A.npnct21.log                 FALSE 5.482747e-02             <NA>
## S.nuppr.log                   FALSE 2.718459e-01             <NA>
## H.nchrs.log                   FALSE 1.710624e-01             <NA>
## A.year.log                    FALSE 5.094457e-02       S.year.log
## A.npnct01.log                 FALSE 3.093101e-02    S.npnct01.log
## S.will.log                    FALSE 6.103349e-02             <NA>
## A.ndgts.log                   FALSE 1.249484e-01      S.ndgts.log
## PubDate.last100.log           FALSE 7.663322e-03             <NA>
## A.npnct22.log                 FALSE 1.923169e-02    S.npnct22.log
## A.nuppr.log                   FALSE 2.720962e-01      S.nuppr.log
## S.day.log                     FALSE 4.555421e-02             <NA>
## S.one.log                     FALSE 4.891059e-03             <NA>
## A.one.log                     FALSE 4.368856e-03             <NA>
## PubDate.second.fctr           FALSE 1.187946e-02             <NA>
## H.X2014.log                   FALSE 4.620638e-02             <NA>
## H.report.log                  FALSE 6.494810e-02             <NA>
## S.take.log                    FALSE 2.569295e-02             <NA>
## S.nwrds.unq.log               FALSE 2.507969e-01      S.nchrs.log
## S.npnct09.log                 FALSE 3.986882e-03             <NA>
## A.nwrds.unq.log               FALSE 2.506012e-01             <NA>
## H.npnct12.log                 FALSE 1.333613e-02             <NA>
## H.fashion.log                 FALSE 8.204998e-02       H.week.log
## S.npnct21.log                 FALSE 5.503894e-02    A.npnct21.log
## S.new.log                     FALSE 3.483189e-02             <NA>
## H.npnct15.log                 FALSE 6.158577e-02             <NA>
## A.new.log                     FALSE 3.524871e-02        S.new.log
## A.nchrs.log                   FALSE 2.245488e-01             <NA>
## S.nchrs.log                   FALSE 2.246930e-01      A.nchrs.log
## S.npnct30.log                 FALSE 4.370037e-02             <NA>
## H.npnct17.log                 FALSE 3.039622e-02             <NA>
## A.npnct06.log                 FALSE 2.389145e-02    S.npnct06.log
## A.npnct30.log                 FALSE 4.373349e-02    S.npnct30.log
## H.nwrds.unq.log               FALSE 2.044964e-01      H.nuppr.log
## S.nwrds.log                   FALSE 2.453541e-01      A.nwrds.log
## A.npnct07.log                 FALSE 1.214357e-02    S.npnct07.log
## H.npnct01.log                 FALSE 2.271577e-02             <NA>
## A.nwrds.log                   FALSE 2.450733e-01             <NA>
## H.has.ebola                   FALSE 2.588140e-02             <NA>
## A.npnct14.log                 FALSE 4.999563e-02             <NA>
## S.npnct14.log                 FALSE 5.332519e-02    A.npnct14.log
## PubDate.last1.log             FALSE 4.635751e-02             <NA>
## A.npnct02.log                 FALSE 1.451467e-02    A.npnct18.log
## H.day.log                     FALSE 6.272898e-02             <NA>
## A.first.log                   FALSE 5.345938e-02      S.first.log
## S.npnct03.log                 FALSE 1.240734e-02             <NA>
## H.newyork.log                 FALSE 5.797009e-02             <NA>
## A.has.year.colon              FALSE 1.755336e-02 S.has.year.colon
## PubDate.wkend                 FALSE 1.067288e-01             <NA>
## H.npnct16.log                 FALSE 8.273237e-02             <NA>
## H.npnct14.log                 FALSE 2.524770e-02             <NA>
## H.nwrds.log                   FALSE 2.006864e-01             <NA>
## H.npnct02.log                 FALSE 2.001851e-02             <NA>
## H.daili.log                   FALSE 6.919298e-02             <NA>
## A.compani.log                 FALSE 5.268413e-02    S.compani.log
## A.day.log                     FALSE 4.581783e-02        S.day.log
## A.has.http                    FALSE 1.359260e-02    A.npnct19.log
## A.npnct03.log                 FALSE 1.359260e-02    S.npnct03.log
## A.npnct05.log                 FALSE           NA             <NA>
## A.npnct08.log                 FALSE 3.258100e-03             <NA>
## A.npnct09.log                 FALSE 4.775988e-03             <NA>
## A.npnct10.log                 FALSE           NA             <NA>
## A.npnct11.log                 FALSE 5.547032e-03             <NA>
## A.npnct16.log                 FALSE 6.893301e-02    S.npnct16.log
## A.npnct20.log                 FALSE 1.451467e-02             <NA>
## A.npnct23.log                 FALSE 1.537569e-02    A.npnct25.log
## A.npnct24.log                 FALSE           NA             <NA>
## A.npnct25.log                 FALSE 1.537569e-02             <NA>
## A.npnct26.log                 FALSE 9.890046e-19             <NA>
## A.npnct27.log                 FALSE 5.547032e-03             <NA>
## A.npnct28.log                 FALSE           NA             <NA>
## A.npnct29.log                 FALSE           NA             <NA>
## A.npnct31.log                 FALSE           NA             <NA>
## A.npnct32.log                 FALSE           NA             <NA>
## A.take.log                    FALSE 2.601772e-02       S.take.log
## A.time.log                    FALSE 5.779371e-02       S.time.log
## H.has.http                    FALSE           NA             <NA>
## H.npnct03.log                 FALSE 9.533020e-03             <NA>
## H.npnct09.log                 FALSE 5.375262e-02             <NA>
## H.npnct10.log                 FALSE           NA             <NA>
## H.npnct11.log                 FALSE 5.547032e-03             <NA>
## H.npnct18.log                 FALSE           NA             <NA>
## H.npnct19.log                 FALSE           NA             <NA>
## H.npnct20.log                 FALSE           NA             <NA>
## H.npnct22.log                 FALSE 5.547032e-03             <NA>
## H.npnct23.log                 FALSE           NA             <NA>
## H.npnct24.log                 FALSE           NA             <NA>
## H.npnct25.log                 FALSE           NA             <NA>
## H.npnct26.log                 FALSE 9.890046e-19             <NA>
## H.npnct27.log                 FALSE           NA             <NA>
## H.npnct28.log                 FALSE           NA             <NA>
## H.npnct29.log                 FALSE           NA             <NA>
## H.npnct31.log                 FALSE           NA             <NA>
## H.npnct32.log                 FALSE           NA             <NA>
## Popular                        TRUE 1.000000e+00             <NA>
## Popular.fctr                   TRUE           NA             <NA>
## PubDate.last1                  TRUE 3.592267e-02             <NA>
## PubDate.last10                 TRUE 5.398093e-02             <NA>
## PubDate.last100                TRUE 3.989229e-02             <NA>
## PubDate.month.fctr             TRUE 1.914874e-02             <NA>
## PubDate.POSIX                  TRUE 1.568326e-02             <NA>
## PubDate.year.fctr             FALSE           NA             <NA>
## PubDate.zoo                    TRUE 1.568326e-02             <NA>
## S.articl.log                  FALSE 5.952055e-02             <NA>
## S.fashion.log                 FALSE 8.724932e-02             <NA>
## S.first.log                   FALSE 5.345938e-02             <NA>
## S.has.http                    FALSE           NA             <NA>
## S.has.year.colon              FALSE 1.755336e-02             <NA>
## S.intern.log                  FALSE 6.864274e-02             <NA>
## S.make.log                    FALSE 2.334962e-02             <NA>
## S.newyork.log                 FALSE 6.219997e-02             <NA>
## S.npnct01.log                 FALSE 3.093101e-02             <NA>
## S.npnct02.log                 FALSE 5.547032e-03             <NA>
## S.npnct04.log                 FALSE 6.294642e-02             <NA>
## S.npnct05.log                 FALSE           NA             <NA>
## S.npnct06.log                 FALSE 2.389145e-02             <NA>
## S.npnct07.log                 FALSE 1.214357e-02             <NA>
## S.npnct10.log                 FALSE           NA             <NA>
## S.npnct11.log                 FALSE 5.547032e-03             <NA>
## S.npnct17.log                 FALSE 1.587454e-03             <NA>
## S.npnct18.log                 FALSE           NA             <NA>
## S.npnct19.log                 FALSE           NA             <NA>
## S.npnct20.log                 FALSE           NA             <NA>
## S.npnct22.log                 FALSE 1.923169e-02             <NA>
## S.npnct23.log                 FALSE 2.760321e-02    A.npnct23.log
## S.npnct24.log                 FALSE           NA             <NA>
## S.npnct25.log                 FALSE 2.760321e-02             <NA>
## S.npnct26.log                 FALSE 9.890046e-19             <NA>
## S.npnct27.log                 FALSE           NA             <NA>
## S.npnct28.log                 FALSE           NA             <NA>
## S.npnct29.log                 FALSE           NA             <NA>
## S.npnct31.log                 FALSE           NA             <NA>
## S.npnct32.log                 FALSE           NA             <NA>
## S.presid.log                  FALSE 2.014404e-02             <NA>
## S.report.log                  FALSE 5.032801e-02             <NA>
## S.said.log                    FALSE 3.735051e-04             <NA>
## S.share.log                   FALSE 5.138139e-02             <NA>
## S.show.log                    FALSE 4.897915e-02             <NA>
## S.week.log                    FALSE 8.840293e-02             <NA>
## S.year.log                    FALSE 5.094457e-02             <NA>
## UniqueID                       TRUE 1.182492e-02             <NA>
## WordCount                      TRUE 2.575265e-01             <NA>
##                       freqRatio percentUnique zeroVar   nzv myNearZV
## myCategory.fctr        1.337185    0.30618494   FALSE FALSE    FALSE
## WordCount.log          1.300000   24.14268218   FALSE FALSE    FALSE
## H.npnct21.log         14.995098    0.06123699   FALSE FALSE    FALSE
## PubDate.hour.fctr      1.835040    0.04592774   FALSE FALSE    FALSE
## A.make.log            27.378261    0.04592774   FALSE  TRUE    FALSE
## A.fashion.log         25.737705    0.04592774   FALSE  TRUE    FALSE
## H.npnct30.log         24.123077    0.03061849   FALSE  TRUE    FALSE
## A.report.log          24.204633    0.06123699   FALSE  TRUE    FALSE
## A.articl.log          30.863415    0.03061849   FALSE  TRUE    FALSE
## A.npnct04.log         28.536364    0.07654623   FALSE  TRUE    FALSE
## H.X2015.log           45.326241    0.03061849   FALSE  TRUE    FALSE
## A.share.log           32.654639    0.04592774   FALSE  TRUE    FALSE
## H.today.log           36.757225    0.03061849   FALSE  TRUE    FALSE
## H.new.log             25.228916    0.04592774   FALSE  TRUE    FALSE
## H.has.year.colon      32.670103    0.03061849   FALSE  TRUE    FALSE
## A.said.log            25.212851    0.04592774   FALSE  TRUE    FALSE
## H.ndgts.log           13.616137    0.18371096   FALSE FALSE    FALSE
## PubDate.last10.log     1.666667   79.05695040   FALSE FALSE    FALSE
## H.npnct04.log         38.325301    0.04592774   FALSE  TRUE    FALSE
## H.npnct08.log        111.620690    0.03061849   FALSE  TRUE    FALSE
## A.newyork.log         15.153465    0.06123699   FALSE FALSE    FALSE
## S.npnct15.log        203.062500    0.04592774   FALSE  TRUE    FALSE
## A.npnct15.log        196.696970    0.10716473   FALSE  TRUE    FALSE
## PubDate.wkday.fctr     1.003268    0.10716473   FALSE FALSE    FALSE
## .rnorm                 2.000000   99.98469075   FALSE FALSE    FALSE
## A.week.log            13.278509    0.04592774   FALSE FALSE    FALSE
## H.npnct13.log         13.126638    0.09185548   FALSE FALSE    FALSE
## A.show.log            30.512077    0.06123699   FALSE  TRUE    FALSE
## A.intern.log          29.801887    0.04592774   FALSE  TRUE    FALSE
## S.time.log            13.483296    0.04592774   FALSE FALSE    FALSE
## H.npnct07.log          5.437234    0.12247397   FALSE FALSE    FALSE
## A.npnct18.log       1087.500000    0.04592774   FALSE  TRUE    FALSE
## H.npnct06.log         68.935484    0.06123699   FALSE  TRUE    FALSE
## S.state.log           30.655340    0.04592774   FALSE  TRUE    FALSE
## PubDate.minute.fctr    1.483365    0.06123699   FALSE FALSE    FALSE
## A.npnct19.log       1631.500000    0.06123699   FALSE  TRUE    FALSE
## A.can.log             26.166667    0.04592774   FALSE  TRUE    FALSE
## PubDate.date.fctr      1.021394    0.07654623   FALSE FALSE    FALSE
## S.npnct12.log          1.660473    0.13778322   FALSE FALSE    FALSE
## A.npnct12.log          1.660473    0.13778322   FALSE FALSE    FALSE
## H.nuppr.log            1.033930    0.29087569   FALSE FALSE    FALSE
## H.week.log            24.818182    0.03061849   FALSE  TRUE    FALSE
## A.state.log           30.502415    0.04592774   FALSE  TRUE    FALSE
## S.npnct13.log          5.706263    0.09185548   FALSE FALSE    FALSE
## A.npnct13.log          5.715368    0.12247397   FALSE FALSE    FALSE
## A.presid.log          26.854701    0.06123699   FALSE  TRUE    FALSE
## S.npnct16.log         13.647191    0.04592774   FALSE FALSE    FALSE
## S.npnct08.log        175.513514    0.04592774   FALSE  TRUE    FALSE
## S.can.log             26.058091    0.04592774   FALSE  TRUE    FALSE
## A.npnct17.log        434.133333    0.04592774   FALSE  TRUE    FALSE
## A.will.log            11.212406    0.06123699   FALSE FALSE    FALSE
## S.ndgts.log           10.511247    0.26025720   FALSE FALSE    FALSE
## S.compani.log         18.093842    0.04592774   FALSE FALSE    FALSE
## H.npnct05.log        543.333333    0.03061849   FALSE  TRUE    FALSE
## A.npnct21.log         12.798715    0.07654623   FALSE FALSE    FALSE
## S.nuppr.log            1.152620    0.33680343   FALSE FALSE    FALSE
## H.nchrs.log            1.023810    1.57685242   FALSE FALSE    FALSE
## A.year.log            18.456716    0.06123699   FALSE FALSE    FALSE
## A.npnct01.log        309.952381    0.06123699   FALSE  TRUE    FALSE
## S.will.log            11.237288    0.06123699   FALSE FALSE    FALSE
## A.ndgts.log           10.501022    0.29087569   FALSE FALSE    FALSE
## PubDate.last100.log   25.000000   92.19228414   FALSE FALSE    FALSE
## A.npnct22.log        543.333333    0.03061849   FALSE  TRUE    FALSE
## A.nuppr.log            1.151308    0.33680343   FALSE FALSE    FALSE
## S.day.log             24.692913    0.04592774   FALSE  TRUE    FALSE
## S.one.log             22.777372    0.04592774   FALSE  TRUE    FALSE
## A.one.log             22.773723    0.04592774   FALSE  TRUE    FALSE
## PubDate.second.fctr    1.018204    0.06123699   FALSE FALSE    FALSE
## H.X2014.log           63.673267    0.03061849   FALSE  TRUE    FALSE
## H.report.log          30.403846    0.03061849   FALSE  TRUE    FALSE
## S.take.log            29.376744    0.04592774   FALSE  TRUE    FALSE
## S.nwrds.unq.log        1.061567    0.44396816   FALSE FALSE    FALSE
## S.npnct09.log        175.486486    0.06123699   FALSE  TRUE    FALSE
## A.nwrds.unq.log        1.061567    0.55113288   FALSE FALSE    FALSE
## H.npnct12.log          4.937442    0.07654623   FALSE FALSE    FALSE
## H.fashion.log         28.542986    0.04592774   FALSE  TRUE    FALSE
## S.npnct21.log         12.862366    0.07654623   FALSE FALSE    FALSE
## S.new.log             10.124573    0.04592774   FALSE FALSE    FALSE
## H.npnct15.log         52.983471    0.03061849   FALSE  TRUE    FALSE
## A.new.log             10.086735    0.04592774   FALSE FALSE    FALSE
## A.nchrs.log            1.328571    4.39375383   FALSE FALSE    FALSE
## S.nchrs.log            1.328571    3.72014697   FALSE FALSE    FALSE
## S.npnct30.log        134.791667    0.04592774   FALSE  TRUE    FALSE
## H.npnct17.log         96.104478    0.06123699   FALSE  TRUE    FALSE
## A.npnct06.log        115.642857    0.03061849   FALSE  TRUE    FALSE
## A.npnct30.log        126.862745    0.04592774   FALSE  TRUE    FALSE
## H.nwrds.unq.log        1.019071    0.21432945   FALSE FALSE    FALSE
## S.nwrds.log            1.029183    0.45927740   FALSE FALSE    FALSE
## A.npnct07.log       1631.750000    0.04592774   FALSE  TRUE    FALSE
## H.npnct01.log        282.913043    0.04592774   FALSE  TRUE    FALSE
## A.nwrds.log            1.029183    0.59706062   FALSE FALSE    FALSE
## H.has.ebola           73.227273    0.03061849   FALSE  TRUE    FALSE
## A.npnct14.log          4.603330    0.16840171   FALSE FALSE    FALSE
## S.npnct14.log          4.672000    0.16840171   FALSE FALSE    FALSE
## PubDate.last1.log      1.142857   36.49724434   FALSE FALSE    FALSE
## A.npnct02.log       1087.500000    0.04592774   FALSE  TRUE    FALSE
## H.day.log             29.801887    0.04592774   FALSE  TRUE    FALSE
## A.first.log           29.509346    0.04592774   FALSE  TRUE    FALSE
## S.npnct03.log       1305.400000    0.03061849   FALSE  TRUE    FALSE
## H.newyork.log         26.795745    0.03061849   FALSE  TRUE    FALSE
## A.has.year.colon     652.200000    0.03061849   FALSE  TRUE    FALSE
## PubDate.wkend          9.095827    0.03061849   FALSE FALSE    FALSE
## H.npnct16.log          3.914910    0.04592774   FALSE FALSE    FALSE
## H.npnct14.log         22.802326    0.12247397   FALSE  TRUE    FALSE
## H.nwrds.log            1.019119    0.21432945   FALSE FALSE    FALSE
## H.npnct02.log        501.461538    0.03061849   FALSE  TRUE    FALSE
## H.daili.log           41.973684    0.03061849   FALSE  TRUE    FALSE
## A.compani.log         18.147059    0.04592774   FALSE FALSE    FALSE
## A.day.log             24.592157    0.04592774   FALSE  TRUE    FALSE
## A.has.http          1087.666667    0.03061849   FALSE  TRUE    FALSE
## A.npnct03.log       1087.666667    0.03061849   FALSE  TRUE    FALSE
## A.npnct05.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## A.npnct08.log        170.868421    0.04592774   FALSE  TRUE    FALSE
## A.npnct09.log        170.842105    0.06123699   FALSE  TRUE    FALSE
## A.npnct10.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## A.npnct11.log       6531.000000    0.03061849   FALSE  TRUE     TRUE
## A.npnct16.log         13.482222    0.04592774   FALSE FALSE    FALSE
## A.npnct20.log       1087.500000    0.04592774   FALSE  TRUE    FALSE
## A.npnct23.log       3264.500000    0.04592774   FALSE  TRUE     TRUE
## A.npnct24.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## A.npnct25.log       3264.500000    0.04592774   FALSE  TRUE     TRUE
## A.npnct26.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## A.npnct27.log       6531.000000    0.03061849   FALSE  TRUE     TRUE
## A.npnct28.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## A.npnct29.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## A.npnct31.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## A.npnct32.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## A.take.log            29.236111    0.04592774   FALSE  TRUE    FALSE
## A.time.log            13.451111    0.04592774   FALSE FALSE    FALSE
## H.has.http             0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct03.log       2176.333333    0.03061849   FALSE  TRUE     TRUE
## H.npnct09.log        111.620690    0.03061849   FALSE  TRUE    FALSE
## H.npnct10.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct11.log       6531.000000    0.03061849   FALSE  TRUE     TRUE
## H.npnct18.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct19.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct20.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct22.log       6531.000000    0.03061849   FALSE  TRUE     TRUE
## H.npnct23.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct24.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct25.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct26.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct27.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct28.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct29.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct31.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## H.npnct32.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## Popular                4.976212    0.03061849   FALSE FALSE    FALSE
## Popular.fctr                 NA            NA      NA    NA       NA
## PubDate.last1          1.142857   36.49724434   FALSE FALSE    FALSE
## PubDate.last10         1.666667   79.05695040   FALSE FALSE    FALSE
## PubDate.last100       25.000000   92.52908757   FALSE FALSE    FALSE
## PubDate.month.fctr     1.017514    0.04592774   FALSE FALSE    FALSE
## PubDate.POSIX          1.000000   99.86221678   FALSE FALSE    FALSE
## PubDate.year.fctr      0.000000    0.01530925    TRUE  TRUE     TRUE
## PubDate.zoo            1.000000   99.86221678   FALSE FALSE    FALSE
## S.articl.log          30.863415    0.03061849   FALSE  TRUE    FALSE
## S.fashion.log         25.737705    0.04592774   FALSE  TRUE    FALSE
## S.first.log           29.509346    0.04592774   FALSE  TRUE    FALSE
## S.has.http             0.000000    0.01530925    TRUE  TRUE     TRUE
## S.has.year.colon     652.200000    0.03061849   FALSE  TRUE    FALSE
## S.intern.log          29.801887    0.04592774   FALSE  TRUE    FALSE
## S.make.log            27.378261    0.04592774   FALSE  TRUE    FALSE
## S.newyork.log         15.153465    0.06123699   FALSE FALSE    FALSE
## S.npnct01.log        309.952381    0.06123699   FALSE  TRUE    FALSE
## S.npnct02.log       6531.000000    0.03061849   FALSE  TRUE     TRUE
## S.npnct04.log         28.536364    0.07654623   FALSE  TRUE    FALSE
## S.npnct05.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct06.log        115.642857    0.03061849   FALSE  TRUE    FALSE
## S.npnct07.log       1631.750000    0.04592774   FALSE  TRUE    FALSE
## S.npnct10.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct11.log       6531.000000    0.03061849   FALSE  TRUE     TRUE
## S.npnct17.log        434.133333    0.04592774   FALSE  TRUE    FALSE
## S.npnct18.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct19.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct20.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct22.log        543.333333    0.03061849   FALSE  TRUE    FALSE
## S.npnct23.log       6531.000000    0.03061849   FALSE  TRUE     TRUE
## S.npnct24.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct25.log       6531.000000    0.03061849   FALSE  TRUE     TRUE
## S.npnct26.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct27.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct28.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct29.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct31.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.npnct32.log          0.000000    0.01530925    TRUE  TRUE     TRUE
## S.presid.log          26.854701    0.06123699   FALSE  TRUE    FALSE
## S.report.log          24.204633    0.06123699   FALSE  TRUE    FALSE
## S.said.log            25.212851    0.04592774   FALSE  TRUE    FALSE
## S.share.log           32.654639    0.04592774   FALSE  TRUE    FALSE
## S.show.log            30.512077    0.06123699   FALSE  TRUE    FALSE
## S.week.log            13.278509    0.04592774   FALSE FALSE    FALSE
## S.year.log            18.456716    0.06123699   FALSE FALSE    FALSE
## UniqueID               1.000000  100.00000000   FALSE FALSE    FALSE
## WordCount              2.315789   24.15799143   FALSE FALSE    FALSE
##                     is.cor.y.abs.low rsp_var_raw id_var rsp_var
## myCategory.fctr                FALSE       FALSE     NA      NA
## WordCount.log                  FALSE       FALSE     NA      NA
## H.npnct21.log                  FALSE       FALSE     NA      NA
## PubDate.hour.fctr              FALSE       FALSE     NA      NA
## A.make.log                     FALSE       FALSE     NA      NA
## A.fashion.log                  FALSE       FALSE     NA      NA
## H.npnct30.log                  FALSE       FALSE     NA      NA
## A.report.log                   FALSE       FALSE     NA      NA
## A.articl.log                   FALSE       FALSE     NA      NA
## A.npnct04.log                  FALSE       FALSE     NA      NA
## H.X2015.log                    FALSE       FALSE     NA      NA
## A.share.log                    FALSE       FALSE     NA      NA
## H.today.log                    FALSE       FALSE     NA      NA
## H.new.log                      FALSE       FALSE     NA      NA
## H.has.year.colon               FALSE       FALSE     NA      NA
## A.said.log                      TRUE       FALSE     NA      NA
## H.ndgts.log                    FALSE       FALSE     NA      NA
## PubDate.last10.log             FALSE       FALSE     NA      NA
## H.npnct04.log                  FALSE       FALSE     NA      NA
## H.npnct08.log                  FALSE       FALSE     NA      NA
## A.newyork.log                  FALSE       FALSE     NA      NA
## S.npnct15.log                  FALSE       FALSE     NA      NA
## A.npnct15.log                  FALSE       FALSE     NA      NA
## PubDate.wkday.fctr             FALSE       FALSE     NA      NA
## .rnorm                         FALSE       FALSE     NA      NA
## A.week.log                     FALSE       FALSE     NA      NA
## H.npnct13.log                  FALSE       FALSE     NA      NA
## A.show.log                     FALSE       FALSE     NA      NA
## A.intern.log                   FALSE       FALSE     NA      NA
## S.time.log                     FALSE       FALSE     NA      NA
## H.npnct07.log                  FALSE       FALSE     NA      NA
## A.npnct18.log                  FALSE       FALSE     NA      NA
## H.npnct06.log                  FALSE       FALSE     NA      NA
## S.state.log                     TRUE       FALSE     NA      NA
## PubDate.minute.fctr            FALSE       FALSE     NA      NA
## A.npnct19.log                  FALSE       FALSE     NA      NA
## A.can.log                      FALSE       FALSE     NA      NA
## PubDate.date.fctr              FALSE       FALSE     NA      NA
## S.npnct12.log                  FALSE       FALSE     NA      NA
## A.npnct12.log                  FALSE       FALSE     NA      NA
## H.nuppr.log                    FALSE       FALSE     NA      NA
## H.week.log                     FALSE       FALSE     NA      NA
## A.state.log                     TRUE       FALSE     NA      NA
## S.npnct13.log                  FALSE       FALSE     NA      NA
## A.npnct13.log                  FALSE       FALSE     NA      NA
## A.presid.log                   FALSE       FALSE     NA      NA
## S.npnct16.log                  FALSE       FALSE     NA      NA
## S.npnct08.log                   TRUE       FALSE     NA      NA
## S.can.log                      FALSE       FALSE     NA      NA
## A.npnct17.log                   TRUE       FALSE     NA      NA
## A.will.log                     FALSE       FALSE     NA      NA
## S.ndgts.log                    FALSE       FALSE     NA      NA
## S.compani.log                  FALSE       FALSE     NA      NA
## H.npnct05.log                  FALSE       FALSE     NA      NA
## A.npnct21.log                  FALSE       FALSE     NA      NA
## S.nuppr.log                    FALSE       FALSE     NA      NA
## H.nchrs.log                    FALSE       FALSE     NA      NA
## A.year.log                     FALSE       FALSE     NA      NA
## A.npnct01.log                  FALSE       FALSE     NA      NA
## S.will.log                     FALSE       FALSE     NA      NA
## A.ndgts.log                    FALSE       FALSE     NA      NA
## PubDate.last100.log             TRUE       FALSE     NA      NA
## A.npnct22.log                  FALSE       FALSE     NA      NA
## A.nuppr.log                    FALSE       FALSE     NA      NA
## S.day.log                      FALSE       FALSE     NA      NA
## S.one.log                       TRUE       FALSE     NA      NA
## A.one.log                       TRUE       FALSE     NA      NA
## PubDate.second.fctr            FALSE       FALSE     NA      NA
## H.X2014.log                    FALSE       FALSE     NA      NA
## H.report.log                   FALSE       FALSE     NA      NA
## S.take.log                     FALSE       FALSE     NA      NA
## S.nwrds.unq.log                FALSE       FALSE     NA      NA
## S.npnct09.log                   TRUE       FALSE     NA      NA
## A.nwrds.unq.log                FALSE       FALSE     NA      NA
## H.npnct12.log                  FALSE       FALSE     NA      NA
## H.fashion.log                  FALSE       FALSE     NA      NA
## S.npnct21.log                  FALSE       FALSE     NA      NA
## S.new.log                      FALSE       FALSE     NA      NA
## H.npnct15.log                  FALSE       FALSE     NA      NA
## A.new.log                      FALSE       FALSE     NA      NA
## A.nchrs.log                    FALSE       FALSE     NA      NA
## S.nchrs.log                    FALSE       FALSE     NA      NA
## S.npnct30.log                  FALSE       FALSE     NA      NA
## H.npnct17.log                  FALSE       FALSE     NA      NA
## A.npnct06.log                  FALSE       FALSE     NA      NA
## A.npnct30.log                  FALSE       FALSE     NA      NA
## H.nwrds.unq.log                FALSE       FALSE     NA      NA
## S.nwrds.log                    FALSE       FALSE     NA      NA
## A.npnct07.log                  FALSE       FALSE     NA      NA
## H.npnct01.log                  FALSE       FALSE     NA      NA
## A.nwrds.log                    FALSE       FALSE     NA      NA
## H.has.ebola                    FALSE       FALSE     NA      NA
## A.npnct14.log                  FALSE       FALSE     NA      NA
## S.npnct14.log                  FALSE       FALSE     NA      NA
## PubDate.last1.log              FALSE       FALSE     NA      NA
## A.npnct02.log                  FALSE       FALSE     NA      NA
## H.day.log                      FALSE       FALSE     NA      NA
## A.first.log                    FALSE       FALSE     NA      NA
## S.npnct03.log                  FALSE       FALSE     NA      NA
## H.newyork.log                  FALSE       FALSE     NA      NA
## A.has.year.colon               FALSE       FALSE     NA      NA
## PubDate.wkend                  FALSE       FALSE     NA      NA
## H.npnct16.log                  FALSE       FALSE     NA      NA
## H.npnct14.log                  FALSE       FALSE     NA      NA
## H.nwrds.log                    FALSE       FALSE     NA      NA
## H.npnct02.log                  FALSE       FALSE     NA      NA
## H.daili.log                    FALSE       FALSE     NA      NA
## A.compani.log                  FALSE       FALSE     NA      NA
## A.day.log                      FALSE       FALSE     NA      NA
## A.has.http                     FALSE       FALSE     NA      NA
## A.npnct03.log                  FALSE       FALSE     NA      NA
## A.npnct05.log                     NA       FALSE     NA      NA
## A.npnct08.log                   TRUE       FALSE     NA      NA
## A.npnct09.log                   TRUE       FALSE     NA      NA
## A.npnct10.log                     NA       FALSE     NA      NA
## A.npnct11.log                   TRUE       FALSE     NA      NA
## A.npnct16.log                  FALSE       FALSE     NA      NA
## A.npnct20.log                  FALSE       FALSE     NA      NA
## A.npnct23.log                  FALSE       FALSE     NA      NA
## A.npnct24.log                     NA       FALSE     NA      NA
## A.npnct25.log                  FALSE       FALSE     NA      NA
## A.npnct26.log                   TRUE       FALSE     NA      NA
## A.npnct27.log                   TRUE       FALSE     NA      NA
## A.npnct28.log                     NA       FALSE     NA      NA
## A.npnct29.log                     NA       FALSE     NA      NA
## A.npnct31.log                     NA       FALSE     NA      NA
## A.npnct32.log                     NA       FALSE     NA      NA
## A.take.log                     FALSE       FALSE     NA      NA
## A.time.log                     FALSE       FALSE     NA      NA
## H.has.http                        NA       FALSE     NA      NA
## H.npnct03.log                  FALSE       FALSE     NA      NA
## H.npnct09.log                  FALSE       FALSE     NA      NA
## H.npnct10.log                     NA       FALSE     NA      NA
## H.npnct11.log                   TRUE       FALSE     NA      NA
## H.npnct18.log                     NA       FALSE     NA      NA
## H.npnct19.log                     NA       FALSE     NA      NA
## H.npnct20.log                     NA       FALSE     NA      NA
## H.npnct22.log                   TRUE       FALSE     NA      NA
## H.npnct23.log                     NA       FALSE     NA      NA
## H.npnct24.log                     NA       FALSE     NA      NA
## H.npnct25.log                     NA       FALSE     NA      NA
## H.npnct26.log                   TRUE       FALSE     NA      NA
## H.npnct27.log                     NA       FALSE     NA      NA
## H.npnct28.log                     NA       FALSE     NA      NA
## H.npnct29.log                     NA       FALSE     NA      NA
## H.npnct31.log                     NA       FALSE     NA      NA
## H.npnct32.log                     NA       FALSE     NA      NA
## Popular                        FALSE        TRUE     NA      NA
## Popular.fctr                      NA          NA     NA    TRUE
## PubDate.last1                  FALSE       FALSE     NA      NA
## PubDate.last10                 FALSE       FALSE     NA      NA
## PubDate.last100                FALSE       FALSE     NA      NA
## PubDate.month.fctr             FALSE       FALSE     NA      NA
## PubDate.POSIX                  FALSE       FALSE     NA      NA
## PubDate.year.fctr                 NA       FALSE     NA      NA
## PubDate.zoo                    FALSE       FALSE     NA      NA
## S.articl.log                   FALSE       FALSE     NA      NA
## S.fashion.log                  FALSE       FALSE     NA      NA
## S.first.log                    FALSE       FALSE     NA      NA
## S.has.http                        NA       FALSE     NA      NA
## S.has.year.colon               FALSE       FALSE     NA      NA
## S.intern.log                   FALSE       FALSE     NA      NA
## S.make.log                     FALSE       FALSE     NA      NA
## S.newyork.log                  FALSE       FALSE     NA      NA
## S.npnct01.log                  FALSE       FALSE     NA      NA
## S.npnct02.log                   TRUE       FALSE     NA      NA
## S.npnct04.log                  FALSE       FALSE     NA      NA
## S.npnct05.log                     NA       FALSE     NA      NA
## S.npnct06.log                  FALSE       FALSE     NA      NA
## S.npnct07.log                  FALSE       FALSE     NA      NA
## S.npnct10.log                     NA       FALSE     NA      NA
## S.npnct11.log                   TRUE       FALSE     NA      NA
## S.npnct17.log                   TRUE       FALSE     NA      NA
## S.npnct18.log                     NA       FALSE     NA      NA
## S.npnct19.log                     NA       FALSE     NA      NA
## S.npnct20.log                     NA       FALSE     NA      NA
## S.npnct22.log                  FALSE       FALSE     NA      NA
## S.npnct23.log                  FALSE       FALSE     NA      NA
## S.npnct24.log                     NA       FALSE     NA      NA
## S.npnct25.log                  FALSE       FALSE     NA      NA
## S.npnct26.log                   TRUE       FALSE     NA      NA
## S.npnct27.log                     NA       FALSE     NA      NA
## S.npnct28.log                     NA       FALSE     NA      NA
## S.npnct29.log                     NA       FALSE     NA      NA
## S.npnct31.log                     NA       FALSE     NA      NA
## S.npnct32.log                     NA       FALSE     NA      NA
## S.presid.log                   FALSE       FALSE     NA      NA
## S.report.log                   FALSE       FALSE     NA      NA
## S.said.log                      TRUE       FALSE     NA      NA
## S.share.log                    FALSE       FALSE     NA      NA
## S.show.log                     FALSE       FALSE     NA      NA
## S.week.log                     FALSE       FALSE     NA      NA
## S.year.log                     FALSE       FALSE     NA      NA
## UniqueID                       FALSE       FALSE   TRUE      NA
## WordCount                      FALSE       FALSE     NA      NA
##                     All.X.glm.importance Final.glm.importance
## myCategory.fctr             100.00000000         100.00000000
## WordCount.log                70.43241920          70.43241920
## H.npnct21.log                32.71499686          32.71499686
## PubDate.hour.fctr            28.49497858          28.49497858
## A.make.log                   24.82458430          24.82458430
## A.fashion.log                22.86799201          22.86799201
## H.npnct30.log                18.66889549          18.66889549
## A.report.log                 17.94396169          17.94396169
## A.articl.log                 17.31584878          17.31584878
## A.npnct04.log                16.42726704          16.42726704
## H.X2015.log                  16.18781324          16.18781324
## A.share.log                  16.05772852          16.05772852
## H.today.log                  15.09058570          15.09058570
## H.new.log                    14.58884852          14.58884852
## H.has.year.colon             14.48643170          14.48643170
## A.said.log                   13.53465142          13.53465142
## H.ndgts.log                  13.52245535          13.52245535
## PubDate.last10.log           12.97780472          12.97780472
## H.npnct04.log                12.76927377          12.76927377
## H.npnct08.log                12.06593521          12.06593521
## A.newyork.log                11.59720267          11.59720267
## S.npnct15.log                10.97856358          10.97856358
## A.npnct15.log                10.79302566          10.79302566
## PubDate.wkday.fctr           10.68599632          10.68599632
## .rnorm                       10.55274877          10.55274877
## A.week.log                    9.81958866           9.81958866
## H.npnct13.log                 9.71218057           9.71218057
## A.show.log                    9.68407375           9.68407375
## A.intern.log                  9.66632050           9.66632050
## S.time.log                    9.55323497           9.55323497
## H.npnct07.log                 9.53685753           9.53685753
## A.npnct18.log                 9.41984800           9.41984800
## H.npnct06.log                 9.23627876           9.23627876
## S.state.log                   8.92098916           8.92098916
## PubDate.minute.fctr           8.53101958           8.53101958
## A.npnct19.log                 8.47715771           8.47715771
## A.can.log                     8.10387656           8.10387656
## PubDate.date.fctr             8.03583604           8.03583604
## S.npnct12.log                 8.03465413           8.03465413
## A.npnct12.log                 7.98621020           7.98621020
## H.nuppr.log                   7.85639322           7.85639322
## H.week.log                    7.79627328           7.79627328
## A.state.log                   7.26321159           7.26321159
## S.npnct13.log                 7.17910453           7.17910453
## A.npnct13.log                 7.17784272           7.17784272
## A.presid.log                  7.16305732           7.16305732
## S.npnct16.log                 7.14944805           7.14944805
## S.npnct08.log                 7.07080676           7.07080676
## S.can.log                     7.02341628           7.02341628
## A.npnct17.log                 6.89744623           6.89744623
## A.will.log                    6.81698075           6.81698075
## S.ndgts.log                   6.75768296           6.75768296
## S.compani.log                 6.68327086           6.68327086
## H.npnct05.log                 6.59752354           6.59752354
## A.npnct21.log                 6.53946049           6.53946049
## S.nuppr.log                   6.48716764           6.48716764
## H.nchrs.log                   6.36565681           6.36565681
## A.year.log                    6.30335271           6.30335271
## A.npnct01.log                 6.26548239           6.26548239
## S.will.log                    6.21988118           6.21988118
## A.ndgts.log                   6.12230312           6.12230312
## PubDate.last100.log           6.10602730           6.10602730
## A.npnct22.log                 5.81913883           5.81913883
## A.nuppr.log                   5.73056332           5.73056332
## S.day.log                     5.55760002           5.55760002
## S.one.log                     5.29435441           5.29435441
## A.one.log                     5.19360228           5.19360228
## PubDate.second.fctr           4.57971207           4.57971207
## H.X2014.log                   4.46472089           4.46472089
## H.report.log                  4.40029310           4.40029310
## S.take.log                    4.26732531           4.26732531
## S.nwrds.unq.log               4.13474268           4.13474268
## S.npnct09.log                 4.11245488           4.11245488
## A.nwrds.unq.log               3.95688107           3.95688107
## H.npnct12.log                 3.76801998           3.76801998
## H.fashion.log                 3.76390763           3.76390763
## S.npnct21.log                 3.66026012           3.66026012
## S.new.log                     3.52281315           3.52281315
## H.npnct15.log                 3.45385040           3.45385040
## A.new.log                     3.41102464           3.41102464
## A.nchrs.log                   3.12081764           3.12081764
## S.nchrs.log                   2.97802811           2.97802811
## S.npnct30.log                 2.88153488           2.88153488
## H.npnct17.log                 2.24624596           2.24624596
## A.npnct06.log                 2.18674221           2.18674221
## A.npnct30.log                 2.11807078           2.11807078
## H.nwrds.unq.log               2.09016785           2.09016785
## S.nwrds.log                   2.07125498           2.07125498
## A.npnct07.log                 2.04536000           2.04536000
## H.npnct01.log                 2.03495324           2.03495324
## A.nwrds.log                   1.93513242           1.93513242
## H.has.ebola                   1.75502087           1.75502087
## A.npnct14.log                 1.53950062           1.53950062
## S.npnct14.log                 1.28524340           1.28524340
## PubDate.last1.log             1.18736302           1.18736302
## A.npnct02.log                 1.14672657           1.14672657
## H.day.log                     1.12673197           1.12673197
## A.first.log                   1.11773697           1.11773697
## S.npnct03.log                 1.07345568           1.07345568
## H.newyork.log                 1.03389528           1.03389528
## A.has.year.colon              0.89413765           0.89413765
## PubDate.wkend                 0.64555965           0.64555965
## H.npnct16.log                 0.59350225           0.59350225
## H.npnct14.log                 0.52235605           0.52235605
## H.nwrds.log                   0.46021386           0.46021386
## H.npnct02.log                 0.04154952           0.04154952
## H.daili.log                   0.00000000           0.00000000
## A.compani.log                         NA                   NA
## A.day.log                             NA                   NA
## A.has.http                            NA                   NA
## A.npnct03.log                         NA                   NA
## A.npnct05.log                         NA                   NA
## A.npnct08.log                         NA                   NA
## A.npnct09.log                         NA                   NA
## A.npnct10.log                         NA                   NA
## A.npnct11.log                         NA                   NA
## A.npnct16.log                         NA                   NA
## A.npnct20.log                         NA                   NA
## A.npnct23.log                         NA                   NA
## A.npnct24.log                         NA                   NA
## A.npnct25.log                         NA                   NA
## A.npnct26.log                         NA                   NA
## A.npnct27.log                         NA                   NA
## A.npnct28.log                         NA                   NA
## A.npnct29.log                         NA                   NA
## A.npnct31.log                         NA                   NA
## A.npnct32.log                         NA                   NA
## A.take.log                            NA                   NA
## A.time.log                            NA                   NA
## H.has.http                            NA                   NA
## H.npnct03.log                         NA                   NA
## H.npnct09.log                         NA                   NA
## H.npnct10.log                         NA                   NA
## H.npnct11.log                         NA                   NA
## H.npnct18.log                         NA                   NA
## H.npnct19.log                         NA                   NA
## H.npnct20.log                         NA                   NA
## H.npnct22.log                         NA                   NA
## H.npnct23.log                         NA                   NA
## H.npnct24.log                         NA                   NA
## H.npnct25.log                         NA                   NA
## H.npnct26.log                         NA                   NA
## H.npnct27.log                         NA                   NA
## H.npnct28.log                         NA                   NA
## H.npnct29.log                         NA                   NA
## H.npnct31.log                         NA                   NA
## H.npnct32.log                         NA                   NA
## Popular                               NA                   NA
## Popular.fctr                          NA                   NA
## PubDate.last1                         NA                   NA
## PubDate.last10                        NA                   NA
## PubDate.last100                       NA                   NA
## PubDate.month.fctr                    NA                   NA
## PubDate.POSIX                         NA                   NA
## PubDate.year.fctr                     NA                   NA
## PubDate.zoo                           NA                   NA
## S.articl.log                          NA                   NA
## S.fashion.log                         NA                   NA
## S.first.log                           NA                   NA
## S.has.http                            NA                   NA
## S.has.year.colon                      NA                   NA
## S.intern.log                          NA                   NA
## S.make.log                            NA                   NA
## S.newyork.log                         NA                   NA
## S.npnct01.log                         NA                   NA
## S.npnct02.log                         NA                   NA
## S.npnct04.log                         NA                   NA
## S.npnct05.log                         NA                   NA
## S.npnct06.log                         NA                   NA
## S.npnct07.log                         NA                   NA
## S.npnct10.log                         NA                   NA
## S.npnct11.log                         NA                   NA
## S.npnct17.log                         NA                   NA
## S.npnct18.log                         NA                   NA
## S.npnct19.log                         NA                   NA
## S.npnct20.log                         NA                   NA
## S.npnct22.log                         NA                   NA
## S.npnct23.log                         NA                   NA
## S.npnct24.log                         NA                   NA
## S.npnct25.log                         NA                   NA
## S.npnct26.log                         NA                   NA
## S.npnct27.log                         NA                   NA
## S.npnct28.log                         NA                   NA
## S.npnct29.log                         NA                   NA
## S.npnct31.log                         NA                   NA
## S.npnct32.log                         NA                   NA
## S.presid.log                          NA                   NA
## S.report.log                          NA                   NA
## S.said.log                            NA                   NA
## S.share.log                           NA                   NA
## S.show.log                            NA                   NA
## S.week.log                            NA                   NA
## S.year.log                            NA                   NA
## UniqueID                              NA                   NA
## WordCount                             NA                   NA
```

```r
glb_analytics_diag_plots(obs_df=glb_trnent_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnent_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 106
```

![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_1-1.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_1-2.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_1-3.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_1-4.png) ![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_1-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 6370     6370            Y                        0.7203429879
## 1507     1507            N                        0.0004021506
##      Popular.fctr.predict.Final.glm
## 6370                              N
## 1507                              N
##      Popular.fctr.predict.Final.glm.accurate
## 6370                                   FALSE
## 1507                                    TRUE
##      Popular.fctr.predict.Final.glm.error .label
## 6370                            -0.179657   6370
## 1507                             0.000000   1507
## [1] "Inaccurate: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 6101     6101            Y                         0.002533866
## 4721     4721            Y                         0.002748124
## 1923     1923            Y                         0.002923488
## 2182     2182            Y                         0.003895315
## 3113     3113            Y                         0.005880657
## 5486     5486            Y                         0.005958308
##      Popular.fctr.predict.Final.glm
## 6101                              N
## 4721                              N
## 1923                              N
## 2182                              N
## 3113                              N
## 5486                              N
##      Popular.fctr.predict.Final.glm.accurate
## 6101                                   FALSE
## 4721                                   FALSE
## 1923                                   FALSE
## 2182                                   FALSE
## 3113                                   FALSE
## 5486                                   FALSE
##      Popular.fctr.predict.Final.glm.error
## 6101                           -0.8974661
## 4721                           -0.8972519
## 1923                           -0.8970765
## 2182                           -0.8961047
## 3113                           -0.8941193
## 5486                           -0.8940417
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 3113     3113            Y                         0.005880657
## 3880     3880            Y                         0.186338969
## 825       825            Y                         0.229296417
## 2752     2752            Y                         0.316310041
## 6143     6143            Y                         0.629357041
## 371       371            Y                         0.851015096
##      Popular.fctr.predict.Final.glm
## 3113                              N
## 3880                              N
## 825                               N
## 2752                              N
## 6143                              N
## 371                               N
##      Popular.fctr.predict.Final.glm.accurate
## 3113                                   FALSE
## 3880                                   FALSE
## 825                                    FALSE
## 2752                                   FALSE
## 6143                                   FALSE
## 371                                    FALSE
##      Popular.fctr.predict.Final.glm.error
## 3113                           -0.8941193
## 3880                           -0.7136610
## 825                            -0.6707036
## 2752                           -0.5836900
## 6143                           -0.2706430
## 371                            -0.0489849
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 1612     1612            N                           0.9548095
## 1667     1667            N                           0.9549935
## 1448     1448            N                           0.9751221
## 4882     4882            N                           0.9769537
## 59         59            N                           0.9770689
## 770       770            N                           0.9798837
##      Popular.fctr.predict.Final.glm
## 1612                              Y
## 1667                              Y
## 1448                              Y
## 4882                              Y
## 59                                Y
## 770                               Y
##      Popular.fctr.predict.Final.glm.accurate
## 1612                                   FALSE
## 1667                                   FALSE
## 1448                                   FALSE
## 4882                                   FALSE
## 59                                     FALSE
## 770                                    FALSE
##      Popular.fctr.predict.Final.glm.error
## 1612                           0.05480953
## 1667                           0.05499353
## 1448                           0.07512209
## 4882                           0.07695368
## 59                             0.07706890
## 770                            0.07988369
```

![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_1-6.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

print(glb_trnent_df[glb_trnent_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_trnent_df), value=TRUE)])
```

```
##      Popular.fctr Popular.fctr.predict.Final.glm.prob
## 92              Y                         0.040690149
## 693             Y                         0.052822202
## 4020            Y                         0.007595007
## 4721            Y                         0.002748124
##      Popular.fctr.predict.Final.glm
## 92                                N
## 693                               N
## 4020                              N
## 4721                              N
```

```r
sav_entity_df <- glb_entity_df
print(setdiff(names(glb_trnent_df), names(glb_entity_df)))
```

```
## [1] "Popular.fctr.predict.Final.glm.prob"
## [2] "Popular.fctr.predict.Final.glm"
```

```r
for (col in setdiff(names(glb_trnent_df), names(glb_entity_df)))
    # Merge or cbind ?
    glb_entity_df[glb_entity_df$.src == "Train", col] <- glb_trnent_df[, col]

print(setdiff(names(glb_fitent_df), names(glb_entity_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBent_df), names(glb_entity_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBent_df), names(glb_entity_df)))
    # Merge or cbind ?
    glb_entity_df[glb_entity_df$.lcn == "OOB", col] <- glb_OOBent_df[, col]
    
print(setdiff(names(glb_newent_df), names(glb_entity_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_entity_df, 
         #glb_trnent_df, glb_fitent_df, glb_OOBent_df, glb_newent_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](NYTBlogs_txtfeat_files/figure-html/fit.data.training_1-7.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          7          1 476.848 487.891  11.043
## 15  predict.data.new          8          0 487.892      NA      NA
```

## Step `8.0: predict data new`

```r
# Compute final model predictions
glb_newent_df <- glb_get_predictions(glb_newent_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(glb_newent_df, mdl_id = glb_fin_mdl_id,
## rsp_var_out = glb_rsp_var_out, : Using default probability threshold: 0.9
```

```r
glb_analytics_diag_plots(obs_df=glb_newent_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_newent_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 106
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_txtfeat_files/figure-html/predict.data.new-1.png) 

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_txtfeat_files/figure-html/predict.data.new-2.png) 

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_txtfeat_files/figure-html/predict.data.new-3.png) 

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_txtfeat_files/figure-html/predict.data.new-4.png) 

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_txtfeat_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 6753     6753         <NA>                        8.292048e-01
## 7309     7309         <NA>                        2.017900e-07
##      Popular.fctr.predict.Final.glm
## 6753                              N
## 7309                              N
##      Popular.fctr.predict.Final.glm.accurate
## 6753                                      NA
## 7309                                      NA
##      Popular.fctr.predict.Final.glm.error .label
## 6753                                    0   6753
## 7309                                    0   7309
## [1] "Inaccurate: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## NA         NA         <NA>                                  NA
## NA.1       NA         <NA>                                  NA
## NA.2       NA         <NA>                                  NA
## NA.3       NA         <NA>                                  NA
## NA.4       NA         <NA>                                  NA
## NA.5       NA         <NA>                                  NA
##      Popular.fctr.predict.Final.glm
## NA                             <NA>
## NA.1                           <NA>
## NA.2                           <NA>
## NA.3                           <NA>
## NA.4                           <NA>
## NA.5                           <NA>
##      Popular.fctr.predict.Final.glm.accurate
## NA                                        NA
## NA.1                                      NA
## NA.2                                      NA
## NA.3                                      NA
## NA.4                                      NA
## NA.5                                      NA
##      Popular.fctr.predict.Final.glm.error
## NA                                     NA
## NA.1                                   NA
## NA.2                                   NA
## NA.3                                   NA
## NA.4                                   NA
## NA.5                                   NA
##         UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## NA.258        NA         <NA>                                  NA
## NA.373        NA         <NA>                                  NA
## NA.568        NA         <NA>                                  NA
## NA.1724       NA         <NA>                                  NA
## NA.1759       NA         <NA>                                  NA
## NA.1843       NA         <NA>                                  NA
##         Popular.fctr.predict.Final.glm
## NA.258                            <NA>
## NA.373                            <NA>
## NA.568                            <NA>
## NA.1724                           <NA>
## NA.1759                           <NA>
## NA.1843                           <NA>
##         Popular.fctr.predict.Final.glm.accurate
## NA.258                                       NA
## NA.373                                       NA
## NA.568                                       NA
## NA.1724                                      NA
## NA.1759                                      NA
## NA.1843                                      NA
##         Popular.fctr.predict.Final.glm.error
## NA.258                                    NA
## NA.373                                    NA
## NA.568                                    NA
## NA.1724                                   NA
## NA.1759                                   NA
## NA.1843                                   NA
##         UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## NA.1864       NA         <NA>                                  NA
## NA.1865       NA         <NA>                                  NA
## NA.1866       NA         <NA>                                  NA
## NA.1867       NA         <NA>                                  NA
## NA.1868       NA         <NA>                                  NA
## NA.1869       NA         <NA>                                  NA
##         Popular.fctr.predict.Final.glm
## NA.1864                           <NA>
## NA.1865                           <NA>
## NA.1866                           <NA>
## NA.1867                           <NA>
## NA.1868                           <NA>
## NA.1869                           <NA>
##         Popular.fctr.predict.Final.glm.accurate
## NA.1864                                      NA
## NA.1865                                      NA
## NA.1866                                      NA
## NA.1867                                      NA
## NA.1868                                      NA
## NA.1869                                      NA
##         Popular.fctr.predict.Final.glm.error
## NA.1864                                   NA
## NA.1865                                   NA
## NA.1866                                   NA
## NA.1867                                   NA
## NA.1868                                   NA
## NA.1869                                   NA
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_txtfeat_files/figure-html/predict.data.new-6.png) 

```r
submit_df <- glb_newent_df[, c(glb_id_vars, 
                               paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
names(submit_df)[2] <- "Probability1"
write.csv(submit_df, 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
           "_submit.csv"), row.names=FALSE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                    "opt.prob.threshold.OOB"])
```

```
## [1] 0.9
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: All.X.glm"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.glm"
```

```r
print(dim(glb_fitent_df))
```

```
## [1] 4475  204
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 9                  All.X.glm        0.9042295   0.7972982     0.6336042
## 8              Low.cor.X.glm        0.9037433   0.9262548     0.6675461
## 10      All.X.no.rnorm.rpart        0.8862421   0.7084504     0.5054039
## 11         All.X.no.rnorm.rf        0.8838114   0.9217314     0.6079058
## 1          MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.8327662   0.5000000     0.0000000
## 5            Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 7    Interact.High.cor.Y.glm        0.7914439   0.7739949     0.3256506
## 6              Max.cor.Y.glm        0.7316480   0.7102060     0.2283681
## 2    Random.myrandom_classfr        0.1672338   0.4909227     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 9    30122.145                    0.9
## 8     2079.176                    0.3
## 10          NA                    0.7
## 11          NA                    0.3
## 1           NA                    0.5
## 3           NA                    0.5
## 4           NA                    0.5
## 5           NA                    0.5
## 7     3424.717                    0.3
## 6     3714.601                    0.2
## 2           NA                    0.1
```

```r
print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
```

```
## [1] "All.X.glm OOB confusion matrix & accuracy: "
```

```r
print(t(confusionMatrix(glb_OOBent_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                        glb_OOBent_df[, glb_rsp_var])$table))
```

```
##          Prediction
## Reference    N    Y
##         N 1641   72
##         Y  125  219
```

```r
tmp_OOBent_df <- glb_OOBent_df[, c("myCategory", predct_accurate_var_name)]
names(tmp_OOBent_df)[2] <- "accurate.OOB"
aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBent_df, names(tmp_OOBent_df)) 
aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                        .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                        max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
```

```
## [1] "myCategory" ".n.OOB"
```

```r
glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
```

```
##                              myCategory .n.OOB .n.Tst .freqRatio.Tst
## 1                                    ##    407    338    0.180748663
## 6        Business#Business Day#Dealbook    312    304    0.162566845
## 15                        OpEd#Opinion#    154    164    0.087700535
## 18                         Styles#U.S.#     54     62    0.033155080
## 10                        Culture#Arts#    225    244    0.130481283
## 9                  Business#Technology#    114    113    0.060427807
## 16                      Science#Health#     66     57    0.030481283
## 20                             TStyle##    221    105    0.056149733
## 4            #Opinion#The Public Editor     10     10    0.005347594
## 8            Business#Crosswords/Games#     40     42    0.022459893
## 13                 Metro#N.Y. / Region#     60     67    0.035828877
## 3              #Opinion#Room For Debate     21     24    0.012834225
## 7  Business#Business Day#Small Business     45     42    0.022459893
## 5                       #U.S.#Education     93     90    0.048128342
## 17                      Styles##Fashion     41     15    0.008021390
## 2                          #Multimedia#     42     52    0.027807487
## 11                       Foreign#World#     47     47    0.025133690
## 12           Foreign#World#Asia Pacific     61     56    0.029946524
## 14                              myOther     13      3    0.001604278
## 19                       Travel#Travel#     31     35    0.018716578
##    .freqRatio.OOB accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 1     0.197860963                 37               370        0.9090909
## 6     0.151677200                 32               280        0.8974359
## 15    0.074866310                 32               122        0.7922078
## 18    0.026251823                 27                27        0.5000000
## 10    0.109382596                 18               207        0.9200000
## 9     0.055420515                 16                98        0.8596491
## 16    0.032085561                 15                51        0.7727273
## 20    0.107438017                  5               216        0.9773756
## 4     0.004861449                  3                 7        0.7000000
## 8     0.019445795                  3                37        0.9250000
## 13    0.029168692                  3                57        0.9500000
## 3     0.010209042                  2                19        0.9047619
## 7     0.021876519                  2                43        0.9555556
## 5     0.045211473                  1                92        0.9892473
## 17    0.019931940                  1                40        0.9756098
## 2     0.020418085                  0                42        1.0000000
## 11    0.022848809                  0                47        1.0000000
## 12    0.029654837                  0                61        1.0000000
## 14    0.006319883                  0                13        1.0000000
## 19    0.015070491                  0                31        1.0000000
```

```r
dsp_NewsDesk.nb_conf_mtrx <- function(NewsDesk.nb) {
    print(sprintf("%s OOB::NewsDesk.nb=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, NewsDesk.nb))
    print(t(confusionMatrix(
        glb_OOBent_df[glb_OOBent_df$NewsDesk.nb == NewsDesk.nb, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBent_df[glb_OOBent_df$NewsDesk.nb == NewsDesk.nb, glb_rsp_var])$table))
    print(sum(glb_OOBent_df[glb_OOBent_df$NewsDesk.nb == NewsDesk.nb, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBent_df[glb_OOBent_df$NewsDesk.nb == NewsDesk.nb, ]))
    err_ids <- glb_OOBent_df[(glb_OOBent_df$NewsDesk.nb == NewsDesk.nb) & 
                             (!glb_OOBent_df[, predct_accurate_var_name]), glb_id_vars]
    print(sprintf("%s OOB::NewsDesk.nb=%s errors: ", glb_sel_mdl_id, NewsDesk.nb))
    print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% err_ids, 
                        c("Headline.pfx", "Headline", "Popular")])
}
#dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="myMultimedia")

print("FN_OOB_ids:")
```

```
## [1] "FN_OOB_ids:"
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_OOBent_df), value=TRUE)])
```

```
## [1] Popular.fctr                           
## [2] Popular.fctr.predict.All.X.glm.prob    
## [3] Popular.fctr.predict.All.X.glm         
## [4] Popular.fctr.predict.All.X.glm.accurate
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_txt_vars])
```

```
## [1] Headline Snippet  Abstract
## <0 rows> (or 0-length row.names)
```

```r
print(dsp_vctr <- colSums(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    setdiff(grep("[HSA].", names(glb_OOBent_df), value=TRUE),
                            union(myfind_chr_cols_df(glb_OOBent_df),
                grep(".fctr", names(glb_OOBent_df), fixed=TRUE, value=TRUE)))]))
```

```
##    PubDate.POSIX      H.X2014.log      H.X2015.log      H.daili.log 
##                0                0                0                0 
##        H.day.log    H.fashion.log        H.new.log    H.newyork.log 
##                0                0                0                0 
##     H.report.log      H.today.log       H.week.log       H.has.http 
##                0                0                0                0 
##      H.has.ebola      H.nwrds.log  H.nwrds.unq.log      H.nchrs.log 
##                0                0                0                0 
##      H.nuppr.log      H.ndgts.log    H.npnct01.log    H.npnct02.log 
##                0                0                0                0 
##    H.npnct03.log    H.npnct04.log    H.npnct05.log    H.npnct06.log 
##                0                0                0                0 
##    H.npnct07.log    H.npnct08.log    H.npnct09.log    H.npnct10.log 
##                0                0                0                0 
##    H.npnct11.log    H.npnct12.log    H.npnct13.log    H.npnct14.log 
##                0                0                0                0 
##    H.npnct15.log    H.npnct16.log    H.npnct17.log    H.npnct18.log 
##                0                0                0                0 
##    H.npnct19.log    H.npnct20.log    H.npnct21.log    H.npnct22.log 
##                0                0                0                0 
##    H.npnct23.log    H.npnct24.log    H.npnct25.log    H.npnct26.log 
##                0                0                0                0 
##    H.npnct27.log    H.npnct28.log    H.npnct29.log    H.npnct30.log 
##                0                0                0                0 
##    H.npnct31.log    H.npnct32.log H.has.year.colon     S.articl.log 
##                0                0                0                0 
##        S.can.log    S.compani.log        S.day.log    S.fashion.log 
##                0                0                0                0 
##      S.first.log     S.intern.log       S.make.log        S.new.log 
##                0                0                0                0 
##    S.newyork.log        S.one.log     S.presid.log     S.report.log 
##                0                0                0                0 
##       S.said.log      S.share.log       S.show.log      S.state.log 
##                0                0                0                0 
##       S.take.log       S.time.log       S.week.log       S.will.log 
##                0                0                0                0 
##       S.year.log       S.has.http      S.nwrds.log  S.nwrds.unq.log 
##                0                0                0                0 
##      S.nchrs.log      S.nuppr.log      S.ndgts.log    S.npnct01.log 
##                0                0                0                0 
##    S.npnct02.log    S.npnct03.log    S.npnct04.log    S.npnct05.log 
##                0                0                0                0 
##    S.npnct06.log    S.npnct07.log    S.npnct08.log    S.npnct09.log 
##                0                0                0                0 
##    S.npnct10.log    S.npnct11.log    S.npnct12.log    S.npnct13.log 
##                0                0                0                0 
##    S.npnct14.log    S.npnct15.log    S.npnct16.log    S.npnct17.log 
##                0                0                0                0 
##    S.npnct18.log    S.npnct19.log    S.npnct20.log    S.npnct21.log 
##                0                0                0                0 
##    S.npnct22.log    S.npnct23.log    S.npnct24.log    S.npnct25.log 
##                0                0                0                0 
##    S.npnct26.log    S.npnct27.log    S.npnct28.log    S.npnct29.log 
##                0                0                0                0 
##    S.npnct30.log    S.npnct31.log    S.npnct32.log S.has.year.colon 
##                0                0                0                0 
##     A.articl.log        A.can.log    A.compani.log        A.day.log 
##                0                0                0                0 
##    A.fashion.log      A.first.log     A.intern.log       A.make.log 
##                0                0                0                0 
##        A.new.log    A.newyork.log        A.one.log     A.presid.log 
##                0                0                0                0 
##     A.report.log       A.said.log      A.share.log       A.show.log 
##                0                0                0                0 
##      A.state.log       A.take.log       A.time.log       A.week.log 
##                0                0                0                0 
##       A.will.log       A.year.log       A.has.http      A.nwrds.log 
##                0                0                0                0 
##  A.nwrds.unq.log      A.nchrs.log      A.nuppr.log      A.ndgts.log 
##                0                0                0                0 
##    A.npnct01.log    A.npnct02.log    A.npnct03.log    A.npnct04.log 
##                0                0                0                0 
##    A.npnct05.log    A.npnct06.log    A.npnct07.log    A.npnct08.log 
##                0                0                0                0 
##    A.npnct09.log    A.npnct10.log    A.npnct11.log    A.npnct12.log 
##                0                0                0                0 
##    A.npnct13.log    A.npnct14.log    A.npnct15.log    A.npnct16.log 
##                0                0                0                0 
##    A.npnct17.log    A.npnct18.log    A.npnct19.log    A.npnct20.log 
##                0                0                0                0 
##    A.npnct21.log    A.npnct22.log    A.npnct23.log    A.npnct24.log 
##                0                0                0                0 
##    A.npnct25.log    A.npnct26.log    A.npnct27.log    A.npnct28.log 
##                0                0                0                0 
##    A.npnct29.log    A.npnct30.log    A.npnct31.log    A.npnct32.log 
##                0                0                0                0 
## A.has.year.colon 
##                0
```

```r
dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBent_df[glb_OOBent_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBent_df), value=TRUE)])
    print(glb_newent_df[glb_newent_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newent_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newent_df[glb_newent_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newent_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newent_df),
                    grep(".fctr", names(glb_newent_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newent_df[glb_newent_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newent_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBent_df), value=TRUE)])

# print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_entity_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_entity_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_entity_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(subset(glb_feats_df, !is.na(importance))[,
    c("zeroVar", "nzv", "myNearZV", 
      grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
```

```
##                     zeroVar   nzv myNearZV   importance
## myCategory.fctr       FALSE FALSE    FALSE 100.00000000
## WordCount.log         FALSE FALSE    FALSE  70.43241920
## H.npnct21.log         FALSE FALSE    FALSE  32.71499686
## PubDate.hour.fctr     FALSE FALSE    FALSE  28.49497858
## A.make.log            FALSE  TRUE    FALSE  24.82458430
## A.fashion.log         FALSE  TRUE    FALSE  22.86799201
## H.npnct30.log         FALSE  TRUE    FALSE  18.66889549
## A.report.log          FALSE  TRUE    FALSE  17.94396169
## A.articl.log          FALSE  TRUE    FALSE  17.31584878
## A.npnct04.log         FALSE  TRUE    FALSE  16.42726704
## H.X2015.log           FALSE  TRUE    FALSE  16.18781324
## A.share.log           FALSE  TRUE    FALSE  16.05772852
## H.today.log           FALSE  TRUE    FALSE  15.09058570
## H.new.log             FALSE  TRUE    FALSE  14.58884852
## H.has.year.colon      FALSE  TRUE    FALSE  14.48643170
## A.said.log            FALSE  TRUE    FALSE  13.53465142
## H.ndgts.log           FALSE FALSE    FALSE  13.52245535
## PubDate.last10.log    FALSE FALSE    FALSE  12.97780472
## H.npnct04.log         FALSE  TRUE    FALSE  12.76927377
## H.npnct08.log         FALSE  TRUE    FALSE  12.06593521
## A.newyork.log         FALSE FALSE    FALSE  11.59720267
## S.npnct15.log         FALSE  TRUE    FALSE  10.97856358
## A.npnct15.log         FALSE  TRUE    FALSE  10.79302566
## PubDate.wkday.fctr    FALSE FALSE    FALSE  10.68599632
## .rnorm                FALSE FALSE    FALSE  10.55274877
## A.week.log            FALSE FALSE    FALSE   9.81958866
## H.npnct13.log         FALSE FALSE    FALSE   9.71218057
## A.show.log            FALSE  TRUE    FALSE   9.68407375
## A.intern.log          FALSE  TRUE    FALSE   9.66632050
## S.time.log            FALSE FALSE    FALSE   9.55323497
## H.npnct07.log         FALSE FALSE    FALSE   9.53685753
## A.npnct18.log         FALSE  TRUE    FALSE   9.41984800
## H.npnct06.log         FALSE  TRUE    FALSE   9.23627876
## S.state.log           FALSE  TRUE    FALSE   8.92098916
## PubDate.minute.fctr   FALSE FALSE    FALSE   8.53101958
## A.npnct19.log         FALSE  TRUE    FALSE   8.47715771
## A.can.log             FALSE  TRUE    FALSE   8.10387656
## PubDate.date.fctr     FALSE FALSE    FALSE   8.03583604
## S.npnct12.log         FALSE FALSE    FALSE   8.03465413
## A.npnct12.log         FALSE FALSE    FALSE   7.98621020
## H.nuppr.log           FALSE FALSE    FALSE   7.85639322
## H.week.log            FALSE  TRUE    FALSE   7.79627328
## A.state.log           FALSE  TRUE    FALSE   7.26321159
## S.npnct13.log         FALSE FALSE    FALSE   7.17910453
## A.npnct13.log         FALSE FALSE    FALSE   7.17784272
## A.presid.log          FALSE  TRUE    FALSE   7.16305732
## S.npnct16.log         FALSE FALSE    FALSE   7.14944805
## S.npnct08.log         FALSE  TRUE    FALSE   7.07080676
## S.can.log             FALSE  TRUE    FALSE   7.02341628
## A.npnct17.log         FALSE  TRUE    FALSE   6.89744623
## A.will.log            FALSE FALSE    FALSE   6.81698075
## S.ndgts.log           FALSE FALSE    FALSE   6.75768296
## S.compani.log         FALSE FALSE    FALSE   6.68327086
## H.npnct05.log         FALSE  TRUE    FALSE   6.59752354
## A.npnct21.log         FALSE FALSE    FALSE   6.53946049
## S.nuppr.log           FALSE FALSE    FALSE   6.48716764
## H.nchrs.log           FALSE FALSE    FALSE   6.36565681
## A.year.log            FALSE FALSE    FALSE   6.30335271
## A.npnct01.log         FALSE  TRUE    FALSE   6.26548239
## S.will.log            FALSE FALSE    FALSE   6.21988118
## A.ndgts.log           FALSE FALSE    FALSE   6.12230312
## PubDate.last100.log   FALSE FALSE    FALSE   6.10602730
## A.npnct22.log         FALSE  TRUE    FALSE   5.81913883
## A.nuppr.log           FALSE FALSE    FALSE   5.73056332
## S.day.log             FALSE  TRUE    FALSE   5.55760002
## S.one.log             FALSE  TRUE    FALSE   5.29435441
## A.one.log             FALSE  TRUE    FALSE   5.19360228
## PubDate.second.fctr   FALSE FALSE    FALSE   4.57971207
## H.X2014.log           FALSE  TRUE    FALSE   4.46472089
## H.report.log          FALSE  TRUE    FALSE   4.40029310
## S.take.log            FALSE  TRUE    FALSE   4.26732531
## S.nwrds.unq.log       FALSE FALSE    FALSE   4.13474268
## S.npnct09.log         FALSE  TRUE    FALSE   4.11245488
## A.nwrds.unq.log       FALSE FALSE    FALSE   3.95688107
## H.npnct12.log         FALSE FALSE    FALSE   3.76801998
## H.fashion.log         FALSE  TRUE    FALSE   3.76390763
## S.npnct21.log         FALSE FALSE    FALSE   3.66026012
## S.new.log             FALSE FALSE    FALSE   3.52281315
## H.npnct15.log         FALSE  TRUE    FALSE   3.45385040
## A.new.log             FALSE FALSE    FALSE   3.41102464
## A.nchrs.log           FALSE FALSE    FALSE   3.12081764
## S.nchrs.log           FALSE FALSE    FALSE   2.97802811
## S.npnct30.log         FALSE  TRUE    FALSE   2.88153488
## H.npnct17.log         FALSE  TRUE    FALSE   2.24624596
## A.npnct06.log         FALSE  TRUE    FALSE   2.18674221
## A.npnct30.log         FALSE  TRUE    FALSE   2.11807078
## H.nwrds.unq.log       FALSE FALSE    FALSE   2.09016785
## S.nwrds.log           FALSE FALSE    FALSE   2.07125498
## A.npnct07.log         FALSE  TRUE    FALSE   2.04536000
## H.npnct01.log         FALSE  TRUE    FALSE   2.03495324
## A.nwrds.log           FALSE FALSE    FALSE   1.93513242
## H.has.ebola           FALSE  TRUE    FALSE   1.75502087
## A.npnct14.log         FALSE FALSE    FALSE   1.53950062
## S.npnct14.log         FALSE FALSE    FALSE   1.28524340
## PubDate.last1.log     FALSE FALSE    FALSE   1.18736302
## A.npnct02.log         FALSE  TRUE    FALSE   1.14672657
## H.day.log             FALSE  TRUE    FALSE   1.12673197
## A.first.log           FALSE  TRUE    FALSE   1.11773697
## S.npnct03.log         FALSE  TRUE    FALSE   1.07345568
## H.newyork.log         FALSE  TRUE    FALSE   1.03389528
## A.has.year.colon      FALSE  TRUE    FALSE   0.89413765
## PubDate.wkend         FALSE FALSE    FALSE   0.64555965
## H.npnct16.log         FALSE FALSE    FALSE   0.59350225
## H.npnct14.log         FALSE  TRUE    FALSE   0.52235605
## H.nwrds.log           FALSE FALSE    FALSE   0.46021386
## H.npnct02.log         FALSE  TRUE    FALSE   0.04154952
## H.daili.log           FALSE  TRUE    FALSE   0.00000000
##                     All.X.glm.importance Final.glm.importance
## myCategory.fctr             100.00000000         100.00000000
## WordCount.log                70.43241920          70.43241920
## H.npnct21.log                32.71499686          32.71499686
## PubDate.hour.fctr            28.49497858          28.49497858
## A.make.log                   24.82458430          24.82458430
## A.fashion.log                22.86799201          22.86799201
## H.npnct30.log                18.66889549          18.66889549
## A.report.log                 17.94396169          17.94396169
## A.articl.log                 17.31584878          17.31584878
## A.npnct04.log                16.42726704          16.42726704
## H.X2015.log                  16.18781324          16.18781324
## A.share.log                  16.05772852          16.05772852
## H.today.log                  15.09058570          15.09058570
## H.new.log                    14.58884852          14.58884852
## H.has.year.colon             14.48643170          14.48643170
## A.said.log                   13.53465142          13.53465142
## H.ndgts.log                  13.52245535          13.52245535
## PubDate.last10.log           12.97780472          12.97780472
## H.npnct04.log                12.76927377          12.76927377
## H.npnct08.log                12.06593521          12.06593521
## A.newyork.log                11.59720267          11.59720267
## S.npnct15.log                10.97856358          10.97856358
## A.npnct15.log                10.79302566          10.79302566
## PubDate.wkday.fctr           10.68599632          10.68599632
## .rnorm                       10.55274877          10.55274877
## A.week.log                    9.81958866           9.81958866
## H.npnct13.log                 9.71218057           9.71218057
## A.show.log                    9.68407375           9.68407375
## A.intern.log                  9.66632050           9.66632050
## S.time.log                    9.55323497           9.55323497
## H.npnct07.log                 9.53685753           9.53685753
## A.npnct18.log                 9.41984800           9.41984800
## H.npnct06.log                 9.23627876           9.23627876
## S.state.log                   8.92098916           8.92098916
## PubDate.minute.fctr           8.53101958           8.53101958
## A.npnct19.log                 8.47715771           8.47715771
## A.can.log                     8.10387656           8.10387656
## PubDate.date.fctr             8.03583604           8.03583604
## S.npnct12.log                 8.03465413           8.03465413
## A.npnct12.log                 7.98621020           7.98621020
## H.nuppr.log                   7.85639322           7.85639322
## H.week.log                    7.79627328           7.79627328
## A.state.log                   7.26321159           7.26321159
## S.npnct13.log                 7.17910453           7.17910453
## A.npnct13.log                 7.17784272           7.17784272
## A.presid.log                  7.16305732           7.16305732
## S.npnct16.log                 7.14944805           7.14944805
## S.npnct08.log                 7.07080676           7.07080676
## S.can.log                     7.02341628           7.02341628
## A.npnct17.log                 6.89744623           6.89744623
## A.will.log                    6.81698075           6.81698075
## S.ndgts.log                   6.75768296           6.75768296
## S.compani.log                 6.68327086           6.68327086
## H.npnct05.log                 6.59752354           6.59752354
## A.npnct21.log                 6.53946049           6.53946049
## S.nuppr.log                   6.48716764           6.48716764
## H.nchrs.log                   6.36565681           6.36565681
## A.year.log                    6.30335271           6.30335271
## A.npnct01.log                 6.26548239           6.26548239
## S.will.log                    6.21988118           6.21988118
## A.ndgts.log                   6.12230312           6.12230312
## PubDate.last100.log           6.10602730           6.10602730
## A.npnct22.log                 5.81913883           5.81913883
## A.nuppr.log                   5.73056332           5.73056332
## S.day.log                     5.55760002           5.55760002
## S.one.log                     5.29435441           5.29435441
## A.one.log                     5.19360228           5.19360228
## PubDate.second.fctr           4.57971207           4.57971207
## H.X2014.log                   4.46472089           4.46472089
## H.report.log                  4.40029310           4.40029310
## S.take.log                    4.26732531           4.26732531
## S.nwrds.unq.log               4.13474268           4.13474268
## S.npnct09.log                 4.11245488           4.11245488
## A.nwrds.unq.log               3.95688107           3.95688107
## H.npnct12.log                 3.76801998           3.76801998
## H.fashion.log                 3.76390763           3.76390763
## S.npnct21.log                 3.66026012           3.66026012
## S.new.log                     3.52281315           3.52281315
## H.npnct15.log                 3.45385040           3.45385040
## A.new.log                     3.41102464           3.41102464
## A.nchrs.log                   3.12081764           3.12081764
## S.nchrs.log                   2.97802811           2.97802811
## S.npnct30.log                 2.88153488           2.88153488
## H.npnct17.log                 2.24624596           2.24624596
## A.npnct06.log                 2.18674221           2.18674221
## A.npnct30.log                 2.11807078           2.11807078
## H.nwrds.unq.log               2.09016785           2.09016785
## S.nwrds.log                   2.07125498           2.07125498
## A.npnct07.log                 2.04536000           2.04536000
## H.npnct01.log                 2.03495324           2.03495324
## A.nwrds.log                   1.93513242           1.93513242
## H.has.ebola                   1.75502087           1.75502087
## A.npnct14.log                 1.53950062           1.53950062
## S.npnct14.log                 1.28524340           1.28524340
## PubDate.last1.log             1.18736302           1.18736302
## A.npnct02.log                 1.14672657           1.14672657
## H.day.log                     1.12673197           1.12673197
## A.first.log                   1.11773697           1.11773697
## S.npnct03.log                 1.07345568           1.07345568
## H.newyork.log                 1.03389528           1.03389528
## A.has.year.colon              0.89413765           0.89413765
## PubDate.wkend                 0.64555965           0.64555965
## H.npnct16.log                 0.59350225           0.59350225
## H.npnct14.log                 0.52235605           0.52235605
## H.nwrds.log                   0.46021386           0.46021386
## H.npnct02.log                 0.04154952           0.04154952
## H.daili.log                   0.00000000           0.00000000
```

```r
print(subset(glb_feats_df, is.na(importance))[,
    c("zeroVar", "nzv", "myNearZV", 
      grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
```

```
##                    zeroVar   nzv myNearZV importance All.X.glm.importance
## A.compani.log        FALSE FALSE    FALSE         NA                   NA
## A.day.log            FALSE  TRUE    FALSE         NA                   NA
## A.has.http           FALSE  TRUE    FALSE         NA                   NA
## A.npnct03.log        FALSE  TRUE    FALSE         NA                   NA
## A.npnct05.log         TRUE  TRUE     TRUE         NA                   NA
## A.npnct08.log        FALSE  TRUE    FALSE         NA                   NA
## A.npnct09.log        FALSE  TRUE    FALSE         NA                   NA
## A.npnct10.log         TRUE  TRUE     TRUE         NA                   NA
## A.npnct11.log        FALSE  TRUE     TRUE         NA                   NA
## A.npnct16.log        FALSE FALSE    FALSE         NA                   NA
## A.npnct20.log        FALSE  TRUE    FALSE         NA                   NA
## A.npnct23.log        FALSE  TRUE     TRUE         NA                   NA
## A.npnct24.log         TRUE  TRUE     TRUE         NA                   NA
## A.npnct25.log        FALSE  TRUE     TRUE         NA                   NA
## A.npnct26.log         TRUE  TRUE     TRUE         NA                   NA
## A.npnct27.log        FALSE  TRUE     TRUE         NA                   NA
## A.npnct28.log         TRUE  TRUE     TRUE         NA                   NA
## A.npnct29.log         TRUE  TRUE     TRUE         NA                   NA
## A.npnct31.log         TRUE  TRUE     TRUE         NA                   NA
## A.npnct32.log         TRUE  TRUE     TRUE         NA                   NA
## A.take.log           FALSE  TRUE    FALSE         NA                   NA
## A.time.log           FALSE FALSE    FALSE         NA                   NA
## H.has.http            TRUE  TRUE     TRUE         NA                   NA
## H.npnct03.log        FALSE  TRUE     TRUE         NA                   NA
## H.npnct09.log        FALSE  TRUE    FALSE         NA                   NA
## H.npnct10.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct11.log        FALSE  TRUE     TRUE         NA                   NA
## H.npnct18.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct19.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct20.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct22.log        FALSE  TRUE     TRUE         NA                   NA
## H.npnct23.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct24.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct25.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct26.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct27.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct28.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct29.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct31.log         TRUE  TRUE     TRUE         NA                   NA
## H.npnct32.log         TRUE  TRUE     TRUE         NA                   NA
## Popular              FALSE FALSE    FALSE         NA                   NA
## Popular.fctr            NA    NA       NA         NA                   NA
## PubDate.last1        FALSE FALSE    FALSE         NA                   NA
## PubDate.last10       FALSE FALSE    FALSE         NA                   NA
## PubDate.last100      FALSE FALSE    FALSE         NA                   NA
## PubDate.month.fctr   FALSE FALSE    FALSE         NA                   NA
## PubDate.POSIX        FALSE FALSE    FALSE         NA                   NA
## PubDate.year.fctr     TRUE  TRUE     TRUE         NA                   NA
## PubDate.zoo          FALSE FALSE    FALSE         NA                   NA
## S.articl.log         FALSE  TRUE    FALSE         NA                   NA
## S.fashion.log        FALSE  TRUE    FALSE         NA                   NA
## S.first.log          FALSE  TRUE    FALSE         NA                   NA
## S.has.http            TRUE  TRUE     TRUE         NA                   NA
## S.has.year.colon     FALSE  TRUE    FALSE         NA                   NA
## S.intern.log         FALSE  TRUE    FALSE         NA                   NA
## S.make.log           FALSE  TRUE    FALSE         NA                   NA
## S.newyork.log        FALSE FALSE    FALSE         NA                   NA
## S.npnct01.log        FALSE  TRUE    FALSE         NA                   NA
## S.npnct02.log        FALSE  TRUE     TRUE         NA                   NA
## S.npnct04.log        FALSE  TRUE    FALSE         NA                   NA
## S.npnct05.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct06.log        FALSE  TRUE    FALSE         NA                   NA
## S.npnct07.log        FALSE  TRUE    FALSE         NA                   NA
## S.npnct10.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct11.log        FALSE  TRUE     TRUE         NA                   NA
## S.npnct17.log        FALSE  TRUE    FALSE         NA                   NA
## S.npnct18.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct19.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct20.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct22.log        FALSE  TRUE    FALSE         NA                   NA
## S.npnct23.log        FALSE  TRUE     TRUE         NA                   NA
## S.npnct24.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct25.log        FALSE  TRUE     TRUE         NA                   NA
## S.npnct26.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct27.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct28.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct29.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct31.log         TRUE  TRUE     TRUE         NA                   NA
## S.npnct32.log         TRUE  TRUE     TRUE         NA                   NA
## S.presid.log         FALSE  TRUE    FALSE         NA                   NA
## S.report.log         FALSE  TRUE    FALSE         NA                   NA
## S.said.log           FALSE  TRUE    FALSE         NA                   NA
## S.share.log          FALSE  TRUE    FALSE         NA                   NA
## S.show.log           FALSE  TRUE    FALSE         NA                   NA
## S.week.log           FALSE FALSE    FALSE         NA                   NA
## S.year.log           FALSE FALSE    FALSE         NA                   NA
## UniqueID             FALSE FALSE    FALSE         NA                   NA
## WordCount            FALSE FALSE    FALSE         NA                   NA
##                    Final.glm.importance
## A.compani.log                        NA
## A.day.log                            NA
## A.has.http                           NA
## A.npnct03.log                        NA
## A.npnct05.log                        NA
## A.npnct08.log                        NA
## A.npnct09.log                        NA
## A.npnct10.log                        NA
## A.npnct11.log                        NA
## A.npnct16.log                        NA
## A.npnct20.log                        NA
## A.npnct23.log                        NA
## A.npnct24.log                        NA
## A.npnct25.log                        NA
## A.npnct26.log                        NA
## A.npnct27.log                        NA
## A.npnct28.log                        NA
## A.npnct29.log                        NA
## A.npnct31.log                        NA
## A.npnct32.log                        NA
## A.take.log                           NA
## A.time.log                           NA
## H.has.http                           NA
## H.npnct03.log                        NA
## H.npnct09.log                        NA
## H.npnct10.log                        NA
## H.npnct11.log                        NA
## H.npnct18.log                        NA
## H.npnct19.log                        NA
## H.npnct20.log                        NA
## H.npnct22.log                        NA
## H.npnct23.log                        NA
## H.npnct24.log                        NA
## H.npnct25.log                        NA
## H.npnct26.log                        NA
## H.npnct27.log                        NA
## H.npnct28.log                        NA
## H.npnct29.log                        NA
## H.npnct31.log                        NA
## H.npnct32.log                        NA
## Popular                              NA
## Popular.fctr                         NA
## PubDate.last1                        NA
## PubDate.last10                       NA
## PubDate.last100                      NA
## PubDate.month.fctr                   NA
## PubDate.POSIX                        NA
## PubDate.year.fctr                    NA
## PubDate.zoo                          NA
## S.articl.log                         NA
## S.fashion.log                        NA
## S.first.log                          NA
## S.has.http                           NA
## S.has.year.colon                     NA
## S.intern.log                         NA
## S.make.log                           NA
## S.newyork.log                        NA
## S.npnct01.log                        NA
## S.npnct02.log                        NA
## S.npnct04.log                        NA
## S.npnct05.log                        NA
## S.npnct06.log                        NA
## S.npnct07.log                        NA
## S.npnct10.log                        NA
## S.npnct11.log                        NA
## S.npnct17.log                        NA
## S.npnct18.log                        NA
## S.npnct19.log                        NA
## S.npnct20.log                        NA
## S.npnct22.log                        NA
## S.npnct23.log                        NA
## S.npnct24.log                        NA
## S.npnct25.log                        NA
## S.npnct26.log                        NA
## S.npnct27.log                        NA
## S.npnct28.log                        NA
## S.npnct29.log                        NA
## S.npnct31.log                        NA
## S.npnct32.log                        NA
## S.presid.log                         NA
## S.report.log                         NA
## S.said.log                           NA
## S.share.log                          NA
## S.show.log                           NA
## S.week.log                           NA
## S.year.log                           NA
## UniqueID                             NA
## WordCount                            NA
```

```r
sav_entity_df <- glb_entity_df
print(setdiff(names(glb_trnent_df), names(glb_entity_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_trnent_df), names(glb_entity_df)))
    # Merge or cbind ?
    glb_entity_df[glb_entity_df$.src == "Train", col] <- glb_trnent_df[, col]

print(setdiff(names(glb_fitent_df), names(glb_entity_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBent_df), names(glb_entity_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBent_df), names(glb_entity_df)))
    # Merge or cbind ?
    glb_entity_df[glb_entity_df$.lcn == "OOB", col] <- glb_OOBent_df[, col]
    
print(setdiff(names(glb_newent_df), names(glb_entity_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_entity_df, 
         #glb_trnent_df, glb_fitent_df, glb_OOBent_df, glb_newent_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor     bgn    end elapsed
## 15     predict.data.new          8          0 487.892 497.05   9.158
## 16 display.session.info          9          0 497.050     NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor     bgn     end elapsed
## 10              fit.models          6          1 228.568 432.444 203.876
## 6         extract.features          3          0  36.591 147.378 110.787
## 7          select.features          4          0 147.378 188.221  40.843
## 9               fit.models          6          0 189.590 228.568  38.978
## 11              fit.models          6          2 432.444 451.331  18.888
## 13       fit.data.training          7          0 458.165 476.847  18.682
## 2             inspect.data          2          0   9.044  27.361  18.317
## 14       fit.data.training          7          1 476.848 487.891  11.043
## 15        predict.data.new          8          0 487.892 497.050   9.158
## 12              fit.models          6          3 451.332 458.164   6.833
## 4      manage.missing.data          2          2  31.544  36.532   4.989
## 3             cleanse.data          2          1  27.362  31.544   4.182
## 8  partition.data.training          5          0 188.222 189.589   1.367
## 1              import.data          1          0   8.129   9.043   0.915
## 5              encode.data          2          3  36.533  36.590   0.057
##    duration
## 10  203.876
## 6   110.787
## 7    40.843
## 9    38.978
## 11   18.887
## 13   18.682
## 2    18.317
## 14   11.043
## 15    9.158
## 12    6.832
## 4     4.988
## 3     4.182
## 8     1.367
## 1     0.914
## 5     0.057
## [1] "Total Elapsed Time: 497.05 secs"
```

![](NYTBlogs_txtfeat_files/figure-html/display.session.info-1.png) 

```
##                label step_major step_minor     bgn     end elapsed
## 4    fit.models_1_rf          4          0 263.087 432.436 169.349
## 2   fit.models_1_glm          2          0 232.726 250.408  17.682
## 3 fit.models_1_rpart          3          0 250.408 263.087  12.679
## 1   fit.models_1_bgn          1          0 232.714 232.725   0.012
##   duration
## 4  169.349
## 2   17.682
## 3   12.679
## 1    0.011
## [1] "Total Elapsed Time: 432.436 secs"
```

![](NYTBlogs_txtfeat_files/figure-html/display.session.info-2.png) 

```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
##  [1] tcltk     grid      parallel  stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
##  [1] randomForest_4.6-10 rpart.plot_1.5.2    rpart_4.1-9        
##  [4] ROCR_1.0-7          gplots_2.16.0       caTools_1.17.1     
##  [7] caret_6.0-41        tm_0.6              NLP_0.1-6          
## [10] mice_2.22           lattice_0.20-31     Rcpp_0.11.5        
## [13] plyr_1.8.1          zoo_1.7-12          sqldf_0.4-10       
## [16] RSQLite_1.0.0       DBI_0.3.1           gsubfn_0.6-6       
## [19] proto_0.3-10        reshape2_1.4.1      doMC_1.3.3         
## [22] iterators_1.0.7     foreach_1.4.2       doBy_4.5-13        
## [25] survival_2.38-1     ggplot2_1.0.1      
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6        BradleyTerry2_1.0-6 brglm_0.5-9        
##  [4] car_2.0-25          chron_2.3-45        class_7.3-12       
##  [7] codetools_0.2-11    colorspace_1.2-6    compiler_3.1.3     
## [10] digest_0.6.8        e1071_1.6-4         evaluate_0.5.5     
## [13] formatR_1.1         gdata_2.13.3        gtable_0.1.2       
## [16] gtools_3.4.1        htmltools_0.2.6     KernSmooth_2.23-14 
## [19] knitr_1.9           labeling_0.3        lme4_1.1-7         
## [22] MASS_7.3-40         Matrix_1.2-0        mgcv_1.8-6         
## [25] minqa_1.2.4         munsell_0.4.2       nlme_3.1-120       
## [28] nloptr_1.0.4        nnet_7.3-9          pbkrtest_0.4-2     
## [31] quantreg_5.11       RColorBrewer_1.1-2  rmarkdown_0.5.1    
## [34] scales_0.2.4        slam_0.1-32         SparseM_1.6        
## [37] splines_3.1.3       stringr_0.6.2       tools_3.1.3        
## [40] yaml_2.1.13
```
