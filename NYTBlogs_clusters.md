# NYTimes:Blogs:: Popular classification:: clusters
bdanalytics  

**  **    
**Date: (Tue) May 19, 2015**    

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
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")

#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTrain.csv"
glb_newdt_url <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTest.csv"
glb_out_pfx <- "NYTBlogs_clusters_"
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

# Remember to use unstemmed words
glb_append_stop_words[["Headline"]] <- c(NULL
                            ,"clip" # Highly correlated to H.P.daily.clip.report
                            ,"springsummer" # Highly correlated to H.npnct14.log
                            )
glb_append_stop_words[["Snippet"]] <- c(NULL
                            ,"herald" # Highly correlated to S.T.tribun
                            ,"photo"  # Highly correlated to A.T.photo
    ,"senate", "senator", "senatorial", "senators", "senatorselect" # Highly correlated to A.T.senat
    ,"archival", "archive", "archives", "archivist" # Highly correlated to H.P.year.colon
    ,"year", "years" # Highly correlated with A.T.year
    ,"appear", "appeared", "appearing", "appears" # Highly correlated with A.T.appear
                            )
glb_append_stop_words[["Abstract"]] <- 
    c("archives", "articles", "diary", "first", "herald", 
      "president", "share", "show", "tribune" # These are repeated in Snippet terms
      ,"highlight", "highlighted", "highlighting", "highlights" # Correlated with S.T.highlight
      ,"make", "makes" # correlated with S.T.make
     )
# Remember to use stemmed terms 
glb_important_terms <- list()

# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitent_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBent_df))
#       numrows(glb_OOBent_df) = 1.1 * numrows(glb_newent_df)

glb_sprs_thresholds <- c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
#glb_sprs_thresholds <- c(0.990, 0.970, 0.970) # Generates 41, 22, 22 terms
#glb_sprs_thresholds <- c(0.985, 0.970, 0.970) # Generates 16, 22, 22 terms
#glb_sprs_thresholds <- c(0.975, 0.965, 0.965) # Generates 08, 14, 14 terms
#glb_sprs_thresholds <- c(0.982, 0.980, 0.980) # Generates 10, 61, 62 terms
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

glb_cluster <- FALSE # or TRUE

glb_models_lst <- list(); glb_models_df <- data.frame()
# rpart:  .rnorm messes with the models badly
#         caret creates dummy vars for factor feats which messes up the tuning
#             - better to feed as.numeric(<feat>.fctr) to caret 
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        #glb_models_method_vctr <- c("glm", "rpart", "rf") else  
        #glb_models_method_vctr <- c("glm", "bayesglm", "rpart") else              
        glb_models_method_vctr <- c("glm", "rpart") else                          
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

![](NYTBlogs_clusters_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor    bgn end elapsed
## 1 import.data          1          0 12.361  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

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
##          label step_major step_minor    bgn    end elapsed
## 1  import.data          1          0 12.361 13.436   1.075
## 2 inspect.data          2          0 13.437     NA      NA
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

![](NYTBlogs_clusters_files/figure-html/inspect.data-1.png) 

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

![](NYTBlogs_clusters_files/figure-html/inspect.data-2.png) 

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

![](NYTBlogs_clusters_files/figure-html/inspect.data-3.png) 

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

![](NYTBlogs_clusters_files/figure-html/inspect.data-4.png) 

```r
srt_entity_df <- orderBy(~PubDate.POSIX, glb_entity_df)
print(myplot_scatter(subset(srt_entity_df, 
                            PubDate.POSIX < strptime("2014-09-02", "%Y-%m-%d")), 
                            xcol_name="PubDate.POSIX", ycol_name=glb_rsp_var,
                           colorcol_name=glb_rsp_var
                     ))
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-5.png) 

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

![](NYTBlogs_clusters_files/figure-html/inspect.data-6.png) 

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

![](NYTBlogs_clusters_files/figure-html/inspect.data-7.png) 

```r
last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
srt_entity_df[, "PubDate.last10"] <- last10
srt_entity_df[is.na(srt_entity_df$PubDate.last10), "PubDate.last10"] <- 0
srt_entity_df[, "PubDate.last10.log"] <- log(1 + srt_entity_df[, "PubDate.last10"])
print(gp <- myplot_box(df=subset(srt_entity_df, PubDate.last10.log > 0), 
                       ycol_names="PubDate.last10.log", 
                       xcol_name=glb_rsp_var))
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-8.png) 

```r
last100 = as.numeric(merge(z-lag(z, -100), b, all = TRUE))
srt_entity_df[, "PubDate.last100"] <- last100
srt_entity_df[is.na(srt_entity_df$PubDate.last100), "PubDate.last100"] <- 0
srt_entity_df[, "PubDate.last100.log"] <- log(1 + srt_entity_df[, "PubDate.last100"])
print(gp <- myplot_box(df=subset(srt_entity_df, PubDate.last100.log > 0), 
                       ycol_names="PubDate.last100.log", 
                       xcol_name=glb_rsp_var))
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-9.png) 

```r
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

![](NYTBlogs_clusters_files/figure-html/inspect.data-10.png) 

```
## [1] "var: PubDate.date.fctr"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-11.png) 

```
## [1] "var: PubDate.wkday.fctr"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-12.png) 

```
## [1] "var: PubDate.wkend"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-13.png) 

```
## [1] "var: PubDate.hour.fctr"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-14.png) 

```
## [1] "var: PubDate.minute.fctr"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-15.png) 

```
## [1] "var: PubDate.second.fctr"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-16.png) 

```
## [1] "var: PubDate.last1.log"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-17.png) 

```
## [1] "var: PubDate.last10.log"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-18.png) 

```
## [1] "var: PubDate.last100.log"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-19.png) 

```
## [1] "var: WordCount.log"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-20.png) 

```
## [1] "var: .rnorm"
```

![](NYTBlogs_clusters_files/figure-html/inspect.data-21.png) 

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

rm(srt_entity_df, last1, last10, last100, pd)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cleanse.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0 13.437 32.417   18.98
## 3 cleanse.data          2          1 32.417     NA      NA
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
        Headline.pfx=NULL, NewsDesk.nb=NULL, clusterid=NULL, myCategory=NULL,
        perl=FALSE) {
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
            tmp_entity_df[grep(Headline.contains, tmp_entity_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_entity_df <- 
            tmp_entity_df[grep(Snippet.contains, tmp_entity_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_entity_df <- 
            tmp_entity_df[grep(Abstract.contains, tmp_entity_df$Abstract, perl=perl), ]
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
    if (!is.null(clusterid)) {
        if (any(grepl("clusterid", names(tmp_entity_df), fixed=TRUE)) > 0) 
            tmp_entity_df <- 
                tmp_entity_df[tmp_entity_df$clusterid == clusterid, ] else
        warning("glb_entity_df does not contain clusterid; ignoring that filter")                    
    }    
    if (!is.null(myCategory)) {
        if (any(grepl("myCategory", names(tmp_entity_df), fixed=TRUE)) > 0) 
            tmp_entity_df <- 
                tmp_entity_df[tmp_entity_df$myCategory == myCategory, ] else
        warning("glb_entity_df does not contain myCategory; ignoring that filter")                    
    }    
    
    return(glb_entity_df$UniqueID %in% tmp_entity_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_entity_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
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
##      UniqueID Popular    myCategory
## 305       305       0 OpEd#Opinion#
## 844       844       1 OpEd#Opinion#
## 1331     1331       0 OpEd#Opinion#
## 1974     1974       0 OpEd#Opinion#
## 2563     2563       0 OpEd#Opinion#
## 3091     3091       0 OpEd#Opinion#
## 3589     3589       0 OpEd#Opinion#
## 4631     4631       0 OpEd#Opinion#
## 5125     5125       0 OpEd#Opinion#
## 5630     5630       0 OpEd#Opinion#
## 6095     6095       0 OpEd#Opinion#
## 6513     6513       1 OpEd#Opinion#
## 6927     6927      NA OpEd#Opinion#
## 7473     7473      NA     #Opinion#
## 7931     7931      NA OpEd#Opinion#
## 8217     8217      NA OpEd#Opinion#
##                                                       Headline NewsDesk
## 305              Friday Night Music: Lucius Covers John Lennon     OpEd
## 844                         Friday Night Music: Cheryl Wheeler     OpEd
## 1331            Friday Night Music: Cheryl Wheeler, Summer Fly     OpEd
## 1974                                 Friday Night Music: Quilt     OpEd
## 2563                   Friday Night Music: Lucius in Asheville     OpEd
## 3091 Friday Night Music: Sarah Jarosz and the Milk Carton Kids     OpEd
## 3589               Friday Night Music: Lucius Covers the Kinks     OpEd
## 4631                                Friday Night Music: Amason     OpEd
## 5125     Friday Night Music: Suzanne Vega, Jacob and the Angel     OpEd
## 5630      Friday Night Music: Suzanne Vega, I Never Wear White     OpEd
## 6095      Friday Night Music: Jessica Hernandez and the Deltas     OpEd
## 6513                         Saturday Morning Music: Stay Gold     OpEd
## 6927                      Friday Night Music: Lucius, Monsters     OpEd
## 7473                   Friday Night Music: Peter Gabriel, 1993         
## 7931         Friday Night Music: The Roches, Winter Wonderland     OpEd
## 8217      Friday Night Music: Sarah Jarosz and Aoife O'Donovan     OpEd
##      SectionName SubsectionName
## 305      Opinion               
## 844      Opinion               
## 1331     Opinion               
## 1974     Opinion               
## 2563     Opinion               
## 3091     Opinion               
## 3589     Opinion               
## 4631     Opinion               
## 5125     Opinion               
## 5630     Opinion               
## 6095     Opinion               
## 6513     Opinion               
## 6927     Opinion               
## 7473     Opinion               
## 7931     Opinion               
## 8217     Opinion
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
##      UniqueID Popular myCategory
## 516       516       0  #Opinion#
## 918       918       0  #Opinion#
## 7473     7473      NA  #Opinion#
## 7445     7445      NA  #Opinion#
## 7419     7419      NA  #Opinion#
## 7505     7505      NA  #Opinion#
## 7509     7509      NA  #Opinion#
##                                                              Headline
## 516             This Is Life Among the Roma, Europes Forgotten People
## 918          What Might Happen If Iran Becomes America's Covert Ally?
## 7473                          Friday Night Music: Peter Gabriel, 1993
## 7445 Senate Committee Bothered to Authorize War Against Islamic State
## 7419                                       Joe on WNYCs Money Talking
## 7505           Rev. Dr. William Barber II on Todays Protest Movements
## 7509                          Did Salaita Cross the Line of Civility?
##      NewsDesk SectionName SubsectionName
## 516               Opinion               
## 918               Opinion               
## 7473              Opinion               
## 7445              Opinion               
## 7419              Opinion               
## 7505              Opinion               
## 7509              Opinion
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
## 3        cleanse.data          2          1 32.417 36.577    4.16
## 4 manage.missing.data          2          2 36.578     NA      NA
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
## 4 manage.missing.data          2          2 36.578 42.019   5.441
## 5         encode.data          2          3 42.019     NA      NA
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
##              label step_major step_minor    bgn    end elapsed
## 5      encode.data          2          3 42.019 42.079    0.06
## 6 extract.features          3          0 42.079     NA      NA
```

## Step `3.0: extract features`

```r
#```{r extract_features, cache=FALSE, eval=glb_is_textual}
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 42.232  NA      NA
```

```r
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
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnent_df <- mutate(glb_trnent_df,
#                     )
# 
# glb_newent_df <- mutate(glb_newent_df,
#                     )

#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn   end
## 1                extract.features_bgn          1          0 42.232 42.24
## 2 extract.features_factorize.str.vars          2          0 42.241    NA
##   elapsed
## 1   0.008
## 2      NA
```

```r
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
    require(foreach)
    require(tm)

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    corpus_lst <- list(); 
    corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        print(sprintf("Building corpus for %s...", txt_var))
        
        # Combine "new york" to "newyork"
        #   shd be created as a tm_map::content_transformer
        txt_vctr <- glb_entity_df[, txt_var]
        
        txt_vctr <- gsub("[Nn]ew [Dd]elhi",   "newdelhi",   txt_vctr)                        
        txt_vctr <- gsub("[Nn]ew [Gg]uinea",  "newguinea",   txt_vctr)                                
        txt_vctr <- gsub("[Nn]ew [Jj]ersey",  "newjersey",  txt_vctr)                
        txt_vctr <- gsub("[Nn]ew [Oo]rleans", "neworleans",  txt_vctr)                
        txt_vctr <- gsub("[Nn]ew [Yy]ear",    "newyear",    txt_vctr)        
        txt_vctr <- gsub("[Nn]ew [Yy]ork",    "newyork",    txt_vctr)
        txt_vctr <- gsub("[Nn]ew [Zz]ealand", "newzealand", txt_vctr)
        
        txt_vctr <- gsub("-[Yy]ear-", " year ", txt_vctr)
        txt_vctr <- gsub("-[Yy]ear",  " year",  txt_vctr)
        txt_vctr <- gsub("[Yy]ear-",  "year ",  txt_vctr)
        txt_vctr <- gsub("-[Ww]eek-", " week ", txt_vctr)
        txt_vctr <- gsub("-[Ww]eek",  " week",  txt_vctr)
        txt_vctr <- gsub("[Ww]eek-",  "week ",  txt_vctr)
        #grep("10(.*)year", txt_vctr, value=TRUE)        
        #print(sel_vctr <- head(grep("-year", txt_vctr, value=TRUE)))
        #gsub("-year", " year", sel_vctr)
        
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation)
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))        
        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english")))
        txt_corpus <- tm_map(txt_corpus, stemDocument)
        #corpus_lst[[txt_var]] <- txt_corpus
    }
    names(corpus_lst) <- glb_txt_vars

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_entity_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("week", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_corpus_lst, corpus_lst))
#     print(all.equal(length(sav_corpus_lst), length(corpus_lst)))
#     print(all.equal(names(sav_corpus_lst), names(corpus_lst)))
#     print(all.equal(sav_corpus_lst[["Headline"]], corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))

    rm(full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_entity_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_entity_df[, c(glb_id_vars, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_entity_df[, c(glb_id_vars, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_entity_df <- cbind(glb_entity_df, txt_X_df) # TfIdf is normalized
        #glb_entity_df <- cbind(glb_entity_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_entity_df)
    #chk_entity_df <- glb_entity_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_entity_df[, c(glb_id_vars, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_entity_df[, c(glb_id_vars, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_entity_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_entity_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_entity_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_entity_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_entity_df[, txt_var]))
        }
#         print(head(glb_entity_df[glb_entity_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_entity_df[, txt_var]))    
    
        # Create user-specified pattern vectors 
        #   <txt_var>.P.year.colon
        txt_X_df[, paste0(txt_var_pfx, ".P.year.colon")] <-
            as.integer(0 + mycount_pattern_occ("[0-9]{4}:", glb_entity_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.daily.clip.report")] <-
            as.integer(0 + mycount_pattern_occ("Daily Clip Report", glb_entity_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.fashion.week")] <-
            as.integer(0 + mycount_pattern_occ("Fashion Week", glb_entity_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.first.draft")] <-
            as.integer(0 + mycount_pattern_occ("First Draft", glb_entity_df[, txt_var]))

#sum(mycount_pattern_occ("Metropolitan Diary:", glb_entity_df$Abstract) > 0)
        if (txt_var %in% c("Snippet", "Abstract")) {
            txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
                as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
                                                   glb_entity_df[, txt_var]))
        }

#sum(mycount_pattern_occ("[0-9]{4}:", glb_entity_df$Headline) > 0)
#sum(mycount_pattern_occ("Quandary(.*)(?=:)", glb_entity_df$Headline, perl=TRUE) > 0)
#sum(mycount_pattern_occ("No Comment(.*):", glb_entity_df$Headline) > 0)
#sum(mycount_pattern_occ("Recap:", glb_entity_df$Headline) > 0)
        if (txt_var %in% c("Headline")) {
            txt_X_df[, paste0(txt_var_pfx, ".P.quandary")] <-
                as.integer(0 + mycount_pattern_occ("Quandary(.*)(?=:)", glb_entity_df[, txt_var], perl=TRUE))
            txt_X_df[, paste0(txt_var_pfx, ".P.recap.colon")] <-
                as.integer(0 + mycount_pattern_occ("Recap:", glb_entity_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.no.comment.colon")] <-
                as.integer(0 + mycount_pattern_occ("No Comment(.*):", glb_entity_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.today.in.politic")] <-
                as.integer(0 + mycount_pattern_occ("Today in Politic", glb_entity_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.today.in.smallbusiness")] <-
                as.integer(0 + mycount_pattern_occ("Today in Small Business:", glb_entity_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.what.we.are")] <-
                as.integer(0 + mycount_pattern_occ("What We're", glb_entity_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.readers.respond")] <-
                as.integer(0 + mycount_pattern_occ("Readers Respond", glb_entity_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.s.notebook")] <-
                as.integer(0 + mycount_pattern_occ("s Notebook", glb_entity_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.verbatim.colon")] <-
                as.integer(0 + mycount_pattern_occ("Verbatim:", glb_entity_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.on.this.day")] <-
                as.integer(0 + mycount_pattern_occ("On This Day", glb_entity_df[, txt_var]))            
        }

#summary(glb_entity_df[ ,grep("P.on.this.day", names(glb_entity_df), value=TRUE)])
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_vars, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_entity_df <- cbind(glb_entity_df, txt_X_df)
    }
    glb_entity_df <- cbind(glb_entity_df, txt_X_df)

    # Generate summaries
#     print(summary(glb_entity_df))
#     print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
#     print(summary(glb_trnent_df))
#     print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
#     print(summary(glb_newent_df))
#     print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

    rm(log_X_df, txt_X_df)
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
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 42.241 42.671
## 3       extract.features_build.corpus          3          0 42.672     NA
##   elapsed
## 2    0.43
## 3      NA
##                           label step_major step_minor    bgn    end
## 3 extract.features_build.corpus          3          0 42.672 54.719
## 4  extract.features_extract.DTM          4          0 54.719     NA
##   elapsed
## 3  12.047
## 4      NA
## [1] "Extracting TfIDf terms for Headline..."
```

```
## Warning in weighting(x): empty document(s): character(0) character(0)
## character(0)
```

```
## [1] "Extracting TfIDf terms for Snippet..."
```

```
## Warning in weighting(x): empty document(s): character(0) character(0)
```

```
## [1] "Extracting TfIDf terms for Abstract..."
```

```
## Warning in weighting(x): empty document(s): character(0) character(0)
```

```
##                          label step_major step_minor    bgn   end elapsed
## 4 extract.features_extract.DTM          4          0 54.719 68.42  13.702
## 5  extract.features_report.DTM          5          0 68.421    NA      NA
## [1] "Reporting TfIDf terms for Headline..."
## [1] "   Full TermMatrix:"
## <<DocumentTermMatrix (documents: 8402, terms: 9196)>>
## Non-/sparse entries: 44170/77220622
## Sparsity           : 100%
## Maximal term length: 31
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
## [1] "   Sparse TermMatrix:"
## <<DocumentTermMatrix (documents: 8402, terms: 28)>>
## Non-/sparse entries: 4572/230684
## Sparsity           : 98%
## Maximal term length: 7
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
```

```
## Warning in myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full",
## colorcol_name = "in.sprs"): converting in.sprs to class:factor
```

![](NYTBlogs_clusters_files/figure-html/extract.features-1.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTBlogs_clusters_files/figure-html/extract.features-2.png) ![](NYTBlogs_clusters_files/figure-html/extract.features-3.png) 

```
## [1] "Reporting TfIDf terms for Snippet..."
## [1] "   Full TermMatrix:"
## <<DocumentTermMatrix (documents: 8402, terms: 13753)>>
## Non-/sparse entries: 104069/115448637
## Sparsity           : 100%
## Maximal term length: 25
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
## [1] "   Sparse TermMatrix:"
## <<DocumentTermMatrix (documents: 8402, terms: 21)>>
## Non-/sparse entries: 8172/168270
## Sparsity           : 95%
## Maximal term length: 7
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
```

```
## Warning in myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full",
## colorcol_name = "in.sprs"): converting in.sprs to class:factor
```

![](NYTBlogs_clusters_files/figure-html/extract.features-4.png) ![](NYTBlogs_clusters_files/figure-html/extract.features-5.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTBlogs_clusters_files/figure-html/extract.features-6.png) ![](NYTBlogs_clusters_files/figure-html/extract.features-7.png) 

```
## [1] "Reporting TfIDf terms for Abstract..."
## [1] "   Full TermMatrix:"
## <<DocumentTermMatrix (documents: 8402, terms: 13800)>>
## Non-/sparse entries: 103733/115843867
## Sparsity           : 100%
## Maximal term length: 112
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
## [1] "   Sparse TermMatrix:"
## <<DocumentTermMatrix (documents: 8402, terms: 16)>>
## Non-/sparse entries: 7079/127353
## Sparsity           : 95%
## Maximal term length: 7
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
```

```
## Warning in myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full",
## colorcol_name = "in.sprs"): converting in.sprs to class:factor
```

![](NYTBlogs_clusters_files/figure-html/extract.features-8.png) ![](NYTBlogs_clusters_files/figure-html/extract.features-9.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTBlogs_clusters_files/figure-html/extract.features-10.png) ![](NYTBlogs_clusters_files/figure-html/extract.features-11.png) 

```
##                         label step_major step_minor    bgn    end elapsed
## 5 extract.features_report.DTM          5          0 68.421 92.095  23.674
## 6   extract.features_bind.DTM          6          0 92.096     NA      NA
## [1] "Binding DTM for Headline..."
## [1] "Binding DTM for Snippet..."
## [1] "Binding DTM for Abstract..."
##                       label step_major step_minor    bgn    end elapsed
## 6 extract.features_bind.DTM          6          0 92.096 92.159   0.064
## 7 extract.features_bind.DXM          7          0 92.160     NA      NA
```

```
## Warning in rm(log_X_df, txt_X_df): object 'log_X_df' not found
```

![](NYTBlogs_clusters_files/figure-html/extract.features-12.png) 

```r
# print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

# print(myplot_scatter(glb_trnent_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus)

extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                       label step_major step_minor     bgn     end elapsed
## 7 extract.features_bind.DXM          7          0  92.160 121.043  28.884
## 8      extract.features_end          8          0 121.045      NA      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn     end
## 7           extract.features_bind.DXM          7          0 92.160 121.043
## 5         extract.features_report.DTM          5          0 68.421  92.095
## 4        extract.features_extract.DTM          4          0 54.719  68.420
## 3       extract.features_build.corpus          3          0 42.672  54.719
## 2 extract.features_factorize.str.vars          2          0 42.241  42.671
## 6           extract.features_bind.DTM          6          0 92.096  92.159
## 1                extract.features_bgn          1          0 42.232  42.240
##   elapsed duration
## 7  28.884   28.883
## 5  23.674   23.674
## 4  13.702   13.701
## 3  12.047   12.047
## 2   0.430    0.430
## 6   0.064    0.063
## 1   0.008    0.008
## [1] "Total Elapsed Time: 121.043 secs"
```

![](NYTBlogs_clusters_files/figure-html/extract.features-13.png) 

```r
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

![](NYTBlogs_clusters_files/figure-html/extract.features-14.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 6 extract.features          3          0  42.079 122.711  80.632
## 7     cluster.data          4          0 122.711      NA      NA
```

## Step `4.0: cluster data`

```r
require(proxy)
```

```
## Loading required package: proxy
## 
## Attaching package: 'proxy'
## 
## The following objects are masked from 'package:stats':
## 
##     as.dist, dist
## 
## The following object is masked from 'package:base':
## 
##     as.matrix
```

```r
require(dynamicTreeCut)
```

```
## Loading required package: dynamicTreeCut
```

```r
glb_entity_df$clusterid <- 1
if (glb_cluster) {
    
    for (myCategory in c("Science#Health#")) {
        ctgry_entity_df <- glb_entity_df[glb_entity_df$myCategory == myCategory, ]
        cluster_vars <- grep("[HSA]\\.T\\.", names(ctgry_entity_df), value=TRUE)
        
        dstns_dist <- dist(ctgry_entity_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_entity_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_entity_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        plot(clusters, hang=-1)
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        table(clusterGroups)
        clusterGroups[clusterGroups==0] = 1
        table(clusterGroups)    
        #summary(factor(clusterGroups))
    
        # add to glb_entity_df - then split the data again
        glb_entity_df[glb_entity_df$myCategory==myCategory,]$clusterid <- clusterGroups
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_entity_df,
        c("myCategory", "clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_entity_df$myCategory, glb_entity_df$clusterid, 
                                 glb_entity_df[, glb_rsp_var], 
                                 useNA="ifany"))
    dsp_obs(clusterid=18, cols=c("UniqueID", "Popular", "myCategory", "clusterid", "Headline"),
            all=TRUE)
    
    glb_entity_df$clusterid.fctr <- as.factor(glb_entity_df$clusterid)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, "clusterid")
}

# Re-partition
glb_trnent_df <- subset(glb_entity_df, .src == "Train")
glb_newent_df <- subset(glb_entity_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##             label step_major step_minor     bgn     end elapsed
## 7    cluster.data          4          0 122.711 125.885   3.174
## 8 select.features          5          0 125.885      NA      NA
```

## Step `5.0: select features`

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
##                                                        id         cor.y
## Popular                                           Popular  1.000000e+00
## A.nuppr.log                                   A.nuppr.log -2.720962e-01
## S.nuppr.log                                   S.nuppr.log -2.718459e-01
## WordCount.log                               WordCount.log  2.656836e-01
## WordCount                                       WordCount  2.575265e-01
## S.nwrds.unq.log                           S.nwrds.unq.log -2.343044e-01
## A.nwrds.unq.log                           A.nwrds.unq.log -2.261526e-01
## S.nchrs.log                                   S.nchrs.log -2.246930e-01
## A.nchrs.log                                   A.nchrs.log -2.245488e-01
## H.nwrds.unq.log                           H.nwrds.unq.log -1.957553e-01
## H.nchrs.log                                   H.nchrs.log -1.710624e-01
## H.nwrds.log                                   H.nwrds.log  1.404401e-01
## PubDate.hour.fctr                       PubDate.hour.fctr  1.354368e-01
## H.npnct19.log                               H.npnct19.log  1.283641e-01
## H.nuppr.log                                   H.nuppr.log -1.278085e-01
## S.nwrds.log                                   S.nwrds.log  1.262344e-01
## A.ndgts.log                                   A.ndgts.log -1.249484e-01
## S.ndgts.log                                   S.ndgts.log -1.242046e-01
## A.nwrds.log                                   A.nwrds.log  1.216578e-01
## H.ndgts.log                                   H.ndgts.log -1.196633e-01
## PubDate.wkend                               PubDate.wkend  1.067288e-01
## A.npnct11.log                               A.npnct11.log -9.183870e-02
## S.npnct11.log                               S.npnct11.log -9.158156e-02
## H.P.recap.colon                           H.P.recap.colon  9.008096e-02
## H.npnct28.log                               H.npnct28.log -8.917338e-02
## H.P.quandary                                 H.P.quandary  8.734922e-02
## S.T.week                                         S.T.week -8.627682e-02
## A.T.week                                         A.T.week -8.570201e-02
## A.T.fashion                                   A.T.fashion -8.349527e-02
## S.T.fashion                                   S.T.fashion -8.344900e-02
## H.npnct15.log                               H.npnct15.log -8.273237e-02
## H.T.fashion                                   H.T.fashion -8.000421e-02
## H.P.year.colon                             H.P.year.colon -7.842875e-02
## H.P.fashion.week                         H.P.fashion.week -7.632046e-02
## S.P.fashion.week                         S.P.fashion.week -7.080716e-02
## A.P.fashion.week                         A.P.fashion.week -7.080716e-02
## H.T.week                                         H.T.week -6.991953e-02
## A.T.photo                                       A.T.photo -6.984501e-02
## S.T.intern                                     S.T.intern -6.932606e-02
## A.npnct15.log                               A.npnct15.log -6.893301e-02
## S.T.tribun                                     S.T.tribun -6.880320e-02
## S.npnct15.log                               S.npnct15.log -6.770952e-02
## H.T.X2015                                       H.T.X2015 -6.570743e-02
## S.npnct04.log                               S.npnct04.log -6.294642e-02
## A.npnct04.log                               A.npnct04.log -6.294642e-02
## S.T.highlight                               S.T.highlight -6.283750e-02
## S.T.diari                                       S.T.diari -6.227998e-02
## H.npnct14.log                               H.npnct14.log -6.158577e-02
## S.T.scene                                       S.T.scene -6.098895e-02
## A.T.scene                                       A.T.scene -6.091747e-02
## H.P.no.comment.colon                 H.P.no.comment.colon  6.074669e-02
## A.T.intern                                     A.T.intern -6.065381e-02
## H.T.day                                           H.T.day -6.029849e-02
## S.T.newyork                                   S.T.newyork -5.943875e-02
## H.T.report                                     H.T.report -5.934795e-02
## H.T.daili                                       H.T.daili -5.852819e-02
## H.T.today                                       H.T.today -5.831308e-02
## A.T.newyork                                   A.T.newyork -5.769443e-02
## H.T.newyork                                   H.T.newyork -5.717575e-02
## S.T.time                                         S.T.time -5.574436e-02
## S.npnct19.log                               S.npnct19.log  5.503894e-02
## A.npnct19.log                               A.npnct19.log  5.482747e-02
## A.T.time                                         A.T.time -5.456053e-02
## S.T.articl                                     S.T.articl -5.446081e-02
## PubDate.last10                             PubDate.last10  5.398093e-02
## H.npnct08.log                               H.npnct08.log  5.375262e-02
## S.npnct13.log                               S.npnct13.log -5.332519e-02
## H.npnct04.log                               H.npnct04.log -5.126277e-02
## S.T.share                                       S.T.share -5.076046e-02
## A.npnct13.log                               A.npnct13.log -4.999563e-02
## PubDate.last10.log                     PubDate.last10.log  4.931702e-02
## H.T.busi                                         H.T.busi -4.901905e-02
## S.T.word                                         S.T.word -4.876372e-02
## A.T.editor                                     A.T.editor -4.875955e-02
## A.T.report                                     A.T.report -4.847952e-02
## H.T.morn                                         H.T.morn -4.838380e-02
## A.T.word                                         A.T.word -4.818014e-02
## S.T.report                                     S.T.report -4.746396e-02
## S.T.compani                                   S.T.compani -4.739476e-02
## A.T.compani                                   A.T.compani -4.732205e-02
## PubDate.last1.log                       PubDate.last1.log  4.635751e-02
## S.T.past                                         S.T.past -4.562700e-02
## H.T.X2014                                       H.T.X2014 -4.497745e-02
## S.T.first                                       S.T.first -4.494610e-02
## H.T.first                                       H.T.first -4.458885e-02
## H.P.readers.respond                   H.P.readers.respond  4.432886e-02
## H.T.news                                         H.T.news -4.415284e-02
## A.T.day                                           A.T.day -4.400791e-02
## H.P.daily.clip.report               H.P.daily.clip.report -4.388279e-02
## S.P.daily.clip.report               S.P.daily.clip.report -4.388279e-02
## A.P.daily.clip.report               A.P.daily.clip.report -4.388279e-02
## A.npnct28.log                               A.npnct28.log -4.373349e-02
## S.npnct28.log                               S.npnct28.log -4.370037e-02
## H.T.new                                           H.T.new -4.327803e-02
## H.P.first.draft                           H.P.first.draft -4.316253e-02
## S.T.photo                                       S.T.photo -4.269774e-02
## H.P.today.in.smallbusiness     H.P.today.in.smallbusiness -4.243051e-02
## S.T.show                                         S.T.show -4.218975e-02
## S.T.day                                           S.T.day -4.209616e-02
## A.T.senat                                       A.T.senat -4.140312e-02
## H.T.pictur                                     H.T.pictur -3.993172e-02
## PubDate.last100                           PubDate.last100  3.989229e-02
## PubDate.wkday.fctr                     PubDate.wkday.fctr -3.980129e-02
## A.T.appear                                     A.T.appear -3.949035e-02
## S.T.will                                         S.T.will -3.931556e-02
## S.T.make                                         S.T.make  3.924228e-02
## A.T.will                                         A.T.will -3.859405e-02
## H.P.what.we.are                           H.P.what.we.are -3.775209e-02
## A.npnct12.log                               A.npnct12.log -3.760012e-02
## H.P.today.in.politic                 H.P.today.in.politic -3.733661e-02
## S.npnct12.log                               S.npnct12.log -3.638891e-02
## PubDate.last1                               PubDate.last1  3.592267e-02
## A.T.year                                         A.T.year -3.497471e-02
## H.T.read                                         H.T.read -3.467043e-02
## PubDate.minute.fctr                   PubDate.minute.fctr -3.407385e-02
## H.T.get                                           H.T.get  3.312638e-02
## H.T.art                                           H.T.art -3.280483e-02
## H.P.verbatim.colon                     H.P.verbatim.colon -3.194363e-02
## H.npnct06.log                               H.npnct06.log  3.190718e-02
## H.T.china                                       H.T.china -3.144808e-02
## S.npnct01.log                               S.npnct01.log  3.093101e-02
## A.npnct01.log                               A.npnct01.log  3.093101e-02
## H.T.polit                                       H.T.polit -3.062866e-02
## S.T.can                                           S.T.can  3.062115e-02
## H.npnct16.log                               H.npnct16.log  3.039622e-02
## A.T.can                                           A.T.can  3.032288e-02
## H.T.billion                                   H.T.billion -2.949817e-02
## S.P.metropolitan.diary.colon S.P.metropolitan.diary.colon -2.841404e-02
## A.P.metropolitan.diary.colon A.P.metropolitan.diary.colon -2.841404e-02
## S.T.new                                           S.T.new -2.833575e-02
## A.T.new                                           A.T.new -2.779223e-02
## S.npnct21.log                               S.npnct21.log  2.760321e-02
## S.npnct23.log                               S.npnct23.log  2.760321e-02
## H.T.ebola                                       H.T.ebola  2.728582e-02
## H.T.deal                                         H.T.deal -2.559418e-02
## H.npnct13.log                               H.npnct13.log -2.524770e-02
## A.npnct14.log                               A.npnct14.log -2.407715e-02
## S.npnct06.log                               S.npnct06.log -2.389145e-02
## A.npnct06.log                               A.npnct06.log -2.389145e-02
## S.T.take                                         S.T.take -2.307456e-02
## A.T.take                                         A.T.take -2.287870e-02
## H.npnct01.log                               H.npnct01.log  2.271577e-02
## H.P.on.this.day                           H.P.on.this.day -2.150663e-02
## S.P.first.draft                           S.P.first.draft -2.150663e-02
## A.P.first.draft                           A.P.first.draft -2.150663e-02
## S.npnct14.log                               S.npnct14.log -2.121844e-02
## H.T.test                                         H.T.test -2.057181e-02
## A.T.obama                                       A.T.obama -2.020436e-02
## H.npnct02.log                               H.npnct02.log -2.001851e-02
## S.T.obama                                       S.T.obama -1.933422e-02
## S.npnct20.log                               S.npnct20.log -1.923169e-02
## A.npnct20.log                               A.npnct20.log -1.923169e-02
## PubDate.month.fctr                     PubDate.month.fctr  1.914874e-02
## A.T.said                                         A.T.said  1.831840e-02
## S.T.said                                         S.T.said  1.801374e-02
## S.P.year.colon                             S.P.year.colon -1.755336e-02
## A.P.year.colon                             A.P.year.colon -1.755336e-02
## PubDate.POSIX                               PubDate.POSIX  1.568326e-02
## PubDate.zoo                                   PubDate.zoo  1.568326e-02
## A.npnct21.log                               A.npnct21.log  1.537569e-02
## A.npnct23.log                               A.npnct23.log  1.537569e-02
## A.npnct17.log                               A.npnct17.log -1.457558e-02
## A.npnct02.log                               A.npnct02.log -1.451467e-02
## H.T.make                                         H.T.make  1.430572e-02
## H.T.big                                           H.T.big -1.390748e-02
## H.T.word                                         H.T.word -1.382927e-02
## A.npnct03.log                               A.npnct03.log -1.359260e-02
## H.npnct11.log                               H.npnct11.log  1.333613e-02
## H.npnct12.log                               H.npnct12.log -1.305305e-02
## A.P.http                                         A.P.http -1.294748e-02
## A.npnct18.log                               A.npnct18.log -1.271661e-02
## S.npnct03.log                               S.npnct03.log -1.240734e-02
## myCategory.fctr                           myCategory.fctr  1.234541e-02
## S.npnct07.log                               S.npnct07.log -1.214357e-02
## A.npnct07.log                               A.npnct07.log -1.214357e-02
## H.npnct07.log                               H.npnct07.log -1.201741e-02
## PubDate.second.fctr                   PubDate.second.fctr -1.187946e-02
## UniqueID                                         UniqueID  1.182492e-02
## PubDate.date.fctr                       PubDate.date.fctr -1.164756e-02
## A.T.one                                           A.T.one  1.048320e-02
## A.T.state                                       A.T.state  1.007193e-02
## H.T.bank                                         H.T.bank -9.989139e-03
## H.T.say                                           H.T.say -9.960773e-03
## H.T.obama                                       H.T.obama -9.907543e-03
## S.T.state                                       S.T.state  9.894250e-03
## H.npnct05.log                               H.npnct05.log -9.653967e-03
## S.T.one                                           S.T.one  9.592876e-03
## H.npnct03.log                               H.npnct03.log  9.533020e-03
## .rnorm                                             .rnorm -8.244230e-03
## H.P.s.notebook                             H.P.s.notebook  7.755542e-03
## PubDate.last100.log                   PubDate.last100.log -7.663322e-03
## H.npnct10.log                               H.npnct10.log -5.547032e-03
## H.npnct20.log                               H.npnct20.log -5.547032e-03
## S.npnct02.log                               S.npnct02.log -5.547032e-03
## S.npnct10.log                               S.npnct10.log -5.547032e-03
## A.npnct10.log                               A.npnct10.log -5.547032e-03
## A.npnct25.log                               A.npnct25.log -5.547032e-03
## A.npnct08.log                               A.npnct08.log -4.193476e-03
## S.npnct08.log                               S.npnct08.log -3.372706e-03
## S.T.presid                                     S.T.presid -2.581591e-03
## H.T.time                                         H.T.time -2.527450e-03
## S.npnct16.log                               S.npnct16.log -1.587454e-03
## A.npnct16.log                               A.npnct16.log -1.587454e-03
## H.T.take                                         H.T.take -9.276608e-04
## H.npnct24.log                               H.npnct24.log -9.890046e-19
## S.npnct24.log                               S.npnct24.log -9.890046e-19
## A.npnct24.log                               A.npnct24.log -9.890046e-19
## H.npnct09.log                               H.npnct09.log            NA
## H.npnct17.log                               H.npnct17.log            NA
## H.npnct18.log                               H.npnct18.log            NA
## H.npnct21.log                               H.npnct21.log            NA
## H.npnct22.log                               H.npnct22.log            NA
## H.npnct23.log                               H.npnct23.log            NA
## H.npnct25.log                               H.npnct25.log            NA
## H.npnct26.log                               H.npnct26.log            NA
## H.npnct27.log                               H.npnct27.log            NA
## H.npnct29.log                               H.npnct29.log            NA
## H.npnct30.log                               H.npnct30.log            NA
## H.P.http                                         H.P.http            NA
## S.npnct05.log                               S.npnct05.log            NA
## S.npnct09.log                               S.npnct09.log            NA
## S.npnct17.log                               S.npnct17.log            NA
## S.npnct18.log                               S.npnct18.log            NA
## S.npnct22.log                               S.npnct22.log            NA
## S.npnct25.log                               S.npnct25.log            NA
## S.npnct26.log                               S.npnct26.log            NA
## S.npnct27.log                               S.npnct27.log            NA
## S.npnct29.log                               S.npnct29.log            NA
## S.npnct30.log                               S.npnct30.log            NA
## S.P.http                                         S.P.http            NA
## A.npnct05.log                               A.npnct05.log            NA
## A.npnct09.log                               A.npnct09.log            NA
## A.npnct22.log                               A.npnct22.log            NA
## A.npnct26.log                               A.npnct26.log            NA
## A.npnct27.log                               A.npnct27.log            NA
## A.npnct29.log                               A.npnct29.log            NA
## A.npnct30.log                               A.npnct30.log            NA
## clusterid                                       clusterid            NA
## PubDate.year.fctr                       PubDate.year.fctr            NA
##                              exclude.as.feat    cor.y.abs
## Popular                                    1 1.000000e+00
## A.nuppr.log                                0 2.720962e-01
## S.nuppr.log                                0 2.718459e-01
## WordCount.log                              0 2.656836e-01
## WordCount                                  1 2.575265e-01
## S.nwrds.unq.log                            0 2.343044e-01
## A.nwrds.unq.log                            0 2.261526e-01
## S.nchrs.log                                0 2.246930e-01
## A.nchrs.log                                0 2.245488e-01
## H.nwrds.unq.log                            0 1.957553e-01
## H.nchrs.log                                0 1.710624e-01
## H.nwrds.log                                0 1.404401e-01
## PubDate.hour.fctr                          0 1.354368e-01
## H.npnct19.log                              0 1.283641e-01
## H.nuppr.log                                0 1.278085e-01
## S.nwrds.log                                0 1.262344e-01
## A.ndgts.log                                0 1.249484e-01
## S.ndgts.log                                0 1.242046e-01
## A.nwrds.log                                0 1.216578e-01
## H.ndgts.log                                0 1.196633e-01
## PubDate.wkend                              0 1.067288e-01
## A.npnct11.log                              0 9.183870e-02
## S.npnct11.log                              0 9.158156e-02
## H.P.recap.colon                            0 9.008096e-02
## H.npnct28.log                              0 8.917338e-02
## H.P.quandary                               0 8.734922e-02
## S.T.week                                   0 8.627682e-02
## A.T.week                                   0 8.570201e-02
## A.T.fashion                                0 8.349527e-02
## S.T.fashion                                0 8.344900e-02
## H.npnct15.log                              0 8.273237e-02
## H.T.fashion                                0 8.000421e-02
## H.P.year.colon                             0 7.842875e-02
## H.P.fashion.week                           0 7.632046e-02
## S.P.fashion.week                           0 7.080716e-02
## A.P.fashion.week                           0 7.080716e-02
## H.T.week                                   0 6.991953e-02
## A.T.photo                                  0 6.984501e-02
## S.T.intern                                 0 6.932606e-02
## A.npnct15.log                              0 6.893301e-02
## S.T.tribun                                 0 6.880320e-02
## S.npnct15.log                              0 6.770952e-02
## H.T.X2015                                  0 6.570743e-02
## S.npnct04.log                              0 6.294642e-02
## A.npnct04.log                              0 6.294642e-02
## S.T.highlight                              0 6.283750e-02
## S.T.diari                                  0 6.227998e-02
## H.npnct14.log                              0 6.158577e-02
## S.T.scene                                  0 6.098895e-02
## A.T.scene                                  0 6.091747e-02
## H.P.no.comment.colon                       0 6.074669e-02
## A.T.intern                                 0 6.065381e-02
## H.T.day                                    0 6.029849e-02
## S.T.newyork                                0 5.943875e-02
## H.T.report                                 0 5.934795e-02
## H.T.daili                                  0 5.852819e-02
## H.T.today                                  0 5.831308e-02
## A.T.newyork                                0 5.769443e-02
## H.T.newyork                                0 5.717575e-02
## S.T.time                                   0 5.574436e-02
## S.npnct19.log                              0 5.503894e-02
## A.npnct19.log                              0 5.482747e-02
## A.T.time                                   0 5.456053e-02
## S.T.articl                                 0 5.446081e-02
## PubDate.last10                             1 5.398093e-02
## H.npnct08.log                              0 5.375262e-02
## S.npnct13.log                              0 5.332519e-02
## H.npnct04.log                              0 5.126277e-02
## S.T.share                                  0 5.076046e-02
## A.npnct13.log                              0 4.999563e-02
## PubDate.last10.log                         0 4.931702e-02
## H.T.busi                                   0 4.901905e-02
## S.T.word                                   0 4.876372e-02
## A.T.editor                                 0 4.875955e-02
## A.T.report                                 0 4.847952e-02
## H.T.morn                                   0 4.838380e-02
## A.T.word                                   0 4.818014e-02
## S.T.report                                 0 4.746396e-02
## S.T.compani                                0 4.739476e-02
## A.T.compani                                0 4.732205e-02
## PubDate.last1.log                          0 4.635751e-02
## S.T.past                                   0 4.562700e-02
## H.T.X2014                                  0 4.497745e-02
## S.T.first                                  0 4.494610e-02
## H.T.first                                  0 4.458885e-02
## H.P.readers.respond                        0 4.432886e-02
## H.T.news                                   0 4.415284e-02
## A.T.day                                    0 4.400791e-02
## H.P.daily.clip.report                      0 4.388279e-02
## S.P.daily.clip.report                      0 4.388279e-02
## A.P.daily.clip.report                      0 4.388279e-02
## A.npnct28.log                              0 4.373349e-02
## S.npnct28.log                              0 4.370037e-02
## H.T.new                                    0 4.327803e-02
## H.P.first.draft                            0 4.316253e-02
## S.T.photo                                  0 4.269774e-02
## H.P.today.in.smallbusiness                 0 4.243051e-02
## S.T.show                                   0 4.218975e-02
## S.T.day                                    0 4.209616e-02
## A.T.senat                                  0 4.140312e-02
## H.T.pictur                                 0 3.993172e-02
## PubDate.last100                            1 3.989229e-02
## PubDate.wkday.fctr                         0 3.980129e-02
## A.T.appear                                 0 3.949035e-02
## S.T.will                                   0 3.931556e-02
## S.T.make                                   0 3.924228e-02
## A.T.will                                   0 3.859405e-02
## H.P.what.we.are                            0 3.775209e-02
## A.npnct12.log                              0 3.760012e-02
## H.P.today.in.politic                       0 3.733661e-02
## S.npnct12.log                              0 3.638891e-02
## PubDate.last1                              1 3.592267e-02
## A.T.year                                   0 3.497471e-02
## H.T.read                                   0 3.467043e-02
## PubDate.minute.fctr                        0 3.407385e-02
## H.T.get                                    0 3.312638e-02
## H.T.art                                    0 3.280483e-02
## H.P.verbatim.colon                         0 3.194363e-02
## H.npnct06.log                              0 3.190718e-02
## H.T.china                                  0 3.144808e-02
## S.npnct01.log                              0 3.093101e-02
## A.npnct01.log                              0 3.093101e-02
## H.T.polit                                  0 3.062866e-02
## S.T.can                                    0 3.062115e-02
## H.npnct16.log                              0 3.039622e-02
## A.T.can                                    0 3.032288e-02
## H.T.billion                                0 2.949817e-02
## S.P.metropolitan.diary.colon               0 2.841404e-02
## A.P.metropolitan.diary.colon               0 2.841404e-02
## S.T.new                                    0 2.833575e-02
## A.T.new                                    0 2.779223e-02
## S.npnct21.log                              0 2.760321e-02
## S.npnct23.log                              0 2.760321e-02
## H.T.ebola                                  0 2.728582e-02
## H.T.deal                                   0 2.559418e-02
## H.npnct13.log                              0 2.524770e-02
## A.npnct14.log                              0 2.407715e-02
## S.npnct06.log                              0 2.389145e-02
## A.npnct06.log                              0 2.389145e-02
## S.T.take                                   0 2.307456e-02
## A.T.take                                   0 2.287870e-02
## H.npnct01.log                              0 2.271577e-02
## H.P.on.this.day                            0 2.150663e-02
## S.P.first.draft                            0 2.150663e-02
## A.P.first.draft                            0 2.150663e-02
## S.npnct14.log                              0 2.121844e-02
## H.T.test                                   0 2.057181e-02
## A.T.obama                                  0 2.020436e-02
## H.npnct02.log                              0 2.001851e-02
## S.T.obama                                  0 1.933422e-02
## S.npnct20.log                              0 1.923169e-02
## A.npnct20.log                              0 1.923169e-02
## PubDate.month.fctr                         1 1.914874e-02
## A.T.said                                   0 1.831840e-02
## S.T.said                                   0 1.801374e-02
## S.P.year.colon                             0 1.755336e-02
## A.P.year.colon                             0 1.755336e-02
## PubDate.POSIX                              1 1.568326e-02
## PubDate.zoo                                1 1.568326e-02
## A.npnct21.log                              0 1.537569e-02
## A.npnct23.log                              0 1.537569e-02
## A.npnct17.log                              0 1.457558e-02
## A.npnct02.log                              0 1.451467e-02
## H.T.make                                   0 1.430572e-02
## H.T.big                                    0 1.390748e-02
## H.T.word                                   0 1.382927e-02
## A.npnct03.log                              0 1.359260e-02
## H.npnct11.log                              0 1.333613e-02
## H.npnct12.log                              0 1.305305e-02
## A.P.http                                   0 1.294748e-02
## A.npnct18.log                              0 1.271661e-02
## S.npnct03.log                              0 1.240734e-02
## myCategory.fctr                            0 1.234541e-02
## S.npnct07.log                              0 1.214357e-02
## A.npnct07.log                              0 1.214357e-02
## H.npnct07.log                              0 1.201741e-02
## PubDate.second.fctr                        0 1.187946e-02
## UniqueID                                   1 1.182492e-02
## PubDate.date.fctr                          0 1.164756e-02
## A.T.one                                    0 1.048320e-02
## A.T.state                                  0 1.007193e-02
## H.T.bank                                   0 9.989139e-03
## H.T.say                                    0 9.960773e-03
## H.T.obama                                  0 9.907543e-03
## S.T.state                                  0 9.894250e-03
## H.npnct05.log                              0 9.653967e-03
## S.T.one                                    0 9.592876e-03
## H.npnct03.log                              0 9.533020e-03
## .rnorm                                     0 8.244230e-03
## H.P.s.notebook                             0 7.755542e-03
## PubDate.last100.log                        0 7.663322e-03
## H.npnct10.log                              0 5.547032e-03
## H.npnct20.log                              0 5.547032e-03
## S.npnct02.log                              0 5.547032e-03
## S.npnct10.log                              0 5.547032e-03
## A.npnct10.log                              0 5.547032e-03
## A.npnct25.log                              0 5.547032e-03
## A.npnct08.log                              0 4.193476e-03
## S.npnct08.log                              0 3.372706e-03
## S.T.presid                                 0 2.581591e-03
## H.T.time                                   0 2.527450e-03
## S.npnct16.log                              0 1.587454e-03
## A.npnct16.log                              0 1.587454e-03
## H.T.take                                   0 9.276608e-04
## H.npnct24.log                              0 9.890046e-19
## S.npnct24.log                              0 9.890046e-19
## A.npnct24.log                              0 9.890046e-19
## H.npnct09.log                              0           NA
## H.npnct17.log                              0           NA
## H.npnct18.log                              0           NA
## H.npnct21.log                              0           NA
## H.npnct22.log                              0           NA
## H.npnct23.log                              0           NA
## H.npnct25.log                              0           NA
## H.npnct26.log                              0           NA
## H.npnct27.log                              0           NA
## H.npnct29.log                              0           NA
## H.npnct30.log                              0           NA
## H.P.http                                   0           NA
## S.npnct05.log                              0           NA
## S.npnct09.log                              0           NA
## S.npnct17.log                              0           NA
## S.npnct18.log                              0           NA
## S.npnct22.log                              0           NA
## S.npnct25.log                              0           NA
## S.npnct26.log                              0           NA
## S.npnct27.log                              0           NA
## S.npnct29.log                              0           NA
## S.npnct30.log                              0           NA
## S.P.http                                   0           NA
## A.npnct05.log                              0           NA
## A.npnct09.log                              0           NA
## A.npnct22.log                              0           NA
## A.npnct26.log                              0           NA
## A.npnct27.log                              0           NA
## A.npnct29.log                              0           NA
## A.npnct30.log                              0           NA
## clusterid                                  0           NA
## PubDate.year.fctr                          0           NA
```

```r
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
## [1] "cor(A.npnct20.log, S.npnct20.log)=1.0000"
## [1] "cor(Popular.fctr, A.npnct20.log)=-0.0192"
## [1] "cor(Popular.fctr, S.npnct20.log)=-0.0192"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct20.log as highly correlated with
## A.npnct20.log
```

```
## [1] "cor(A.npnct21.log, A.npnct23.log)=1.0000"
## [1] "cor(Popular.fctr, A.npnct21.log)=0.0154"
## [1] "cor(Popular.fctr, A.npnct23.log)=0.0154"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct23.log as highly correlated with
## A.npnct21.log
```

```
## [1] "cor(A.P.daily.clip.report, H.P.daily.clip.report)=1.0000"
## [1] "cor(Popular.fctr, A.P.daily.clip.report)=-0.0439"
## [1] "cor(Popular.fctr, H.P.daily.clip.report)=-0.0439"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.P.daily.clip.report as highly correlated
## with A.P.daily.clip.report
```

```
## [1] "cor(A.P.daily.clip.report, S.P.daily.clip.report)=1.0000"
## [1] "cor(Popular.fctr, A.P.daily.clip.report)=-0.0439"
## [1] "cor(Popular.fctr, S.P.daily.clip.report)=-0.0439"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.P.daily.clip.report as highly correlated
## with A.P.daily.clip.report
```

```
## [1] "cor(A.P.fashion.week, S.P.fashion.week)=1.0000"
## [1] "cor(Popular.fctr, A.P.fashion.week)=-0.0708"
## [1] "cor(Popular.fctr, S.P.fashion.week)=-0.0708"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.P.fashion.week as highly correlated with
## A.P.fashion.week
```

```
## [1] "cor(A.P.first.draft, S.P.first.draft)=1.0000"
## [1] "cor(Popular.fctr, A.P.first.draft)=-0.0215"
## [1] "cor(Popular.fctr, S.P.first.draft)=-0.0215"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.P.first.draft as highly correlated with
## A.P.first.draft
```

```
## [1] "cor(A.P.metropolitan.diary.colon, S.P.metropolitan.diary.colon)=1.0000"
## [1] "cor(Popular.fctr, A.P.metropolitan.diary.colon)=-0.0284"
## [1] "cor(Popular.fctr, S.P.metropolitan.diary.colon)=-0.0284"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.P.metropolitan.diary.colon as highly
## correlated with A.P.metropolitan.diary.colon
```

```
## [1] "cor(A.P.year.colon, S.P.year.colon)=1.0000"
## [1] "cor(Popular.fctr, A.P.year.colon)=-0.0176"
## [1] "cor(Popular.fctr, S.P.year.colon)=-0.0176"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.P.year.colon as highly correlated with
## A.P.year.colon
```

```
## [1] "cor(S.npnct21.log, S.npnct23.log)=1.0000"
## [1] "cor(Popular.fctr, S.npnct21.log)=0.0276"
## [1] "cor(Popular.fctr, S.npnct23.log)=0.0276"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct23.log as highly correlated with
## S.npnct21.log
```

```
## [1] "cor(A.npnct11.log, S.npnct11.log)=0.9997"
## [1] "cor(Popular.fctr, A.npnct11.log)=-0.0918"
## [1] "cor(Popular.fctr, S.npnct11.log)=-0.0916"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct11.log as highly correlated with
## A.npnct11.log
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
## [1] "cor(A.T.can, S.T.can)=0.9990"
## [1] "cor(Popular.fctr, A.T.can)=0.0303"
## [1] "cor(Popular.fctr, S.T.can)=0.0306"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.can as highly correlated with S.T.can
```

```
## [1] "cor(A.T.scene, S.T.scene)=0.9990"
## [1] "cor(Popular.fctr, A.T.scene)=-0.0609"
## [1] "cor(Popular.fctr, S.T.scene)=-0.0610"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.scene as highly correlated with S.T.scene
```

```
## [1] "cor(A.T.state, S.T.state)=0.9989"
## [1] "cor(Popular.fctr, A.T.state)=0.0101"
## [1] "cor(Popular.fctr, S.T.state)=0.0099"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.state as highly correlated with A.T.state
```

```
## [1] "cor(A.npnct28.log, S.npnct28.log)=0.9989"
## [1] "cor(Popular.fctr, A.npnct28.log)=-0.0437"
## [1] "cor(Popular.fctr, S.npnct28.log)=-0.0437"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct28.log as highly correlated with
## A.npnct28.log
```

```
## [1] "cor(A.T.fashion, S.T.fashion)=0.9989"
## [1] "cor(Popular.fctr, A.T.fashion)=-0.0835"
## [1] "cor(Popular.fctr, S.T.fashion)=-0.0834"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.fashion as highly correlated with
## A.T.fashion
```

```
## [1] "cor(A.T.compani, S.T.compani)=0.9988"
## [1] "cor(Popular.fctr, A.T.compani)=-0.0473"
## [1] "cor(Popular.fctr, S.T.compani)=-0.0474"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.compani as highly correlated with
## S.T.compani
```

```
## [1] "cor(A.T.said, S.T.said)=0.9988"
## [1] "cor(Popular.fctr, A.T.said)=0.0183"
## [1] "cor(Popular.fctr, S.T.said)=0.0180"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.said as highly correlated with A.T.said
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
## [1] "cor(A.T.take, S.T.take)=0.9985"
## [1] "cor(Popular.fctr, A.T.take)=-0.0229"
## [1] "cor(Popular.fctr, S.T.take)=-0.0231"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.take as highly correlated with S.T.take
```

```
## [1] "cor(A.T.report, S.T.report)=0.9983"
## [1] "cor(Popular.fctr, A.T.report)=-0.0485"
## [1] "cor(Popular.fctr, S.T.report)=-0.0475"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.report as highly correlated with
## A.T.report
```

```
## [1] "cor(A.T.will, S.T.will)=0.9981"
## [1] "cor(Popular.fctr, A.T.will)=-0.0386"
## [1] "cor(Popular.fctr, S.T.will)=-0.0393"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.will as highly correlated with S.T.will
```

```
## [1] "cor(A.T.week, S.T.week)=0.9980"
## [1] "cor(Popular.fctr, A.T.week)=-0.0857"
## [1] "cor(Popular.fctr, S.T.week)=-0.0863"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.week as highly correlated with S.T.week
```

```
## [1] "cor(A.T.one, S.T.one)=0.9975"
## [1] "cor(Popular.fctr, A.T.one)=0.0105"
## [1] "cor(Popular.fctr, S.T.one)=0.0096"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.one as highly correlated with A.T.one
```

```
## [1] "cor(A.T.day, S.T.day)=0.9967"
## [1] "cor(Popular.fctr, A.T.day)=-0.0440"
## [1] "cor(Popular.fctr, S.T.day)=-0.0421"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.day as highly correlated with A.T.day
```

```
## [1] "cor(A.T.obama, S.T.obama)=0.9957"
## [1] "cor(Popular.fctr, A.T.obama)=-0.0202"
## [1] "cor(Popular.fctr, S.T.obama)=-0.0193"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.obama as highly correlated with A.T.obama
```

```
## [1] "cor(A.npnct19.log, S.npnct19.log)=0.9957"
## [1] "cor(Popular.fctr, A.npnct19.log)=0.0548"
## [1] "cor(Popular.fctr, S.npnct19.log)=0.0550"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct19.log as highly correlated with
## S.npnct19.log
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
## [1] "cor(A.T.word, S.T.word)=0.9948"
## [1] "cor(Popular.fctr, A.T.word)=-0.0482"
## [1] "cor(Popular.fctr, S.T.word)=-0.0488"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.word as highly correlated with S.T.word
```

```
## [1] "cor(A.T.new, S.T.new)=0.9944"
## [1] "cor(Popular.fctr, A.T.new)=-0.0278"
## [1] "cor(Popular.fctr, S.T.new)=-0.0283"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.new as highly correlated with S.T.new
```

```
## [1] "cor(A.T.newyork, S.T.newyork)=0.9936"
## [1] "cor(Popular.fctr, A.T.newyork)=-0.0577"
## [1] "cor(Popular.fctr, S.T.newyork)=-0.0594"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.newyork as highly correlated with
## S.T.newyork
```

```
## [1] "cor(A.npnct12.log, S.npnct12.log)=0.9935"
## [1] "cor(Popular.fctr, A.npnct12.log)=-0.0376"
## [1] "cor(Popular.fctr, S.npnct12.log)=-0.0364"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct12.log as highly correlated with
## A.npnct12.log
```

```
## [1] "cor(A.T.time, S.T.time)=0.9921"
## [1] "cor(Popular.fctr, A.T.time)=-0.0546"
## [1] "cor(Popular.fctr, S.T.time)=-0.0557"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.time as highly correlated with S.T.time
```

```
## [1] "cor(A.npnct15.log, S.npnct15.log)=0.9917"
## [1] "cor(Popular.fctr, A.npnct15.log)=-0.0689"
## [1] "cor(Popular.fctr, S.npnct15.log)=-0.0677"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.npnct15.log as highly correlated with
## A.npnct15.log
```

```
## [1] "cor(A.nwrds.unq.log, S.nwrds.unq.log)=0.9904"
## [1] "cor(Popular.fctr, A.nwrds.unq.log)=-0.2262"
## [1] "cor(Popular.fctr, S.nwrds.unq.log)=-0.2343"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.nwrds.unq.log as highly correlated with
## S.nwrds.unq.log
```

```
## [1] "cor(A.npnct18.log, A.P.http)=0.9882"
## [1] "cor(Popular.fctr, A.npnct18.log)=-0.0127"
## [1] "cor(Popular.fctr, A.P.http)=-0.0129"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct18.log as highly correlated with
## A.P.http
```

```
## [1] "cor(A.nwrds.log, S.nwrds.log)=0.9879"
## [1] "cor(Popular.fctr, A.nwrds.log)=0.1217"
## [1] "cor(Popular.fctr, S.nwrds.log)=0.1262"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.nwrds.log as highly correlated with
## S.nwrds.log
```

```
## [1] "cor(A.npnct13.log, S.npnct13.log)=0.9795"
## [1] "cor(Popular.fctr, A.npnct13.log)=-0.0500"
## [1] "cor(Popular.fctr, S.npnct13.log)=-0.0533"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct13.log as highly correlated with
## S.npnct13.log
```

```
## [1] "cor(S.nchrs.log, S.nwrds.unq.log)=0.9502"
## [1] "cor(Popular.fctr, S.nchrs.log)=-0.2247"
## [1] "cor(Popular.fctr, S.nwrds.unq.log)=-0.2343"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.nchrs.log as highly correlated with
## S.nwrds.unq.log
```

```
## [1] "cor(A.P.daily.clip.report, H.T.daili)=0.9447"
## [1] "cor(Popular.fctr, A.P.daily.clip.report)=-0.0439"
## [1] "cor(Popular.fctr, H.T.daili)=-0.0585"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.P.daily.clip.report as highly correlated
## with H.T.daili
```

```
## [1] "cor(A.T.intern, S.T.intern)=0.9349"
## [1] "cor(Popular.fctr, A.T.intern)=-0.0607"
## [1] "cor(Popular.fctr, S.T.intern)=-0.0693"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.intern as highly correlated with
## S.T.intern
```

```
## [1] "cor(S.T.intern, S.T.tribun)=0.9348"
## [1] "cor(Popular.fctr, S.T.intern)=-0.0693"
## [1] "cor(Popular.fctr, S.T.tribun)=-0.0688"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.tribun as highly correlated with
## S.T.intern
```

```
## [1] "cor(A.npnct02.log, A.P.http)=0.9261"
## [1] "cor(Popular.fctr, A.npnct02.log)=-0.0145"
## [1] "cor(Popular.fctr, A.P.http)=-0.0129"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.P.http as highly correlated with
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
## [1] "cor(H.T.daili, H.T.report)=0.9084"
## [1] "cor(Popular.fctr, H.T.daili)=-0.0585"
## [1] "cor(Popular.fctr, H.T.report)=-0.0593"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.T.daili as highly correlated with H.T.report
```

```
## [1] "cor(H.npnct14.log, H.T.X2015)=0.9049"
## [1] "cor(Popular.fctr, H.npnct14.log)=-0.0616"
## [1] "cor(Popular.fctr, H.T.X2015)=-0.0657"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.npnct14.log as highly correlated with
## H.T.X2015
```

```
## [1] "cor(H.nchrs.log, H.nwrds.unq.log)=0.8950"
## [1] "cor(Popular.fctr, H.nchrs.log)=-0.1711"
## [1] "cor(Popular.fctr, H.nwrds.unq.log)=-0.1958"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.nchrs.log as highly correlated with
## H.nwrds.unq.log
```

```
## [1] "cor(S.T.diari, S.T.scene)=0.8767"
## [1] "cor(Popular.fctr, S.T.diari)=-0.0623"
## [1] "cor(Popular.fctr, S.T.scene)=-0.0610"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.scene as highly correlated with S.T.diari
```

```
## [1] "cor(A.npnct02.log, A.npnct17.log)=0.8745"
## [1] "cor(Popular.fctr, A.npnct02.log)=-0.0145"
## [1] "cor(Popular.fctr, A.npnct17.log)=-0.0146"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct02.log as highly correlated with
## A.npnct17.log
```

```
## [1] "cor(H.P.today.in.politic, H.T.polit)=0.8675"
## [1] "cor(Popular.fctr, H.P.today.in.politic)=-0.0373"
## [1] "cor(Popular.fctr, H.T.polit)=-0.0306"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.T.polit as highly correlated with
## H.P.today.in.politic
```

```
## [1] "cor(H.P.fashion.week, H.T.fashion)=0.8546"
## [1] "cor(Popular.fctr, H.P.fashion.week)=-0.0763"
## [1] "cor(Popular.fctr, H.T.fashion)=-0.0800"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.P.fashion.week as highly correlated with
## H.T.fashion
```

```
## [1] "cor(H.P.what.we.are, H.T.read)=0.8479"
## [1] "cor(Popular.fctr, H.P.what.we.are)=-0.0378"
## [1] "cor(Popular.fctr, H.T.read)=-0.0347"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.T.read as highly correlated with
## H.P.what.we.are
```

```
## [1] "cor(A.npnct28.log, H.T.morn)=0.8327"
## [1] "cor(Popular.fctr, A.npnct28.log)=-0.0437"
## [1] "cor(Popular.fctr, H.T.morn)=-0.0484"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct28.log as highly correlated with
## H.T.morn
```

```
## [1] "cor(H.nuppr.log, H.nwrds.unq.log)=0.8317"
## [1] "cor(Popular.fctr, H.nuppr.log)=-0.1278"
## [1] "cor(Popular.fctr, H.nwrds.unq.log)=-0.1958"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.nuppr.log as highly correlated with
## H.nwrds.unq.log
```

```
## [1] "cor(H.P.year.colon, S.T.intern)=0.8168"
## [1] "cor(Popular.fctr, H.P.year.colon)=-0.0784"
## [1] "cor(Popular.fctr, S.T.intern)=-0.0693"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.intern as highly correlated with
## H.P.year.colon
```

```
## [1] "cor(H.npnct06.log, H.npnct16.log)=0.8106"
## [1] "cor(Popular.fctr, H.npnct06.log)=0.0319"
## [1] "cor(Popular.fctr, H.npnct16.log)=0.0304"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.npnct16.log as highly correlated with
## H.npnct06.log
```

```
## [1] "cor(A.P.fashion.week, A.T.fashion)=0.8084"
## [1] "cor(Popular.fctr, A.P.fashion.week)=-0.0708"
## [1] "cor(Popular.fctr, A.T.fashion)=-0.0835"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.P.fashion.week as highly correlated with
## A.T.fashion
```

```
## [1] "cor(H.P.today.in.politic, H.T.today)=0.8045"
## [1] "cor(Popular.fctr, H.P.today.in.politic)=-0.0373"
## [1] "cor(Popular.fctr, H.T.today)=-0.0583"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.P.today.in.politic as highly correlated with
## H.T.today
```

```
## [1] "cor(H.T.word, S.T.past)=0.7901"
## [1] "cor(Popular.fctr, H.T.word)=-0.0138"
## [1] "cor(Popular.fctr, S.T.past)=-0.0456"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.T.word as highly correlated with S.T.past
```

```
## [1] "cor(H.T.X2015, S.T.diari)=0.7863"
## [1] "cor(Popular.fctr, H.T.X2015)=-0.0657"
## [1] "cor(Popular.fctr, S.T.diari)=-0.0623"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.diari as highly correlated with H.T.X2015
```

```
## [1] "cor(A.npnct14.log, A.npnct17.log)=0.7663"
## [1] "cor(Popular.fctr, A.npnct14.log)=-0.0241"
## [1] "cor(Popular.fctr, A.npnct17.log)=-0.0146"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct17.log as highly correlated with
## A.npnct14.log
```

```
## [1] "cor(A.T.fashion, H.T.X2015)=0.7588"
## [1] "cor(Popular.fctr, A.T.fashion)=-0.0835"
## [1] "cor(Popular.fctr, H.T.X2015)=-0.0657"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.T.X2015 as highly correlated with
## A.T.fashion
```

```
## [1] "cor(A.npnct21.log, S.npnct21.log)=0.7461"
## [1] "cor(Popular.fctr, A.npnct21.log)=0.0154"
## [1] "cor(Popular.fctr, S.npnct21.log)=0.0276"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.npnct21.log as highly correlated with
## S.npnct21.log
```

```
## [1] "cor(A.T.appear, S.T.past)=0.7410"
## [1] "cor(Popular.fctr, A.T.appear)=-0.0395"
## [1] "cor(Popular.fctr, S.T.past)=-0.0456"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.T.appear as highly correlated with S.T.past
```

```
## [1] "cor(H.P.first.draft, H.T.first)=0.7287"
## [1] "cor(Popular.fctr, H.P.first.draft)=-0.0432"
## [1] "cor(Popular.fctr, H.T.first)=-0.0446"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.P.first.draft as highly correlated with
## H.T.first
```

```
## [1] "cor(H.T.pictur, S.T.photo)=0.7198"
## [1] "cor(Popular.fctr, H.T.pictur)=-0.0399"
## [1] "cor(Popular.fctr, S.T.photo)=-0.0427"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.T.pictur as highly correlated with S.T.photo
```

```
## [1] "cor(S.T.articl, S.T.past)=0.7197"
## [1] "cor(Popular.fctr, S.T.articl)=-0.0545"
## [1] "cor(Popular.fctr, S.T.past)=-0.0456"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.past as highly correlated with S.T.articl
```

```
## [1] "cor(A.T.photo, S.T.photo)=0.7121"
## [1] "cor(Popular.fctr, A.T.photo)=-0.0698"
## [1] "cor(Popular.fctr, S.T.photo)=-0.0427"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.T.photo as highly correlated with A.T.photo
```

```
## [1] "cor(H.npnct04.log, H.T.billion)=0.7010"
## [1] "cor(Popular.fctr, H.npnct04.log)=-0.0513"
## [1] "cor(Popular.fctr, H.T.billion)=-0.0295"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.T.billion as highly correlated with
## H.npnct04.log
```

```
##                                                        id         cor.y
## Popular                                           Popular  1.000000e+00
## WordCount.log                               WordCount.log  2.656836e-01
## WordCount                                       WordCount  2.575265e-01
## H.nwrds.log                                   H.nwrds.log  1.404401e-01
## PubDate.hour.fctr                       PubDate.hour.fctr  1.354368e-01
## H.npnct19.log                               H.npnct19.log  1.283641e-01
## S.nwrds.log                                   S.nwrds.log  1.262344e-01
## A.nwrds.log                                   A.nwrds.log  1.216578e-01
## PubDate.wkend                               PubDate.wkend  1.067288e-01
## H.P.recap.colon                           H.P.recap.colon  9.008096e-02
## H.P.quandary                                 H.P.quandary  8.734922e-02
## H.P.no.comment.colon                 H.P.no.comment.colon  6.074669e-02
## S.npnct19.log                               S.npnct19.log  5.503894e-02
## A.npnct19.log                               A.npnct19.log  5.482747e-02
## PubDate.last10                             PubDate.last10  5.398093e-02
## H.npnct08.log                               H.npnct08.log  5.375262e-02
## PubDate.last10.log                     PubDate.last10.log  4.931702e-02
## PubDate.last1.log                       PubDate.last1.log  4.635751e-02
## H.P.readers.respond                   H.P.readers.respond  4.432886e-02
## PubDate.last100                           PubDate.last100  3.989229e-02
## S.T.make                                         S.T.make  3.924228e-02
## PubDate.last1                               PubDate.last1  3.592267e-02
## H.T.get                                           H.T.get  3.312638e-02
## H.npnct06.log                               H.npnct06.log  3.190718e-02
## A.npnct01.log                               A.npnct01.log  3.093101e-02
## S.npnct01.log                               S.npnct01.log  3.093101e-02
## S.T.can                                           S.T.can  3.062115e-02
## H.npnct16.log                               H.npnct16.log  3.039622e-02
## A.T.can                                           A.T.can  3.032288e-02
## S.npnct21.log                               S.npnct21.log  2.760321e-02
## S.npnct23.log                               S.npnct23.log  2.760321e-02
## H.T.ebola                                       H.T.ebola  2.728582e-02
## H.npnct01.log                               H.npnct01.log  2.271577e-02
## PubDate.month.fctr                     PubDate.month.fctr  1.914874e-02
## A.T.said                                         A.T.said  1.831840e-02
## S.T.said                                         S.T.said  1.801374e-02
## PubDate.POSIX                               PubDate.POSIX  1.568326e-02
## PubDate.zoo                                   PubDate.zoo  1.568326e-02
## A.npnct21.log                               A.npnct21.log  1.537569e-02
## A.npnct23.log                               A.npnct23.log  1.537569e-02
## H.T.make                                         H.T.make  1.430572e-02
## H.npnct11.log                               H.npnct11.log  1.333613e-02
## myCategory.fctr                           myCategory.fctr  1.234541e-02
## UniqueID                                         UniqueID  1.182492e-02
## A.T.one                                           A.T.one  1.048320e-02
## A.T.state                                       A.T.state  1.007193e-02
## S.T.state                                       S.T.state  9.894250e-03
## S.T.one                                           S.T.one  9.592876e-03
## H.npnct03.log                               H.npnct03.log  9.533020e-03
## H.P.s.notebook                             H.P.s.notebook  7.755542e-03
## A.npnct24.log                               A.npnct24.log -9.890046e-19
## H.npnct24.log                               H.npnct24.log -9.890046e-19
## S.npnct24.log                               S.npnct24.log -9.890046e-19
## H.T.take                                         H.T.take -9.276608e-04
## A.npnct16.log                               A.npnct16.log -1.587454e-03
## S.npnct16.log                               S.npnct16.log -1.587454e-03
## H.T.time                                         H.T.time -2.527450e-03
## S.T.presid                                     S.T.presid -2.581591e-03
## S.npnct08.log                               S.npnct08.log -3.372706e-03
## A.npnct08.log                               A.npnct08.log -4.193476e-03
## A.npnct25.log                               A.npnct25.log -5.547032e-03
## A.npnct10.log                               A.npnct10.log -5.547032e-03
## H.npnct10.log                               H.npnct10.log -5.547032e-03
## H.npnct20.log                               H.npnct20.log -5.547032e-03
## S.npnct02.log                               S.npnct02.log -5.547032e-03
## S.npnct10.log                               S.npnct10.log -5.547032e-03
## PubDate.last100.log                   PubDate.last100.log -7.663322e-03
## .rnorm                                             .rnorm -8.244230e-03
## H.npnct05.log                               H.npnct05.log -9.653967e-03
## H.T.obama                                       H.T.obama -9.907543e-03
## H.T.say                                           H.T.say -9.960773e-03
## H.T.bank                                         H.T.bank -9.989139e-03
## PubDate.date.fctr                       PubDate.date.fctr -1.164756e-02
## PubDate.second.fctr                   PubDate.second.fctr -1.187946e-02
## H.npnct07.log                               H.npnct07.log -1.201741e-02
## A.npnct07.log                               A.npnct07.log -1.214357e-02
## S.npnct07.log                               S.npnct07.log -1.214357e-02
## S.npnct03.log                               S.npnct03.log -1.240734e-02
## A.npnct18.log                               A.npnct18.log -1.271661e-02
## A.P.http                                         A.P.http -1.294748e-02
## H.npnct12.log                               H.npnct12.log -1.305305e-02
## A.npnct03.log                               A.npnct03.log -1.359260e-02
## H.T.word                                         H.T.word -1.382927e-02
## H.T.big                                           H.T.big -1.390748e-02
## A.npnct02.log                               A.npnct02.log -1.451467e-02
## A.npnct17.log                               A.npnct17.log -1.457558e-02
## A.P.year.colon                             A.P.year.colon -1.755336e-02
## S.P.year.colon                             S.P.year.colon -1.755336e-02
## A.npnct20.log                               A.npnct20.log -1.923169e-02
## S.npnct20.log                               S.npnct20.log -1.923169e-02
## S.T.obama                                       S.T.obama -1.933422e-02
## H.npnct02.log                               H.npnct02.log -2.001851e-02
## A.T.obama                                       A.T.obama -2.020436e-02
## H.T.test                                         H.T.test -2.057181e-02
## S.npnct14.log                               S.npnct14.log -2.121844e-02
## A.P.first.draft                           A.P.first.draft -2.150663e-02
## H.P.on.this.day                           H.P.on.this.day -2.150663e-02
## S.P.first.draft                           S.P.first.draft -2.150663e-02
## A.T.take                                         A.T.take -2.287870e-02
## S.T.take                                         S.T.take -2.307456e-02
## A.npnct06.log                               A.npnct06.log -2.389145e-02
## S.npnct06.log                               S.npnct06.log -2.389145e-02
## A.npnct14.log                               A.npnct14.log -2.407715e-02
## H.npnct13.log                               H.npnct13.log -2.524770e-02
## H.T.deal                                         H.T.deal -2.559418e-02
## A.T.new                                           A.T.new -2.779223e-02
## S.T.new                                           S.T.new -2.833575e-02
## A.P.metropolitan.diary.colon A.P.metropolitan.diary.colon -2.841404e-02
## S.P.metropolitan.diary.colon S.P.metropolitan.diary.colon -2.841404e-02
## H.T.billion                                   H.T.billion -2.949817e-02
## H.T.polit                                       H.T.polit -3.062866e-02
## H.T.china                                       H.T.china -3.144808e-02
## H.P.verbatim.colon                     H.P.verbatim.colon -3.194363e-02
## H.T.art                                           H.T.art -3.280483e-02
## PubDate.minute.fctr                   PubDate.minute.fctr -3.407385e-02
## H.T.read                                         H.T.read -3.467043e-02
## A.T.year                                         A.T.year -3.497471e-02
## S.npnct12.log                               S.npnct12.log -3.638891e-02
## H.P.today.in.politic                 H.P.today.in.politic -3.733661e-02
## A.npnct12.log                               A.npnct12.log -3.760012e-02
## H.P.what.we.are                           H.P.what.we.are -3.775209e-02
## A.T.will                                         A.T.will -3.859405e-02
## S.T.will                                         S.T.will -3.931556e-02
## A.T.appear                                     A.T.appear -3.949035e-02
## PubDate.wkday.fctr                     PubDate.wkday.fctr -3.980129e-02
## H.T.pictur                                     H.T.pictur -3.993172e-02
## A.T.senat                                       A.T.senat -4.140312e-02
## S.T.day                                           S.T.day -4.209616e-02
## S.T.show                                         S.T.show -4.218975e-02
## H.P.today.in.smallbusiness     H.P.today.in.smallbusiness -4.243051e-02
## S.T.photo                                       S.T.photo -4.269774e-02
## H.P.first.draft                           H.P.first.draft -4.316253e-02
## H.T.new                                           H.T.new -4.327803e-02
## S.npnct28.log                               S.npnct28.log -4.370037e-02
## A.npnct28.log                               A.npnct28.log -4.373349e-02
## A.P.daily.clip.report               A.P.daily.clip.report -4.388279e-02
## H.P.daily.clip.report               H.P.daily.clip.report -4.388279e-02
## S.P.daily.clip.report               S.P.daily.clip.report -4.388279e-02
## A.T.day                                           A.T.day -4.400791e-02
## H.T.news                                         H.T.news -4.415284e-02
## H.T.first                                       H.T.first -4.458885e-02
## S.T.first                                       S.T.first -4.494610e-02
## H.T.X2014                                       H.T.X2014 -4.497745e-02
## S.T.past                                         S.T.past -4.562700e-02
## A.T.compani                                   A.T.compani -4.732205e-02
## S.T.compani                                   S.T.compani -4.739476e-02
## S.T.report                                     S.T.report -4.746396e-02
## A.T.word                                         A.T.word -4.818014e-02
## H.T.morn                                         H.T.morn -4.838380e-02
## A.T.report                                     A.T.report -4.847952e-02
## A.T.editor                                     A.T.editor -4.875955e-02
## S.T.word                                         S.T.word -4.876372e-02
## H.T.busi                                         H.T.busi -4.901905e-02
## A.npnct13.log                               A.npnct13.log -4.999563e-02
## S.T.share                                       S.T.share -5.076046e-02
## H.npnct04.log                               H.npnct04.log -5.126277e-02
## S.npnct13.log                               S.npnct13.log -5.332519e-02
## S.T.articl                                     S.T.articl -5.446081e-02
## A.T.time                                         A.T.time -5.456053e-02
## S.T.time                                         S.T.time -5.574436e-02
## H.T.newyork                                   H.T.newyork -5.717575e-02
## A.T.newyork                                   A.T.newyork -5.769443e-02
## H.T.today                                       H.T.today -5.831308e-02
## H.T.daili                                       H.T.daili -5.852819e-02
## H.T.report                                     H.T.report -5.934795e-02
## S.T.newyork                                   S.T.newyork -5.943875e-02
## H.T.day                                           H.T.day -6.029849e-02
## A.T.intern                                     A.T.intern -6.065381e-02
## A.T.scene                                       A.T.scene -6.091747e-02
## S.T.scene                                       S.T.scene -6.098895e-02
## H.npnct14.log                               H.npnct14.log -6.158577e-02
## S.T.diari                                       S.T.diari -6.227998e-02
## S.T.highlight                               S.T.highlight -6.283750e-02
## A.npnct04.log                               A.npnct04.log -6.294642e-02
## S.npnct04.log                               S.npnct04.log -6.294642e-02
## H.T.X2015                                       H.T.X2015 -6.570743e-02
## S.npnct15.log                               S.npnct15.log -6.770952e-02
## S.T.tribun                                     S.T.tribun -6.880320e-02
## A.npnct15.log                               A.npnct15.log -6.893301e-02
## S.T.intern                                     S.T.intern -6.932606e-02
## A.T.photo                                       A.T.photo -6.984501e-02
## H.T.week                                         H.T.week -6.991953e-02
## A.P.fashion.week                         A.P.fashion.week -7.080716e-02
## S.P.fashion.week                         S.P.fashion.week -7.080716e-02
## H.P.fashion.week                         H.P.fashion.week -7.632046e-02
## H.P.year.colon                             H.P.year.colon -7.842875e-02
## H.T.fashion                                   H.T.fashion -8.000421e-02
## H.npnct15.log                               H.npnct15.log -8.273237e-02
## S.T.fashion                                   S.T.fashion -8.344900e-02
## A.T.fashion                                   A.T.fashion -8.349527e-02
## A.T.week                                         A.T.week -8.570201e-02
## S.T.week                                         S.T.week -8.627682e-02
## H.npnct28.log                               H.npnct28.log -8.917338e-02
## S.npnct11.log                               S.npnct11.log -9.158156e-02
## A.npnct11.log                               A.npnct11.log -9.183870e-02
## H.ndgts.log                                   H.ndgts.log -1.196633e-01
## S.ndgts.log                                   S.ndgts.log -1.242046e-01
## A.ndgts.log                                   A.ndgts.log -1.249484e-01
## H.nuppr.log                                   H.nuppr.log -1.278085e-01
## H.nchrs.log                                   H.nchrs.log -1.710624e-01
## H.nwrds.unq.log                           H.nwrds.unq.log -1.957553e-01
## A.nchrs.log                                   A.nchrs.log -2.245488e-01
## S.nchrs.log                                   S.nchrs.log -2.246930e-01
## A.nwrds.unq.log                           A.nwrds.unq.log -2.261526e-01
## S.nwrds.unq.log                           S.nwrds.unq.log -2.343044e-01
## S.nuppr.log                                   S.nuppr.log -2.718459e-01
## A.nuppr.log                                   A.nuppr.log -2.720962e-01
## A.npnct05.log                               A.npnct05.log            NA
## A.npnct09.log                               A.npnct09.log            NA
## A.npnct22.log                               A.npnct22.log            NA
## A.npnct26.log                               A.npnct26.log            NA
## A.npnct27.log                               A.npnct27.log            NA
## A.npnct29.log                               A.npnct29.log            NA
## A.npnct30.log                               A.npnct30.log            NA
## clusterid                                       clusterid            NA
## H.npnct09.log                               H.npnct09.log            NA
## H.npnct17.log                               H.npnct17.log            NA
## H.npnct18.log                               H.npnct18.log            NA
## H.npnct21.log                               H.npnct21.log            NA
## H.npnct22.log                               H.npnct22.log            NA
## H.npnct23.log                               H.npnct23.log            NA
## H.npnct25.log                               H.npnct25.log            NA
## H.npnct26.log                               H.npnct26.log            NA
## H.npnct27.log                               H.npnct27.log            NA
## H.npnct29.log                               H.npnct29.log            NA
## H.npnct30.log                               H.npnct30.log            NA
## H.P.http                                         H.P.http            NA
## PubDate.year.fctr                       PubDate.year.fctr            NA
## S.npnct05.log                               S.npnct05.log            NA
## S.npnct09.log                               S.npnct09.log            NA
## S.npnct17.log                               S.npnct17.log            NA
## S.npnct18.log                               S.npnct18.log            NA
## S.npnct22.log                               S.npnct22.log            NA
## S.npnct25.log                               S.npnct25.log            NA
## S.npnct26.log                               S.npnct26.log            NA
## S.npnct27.log                               S.npnct27.log            NA
## S.npnct29.log                               S.npnct29.log            NA
## S.npnct30.log                               S.npnct30.log            NA
## S.P.http                                         S.P.http            NA
##                              exclude.as.feat    cor.y.abs
## Popular                                    1 1.000000e+00
## WordCount.log                              0 2.656836e-01
## WordCount                                  1 2.575265e-01
## H.nwrds.log                                0 1.404401e-01
## PubDate.hour.fctr                          0 1.354368e-01
## H.npnct19.log                              0 1.283641e-01
## S.nwrds.log                                0 1.262344e-01
## A.nwrds.log                                0 1.216578e-01
## PubDate.wkend                              0 1.067288e-01
## H.P.recap.colon                            0 9.008096e-02
## H.P.quandary                               0 8.734922e-02
## H.P.no.comment.colon                       0 6.074669e-02
## S.npnct19.log                              0 5.503894e-02
## A.npnct19.log                              0 5.482747e-02
## PubDate.last10                             1 5.398093e-02
## H.npnct08.log                              0 5.375262e-02
## PubDate.last10.log                         0 4.931702e-02
## PubDate.last1.log                          0 4.635751e-02
## H.P.readers.respond                        0 4.432886e-02
## PubDate.last100                            1 3.989229e-02
## S.T.make                                   0 3.924228e-02
## PubDate.last1                              1 3.592267e-02
## H.T.get                                    0 3.312638e-02
## H.npnct06.log                              0 3.190718e-02
## A.npnct01.log                              0 3.093101e-02
## S.npnct01.log                              0 3.093101e-02
## S.T.can                                    0 3.062115e-02
## H.npnct16.log                              0 3.039622e-02
## A.T.can                                    0 3.032288e-02
## S.npnct21.log                              0 2.760321e-02
## S.npnct23.log                              0 2.760321e-02
## H.T.ebola                                  0 2.728582e-02
## H.npnct01.log                              0 2.271577e-02
## PubDate.month.fctr                         1 1.914874e-02
## A.T.said                                   0 1.831840e-02
## S.T.said                                   0 1.801374e-02
## PubDate.POSIX                              1 1.568326e-02
## PubDate.zoo                                1 1.568326e-02
## A.npnct21.log                              0 1.537569e-02
## A.npnct23.log                              0 1.537569e-02
## H.T.make                                   0 1.430572e-02
## H.npnct11.log                              0 1.333613e-02
## myCategory.fctr                            0 1.234541e-02
## UniqueID                                   1 1.182492e-02
## A.T.one                                    0 1.048320e-02
## A.T.state                                  0 1.007193e-02
## S.T.state                                  0 9.894250e-03
## S.T.one                                    0 9.592876e-03
## H.npnct03.log                              0 9.533020e-03
## H.P.s.notebook                             0 7.755542e-03
## A.npnct24.log                              0 9.890046e-19
## H.npnct24.log                              0 9.890046e-19
## S.npnct24.log                              0 9.890046e-19
## H.T.take                                   0 9.276608e-04
## A.npnct16.log                              0 1.587454e-03
## S.npnct16.log                              0 1.587454e-03
## H.T.time                                   0 2.527450e-03
## S.T.presid                                 0 2.581591e-03
## S.npnct08.log                              0 3.372706e-03
## A.npnct08.log                              0 4.193476e-03
## A.npnct25.log                              0 5.547032e-03
## A.npnct10.log                              0 5.547032e-03
## H.npnct10.log                              0 5.547032e-03
## H.npnct20.log                              0 5.547032e-03
## S.npnct02.log                              0 5.547032e-03
## S.npnct10.log                              0 5.547032e-03
## PubDate.last100.log                        0 7.663322e-03
## .rnorm                                     0 8.244230e-03
## H.npnct05.log                              0 9.653967e-03
## H.T.obama                                  0 9.907543e-03
## H.T.say                                    0 9.960773e-03
## H.T.bank                                   0 9.989139e-03
## PubDate.date.fctr                          0 1.164756e-02
## PubDate.second.fctr                        0 1.187946e-02
## H.npnct07.log                              0 1.201741e-02
## A.npnct07.log                              0 1.214357e-02
## S.npnct07.log                              0 1.214357e-02
## S.npnct03.log                              0 1.240734e-02
## A.npnct18.log                              0 1.271661e-02
## A.P.http                                   0 1.294748e-02
## H.npnct12.log                              0 1.305305e-02
## A.npnct03.log                              0 1.359260e-02
## H.T.word                                   0 1.382927e-02
## H.T.big                                    0 1.390748e-02
## A.npnct02.log                              0 1.451467e-02
## A.npnct17.log                              0 1.457558e-02
## A.P.year.colon                             0 1.755336e-02
## S.P.year.colon                             0 1.755336e-02
## A.npnct20.log                              0 1.923169e-02
## S.npnct20.log                              0 1.923169e-02
## S.T.obama                                  0 1.933422e-02
## H.npnct02.log                              0 2.001851e-02
## A.T.obama                                  0 2.020436e-02
## H.T.test                                   0 2.057181e-02
## S.npnct14.log                              0 2.121844e-02
## A.P.first.draft                            0 2.150663e-02
## H.P.on.this.day                            0 2.150663e-02
## S.P.first.draft                            0 2.150663e-02
## A.T.take                                   0 2.287870e-02
## S.T.take                                   0 2.307456e-02
## A.npnct06.log                              0 2.389145e-02
## S.npnct06.log                              0 2.389145e-02
## A.npnct14.log                              0 2.407715e-02
## H.npnct13.log                              0 2.524770e-02
## H.T.deal                                   0 2.559418e-02
## A.T.new                                    0 2.779223e-02
## S.T.new                                    0 2.833575e-02
## A.P.metropolitan.diary.colon               0 2.841404e-02
## S.P.metropolitan.diary.colon               0 2.841404e-02
## H.T.billion                                0 2.949817e-02
## H.T.polit                                  0 3.062866e-02
## H.T.china                                  0 3.144808e-02
## H.P.verbatim.colon                         0 3.194363e-02
## H.T.art                                    0 3.280483e-02
## PubDate.minute.fctr                        0 3.407385e-02
## H.T.read                                   0 3.467043e-02
## A.T.year                                   0 3.497471e-02
## S.npnct12.log                              0 3.638891e-02
## H.P.today.in.politic                       0 3.733661e-02
## A.npnct12.log                              0 3.760012e-02
## H.P.what.we.are                            0 3.775209e-02
## A.T.will                                   0 3.859405e-02
## S.T.will                                   0 3.931556e-02
## A.T.appear                                 0 3.949035e-02
## PubDate.wkday.fctr                         0 3.980129e-02
## H.T.pictur                                 0 3.993172e-02
## A.T.senat                                  0 4.140312e-02
## S.T.day                                    0 4.209616e-02
## S.T.show                                   0 4.218975e-02
## H.P.today.in.smallbusiness                 0 4.243051e-02
## S.T.photo                                  0 4.269774e-02
## H.P.first.draft                            0 4.316253e-02
## H.T.new                                    0 4.327803e-02
## S.npnct28.log                              0 4.370037e-02
## A.npnct28.log                              0 4.373349e-02
## A.P.daily.clip.report                      0 4.388279e-02
## H.P.daily.clip.report                      0 4.388279e-02
## S.P.daily.clip.report                      0 4.388279e-02
## A.T.day                                    0 4.400791e-02
## H.T.news                                   0 4.415284e-02
## H.T.first                                  0 4.458885e-02
## S.T.first                                  0 4.494610e-02
## H.T.X2014                                  0 4.497745e-02
## S.T.past                                   0 4.562700e-02
## A.T.compani                                0 4.732205e-02
## S.T.compani                                0 4.739476e-02
## S.T.report                                 0 4.746396e-02
## A.T.word                                   0 4.818014e-02
## H.T.morn                                   0 4.838380e-02
## A.T.report                                 0 4.847952e-02
## A.T.editor                                 0 4.875955e-02
## S.T.word                                   0 4.876372e-02
## H.T.busi                                   0 4.901905e-02
## A.npnct13.log                              0 4.999563e-02
## S.T.share                                  0 5.076046e-02
## H.npnct04.log                              0 5.126277e-02
## S.npnct13.log                              0 5.332519e-02
## S.T.articl                                 0 5.446081e-02
## A.T.time                                   0 5.456053e-02
## S.T.time                                   0 5.574436e-02
## H.T.newyork                                0 5.717575e-02
## A.T.newyork                                0 5.769443e-02
## H.T.today                                  0 5.831308e-02
## H.T.daili                                  0 5.852819e-02
## H.T.report                                 0 5.934795e-02
## S.T.newyork                                0 5.943875e-02
## H.T.day                                    0 6.029849e-02
## A.T.intern                                 0 6.065381e-02
## A.T.scene                                  0 6.091747e-02
## S.T.scene                                  0 6.098895e-02
## H.npnct14.log                              0 6.158577e-02
## S.T.diari                                  0 6.227998e-02
## S.T.highlight                              0 6.283750e-02
## A.npnct04.log                              0 6.294642e-02
## S.npnct04.log                              0 6.294642e-02
## H.T.X2015                                  0 6.570743e-02
## S.npnct15.log                              0 6.770952e-02
## S.T.tribun                                 0 6.880320e-02
## A.npnct15.log                              0 6.893301e-02
## S.T.intern                                 0 6.932606e-02
## A.T.photo                                  0 6.984501e-02
## H.T.week                                   0 6.991953e-02
## A.P.fashion.week                           0 7.080716e-02
## S.P.fashion.week                           0 7.080716e-02
## H.P.fashion.week                           0 7.632046e-02
## H.P.year.colon                             0 7.842875e-02
## H.T.fashion                                0 8.000421e-02
## H.npnct15.log                              0 8.273237e-02
## S.T.fashion                                0 8.344900e-02
## A.T.fashion                                0 8.349527e-02
## A.T.week                                   0 8.570201e-02
## S.T.week                                   0 8.627682e-02
## H.npnct28.log                              0 8.917338e-02
## S.npnct11.log                              0 9.158156e-02
## A.npnct11.log                              0 9.183870e-02
## H.ndgts.log                                0 1.196633e-01
## S.ndgts.log                                0 1.242046e-01
## A.ndgts.log                                0 1.249484e-01
## H.nuppr.log                                0 1.278085e-01
## H.nchrs.log                                0 1.710624e-01
## H.nwrds.unq.log                            0 1.957553e-01
## A.nchrs.log                                0 2.245488e-01
## S.nchrs.log                                0 2.246930e-01
## A.nwrds.unq.log                            0 2.261526e-01
## S.nwrds.unq.log                            0 2.343044e-01
## S.nuppr.log                                0 2.718459e-01
## A.nuppr.log                                0 2.720962e-01
## A.npnct05.log                              0           NA
## A.npnct09.log                              0           NA
## A.npnct22.log                              0           NA
## A.npnct26.log                              0           NA
## A.npnct27.log                              0           NA
## A.npnct29.log                              0           NA
## A.npnct30.log                              0           NA
## clusterid                                  0           NA
## H.npnct09.log                              0           NA
## H.npnct17.log                              0           NA
## H.npnct18.log                              0           NA
## H.npnct21.log                              0           NA
## H.npnct22.log                              0           NA
## H.npnct23.log                              0           NA
## H.npnct25.log                              0           NA
## H.npnct26.log                              0           NA
## H.npnct27.log                              0           NA
## H.npnct29.log                              0           NA
## H.npnct30.log                              0           NA
## H.P.http                                   0           NA
## PubDate.year.fctr                          0           NA
## S.npnct05.log                              0           NA
## S.npnct09.log                              0           NA
## S.npnct17.log                              0           NA
## S.npnct18.log                              0           NA
## S.npnct22.log                              0           NA
## S.npnct25.log                              0           NA
## S.npnct26.log                              0           NA
## S.npnct27.log                              0           NA
## S.npnct29.log                              0           NA
## S.npnct30.log                              0           NA
## S.P.http                                   0           NA
##                                                cor.high.X   freqRatio
## Popular                                              <NA>    4.976212
## WordCount.log                                        <NA>    1.300000
## WordCount                                            <NA>    2.315789
## H.nwrds.log                                          <NA>    1.127273
## PubDate.hour.fctr                                    <NA>    1.835040
## H.npnct19.log                                        <NA>   14.995098
## S.nwrds.log                                   A.nwrds.log    2.583333
## A.nwrds.log                                          <NA>    2.583333
## PubDate.wkend                                        <NA>    9.095827
## H.P.recap.colon                                      <NA>   93.666667
## H.P.quandary                                         <NA>  652.200000
## H.P.no.comment.colon                                 <NA>  724.777778
## S.npnct19.log                               A.npnct19.log   12.862366
## A.npnct19.log                                        <NA>   12.798715
## PubDate.last10                                       <NA>    1.666667
## H.npnct08.log                                        <NA>  111.620690
## PubDate.last10.log                                   <NA>    1.666667
## PubDate.last1.log                                    <NA>    1.142857
## H.P.readers.respond                                  <NA>  342.789474
## PubDate.last100                                      <NA>   25.000000
## S.T.make                                             <NA>  273.782609
## PubDate.last1                                        <NA>    1.142857
## H.T.get                                              <NA>  430.866667
## H.npnct06.log                               H.npnct16.log   68.935484
## A.npnct01.log                               S.npnct01.log  309.952381
## S.npnct01.log                                        <NA>  309.952381
## S.T.can                                           A.T.can  261.666667
## H.npnct16.log                                        <NA>   96.104478
## A.T.can                                              <NA>  241.538462
## S.npnct21.log                               A.npnct21.log 6531.000000
## S.npnct23.log                                        <NA> 6531.000000
## H.T.ebola                                            <NA>  293.000000
## H.npnct01.log                                        <NA>  282.913043
## PubDate.month.fctr                                   <NA>    1.017514
## A.T.said                                         S.T.said  202.516129
## S.T.said                                             <NA>  174.388889
## PubDate.POSIX                                        <NA>    1.000000
## PubDate.zoo                                          <NA>    1.000000
## A.npnct21.log                               A.npnct23.log 3264.500000
## A.npnct23.log                                        <NA> 3264.500000
## H.T.make                                             <NA>  322.200000
## H.npnct11.log                                        <NA>    4.937442
## myCategory.fctr                                      <NA>    1.337185
## UniqueID                                             <NA>    1.000000
## A.T.one                                           S.T.one  231.111111
## A.T.state                                       S.T.state  332.315789
## S.T.state                                            <NA>  315.750000
## S.T.one                                              <NA>  222.892857
## H.npnct03.log                                        <NA> 2176.333333
## H.P.s.notebook                                       <NA>  815.500000
## A.npnct24.log                                        <NA>    0.000000
## H.npnct24.log                                        <NA>    0.000000
## S.npnct24.log                                        <NA>    0.000000
## H.T.take                                             <NA>  322.250000
## A.npnct16.log                                        <NA>  434.133333
## S.npnct16.log                                        <NA>  434.133333
## H.T.time                                             <NA>  247.538462
## S.T.presid                                           <NA>  241.692308
## S.npnct08.log                                        <NA>  175.486486
## A.npnct08.log                                        <NA>  170.842105
## A.npnct25.log                                        <NA> 6531.000000
## A.npnct10.log                                        <NA> 6531.000000
## H.npnct10.log                                        <NA> 6531.000000
## H.npnct20.log                                        <NA> 6531.000000
## S.npnct02.log                                        <NA> 6531.000000
## S.npnct10.log                                        <NA> 6531.000000
## PubDate.last100.log                                  <NA>   25.000000
## .rnorm                                               <NA>    2.000000
## H.npnct05.log                                        <NA>  543.333333
## H.T.obama                                            <NA>  229.750000
## H.T.say                                              <NA>  247.461538
## H.T.bank                                             <NA>  221.689655
## PubDate.date.fctr                                    <NA>    1.021394
## PubDate.second.fctr                                  <NA>    1.018204
## H.npnct07.log                                        <NA>    5.437234
## A.npnct07.log                               S.npnct07.log 1631.750000
## S.npnct07.log                                        <NA> 1631.750000
## S.npnct03.log                                        <NA> 1305.400000
## A.npnct18.log                                        <NA> 1631.500000
## A.P.http                                    A.npnct18.log 1305.200000
## H.npnct12.log                                        <NA>   13.126638
## A.npnct03.log                               S.npnct03.log 1087.666667
## H.T.word                                             <NA>  104.096774
## H.T.big                                              <NA>  403.562500
## A.npnct02.log                                    A.P.http 1087.500000
## A.npnct17.log                               A.npnct02.log 1087.500000
## A.P.year.colon                             S.P.year.colon  652.200000
## S.P.year.colon                                       <NA>  652.200000
## A.npnct20.log                               S.npnct20.log  543.333333
## S.npnct20.log                                        <NA>  543.333333
## S.T.obama                                            <NA>  335.684211
## H.npnct02.log                                        <NA>  501.461538
## A.T.obama                                       S.T.obama  354.333333
## H.T.test                                             <NA>  280.000000
## S.npnct14.log                                        <NA>  203.062500
## A.P.first.draft                           S.P.first.draft  434.466667
## H.P.on.this.day                                      <NA>  434.466667
## S.P.first.draft                                      <NA>  434.466667
## A.T.take                                             <NA>  263.125000
## S.T.take                                         A.T.take  263.166667
## A.npnct06.log                               S.npnct06.log  115.642857
## S.npnct06.log                                        <NA>  115.642857
## A.npnct14.log                               A.npnct17.log  196.696970
## H.npnct13.log                                        <NA>   22.802326
## H.T.deal                                             <NA>  258.080000
## A.T.new                                              <NA>  104.052632
## S.T.new                                           A.T.new  100.559322
## A.P.metropolitan.diary.colon S.P.metropolitan.diary.colon   99.492308
## S.P.metropolitan.diary.colon                         <NA>   99.492308
## H.T.billion                                          <NA>  229.892857
## H.T.polit                                            <NA>  126.254902
## H.T.china                                            <NA>  238.555556
## H.P.verbatim.colon                                   <NA>  196.939394
## H.T.art                                              <NA>  307.333333
## PubDate.minute.fctr                                  <NA>    1.483365
## H.T.read                                             <NA>  179.388889
## A.T.year                                             <NA>  138.727273
## S.npnct12.log                                        <NA>    5.706263
## H.P.today.in.politic                            H.T.polit  144.155556
## A.npnct12.log                               S.npnct12.log    5.715368
## H.P.what.we.are                                  H.T.read  141.000000
## A.T.will                                             <NA>  112.547170
## S.T.will                                         A.T.will  112.584906
## A.T.appear                                           <NA>  228.821429
## PubDate.wkday.fctr                                   <NA>    1.003268
## H.T.pictur                                           <NA>  104.032258
## A.T.senat                                            <NA>  316.450000
## S.T.day                                              <NA>   88.338028
## S.T.show                                             <NA>  274.608696
## H.P.today.in.smallbusiness                           <NA>  111.620690
## S.T.photo                                      H.T.pictur  248.038462
## H.P.first.draft                                      <NA>  107.866667
## H.T.new                                              <NA>  116.333333
## S.npnct28.log                                        <NA>  134.791667
## A.npnct28.log                               S.npnct28.log  126.862745
## A.P.daily.clip.report               S.P.daily.clip.report  104.354839
## H.P.daily.clip.report                                <NA>  104.354839
## S.P.daily.clip.report                                <NA>  104.354839
## A.T.day                                           S.T.day   85.904110
## H.T.news                                             <NA>  238.518519
## H.T.first                                 H.P.first.draft  194.727273
## S.T.first                                            <NA>  217.724138
## H.T.X2014                                            <NA>  112.824561
## S.T.past                                       A.T.appear  229.285714
## A.T.compani                                          <NA>  128.541667
## S.T.compani                                   A.T.compani  140.227273
## S.T.report                                           <NA>   78.362500
## A.T.word                                             <NA>  133.145833
## H.T.morn                                    A.npnct28.log  165.205128
## A.T.report                                     S.T.report   88.295775
## A.T.editor                                           <NA>  160.225000
## S.T.word                                         A.T.word  133.145833
## H.T.busi                                             <NA>  229.428571
## A.npnct13.log                                        <NA>    4.603330
## S.T.share                                            <NA>  218.448276
## H.npnct04.log                                 H.T.billion   38.325301
## S.npnct13.log                               A.npnct13.log    4.672000
## S.T.articl                                       S.T.past   85.500000
## A.T.time                                             <NA>   68.011236
## S.T.time                                         A.T.time   65.804348
## H.T.newyork                                          <NA>   95.409091
## A.T.newyork                                          <NA>  103.762712
## H.T.today                            H.P.today.in.politic  138.239130
## H.T.daili                           A.P.daily.clip.report  102.903226
## H.T.report                                      H.T.daili  102.000000
## S.T.newyork                                   A.T.newyork  145.761905
## H.T.day                                              <NA>   86.547945
## A.T.intern                                           <NA>  157.950000
## A.T.scene                                            <NA>   71.921348
## S.T.scene                                       A.T.scene   65.316327
## H.npnct14.log                                        <NA>   52.983471
## S.T.diari                                       S.T.scene   64.959184
## S.T.highlight                                        <NA>  187.500000
## A.npnct04.log                               S.npnct04.log   28.536364
## S.npnct04.log                                        <NA>   28.536364
## H.T.X2015                                       S.T.diari  101.444444
## S.npnct15.log                                        <NA>   13.647191
## S.T.tribun                                           <NA>  138.456522
## A.npnct15.log                               S.npnct15.log   13.482222
## S.T.intern                                     S.T.tribun  131.625000
## A.T.photo                                       S.T.photo   63.360000
## H.T.week                                             <NA>   53.666667
## A.P.fashion.week                         S.P.fashion.week   40.081761
## S.P.fashion.week                                     <NA>   40.081761
## H.P.fashion.week                                     <NA>   34.500000
## H.P.year.colon                                 S.T.intern   32.670103
## H.T.fashion                              H.P.fashion.week   71.681818
## H.npnct15.log                                        <NA>    3.914910
## S.T.fashion                                          <NA>   59.809524
## A.T.fashion                                     H.T.X2015   66.105263
## A.T.week                                             <NA>   47.273438
## S.T.week                                         A.T.week   48.408000
## H.npnct28.log                                        <NA>   24.123077
## S.npnct11.log                                        <NA>    1.660473
## A.npnct11.log                               S.npnct11.log    1.660473
## H.ndgts.log                                          <NA>   13.616137
## S.ndgts.log                                          <NA>   10.511247
## A.ndgts.log                                   S.ndgts.log   10.501022
## H.nuppr.log                                          <NA>    1.033930
## H.nchrs.log                                          <NA>    1.023810
## H.nwrds.unq.log                               H.nuppr.log    1.008878
## A.nchrs.log                                          <NA>    1.328571
## S.nchrs.log                                   A.nchrs.log    1.328571
## A.nwrds.unq.log                                      <NA>    1.128405
## S.nwrds.unq.log                               S.nchrs.log    1.092486
## S.nuppr.log                                          <NA>    1.152620
## A.nuppr.log                                   S.nuppr.log    1.151308
## A.npnct05.log                                        <NA>    0.000000
## A.npnct09.log                                        <NA>    0.000000
## A.npnct22.log                                        <NA>    0.000000
## A.npnct26.log                                        <NA>    0.000000
## A.npnct27.log                                        <NA>    0.000000
## A.npnct29.log                                        <NA>    0.000000
## A.npnct30.log                                        <NA>    0.000000
## clusterid                                            <NA>    0.000000
## H.npnct09.log                                        <NA>    0.000000
## H.npnct17.log                                        <NA>    0.000000
## H.npnct18.log                                        <NA>    0.000000
## H.npnct21.log                                        <NA>    0.000000
## H.npnct22.log                                        <NA>    0.000000
## H.npnct23.log                                        <NA>    0.000000
## H.npnct25.log                                        <NA>    0.000000
## H.npnct26.log                                        <NA>    0.000000
## H.npnct27.log                                        <NA>    0.000000
## H.npnct29.log                                        <NA>    0.000000
## H.npnct30.log                                        <NA>    0.000000
## H.P.http                                             <NA>    0.000000
## PubDate.year.fctr                                    <NA>    0.000000
## S.npnct05.log                                        <NA>    0.000000
## S.npnct09.log                                        <NA>    0.000000
## S.npnct17.log                                        <NA>    0.000000
## S.npnct18.log                                        <NA>    0.000000
## S.npnct22.log                                        <NA>    0.000000
## S.npnct25.log                                        <NA>    0.000000
## S.npnct26.log                                        <NA>    0.000000
## S.npnct27.log                                        <NA>    0.000000
## S.npnct29.log                                        <NA>    0.000000
## S.npnct30.log                                        <NA>    0.000000
## S.P.http                                             <NA>    0.000000
##                              percentUnique zeroVar   nzv myNearZV
## Popular                         0.03061849   FALSE FALSE    FALSE
## WordCount.log                  24.14268218   FALSE FALSE    FALSE
## WordCount                      24.15799143   FALSE FALSE    FALSE
## H.nwrds.log                    84.15492958   FALSE FALSE    FALSE
## PubDate.hour.fctr               0.04592774   FALSE FALSE    FALSE
## H.npnct19.log                   0.06123699   FALSE FALSE    FALSE
## S.nwrds.log                    93.50887936   FALSE FALSE    FALSE
## A.nwrds.log                    93.50887936   FALSE FALSE    FALSE
## PubDate.wkend                   0.03061849   FALSE FALSE    FALSE
## H.P.recap.colon                 0.03061849   FALSE  TRUE    FALSE
## H.P.quandary                    0.03061849   FALSE  TRUE    FALSE
## H.P.no.comment.colon            0.03061849   FALSE  TRUE    FALSE
## S.npnct19.log                   0.07654623   FALSE FALSE    FALSE
## A.npnct19.log                   0.07654623   FALSE FALSE    FALSE
## PubDate.last10                 79.05695040   FALSE FALSE    FALSE
## H.npnct08.log                   0.03061849   FALSE  TRUE    FALSE
## PubDate.last10.log             79.05695040   FALSE FALSE    FALSE
## PubDate.last1.log              36.49724434   FALSE FALSE    FALSE
## H.P.readers.respond             0.03061849   FALSE  TRUE    FALSE
## PubDate.last100                92.52908757   FALSE FALSE    FALSE
## S.T.make                        0.41334966   FALSE  TRUE    FALSE
## PubDate.last1                  36.49724434   FALSE FALSE    FALSE
## H.T.get                         0.18371096   FALSE  TRUE    FALSE
## H.npnct06.log                   0.06123699   FALSE  TRUE    FALSE
## A.npnct01.log                   0.06123699   FALSE  TRUE    FALSE
## S.npnct01.log                   0.06123699   FALSE  TRUE    FALSE
## S.T.can                         0.41334966   FALSE  TRUE    FALSE
## H.npnct16.log                   0.06123699   FALSE  TRUE    FALSE
## A.T.can                         0.48989590   FALSE  TRUE    FALSE
## S.npnct21.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct23.log                   0.03061849   FALSE  TRUE     TRUE
## H.T.ebola                       0.16840171   FALSE  TRUE    FALSE
## H.npnct01.log                   0.04592774   FALSE  TRUE    FALSE
## PubDate.month.fctr              0.04592774   FALSE FALSE    FALSE
## A.T.said                        0.39804042   FALSE  TRUE    FALSE
## S.T.said                        0.36742192   FALSE  TRUE    FALSE
## PubDate.POSIX                  99.86221678   FALSE FALSE    FALSE
## PubDate.zoo                    99.86221678   FALSE FALSE    FALSE
## A.npnct21.log                   0.04592774   FALSE  TRUE     TRUE
## A.npnct23.log                   0.04592774   FALSE  TRUE     TRUE
## H.T.make                        0.13778322   FALSE  TRUE    FALSE
## H.npnct11.log                   0.07654623   FALSE FALSE    FALSE
## myCategory.fctr                 0.30618494   FALSE FALSE    FALSE
## UniqueID                      100.00000000   FALSE FALSE    FALSE
## A.T.one                         0.52051439   FALSE  TRUE    FALSE
## A.T.state                       0.44396816   FALSE  TRUE    FALSE
## S.T.state                       0.39804042   FALSE  TRUE    FALSE
## S.T.one                         0.47458665   FALSE  TRUE    FALSE
## H.npnct03.log                   0.03061849   FALSE  TRUE     TRUE
## H.P.s.notebook                  0.03061849   FALSE  TRUE    FALSE
## A.npnct24.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct24.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct24.log                   0.01530925    TRUE  TRUE     TRUE
## H.T.take                        0.15309247   FALSE  TRUE    FALSE
## A.npnct16.log                   0.04592774   FALSE  TRUE    FALSE
## S.npnct16.log                   0.04592774   FALSE  TRUE    FALSE
## H.T.time                        0.16840171   FALSE  TRUE    FALSE
## S.T.presid                      0.41334966   FALSE  TRUE    FALSE
## S.npnct08.log                   0.06123699   FALSE  TRUE    FALSE
## A.npnct08.log                   0.06123699   FALSE  TRUE    FALSE
## A.npnct25.log                   0.03061849   FALSE  TRUE     TRUE
## A.npnct10.log                   0.03061849   FALSE  TRUE     TRUE
## H.npnct10.log                   0.03061849   FALSE  TRUE     TRUE
## H.npnct20.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct02.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct10.log                   0.03061849   FALSE  TRUE     TRUE
## PubDate.last100.log            92.19228414   FALSE FALSE    FALSE
## .rnorm                         99.98469075   FALSE FALSE    FALSE
## H.npnct05.log                   0.03061849   FALSE  TRUE    FALSE
## H.T.obama                       0.16840171   FALSE  TRUE    FALSE
## H.T.say                         0.16840171   FALSE  TRUE    FALSE
## H.T.bank                        0.13778322   FALSE  TRUE    FALSE
## PubDate.date.fctr               0.07654623   FALSE FALSE    FALSE
## PubDate.second.fctr             0.06123699   FALSE FALSE    FALSE
## H.npnct07.log                   0.12247397   FALSE FALSE    FALSE
## A.npnct07.log                   0.04592774   FALSE  TRUE    FALSE
## S.npnct07.log                   0.04592774   FALSE  TRUE    FALSE
## S.npnct03.log                   0.03061849   FALSE  TRUE    FALSE
## A.npnct18.log                   0.06123699   FALSE  TRUE    FALSE
## A.P.http                        0.04592774   FALSE  TRUE    FALSE
## H.npnct12.log                   0.09185548   FALSE FALSE    FALSE
## A.npnct03.log                   0.03061849   FALSE  TRUE    FALSE
## H.T.word                        0.13778322   FALSE  TRUE    FALSE
## H.T.big                         0.19902021   FALSE  TRUE    FALSE
## A.npnct02.log                   0.04592774   FALSE  TRUE    FALSE
## A.npnct17.log                   0.04592774   FALSE  TRUE    FALSE
## A.P.year.colon                  0.03061849   FALSE  TRUE    FALSE
## S.P.year.colon                  0.03061849   FALSE  TRUE    FALSE
## A.npnct20.log                   0.03061849   FALSE  TRUE    FALSE
## S.npnct20.log                   0.03061849   FALSE  TRUE    FALSE
## S.T.obama                       0.36742192   FALSE  TRUE    FALSE
## H.npnct02.log                   0.03061849   FALSE  TRUE    FALSE
## A.T.obama                       0.36742192   FALSE  TRUE    FALSE
## H.T.test                        0.13778322   FALSE  TRUE    FALSE
## S.npnct14.log                   0.04592774   FALSE  TRUE    FALSE
## A.P.first.draft                 0.03061849   FALSE  TRUE    FALSE
## H.P.on.this.day                 0.03061849   FALSE  TRUE    FALSE
## S.P.first.draft                 0.03061849   FALSE  TRUE    FALSE
## A.T.take                        0.42865891   FALSE  TRUE    FALSE
## S.T.take                        0.38273117   FALSE  TRUE    FALSE
## A.npnct06.log                   0.03061849   FALSE  TRUE    FALSE
## S.npnct06.log                   0.03061849   FALSE  TRUE    FALSE
## A.npnct14.log                   0.10716473   FALSE  TRUE    FALSE
## H.npnct13.log                   0.12247397   FALSE  TRUE    FALSE
## H.T.deal                        0.13778322   FALSE  TRUE    FALSE
## A.T.new                         0.55113288   FALSE  TRUE    FALSE
## S.T.new                         0.48989590   FALSE  TRUE    FALSE
## A.P.metropolitan.diary.colon    0.03061849   FALSE  TRUE    FALSE
## S.P.metropolitan.diary.colon    0.03061849   FALSE  TRUE    FALSE
## H.T.billion                     0.13778322   FALSE  TRUE    FALSE
## H.T.polit                       0.13778322   FALSE  TRUE    FALSE
## H.T.china                       0.18371096   FALSE  TRUE    FALSE
## H.P.verbatim.colon              0.03061849   FALSE  TRUE    FALSE
## H.T.art                         0.19902021   FALSE  TRUE    FALSE
## PubDate.minute.fctr             0.06123699   FALSE FALSE    FALSE
## H.T.read                        0.16840171   FALSE  TRUE    FALSE
## A.T.year                        0.48989590   FALSE  TRUE    FALSE
## S.npnct12.log                   0.09185548   FALSE FALSE    FALSE
## H.P.today.in.politic            0.03061849   FALSE  TRUE    FALSE
## A.npnct12.log                   0.12247397   FALSE FALSE    FALSE
## H.P.what.we.are                 0.03061849   FALSE  TRUE    FALSE
## A.T.will                        0.62767912   FALSE  TRUE    FALSE
## S.T.will                        0.55113288   FALSE  TRUE    FALSE
## A.T.appear                      0.30618494   FALSE  TRUE    FALSE
## PubDate.wkday.fctr              0.10716473   FALSE FALSE    FALSE
## H.T.pictur                      0.10716473   FALSE  TRUE    FALSE
## A.T.senat                       0.52051439   FALSE  TRUE    FALSE
## S.T.day                         0.39804042   FALSE  TRUE    FALSE
## S.T.show                        0.39804042   FALSE  TRUE    FALSE
## H.P.today.in.smallbusiness      0.03061849   FALSE  TRUE    FALSE
## S.T.photo                       0.27556644   FALSE  TRUE    FALSE
## H.P.first.draft                 0.03061849   FALSE  TRUE    FALSE
## H.T.new                         0.19902021   FALSE  TRUE    FALSE
## S.npnct28.log                   0.04592774   FALSE  TRUE    FALSE
## A.npnct28.log                   0.04592774   FALSE  TRUE    FALSE
## A.P.daily.clip.report           0.03061849   FALSE  TRUE    FALSE
## H.P.daily.clip.report           0.03061849   FALSE  TRUE    FALSE
## S.P.daily.clip.report           0.03061849   FALSE  TRUE    FALSE
## A.T.day                         0.42865891   FALSE  TRUE    FALSE
## H.T.news                        0.16840171   FALSE  TRUE    FALSE
## H.T.first                       0.15309247   FALSE  TRUE    FALSE
## S.T.first                       0.39804042   FALSE  TRUE    FALSE
## H.T.X2014                       0.13778322   FALSE  TRUE    FALSE
## S.T.past                        0.29087569   FALSE  TRUE    FALSE
## A.T.compani                     0.45927740   FALSE  TRUE    FALSE
## S.T.compani                     0.44396816   FALSE  TRUE    FALSE
## S.T.report                      0.35211268   FALSE  TRUE    FALSE
## A.T.word                        0.32149418   FALSE  TRUE    FALSE
## H.T.morn                        0.07654623   FALSE  TRUE    FALSE
## A.T.report                      0.36742192   FALSE  TRUE    FALSE
## A.T.editor                      0.29087569   FALSE  TRUE    FALSE
## S.T.word                        0.30618494   FALSE  TRUE    FALSE
## H.T.busi                        0.18371096   FALSE  TRUE    FALSE
## A.npnct13.log                   0.16840171   FALSE FALSE    FALSE
## S.T.share                       0.36742192   FALSE  TRUE    FALSE
## H.npnct04.log                   0.04592774   FALSE  TRUE    FALSE
## S.npnct13.log                   0.16840171   FALSE FALSE    FALSE
## S.T.articl                      0.32149418   FALSE  TRUE    FALSE
## A.T.time                        0.42865891   FALSE  TRUE    FALSE
## S.T.time                        0.44396816   FALSE  TRUE    FALSE
## H.T.newyork                     0.15309247   FALSE  TRUE    FALSE
## A.T.newyork                     0.42865891   FALSE  TRUE    FALSE
## H.T.today                       0.13778322   FALSE  TRUE    FALSE
## H.T.daili                       0.16840171   FALSE  TRUE    FALSE
## H.T.report                      0.16840171   FALSE  TRUE    FALSE
## S.T.newyork                     0.41334966   FALSE  TRUE    FALSE
## H.T.day                         0.18371096   FALSE  TRUE    FALSE
## A.T.intern                      0.35211268   FALSE  TRUE    FALSE
## A.T.scene                       0.26025720   FALSE  TRUE    FALSE
## S.T.scene                       0.26025720   FALSE  TRUE    FALSE
## H.npnct14.log                   0.03061849   FALSE  TRUE    FALSE
## S.T.diari                       0.18371096   FALSE  TRUE    FALSE
## S.T.highlight                   0.27556644   FALSE  TRUE    FALSE
## A.npnct04.log                   0.07654623   FALSE  TRUE    FALSE
## S.npnct04.log                   0.07654623   FALSE  TRUE    FALSE
## H.T.X2015                       0.12247397   FALSE  TRUE    FALSE
## S.npnct15.log                   0.04592774   FALSE FALSE    FALSE
## S.T.tribun                      0.21432945   FALSE  TRUE    FALSE
## A.npnct15.log                   0.04592774   FALSE FALSE    FALSE
## S.T.intern                      0.32149418   FALSE  TRUE    FALSE
## A.T.photo                       0.29087569   FALSE  TRUE    FALSE
## H.T.week                        0.16840171   FALSE  TRUE    FALSE
## A.P.fashion.week                0.03061849   FALSE  TRUE    FALSE
## S.P.fashion.week                0.03061849   FALSE  TRUE    FALSE
## H.P.fashion.week                0.03061849   FALSE  TRUE    FALSE
## H.P.year.colon                  0.03061849   FALSE  TRUE    FALSE
## H.T.fashion                     0.19902021   FALSE  TRUE    FALSE
## H.npnct15.log                   0.04592774   FALSE FALSE    FALSE
## S.T.fashion                     0.41334966   FALSE  TRUE    FALSE
## A.T.fashion                     0.41334966   FALSE  TRUE    FALSE
## A.T.week                        0.50520514   FALSE  TRUE    FALSE
## S.T.week                        0.44396816   FALSE  TRUE    FALSE
## H.npnct28.log                   0.03061849   FALSE  TRUE    FALSE
## S.npnct11.log                   0.13778322   FALSE FALSE    FALSE
## A.npnct11.log                   0.13778322   FALSE FALSE    FALSE
## H.ndgts.log                     0.18371096   FALSE FALSE    FALSE
## S.ndgts.log                     0.26025720   FALSE FALSE    FALSE
## A.ndgts.log                     0.29087569   FALSE FALSE    FALSE
## H.nuppr.log                     0.29087569   FALSE FALSE    FALSE
## H.nchrs.log                     1.57685242   FALSE FALSE    FALSE
## H.nwrds.unq.log                 0.21432945   FALSE FALSE    FALSE
## A.nchrs.log                     4.39375383   FALSE FALSE    FALSE
## S.nchrs.log                     3.72014697   FALSE FALSE    FALSE
## A.nwrds.unq.log                 0.55113288   FALSE FALSE    FALSE
## S.nwrds.unq.log                 0.44396816   FALSE FALSE    FALSE
## S.nuppr.log                     0.33680343   FALSE FALSE    FALSE
## A.nuppr.log                     0.33680343   FALSE FALSE    FALSE
## A.npnct05.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct09.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct22.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct26.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct27.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct29.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct30.log                   0.01530925    TRUE  TRUE     TRUE
## clusterid                       0.01530925    TRUE  TRUE     TRUE
## H.npnct09.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct17.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct18.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct21.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct22.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct23.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct25.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct26.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct27.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct29.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct30.log                   0.01530925    TRUE  TRUE     TRUE
## H.P.http                        0.01530925    TRUE  TRUE     TRUE
## PubDate.year.fctr               0.01530925    TRUE  TRUE     TRUE
## S.npnct05.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct09.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct17.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct18.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct22.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct25.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct26.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct27.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct29.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct30.log                   0.01530925    TRUE  TRUE     TRUE
## S.P.http                        0.01530925    TRUE  TRUE     TRUE
##                              is.cor.y.abs.low
## Popular                                 FALSE
## WordCount.log                           FALSE
## WordCount                               FALSE
## H.nwrds.log                             FALSE
## PubDate.hour.fctr                       FALSE
## H.npnct19.log                           FALSE
## S.nwrds.log                             FALSE
## A.nwrds.log                             FALSE
## PubDate.wkend                           FALSE
## H.P.recap.colon                         FALSE
## H.P.quandary                            FALSE
## H.P.no.comment.colon                    FALSE
## S.npnct19.log                           FALSE
## A.npnct19.log                           FALSE
## PubDate.last10                          FALSE
## H.npnct08.log                           FALSE
## PubDate.last10.log                      FALSE
## PubDate.last1.log                       FALSE
## H.P.readers.respond                     FALSE
## PubDate.last100                         FALSE
## S.T.make                                FALSE
## PubDate.last1                           FALSE
## H.T.get                                 FALSE
## H.npnct06.log                           FALSE
## A.npnct01.log                           FALSE
## S.npnct01.log                           FALSE
## S.T.can                                 FALSE
## H.npnct16.log                           FALSE
## A.T.can                                 FALSE
## S.npnct21.log                           FALSE
## S.npnct23.log                           FALSE
## H.T.ebola                               FALSE
## H.npnct01.log                           FALSE
## PubDate.month.fctr                      FALSE
## A.T.said                                FALSE
## S.T.said                                FALSE
## PubDate.POSIX                           FALSE
## PubDate.zoo                             FALSE
## A.npnct21.log                           FALSE
## A.npnct23.log                           FALSE
## H.T.make                                FALSE
## H.npnct11.log                           FALSE
## myCategory.fctr                         FALSE
## UniqueID                                FALSE
## A.T.one                                 FALSE
## A.T.state                               FALSE
## S.T.state                               FALSE
## S.T.one                                 FALSE
## H.npnct03.log                           FALSE
## H.P.s.notebook                           TRUE
## A.npnct24.log                            TRUE
## H.npnct24.log                            TRUE
## S.npnct24.log                            TRUE
## H.T.take                                 TRUE
## A.npnct16.log                            TRUE
## S.npnct16.log                            TRUE
## H.T.time                                 TRUE
## S.T.presid                               TRUE
## S.npnct08.log                            TRUE
## A.npnct08.log                            TRUE
## A.npnct25.log                            TRUE
## A.npnct10.log                            TRUE
## H.npnct10.log                            TRUE
## H.npnct20.log                            TRUE
## S.npnct02.log                            TRUE
## S.npnct10.log                            TRUE
## PubDate.last100.log                      TRUE
## .rnorm                                  FALSE
## H.npnct05.log                           FALSE
## H.T.obama                               FALSE
## H.T.say                                 FALSE
## H.T.bank                                FALSE
## PubDate.date.fctr                       FALSE
## PubDate.second.fctr                     FALSE
## H.npnct07.log                           FALSE
## A.npnct07.log                           FALSE
## S.npnct07.log                           FALSE
## S.npnct03.log                           FALSE
## A.npnct18.log                           FALSE
## A.P.http                                FALSE
## H.npnct12.log                           FALSE
## A.npnct03.log                           FALSE
## H.T.word                                FALSE
## H.T.big                                 FALSE
## A.npnct02.log                           FALSE
## A.npnct17.log                           FALSE
## A.P.year.colon                          FALSE
## S.P.year.colon                          FALSE
## A.npnct20.log                           FALSE
## S.npnct20.log                           FALSE
## S.T.obama                               FALSE
## H.npnct02.log                           FALSE
## A.T.obama                               FALSE
## H.T.test                                FALSE
## S.npnct14.log                           FALSE
## A.P.first.draft                         FALSE
## H.P.on.this.day                         FALSE
## S.P.first.draft                         FALSE
## A.T.take                                FALSE
## S.T.take                                FALSE
## A.npnct06.log                           FALSE
## S.npnct06.log                           FALSE
## A.npnct14.log                           FALSE
## H.npnct13.log                           FALSE
## H.T.deal                                FALSE
## A.T.new                                 FALSE
## S.T.new                                 FALSE
## A.P.metropolitan.diary.colon            FALSE
## S.P.metropolitan.diary.colon            FALSE
## H.T.billion                             FALSE
## H.T.polit                               FALSE
## H.T.china                               FALSE
## H.P.verbatim.colon                      FALSE
## H.T.art                                 FALSE
## PubDate.minute.fctr                     FALSE
## H.T.read                                FALSE
## A.T.year                                FALSE
## S.npnct12.log                           FALSE
## H.P.today.in.politic                    FALSE
## A.npnct12.log                           FALSE
## H.P.what.we.are                         FALSE
## A.T.will                                FALSE
## S.T.will                                FALSE
## A.T.appear                              FALSE
## PubDate.wkday.fctr                      FALSE
## H.T.pictur                              FALSE
## A.T.senat                               FALSE
## S.T.day                                 FALSE
## S.T.show                                FALSE
## H.P.today.in.smallbusiness              FALSE
## S.T.photo                               FALSE
## H.P.first.draft                         FALSE
## H.T.new                                 FALSE
## S.npnct28.log                           FALSE
## A.npnct28.log                           FALSE
## A.P.daily.clip.report                   FALSE
## H.P.daily.clip.report                   FALSE
## S.P.daily.clip.report                   FALSE
## A.T.day                                 FALSE
## H.T.news                                FALSE
## H.T.first                               FALSE
## S.T.first                               FALSE
## H.T.X2014                               FALSE
## S.T.past                                FALSE
## A.T.compani                             FALSE
## S.T.compani                             FALSE
## S.T.report                              FALSE
## A.T.word                                FALSE
## H.T.morn                                FALSE
## A.T.report                              FALSE
## A.T.editor                              FALSE
## S.T.word                                FALSE
## H.T.busi                                FALSE
## A.npnct13.log                           FALSE
## S.T.share                               FALSE
## H.npnct04.log                           FALSE
## S.npnct13.log                           FALSE
## S.T.articl                              FALSE
## A.T.time                                FALSE
## S.T.time                                FALSE
## H.T.newyork                             FALSE
## A.T.newyork                             FALSE
## H.T.today                               FALSE
## H.T.daili                               FALSE
## H.T.report                              FALSE
## S.T.newyork                             FALSE
## H.T.day                                 FALSE
## A.T.intern                              FALSE
## A.T.scene                               FALSE
## S.T.scene                               FALSE
## H.npnct14.log                           FALSE
## S.T.diari                               FALSE
## S.T.highlight                           FALSE
## A.npnct04.log                           FALSE
## S.npnct04.log                           FALSE
## H.T.X2015                               FALSE
## S.npnct15.log                           FALSE
## S.T.tribun                              FALSE
## A.npnct15.log                           FALSE
## S.T.intern                              FALSE
## A.T.photo                               FALSE
## H.T.week                                FALSE
## A.P.fashion.week                        FALSE
## S.P.fashion.week                        FALSE
## H.P.fashion.week                        FALSE
## H.P.year.colon                          FALSE
## H.T.fashion                             FALSE
## H.npnct15.log                           FALSE
## S.T.fashion                             FALSE
## A.T.fashion                             FALSE
## A.T.week                                FALSE
## S.T.week                                FALSE
## H.npnct28.log                           FALSE
## S.npnct11.log                           FALSE
## A.npnct11.log                           FALSE
## H.ndgts.log                             FALSE
## S.ndgts.log                             FALSE
## A.ndgts.log                             FALSE
## H.nuppr.log                             FALSE
## H.nchrs.log                             FALSE
## H.nwrds.unq.log                         FALSE
## A.nchrs.log                             FALSE
## S.nchrs.log                             FALSE
## A.nwrds.unq.log                         FALSE
## S.nwrds.unq.log                         FALSE
## S.nuppr.log                             FALSE
## A.nuppr.log                             FALSE
## A.npnct05.log                              NA
## A.npnct09.log                              NA
## A.npnct22.log                              NA
## A.npnct26.log                              NA
## A.npnct27.log                              NA
## A.npnct29.log                              NA
## A.npnct30.log                              NA
## clusterid                                  NA
## H.npnct09.log                              NA
## H.npnct17.log                              NA
## H.npnct18.log                              NA
## H.npnct21.log                              NA
## H.npnct22.log                              NA
## H.npnct23.log                              NA
## H.npnct25.log                              NA
## H.npnct26.log                              NA
## H.npnct27.log                              NA
## H.npnct29.log                              NA
## H.npnct30.log                              NA
## H.P.http                                   NA
## PubDate.year.fctr                          NA
## S.npnct05.log                              NA
## S.npnct09.log                              NA
## S.npnct17.log                              NA
## S.npnct18.log                              NA
## S.npnct22.log                              NA
## S.npnct25.log                              NA
## S.npnct26.log                              NA
## S.npnct27.log                              NA
## S.npnct29.log                              NA
## S.npnct30.log                              NA
## S.P.http                                   NA
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
## Warning: Removed 13 rows containing missing values (geom_point).
```

```
## Warning: Removed 13 rows containing missing values (geom_point).
```

```
## Warning: Removed 13 rows containing missing values (geom_point).
```

![](NYTBlogs_clusters_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##                                  id         cor.y exclude.as.feat
## S.npnct21.log         S.npnct21.log  2.760321e-02               0
## S.npnct23.log         S.npnct23.log  2.760321e-02               0
## A.npnct21.log         A.npnct21.log  1.537569e-02               0
## A.npnct23.log         A.npnct23.log  1.537569e-02               0
## H.npnct03.log         H.npnct03.log  9.533020e-03               0
## A.npnct24.log         A.npnct24.log -9.890046e-19               0
## H.npnct24.log         H.npnct24.log -9.890046e-19               0
## S.npnct24.log         S.npnct24.log -9.890046e-19               0
## A.npnct25.log         A.npnct25.log -5.547032e-03               0
## A.npnct10.log         A.npnct10.log -5.547032e-03               0
## H.npnct10.log         H.npnct10.log -5.547032e-03               0
## H.npnct20.log         H.npnct20.log -5.547032e-03               0
## S.npnct02.log         S.npnct02.log -5.547032e-03               0
## S.npnct10.log         S.npnct10.log -5.547032e-03               0
## A.npnct05.log         A.npnct05.log            NA               0
## A.npnct09.log         A.npnct09.log            NA               0
## A.npnct22.log         A.npnct22.log            NA               0
## A.npnct26.log         A.npnct26.log            NA               0
## A.npnct27.log         A.npnct27.log            NA               0
## A.npnct29.log         A.npnct29.log            NA               0
## A.npnct30.log         A.npnct30.log            NA               0
## clusterid                 clusterid            NA               0
## H.npnct09.log         H.npnct09.log            NA               0
## H.npnct17.log         H.npnct17.log            NA               0
## H.npnct18.log         H.npnct18.log            NA               0
## H.npnct21.log         H.npnct21.log            NA               0
## H.npnct22.log         H.npnct22.log            NA               0
## H.npnct23.log         H.npnct23.log            NA               0
## H.npnct25.log         H.npnct25.log            NA               0
## H.npnct26.log         H.npnct26.log            NA               0
## H.npnct27.log         H.npnct27.log            NA               0
## H.npnct29.log         H.npnct29.log            NA               0
## H.npnct30.log         H.npnct30.log            NA               0
## H.P.http                   H.P.http            NA               0
## PubDate.year.fctr PubDate.year.fctr            NA               0
## S.npnct05.log         S.npnct05.log            NA               0
## S.npnct09.log         S.npnct09.log            NA               0
## S.npnct17.log         S.npnct17.log            NA               0
## S.npnct18.log         S.npnct18.log            NA               0
## S.npnct22.log         S.npnct22.log            NA               0
## S.npnct25.log         S.npnct25.log            NA               0
## S.npnct26.log         S.npnct26.log            NA               0
## S.npnct27.log         S.npnct27.log            NA               0
## S.npnct29.log         S.npnct29.log            NA               0
## S.npnct30.log         S.npnct30.log            NA               0
## S.P.http                   S.P.http            NA               0
##                      cor.y.abs    cor.high.X freqRatio percentUnique
## S.npnct21.log     2.760321e-02 A.npnct21.log  6531.000    0.03061849
## S.npnct23.log     2.760321e-02          <NA>  6531.000    0.03061849
## A.npnct21.log     1.537569e-02 A.npnct23.log  3264.500    0.04592774
## A.npnct23.log     1.537569e-02          <NA>  3264.500    0.04592774
## H.npnct03.log     9.533020e-03          <NA>  2176.333    0.03061849
## A.npnct24.log     9.890046e-19          <NA>     0.000    0.01530925
## H.npnct24.log     9.890046e-19          <NA>     0.000    0.01530925
## S.npnct24.log     9.890046e-19          <NA>     0.000    0.01530925
## A.npnct25.log     5.547032e-03          <NA>  6531.000    0.03061849
## A.npnct10.log     5.547032e-03          <NA>  6531.000    0.03061849
## H.npnct10.log     5.547032e-03          <NA>  6531.000    0.03061849
## H.npnct20.log     5.547032e-03          <NA>  6531.000    0.03061849
## S.npnct02.log     5.547032e-03          <NA>  6531.000    0.03061849
## S.npnct10.log     5.547032e-03          <NA>  6531.000    0.03061849
## A.npnct05.log               NA          <NA>     0.000    0.01530925
## A.npnct09.log               NA          <NA>     0.000    0.01530925
## A.npnct22.log               NA          <NA>     0.000    0.01530925
## A.npnct26.log               NA          <NA>     0.000    0.01530925
## A.npnct27.log               NA          <NA>     0.000    0.01530925
## A.npnct29.log               NA          <NA>     0.000    0.01530925
## A.npnct30.log               NA          <NA>     0.000    0.01530925
## clusterid                   NA          <NA>     0.000    0.01530925
## H.npnct09.log               NA          <NA>     0.000    0.01530925
## H.npnct17.log               NA          <NA>     0.000    0.01530925
## H.npnct18.log               NA          <NA>     0.000    0.01530925
## H.npnct21.log               NA          <NA>     0.000    0.01530925
## H.npnct22.log               NA          <NA>     0.000    0.01530925
## H.npnct23.log               NA          <NA>     0.000    0.01530925
## H.npnct25.log               NA          <NA>     0.000    0.01530925
## H.npnct26.log               NA          <NA>     0.000    0.01530925
## H.npnct27.log               NA          <NA>     0.000    0.01530925
## H.npnct29.log               NA          <NA>     0.000    0.01530925
## H.npnct30.log               NA          <NA>     0.000    0.01530925
## H.P.http                    NA          <NA>     0.000    0.01530925
## PubDate.year.fctr           NA          <NA>     0.000    0.01530925
## S.npnct05.log               NA          <NA>     0.000    0.01530925
## S.npnct09.log               NA          <NA>     0.000    0.01530925
## S.npnct17.log               NA          <NA>     0.000    0.01530925
## S.npnct18.log               NA          <NA>     0.000    0.01530925
## S.npnct22.log               NA          <NA>     0.000    0.01530925
## S.npnct25.log               NA          <NA>     0.000    0.01530925
## S.npnct26.log               NA          <NA>     0.000    0.01530925
## S.npnct27.log               NA          <NA>     0.000    0.01530925
## S.npnct29.log               NA          <NA>     0.000    0.01530925
## S.npnct30.log               NA          <NA>     0.000    0.01530925
## S.P.http                    NA          <NA>     0.000    0.01530925
##                   zeroVar  nzv myNearZV is.cor.y.abs.low
## S.npnct21.log       FALSE TRUE     TRUE            FALSE
## S.npnct23.log       FALSE TRUE     TRUE            FALSE
## A.npnct21.log       FALSE TRUE     TRUE            FALSE
## A.npnct23.log       FALSE TRUE     TRUE            FALSE
## H.npnct03.log       FALSE TRUE     TRUE            FALSE
## A.npnct24.log        TRUE TRUE     TRUE             TRUE
## H.npnct24.log        TRUE TRUE     TRUE             TRUE
## S.npnct24.log        TRUE TRUE     TRUE             TRUE
## A.npnct25.log       FALSE TRUE     TRUE             TRUE
## A.npnct10.log       FALSE TRUE     TRUE             TRUE
## H.npnct10.log       FALSE TRUE     TRUE             TRUE
## H.npnct20.log       FALSE TRUE     TRUE             TRUE
## S.npnct02.log       FALSE TRUE     TRUE             TRUE
## S.npnct10.log       FALSE TRUE     TRUE             TRUE
## A.npnct05.log        TRUE TRUE     TRUE               NA
## A.npnct09.log        TRUE TRUE     TRUE               NA
## A.npnct22.log        TRUE TRUE     TRUE               NA
## A.npnct26.log        TRUE TRUE     TRUE               NA
## A.npnct27.log        TRUE TRUE     TRUE               NA
## A.npnct29.log        TRUE TRUE     TRUE               NA
## A.npnct30.log        TRUE TRUE     TRUE               NA
## clusterid            TRUE TRUE     TRUE               NA
## H.npnct09.log        TRUE TRUE     TRUE               NA
## H.npnct17.log        TRUE TRUE     TRUE               NA
## H.npnct18.log        TRUE TRUE     TRUE               NA
## H.npnct21.log        TRUE TRUE     TRUE               NA
## H.npnct22.log        TRUE TRUE     TRUE               NA
## H.npnct23.log        TRUE TRUE     TRUE               NA
## H.npnct25.log        TRUE TRUE     TRUE               NA
## H.npnct26.log        TRUE TRUE     TRUE               NA
## H.npnct27.log        TRUE TRUE     TRUE               NA
## H.npnct29.log        TRUE TRUE     TRUE               NA
## H.npnct30.log        TRUE TRUE     TRUE               NA
## H.P.http             TRUE TRUE     TRUE               NA
## PubDate.year.fctr    TRUE TRUE     TRUE               NA
## S.npnct05.log        TRUE TRUE     TRUE               NA
## S.npnct09.log        TRUE TRUE     TRUE               NA
## S.npnct17.log        TRUE TRUE     TRUE               NA
## S.npnct18.log        TRUE TRUE     TRUE               NA
## S.npnct22.log        TRUE TRUE     TRUE               NA
## S.npnct25.log        TRUE TRUE     TRUE               NA
## S.npnct26.log        TRUE TRUE     TRUE               NA
## S.npnct27.log        TRUE TRUE     TRUE               NA
## S.npnct29.log        TRUE TRUE     TRUE               NA
## S.npnct30.log        TRUE TRUE     TRUE               NA
## S.P.http             TRUE TRUE     TRUE               NA
```

```r
glb_entity_df <- glb_entity_df[, setdiff(names(glb_entity_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor     bgn     end elapsed
## 8         select.features          5          0 125.885 210.291  84.406
## 9 partition.data.training          6          0 210.292      NA      NA
```

## Step `6.0: partition data training`

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
## [1] 239  11
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
##  [1] "S.npnct21.log"     "S.npnct23.log"     "A.npnct21.log"    
##  [4] "A.npnct23.log"     "H.npnct03.log"     "A.npnct24.log"    
##  [7] "H.npnct24.log"     "S.npnct24.log"     "A.npnct25.log"    
## [10] "A.npnct10.log"     "H.npnct10.log"     "H.npnct20.log"    
## [13] "S.npnct02.log"     "S.npnct10.log"     "A.npnct05.log"    
## [16] "A.npnct09.log"     "A.npnct22.log"     "A.npnct26.log"    
## [19] "A.npnct27.log"     "A.npnct29.log"     "A.npnct30.log"    
## [22] "clusterid"         "H.npnct09.log"     "H.npnct17.log"    
## [25] "H.npnct18.log"     "H.npnct21.log"     "H.npnct22.log"    
## [28] "H.npnct23.log"     "H.npnct25.log"     "H.npnct26.log"    
## [31] "H.npnct27.log"     "H.npnct29.log"     "H.npnct30.log"    
## [34] "H.P.http"          "PubDate.year.fctr" "S.npnct05.log"    
## [37] "S.npnct09.log"     "S.npnct17.log"     "S.npnct18.log"    
## [40] "S.npnct22.log"     "S.npnct25.log"     "S.npnct26.log"    
## [43] "S.npnct27.log"     "S.npnct29.log"     "S.npnct30.log"    
## [46] "S.P.http"
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
## [1] 8402  204
```

```r
print("glb_trnent_df: "); print(dim(glb_trnent_df))
```

```
## [1] "glb_trnent_df: "
```

```
## [1] 6532  249
```

```r
print("glb_fitent_df: "); print(dim(glb_fitent_df))
```

```
## [1] "glb_fitent_df: "
```

```
## [1] 4475  249
```

```r
print("glb_OOBent_df: "); print(dim(glb_OOBent_df))
```

```
## [1] "glb_OOBent_df: "
```

```
## [1] 2057  249
```

```r
print("glb_newent_df: "); print(dim(glb_newent_df))
```

```
## [1] "glb_newent_df: "
```

```
## [1] 1870  249
```

```r
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

rm(split)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor     bgn     end elapsed
## 9  partition.data.training          6          0 210.292 211.669   1.377
## 10              fit.models          7          0 211.670      NA      NA
```

## Step `7.0: fit models`

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
## 1                      0.747                 0.003         0.5
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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-1.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-2.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-3.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-4.png) 

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
## 1                      0.253                 0.002   0.5007516
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
## 1                      0.595                 0.053         0.5
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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-5.png) 

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
## 1                      0.513                 0.054         0.5
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
## 1                      1.106                 0.053         0.5
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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-6.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_0-7.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_0-8.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_0-9.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-10.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-11.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-12.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-13.png) 

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
## 1                      1.119                 0.077   0.7073742
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
## [1] "    indep_vars: A.nuppr.log, A.nuppr.log:A.nwrds.log, A.nuppr.log:A.npnct19.log, A.nuppr.log:H.npnct16.log, A.nuppr.log:S.npnct01.log, A.nuppr.log:A.T.can, A.nuppr.log:A.npnct21.log, A.nuppr.log:S.T.said, A.nuppr.log:A.npnct23.log, A.nuppr.log:S.T.one, A.nuppr.log:S.T.state, A.nuppr.log:S.npnct07.log, A.nuppr.log:A.npnct18.log, A.nuppr.log:S.npnct03.log, A.nuppr.log:A.P.http, A.nuppr.log:A.npnct02.log, A.nuppr.log:S.P.year.colon, A.nuppr.log:S.npnct20.log, A.nuppr.log:S.T.obama, A.nuppr.log:S.P.first.draft, A.nuppr.log:A.T.take, A.nuppr.log:S.npnct06.log, A.nuppr.log:A.npnct17.log, A.nuppr.log:A.T.new, A.nuppr.log:S.P.metropolitan.diary.colon, A.nuppr.log:H.T.polit, A.nuppr.log:S.npnct12.log, A.nuppr.log:H.T.read, A.nuppr.log:A.T.will, A.nuppr.log:H.T.pictur, A.nuppr.log:S.npnct28.log, A.nuppr.log:S.P.daily.clip.report, A.nuppr.log:S.T.day, A.nuppr.log:H.P.first.draft, A.nuppr.log:A.T.appear, A.nuppr.log:A.T.compani, A.nuppr.log:A.npnct28.log, A.nuppr.log:S.T.report, A.nuppr.log:A.T.word, A.nuppr.log:H.T.billion, A.nuppr.log:A.npnct13.log, A.nuppr.log:S.T.past, A.nuppr.log:A.T.time, A.nuppr.log:H.P.today.in.politic, A.nuppr.log:A.P.daily.clip.report, A.nuppr.log:H.T.daili, A.nuppr.log:A.T.newyork, A.nuppr.log:A.T.scene, A.nuppr.log:S.T.scene, A.nuppr.log:S.npnct04.log, A.nuppr.log:S.T.diari, A.nuppr.log:S.npnct15.log, A.nuppr.log:S.T.tribun, A.nuppr.log:S.T.photo, A.nuppr.log:S.P.fashion.week, A.nuppr.log:S.T.intern, A.nuppr.log:H.P.fashion.week, A.nuppr.log:H.T.X2015, A.nuppr.log:A.T.week, A.nuppr.log:S.npnct11.log, A.nuppr.log:S.ndgts.log, A.nuppr.log:H.nuppr.log, A.nuppr.log:A.nchrs.log, A.nuppr.log:S.nchrs.log, A.nuppr.log:S.nuppr.log"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: not plotting observations with leverage one:
##   1143, 3637, 4105
```

![](NYTBlogs_clusters_files/figure-html/fit.models_0-14.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_0-15.png) 

```
## Warning: not plotting observations with leverage one:
##   1143, 3637, 4105
```

![](NYTBlogs_clusters_files/figure-html/fit.models_0-16.png) 

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
## -1.7337  -0.6552  -0.3421   0.0000   3.0801  
## 
## Coefficients: (2 not defined because of singularities)
##                                              Estimate Std. Error z value
## (Intercept)                                -4.645e-01  3.126e-01  -1.486
## A.nuppr.log                                 3.306e+00  9.802e-01   3.373
## `A.nuppr.log:A.nwrds.log`                   2.422e-01  3.189e-01   0.760
## `A.nuppr.log:A.npnct19.log`                 5.410e-01  1.376e-01   3.930
## `A.nuppr.log:H.npnct16.log`                 7.535e-01  2.559e-01   2.944
## `A.nuppr.log:S.npnct01.log`                 9.005e-01  4.711e-01   1.911
## `A.nuppr.log:A.T.can`                       1.403e-01  4.108e-01   0.342
## `A.nuppr.log:A.npnct21.log`                -9.641e+00  6.888e+03  -0.001
## `A.nuppr.log:S.T.said`                      1.060e+00  4.019e-01   2.638
## `A.nuppr.log:A.npnct23.log`                        NA         NA      NA
## `A.nuppr.log:S.T.one`                      -3.706e-02  3.658e-01  -0.101
## `A.nuppr.log:S.T.state`                     1.254e+00  3.561e-01   3.522
## `A.nuppr.log:S.npnct07.log`                -3.642e+01  9.181e+03  -0.004
## `A.nuppr.log:A.npnct18.log`                 4.495e+01  2.749e+04   0.002
## `A.nuppr.log:S.npnct03.log`                -1.261e+01  4.958e+03  -0.003
## `A.nuppr.log:A.P.http`                     -3.071e+01  3.019e+04  -0.001
## `A.nuppr.log:A.npnct02.log`                -1.087e+01  9.007e+03  -0.001
## `A.nuppr.log:S.P.year.colon`               -1.237e+01  2.480e+03  -0.005
## `A.nuppr.log:S.npnct20.log`                -1.632e+01  4.731e+03  -0.003
## `A.nuppr.log:S.T.obama`                    -4.219e-01  3.322e-01  -1.270
## `A.nuppr.log:S.P.first.draft`              -8.846e+00  1.946e+03  -0.005
## `A.nuppr.log:A.T.take`                     -1.019e+00  5.670e-01  -1.797
## `A.nuppr.log:S.npnct06.log`                -8.287e-01  8.382e-01  -0.989
## `A.nuppr.log:A.npnct17.log`                -9.103e+00  7.947e+03  -0.001
## `A.nuppr.log:A.T.new`                      -7.046e-01  4.016e-01  -1.754
## `A.nuppr.log:S.P.metropolitan.diary.colon` -4.389e+00  2.560e+00  -1.715
## `A.nuppr.log:H.T.polit`                    -4.866e-01  3.490e-01  -1.394
## `A.nuppr.log:S.npnct12.log`                -7.900e-02  1.013e-01  -0.780
## `A.nuppr.log:H.T.read`                     -3.573e-01  2.075e-01  -1.722
## `A.nuppr.log:A.T.will`                     -1.334e+00  4.028e-01  -3.312
## `A.nuppr.log:H.T.pictur`                   -8.213e-02  2.412e-01  -0.341
## `A.nuppr.log:S.npnct28.log`                -1.599e+01  1.741e+04  -0.001
## `A.nuppr.log:S.P.daily.clip.report`         9.725e+01  7.405e+03   0.013
## `A.nuppr.log:S.T.day`                      -9.484e-01  5.716e-01  -1.659
## `A.nuppr.log:H.P.first.draft`              -2.096e+01  1.513e+03  -0.014
## `A.nuppr.log:A.T.appear`                   -4.812e-01  5.103e-01  -0.943
## `A.nuppr.log:A.T.compani`                  -1.461e+00  5.680e-01  -2.572
## `A.nuppr.log:A.npnct28.log`                 6.848e+00  1.714e+04   0.000
## `A.nuppr.log:S.T.report`                   -1.353e+00  7.034e-01  -1.924
## `A.nuppr.log:A.T.word`                     -2.454e+00  7.771e-01  -3.158
## `A.nuppr.log:H.T.billion`                  -6.426e-01  5.623e-01  -1.143
## `A.nuppr.log:A.npnct13.log`                 7.358e-01  1.099e-01   6.693
## `A.nuppr.log:S.T.past`                     -2.715e-01  6.993e-01  -0.388
## `A.nuppr.log:A.T.time`                     -3.977e-01  3.707e-01  -1.073
## `A.nuppr.log:H.P.today.in.politic`         -2.047e+01  2.257e+03  -0.009
## `A.nuppr.log:A.P.daily.clip.report`                NA         NA      NA
## `A.nuppr.log:H.T.daili`                    -4.051e+01  2.748e+03  -0.015
## `A.nuppr.log:A.T.newyork`                   9.127e-01  3.531e-01   2.585
## `A.nuppr.log:A.T.scene`                     2.915e+02  4.574e+04   0.006
## `A.nuppr.log:S.T.scene`                    -3.298e+02  4.632e+04  -0.007
## `A.nuppr.log:S.npnct04.log`                -1.108e+00  4.325e-01  -2.563
## `A.nuppr.log:S.T.diari`                     8.332e+00  5.461e+00   1.526
## `A.nuppr.log:S.npnct15.log`                -4.369e-02  2.346e-01  -0.186
## `A.nuppr.log:S.T.tribun`                   -2.104e+01  1.444e+03  -0.015
## `A.nuppr.log:S.T.photo`                    -3.043e+00  1.385e+00  -2.197
## `A.nuppr.log:S.P.fashion.week`             -8.563e+00  6.549e+02  -0.013
## `A.nuppr.log:S.T.intern`                   -1.078e+00  1.303e+00  -0.827
## `A.nuppr.log:H.P.fashion.week`             -1.827e+01  7.748e+02  -0.024
## `A.nuppr.log:H.T.X2015`                    -2.050e+01  8.957e+02  -0.023
## `A.nuppr.log:A.T.week`                     -2.094e+00  5.276e-01  -3.969
## `A.nuppr.log:S.npnct11.log`                 6.813e-02  7.051e-02   0.966
## `A.nuppr.log:S.ndgts.log`                  -2.607e-01  7.400e-02  -3.523
## `A.nuppr.log:H.nuppr.log`                  -6.670e-01  9.949e-02  -6.704
## `A.nuppr.log:A.nchrs.log`                  -1.939e-01  2.711e+00  -0.072
## `A.nuppr.log:S.nchrs.log`                  -4.263e-01  2.718e+00  -0.157
## `A.nuppr.log:S.nuppr.log`                  -4.418e-01  1.817e-01  -2.432
##                                            Pr(>|z|)    
## (Intercept)                                0.137241    
## A.nuppr.log                                0.000745 ***
## `A.nuppr.log:A.nwrds.log`                  0.447519    
## `A.nuppr.log:A.npnct19.log`                8.48e-05 ***
## `A.nuppr.log:H.npnct16.log`                0.003237 ** 
## `A.nuppr.log:S.npnct01.log`                0.055968 .  
## `A.nuppr.log:A.T.can`                      0.732699    
## `A.nuppr.log:A.npnct21.log`                0.998883    
## `A.nuppr.log:S.T.said`                     0.008349 ** 
## `A.nuppr.log:A.npnct23.log`                      NA    
## `A.nuppr.log:S.T.one`                      0.919308    
## `A.nuppr.log:S.T.state`                    0.000428 ***
## `A.nuppr.log:S.npnct07.log`                0.996835    
## `A.nuppr.log:A.npnct18.log`                0.998695    
## `A.nuppr.log:S.npnct03.log`                0.997970    
## `A.nuppr.log:A.P.http`                     0.999188    
## `A.nuppr.log:A.npnct02.log`                0.999037    
## `A.nuppr.log:S.P.year.colon`               0.996022    
## `A.nuppr.log:S.npnct20.log`                0.997248    
## `A.nuppr.log:S.T.obama`                    0.204157    
## `A.nuppr.log:S.P.first.draft`              0.996373    
## `A.nuppr.log:A.T.take`                     0.072398 .  
## `A.nuppr.log:S.npnct06.log`                0.322816    
## `A.nuppr.log:A.npnct17.log`                0.999086    
## `A.nuppr.log:A.T.new`                      0.079357 .  
## `A.nuppr.log:S.P.metropolitan.diary.colon` 0.086434 .  
## `A.nuppr.log:H.T.polit`                    0.163234    
## `A.nuppr.log:S.npnct12.log`                0.435556    
## `A.nuppr.log:H.T.read`                     0.085024 .  
## `A.nuppr.log:A.T.will`                     0.000928 ***
## `A.nuppr.log:H.T.pictur`                   0.733439    
## `A.nuppr.log:S.npnct28.log`                0.999267    
## `A.nuppr.log:S.P.daily.clip.report`        0.989521    
## `A.nuppr.log:S.T.day`                      0.097047 .  
## `A.nuppr.log:H.P.first.draft`              0.988952    
## `A.nuppr.log:A.T.appear`                   0.345711    
## `A.nuppr.log:A.T.compani`                  0.010119 *  
## `A.nuppr.log:A.npnct28.log`                0.999681    
## `A.nuppr.log:S.T.report`                   0.054388 .  
## `A.nuppr.log:A.T.word`                     0.001587 ** 
## `A.nuppr.log:H.T.billion`                  0.253144    
## `A.nuppr.log:A.npnct13.log`                2.19e-11 ***
## `A.nuppr.log:S.T.past`                     0.697844    
## `A.nuppr.log:A.T.time`                     0.283279    
## `A.nuppr.log:H.P.today.in.politic`         0.992762    
## `A.nuppr.log:A.P.daily.clip.report`              NA    
## `A.nuppr.log:H.T.daili`                    0.988237    
## `A.nuppr.log:A.T.newyork`                  0.009751 ** 
## `A.nuppr.log:A.T.scene`                    0.994915    
## `A.nuppr.log:S.T.scene`                    0.994319    
## `A.nuppr.log:S.npnct04.log`                0.010389 *  
## `A.nuppr.log:S.T.diari`                    0.127055    
## `A.nuppr.log:S.npnct15.log`                0.852239    
## `A.nuppr.log:S.T.tribun`                   0.988374    
## `A.nuppr.log:S.T.photo`                    0.028002 *  
## `A.nuppr.log:S.P.fashion.week`             0.989569    
## `A.nuppr.log:S.T.intern`                   0.408100    
## `A.nuppr.log:H.P.fashion.week`             0.981187    
## `A.nuppr.log:H.T.X2015`                    0.981744    
## `A.nuppr.log:A.T.week`                     7.22e-05 ***
## `A.nuppr.log:S.npnct11.log`                0.333947    
## `A.nuppr.log:S.ndgts.log`                  0.000427 ***
## `A.nuppr.log:H.nuppr.log`                  2.03e-11 ***
## `A.nuppr.log:A.nchrs.log`                  0.942989    
## `A.nuppr.log:S.nchrs.log`                  0.875392    
## `A.nuppr.log:S.nuppr.log`                  0.015020 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 3189.9  on 4411  degrees of freedom
## AIC: 3317.9
## 
## Number of Fisher Scoring iterations: 19
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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-17.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_0-18.png) 

```
##    threshold     f.score
## 1        0.0 0.286753446
## 2        0.1 0.408568767
## 3        0.2 0.473595977
## 4        0.3 0.473653049
## 5        0.4 0.388746803
## 6        0.5 0.295652174
## 7        0.6 0.121437423
## 8        0.7 0.028795812
## 9        0.8 0.002666667
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3186
## 2            Y                                            349
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            540
## 2                                            400
##          Prediction
## Reference    N    Y
##         N 3186  540
##         Y  349  400
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.013408e-01   3.531423e-01   7.893440e-01   8.129430e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.860983e-10 
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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-19.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_0-20.png) 

```
##    threshold     f.score
## 1        0.0 0.286547272
## 2        0.1 0.399179768
## 3        0.2 0.458141674
## 4        0.3 0.463730570
## 5        0.4 0.377570093
## 6        0.5 0.278301887
## 7        0.6 0.118598383
## 8        0.7 0.056022409
## 9        0.8 0.005780347
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](NYTBlogs_clusters_files/figure-html/fit.models_0-21.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1464
## 2            Y                                            165
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            249
## 2                                            179
##          Prediction
## Reference    N    Y
##         N 1464  249
##         Y  165  179
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.987360e-01   3.416531e-01   7.807460e-01   8.158705e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   9.999762e-01   4.518561e-05 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1 A.nuppr.log, A.nuppr.log:A.nwrds.log, A.nuppr.log:A.npnct19.log, A.nuppr.log:H.npnct16.log, A.nuppr.log:S.npnct01.log, A.nuppr.log:A.T.can, A.nuppr.log:A.npnct21.log, A.nuppr.log:S.T.said, A.nuppr.log:A.npnct23.log, A.nuppr.log:S.T.one, A.nuppr.log:S.T.state, A.nuppr.log:S.npnct07.log, A.nuppr.log:A.npnct18.log, A.nuppr.log:S.npnct03.log, A.nuppr.log:A.P.http, A.nuppr.log:A.npnct02.log, A.nuppr.log:S.P.year.colon, A.nuppr.log:S.npnct20.log, A.nuppr.log:S.T.obama, A.nuppr.log:S.P.first.draft, A.nuppr.log:A.T.take, A.nuppr.log:S.npnct06.log, A.nuppr.log:A.npnct17.log, A.nuppr.log:A.T.new, A.nuppr.log:S.P.metropolitan.diary.colon, A.nuppr.log:H.T.polit, A.nuppr.log:S.npnct12.log, A.nuppr.log:H.T.read, A.nuppr.log:A.T.will, A.nuppr.log:H.T.pictur, A.nuppr.log:S.npnct28.log, A.nuppr.log:S.P.daily.clip.report, A.nuppr.log:S.T.day, A.nuppr.log:H.P.first.draft, A.nuppr.log:A.T.appear, A.nuppr.log:A.T.compani, A.nuppr.log:A.npnct28.log, A.nuppr.log:S.T.report, A.nuppr.log:A.T.word, A.nuppr.log:H.T.billion, A.nuppr.log:A.npnct13.log, A.nuppr.log:S.T.past, A.nuppr.log:A.T.time, A.nuppr.log:H.P.today.in.politic, A.nuppr.log:A.P.daily.clip.report, A.nuppr.log:H.T.daili, A.nuppr.log:A.T.newyork, A.nuppr.log:A.T.scene, A.nuppr.log:S.T.scene, A.nuppr.log:S.npnct04.log, A.nuppr.log:S.T.diari, A.nuppr.log:S.npnct15.log, A.nuppr.log:S.T.tribun, A.nuppr.log:S.T.photo, A.nuppr.log:S.P.fashion.week, A.nuppr.log:S.T.intern, A.nuppr.log:H.P.fashion.week, A.nuppr.log:H.T.X2015, A.nuppr.log:A.T.week, A.nuppr.log:S.npnct11.log, A.nuppr.log:S.ndgts.log, A.nuppr.log:H.nuppr.log, A.nuppr.log:A.nchrs.log, A.nuppr.log:S.nchrs.log, A.nuppr.log:S.nuppr.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      3.425                 1.367
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1     0.81016                    0.3        0.473653        0.8513966
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.789344              0.812943     0.2402243    0.785924
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.4637306         0.798736
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1              0.780746             0.8158705     0.3416531    3317.941
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.003694869      0.01756053
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
## [1] "    indep_vars: WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, A.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, S.npnct01.log, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, S.T.state, S.T.one, H.P.s.notebook, H.T.take, A.npnct16.log, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, A.npnct08.log, PubDate.last100.log, .rnorm, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, H.T.test, S.npnct14.log, H.P.on.this.day, S.P.first.draft, A.T.take, S.npnct06.log, H.npnct13.log, H.T.deal, A.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, A.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, H.P.first.draft, H.T.new, S.npnct28.log, H.P.daily.clip.report, S.P.daily.clip.report, H.T.news, S.T.first, H.T.X2014, A.T.compani, S.T.report, A.T.word, A.T.editor, H.T.busi, A.npnct13.log, S.T.share, A.T.time, H.T.newyork, A.T.newyork, H.T.day, A.T.intern, A.T.scene, H.npnct14.log, S.T.highlight, S.npnct04.log, S.npnct15.log, S.T.tribun, H.T.week, S.P.fashion.week, H.P.fashion.week, H.npnct15.log, S.T.fashion, A.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, A.nchrs.log, A.nwrds.unq.log, S.nuppr.log"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](NYTBlogs_clusters_files/figure-html/fit.models_0-22.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_0-23.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_0-24.png) 

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
## -3.2541  -0.2961  -0.1167   0.0000   3.5431  
## 
## Coefficients: (3 not defined because of singularities)
##                                                         Estimate
## (Intercept)                                           -1.204e+00
## WordCount.log                                          1.198e+00
## H.nwrds.log                                            5.759e-01
## `PubDate.hour.fctr(7.67,15.3]`                         2.076e-01
## `PubDate.hour.fctr(15.3,23]`                           3.391e-01
## H.npnct19.log                                          1.346e+00
## A.nwrds.log                                           -1.226e+00
## PubDate.wkend                                         -2.851e-01
## H.P.recap.colon                                        1.328e+00
## H.P.quandary                                           2.206e+01
## H.P.no.comment.colon                                   2.371e+00
## A.npnct19.log                                          1.641e+00
## H.npnct08.log                                          1.326e+00
## PubDate.last10.log                                     2.011e-01
## PubDate.last1.log                                     -3.762e-02
## H.P.readers.respond                                    6.883e+00
## S.T.make                                              -1.161e+00
## H.T.get                                                4.199e-01
## S.npnct01.log                                          1.713e+00
## H.npnct16.log                                          7.538e-01
## A.T.can                                               -1.136e+00
## H.T.ebola                                              2.054e-02
## H.npnct01.log                                         -1.581e+00
## S.T.said                                               1.084e+00
## H.T.make                                              -1.114e-01
## H.npnct11.log                                          4.617e-01
## `myCategory.fctrForeign#World#Asia Pacific`           -3.806e+00
## `myCategory.fctr#Multimedia#`                         -4.242e+00
## `myCategory.fctrCulture#Arts#`                        -3.210e+00
## `myCategory.fctrBusiness#Business Day#Dealbook`       -2.641e+00
## myCategory.fctrmyOther                                -2.092e+01
## `myCategory.fctrBusiness#Technology#`                 -1.982e+00
## `myCategory.fctrBusiness#Crosswords/Games#`            6.723e-01
## `myCategory.fctrTStyle##`                             -4.380e+00
## `myCategory.fctrForeign#World#`                       -1.789e+01
## `myCategory.fctrOpEd#Opinion#`                         5.482e-01
## `myCategory.fctrStyles##Fashion`                      -2.070e+01
## `myCategory.fctr#Opinion#Room For Debate`             -7.207e+00
## `myCategory.fctr#U.S.#Education`                      -2.270e+01
## `myCategory.fctr##`                                   -2.653e+00
## `myCategory.fctrMetro#N.Y. / Region#`                 -2.096e+00
## `myCategory.fctrBusiness#Business Day#Small Business` -4.342e+00
## `myCategory.fctrStyles#U.S.#`                         -6.593e-01
## `myCategory.fctrTravel#Travel#`                       -4.107e+00
## `myCategory.fctr#Opinion#The Public Editor`            8.029e-01
## S.T.state                                              1.473e+00
## S.T.one                                               -1.042e+00
## H.P.s.notebook                                        -1.734e+01
## H.T.take                                              -3.269e-01
## A.npnct16.log                                         -9.075e-02
## S.npnct16.log                                                 NA
## H.T.time                                               1.308e-01
## S.T.presid                                             2.548e-01
## S.npnct08.log                                          9.327e-01
## A.npnct08.log                                                 NA
## PubDate.last100.log                                    1.604e-02
## .rnorm                                                -8.863e-02
## H.npnct05.log                                         -2.517e+01
## H.T.obama                                             -2.215e-01
## H.T.say                                               -5.420e-01
## H.T.bank                                               3.457e-01
## `PubDate.date.fctr(7,13]`                             -1.005e-01
## `PubDate.date.fctr(13,19]`                            -1.988e-01
## `PubDate.date.fctr(19,25]`                            -1.533e-01
## `PubDate.date.fctr(25,31]`                             2.708e-02
## `PubDate.second.fctr(14.8,29.5]`                       9.995e-02
## `PubDate.second.fctr(29.5,44.2]`                       2.368e-02
## `PubDate.second.fctr(44.2,59.1]`                      -2.668e-01
## H.npnct07.log                                          2.122e-01
## S.npnct07.log                                         -2.478e+01
## S.npnct03.log                                         -2.938e+01
## A.npnct18.log                                         -2.835e+01
## H.npnct12.log                                          2.653e-01
## H.T.word                                               2.124e+00
## H.T.big                                               -4.310e-01
## S.P.year.colon                                        -1.036e+01
## S.npnct20.log                                         -2.520e+01
## S.T.obama                                             -2.932e-01
## H.npnct02.log                                         -1.833e+01
## H.T.test                                              -1.079e-02
## S.npnct14.log                                          8.324e-01
## H.P.on.this.day                                       -1.534e+01
## S.P.first.draft                                       -1.540e+01
## A.T.take                                              -7.912e-01
## S.npnct06.log                                          2.972e-01
## H.npnct13.log                                         -1.617e-01
## H.T.deal                                              -2.285e+01
## A.T.new                                                1.806e-01
## S.P.metropolitan.diary.colon                          -8.674e-02
## H.T.billion                                           -4.016e-01
## H.T.polit                                             -9.185e-01
## H.T.china                                             -7.947e-01
## H.P.verbatim.colon                                    -1.514e+01
## H.T.art                                               -9.047e-01
## `PubDate.minute.fctr(14.8,29.5]`                      -1.156e-01
## `PubDate.minute.fctr(29.5,44.2]`                      -2.258e-01
## `PubDate.minute.fctr(44.2,59.1]`                       4.096e-02
## H.T.read                                              -1.136e+00
## A.T.year                                               2.150e-01
## S.npnct12.log                                         -1.650e-01
## A.T.will                                              -1.152e+00
## A.T.appear                                            -5.717e-01
## PubDate.wkday.fctr1                                   -6.478e-01
## PubDate.wkday.fctr2                                   -1.168e+00
## PubDate.wkday.fctr3                                   -7.645e-01
## PubDate.wkday.fctr4                                   -9.341e-01
## PubDate.wkday.fctr5                                   -8.733e-01
## PubDate.wkday.fctr6                                   -1.158e+00
## H.T.pictur                                             1.395e-01
## A.T.senat                                              3.430e-01
## S.T.day                                               -7.347e-01
## S.T.show                                              -1.928e+00
## H.P.today.in.smallbusiness                            -1.574e+01
## H.P.first.draft                                       -1.630e+01
## H.T.new                                               -5.225e-01
## S.npnct28.log                                         -1.476e+01
## H.P.daily.clip.report                                 -1.677e+01
## S.P.daily.clip.report                                         NA
## H.T.news                                              -8.912e-01
## S.T.first                                              7.631e-02
## H.T.X2014                                             -5.151e-01
## A.T.compani                                           -8.798e-01
## S.T.report                                            -2.059e+00
## A.T.word                                              -7.148e-01
## A.T.editor                                            -2.110e+00
## H.T.busi                                              -4.715e-01
## A.npnct13.log                                          1.013e+00
## S.T.share                                             -1.739e+00
## A.T.time                                              -7.228e-01
## H.T.newyork                                           -5.928e-01
## A.T.newyork                                            2.942e+00
## H.T.day                                               -4.041e-01
## A.T.intern                                            -2.279e+00
## A.T.scene                                             -4.357e+01
## H.npnct14.log                                         -2.043e+01
## S.T.highlight                                          1.196e+00
## S.npnct04.log                                         -1.086e+00
## S.npnct15.log                                          2.079e-01
## S.T.tribun                                            -4.239e+01
## H.T.week                                              -8.896e-01
## S.P.fashion.week                                       2.283e+00
## H.P.fashion.week                                      -1.211e+01
## H.npnct15.log                                         -1.188e+00
## S.T.fashion                                           -5.430e+01
## A.T.week                                              -5.344e-01
## H.npnct28.log                                         -5.339e-01
## S.npnct11.log                                         -1.971e-01
## H.ndgts.log                                            2.962e-01
## S.ndgts.log                                           -3.608e-01
## H.nuppr.log                                            1.163e+00
## H.nchrs.log                                           -1.636e+00
## A.nchrs.log                                            2.849e-01
## A.nwrds.unq.log                                       -1.024e+00
## S.nuppr.log                                           -5.662e-01
##                                                       Std. Error z value
## (Intercept)                                            3.116e+00  -0.386
## WordCount.log                                          9.882e-02  12.119
## H.nwrds.log                                            7.342e-01   0.784
## `PubDate.hour.fctr(7.67,15.3]`                         2.493e-01   0.833
## `PubDate.hour.fctr(15.3,23]`                           2.548e-01   1.331
## H.npnct19.log                                          3.298e-01   4.082
## A.nwrds.log                                            8.415e-01  -1.456
## PubDate.wkend                                          4.543e-01  -0.628
## H.P.recap.colon                                        5.963e-01   2.227
## H.P.quandary                                           6.754e+03   0.003
## H.P.no.comment.colon                                   1.066e+00   2.223
## A.npnct19.log                                          3.477e-01   4.720
## H.npnct08.log                                          4.553e-01   2.911
## PubDate.last10.log                                     1.261e-01   1.595
## PubDate.last1.log                                      4.553e-02  -0.826
## H.P.readers.respond                                    1.117e+00   6.160
## S.T.make                                               6.237e-01  -1.861
## H.T.get                                                4.381e-01   0.958
## S.npnct01.log                                          1.895e+00   0.904
## H.npnct16.log                                          6.132e-01   1.229
## A.T.can                                                8.465e-01  -1.342
## H.T.ebola                                              3.135e-01   0.066
## H.npnct01.log                                          1.310e+00  -1.207
## S.T.said                                               8.391e-01   1.292
## H.T.make                                               3.443e-01  -0.324
## H.npnct11.log                                          2.139e-01   2.158
## `myCategory.fctrForeign#World#Asia Pacific`            6.904e-01  -5.513
## `myCategory.fctr#Multimedia#`                          8.152e-01  -5.204
## `myCategory.fctrCulture#Arts#`                         4.334e-01  -7.406
## `myCategory.fctrBusiness#Business Day#Dealbook`        3.230e-01  -8.176
## myCategory.fctrmyOther                                 2.994e+03  -0.007
## `myCategory.fctrBusiness#Technology#`                  3.342e-01  -5.930
## `myCategory.fctrBusiness#Crosswords/Games#`            5.402e-01   1.244
## `myCategory.fctrTStyle##`                              5.061e-01  -8.653
## `myCategory.fctrForeign#World#`                        1.102e+03  -0.016
## `myCategory.fctrOpEd#Opinion#`                         3.062e-01   1.790
## `myCategory.fctrStyles##Fashion`                       1.653e+03  -0.013
## `myCategory.fctr#Opinion#Room For Debate`              8.625e-01  -8.356
## `myCategory.fctr#U.S.#Education`                       9.504e+02  -0.024
## `myCategory.fctr##`                                    3.036e-01  -8.738
## `myCategory.fctrMetro#N.Y. / Region#`                  5.664e-01  -3.702
## `myCategory.fctrBusiness#Business Day#Small Business`  7.101e-01  -6.115
## `myCategory.fctrStyles#U.S.#`                          3.471e-01  -1.899
## `myCategory.fctrTravel#Travel#`                        1.058e+00  -3.883
## `myCategory.fctr#Opinion#The Public Editor`            1.211e+00   0.663
## S.T.state                                              8.412e-01   1.751
## S.T.one                                                6.249e-01  -1.668
## H.P.s.notebook                                         8.530e+03  -0.002
## H.T.take                                               4.774e-01  -0.685
## A.npnct16.log                                          1.368e+00  -0.066
## S.npnct16.log                                                 NA      NA
## H.T.time                                               3.044e-01   0.430
## S.T.presid                                             8.365e-01   0.305
## S.npnct08.log                                          6.398e-01   1.458
## A.npnct08.log                                                 NA      NA
## PubDate.last100.log                                    4.539e-02   0.353
## .rnorm                                                 6.536e-02  -1.356
## H.npnct05.log                                          1.024e+04  -0.002
## H.T.obama                                              4.607e-01  -0.481
## H.T.say                                                4.355e-01  -1.244
## H.T.bank                                               4.874e-01   0.709
## `PubDate.date.fctr(7,13]`                              2.022e-01  -0.497
## `PubDate.date.fctr(13,19]`                             1.991e-01  -0.999
## `PubDate.date.fctr(19,25]`                             1.967e-01  -0.779
## `PubDate.date.fctr(25,31]`                             2.128e-01   0.127
## `PubDate.second.fctr(14.8,29.5]`                       1.805e-01   0.554
## `PubDate.second.fctr(29.5,44.2]`                       1.765e-01   0.134
## `PubDate.second.fctr(44.2,59.1]`                       1.829e-01  -1.459
## H.npnct07.log                                          2.016e-01   1.052
## S.npnct07.log                                          1.111e+04  -0.002
## S.npnct03.log                                          8.755e+03  -0.003
## A.npnct18.log                                          9.059e+03  -0.003
## H.npnct12.log                                          3.163e-01   0.839
## H.T.word                                               8.960e-01   2.370
## H.T.big                                                6.097e-01  -0.707
## S.P.year.colon                                         3.599e+03  -0.003
## S.npnct20.log                                          7.543e+03  -0.003
## S.T.obama                                              8.928e-01  -0.328
## H.npnct02.log                                          5.008e+03  -0.004
## H.T.test                                               7.158e-01  -0.015
## S.npnct14.log                                          1.758e+00   0.473
## H.P.on.this.day                                        5.500e+03  -0.003
## S.P.first.draft                                        4.226e+03  -0.004
## A.T.take                                               1.045e+00  -0.757
## S.npnct06.log                                          1.553e+00   0.191
## H.npnct13.log                                          1.996e-01  -0.810
## H.T.deal                                               2.753e+03  -0.008
## A.T.new                                                7.327e-01   0.246
## S.P.metropolitan.diary.colon                           8.570e-01  -0.101
## H.T.billion                                            8.466e-01  -0.474
## H.T.polit                                              3.374e-01  -2.722
## H.T.china                                              1.002e+00  -0.793
## H.P.verbatim.colon                                     3.709e+03  -0.004
## H.T.art                                                8.627e-01  -1.049
## `PubDate.minute.fctr(14.8,29.5]`                       1.867e-01  -0.619
## `PubDate.minute.fctr(29.5,44.2]`                       1.836e-01  -1.230
## `PubDate.minute.fctr(44.2,59.1]`                       1.881e-01   0.218
## H.T.read                                               3.940e-01  -2.884
## A.T.year                                               9.183e-01   0.234
## S.npnct12.log                                          2.086e-01  -0.791
## A.T.will                                               7.988e-01  -1.442
## A.T.appear                                             1.109e+00  -0.516
## PubDate.wkday.fctr1                                    5.378e-01  -1.205
## PubDate.wkday.fctr2                                    5.879e-01  -1.988
## PubDate.wkday.fctr3                                    5.801e-01  -1.318
## PubDate.wkday.fctr4                                    5.726e-01  -1.631
## PubDate.wkday.fctr5                                    5.803e-01  -1.505
## PubDate.wkday.fctr6                                    5.017e-01  -2.308
## H.T.pictur                                             6.845e-01   0.204
## A.T.senat                                              8.750e-01   0.392
## S.T.day                                                1.018e+00  -0.722
## S.T.show                                               1.208e+00  -1.596
## H.P.today.in.smallbusiness                             2.890e+03  -0.005
## H.P.first.draft                                        2.278e+03  -0.007
## H.T.new                                                4.869e-01  -1.073
## S.npnct28.log                                          2.102e+03  -0.007
## H.P.daily.clip.report                                  2.626e+03  -0.006
## S.P.daily.clip.report                                         NA      NA
## H.T.news                                               7.994e-01  -1.115
## S.T.first                                              1.033e+00   0.074
## H.T.X2014                                              9.100e-01  -0.566
## A.T.compani                                            9.135e-01  -0.963
## S.T.report                                             1.214e+00  -1.697
## A.T.word                                               1.077e+00  -0.664
## A.T.editor                                             1.879e+00  -1.123
## H.T.busi                                               7.578e-01  -0.622
## A.npnct13.log                                          2.716e-01   3.732
## S.T.share                                              1.056e+00  -1.646
## A.T.time                                               9.710e-01  -0.744
## H.T.newyork                                            5.432e-01  -1.091
## A.T.newyork                                            1.039e+00   2.833
## H.T.day                                                6.410e-01  -0.630
## A.T.intern                                             2.556e+00  -0.892
## A.T.scene                                              2.368e+03  -0.018
## H.npnct14.log                                          1.808e+03  -0.011
## S.T.highlight                                          2.326e+00   0.514
## S.npnct04.log                                          6.941e-01  -1.564
## S.npnct15.log                                          5.331e-01   0.390
## S.T.tribun                                             2.552e+03  -0.017
## H.T.week                                               7.026e-01  -1.266
## S.P.fashion.week                                       1.128e+03   0.002
## H.P.fashion.week                                       9.413e+02  -0.013
## H.npnct15.log                                          3.473e-01  -3.421
## S.T.fashion                                            2.686e+03  -0.020
## A.T.week                                               8.535e-01  -0.626
## H.npnct28.log                                          1.721e+00  -0.310
## S.npnct11.log                                          1.496e-01  -1.318
## H.ndgts.log                                            2.412e-01   1.228
## S.ndgts.log                                            1.608e-01  -2.245
## H.nuppr.log                                            4.350e-01   2.674
## H.nchrs.log                                            3.712e-01  -4.407
## A.nchrs.log                                            5.081e-01   0.561
## A.nwrds.unq.log                                        5.664e-01  -1.808
## S.nuppr.log                                            1.622e-01  -3.491
##                                                       Pr(>|z|)    
## (Intercept)                                           0.699152    
## WordCount.log                                          < 2e-16 ***
## H.nwrds.log                                           0.432833    
## `PubDate.hour.fctr(7.67,15.3]`                        0.405052    
## `PubDate.hour.fctr(15.3,23]`                          0.183273    
## H.npnct19.log                                         4.46e-05 ***
## A.nwrds.log                                           0.145282    
## PubDate.wkend                                         0.530297    
## H.P.recap.colon                                       0.025936 *  
## H.P.quandary                                          0.997394    
## H.P.no.comment.colon                                  0.026197 *  
## A.npnct19.log                                         2.36e-06 ***
## H.npnct08.log                                         0.003598 ** 
## PubDate.last10.log                                    0.110759    
## PubDate.last1.log                                     0.408662    
## H.P.readers.respond                                   7.29e-10 ***
## S.T.make                                              0.062679 .  
## H.T.get                                               0.337812    
## S.npnct01.log                                         0.365941    
## H.npnct16.log                                         0.218975    
## A.T.can                                               0.179638    
## H.T.ebola                                             0.947751    
## H.npnct01.log                                         0.227588    
## S.T.said                                              0.196228    
## H.T.make                                              0.746262    
## H.npnct11.log                                         0.030930 *  
## `myCategory.fctrForeign#World#Asia Pacific`           3.53e-08 ***
## `myCategory.fctr#Multimedia#`                         1.95e-07 ***
## `myCategory.fctrCulture#Arts#`                        1.30e-13 ***
## `myCategory.fctrBusiness#Business Day#Dealbook`       2.92e-16 ***
## myCategory.fctrmyOther                                0.994424    
## `myCategory.fctrBusiness#Technology#`                 3.03e-09 ***
## `myCategory.fctrBusiness#Crosswords/Games#`           0.213335    
## `myCategory.fctrTStyle##`                              < 2e-16 ***
## `myCategory.fctrForeign#World#`                       0.987049    
## `myCategory.fctrOpEd#Opinion#`                        0.073410 .  
## `myCategory.fctrStyles##Fashion`                      0.990010    
## `myCategory.fctr#Opinion#Room For Debate`              < 2e-16 ***
## `myCategory.fctr#U.S.#Education`                      0.980947    
## `myCategory.fctr##`                                    < 2e-16 ***
## `myCategory.fctrMetro#N.Y. / Region#`                 0.000214 ***
## `myCategory.fctrBusiness#Business Day#Small Business` 9.67e-10 ***
## `myCategory.fctrStyles#U.S.#`                         0.057539 .  
## `myCategory.fctrTravel#Travel#`                       0.000103 ***
## `myCategory.fctr#Opinion#The Public Editor`           0.507368    
## S.T.state                                             0.080002 .  
## S.T.one                                               0.095296 .  
## H.P.s.notebook                                        0.998378    
## H.T.take                                              0.493524    
## A.npnct16.log                                         0.947117    
## S.npnct16.log                                               NA    
## H.T.time                                              0.667534    
## S.T.presid                                            0.760626    
## S.npnct08.log                                         0.144900    
## A.npnct08.log                                               NA    
## PubDate.last100.log                                   0.723858    
## .rnorm                                                0.175079    
## H.npnct05.log                                         0.998038    
## H.T.obama                                             0.630629    
## H.T.say                                               0.213346    
## H.T.bank                                              0.478124    
## `PubDate.date.fctr(7,13]`                             0.619094    
## `PubDate.date.fctr(13,19]`                            0.317992    
## `PubDate.date.fctr(19,25]`                            0.435728    
## `PubDate.date.fctr(25,31]`                            0.898757    
## `PubDate.second.fctr(14.8,29.5]`                      0.579679    
## `PubDate.second.fctr(29.5,44.2]`                      0.893283    
## `PubDate.second.fctr(44.2,59.1]`                      0.144683    
## H.npnct07.log                                         0.292633    
## S.npnct07.log                                         0.998219    
## S.npnct03.log                                         0.997322    
## A.npnct18.log                                         0.997503    
## H.npnct12.log                                         0.401470    
## H.T.word                                              0.017781 *  
## H.T.big                                               0.479618    
## S.P.year.colon                                        0.997703    
## S.npnct20.log                                         0.997334    
## S.T.obama                                             0.742631    
## H.npnct02.log                                         0.997080    
## H.T.test                                              0.987971    
## S.npnct14.log                                         0.635895    
## H.P.on.this.day                                       0.997774    
## S.P.first.draft                                       0.997093    
## A.T.take                                              0.448864    
## S.npnct06.log                                         0.848194    
## H.npnct13.log                                         0.417803    
## H.T.deal                                              0.993378    
## A.T.new                                               0.805328    
## S.P.metropolitan.diary.colon                          0.919384    
## H.T.billion                                           0.635251    
## H.T.polit                                             0.006487 ** 
## H.T.china                                             0.427701    
## H.P.verbatim.colon                                    0.996743    
## H.T.art                                               0.294355    
## `PubDate.minute.fctr(14.8,29.5]`                      0.535811    
## `PubDate.minute.fctr(29.5,44.2]`                      0.218616    
## `PubDate.minute.fctr(44.2,59.1]`                      0.827570    
## H.T.read                                              0.003921 ** 
## A.T.year                                              0.814927    
## S.npnct12.log                                         0.428770    
## A.T.will                                              0.149213    
## A.T.appear                                            0.606099    
## PubDate.wkday.fctr1                                   0.228387    
## PubDate.wkday.fctr2                                   0.046859 *  
## PubDate.wkday.fctr3                                   0.187527    
## PubDate.wkday.fctr4                                   0.102815    
## PubDate.wkday.fctr5                                   0.132300    
## PubDate.wkday.fctr6                                   0.021001 *  
## H.T.pictur                                            0.838529    
## A.T.senat                                             0.695080    
## S.T.day                                               0.470267    
## S.T.show                                              0.110462    
## H.P.today.in.smallbusiness                            0.995654    
## H.P.first.draft                                       0.994289    
## H.T.new                                               0.283220    
## S.npnct28.log                                         0.994397    
## H.P.daily.clip.report                                 0.994903    
## S.P.daily.clip.report                                       NA    
## H.T.news                                              0.264920    
## S.T.first                                             0.941097    
## H.T.X2014                                             0.571402    
## A.T.compani                                           0.335550    
## S.T.report                                            0.089723 .  
## A.T.word                                              0.506906    
## A.T.editor                                            0.261447    
## H.T.busi                                              0.533790    
## A.npnct13.log                                         0.000190 ***
## S.T.share                                             0.099792 .  
## A.T.time                                              0.456618    
## H.T.newyork                                           0.275134    
## A.T.newyork                                           0.004618 ** 
## H.T.day                                               0.528420    
## A.T.intern                                            0.372587    
## A.T.scene                                             0.985323    
## H.npnct14.log                                         0.990985    
## S.T.highlight                                         0.607151    
## S.npnct04.log                                         0.117764    
## S.npnct15.log                                         0.696632    
## S.T.tribun                                            0.986749    
## H.T.week                                              0.205470    
## S.P.fashion.week                                      0.998386    
## H.P.fashion.week                                      0.989733    
## H.npnct15.log                                         0.000623 ***
## S.T.fashion                                           0.983868    
## A.T.week                                              0.531242    
## H.npnct28.log                                         0.756475    
## S.npnct11.log                                         0.187571    
## H.ndgts.log                                           0.219418    
## S.ndgts.log                                           0.024787 *  
## H.nuppr.log                                           0.007487 ** 
## H.nchrs.log                                           1.05e-05 ***
## A.nchrs.log                                           0.574913    
## A.nwrds.unq.log                                       0.070603 .  
## S.nuppr.log                                           0.000481 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 1743.2  on 4324  degrees of freedom
## AIC: 2045.2
## 
## Number of Fisher Scoring iterations: 19
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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-25.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_0-26.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6711153
## 3        0.2 0.7435028
## 4        0.3 0.7595731
## 5        0.4 0.7521827
## 6        0.5 0.7453770
## 7        0.6 0.7217589
## 8        0.7 0.6747967
## 9        0.8 0.5941124
## 10       0.9 0.4437690
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3487
## 2            Y                                  144
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  239
## 2                                  605
##          Prediction
## Reference    N    Y
##         N 3487  239
##         Y  144  605
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.144134e-01   7.077389e-01   9.058322e-01   9.224492e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   4.429322e-57   1.561688e-06 
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

![](NYTBlogs_clusters_files/figure-html/fit.models_0-27.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_0-28.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6475584
## 3        0.2 0.7139241
## 4        0.3 0.7257618
## 5        0.4 0.7261905
## 6        0.5 0.6941363
## 7        0.6 0.6788686
## 8        0.7 0.6225403
## 9        0.8 0.5546875
## 10       0.9 0.3807339
## 11       1.0 0.0000000
```

![](NYTBlogs_clusters_files/figure-html/fit.models_0-29.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1629
## 2            Y                                  100
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   84
## 2                                  244
##          Prediction
## Reference    N    Y
##         N 1629   84
##         Y  100  244
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.105493e-01   6.727694e-01   8.973805e-01   9.225338e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.388088e-24   2.688067e-01 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, A.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, S.npnct01.log, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, S.T.state, S.T.one, H.P.s.notebook, H.T.take, A.npnct16.log, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, A.npnct08.log, PubDate.last100.log, .rnorm, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, H.T.test, S.npnct14.log, H.P.on.this.day, S.P.first.draft, A.T.take, S.npnct06.log, H.npnct13.log, H.T.deal, A.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, A.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, H.P.first.draft, H.T.new, S.npnct28.log, H.P.daily.clip.report, S.P.daily.clip.report, H.T.news, S.T.first, H.T.X2014, A.T.compani, S.T.report, A.T.word, A.T.editor, H.T.busi, A.npnct13.log, S.T.share, A.T.time, H.T.newyork, A.T.newyork, H.T.day, A.T.intern, A.T.scene, H.npnct14.log, S.T.highlight, S.npnct04.log, S.npnct15.log, S.T.tribun, H.T.week, S.P.fashion.week, H.P.fashion.week, H.npnct15.log, S.T.fashion, A.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, A.nchrs.log, A.nwrds.unq.log, S.nuppr.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      8.837                  4.39
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.955718                    0.3       0.7595731        0.9072625
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9058322             0.9224492      0.649517   0.9200352
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7261905        0.9105493
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8973805             0.9225338     0.6727694    2045.192
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002054744     0.007653537
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          7          0 211.670 252.083  40.413
## 11 fit.models          7          1 252.084      NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor     bgn end elapsed
## 1 fit.models_1_bgn          1          0 256.226  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

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
    
    if (method %in% c("glm")) # for a "robust" glm model
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(NULL
                                    ,"A.nchrs.log"      # correlated to "S.*"                                                      
                                    ,"A.ndgts.log"      # correlated to "S.*"
                                    ,"A.nuppr.log"      # correlated to "S.*"
                                    ,"A.npnct01.log" # identical  to "S.npnct01.log"
                                    ,"A.npnct03.log" # correlated to "S.npnct03.log"
                                    ,"A.npnct04.log" # correlated to "S.npnct04.log"
                                    ,"A.npnct06.log" # identical  to "S.npnct06.log"
                                    ,"A.npnct07.log" # identical  to "S.npnct07.log"
                                    ,"A.npnct08.log" # correlated to "S.npnct08.log"
                                    ,"A.npnct11.log" # correlated to "S.*"
                                    ,"A.npnct12.log" # correlated to "S.*"
                                    ,"S.npnct14.log" # correlated to "A.*"
                                    ,"A.npnct15.log" # correlated to "S.npnct15.log"
                                    ,"A.npnct16.log" # correlated to "S.npnct16.log"
                                    ,"A.npnct19.log" # correlated to "S.*"
                                    ,"A.npnct20.log" # identical  to "S.npnct20.log"
                                    ,"A.npnct21.log" # correlated to "S.npnct21.log"
                                    ,"A.P.daily.clip.report" # identical  to "S.*"
                                    ,"S.P.daily.clip.report" # identical  to "H.*"
                                    ,"A.P.http" # correlated  to "A.npnct14.log"
                                    ,"A.P.fashion.week" # identical  to "S.*"
                                    ,"H.P.first.draft" # correlated  to "H.T.first"
                                    ,"A.P.first.draft" # identical  to "S.*"
                                    ,"A.P.metropolitan.diary.colon" # identical  to "S.*"
                                    ,"A.P.year.colon" # identical  to "S.P.year.colon"
                                                      ))
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitent_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.make", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.make", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_entity_df$S.nuppr.log, glb_entity_df$A.nuppr.log)
#           all.equal(glb_entity_df$S.npnct19.log, glb_entity_df$A.npnct19.log)
#           all.equal(glb_entity_df$S.P.year.colon, glb_entity_df$A.P.year.colon)
#           all.equal(glb_entity_df$S.T.share, glb_entity_df$A.T.share)
#           all.equal(glb_entity_df$H.T.clip, glb_entity_df$H.P.daily.clip.report)
#           cor(glb_entity_df$S.T.herald, glb_entity_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_entity_df[, setdiff(names(glb_entity_df), myfind_chr_cols_df(glb_entity_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
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
## 1 fit.models_1_bgn          1          0 256.226 256.239   0.013
## 2 fit.models_1_glm          2          0 256.239      NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, S.nwrds.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, S.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, H.npnct06.log, S.npnct01.log, S.T.can, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, A.T.said, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, A.T.one, A.T.state, S.T.state, S.T.one, H.P.s.notebook, H.T.take, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, PubDate.last100.log, .rnorm, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, A.npnct02.log, A.npnct17.log, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, A.T.obama, H.T.test, H.P.on.this.day, S.P.first.draft, A.T.take, S.T.take, S.npnct06.log, A.npnct14.log, H.npnct13.log, H.T.deal, A.T.new, S.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, H.P.today.in.politic, H.P.what.we.are, A.T.will, S.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, S.T.photo, H.T.new, S.npnct28.log, A.npnct28.log, H.P.daily.clip.report, A.T.day, H.T.news, H.T.first, S.T.first, H.T.X2014, S.T.past, A.T.compani, S.T.compani, S.T.report, A.T.word, H.T.morn, A.T.report, A.T.editor, S.T.word, H.T.busi, A.npnct13.log, S.T.share, H.npnct04.log, S.npnct13.log, S.T.articl, A.T.time, S.T.time, H.T.newyork, A.T.newyork, H.T.today, H.T.daili, H.T.report, S.T.newyork, H.T.day, A.T.intern, A.T.scene, S.T.scene, H.npnct14.log, S.T.diari, S.T.highlight, S.npnct04.log, H.T.X2015, S.npnct15.log, S.T.tribun, S.T.intern, A.T.photo, H.T.week, S.P.fashion.week, H.P.fashion.week, H.P.year.colon, H.T.fashion, H.npnct15.log, S.T.fashion, A.T.fashion, A.T.week, S.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.unq.log, S.nchrs.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](NYTBlogs_clusters_files/figure-html/fit.models_1-1.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_1-2.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_1-3.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](NYTBlogs_clusters_files/figure-html/fit.models_1-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.8673  -0.2857  -0.0946   0.0000   3.4760  
## 
## Coefficients:
##                                                         Estimate
## (Intercept)                                           -6.730e-01
## WordCount.log                                          1.197e+00
## H.nwrds.log                                            5.124e-02
## `PubDate.hour.fctr(7.67,15.3]`                         1.151e-01
## `PubDate.hour.fctr(15.3,23]`                           2.825e-01
## H.npnct19.log                                          1.319e+00
## S.nwrds.log                                           -2.097e+01
## A.nwrds.log                                            1.978e+01
## PubDate.wkend                                         -2.703e-01
## H.P.recap.colon                                        1.199e+00
## H.P.quandary                                           2.390e+01
## H.P.no.comment.colon                                   2.391e+00
## S.npnct19.log                                          1.657e+00
## H.npnct08.log                                          1.210e+00
## PubDate.last10.log                                     2.080e-01
## PubDate.last1.log                                     -4.834e-02
## H.P.readers.respond                                    6.899e+00
## S.T.make                                              -2.746e+00
## H.T.get                                                3.812e-01
## H.npnct06.log                                          1.183e+00
## S.npnct01.log                                          1.623e+00
## S.T.can                                               -4.790e+00
## H.npnct16.log                                         -3.699e-01
## A.T.can                                                3.345e+00
## H.T.ebola                                             -5.569e-02
## H.npnct01.log                                         -1.531e+00
## A.T.said                                               1.336e+01
## S.T.said                                              -1.239e+01
## H.T.make                                               1.647e-02
## H.npnct11.log                                          4.688e-01
## `myCategory.fctrForeign#World#Asia Pacific`           -3.844e+00
## `myCategory.fctr#Multimedia#`                         -4.346e+00
## `myCategory.fctrCulture#Arts#`                        -3.270e+00
## `myCategory.fctrBusiness#Business Day#Dealbook`       -2.642e+00
## myCategory.fctrmyOther                                -2.093e+01
## `myCategory.fctrBusiness#Technology#`                 -1.975e+00
## `myCategory.fctrBusiness#Crosswords/Games#`            6.459e-01
## `myCategory.fctrTStyle##`                             -4.596e+00
## `myCategory.fctrForeign#World#`                       -2.001e+01
## `myCategory.fctrOpEd#Opinion#`                         5.038e-01
## `myCategory.fctrStyles##Fashion`                      -2.602e+01
## `myCategory.fctr#Opinion#Room For Debate`             -7.231e+00
## `myCategory.fctr#U.S.#Education`                      -2.308e+01
## `myCategory.fctr##`                                   -2.760e+00
## `myCategory.fctrMetro#N.Y. / Region#`                 -1.832e+00
## `myCategory.fctrBusiness#Business Day#Small Business` -4.601e+00
## `myCategory.fctrStyles#U.S.#`                         -7.904e-01
## `myCategory.fctrTravel#Travel#`                       -4.178e+00
## `myCategory.fctr#Opinion#The Public Editor`            2.568e+00
## A.T.one                                                1.924e+01
## A.T.state                                             -1.952e+01
## S.T.state                                              2.101e+01
## S.T.one                                               -2.033e+01
## H.P.s.notebook                                        -1.761e+01
## H.T.take                                              -2.544e-01
## S.npnct16.log                                         -3.407e-01
## H.T.time                                               7.525e-02
## S.T.presid                                            -1.367e+00
## S.npnct08.log                                          1.062e+00
## PubDate.last100.log                                    2.296e-02
## .rnorm                                                -9.510e-02
## H.npnct05.log                                         -2.714e+01
## H.T.obama                                             -1.410e-01
## H.T.say                                               -5.657e-01
## H.T.bank                                               3.310e-01
## `PubDate.date.fctr(7,13]`                             -6.804e-02
## `PubDate.date.fctr(13,19]`                            -1.733e-01
## `PubDate.date.fctr(19,25]`                            -1.369e-01
## `PubDate.date.fctr(25,31]`                             3.848e-02
## `PubDate.second.fctr(14.8,29.5]`                       1.037e-01
## `PubDate.second.fctr(29.5,44.2]`                      -1.778e-02
## `PubDate.second.fctr(44.2,59.1]`                      -3.124e-01
## H.npnct07.log                                          2.757e-01
## S.npnct07.log                                         -3.392e+01
## S.npnct03.log                                         -3.175e+01
## A.npnct18.log                                          7.156e+00
## H.npnct12.log                                          2.157e-01
## H.T.word                                               2.158e+00
## H.T.big                                               -4.885e-01
## A.npnct02.log                                         -1.996e+01
## A.npnct17.log                                         -4.250e+00
## S.P.year.colon                                         6.367e+00
## S.npnct20.log                                         -2.685e+01
## S.T.obama                                              1.095e+01
## H.npnct02.log                                         -1.946e+01
## A.T.obama                                             -1.098e+01
## H.T.test                                               5.330e-05
## H.P.on.this.day                                       -1.759e+01
## S.P.first.draft                                       -1.657e+01
## A.T.take                                               1.692e+01
## S.T.take                                              -1.807e+01
## S.npnct06.log                                         -2.084e-01
## A.npnct14.log                                          9.655e-01
## H.npnct13.log                                         -2.198e-01
## H.T.deal                                              -2.385e+01
## A.T.new                                                1.333e+01
## S.T.new                                               -1.348e+01
## S.P.metropolitan.diary.colon                          -9.095e+00
## H.T.billion                                            1.087e+00
## H.T.polit                                             -4.256e-01
## H.T.china                                             -7.253e-01
## H.P.verbatim.colon                                    -1.622e+01
## H.T.art                                               -9.534e-01
## `PubDate.minute.fctr(14.8,29.5]`                      -1.079e-01
## `PubDate.minute.fctr(29.5,44.2]`                      -2.476e-01
## `PubDate.minute.fctr(44.2,59.1]`                       4.125e-02
## H.T.read                                              -7.970e-01
## A.T.year                                               3.201e+00
## S.npnct12.log                                         -1.758e-01
## H.P.today.in.politic                                  -1.708e+01
## H.P.what.we.are                                       -1.874e+01
## A.T.will                                               4.619e+00
## S.T.will                                              -5.924e+00
## A.T.appear                                             6.208e-01
## PubDate.wkday.fctr1                                   -6.053e-01
## PubDate.wkday.fctr2                                   -1.206e+00
## PubDate.wkday.fctr3                                   -7.040e-01
## PubDate.wkday.fctr4                                   -9.371e-01
## PubDate.wkday.fctr5                                   -8.713e-01
## PubDate.wkday.fctr6                                   -1.296e+00
## H.T.pictur                                             3.942e-02
## A.T.senat                                              1.797e+00
## S.T.day                                               -2.034e+01
## S.T.show                                              -2.590e+00
## H.P.today.in.smallbusiness                            -1.597e+01
## S.T.photo                                             -3.527e+00
## H.T.new                                               -4.660e-01
## S.npnct28.log                                         -3.541e+01
## A.npnct28.log                                         -1.852e+00
## H.P.daily.clip.report                                  6.207e+01
## A.T.day                                                1.992e+01
## H.T.news                                              -9.551e-01
## H.T.first                                             -9.375e-01
## S.T.first                                             -1.710e+00
## H.T.X2014                                             -9.103e-01
## S.T.past                                               1.189e-01
## A.T.compani                                           -5.600e+00
## S.T.compani                                            4.813e+00
## S.T.report                                            -2.552e+00
## A.T.word                                              -1.469e+01
## H.T.morn                                               1.558e+01
## A.T.report                                             4.837e-01
## A.T.editor                                            -1.518e+00
## S.T.word                                               1.473e+01
## H.T.busi                                              -4.080e-01
## A.npnct13.log                                          4.319e+00
## S.T.share                                             -2.759e+00
## H.npnct04.log                                         -2.615e+00
## S.npnct13.log                                         -3.252e+00
## S.T.articl                                            -6.102e+00
## A.T.time                                               4.865e+00
## S.T.time                                              -5.706e+00
## H.T.newyork                                           -3.619e-01
## A.T.newyork                                            2.120e+01
## H.T.today                                             -9.490e-01
## H.T.daili                                             -2.896e+01
## H.T.report                                            -9.058e-01
## S.T.newyork                                           -1.865e+01
## H.T.day                                               -4.684e-01
## A.T.intern                                             1.397e+02
## A.T.scene                                              1.154e+02
## S.T.scene                                             -1.613e+02
## H.npnct14.log                                         -2.006e+01
## S.T.diari                                              1.841e+01
## S.T.highlight                                          5.575e-01
## S.npnct04.log                                         -9.939e-01
## H.T.X2015                                             -2.376e+01
## S.npnct15.log                                          3.571e-01
## S.T.tribun                                            -2.409e+02
## S.T.intern                                            -1.459e+02
## A.T.photo                                              2.071e+00
## H.T.week                                              -8.230e-01
## S.P.fashion.week                                       2.397e+00
## H.P.fashion.week                                      -1.482e+01
## H.P.year.colon                                        -1.625e+01
## H.T.fashion                                            2.239e+00
## H.npnct15.log                                         -1.129e+00
## S.T.fashion                                           -1.763e+02
## A.T.fashion                                            1.173e+02
## A.T.week                                              -1.138e+01
## S.T.week                                               1.095e+01
## H.npnct28.log                                         -8.161e-01
## S.npnct11.log                                         -1.684e-01
## H.ndgts.log                                            6.082e-01
## S.ndgts.log                                           -3.884e-01
## H.nuppr.log                                            1.333e+00
## H.nchrs.log                                           -9.789e-01
## H.nwrds.unq.log                                       -1.145e+00
## S.nchrs.log                                            1.078e-01
## A.nwrds.unq.log                                       -1.863e-01
## S.nwrds.unq.log                                       -5.698e-01
## S.nuppr.log                                           -5.537e-01
##                                                       Std. Error z value
## (Intercept)                                            3.231e+00  -0.208
## WordCount.log                                          1.031e-01  11.611
## H.nwrds.log                                            8.037e-01   0.064
## `PubDate.hour.fctr(7.67,15.3]`                         2.566e-01   0.448
## `PubDate.hour.fctr(15.3,23]`                           2.618e-01   1.079
## H.npnct19.log                                          3.416e-01   3.861
## S.nwrds.log                                            1.862e+01  -1.126
## A.nwrds.log                                            1.859e+01   1.064
## PubDate.wkend                                          4.627e-01  -0.584
## H.P.recap.colon                                        6.158e-01   1.946
## H.P.quandary                                           1.053e+04   0.002
## H.P.no.comment.colon                                   1.122e+00   2.130
## S.npnct19.log                                          3.721e-01   4.455
## H.npnct08.log                                          4.598e-01   2.632
## PubDate.last10.log                                     1.281e-01   1.624
## PubDate.last1.log                                      4.623e-02  -1.045
## H.P.readers.respond                                    1.101e+00   6.266
## S.T.make                                               1.261e+00  -2.177
## H.T.get                                                4.443e-01   0.858
## H.npnct06.log                                          1.045e+00   1.133
## S.npnct01.log                                          1.882e+00   0.862
## S.T.can                                                1.405e+01  -0.341
## H.npnct16.log                                          1.145e+00  -0.323
## A.T.can                                                1.383e+01   0.242
## H.T.ebola                                              3.380e-01  -0.165
## H.npnct01.log                                          1.303e+00  -1.175
## A.T.said                                               2.100e+01   0.636
## S.T.said                                               2.095e+01  -0.592
## H.T.make                                               3.765e-01   0.044
## H.npnct11.log                                          2.206e-01   2.125
## `myCategory.fctrForeign#World#Asia Pacific`            7.023e-01  -5.473
## `myCategory.fctr#Multimedia#`                          8.233e-01  -5.279
## `myCategory.fctrCulture#Arts#`                         4.462e-01  -7.330
## `myCategory.fctrBusiness#Business Day#Dealbook`        3.285e-01  -8.042
## myCategory.fctrmyOther                                 1.785e+03  -0.012
## `myCategory.fctrBusiness#Technology#`                  3.430e-01  -5.757
## `myCategory.fctrBusiness#Crosswords/Games#`            5.487e-01   1.177
## `myCategory.fctrTStyle##`                              5.429e-01  -8.466
## `myCategory.fctrForeign#World#`                        3.122e+03  -0.006
## `myCategory.fctrOpEd#Opinion#`                         3.150e-01   1.599
## `myCategory.fctrStyles##Fashion`                       2.228e+03  -0.012
## `myCategory.fctr#Opinion#Room For Debate`              8.586e-01  -8.422
## `myCategory.fctr#U.S.#Education`                       1.585e+03  -0.015
## `myCategory.fctr##`                                    3.116e-01  -8.856
## `myCategory.fctrMetro#N.Y. / Region#`                  5.882e-01  -3.115
## `myCategory.fctrBusiness#Business Day#Small Business`  7.242e-01  -6.353
## `myCategory.fctrStyles#U.S.#`                          3.556e-01  -2.223
## `myCategory.fctrTravel#Travel#`                        1.062e+00  -3.936
## `myCategory.fctr#Opinion#The Public Editor`            1.655e+00   1.552
## A.T.one                                                1.792e+01   1.073
## A.T.state                                              2.379e+01  -0.821
## S.T.state                                              2.378e+01   0.883
## S.T.one                                                1.790e+01  -1.136
## H.P.s.notebook                                         1.407e+04  -0.001
## H.T.take                                               4.916e-01  -0.518
## S.npnct16.log                                          1.426e+00  -0.239
## H.T.time                                               3.296e-01   0.228
## S.T.presid                                             1.731e+00  -0.790
## S.npnct08.log                                          6.590e-01   1.611
## PubDate.last100.log                                    4.579e-02   0.502
## .rnorm                                                 6.655e-02  -1.429
## H.npnct05.log                                          1.673e+04  -0.002
## H.T.obama                                              4.759e-01  -0.296
## H.T.say                                                4.393e-01  -1.288
## H.T.bank                                               4.945e-01   0.669
## `PubDate.date.fctr(7,13]`                              2.081e-01  -0.327
## `PubDate.date.fctr(13,19]`                             2.032e-01  -0.853
## `PubDate.date.fctr(19,25]`                             2.005e-01  -0.683
## `PubDate.date.fctr(25,31]`                             2.184e-01   0.176
## `PubDate.second.fctr(14.8,29.5]`                       1.837e-01   0.565
## `PubDate.second.fctr(29.5,44.2]`                       1.806e-01  -0.098
## `PubDate.second.fctr(44.2,59.1]`                       1.867e-01  -1.674
## H.npnct07.log                                          2.057e-01   1.341
## S.npnct07.log                                          1.618e+04  -0.002
## S.npnct03.log                                          1.428e+04  -0.002
## A.npnct18.log                                          7.349e+04   0.000
## H.npnct12.log                                          3.264e-01   0.661
## H.T.word                                               9.493e-01   2.273
## H.T.big                                                6.265e-01  -0.780
## A.npnct02.log                                          3.522e+04  -0.001
## A.npnct17.log                                          2.406e+04   0.000
## S.P.year.colon                                         4.341e+03   0.001
## S.npnct20.log                                          1.181e+04  -0.002
## S.T.obama                                              1.795e+01   0.610
## H.npnct02.log                                          8.242e+03  -0.002
## A.T.obama                                              1.740e+01  -0.631
## H.T.test                                               7.012e-01   0.000
## H.P.on.this.day                                        8.717e+03  -0.002
## S.P.first.draft                                        7.362e+03  -0.002
## A.T.take                                               2.236e+01   0.757
## S.T.take                                               2.265e+01  -0.798
## S.npnct06.log                                          1.705e+00  -0.122
## A.npnct14.log                                          1.852e+00   0.521
## H.npnct13.log                                          2.075e-01  -1.059
## H.T.deal                                               4.492e+03  -0.005
## A.T.new                                                1.715e+01   0.777
## S.T.new                                                1.726e+01  -0.781
## S.P.metropolitan.diary.colon                           4.165e+00  -2.184
## H.T.billion                                            1.216e+00   0.895
## H.T.polit                                              4.602e-01  -0.925
## H.T.china                                              9.693e-01  -0.748
## H.P.verbatim.colon                                     6.079e+03  -0.003
## H.T.art                                                8.759e-01  -1.088
## `PubDate.minute.fctr(14.8,29.5]`                       1.931e-01  -0.559
## `PubDate.minute.fctr(29.5,44.2]`                       1.869e-01  -1.325
## `PubDate.minute.fctr(44.2,59.1]`                       1.921e-01   0.215
## H.T.read                                               5.239e-01  -1.521
## A.T.year                                               2.313e+00   1.384
## S.npnct12.log                                          2.128e-01  -0.826
## H.P.today.in.politic                                   5.265e+03  -0.003
## H.P.what.we.are                                        4.592e+03  -0.004
## A.T.will                                               1.157e+01   0.399
## S.T.will                                               1.182e+01  -0.501
## A.T.appear                                             1.473e+00   0.421
## PubDate.wkday.fctr1                                    5.494e-01  -1.102
## PubDate.wkday.fctr2                                    6.019e-01  -2.004
## PubDate.wkday.fctr3                                    5.937e-01  -1.186
## PubDate.wkday.fctr4                                    5.862e-01  -1.599
## PubDate.wkday.fctr5                                    5.928e-01  -1.470
## PubDate.wkday.fctr6                                    5.168e-01  -2.508
## H.T.pictur                                             6.886e-01   0.057
## A.T.senat                                              1.447e+00   1.242
## S.T.day                                                2.106e+01  -0.966
## S.T.show                                               1.431e+00  -1.811
## H.P.today.in.smallbusiness                             4.729e+03  -0.003
## S.T.photo                                              4.019e+00  -0.878
## H.T.new                                                4.927e-01  -0.946
## S.npnct28.log                                          6.929e+04  -0.001
## A.npnct28.log                                          6.834e+04   0.000
## H.P.daily.clip.report                                  1.474e+04   0.004
## A.T.day                                                2.112e+01   0.943
## H.T.news                                               8.106e-01  -1.178
## H.T.first                                              1.080e+00  -0.868
## S.T.first                                              1.623e+00  -1.054
## H.T.X2014                                              1.005e+00  -0.905
## S.T.past                                               1.867e+00   0.064
## A.T.compani                                            2.706e+01  -0.207
## S.T.compani                                            2.722e+01   0.177
## S.T.report                                             2.836e+01  -0.090
## A.T.word                                               3.693e+01  -0.398
## H.T.morn                                               4.876e+03   0.003
## A.T.report                                             2.795e+01   0.017
## A.T.editor                                             1.861e+00  -0.816
## S.T.word                                               3.713e+01   0.397
## H.T.busi                                               7.113e-01  -0.574
## A.npnct13.log                                          2.637e+00   1.638
## S.T.share                                              1.298e+00  -2.125
## H.npnct04.log                                          1.400e+00  -1.868
## S.npnct13.log                                          2.619e+00  -1.242
## S.T.articl                                             2.645e+00  -2.307
## A.T.time                                               1.717e+01   0.283
## S.T.time                                               1.755e+01  -0.325
## H.T.newyork                                            6.009e-01  -0.602
## A.T.newyork                                            2.060e+01   1.029
## H.T.today                                              7.398e-01  -1.283
## H.T.daili                                              5.336e+03  -0.005
## H.T.report                                             9.290e-01  -0.975
## S.T.newyork                                            2.082e+01  -0.896
## H.T.day                                                6.441e-01  -0.727
## A.T.intern                                             6.973e+01   2.003
## A.T.scene                                              1.093e+05   0.001
## S.T.scene                                              1.111e+05  -0.001
## H.npnct14.log                                          2.502e+03  -0.008
## S.T.diari                                              8.812e+00   2.089
## S.T.highlight                                          3.114e+00   0.179
## S.npnct04.log                                          7.132e-01  -1.394
## H.T.X2015                                              2.262e+03  -0.011
## S.npnct15.log                                          5.632e-01   0.634
## S.T.tribun                                             2.577e+03  -0.093
## S.T.intern                                             7.297e+01  -1.999
## A.T.photo                                              4.160e+00   0.498
## H.T.week                                               6.977e-01  -1.180
## S.P.fashion.week                                       2.074e+03   0.001
## H.P.fashion.week                                       1.572e+03  -0.009
## H.P.year.colon                                         3.034e+03  -0.005
## H.T.fashion                                            1.765e+00   1.269
## H.npnct15.log                                          3.650e-01  -3.093
## S.T.fashion                                            7.542e+04  -0.002
## A.T.fashion                                            7.476e+04   0.002
## A.T.week                                               2.861e+01  -0.398
## S.T.week                                               2.885e+01   0.379
## H.npnct28.log                                          1.865e+00  -0.438
## S.npnct11.log                                          1.533e-01  -1.099
## H.ndgts.log                                            2.761e-01   2.203
## S.ndgts.log                                            1.672e-01  -2.322
## H.nuppr.log                                            4.558e-01   2.925
## H.nchrs.log                                            4.670e-01  -2.096
## H.nwrds.unq.log                                        4.815e-01  -2.378
## S.nchrs.log                                            5.220e-01   0.206
## A.nwrds.unq.log                                        4.645e+00  -0.040
## S.nwrds.unq.log                                        4.663e+00  -0.122
## S.nuppr.log                                            1.666e-01  -3.322
##                                                       Pr(>|z|)    
## (Intercept)                                           0.834989    
## WordCount.log                                          < 2e-16 ***
## H.nwrds.log                                           0.949166    
## `PubDate.hour.fctr(7.67,15.3]`                        0.653818    
## `PubDate.hour.fctr(15.3,23]`                          0.280573    
## H.npnct19.log                                         0.000113 ***
## S.nwrds.log                                           0.260135    
## A.nwrds.log                                           0.287192    
## PubDate.wkend                                         0.559134    
## H.P.recap.colon                                       0.051613 .  
## H.P.quandary                                          0.998189    
## H.P.no.comment.colon                                  0.033166 *  
## S.npnct19.log                                         8.41e-06 ***
## H.npnct08.log                                         0.008496 ** 
## PubDate.last10.log                                    0.104386    
## PubDate.last1.log                                     0.295792    
## H.P.readers.respond                                   3.70e-10 ***
## S.T.make                                              0.029452 *  
## H.T.get                                               0.390811    
## H.npnct06.log                                         0.257338    
## S.npnct01.log                                         0.388567    
## S.T.can                                               0.733079    
## H.npnct16.log                                         0.746551    
## A.T.can                                               0.808815    
## H.T.ebola                                             0.869125    
## H.npnct01.log                                         0.239910    
## A.T.said                                              0.524770    
## S.T.said                                              0.554109    
## H.T.make                                              0.965107    
## H.npnct11.log                                         0.033602 *  
## `myCategory.fctrForeign#World#Asia Pacific`           4.42e-08 ***
## `myCategory.fctr#Multimedia#`                         1.30e-07 ***
## `myCategory.fctrCulture#Arts#`                        2.31e-13 ***
## `myCategory.fctrBusiness#Business Day#Dealbook`       8.81e-16 ***
## myCategory.fctrmyOther                                0.990640    
## `myCategory.fctrBusiness#Technology#`                 8.55e-09 ***
## `myCategory.fctrBusiness#Crosswords/Games#`           0.239140    
## `myCategory.fctrTStyle##`                              < 2e-16 ***
## `myCategory.fctrForeign#World#`                       0.994886    
## `myCategory.fctrOpEd#Opinion#`                        0.109735    
## `myCategory.fctrStyles##Fashion`                      0.990684    
## `myCategory.fctr#Opinion#Room For Debate`              < 2e-16 ***
## `myCategory.fctr#U.S.#Education`                      0.988387    
## `myCategory.fctr##`                                    < 2e-16 ***
## `myCategory.fctrMetro#N.Y. / Region#`                 0.001840 ** 
## `myCategory.fctrBusiness#Business Day#Small Business` 2.11e-10 ***
## `myCategory.fctrStyles#U.S.#`                         0.026232 *  
## `myCategory.fctrTravel#Travel#`                       8.29e-05 ***
## `myCategory.fctr#Opinion#The Public Editor`           0.120716    
## A.T.one                                               0.283050    
## A.T.state                                             0.411821    
## S.T.state                                             0.377041    
## S.T.one                                               0.255932    
## H.P.s.notebook                                        0.999001    
## H.T.take                                              0.604711    
## S.npnct16.log                                         0.811193    
## H.T.time                                              0.819429    
## S.T.presid                                            0.429504    
## S.npnct08.log                                         0.107146    
## PubDate.last100.log                                   0.615975    
## .rnorm                                                0.152998    
## H.npnct05.log                                         0.998705    
## H.T.obama                                             0.767098    
## H.T.say                                               0.197881    
## H.T.bank                                              0.503277    
## `PubDate.date.fctr(7,13]`                             0.743714    
## `PubDate.date.fctr(13,19]`                            0.393663    
## `PubDate.date.fctr(19,25]`                            0.494716    
## `PubDate.date.fctr(25,31]`                            0.860149    
## `PubDate.second.fctr(14.8,29.5]`                      0.572406    
## `PubDate.second.fctr(29.5,44.2]`                      0.921613    
## `PubDate.second.fctr(44.2,59.1]`                      0.094212 .  
## H.npnct07.log                                         0.180038    
## S.npnct07.log                                         0.998327    
## S.npnct03.log                                         0.998225    
## A.npnct18.log                                         0.999922    
## H.npnct12.log                                         0.508755    
## H.T.word                                              0.023035 *  
## H.T.big                                               0.435525    
## A.npnct02.log                                         0.999548    
## A.npnct17.log                                         0.999859    
## S.P.year.colon                                        0.998830    
## S.npnct20.log                                         0.998186    
## S.T.obama                                             0.541695    
## H.npnct02.log                                         0.998116    
## A.T.obama                                             0.528102    
## H.T.test                                              0.999939    
## H.P.on.this.day                                       0.998390    
## S.P.first.draft                                       0.998205    
## A.T.take                                              0.449123    
## S.T.take                                              0.424938    
## S.npnct06.log                                         0.902759    
## A.npnct14.log                                         0.602173    
## H.npnct13.log                                         0.289416    
## H.T.deal                                              0.995764    
## A.T.new                                               0.436920    
## S.T.new                                               0.434963    
## S.P.metropolitan.diary.colon                          0.028998 *  
## H.T.billion                                           0.371011    
## H.T.polit                                             0.355105    
## H.T.china                                             0.454270    
## H.P.verbatim.colon                                    0.997871    
## H.T.art                                               0.276399    
## `PubDate.minute.fctr(14.8,29.5]`                      0.576342    
## `PubDate.minute.fctr(29.5,44.2]`                      0.185250    
## `PubDate.minute.fctr(44.2,59.1]`                      0.829947    
## H.T.read                                              0.128147    
## A.T.year                                              0.166294    
## S.npnct12.log                                         0.408778    
## H.P.today.in.politic                                  0.997412    
## H.P.what.we.are                                       0.996743    
## A.T.will                                              0.689819    
## S.T.will                                              0.616346    
## A.T.appear                                            0.673392    
## PubDate.wkday.fctr1                                   0.270550    
## PubDate.wkday.fctr2                                   0.045099 *  
## PubDate.wkday.fctr3                                   0.235699    
## PubDate.wkday.fctr4                                   0.109869    
## PubDate.wkday.fctr5                                   0.141646    
## PubDate.wkday.fctr6                                   0.012158 *  
## H.T.pictur                                            0.954350    
## A.T.senat                                             0.214192    
## S.T.day                                               0.334188    
## S.T.show                                              0.070211 .  
## H.P.today.in.smallbusiness                            0.997305    
## S.T.photo                                             0.380177    
## H.T.new                                               0.344295    
## S.npnct28.log                                         0.999592    
## A.npnct28.log                                         0.999978    
## H.P.daily.clip.report                                 0.996640    
## A.T.day                                               0.345649    
## H.T.news                                              0.238690    
## H.T.first                                             0.385438    
## S.T.first                                             0.291834    
## H.T.X2014                                             0.365240    
## S.T.past                                              0.949211    
## A.T.compani                                           0.836057    
## S.T.compani                                           0.859681    
## S.T.report                                            0.928281    
## A.T.word                                              0.690713    
## H.T.morn                                              0.997450    
## A.T.report                                            0.986192    
## A.T.editor                                            0.414595    
## S.T.word                                              0.691686    
## H.T.busi                                              0.566271    
## A.npnct13.log                                         0.101441    
## S.T.share                                             0.033584 *  
## H.npnct04.log                                         0.061770 .  
## S.npnct13.log                                         0.214311    
## S.T.articl                                            0.021054 *  
## A.T.time                                              0.776843    
## S.T.time                                              0.745108    
## H.T.newyork                                           0.546971    
## A.T.newyork                                           0.303498    
## H.T.today                                             0.199559    
## H.T.daili                                             0.995671    
## H.T.report                                            0.329563    
## S.T.newyork                                           0.370239    
## H.T.day                                               0.467104    
## A.T.intern                                            0.045171 *  
## A.T.scene                                             0.999158    
## S.T.scene                                             0.998841    
## H.npnct14.log                                         0.993603    
## S.T.diari                                             0.036728 *  
## S.T.highlight                                         0.857891    
## S.npnct04.log                                         0.163452    
## H.T.X2015                                             0.991617    
## S.npnct15.log                                         0.526131    
## S.T.tribun                                            0.925536    
## S.T.intern                                            0.045557 *  
## A.T.photo                                             0.618646    
## H.T.week                                              0.238182    
## S.P.fashion.week                                      0.999078    
## H.P.fashion.week                                      0.992478    
## H.P.year.colon                                        0.995727    
## H.T.fashion                                           0.204576    
## H.npnct15.log                                         0.001983 ** 
## S.T.fashion                                           0.998135    
## A.T.fashion                                           0.998748    
## A.T.week                                              0.690741    
## S.T.week                                              0.704321    
## H.npnct28.log                                         0.661682    
## S.npnct11.log                                         0.271835    
## H.ndgts.log                                           0.027622 *  
## S.ndgts.log                                           0.020213 *  
## H.nuppr.log                                           0.003442 ** 
## H.nchrs.log                                           0.036047 *  
## H.nwrds.unq.log                                       0.017416 *  
## S.nchrs.log                                           0.836453    
## A.nwrds.unq.log                                       0.968005    
## S.nwrds.unq.log                                       0.902742    
## S.nuppr.log                                           0.000892 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 1694.8  on 4283  degrees of freedom
## AIC: 2078.8
## 
## Number of Fisher Scoring iterations: 20
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_clusters_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6797888
## 3        0.2 0.7463194
## 4        0.3 0.7647059
## 5        0.4 0.7585284
## 6        0.5 0.7549575
## 7        0.6 0.7300151
## 8        0.7 0.6855295
## 9        0.8 0.6047745
## 10       0.9 0.4374364
## 11       1.0 0.0000000
```

![](NYTBlogs_clusters_files/figure-html/fit.models_1-6.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.All.X.glm.N
## 1            N                             3488
## 2            Y                              138
##   Popular.fctr.predict.All.X.glm.Y
## 1                              238
## 2                              611
##          Prediction
## Reference    N    Y
##         N 3488  238
##         Y  138  611
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.159777e-01   7.138069e-01   9.074636e-01   9.239441e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.941594e-59   3.298596e-07 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_clusters_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6417112
## 3        0.2 0.7055838
## 4        0.3 0.7144790
## 5        0.4 0.7115666
## 6        0.5 0.6915888
## 7        0.6 0.6710311
## 8        0.7 0.6263345
## 9        0.8 0.5609284
## 10       0.9 0.3927765
## 11       1.0 0.0000000
```

![](NYTBlogs_clusters_files/figure-html/fit.models_1-8.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.All.X.glm.N
## 1            N                             1582
## 2            Y                               80
##   Popular.fctr.predict.All.X.glm.Y
## 1                              131
## 2                              264
##          Prediction
## Reference    N    Y
##         N 1582  131
##         Y   80  264
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.974234e-01   6.523231e-01   8.834974e-01   9.102047e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   4.637200e-17   5.771273e-04 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, S.nwrds.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, S.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, H.npnct06.log, S.npnct01.log, S.T.can, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, A.T.said, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, A.T.one, A.T.state, S.T.state, S.T.one, H.P.s.notebook, H.T.take, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, PubDate.last100.log, .rnorm, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, A.npnct02.log, A.npnct17.log, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, A.T.obama, H.T.test, H.P.on.this.day, S.P.first.draft, A.T.take, S.T.take, S.npnct06.log, A.npnct14.log, H.npnct13.log, H.T.deal, A.T.new, S.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, H.P.today.in.politic, H.P.what.we.are, A.T.will, S.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, S.T.photo, H.T.new, S.npnct28.log, A.npnct28.log, H.P.daily.clip.report, A.T.day, H.T.news, H.T.first, S.T.first, H.T.X2014, S.T.past, A.T.compani, S.T.compani, S.T.report, A.T.word, H.T.morn, A.T.report, A.T.editor, S.T.word, H.T.busi, A.npnct13.log, S.T.share, H.npnct04.log, S.npnct13.log, S.T.articl, A.T.time, S.T.time, H.T.newyork, A.T.newyork, H.T.today, H.T.daili, H.T.report, S.T.newyork, H.T.day, A.T.intern, A.T.scene, S.T.scene, H.npnct14.log, S.T.diari, S.T.highlight, S.npnct04.log, H.T.X2015, S.npnct15.log, S.T.tribun, S.T.intern, A.T.photo, H.T.week, S.P.fashion.week, H.P.fashion.week, H.P.year.colon, H.T.fashion, H.npnct15.log, S.T.fashion, A.T.fashion, A.T.week, S.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.unq.log, S.nchrs.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     12.416                 6.335
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9583173                    0.3       0.7647059        0.9065922
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9074636             0.9239441     0.6463354   0.9150986
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3        0.714479        0.8974234
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8834974             0.9102047     0.6523231    2078.788
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0003702469     0.006070769
##                label step_major step_minor     bgn    end elapsed
## 2   fit.models_1_glm          2          0 256.239 276.33  20.091
## 3 fit.models_1_rpart          3          0 276.330     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, S.nwrds.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, S.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, H.npnct06.log, S.npnct01.log, S.T.can, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, A.T.said, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, A.T.one, A.T.state, S.T.state, S.T.one, H.P.s.notebook, H.T.take, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, PubDate.last100.log, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, A.npnct02.log, A.npnct17.log, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, A.T.obama, H.T.test, H.P.on.this.day, S.P.first.draft, A.T.take, S.T.take, S.npnct06.log, A.npnct14.log, H.npnct13.log, H.T.deal, A.T.new, S.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, H.P.today.in.politic, H.P.what.we.are, A.T.will, S.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, S.T.photo, H.T.new, S.npnct28.log, A.npnct28.log, H.P.daily.clip.report, A.T.day, H.T.news, H.T.first, S.T.first, H.T.X2014, S.T.past, A.T.compani, S.T.compani, S.T.report, A.T.word, H.T.morn, A.T.report, A.T.editor, S.T.word, H.T.busi, A.npnct13.log, S.T.share, H.npnct04.log, S.npnct13.log, S.T.articl, A.T.time, S.T.time, H.T.newyork, A.T.newyork, H.T.today, H.T.daili, H.T.report, S.T.newyork, H.T.day, A.T.intern, A.T.scene, S.T.scene, H.npnct14.log, S.T.diari, S.T.highlight, S.npnct04.log, H.T.X2015, S.npnct15.log, S.T.tribun, S.T.intern, A.T.photo, H.T.week, S.P.fashion.week, H.P.fashion.week, H.P.year.colon, H.T.fashion, H.npnct15.log, S.T.fashion, A.T.fashion, A.T.week, S.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.unq.log, S.nchrs.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0113 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](NYTBlogs_clusters_files/figure-html/fit.models_1-9.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_1-10.png) 

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
##                                        57 
## myCategory.fctrBusiness#Crosswords/Games# 
##                                        19 
##                           A.nwrds.unq.log 
##                                         7 
##                           S.nwrds.unq.log 
##                                         7 
##                               S.nchrs.log 
##                                         7 
##                               H.nchrs.log 
##                                         1 
##                      H.P.no.comment.colon 
##                                         1 
##                               S.nwrds.log 
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
##       myCategory.fctrBusiness#Crosswords/Games# < 0.5       to the left,  improve= 85.77765, (0 missing)
##       S.nchrs.log                               < 3.795426  to the right, improve= 77.76207, (0 missing)
##   Surrogate splits:
##       A.nwrds.unq.log      < 1.497866  to the right, agree=0.928, adj=0.127, (0 split)
##       S.nwrds.unq.log      < 1.497866  to the right, agree=0.927, adj=0.119, (0 split)
##       S.nchrs.log          < 3.725621  to the right, agree=0.927, adj=0.117, (0 split)
##       H.P.no.comment.colon < 0.5       to the left,  agree=0.919, adj=0.019, (0 split)
##       S.nwrds.log          < 2.564001  to the left,  agree=0.918, adj=0.011, (0 split)
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
##       H.nchrs.log                               < 2.861793  to the right, improve=21.22589, (0 missing)
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

![](NYTBlogs_clusters_files/figure-html/fit.models_1-11.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_1-12.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_1-13.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_1-14.png) 

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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 feats
## 1 WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, S.nwrds.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, S.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, H.npnct06.log, S.npnct01.log, S.T.can, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, A.T.said, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, A.T.one, A.T.state, S.T.state, S.T.one, H.P.s.notebook, H.T.take, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, PubDate.last100.log, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, A.npnct02.log, A.npnct17.log, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, A.T.obama, H.T.test, H.P.on.this.day, S.P.first.draft, A.T.take, S.T.take, S.npnct06.log, A.npnct14.log, H.npnct13.log, H.T.deal, A.T.new, S.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, H.P.today.in.politic, H.P.what.we.are, A.T.will, S.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, S.T.photo, H.T.new, S.npnct28.log, A.npnct28.log, H.P.daily.clip.report, A.T.day, H.T.news, H.T.first, S.T.first, H.T.X2014, S.T.past, A.T.compani, S.T.compani, S.T.report, A.T.word, H.T.morn, A.T.report, A.T.editor, S.T.word, H.T.busi, A.npnct13.log, S.T.share, H.npnct04.log, S.npnct13.log, S.T.articl, A.T.time, S.T.time, H.T.newyork, A.T.newyork, H.T.today, H.T.daili, H.T.report, S.T.newyork, H.T.day, A.T.intern, A.T.scene, S.T.scene, H.npnct14.log, S.T.diari, S.T.highlight, S.npnct04.log, H.T.X2015, S.npnct15.log, S.T.tribun, S.T.intern, A.T.photo, H.T.week, S.P.fashion.week, H.P.fashion.week, H.P.year.colon, H.T.fashion, H.npnct15.log, S.T.fashion, A.T.fashion, A.T.week, S.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.unq.log, S.nchrs.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      9.422                  2.04
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A.nuppr.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A.nuppr.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A.nuppr.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A.nuppr.log
## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           A.nuppr.log, A.nuppr.log:A.nwrds.log, A.nuppr.log:A.npnct19.log, A.nuppr.log:H.npnct16.log, A.nuppr.log:S.npnct01.log, A.nuppr.log:A.T.can, A.nuppr.log:A.npnct21.log, A.nuppr.log:S.T.said, A.nuppr.log:A.npnct23.log, A.nuppr.log:S.T.one, A.nuppr.log:S.T.state, A.nuppr.log:S.npnct07.log, A.nuppr.log:A.npnct18.log, A.nuppr.log:S.npnct03.log, A.nuppr.log:A.P.http, A.nuppr.log:A.npnct02.log, A.nuppr.log:S.P.year.colon, A.nuppr.log:S.npnct20.log, A.nuppr.log:S.T.obama, A.nuppr.log:S.P.first.draft, A.nuppr.log:A.T.take, A.nuppr.log:S.npnct06.log, A.nuppr.log:A.npnct17.log, A.nuppr.log:A.T.new, A.nuppr.log:S.P.metropolitan.diary.colon, A.nuppr.log:H.T.polit, A.nuppr.log:S.npnct12.log, A.nuppr.log:H.T.read, A.nuppr.log:A.T.will, A.nuppr.log:H.T.pictur, A.nuppr.log:S.npnct28.log, A.nuppr.log:S.P.daily.clip.report, A.nuppr.log:S.T.day, A.nuppr.log:H.P.first.draft, A.nuppr.log:A.T.appear, A.nuppr.log:A.T.compani, A.nuppr.log:A.npnct28.log, A.nuppr.log:S.T.report, A.nuppr.log:A.T.word, A.nuppr.log:H.T.billion, A.nuppr.log:A.npnct13.log, A.nuppr.log:S.T.past, A.nuppr.log:A.T.time, A.nuppr.log:H.P.today.in.politic, A.nuppr.log:A.P.daily.clip.report, A.nuppr.log:H.T.daili, A.nuppr.log:A.T.newyork, A.nuppr.log:A.T.scene, A.nuppr.log:S.T.scene, A.nuppr.log:S.npnct04.log, A.nuppr.log:S.T.diari, A.nuppr.log:S.npnct15.log, A.nuppr.log:S.T.tribun, A.nuppr.log:S.T.photo, A.nuppr.log:S.P.fashion.week, A.nuppr.log:S.T.intern, A.nuppr.log:H.P.fashion.week, A.nuppr.log:H.T.X2015, A.nuppr.log:A.T.week, A.nuppr.log:S.npnct11.log, A.nuppr.log:S.ndgts.log, A.nuppr.log:H.nuppr.log, A.nuppr.log:A.nchrs.log, A.nuppr.log:S.nchrs.log, A.nuppr.log:S.nuppr.log
## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                  WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, A.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, S.npnct01.log, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, S.T.state, S.T.one, H.P.s.notebook, H.T.take, A.npnct16.log, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, A.npnct08.log, PubDate.last100.log, .rnorm, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, H.T.test, S.npnct14.log, H.P.on.this.day, S.P.first.draft, A.T.take, S.npnct06.log, H.npnct13.log, H.T.deal, A.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, A.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, H.P.first.draft, H.T.new, S.npnct28.log, H.P.daily.clip.report, S.P.daily.clip.report, H.T.news, S.T.first, H.T.X2014, A.T.compani, S.T.report, A.T.word, A.T.editor, H.T.busi, A.npnct13.log, S.T.share, A.T.time, H.T.newyork, A.T.newyork, H.T.day, A.T.intern, A.T.scene, H.npnct14.log, S.T.highlight, S.npnct04.log, S.npnct15.log, S.T.tribun, H.T.week, S.P.fashion.week, H.P.fashion.week, H.npnct15.log, S.T.fashion, A.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, A.nchrs.log, A.nwrds.unq.log, S.nuppr.log
## 9  WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, S.nwrds.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, S.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, H.npnct06.log, S.npnct01.log, S.T.can, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, A.T.said, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, A.T.one, A.T.state, S.T.state, S.T.one, H.P.s.notebook, H.T.take, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, PubDate.last100.log, .rnorm, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, A.npnct02.log, A.npnct17.log, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, A.T.obama, H.T.test, H.P.on.this.day, S.P.first.draft, A.T.take, S.T.take, S.npnct06.log, A.npnct14.log, H.npnct13.log, H.T.deal, A.T.new, S.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, H.P.today.in.politic, H.P.what.we.are, A.T.will, S.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, S.T.photo, H.T.new, S.npnct28.log, A.npnct28.log, H.P.daily.clip.report, A.T.day, H.T.news, H.T.first, S.T.first, H.T.X2014, S.T.past, A.T.compani, S.T.compani, S.T.report, A.T.word, H.T.morn, A.T.report, A.T.editor, S.T.word, H.T.busi, A.npnct13.log, S.T.share, H.npnct04.log, S.npnct13.log, S.T.articl, A.T.time, S.T.time, H.T.newyork, A.T.newyork, H.T.today, H.T.daili, H.T.report, S.T.newyork, H.T.day, A.T.intern, A.T.scene, S.T.scene, H.npnct14.log, S.T.diari, S.T.highlight, S.npnct04.log, H.T.X2015, S.npnct15.log, S.T.tribun, S.T.intern, A.T.photo, H.T.week, S.P.fashion.week, H.P.fashion.week, H.P.year.colon, H.T.fashion, H.npnct15.log, S.T.fashion, A.T.fashion, A.T.week, S.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.unq.log, S.nchrs.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log
## 10         WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, S.nwrds.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, S.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, H.npnct06.log, S.npnct01.log, S.T.can, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, A.T.said, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, A.T.one, A.T.state, S.T.state, S.T.one, H.P.s.notebook, H.T.take, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, PubDate.last100.log, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, A.npnct02.log, A.npnct17.log, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, A.T.obama, H.T.test, H.P.on.this.day, S.P.first.draft, A.T.take, S.T.take, S.npnct06.log, A.npnct14.log, H.npnct13.log, H.T.deal, A.T.new, S.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, H.P.today.in.politic, H.P.what.we.are, A.T.will, S.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, S.T.photo, H.T.new, S.npnct28.log, A.npnct28.log, H.P.daily.clip.report, A.T.day, H.T.news, H.T.first, S.T.first, H.T.X2014, S.T.past, A.T.compani, S.T.compani, S.T.report, A.T.word, H.T.morn, A.T.report, A.T.editor, S.T.word, H.T.busi, A.npnct13.log, S.T.share, H.npnct04.log, S.npnct13.log, S.T.articl, A.T.time, S.T.time, H.T.newyork, A.T.newyork, H.T.today, H.T.daili, H.T.report, S.T.newyork, H.T.day, A.T.intern, A.T.scene, S.T.scene, H.npnct14.log, S.T.diari, S.T.highlight, S.npnct04.log, H.T.X2015, S.npnct15.log, S.T.tribun, S.T.intern, A.T.photo, H.T.week, S.P.fashion.week, H.P.fashion.week, H.P.year.colon, H.T.fashion, H.npnct15.log, S.T.fashion, A.T.fashion, A.T.week, S.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.unq.log, S.nchrs.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log
##    max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1                0                      0.747                 0.003
## 2                0                      0.253                 0.002
## 3                0                      0.595                 0.053
## 4                0                      0.513                 0.054
## 5                1                      1.106                 0.053
## 6                1                      1.119                 0.077
## 7                1                      3.425                 1.367
## 8                1                      8.837                 4.390
## 9                1                     12.416                 6.335
## 10               3                      9.422                 2.040
##    max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.5000000                    0.5       0.0000000        0.8326257
## 2    0.5007516                    0.1       0.2867534        0.1673743
## 3    0.5000000                    0.5       0.0000000        0.8326257
## 4    0.5000000                    0.5       0.0000000        0.8326257
## 5    0.5000000                    0.5       0.0000000        0.8326258
## 6    0.7073742                    0.2       0.3986014        0.8324022
## 7    0.8101600                    0.3       0.4736530        0.8513966
## 8    0.9557180                    0.3       0.7595731        0.9072625
## 9    0.9583173                    0.3       0.7647059        0.9065922
## 10   0.7277461                    0.7       0.5978351        0.8934084
##    max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.8213602             0.8434553  0.0000000000   0.5000000
## 2              0.1565447             0.1786398  0.0000000000   0.4909227
## 3              0.8213602             0.8434553  0.0000000000   0.5000000
## 4              0.8213602             0.8434553  0.0000000000   0.5000000
## 5              0.8213602             0.8434553  0.0000000000   0.5000000
## 6              0.7176970             0.7439004 -0.0004459345   0.7102060
## 7              0.7893440             0.8129430  0.2402243279   0.7859240
## 8              0.9058322             0.9224492  0.6495169633   0.9200352
## 9              0.9074636             0.9239441  0.6463354232   0.9150986
## 10             0.8826068             0.9010121  0.5566658958   0.7084504
##    opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                     0.5       0.0000000        0.8327662
## 2                     0.1       0.2865473        0.1672338
## 3                     0.5       0.0000000        0.8327662
## 4                     0.5       0.0000000        0.8327662
## 5                     0.5       0.0000000        0.8327662
## 6                     0.2       0.3880266        0.7316480
## 7                     0.3       0.4637306        0.7987360
## 8                     0.4       0.7261905        0.9105493
## 9                     0.3       0.7144790        0.8974234
## 10                    0.7       0.5650558        0.8862421
##    max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.8159247             0.8486533     0.0000000
## 2              0.1513467             0.1840753     0.0000000
## 3              0.8159247             0.8486533     0.0000000
## 4              0.8159247             0.8486533     0.0000000
## 5              0.8159247             0.8486533     0.0000000
## 6              0.7119353             0.7506985     0.2283681
## 7              0.7807460             0.8158705     0.3416531
## 8              0.8973805             0.9225338     0.6727694
## 9              0.8834974             0.9102047     0.6523231
## 10             0.8717239             0.8996488     0.5054039
##    max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## 1                  NA              NA          NA
## 2                  NA              NA          NA
## 3                  NA              NA          NA
## 4                  NA              NA          NA
## 5        0.0002791548    0.0000000000          NA
## 6        0.0000648833    0.0007723812    3714.601
## 7        0.0036948691    0.0175605313    3317.941
## 8        0.0020547439    0.0076535367    2045.192
## 9        0.0003702469    0.0060707691    2078.788
## 10       0.0030411362    0.0292229339          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 3 fit.models_1_rpart          3          0 276.33 290.21   13.88
## 4   fit.models_1_end          4          0 290.21     NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1 252.084 290.217  38.133
## 12 fit.models          7          2 290.218      NA      NA
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A.nuppr.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A.nuppr.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A.nuppr.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A.nuppr.log
## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           A.nuppr.log, A.nuppr.log:A.nwrds.log, A.nuppr.log:A.npnct19.log, A.nuppr.log:H.npnct16.log, A.nuppr.log:S.npnct01.log, A.nuppr.log:A.T.can, A.nuppr.log:A.npnct21.log, A.nuppr.log:S.T.said, A.nuppr.log:A.npnct23.log, A.nuppr.log:S.T.one, A.nuppr.log:S.T.state, A.nuppr.log:S.npnct07.log, A.nuppr.log:A.npnct18.log, A.nuppr.log:S.npnct03.log, A.nuppr.log:A.P.http, A.nuppr.log:A.npnct02.log, A.nuppr.log:S.P.year.colon, A.nuppr.log:S.npnct20.log, A.nuppr.log:S.T.obama, A.nuppr.log:S.P.first.draft, A.nuppr.log:A.T.take, A.nuppr.log:S.npnct06.log, A.nuppr.log:A.npnct17.log, A.nuppr.log:A.T.new, A.nuppr.log:S.P.metropolitan.diary.colon, A.nuppr.log:H.T.polit, A.nuppr.log:S.npnct12.log, A.nuppr.log:H.T.read, A.nuppr.log:A.T.will, A.nuppr.log:H.T.pictur, A.nuppr.log:S.npnct28.log, A.nuppr.log:S.P.daily.clip.report, A.nuppr.log:S.T.day, A.nuppr.log:H.P.first.draft, A.nuppr.log:A.T.appear, A.nuppr.log:A.T.compani, A.nuppr.log:A.npnct28.log, A.nuppr.log:S.T.report, A.nuppr.log:A.T.word, A.nuppr.log:H.T.billion, A.nuppr.log:A.npnct13.log, A.nuppr.log:S.T.past, A.nuppr.log:A.T.time, A.nuppr.log:H.P.today.in.politic, A.nuppr.log:A.P.daily.clip.report, A.nuppr.log:H.T.daili, A.nuppr.log:A.T.newyork, A.nuppr.log:A.T.scene, A.nuppr.log:S.T.scene, A.nuppr.log:S.npnct04.log, A.nuppr.log:S.T.diari, A.nuppr.log:S.npnct15.log, A.nuppr.log:S.T.tribun, A.nuppr.log:S.T.photo, A.nuppr.log:S.P.fashion.week, A.nuppr.log:S.T.intern, A.nuppr.log:H.P.fashion.week, A.nuppr.log:H.T.X2015, A.nuppr.log:A.T.week, A.nuppr.log:S.npnct11.log, A.nuppr.log:S.ndgts.log, A.nuppr.log:H.nuppr.log, A.nuppr.log:A.nchrs.log, A.nuppr.log:S.nchrs.log, A.nuppr.log:S.nuppr.log
## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                  WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, A.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, S.npnct01.log, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, S.T.state, S.T.one, H.P.s.notebook, H.T.take, A.npnct16.log, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, A.npnct08.log, PubDate.last100.log, .rnorm, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, H.T.test, S.npnct14.log, H.P.on.this.day, S.P.first.draft, A.T.take, S.npnct06.log, H.npnct13.log, H.T.deal, A.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, A.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, H.P.first.draft, H.T.new, S.npnct28.log, H.P.daily.clip.report, S.P.daily.clip.report, H.T.news, S.T.first, H.T.X2014, A.T.compani, S.T.report, A.T.word, A.T.editor, H.T.busi, A.npnct13.log, S.T.share, A.T.time, H.T.newyork, A.T.newyork, H.T.day, A.T.intern, A.T.scene, H.npnct14.log, S.T.highlight, S.npnct04.log, S.npnct15.log, S.T.tribun, H.T.week, S.P.fashion.week, H.P.fashion.week, H.npnct15.log, S.T.fashion, A.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, A.nchrs.log, A.nwrds.unq.log, S.nuppr.log
## 9  WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, S.nwrds.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, S.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, H.npnct06.log, S.npnct01.log, S.T.can, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, A.T.said, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, A.T.one, A.T.state, S.T.state, S.T.one, H.P.s.notebook, H.T.take, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, PubDate.last100.log, .rnorm, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, A.npnct02.log, A.npnct17.log, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, A.T.obama, H.T.test, H.P.on.this.day, S.P.first.draft, A.T.take, S.T.take, S.npnct06.log, A.npnct14.log, H.npnct13.log, H.T.deal, A.T.new, S.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, H.P.today.in.politic, H.P.what.we.are, A.T.will, S.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, S.T.photo, H.T.new, S.npnct28.log, A.npnct28.log, H.P.daily.clip.report, A.T.day, H.T.news, H.T.first, S.T.first, H.T.X2014, S.T.past, A.T.compani, S.T.compani, S.T.report, A.T.word, H.T.morn, A.T.report, A.T.editor, S.T.word, H.T.busi, A.npnct13.log, S.T.share, H.npnct04.log, S.npnct13.log, S.T.articl, A.T.time, S.T.time, H.T.newyork, A.T.newyork, H.T.today, H.T.daili, H.T.report, S.T.newyork, H.T.day, A.T.intern, A.T.scene, S.T.scene, H.npnct14.log, S.T.diari, S.T.highlight, S.npnct04.log, H.T.X2015, S.npnct15.log, S.T.tribun, S.T.intern, A.T.photo, H.T.week, S.P.fashion.week, H.P.fashion.week, H.P.year.colon, H.T.fashion, H.npnct15.log, S.T.fashion, A.T.fashion, A.T.week, S.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.unq.log, S.nchrs.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log
## 10         WordCount.log, H.nwrds.log, PubDate.hour.fctr, H.npnct19.log, S.nwrds.log, A.nwrds.log, PubDate.wkend, H.P.recap.colon, H.P.quandary, H.P.no.comment.colon, S.npnct19.log, H.npnct08.log, PubDate.last10.log, PubDate.last1.log, H.P.readers.respond, S.T.make, H.T.get, H.npnct06.log, S.npnct01.log, S.T.can, H.npnct16.log, A.T.can, H.T.ebola, H.npnct01.log, A.T.said, S.T.said, H.T.make, H.npnct11.log, myCategory.fctr, A.T.one, A.T.state, S.T.state, S.T.one, H.P.s.notebook, H.T.take, S.npnct16.log, H.T.time, S.T.presid, S.npnct08.log, PubDate.last100.log, H.npnct05.log, H.T.obama, H.T.say, H.T.bank, PubDate.date.fctr, PubDate.second.fctr, H.npnct07.log, S.npnct07.log, S.npnct03.log, A.npnct18.log, H.npnct12.log, H.T.word, H.T.big, A.npnct02.log, A.npnct17.log, S.P.year.colon, S.npnct20.log, S.T.obama, H.npnct02.log, A.T.obama, H.T.test, H.P.on.this.day, S.P.first.draft, A.T.take, S.T.take, S.npnct06.log, A.npnct14.log, H.npnct13.log, H.T.deal, A.T.new, S.T.new, S.P.metropolitan.diary.colon, H.T.billion, H.T.polit, H.T.china, H.P.verbatim.colon, H.T.art, PubDate.minute.fctr, H.T.read, A.T.year, S.npnct12.log, H.P.today.in.politic, H.P.what.we.are, A.T.will, S.T.will, A.T.appear, PubDate.wkday.fctr, H.T.pictur, A.T.senat, S.T.day, S.T.show, H.P.today.in.smallbusiness, S.T.photo, H.T.new, S.npnct28.log, A.npnct28.log, H.P.daily.clip.report, A.T.day, H.T.news, H.T.first, S.T.first, H.T.X2014, S.T.past, A.T.compani, S.T.compani, S.T.report, A.T.word, H.T.morn, A.T.report, A.T.editor, S.T.word, H.T.busi, A.npnct13.log, S.T.share, H.npnct04.log, S.npnct13.log, S.T.articl, A.T.time, S.T.time, H.T.newyork, A.T.newyork, H.T.today, H.T.daili, H.T.report, S.T.newyork, H.T.day, A.T.intern, A.T.scene, S.T.scene, H.npnct14.log, S.T.diari, S.T.highlight, S.npnct04.log, H.T.X2015, S.npnct15.log, S.T.tribun, S.T.intern, A.T.photo, H.T.week, S.P.fashion.week, H.P.fashion.week, H.P.year.colon, H.T.fashion, H.npnct15.log, S.T.fashion, A.T.fashion, A.T.week, S.T.week, H.npnct28.log, S.npnct11.log, H.ndgts.log, S.ndgts.log, H.nuppr.log, H.nchrs.log, H.nwrds.unq.log, S.nchrs.log, A.nwrds.unq.log, S.nwrds.unq.log, S.nuppr.log
##    max.nTuningRuns max.auc.fit opt.prob.threshold.fit max.f.score.fit
## 1                0   0.5000000                    0.5       0.0000000
## 2                0   0.5007516                    0.1       0.2867534
## 3                0   0.5000000                    0.5       0.0000000
## 4                0   0.5000000                    0.5       0.0000000
## 5                1   0.5000000                    0.5       0.0000000
## 6                1   0.7073742                    0.2       0.3986014
## 7                1   0.8101600                    0.3       0.4736530
## 8                1   0.9557180                    0.3       0.7595731
## 9                1   0.9583173                    0.3       0.7647059
## 10               3   0.7277461                    0.7       0.5978351
##    max.Accuracy.fit max.Kappa.fit max.auc.OOB opt.prob.threshold.OOB
## 1         0.8326257  0.0000000000   0.5000000                    0.5
## 2         0.1673743  0.0000000000   0.4909227                    0.1
## 3         0.8326257  0.0000000000   0.5000000                    0.5
## 4         0.8326257  0.0000000000   0.5000000                    0.5
## 5         0.8326258  0.0000000000   0.5000000                    0.5
## 6         0.8324022 -0.0004459345   0.7102060                    0.2
## 7         0.8513966  0.2402243279   0.7859240                    0.3
## 8         0.9072625  0.6495169633   0.9200352                    0.4
## 9         0.9065922  0.6463354232   0.9150986                    0.3
## 10        0.8934084  0.5566658958   0.7084504                    0.7
##    max.f.score.OOB max.Accuracy.OOB max.Kappa.OOB
## 1        0.0000000        0.8327662     0.0000000
## 2        0.2865473        0.1672338     0.0000000
## 3        0.0000000        0.8327662     0.0000000
## 4        0.0000000        0.8327662     0.0000000
## 5        0.0000000        0.8327662     0.0000000
## 6        0.3880266        0.7316480     0.2283681
## 7        0.4637306        0.7987360     0.3416531
## 8        0.7261905        0.9105493     0.6727694
## 9        0.7144790        0.8974234     0.6523231
## 10       0.5650558        0.8862421     0.5054039
##    inv.elapsedtime.everything inv.elapsedtime.final  inv.aic.fit
## 1                  1.33868809           333.3333333           NA
## 2                  3.95256917           500.0000000           NA
## 3                  1.68067227            18.8679245           NA
## 4                  1.94931774            18.5185185           NA
## 5                  0.90415913            18.8679245           NA
## 6                  0.89365505            12.9870130 0.0002692079
## 7                  0.29197080             0.7315289 0.0003013917
## 8                  0.11316057             0.2277904 0.0004889518
## 9                  0.08054124             0.1578532 0.0004810495
## 10                 0.10613458             0.4901961           NA
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
## 10. Consider specifying shapes manually. if you must have them.
```

```
## Warning: Removed 5 rows containing missing values (geom_path).
```

```
## Warning: Removed 60 rows containing missing values (geom_point).
```

```
## Warning: Removed 6 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 10. Consider specifying shapes manually. if you must have them.
```

![](NYTBlogs_clusters_files/figure-html/fit.models_2-1.png) 

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

![](NYTBlogs_clusters_files/figure-html/fit.models_2-2.png) 

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
## 8              Low.cor.X.glm        0.9105493   0.9200352     0.6727694
## 9                  All.X.glm        0.8974234   0.9150986     0.6523231
## 10      All.X.no.rnorm.rpart        0.8862421   0.7084504     0.5054039
## 1          MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.8327662   0.5000000     0.0000000
## 5            Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 7    Interact.High.cor.Y.glm        0.7987360   0.7859240     0.3416531
## 6              Max.cor.Y.glm        0.7316480   0.7102060     0.2283681
## 2    Random.myrandom_classfr        0.1672338   0.4909227     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 8     2045.192                    0.4
## 9     2078.788                    0.3
## 10          NA                    0.7
## 1           NA                    0.5
## 3           NA                    0.5
## 4           NA                    0.5
## 5           NA                    0.5
## 7     3317.941                    0.3
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
## 10. Consider specifying shapes manually. if you must have them.
```

```
## Warning: Removed 27 rows containing missing values (geom_point).
```

```
## Warning: Removed 6 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 10. Consider specifying shapes manually. if you must have them.
```

![](NYTBlogs_clusters_files/figure-html/fit.models_2-3.png) 

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
## [1] "Best model id: Low.cor.X.glm"
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

![](NYTBlogs_clusters_files/figure-html/fit.models_2-4.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_2-5.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_2-6.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](NYTBlogs_clusters_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.2541  -0.2961  -0.1167   0.0000   3.5431  
## 
## Coefficients: (3 not defined because of singularities)
##                                                         Estimate
## (Intercept)                                           -1.204e+00
## WordCount.log                                          1.198e+00
## H.nwrds.log                                            5.759e-01
## `PubDate.hour.fctr(7.67,15.3]`                         2.076e-01
## `PubDate.hour.fctr(15.3,23]`                           3.391e-01
## H.npnct19.log                                          1.346e+00
## A.nwrds.log                                           -1.226e+00
## PubDate.wkend                                         -2.851e-01
## H.P.recap.colon                                        1.328e+00
## H.P.quandary                                           2.206e+01
## H.P.no.comment.colon                                   2.371e+00
## A.npnct19.log                                          1.641e+00
## H.npnct08.log                                          1.326e+00
## PubDate.last10.log                                     2.011e-01
## PubDate.last1.log                                     -3.762e-02
## H.P.readers.respond                                    6.883e+00
## S.T.make                                              -1.161e+00
## H.T.get                                                4.199e-01
## S.npnct01.log                                          1.713e+00
## H.npnct16.log                                          7.538e-01
## A.T.can                                               -1.136e+00
## H.T.ebola                                              2.054e-02
## H.npnct01.log                                         -1.581e+00
## S.T.said                                               1.084e+00
## H.T.make                                              -1.114e-01
## H.npnct11.log                                          4.617e-01
## `myCategory.fctrForeign#World#Asia Pacific`           -3.806e+00
## `myCategory.fctr#Multimedia#`                         -4.242e+00
## `myCategory.fctrCulture#Arts#`                        -3.210e+00
## `myCategory.fctrBusiness#Business Day#Dealbook`       -2.641e+00
## myCategory.fctrmyOther                                -2.092e+01
## `myCategory.fctrBusiness#Technology#`                 -1.982e+00
## `myCategory.fctrBusiness#Crosswords/Games#`            6.723e-01
## `myCategory.fctrTStyle##`                             -4.380e+00
## `myCategory.fctrForeign#World#`                       -1.789e+01
## `myCategory.fctrOpEd#Opinion#`                         5.482e-01
## `myCategory.fctrStyles##Fashion`                      -2.070e+01
## `myCategory.fctr#Opinion#Room For Debate`             -7.207e+00
## `myCategory.fctr#U.S.#Education`                      -2.270e+01
## `myCategory.fctr##`                                   -2.653e+00
## `myCategory.fctrMetro#N.Y. / Region#`                 -2.096e+00
## `myCategory.fctrBusiness#Business Day#Small Business` -4.342e+00
## `myCategory.fctrStyles#U.S.#`                         -6.593e-01
## `myCategory.fctrTravel#Travel#`                       -4.107e+00
## `myCategory.fctr#Opinion#The Public Editor`            8.029e-01
## S.T.state                                              1.473e+00
## S.T.one                                               -1.042e+00
## H.P.s.notebook                                        -1.734e+01
## H.T.take                                              -3.269e-01
## A.npnct16.log                                         -9.075e-02
## S.npnct16.log                                                 NA
## H.T.time                                               1.308e-01
## S.T.presid                                             2.548e-01
## S.npnct08.log                                          9.327e-01
## A.npnct08.log                                                 NA
## PubDate.last100.log                                    1.604e-02
## .rnorm                                                -8.863e-02
## H.npnct05.log                                         -2.517e+01
## H.T.obama                                             -2.215e-01
## H.T.say                                               -5.420e-01
## H.T.bank                                               3.457e-01
## `PubDate.date.fctr(7,13]`                             -1.005e-01
## `PubDate.date.fctr(13,19]`                            -1.988e-01
## `PubDate.date.fctr(19,25]`                            -1.533e-01
## `PubDate.date.fctr(25,31]`                             2.708e-02
## `PubDate.second.fctr(14.8,29.5]`                       9.995e-02
## `PubDate.second.fctr(29.5,44.2]`                       2.368e-02
## `PubDate.second.fctr(44.2,59.1]`                      -2.668e-01
## H.npnct07.log                                          2.122e-01
## S.npnct07.log                                         -2.478e+01
## S.npnct03.log                                         -2.938e+01
## A.npnct18.log                                         -2.835e+01
## H.npnct12.log                                          2.653e-01
## H.T.word                                               2.124e+00
## H.T.big                                               -4.310e-01
## S.P.year.colon                                        -1.036e+01
## S.npnct20.log                                         -2.520e+01
## S.T.obama                                             -2.932e-01
## H.npnct02.log                                         -1.833e+01
## H.T.test                                              -1.079e-02
## S.npnct14.log                                          8.324e-01
## H.P.on.this.day                                       -1.534e+01
## S.P.first.draft                                       -1.540e+01
## A.T.take                                              -7.912e-01
## S.npnct06.log                                          2.972e-01
## H.npnct13.log                                         -1.617e-01
## H.T.deal                                              -2.285e+01
## A.T.new                                                1.806e-01
## S.P.metropolitan.diary.colon                          -8.674e-02
## H.T.billion                                           -4.016e-01
## H.T.polit                                             -9.185e-01
## H.T.china                                             -7.947e-01
## H.P.verbatim.colon                                    -1.514e+01
## H.T.art                                               -9.047e-01
## `PubDate.minute.fctr(14.8,29.5]`                      -1.156e-01
## `PubDate.minute.fctr(29.5,44.2]`                      -2.258e-01
## `PubDate.minute.fctr(44.2,59.1]`                       4.096e-02
## H.T.read                                              -1.136e+00
## A.T.year                                               2.150e-01
## S.npnct12.log                                         -1.650e-01
## A.T.will                                              -1.152e+00
## A.T.appear                                            -5.717e-01
## PubDate.wkday.fctr1                                   -6.478e-01
## PubDate.wkday.fctr2                                   -1.168e+00
## PubDate.wkday.fctr3                                   -7.645e-01
## PubDate.wkday.fctr4                                   -9.341e-01
## PubDate.wkday.fctr5                                   -8.733e-01
## PubDate.wkday.fctr6                                   -1.158e+00
## H.T.pictur                                             1.395e-01
## A.T.senat                                              3.430e-01
## S.T.day                                               -7.347e-01
## S.T.show                                              -1.928e+00
## H.P.today.in.smallbusiness                            -1.574e+01
## H.P.first.draft                                       -1.630e+01
## H.T.new                                               -5.225e-01
## S.npnct28.log                                         -1.476e+01
## H.P.daily.clip.report                                 -1.677e+01
## S.P.daily.clip.report                                         NA
## H.T.news                                              -8.912e-01
## S.T.first                                              7.631e-02
## H.T.X2014                                             -5.151e-01
## A.T.compani                                           -8.798e-01
## S.T.report                                            -2.059e+00
## A.T.word                                              -7.148e-01
## A.T.editor                                            -2.110e+00
## H.T.busi                                              -4.715e-01
## A.npnct13.log                                          1.013e+00
## S.T.share                                             -1.739e+00
## A.T.time                                              -7.228e-01
## H.T.newyork                                           -5.928e-01
## A.T.newyork                                            2.942e+00
## H.T.day                                               -4.041e-01
## A.T.intern                                            -2.279e+00
## A.T.scene                                             -4.357e+01
## H.npnct14.log                                         -2.043e+01
## S.T.highlight                                          1.196e+00
## S.npnct04.log                                         -1.086e+00
## S.npnct15.log                                          2.079e-01
## S.T.tribun                                            -4.239e+01
## H.T.week                                              -8.896e-01
## S.P.fashion.week                                       2.283e+00
## H.P.fashion.week                                      -1.211e+01
## H.npnct15.log                                         -1.188e+00
## S.T.fashion                                           -5.430e+01
## A.T.week                                              -5.344e-01
## H.npnct28.log                                         -5.339e-01
## S.npnct11.log                                         -1.971e-01
## H.ndgts.log                                            2.962e-01
## S.ndgts.log                                           -3.608e-01
## H.nuppr.log                                            1.163e+00
## H.nchrs.log                                           -1.636e+00
## A.nchrs.log                                            2.849e-01
## A.nwrds.unq.log                                       -1.024e+00
## S.nuppr.log                                           -5.662e-01
##                                                       Std. Error z value
## (Intercept)                                            3.116e+00  -0.386
## WordCount.log                                          9.882e-02  12.119
## H.nwrds.log                                            7.342e-01   0.784
## `PubDate.hour.fctr(7.67,15.3]`                         2.493e-01   0.833
## `PubDate.hour.fctr(15.3,23]`                           2.548e-01   1.331
## H.npnct19.log                                          3.298e-01   4.082
## A.nwrds.log                                            8.415e-01  -1.456
## PubDate.wkend                                          4.543e-01  -0.628
## H.P.recap.colon                                        5.963e-01   2.227
## H.P.quandary                                           6.754e+03   0.003
## H.P.no.comment.colon                                   1.066e+00   2.223
## A.npnct19.log                                          3.477e-01   4.720
## H.npnct08.log                                          4.553e-01   2.911
## PubDate.last10.log                                     1.261e-01   1.595
## PubDate.last1.log                                      4.553e-02  -0.826
## H.P.readers.respond                                    1.117e+00   6.160
## S.T.make                                               6.237e-01  -1.861
## H.T.get                                                4.381e-01   0.958
## S.npnct01.log                                          1.895e+00   0.904
## H.npnct16.log                                          6.132e-01   1.229
## A.T.can                                                8.465e-01  -1.342
## H.T.ebola                                              3.135e-01   0.066
## H.npnct01.log                                          1.310e+00  -1.207
## S.T.said                                               8.391e-01   1.292
## H.T.make                                               3.443e-01  -0.324
## H.npnct11.log                                          2.139e-01   2.158
## `myCategory.fctrForeign#World#Asia Pacific`            6.904e-01  -5.513
## `myCategory.fctr#Multimedia#`                          8.152e-01  -5.204
## `myCategory.fctrCulture#Arts#`                         4.334e-01  -7.406
## `myCategory.fctrBusiness#Business Day#Dealbook`        3.230e-01  -8.176
## myCategory.fctrmyOther                                 2.994e+03  -0.007
## `myCategory.fctrBusiness#Technology#`                  3.342e-01  -5.930
## `myCategory.fctrBusiness#Crosswords/Games#`            5.402e-01   1.244
## `myCategory.fctrTStyle##`                              5.061e-01  -8.653
## `myCategory.fctrForeign#World#`                        1.102e+03  -0.016
## `myCategory.fctrOpEd#Opinion#`                         3.062e-01   1.790
## `myCategory.fctrStyles##Fashion`                       1.653e+03  -0.013
## `myCategory.fctr#Opinion#Room For Debate`              8.625e-01  -8.356
## `myCategory.fctr#U.S.#Education`                       9.504e+02  -0.024
## `myCategory.fctr##`                                    3.036e-01  -8.738
## `myCategory.fctrMetro#N.Y. / Region#`                  5.664e-01  -3.702
## `myCategory.fctrBusiness#Business Day#Small Business`  7.101e-01  -6.115
## `myCategory.fctrStyles#U.S.#`                          3.471e-01  -1.899
## `myCategory.fctrTravel#Travel#`                        1.058e+00  -3.883
## `myCategory.fctr#Opinion#The Public Editor`            1.211e+00   0.663
## S.T.state                                              8.412e-01   1.751
## S.T.one                                                6.249e-01  -1.668
## H.P.s.notebook                                         8.530e+03  -0.002
## H.T.take                                               4.774e-01  -0.685
## A.npnct16.log                                          1.368e+00  -0.066
## S.npnct16.log                                                 NA      NA
## H.T.time                                               3.044e-01   0.430
## S.T.presid                                             8.365e-01   0.305
## S.npnct08.log                                          6.398e-01   1.458
## A.npnct08.log                                                 NA      NA
## PubDate.last100.log                                    4.539e-02   0.353
## .rnorm                                                 6.536e-02  -1.356
## H.npnct05.log                                          1.024e+04  -0.002
## H.T.obama                                              4.607e-01  -0.481
## H.T.say                                                4.355e-01  -1.244
## H.T.bank                                               4.874e-01   0.709
## `PubDate.date.fctr(7,13]`                              2.022e-01  -0.497
## `PubDate.date.fctr(13,19]`                             1.991e-01  -0.999
## `PubDate.date.fctr(19,25]`                             1.967e-01  -0.779
## `PubDate.date.fctr(25,31]`                             2.128e-01   0.127
## `PubDate.second.fctr(14.8,29.5]`                       1.805e-01   0.554
## `PubDate.second.fctr(29.5,44.2]`                       1.765e-01   0.134
## `PubDate.second.fctr(44.2,59.1]`                       1.829e-01  -1.459
## H.npnct07.log                                          2.016e-01   1.052
## S.npnct07.log                                          1.111e+04  -0.002
## S.npnct03.log                                          8.755e+03  -0.003
## A.npnct18.log                                          9.059e+03  -0.003
## H.npnct12.log                                          3.163e-01   0.839
## H.T.word                                               8.960e-01   2.370
## H.T.big                                                6.097e-01  -0.707
## S.P.year.colon                                         3.599e+03  -0.003
## S.npnct20.log                                          7.543e+03  -0.003
## S.T.obama                                              8.928e-01  -0.328
## H.npnct02.log                                          5.008e+03  -0.004
## H.T.test                                               7.158e-01  -0.015
## S.npnct14.log                                          1.758e+00   0.473
## H.P.on.this.day                                        5.500e+03  -0.003
## S.P.first.draft                                        4.226e+03  -0.004
## A.T.take                                               1.045e+00  -0.757
## S.npnct06.log                                          1.553e+00   0.191
## H.npnct13.log                                          1.996e-01  -0.810
## H.T.deal                                               2.753e+03  -0.008
## A.T.new                                                7.327e-01   0.246
## S.P.metropolitan.diary.colon                           8.570e-01  -0.101
## H.T.billion                                            8.466e-01  -0.474
## H.T.polit                                              3.374e-01  -2.722
## H.T.china                                              1.002e+00  -0.793
## H.P.verbatim.colon                                     3.709e+03  -0.004
## H.T.art                                                8.627e-01  -1.049
## `PubDate.minute.fctr(14.8,29.5]`                       1.867e-01  -0.619
## `PubDate.minute.fctr(29.5,44.2]`                       1.836e-01  -1.230
## `PubDate.minute.fctr(44.2,59.1]`                       1.881e-01   0.218
## H.T.read                                               3.940e-01  -2.884
## A.T.year                                               9.183e-01   0.234
## S.npnct12.log                                          2.086e-01  -0.791
## A.T.will                                               7.988e-01  -1.442
## A.T.appear                                             1.109e+00  -0.516
## PubDate.wkday.fctr1                                    5.378e-01  -1.205
## PubDate.wkday.fctr2                                    5.879e-01  -1.988
## PubDate.wkday.fctr3                                    5.801e-01  -1.318
## PubDate.wkday.fctr4                                    5.726e-01  -1.631
## PubDate.wkday.fctr5                                    5.803e-01  -1.505
## PubDate.wkday.fctr6                                    5.017e-01  -2.308
## H.T.pictur                                             6.845e-01   0.204
## A.T.senat                                              8.750e-01   0.392
## S.T.day                                                1.018e+00  -0.722
## S.T.show                                               1.208e+00  -1.596
## H.P.today.in.smallbusiness                             2.890e+03  -0.005
## H.P.first.draft                                        2.278e+03  -0.007
## H.T.new                                                4.869e-01  -1.073
## S.npnct28.log                                          2.102e+03  -0.007
## H.P.daily.clip.report                                  2.626e+03  -0.006
## S.P.daily.clip.report                                         NA      NA
## H.T.news                                               7.994e-01  -1.115
## S.T.first                                              1.033e+00   0.074
## H.T.X2014                                              9.100e-01  -0.566
## A.T.compani                                            9.135e-01  -0.963
## S.T.report                                             1.214e+00  -1.697
## A.T.word                                               1.077e+00  -0.664
## A.T.editor                                             1.879e+00  -1.123
## H.T.busi                                               7.578e-01  -0.622
## A.npnct13.log                                          2.716e-01   3.732
## S.T.share                                              1.056e+00  -1.646
## A.T.time                                               9.710e-01  -0.744
## H.T.newyork                                            5.432e-01  -1.091
## A.T.newyork                                            1.039e+00   2.833
## H.T.day                                                6.410e-01  -0.630
## A.T.intern                                             2.556e+00  -0.892
## A.T.scene                                              2.368e+03  -0.018
## H.npnct14.log                                          1.808e+03  -0.011
## S.T.highlight                                          2.326e+00   0.514
## S.npnct04.log                                          6.941e-01  -1.564
## S.npnct15.log                                          5.331e-01   0.390
## S.T.tribun                                             2.552e+03  -0.017
## H.T.week                                               7.026e-01  -1.266
## S.P.fashion.week                                       1.128e+03   0.002
## H.P.fashion.week                                       9.413e+02  -0.013
## H.npnct15.log                                          3.473e-01  -3.421
## S.T.fashion                                            2.686e+03  -0.020
## A.T.week                                               8.535e-01  -0.626
## H.npnct28.log                                          1.721e+00  -0.310
## S.npnct11.log                                          1.496e-01  -1.318
## H.ndgts.log                                            2.412e-01   1.228
## S.ndgts.log                                            1.608e-01  -2.245
## H.nuppr.log                                            4.350e-01   2.674
## H.nchrs.log                                            3.712e-01  -4.407
## A.nchrs.log                                            5.081e-01   0.561
## A.nwrds.unq.log                                        5.664e-01  -1.808
## S.nuppr.log                                            1.622e-01  -3.491
##                                                       Pr(>|z|)    
## (Intercept)                                           0.699152    
## WordCount.log                                          < 2e-16 ***
## H.nwrds.log                                           0.432833    
## `PubDate.hour.fctr(7.67,15.3]`                        0.405052    
## `PubDate.hour.fctr(15.3,23]`                          0.183273    
## H.npnct19.log                                         4.46e-05 ***
## A.nwrds.log                                           0.145282    
## PubDate.wkend                                         0.530297    
## H.P.recap.colon                                       0.025936 *  
## H.P.quandary                                          0.997394    
## H.P.no.comment.colon                                  0.026197 *  
## A.npnct19.log                                         2.36e-06 ***
## H.npnct08.log                                         0.003598 ** 
## PubDate.last10.log                                    0.110759    
## PubDate.last1.log                                     0.408662    
## H.P.readers.respond                                   7.29e-10 ***
## S.T.make                                              0.062679 .  
## H.T.get                                               0.337812    
## S.npnct01.log                                         0.365941    
## H.npnct16.log                                         0.218975    
## A.T.can                                               0.179638    
## H.T.ebola                                             0.947751    
## H.npnct01.log                                         0.227588    
## S.T.said                                              0.196228    
## H.T.make                                              0.746262    
## H.npnct11.log                                         0.030930 *  
## `myCategory.fctrForeign#World#Asia Pacific`           3.53e-08 ***
## `myCategory.fctr#Multimedia#`                         1.95e-07 ***
## `myCategory.fctrCulture#Arts#`                        1.30e-13 ***
## `myCategory.fctrBusiness#Business Day#Dealbook`       2.92e-16 ***
## myCategory.fctrmyOther                                0.994424    
## `myCategory.fctrBusiness#Technology#`                 3.03e-09 ***
## `myCategory.fctrBusiness#Crosswords/Games#`           0.213335    
## `myCategory.fctrTStyle##`                              < 2e-16 ***
## `myCategory.fctrForeign#World#`                       0.987049    
## `myCategory.fctrOpEd#Opinion#`                        0.073410 .  
## `myCategory.fctrStyles##Fashion`                      0.990010    
## `myCategory.fctr#Opinion#Room For Debate`              < 2e-16 ***
## `myCategory.fctr#U.S.#Education`                      0.980947    
## `myCategory.fctr##`                                    < 2e-16 ***
## `myCategory.fctrMetro#N.Y. / Region#`                 0.000214 ***
## `myCategory.fctrBusiness#Business Day#Small Business` 9.67e-10 ***
## `myCategory.fctrStyles#U.S.#`                         0.057539 .  
## `myCategory.fctrTravel#Travel#`                       0.000103 ***
## `myCategory.fctr#Opinion#The Public Editor`           0.507368    
## S.T.state                                             0.080002 .  
## S.T.one                                               0.095296 .  
## H.P.s.notebook                                        0.998378    
## H.T.take                                              0.493524    
## A.npnct16.log                                         0.947117    
## S.npnct16.log                                               NA    
## H.T.time                                              0.667534    
## S.T.presid                                            0.760626    
## S.npnct08.log                                         0.144900    
## A.npnct08.log                                               NA    
## PubDate.last100.log                                   0.723858    
## .rnorm                                                0.175079    
## H.npnct05.log                                         0.998038    
## H.T.obama                                             0.630629    
## H.T.say                                               0.213346    
## H.T.bank                                              0.478124    
## `PubDate.date.fctr(7,13]`                             0.619094    
## `PubDate.date.fctr(13,19]`                            0.317992    
## `PubDate.date.fctr(19,25]`                            0.435728    
## `PubDate.date.fctr(25,31]`                            0.898757    
## `PubDate.second.fctr(14.8,29.5]`                      0.579679    
## `PubDate.second.fctr(29.5,44.2]`                      0.893283    
## `PubDate.second.fctr(44.2,59.1]`                      0.144683    
## H.npnct07.log                                         0.292633    
## S.npnct07.log                                         0.998219    
## S.npnct03.log                                         0.997322    
## A.npnct18.log                                         0.997503    
## H.npnct12.log                                         0.401470    
## H.T.word                                              0.017781 *  
## H.T.big                                               0.479618    
## S.P.year.colon                                        0.997703    
## S.npnct20.log                                         0.997334    
## S.T.obama                                             0.742631    
## H.npnct02.log                                         0.997080    
## H.T.test                                              0.987971    
## S.npnct14.log                                         0.635895    
## H.P.on.this.day                                       0.997774    
## S.P.first.draft                                       0.997093    
## A.T.take                                              0.448864    
## S.npnct06.log                                         0.848194    
## H.npnct13.log                                         0.417803    
## H.T.deal                                              0.993378    
## A.T.new                                               0.805328    
## S.P.metropolitan.diary.colon                          0.919384    
## H.T.billion                                           0.635251    
## H.T.polit                                             0.006487 ** 
## H.T.china                                             0.427701    
## H.P.verbatim.colon                                    0.996743    
## H.T.art                                               0.294355    
## `PubDate.minute.fctr(14.8,29.5]`                      0.535811    
## `PubDate.minute.fctr(29.5,44.2]`                      0.218616    
## `PubDate.minute.fctr(44.2,59.1]`                      0.827570    
## H.T.read                                              0.003921 ** 
## A.T.year                                              0.814927    
## S.npnct12.log                                         0.428770    
## A.T.will                                              0.149213    
## A.T.appear                                            0.606099    
## PubDate.wkday.fctr1                                   0.228387    
## PubDate.wkday.fctr2                                   0.046859 *  
## PubDate.wkday.fctr3                                   0.187527    
## PubDate.wkday.fctr4                                   0.102815    
## PubDate.wkday.fctr5                                   0.132300    
## PubDate.wkday.fctr6                                   0.021001 *  
## H.T.pictur                                            0.838529    
## A.T.senat                                             0.695080    
## S.T.day                                               0.470267    
## S.T.show                                              0.110462    
## H.P.today.in.smallbusiness                            0.995654    
## H.P.first.draft                                       0.994289    
## H.T.new                                               0.283220    
## S.npnct28.log                                         0.994397    
## H.P.daily.clip.report                                 0.994903    
## S.P.daily.clip.report                                       NA    
## H.T.news                                              0.264920    
## S.T.first                                             0.941097    
## H.T.X2014                                             0.571402    
## A.T.compani                                           0.335550    
## S.T.report                                            0.089723 .  
## A.T.word                                              0.506906    
## A.T.editor                                            0.261447    
## H.T.busi                                              0.533790    
## A.npnct13.log                                         0.000190 ***
## S.T.share                                             0.099792 .  
## A.T.time                                              0.456618    
## H.T.newyork                                           0.275134    
## A.T.newyork                                           0.004618 ** 
## H.T.day                                               0.528420    
## A.T.intern                                            0.372587    
## A.T.scene                                             0.985323    
## H.npnct14.log                                         0.990985    
## S.T.highlight                                         0.607151    
## S.npnct04.log                                         0.117764    
## S.npnct15.log                                         0.696632    
## S.T.tribun                                            0.986749    
## H.T.week                                              0.205470    
## S.P.fashion.week                                      0.998386    
## H.P.fashion.week                                      0.989733    
## H.npnct15.log                                         0.000623 ***
## S.T.fashion                                           0.983868    
## A.T.week                                              0.531242    
## H.npnct28.log                                         0.756475    
## S.npnct11.log                                         0.187571    
## H.ndgts.log                                           0.219418    
## S.ndgts.log                                           0.024787 *  
## H.nuppr.log                                           0.007487 ** 
## H.nchrs.log                                           1.05e-05 ***
## A.nchrs.log                                           0.574913    
## A.nwrds.unq.log                                       0.070603 .  
## S.nuppr.log                                           0.000481 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 1743.2  on 4324  degrees of freedom
## AIC: 2045.2
## 
## Number of Fisher Scoring iterations: 19
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
##                                                        id         cor.y
## WordCount.log                               WordCount.log  2.656836e-01
## myCategory.fctr                           myCategory.fctr  1.234541e-02
## H.P.readers.respond                   H.P.readers.respond  4.432886e-02
## A.npnct19.log                               A.npnct19.log  5.482747e-02
## H.nchrs.log                                   H.nchrs.log -1.710624e-01
## H.npnct19.log                               H.npnct19.log  1.283641e-01
## A.npnct13.log                               A.npnct13.log -4.999563e-02
## S.nuppr.log                                   S.nuppr.log -2.718459e-01
## H.npnct15.log                               H.npnct15.log -8.273237e-02
## H.npnct08.log                               H.npnct08.log  5.375262e-02
## H.T.read                                         H.T.read -3.467043e-02
## A.T.newyork                                   A.T.newyork -5.769443e-02
## H.T.polit                                       H.T.polit -3.062866e-02
## H.nuppr.log                                   H.nuppr.log -1.278085e-01
## H.T.word                                         H.T.word -1.382927e-02
## PubDate.wkday.fctr                     PubDate.wkday.fctr -3.980129e-02
## S.ndgts.log                                   S.ndgts.log -1.242046e-01
## H.P.recap.colon                           H.P.recap.colon  9.008096e-02
## H.P.no.comment.colon                 H.P.no.comment.colon  6.074669e-02
## H.npnct11.log                               H.npnct11.log  1.333613e-02
## S.T.make                                         S.T.make  3.924228e-02
## A.nwrds.unq.log                           A.nwrds.unq.log -2.261526e-01
## S.T.state                                       S.T.state  9.894250e-03
## S.T.report                                     S.T.report -4.746396e-02
## S.T.one                                           S.T.one  9.592876e-03
## S.T.share                                       S.T.share -5.076046e-02
## S.T.show                                         S.T.show -4.218975e-02
## PubDate.last10.log                     PubDate.last10.log  4.931702e-02
## S.npnct04.log                               S.npnct04.log -6.294642e-02
## PubDate.second.fctr                   PubDate.second.fctr -1.187946e-02
## S.npnct08.log                               S.npnct08.log -3.372706e-03
## A.nwrds.log                                   A.nwrds.log  1.216578e-01
## A.T.will                                         A.T.will -3.859405e-02
## .rnorm                                             .rnorm -8.244230e-03
## A.T.can                                           A.T.can  3.032288e-02
## PubDate.hour.fctr                       PubDate.hour.fctr  1.354368e-01
## S.npnct11.log                               S.npnct11.log -9.158156e-02
## S.T.said                                         S.T.said  1.801374e-02
## H.T.week                                         H.T.week -6.991953e-02
## H.T.say                                           H.T.say -9.960773e-03
## PubDate.minute.fctr                   PubDate.minute.fctr -3.407385e-02
## H.npnct16.log                               H.npnct16.log  3.039622e-02
## H.ndgts.log                                   H.ndgts.log -1.196633e-01
## H.npnct01.log                               H.npnct01.log  2.271577e-02
## A.T.editor                                     A.T.editor -4.875955e-02
## H.T.news                                         H.T.news -4.415284e-02
## H.T.newyork                                   H.T.newyork -5.717575e-02
## H.T.new                                           H.T.new -4.327803e-02
## H.npnct07.log                               H.npnct07.log -1.201741e-02
## H.T.art                                           H.T.art -3.280483e-02
## PubDate.date.fctr                       PubDate.date.fctr -1.164756e-02
## A.T.compani                                   A.T.compani -4.732205e-02
## H.T.get                                           H.T.get  3.312638e-02
## S.npnct01.log                               S.npnct01.log  3.093101e-02
## A.T.intern                                     A.T.intern -6.065381e-02
## H.npnct12.log                               H.npnct12.log -1.305305e-02
## PubDate.last1.log                       PubDate.last1.log  4.635751e-02
## H.npnct13.log                               H.npnct13.log -2.524770e-02
## H.T.china                                       H.T.china -3.144808e-02
## S.npnct12.log                               S.npnct12.log -3.638891e-02
## H.nwrds.log                                   H.nwrds.log  1.404401e-01
## A.T.take                                         A.T.take -2.287870e-02
## A.T.time                                         A.T.time -5.456053e-02
## S.T.day                                           S.T.day -4.209616e-02
## H.T.bank                                         H.T.bank -9.989139e-03
## H.T.big                                           H.T.big -1.390748e-02
## H.T.take                                         H.T.take -9.276608e-04
## A.T.word                                         A.T.word -4.818014e-02
## H.T.day                                           H.T.day -6.029849e-02
## PubDate.wkend                               PubDate.wkend  1.067288e-01
## A.T.week                                         A.T.week -8.570201e-02
## H.T.busi                                         H.T.busi -4.901905e-02
## H.T.X2014                                       H.T.X2014 -4.497745e-02
## A.nchrs.log                                   A.nchrs.log -2.245488e-01
## A.T.appear                                     A.T.appear -3.949035e-02
## S.T.highlight                               S.T.highlight -6.283750e-02
## H.T.obama                                       H.T.obama -9.907543e-03
## H.T.billion                                   H.T.billion -2.949817e-02
## S.npnct14.log                               S.npnct14.log -2.121844e-02
## H.T.time                                         H.T.time -2.527450e-03
## A.T.senat                                       A.T.senat -4.140312e-02
## S.npnct15.log                               S.npnct15.log -6.770952e-02
## PubDate.last100.log                   PubDate.last100.log -7.663322e-03
## S.T.obama                                       S.T.obama -1.933422e-02
## H.T.make                                         H.T.make  1.430572e-02
## H.npnct28.log                               H.npnct28.log -8.917338e-02
## S.T.presid                                     S.T.presid -2.581591e-03
## A.T.new                                           A.T.new -2.779223e-02
## A.T.year                                         A.T.year -3.497471e-02
## H.T.pictur                                     H.T.pictur -3.993172e-02
## S.npnct06.log                               S.npnct06.log -2.389145e-02
## S.P.metropolitan.diary.colon S.P.metropolitan.diary.colon -2.841404e-02
## S.T.first                                       S.T.first -4.494610e-02
## A.npnct16.log                               A.npnct16.log -1.587454e-03
## H.T.ebola                                       H.T.ebola  2.728582e-02
## S.T.fashion                                   S.T.fashion -8.344900e-02
## A.T.scene                                       A.T.scene -6.091747e-02
## S.T.tribun                                     S.T.tribun -6.880320e-02
## H.T.test                                         H.T.test -2.057181e-02
## H.P.fashion.week                         H.P.fashion.week -7.632046e-02
## H.npnct14.log                               H.npnct14.log -6.158577e-02
## H.T.deal                                         H.T.deal -2.559418e-02
## H.P.first.draft                           H.P.first.draft -4.316253e-02
## S.npnct28.log                               S.npnct28.log -4.370037e-02
## H.P.daily.clip.report               H.P.daily.clip.report -4.388279e-02
## H.P.today.in.smallbusiness     H.P.today.in.smallbusiness -4.243051e-02
## H.P.verbatim.colon                     H.P.verbatim.colon -3.194363e-02
## H.npnct02.log                               H.npnct02.log -2.001851e-02
## S.P.first.draft                           S.P.first.draft -2.150663e-02
## S.npnct03.log                               S.npnct03.log -1.240734e-02
## S.npnct20.log                               S.npnct20.log -1.923169e-02
## H.P.quandary                                 H.P.quandary  8.734922e-02
## A.npnct18.log                               A.npnct18.log -1.271661e-02
## S.P.year.colon                             S.P.year.colon -1.755336e-02
## H.P.on.this.day                           H.P.on.this.day -2.150663e-02
## H.npnct05.log                               H.npnct05.log -9.653967e-03
## S.npnct07.log                               S.npnct07.log -1.214357e-02
## H.P.s.notebook                             H.P.s.notebook  7.755542e-03
## S.P.fashion.week                         S.P.fashion.week -7.080716e-02
## A.ndgts.log                                   A.ndgts.log -1.249484e-01
## A.npnct01.log                               A.npnct01.log  3.093101e-02
## A.npnct02.log                               A.npnct02.log -1.451467e-02
## A.npnct03.log                               A.npnct03.log -1.359260e-02
## A.npnct04.log                               A.npnct04.log -6.294642e-02
## A.npnct05.log                               A.npnct05.log            NA
## A.npnct06.log                               A.npnct06.log -2.389145e-02
## A.npnct07.log                               A.npnct07.log -1.214357e-02
## A.npnct08.log                               A.npnct08.log -4.193476e-03
## A.npnct09.log                               A.npnct09.log            NA
## A.npnct10.log                               A.npnct10.log -5.547032e-03
## A.npnct11.log                               A.npnct11.log -9.183870e-02
## A.npnct12.log                               A.npnct12.log -3.760012e-02
## A.npnct14.log                               A.npnct14.log -2.407715e-02
## A.npnct15.log                               A.npnct15.log -6.893301e-02
## A.npnct17.log                               A.npnct17.log -1.457558e-02
## A.npnct20.log                               A.npnct20.log -1.923169e-02
## A.npnct21.log                               A.npnct21.log  1.537569e-02
## A.npnct22.log                               A.npnct22.log            NA
## A.npnct23.log                               A.npnct23.log  1.537569e-02
## A.npnct24.log                               A.npnct24.log -9.890046e-19
## A.npnct25.log                               A.npnct25.log -5.547032e-03
## A.npnct26.log                               A.npnct26.log            NA
## A.npnct27.log                               A.npnct27.log            NA
## A.npnct28.log                               A.npnct28.log -4.373349e-02
## A.npnct29.log                               A.npnct29.log            NA
## A.npnct30.log                               A.npnct30.log            NA
## A.nuppr.log                                   A.nuppr.log -2.720962e-01
## A.P.daily.clip.report               A.P.daily.clip.report -4.388279e-02
## A.P.fashion.week                         A.P.fashion.week -7.080716e-02
## A.P.first.draft                           A.P.first.draft -2.150663e-02
## A.P.http                                         A.P.http -1.294748e-02
## A.P.metropolitan.diary.colon A.P.metropolitan.diary.colon -2.841404e-02
## A.P.year.colon                             A.P.year.colon -1.755336e-02
## A.T.day                                           A.T.day -4.400791e-02
## A.T.fashion                                   A.T.fashion -8.349527e-02
## A.T.obama                                       A.T.obama -2.020436e-02
## A.T.one                                           A.T.one  1.048320e-02
## A.T.photo                                       A.T.photo -6.984501e-02
## A.T.report                                     A.T.report -4.847952e-02
## A.T.said                                         A.T.said  1.831840e-02
## A.T.state                                       A.T.state  1.007193e-02
## clusterid                                       clusterid            NA
## H.npnct03.log                               H.npnct03.log  9.533020e-03
## H.npnct04.log                               H.npnct04.log -5.126277e-02
## H.npnct06.log                               H.npnct06.log  3.190718e-02
## H.npnct09.log                               H.npnct09.log            NA
## H.npnct10.log                               H.npnct10.log -5.547032e-03
## H.npnct17.log                               H.npnct17.log            NA
## H.npnct18.log                               H.npnct18.log            NA
## H.npnct20.log                               H.npnct20.log -5.547032e-03
## H.npnct21.log                               H.npnct21.log            NA
## H.npnct22.log                               H.npnct22.log            NA
## H.npnct23.log                               H.npnct23.log            NA
## H.npnct24.log                               H.npnct24.log -9.890046e-19
## H.npnct25.log                               H.npnct25.log            NA
## H.npnct26.log                               H.npnct26.log            NA
## H.npnct27.log                               H.npnct27.log            NA
## H.npnct29.log                               H.npnct29.log            NA
## H.npnct30.log                               H.npnct30.log            NA
## H.nwrds.unq.log                           H.nwrds.unq.log -1.957553e-01
## H.P.http                                         H.P.http            NA
## H.P.today.in.politic                 H.P.today.in.politic -3.733661e-02
## H.P.what.we.are                           H.P.what.we.are -3.775209e-02
## H.P.year.colon                             H.P.year.colon -7.842875e-02
## H.T.daili                                       H.T.daili -5.852819e-02
## H.T.fashion                                   H.T.fashion -8.000421e-02
## H.T.first                                       H.T.first -4.458885e-02
## H.T.morn                                         H.T.morn -4.838380e-02
## H.T.report                                     H.T.report -5.934795e-02
## H.T.today                                       H.T.today -5.831308e-02
## H.T.X2015                                       H.T.X2015 -6.570743e-02
## Popular                                           Popular  1.000000e+00
## Popular.fctr                                 Popular.fctr            NA
## PubDate.last1                               PubDate.last1  3.592267e-02
## PubDate.last10                             PubDate.last10  5.398093e-02
## PubDate.last100                           PubDate.last100  3.989229e-02
## PubDate.month.fctr                     PubDate.month.fctr  1.914874e-02
## PubDate.POSIX                               PubDate.POSIX  1.568326e-02
## PubDate.year.fctr                       PubDate.year.fctr            NA
## PubDate.zoo                                   PubDate.zoo  1.568326e-02
## S.nchrs.log                                   S.nchrs.log -2.246930e-01
## S.npnct02.log                               S.npnct02.log -5.547032e-03
## S.npnct05.log                               S.npnct05.log            NA
## S.npnct09.log                               S.npnct09.log            NA
## S.npnct10.log                               S.npnct10.log -5.547032e-03
## S.npnct13.log                               S.npnct13.log -5.332519e-02
## S.npnct16.log                               S.npnct16.log -1.587454e-03
## S.npnct17.log                               S.npnct17.log            NA
## S.npnct18.log                               S.npnct18.log            NA
## S.npnct19.log                               S.npnct19.log  5.503894e-02
## S.npnct21.log                               S.npnct21.log  2.760321e-02
## S.npnct22.log                               S.npnct22.log            NA
## S.npnct23.log                               S.npnct23.log  2.760321e-02
## S.npnct24.log                               S.npnct24.log -9.890046e-19
## S.npnct25.log                               S.npnct25.log            NA
## S.npnct26.log                               S.npnct26.log            NA
## S.npnct27.log                               S.npnct27.log            NA
## S.npnct29.log                               S.npnct29.log            NA
## S.npnct30.log                               S.npnct30.log            NA
## S.nwrds.log                                   S.nwrds.log  1.262344e-01
## S.nwrds.unq.log                           S.nwrds.unq.log -2.343044e-01
## S.P.daily.clip.report               S.P.daily.clip.report -4.388279e-02
## S.P.http                                         S.P.http            NA
## S.T.articl                                     S.T.articl -5.446081e-02
## S.T.can                                           S.T.can  3.062115e-02
## S.T.compani                                   S.T.compani -4.739476e-02
## S.T.diari                                       S.T.diari -6.227998e-02
## S.T.intern                                     S.T.intern -6.932606e-02
## S.T.new                                           S.T.new -2.833575e-02
## S.T.newyork                                   S.T.newyork -5.943875e-02
## S.T.past                                         S.T.past -4.562700e-02
## S.T.photo                                       S.T.photo -4.269774e-02
## S.T.scene                                       S.T.scene -6.098895e-02
## S.T.take                                         S.T.take -2.307456e-02
## S.T.time                                         S.T.time -5.574436e-02
## S.T.week                                         S.T.week -8.627682e-02
## S.T.will                                         S.T.will -3.931556e-02
## S.T.word                                         S.T.word -4.876372e-02
## UniqueID                                         UniqueID  1.182492e-02
## WordCount                                       WordCount  2.575265e-01
##                              exclude.as.feat    cor.y.abs
## WordCount.log                          FALSE 2.656836e-01
## myCategory.fctr                        FALSE 1.234541e-02
## H.P.readers.respond                    FALSE 4.432886e-02
## A.npnct19.log                          FALSE 5.482747e-02
## H.nchrs.log                            FALSE 1.710624e-01
## H.npnct19.log                          FALSE 1.283641e-01
## A.npnct13.log                          FALSE 4.999563e-02
## S.nuppr.log                            FALSE 2.718459e-01
## H.npnct15.log                          FALSE 8.273237e-02
## H.npnct08.log                          FALSE 5.375262e-02
## H.T.read                               FALSE 3.467043e-02
## A.T.newyork                            FALSE 5.769443e-02
## H.T.polit                              FALSE 3.062866e-02
## H.nuppr.log                            FALSE 1.278085e-01
## H.T.word                               FALSE 1.382927e-02
## PubDate.wkday.fctr                     FALSE 3.980129e-02
## S.ndgts.log                            FALSE 1.242046e-01
## H.P.recap.colon                        FALSE 9.008096e-02
## H.P.no.comment.colon                   FALSE 6.074669e-02
## H.npnct11.log                          FALSE 1.333613e-02
## S.T.make                               FALSE 3.924228e-02
## A.nwrds.unq.log                        FALSE 2.261526e-01
## S.T.state                              FALSE 9.894250e-03
## S.T.report                             FALSE 4.746396e-02
## S.T.one                                FALSE 9.592876e-03
## S.T.share                              FALSE 5.076046e-02
## S.T.show                               FALSE 4.218975e-02
## PubDate.last10.log                     FALSE 4.931702e-02
## S.npnct04.log                          FALSE 6.294642e-02
## PubDate.second.fctr                    FALSE 1.187946e-02
## S.npnct08.log                          FALSE 3.372706e-03
## A.nwrds.log                            FALSE 1.216578e-01
## A.T.will                               FALSE 3.859405e-02
## .rnorm                                 FALSE 8.244230e-03
## A.T.can                                FALSE 3.032288e-02
## PubDate.hour.fctr                      FALSE 1.354368e-01
## S.npnct11.log                          FALSE 9.158156e-02
## S.T.said                               FALSE 1.801374e-02
## H.T.week                               FALSE 6.991953e-02
## H.T.say                                FALSE 9.960773e-03
## PubDate.minute.fctr                    FALSE 3.407385e-02
## H.npnct16.log                          FALSE 3.039622e-02
## H.ndgts.log                            FALSE 1.196633e-01
## H.npnct01.log                          FALSE 2.271577e-02
## A.T.editor                             FALSE 4.875955e-02
## H.T.news                               FALSE 4.415284e-02
## H.T.newyork                            FALSE 5.717575e-02
## H.T.new                                FALSE 4.327803e-02
## H.npnct07.log                          FALSE 1.201741e-02
## H.T.art                                FALSE 3.280483e-02
## PubDate.date.fctr                      FALSE 1.164756e-02
## A.T.compani                            FALSE 4.732205e-02
## H.T.get                                FALSE 3.312638e-02
## S.npnct01.log                          FALSE 3.093101e-02
## A.T.intern                             FALSE 6.065381e-02
## H.npnct12.log                          FALSE 1.305305e-02
## PubDate.last1.log                      FALSE 4.635751e-02
## H.npnct13.log                          FALSE 2.524770e-02
## H.T.china                              FALSE 3.144808e-02
## S.npnct12.log                          FALSE 3.638891e-02
## H.nwrds.log                            FALSE 1.404401e-01
## A.T.take                               FALSE 2.287870e-02
## A.T.time                               FALSE 5.456053e-02
## S.T.day                                FALSE 4.209616e-02
## H.T.bank                               FALSE 9.989139e-03
## H.T.big                                FALSE 1.390748e-02
## H.T.take                               FALSE 9.276608e-04
## A.T.word                               FALSE 4.818014e-02
## H.T.day                                FALSE 6.029849e-02
## PubDate.wkend                          FALSE 1.067288e-01
## A.T.week                               FALSE 8.570201e-02
## H.T.busi                               FALSE 4.901905e-02
## H.T.X2014                              FALSE 4.497745e-02
## A.nchrs.log                            FALSE 2.245488e-01
## A.T.appear                             FALSE 3.949035e-02
## S.T.highlight                          FALSE 6.283750e-02
## H.T.obama                              FALSE 9.907543e-03
## H.T.billion                            FALSE 2.949817e-02
## S.npnct14.log                          FALSE 2.121844e-02
## H.T.time                               FALSE 2.527450e-03
## A.T.senat                              FALSE 4.140312e-02
## S.npnct15.log                          FALSE 6.770952e-02
## PubDate.last100.log                    FALSE 7.663322e-03
## S.T.obama                              FALSE 1.933422e-02
## H.T.make                               FALSE 1.430572e-02
## H.npnct28.log                          FALSE 8.917338e-02
## S.T.presid                             FALSE 2.581591e-03
## A.T.new                                FALSE 2.779223e-02
## A.T.year                               FALSE 3.497471e-02
## H.T.pictur                             FALSE 3.993172e-02
## S.npnct06.log                          FALSE 2.389145e-02
## S.P.metropolitan.diary.colon           FALSE 2.841404e-02
## S.T.first                              FALSE 4.494610e-02
## A.npnct16.log                          FALSE 1.587454e-03
## H.T.ebola                              FALSE 2.728582e-02
## S.T.fashion                            FALSE 8.344900e-02
## A.T.scene                              FALSE 6.091747e-02
## S.T.tribun                             FALSE 6.880320e-02
## H.T.test                               FALSE 2.057181e-02
## H.P.fashion.week                       FALSE 7.632046e-02
## H.npnct14.log                          FALSE 6.158577e-02
## H.T.deal                               FALSE 2.559418e-02
## H.P.first.draft                        FALSE 4.316253e-02
## S.npnct28.log                          FALSE 4.370037e-02
## H.P.daily.clip.report                  FALSE 4.388279e-02
## H.P.today.in.smallbusiness             FALSE 4.243051e-02
## H.P.verbatim.colon                     FALSE 3.194363e-02
## H.npnct02.log                          FALSE 2.001851e-02
## S.P.first.draft                        FALSE 2.150663e-02
## S.npnct03.log                          FALSE 1.240734e-02
## S.npnct20.log                          FALSE 1.923169e-02
## H.P.quandary                           FALSE 8.734922e-02
## A.npnct18.log                          FALSE 1.271661e-02
## S.P.year.colon                         FALSE 1.755336e-02
## H.P.on.this.day                        FALSE 2.150663e-02
## H.npnct05.log                          FALSE 9.653967e-03
## S.npnct07.log                          FALSE 1.214357e-02
## H.P.s.notebook                         FALSE 7.755542e-03
## S.P.fashion.week                       FALSE 7.080716e-02
## A.ndgts.log                            FALSE 1.249484e-01
## A.npnct01.log                          FALSE 3.093101e-02
## A.npnct02.log                          FALSE 1.451467e-02
## A.npnct03.log                          FALSE 1.359260e-02
## A.npnct04.log                          FALSE 6.294642e-02
## A.npnct05.log                          FALSE           NA
## A.npnct06.log                          FALSE 2.389145e-02
## A.npnct07.log                          FALSE 1.214357e-02
## A.npnct08.log                          FALSE 4.193476e-03
## A.npnct09.log                          FALSE           NA
## A.npnct10.log                          FALSE 5.547032e-03
## A.npnct11.log                          FALSE 9.183870e-02
## A.npnct12.log                          FALSE 3.760012e-02
## A.npnct14.log                          FALSE 2.407715e-02
## A.npnct15.log                          FALSE 6.893301e-02
## A.npnct17.log                          FALSE 1.457558e-02
## A.npnct20.log                          FALSE 1.923169e-02
## A.npnct21.log                          FALSE 1.537569e-02
## A.npnct22.log                          FALSE           NA
## A.npnct23.log                          FALSE 1.537569e-02
## A.npnct24.log                          FALSE 9.890046e-19
## A.npnct25.log                          FALSE 5.547032e-03
## A.npnct26.log                          FALSE           NA
## A.npnct27.log                          FALSE           NA
## A.npnct28.log                          FALSE 4.373349e-02
## A.npnct29.log                          FALSE           NA
## A.npnct30.log                          FALSE           NA
## A.nuppr.log                            FALSE 2.720962e-01
## A.P.daily.clip.report                  FALSE 4.388279e-02
## A.P.fashion.week                       FALSE 7.080716e-02
## A.P.first.draft                        FALSE 2.150663e-02
## A.P.http                               FALSE 1.294748e-02
## A.P.metropolitan.diary.colon           FALSE 2.841404e-02
## A.P.year.colon                         FALSE 1.755336e-02
## A.T.day                                FALSE 4.400791e-02
## A.T.fashion                            FALSE 8.349527e-02
## A.T.obama                              FALSE 2.020436e-02
## A.T.one                                FALSE 1.048320e-02
## A.T.photo                              FALSE 6.984501e-02
## A.T.report                             FALSE 4.847952e-02
## A.T.said                               FALSE 1.831840e-02
## A.T.state                              FALSE 1.007193e-02
## clusterid                              FALSE           NA
## H.npnct03.log                          FALSE 9.533020e-03
## H.npnct04.log                          FALSE 5.126277e-02
## H.npnct06.log                          FALSE 3.190718e-02
## H.npnct09.log                          FALSE           NA
## H.npnct10.log                          FALSE 5.547032e-03
## H.npnct17.log                          FALSE           NA
## H.npnct18.log                          FALSE           NA
## H.npnct20.log                          FALSE 5.547032e-03
## H.npnct21.log                          FALSE           NA
## H.npnct22.log                          FALSE           NA
## H.npnct23.log                          FALSE           NA
## H.npnct24.log                          FALSE 9.890046e-19
## H.npnct25.log                          FALSE           NA
## H.npnct26.log                          FALSE           NA
## H.npnct27.log                          FALSE           NA
## H.npnct29.log                          FALSE           NA
## H.npnct30.log                          FALSE           NA
## H.nwrds.unq.log                        FALSE 1.957553e-01
## H.P.http                               FALSE           NA
## H.P.today.in.politic                   FALSE 3.733661e-02
## H.P.what.we.are                        FALSE 3.775209e-02
## H.P.year.colon                         FALSE 7.842875e-02
## H.T.daili                              FALSE 5.852819e-02
## H.T.fashion                            FALSE 8.000421e-02
## H.T.first                              FALSE 4.458885e-02
## H.T.morn                               FALSE 4.838380e-02
## H.T.report                             FALSE 5.934795e-02
## H.T.today                              FALSE 5.831308e-02
## H.T.X2015                              FALSE 6.570743e-02
## Popular                                 TRUE 1.000000e+00
## Popular.fctr                            TRUE           NA
## PubDate.last1                           TRUE 3.592267e-02
## PubDate.last10                          TRUE 5.398093e-02
## PubDate.last100                         TRUE 3.989229e-02
## PubDate.month.fctr                      TRUE 1.914874e-02
## PubDate.POSIX                           TRUE 1.568326e-02
## PubDate.year.fctr                      FALSE           NA
## PubDate.zoo                             TRUE 1.568326e-02
## S.nchrs.log                            FALSE 2.246930e-01
## S.npnct02.log                          FALSE 5.547032e-03
## S.npnct05.log                          FALSE           NA
## S.npnct09.log                          FALSE           NA
## S.npnct10.log                          FALSE 5.547032e-03
## S.npnct13.log                          FALSE 5.332519e-02
## S.npnct16.log                          FALSE 1.587454e-03
## S.npnct17.log                          FALSE           NA
## S.npnct18.log                          FALSE           NA
## S.npnct19.log                          FALSE 5.503894e-02
## S.npnct21.log                          FALSE 2.760321e-02
## S.npnct22.log                          FALSE           NA
## S.npnct23.log                          FALSE 2.760321e-02
## S.npnct24.log                          FALSE 9.890046e-19
## S.npnct25.log                          FALSE           NA
## S.npnct26.log                          FALSE           NA
## S.npnct27.log                          FALSE           NA
## S.npnct29.log                          FALSE           NA
## S.npnct30.log                          FALSE           NA
## S.nwrds.log                            FALSE 1.262344e-01
## S.nwrds.unq.log                        FALSE 2.343044e-01
## S.P.daily.clip.report                  FALSE 4.388279e-02
## S.P.http                               FALSE           NA
## S.T.articl                             FALSE 5.446081e-02
## S.T.can                                FALSE 3.062115e-02
## S.T.compani                            FALSE 4.739476e-02
## S.T.diari                              FALSE 6.227998e-02
## S.T.intern                             FALSE 6.932606e-02
## S.T.new                                FALSE 2.833575e-02
## S.T.newyork                            FALSE 5.943875e-02
## S.T.past                               FALSE 4.562700e-02
## S.T.photo                              FALSE 4.269774e-02
## S.T.scene                              FALSE 6.098895e-02
## S.T.take                               FALSE 2.307456e-02
## S.T.time                               FALSE 5.574436e-02
## S.T.week                               FALSE 8.627682e-02
## S.T.will                               FALSE 3.931556e-02
## S.T.word                               FALSE 4.876372e-02
## UniqueID                                TRUE 1.182492e-02
## WordCount                               TRUE 2.575265e-01
##                                                cor.high.X   freqRatio
## WordCount.log                                        <NA>    1.300000
## myCategory.fctr                                      <NA>    1.337185
## H.P.readers.respond                                  <NA>  342.789474
## A.npnct19.log                                        <NA>   12.798715
## H.nchrs.log                                          <NA>    1.023810
## H.npnct19.log                                        <NA>   14.995098
## A.npnct13.log                                        <NA>    4.603330
## S.nuppr.log                                          <NA>    1.152620
## H.npnct15.log                                        <NA>    3.914910
## H.npnct08.log                                        <NA>  111.620690
## H.T.read                                             <NA>  179.388889
## A.T.newyork                                          <NA>  103.762712
## H.T.polit                                            <NA>  126.254902
## H.nuppr.log                                          <NA>    1.033930
## H.T.word                                             <NA>  104.096774
## PubDate.wkday.fctr                                   <NA>    1.003268
## S.ndgts.log                                          <NA>   10.511247
## H.P.recap.colon                                      <NA>   93.666667
## H.P.no.comment.colon                                 <NA>  724.777778
## H.npnct11.log                                        <NA>    4.937442
## S.T.make                                             <NA>  273.782609
## A.nwrds.unq.log                                      <NA>    1.128405
## S.T.state                                            <NA>  315.750000
## S.T.report                                           <NA>   78.362500
## S.T.one                                              <NA>  222.892857
## S.T.share                                            <NA>  218.448276
## S.T.show                                             <NA>  274.608696
## PubDate.last10.log                                   <NA>    1.666667
## S.npnct04.log                                        <NA>   28.536364
## PubDate.second.fctr                                  <NA>    1.018204
## S.npnct08.log                                        <NA>  175.486486
## A.nwrds.log                                          <NA>    2.583333
## A.T.will                                             <NA>  112.547170
## .rnorm                                               <NA>    2.000000
## A.T.can                                              <NA>  241.538462
## PubDate.hour.fctr                                    <NA>    1.835040
## S.npnct11.log                                        <NA>    1.660473
## S.T.said                                             <NA>  174.388889
## H.T.week                                             <NA>   53.666667
## H.T.say                                              <NA>  247.461538
## PubDate.minute.fctr                                  <NA>    1.483365
## H.npnct16.log                                        <NA>   96.104478
## H.ndgts.log                                          <NA>   13.616137
## H.npnct01.log                                        <NA>  282.913043
## A.T.editor                                           <NA>  160.225000
## H.T.news                                             <NA>  238.518519
## H.T.newyork                                          <NA>   95.409091
## H.T.new                                              <NA>  116.333333
## H.npnct07.log                                        <NA>    5.437234
## H.T.art                                              <NA>  307.333333
## PubDate.date.fctr                                    <NA>    1.021394
## A.T.compani                                          <NA>  128.541667
## H.T.get                                              <NA>  430.866667
## S.npnct01.log                                        <NA>  309.952381
## A.T.intern                                           <NA>  157.950000
## H.npnct12.log                                        <NA>   13.126638
## PubDate.last1.log                                    <NA>    1.142857
## H.npnct13.log                                        <NA>   22.802326
## H.T.china                                            <NA>  238.555556
## S.npnct12.log                                        <NA>    5.706263
## H.nwrds.log                                          <NA>    1.127273
## A.T.take                                             <NA>  263.125000
## A.T.time                                             <NA>   68.011236
## S.T.day                                              <NA>   88.338028
## H.T.bank                                             <NA>  221.689655
## H.T.big                                              <NA>  403.562500
## H.T.take                                             <NA>  322.250000
## A.T.word                                             <NA>  133.145833
## H.T.day                                              <NA>   86.547945
## PubDate.wkend                                        <NA>    9.095827
## A.T.week                                             <NA>   47.273438
## H.T.busi                                             <NA>  229.428571
## H.T.X2014                                            <NA>  112.824561
## A.nchrs.log                                          <NA>    1.328571
## A.T.appear                                           <NA>  228.821429
## S.T.highlight                                        <NA>  187.500000
## H.T.obama                                            <NA>  229.750000
## H.T.billion                                          <NA>  229.892857
## S.npnct14.log                                        <NA>  203.062500
## H.T.time                                             <NA>  247.538462
## A.T.senat                                            <NA>  316.450000
## S.npnct15.log                                        <NA>   13.647191
## PubDate.last100.log                                  <NA>   25.000000
## S.T.obama                                            <NA>  335.684211
## H.T.make                                             <NA>  322.200000
## H.npnct28.log                                        <NA>   24.123077
## S.T.presid                                           <NA>  241.692308
## A.T.new                                              <NA>  104.052632
## A.T.year                                             <NA>  138.727273
## H.T.pictur                                           <NA>  104.032258
## S.npnct06.log                                        <NA>  115.642857
## S.P.metropolitan.diary.colon                         <NA>   99.492308
## S.T.first                                            <NA>  217.724138
## A.npnct16.log                                        <NA>  434.133333
## H.T.ebola                                            <NA>  293.000000
## S.T.fashion                                          <NA>   59.809524
## A.T.scene                                            <NA>   71.921348
## S.T.tribun                                           <NA>  138.456522
## H.T.test                                             <NA>  280.000000
## H.P.fashion.week                                     <NA>   34.500000
## H.npnct14.log                                        <NA>   52.983471
## H.T.deal                                             <NA>  258.080000
## H.P.first.draft                                      <NA>  107.866667
## S.npnct28.log                                        <NA>  134.791667
## H.P.daily.clip.report                                <NA>  104.354839
## H.P.today.in.smallbusiness                           <NA>  111.620690
## H.P.verbatim.colon                                   <NA>  196.939394
## H.npnct02.log                                        <NA>  501.461538
## S.P.first.draft                                      <NA>  434.466667
## S.npnct03.log                                        <NA> 1305.400000
## S.npnct20.log                                        <NA>  543.333333
## H.P.quandary                                         <NA>  652.200000
## A.npnct18.log                                        <NA> 1631.500000
## S.P.year.colon                                       <NA>  652.200000
## H.P.on.this.day                                      <NA>  434.466667
## H.npnct05.log                                        <NA>  543.333333
## S.npnct07.log                                        <NA> 1631.750000
## H.P.s.notebook                                       <NA>  815.500000
## S.P.fashion.week                                     <NA>   40.081761
## A.ndgts.log                                   S.ndgts.log   10.501022
## A.npnct01.log                               S.npnct01.log  309.952381
## A.npnct02.log                                    A.P.http 1087.500000
## A.npnct03.log                               S.npnct03.log 1087.666667
## A.npnct04.log                               S.npnct04.log   28.536364
## A.npnct05.log                                        <NA>    0.000000
## A.npnct06.log                               S.npnct06.log  115.642857
## A.npnct07.log                               S.npnct07.log 1631.750000
## A.npnct08.log                                        <NA>  170.842105
## A.npnct09.log                                        <NA>    0.000000
## A.npnct10.log                                        <NA> 6531.000000
## A.npnct11.log                               S.npnct11.log    1.660473
## A.npnct12.log                               S.npnct12.log    5.715368
## A.npnct14.log                               A.npnct17.log  196.696970
## A.npnct15.log                               S.npnct15.log   13.482222
## A.npnct17.log                               A.npnct02.log 1087.500000
## A.npnct20.log                               S.npnct20.log  543.333333
## A.npnct21.log                               A.npnct23.log 3264.500000
## A.npnct22.log                                        <NA>    0.000000
## A.npnct23.log                                        <NA> 3264.500000
## A.npnct24.log                                        <NA>    0.000000
## A.npnct25.log                                        <NA> 6531.000000
## A.npnct26.log                                        <NA>    0.000000
## A.npnct27.log                                        <NA>    0.000000
## A.npnct28.log                               S.npnct28.log  126.862745
## A.npnct29.log                                        <NA>    0.000000
## A.npnct30.log                                        <NA>    0.000000
## A.nuppr.log                                   S.nuppr.log    1.151308
## A.P.daily.clip.report               S.P.daily.clip.report  104.354839
## A.P.fashion.week                         S.P.fashion.week   40.081761
## A.P.first.draft                           S.P.first.draft  434.466667
## A.P.http                                    A.npnct18.log 1305.200000
## A.P.metropolitan.diary.colon S.P.metropolitan.diary.colon   99.492308
## A.P.year.colon                             S.P.year.colon  652.200000
## A.T.day                                           S.T.day   85.904110
## A.T.fashion                                     H.T.X2015   66.105263
## A.T.obama                                       S.T.obama  354.333333
## A.T.one                                           S.T.one  231.111111
## A.T.photo                                       S.T.photo   63.360000
## A.T.report                                     S.T.report   88.295775
## A.T.said                                         S.T.said  202.516129
## A.T.state                                       S.T.state  332.315789
## clusterid                                            <NA>    0.000000
## H.npnct03.log                                        <NA> 2176.333333
## H.npnct04.log                                 H.T.billion   38.325301
## H.npnct06.log                               H.npnct16.log   68.935484
## H.npnct09.log                                        <NA>    0.000000
## H.npnct10.log                                        <NA> 6531.000000
## H.npnct17.log                                        <NA>    0.000000
## H.npnct18.log                                        <NA>    0.000000
## H.npnct20.log                                        <NA> 6531.000000
## H.npnct21.log                                        <NA>    0.000000
## H.npnct22.log                                        <NA>    0.000000
## H.npnct23.log                                        <NA>    0.000000
## H.npnct24.log                                        <NA>    0.000000
## H.npnct25.log                                        <NA>    0.000000
## H.npnct26.log                                        <NA>    0.000000
## H.npnct27.log                                        <NA>    0.000000
## H.npnct29.log                                        <NA>    0.000000
## H.npnct30.log                                        <NA>    0.000000
## H.nwrds.unq.log                               H.nuppr.log    1.008878
## H.P.http                                             <NA>    0.000000
## H.P.today.in.politic                            H.T.polit  144.155556
## H.P.what.we.are                                  H.T.read  141.000000
## H.P.year.colon                                 S.T.intern   32.670103
## H.T.daili                           A.P.daily.clip.report  102.903226
## H.T.fashion                              H.P.fashion.week   71.681818
## H.T.first                                 H.P.first.draft  194.727273
## H.T.morn                                    A.npnct28.log  165.205128
## H.T.report                                      H.T.daili  102.000000
## H.T.today                            H.P.today.in.politic  138.239130
## H.T.X2015                                       S.T.diari  101.444444
## Popular                                              <NA>    4.976212
## Popular.fctr                                         <NA>          NA
## PubDate.last1                                        <NA>    1.142857
## PubDate.last10                                       <NA>    1.666667
## PubDate.last100                                      <NA>   25.000000
## PubDate.month.fctr                                   <NA>    1.017514
## PubDate.POSIX                                        <NA>    1.000000
## PubDate.year.fctr                                    <NA>    0.000000
## PubDate.zoo                                          <NA>    1.000000
## S.nchrs.log                                   A.nchrs.log    1.328571
## S.npnct02.log                                        <NA> 6531.000000
## S.npnct05.log                                        <NA>    0.000000
## S.npnct09.log                                        <NA>    0.000000
## S.npnct10.log                                        <NA> 6531.000000
## S.npnct13.log                               A.npnct13.log    4.672000
## S.npnct16.log                                        <NA>  434.133333
## S.npnct17.log                                        <NA>    0.000000
## S.npnct18.log                                        <NA>    0.000000
## S.npnct19.log                               A.npnct19.log   12.862366
## S.npnct21.log                               A.npnct21.log 6531.000000
## S.npnct22.log                                        <NA>    0.000000
## S.npnct23.log                                        <NA> 6531.000000
## S.npnct24.log                                        <NA>    0.000000
## S.npnct25.log                                        <NA>    0.000000
## S.npnct26.log                                        <NA>    0.000000
## S.npnct27.log                                        <NA>    0.000000
## S.npnct29.log                                        <NA>    0.000000
## S.npnct30.log                                        <NA>    0.000000
## S.nwrds.log                                   A.nwrds.log    2.583333
## S.nwrds.unq.log                               S.nchrs.log    1.092486
## S.P.daily.clip.report                                <NA>  104.354839
## S.P.http                                             <NA>    0.000000
## S.T.articl                                       S.T.past   85.500000
## S.T.can                                           A.T.can  261.666667
## S.T.compani                                   A.T.compani  140.227273
## S.T.diari                                       S.T.scene   64.959184
## S.T.intern                                     S.T.tribun  131.625000
## S.T.new                                           A.T.new  100.559322
## S.T.newyork                                   A.T.newyork  145.761905
## S.T.past                                       A.T.appear  229.285714
## S.T.photo                                      H.T.pictur  248.038462
## S.T.scene                                       A.T.scene   65.316327
## S.T.take                                         A.T.take  263.166667
## S.T.time                                         A.T.time   65.804348
## S.T.week                                         A.T.week   48.408000
## S.T.will                                         A.T.will  112.584906
## S.T.word                                         A.T.word  133.145833
## UniqueID                                             <NA>    1.000000
## WordCount                                            <NA>    2.315789
##                              percentUnique zeroVar   nzv myNearZV
## WordCount.log                  24.14268218   FALSE FALSE    FALSE
## myCategory.fctr                 0.30618494   FALSE FALSE    FALSE
## H.P.readers.respond             0.03061849   FALSE  TRUE    FALSE
## A.npnct19.log                   0.07654623   FALSE FALSE    FALSE
## H.nchrs.log                     1.57685242   FALSE FALSE    FALSE
## H.npnct19.log                   0.06123699   FALSE FALSE    FALSE
## A.npnct13.log                   0.16840171   FALSE FALSE    FALSE
## S.nuppr.log                     0.33680343   FALSE FALSE    FALSE
## H.npnct15.log                   0.04592774   FALSE FALSE    FALSE
## H.npnct08.log                   0.03061849   FALSE  TRUE    FALSE
## H.T.read                        0.16840171   FALSE  TRUE    FALSE
## A.T.newyork                     0.42865891   FALSE  TRUE    FALSE
## H.T.polit                       0.13778322   FALSE  TRUE    FALSE
## H.nuppr.log                     0.29087569   FALSE FALSE    FALSE
## H.T.word                        0.13778322   FALSE  TRUE    FALSE
## PubDate.wkday.fctr              0.10716473   FALSE FALSE    FALSE
## S.ndgts.log                     0.26025720   FALSE FALSE    FALSE
## H.P.recap.colon                 0.03061849   FALSE  TRUE    FALSE
## H.P.no.comment.colon            0.03061849   FALSE  TRUE    FALSE
## H.npnct11.log                   0.07654623   FALSE FALSE    FALSE
## S.T.make                        0.41334966   FALSE  TRUE    FALSE
## A.nwrds.unq.log                 0.55113288   FALSE FALSE    FALSE
## S.T.state                       0.39804042   FALSE  TRUE    FALSE
## S.T.report                      0.35211268   FALSE  TRUE    FALSE
## S.T.one                         0.47458665   FALSE  TRUE    FALSE
## S.T.share                       0.36742192   FALSE  TRUE    FALSE
## S.T.show                        0.39804042   FALSE  TRUE    FALSE
## PubDate.last10.log             79.05695040   FALSE FALSE    FALSE
## S.npnct04.log                   0.07654623   FALSE  TRUE    FALSE
## PubDate.second.fctr             0.06123699   FALSE FALSE    FALSE
## S.npnct08.log                   0.06123699   FALSE  TRUE    FALSE
## A.nwrds.log                    93.50887936   FALSE FALSE    FALSE
## A.T.will                        0.62767912   FALSE  TRUE    FALSE
## .rnorm                         99.98469075   FALSE FALSE    FALSE
## A.T.can                         0.48989590   FALSE  TRUE    FALSE
## PubDate.hour.fctr               0.04592774   FALSE FALSE    FALSE
## S.npnct11.log                   0.13778322   FALSE FALSE    FALSE
## S.T.said                        0.36742192   FALSE  TRUE    FALSE
## H.T.week                        0.16840171   FALSE  TRUE    FALSE
## H.T.say                         0.16840171   FALSE  TRUE    FALSE
## PubDate.minute.fctr             0.06123699   FALSE FALSE    FALSE
## H.npnct16.log                   0.06123699   FALSE  TRUE    FALSE
## H.ndgts.log                     0.18371096   FALSE FALSE    FALSE
## H.npnct01.log                   0.04592774   FALSE  TRUE    FALSE
## A.T.editor                      0.29087569   FALSE  TRUE    FALSE
## H.T.news                        0.16840171   FALSE  TRUE    FALSE
## H.T.newyork                     0.15309247   FALSE  TRUE    FALSE
## H.T.new                         0.19902021   FALSE  TRUE    FALSE
## H.npnct07.log                   0.12247397   FALSE FALSE    FALSE
## H.T.art                         0.19902021   FALSE  TRUE    FALSE
## PubDate.date.fctr               0.07654623   FALSE FALSE    FALSE
## A.T.compani                     0.45927740   FALSE  TRUE    FALSE
## H.T.get                         0.18371096   FALSE  TRUE    FALSE
## S.npnct01.log                   0.06123699   FALSE  TRUE    FALSE
## A.T.intern                      0.35211268   FALSE  TRUE    FALSE
## H.npnct12.log                   0.09185548   FALSE FALSE    FALSE
## PubDate.last1.log              36.49724434   FALSE FALSE    FALSE
## H.npnct13.log                   0.12247397   FALSE  TRUE    FALSE
## H.T.china                       0.18371096   FALSE  TRUE    FALSE
## S.npnct12.log                   0.09185548   FALSE FALSE    FALSE
## H.nwrds.log                    84.15492958   FALSE FALSE    FALSE
## A.T.take                        0.42865891   FALSE  TRUE    FALSE
## A.T.time                        0.42865891   FALSE  TRUE    FALSE
## S.T.day                         0.39804042   FALSE  TRUE    FALSE
## H.T.bank                        0.13778322   FALSE  TRUE    FALSE
## H.T.big                         0.19902021   FALSE  TRUE    FALSE
## H.T.take                        0.15309247   FALSE  TRUE    FALSE
## A.T.word                        0.32149418   FALSE  TRUE    FALSE
## H.T.day                         0.18371096   FALSE  TRUE    FALSE
## PubDate.wkend                   0.03061849   FALSE FALSE    FALSE
## A.T.week                        0.50520514   FALSE  TRUE    FALSE
## H.T.busi                        0.18371096   FALSE  TRUE    FALSE
## H.T.X2014                       0.13778322   FALSE  TRUE    FALSE
## A.nchrs.log                     4.39375383   FALSE FALSE    FALSE
## A.T.appear                      0.30618494   FALSE  TRUE    FALSE
## S.T.highlight                   0.27556644   FALSE  TRUE    FALSE
## H.T.obama                       0.16840171   FALSE  TRUE    FALSE
## H.T.billion                     0.13778322   FALSE  TRUE    FALSE
## S.npnct14.log                   0.04592774   FALSE  TRUE    FALSE
## H.T.time                        0.16840171   FALSE  TRUE    FALSE
## A.T.senat                       0.52051439   FALSE  TRUE    FALSE
## S.npnct15.log                   0.04592774   FALSE FALSE    FALSE
## PubDate.last100.log            92.19228414   FALSE FALSE    FALSE
## S.T.obama                       0.36742192   FALSE  TRUE    FALSE
## H.T.make                        0.13778322   FALSE  TRUE    FALSE
## H.npnct28.log                   0.03061849   FALSE  TRUE    FALSE
## S.T.presid                      0.41334966   FALSE  TRUE    FALSE
## A.T.new                         0.55113288   FALSE  TRUE    FALSE
## A.T.year                        0.48989590   FALSE  TRUE    FALSE
## H.T.pictur                      0.10716473   FALSE  TRUE    FALSE
## S.npnct06.log                   0.03061849   FALSE  TRUE    FALSE
## S.P.metropolitan.diary.colon    0.03061849   FALSE  TRUE    FALSE
## S.T.first                       0.39804042   FALSE  TRUE    FALSE
## A.npnct16.log                   0.04592774   FALSE  TRUE    FALSE
## H.T.ebola                       0.16840171   FALSE  TRUE    FALSE
## S.T.fashion                     0.41334966   FALSE  TRUE    FALSE
## A.T.scene                       0.26025720   FALSE  TRUE    FALSE
## S.T.tribun                      0.21432945   FALSE  TRUE    FALSE
## H.T.test                        0.13778322   FALSE  TRUE    FALSE
## H.P.fashion.week                0.03061849   FALSE  TRUE    FALSE
## H.npnct14.log                   0.03061849   FALSE  TRUE    FALSE
## H.T.deal                        0.13778322   FALSE  TRUE    FALSE
## H.P.first.draft                 0.03061849   FALSE  TRUE    FALSE
## S.npnct28.log                   0.04592774   FALSE  TRUE    FALSE
## H.P.daily.clip.report           0.03061849   FALSE  TRUE    FALSE
## H.P.today.in.smallbusiness      0.03061849   FALSE  TRUE    FALSE
## H.P.verbatim.colon              0.03061849   FALSE  TRUE    FALSE
## H.npnct02.log                   0.03061849   FALSE  TRUE    FALSE
## S.P.first.draft                 0.03061849   FALSE  TRUE    FALSE
## S.npnct03.log                   0.03061849   FALSE  TRUE    FALSE
## S.npnct20.log                   0.03061849   FALSE  TRUE    FALSE
## H.P.quandary                    0.03061849   FALSE  TRUE    FALSE
## A.npnct18.log                   0.06123699   FALSE  TRUE    FALSE
## S.P.year.colon                  0.03061849   FALSE  TRUE    FALSE
## H.P.on.this.day                 0.03061849   FALSE  TRUE    FALSE
## H.npnct05.log                   0.03061849   FALSE  TRUE    FALSE
## S.npnct07.log                   0.04592774   FALSE  TRUE    FALSE
## H.P.s.notebook                  0.03061849   FALSE  TRUE    FALSE
## S.P.fashion.week                0.03061849   FALSE  TRUE    FALSE
## A.ndgts.log                     0.29087569   FALSE FALSE    FALSE
## A.npnct01.log                   0.06123699   FALSE  TRUE    FALSE
## A.npnct02.log                   0.04592774   FALSE  TRUE    FALSE
## A.npnct03.log                   0.03061849   FALSE  TRUE    FALSE
## A.npnct04.log                   0.07654623   FALSE  TRUE    FALSE
## A.npnct05.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct06.log                   0.03061849   FALSE  TRUE    FALSE
## A.npnct07.log                   0.04592774   FALSE  TRUE    FALSE
## A.npnct08.log                   0.06123699   FALSE  TRUE    FALSE
## A.npnct09.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct10.log                   0.03061849   FALSE  TRUE     TRUE
## A.npnct11.log                   0.13778322   FALSE FALSE    FALSE
## A.npnct12.log                   0.12247397   FALSE FALSE    FALSE
## A.npnct14.log                   0.10716473   FALSE  TRUE    FALSE
## A.npnct15.log                   0.04592774   FALSE FALSE    FALSE
## A.npnct17.log                   0.04592774   FALSE  TRUE    FALSE
## A.npnct20.log                   0.03061849   FALSE  TRUE    FALSE
## A.npnct21.log                   0.04592774   FALSE  TRUE     TRUE
## A.npnct22.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct23.log                   0.04592774   FALSE  TRUE     TRUE
## A.npnct24.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct25.log                   0.03061849   FALSE  TRUE     TRUE
## A.npnct26.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct27.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct28.log                   0.04592774   FALSE  TRUE    FALSE
## A.npnct29.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct30.log                   0.01530925    TRUE  TRUE     TRUE
## A.nuppr.log                     0.33680343   FALSE FALSE    FALSE
## A.P.daily.clip.report           0.03061849   FALSE  TRUE    FALSE
## A.P.fashion.week                0.03061849   FALSE  TRUE    FALSE
## A.P.first.draft                 0.03061849   FALSE  TRUE    FALSE
## A.P.http                        0.04592774   FALSE  TRUE    FALSE
## A.P.metropolitan.diary.colon    0.03061849   FALSE  TRUE    FALSE
## A.P.year.colon                  0.03061849   FALSE  TRUE    FALSE
## A.T.day                         0.42865891   FALSE  TRUE    FALSE
## A.T.fashion                     0.41334966   FALSE  TRUE    FALSE
## A.T.obama                       0.36742192   FALSE  TRUE    FALSE
## A.T.one                         0.52051439   FALSE  TRUE    FALSE
## A.T.photo                       0.29087569   FALSE  TRUE    FALSE
## A.T.report                      0.36742192   FALSE  TRUE    FALSE
## A.T.said                        0.39804042   FALSE  TRUE    FALSE
## A.T.state                       0.44396816   FALSE  TRUE    FALSE
## clusterid                       0.01530925    TRUE  TRUE     TRUE
## H.npnct03.log                   0.03061849   FALSE  TRUE     TRUE
## H.npnct04.log                   0.04592774   FALSE  TRUE    FALSE
## H.npnct06.log                   0.06123699   FALSE  TRUE    FALSE
## H.npnct09.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct10.log                   0.03061849   FALSE  TRUE     TRUE
## H.npnct17.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct18.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct20.log                   0.03061849   FALSE  TRUE     TRUE
## H.npnct21.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct22.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct23.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct24.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct25.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct26.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct27.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct29.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct30.log                   0.01530925    TRUE  TRUE     TRUE
## H.nwrds.unq.log                 0.21432945   FALSE FALSE    FALSE
## H.P.http                        0.01530925    TRUE  TRUE     TRUE
## H.P.today.in.politic            0.03061849   FALSE  TRUE    FALSE
## H.P.what.we.are                 0.03061849   FALSE  TRUE    FALSE
## H.P.year.colon                  0.03061849   FALSE  TRUE    FALSE
## H.T.daili                       0.16840171   FALSE  TRUE    FALSE
## H.T.fashion                     0.19902021   FALSE  TRUE    FALSE
## H.T.first                       0.15309247   FALSE  TRUE    FALSE
## H.T.morn                        0.07654623   FALSE  TRUE    FALSE
## H.T.report                      0.16840171   FALSE  TRUE    FALSE
## H.T.today                       0.13778322   FALSE  TRUE    FALSE
## H.T.X2015                       0.12247397   FALSE  TRUE    FALSE
## Popular                         0.03061849   FALSE FALSE    FALSE
## Popular.fctr                            NA      NA    NA       NA
## PubDate.last1                  36.49724434   FALSE FALSE    FALSE
## PubDate.last10                 79.05695040   FALSE FALSE    FALSE
## PubDate.last100                92.52908757   FALSE FALSE    FALSE
## PubDate.month.fctr              0.04592774   FALSE FALSE    FALSE
## PubDate.POSIX                  99.86221678   FALSE FALSE    FALSE
## PubDate.year.fctr               0.01530925    TRUE  TRUE     TRUE
## PubDate.zoo                    99.86221678   FALSE FALSE    FALSE
## S.nchrs.log                     3.72014697   FALSE FALSE    FALSE
## S.npnct02.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct05.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct09.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct10.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct13.log                   0.16840171   FALSE FALSE    FALSE
## S.npnct16.log                   0.04592774   FALSE  TRUE    FALSE
## S.npnct17.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct18.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct19.log                   0.07654623   FALSE FALSE    FALSE
## S.npnct21.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct22.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct23.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct24.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct25.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct26.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct27.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct29.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct30.log                   0.01530925    TRUE  TRUE     TRUE
## S.nwrds.log                    93.50887936   FALSE FALSE    FALSE
## S.nwrds.unq.log                 0.44396816   FALSE FALSE    FALSE
## S.P.daily.clip.report           0.03061849   FALSE  TRUE    FALSE
## S.P.http                        0.01530925    TRUE  TRUE     TRUE
## S.T.articl                      0.32149418   FALSE  TRUE    FALSE
## S.T.can                         0.41334966   FALSE  TRUE    FALSE
## S.T.compani                     0.44396816   FALSE  TRUE    FALSE
## S.T.diari                       0.18371096   FALSE  TRUE    FALSE
## S.T.intern                      0.32149418   FALSE  TRUE    FALSE
## S.T.new                         0.48989590   FALSE  TRUE    FALSE
## S.T.newyork                     0.41334966   FALSE  TRUE    FALSE
## S.T.past                        0.29087569   FALSE  TRUE    FALSE
## S.T.photo                       0.27556644   FALSE  TRUE    FALSE
## S.T.scene                       0.26025720   FALSE  TRUE    FALSE
## S.T.take                        0.38273117   FALSE  TRUE    FALSE
## S.T.time                        0.44396816   FALSE  TRUE    FALSE
## S.T.week                        0.44396816   FALSE  TRUE    FALSE
## S.T.will                        0.55113288   FALSE  TRUE    FALSE
## S.T.word                        0.30618494   FALSE  TRUE    FALSE
## UniqueID                      100.00000000   FALSE FALSE    FALSE
## WordCount                      24.15799143   FALSE FALSE    FALSE
##                              is.cor.y.abs.low rsp_var_raw id_var rsp_var
## WordCount.log                           FALSE       FALSE     NA      NA
## myCategory.fctr                         FALSE       FALSE     NA      NA
## H.P.readers.respond                     FALSE       FALSE     NA      NA
## A.npnct19.log                           FALSE       FALSE     NA      NA
## H.nchrs.log                             FALSE       FALSE     NA      NA
## H.npnct19.log                           FALSE       FALSE     NA      NA
## A.npnct13.log                           FALSE       FALSE     NA      NA
## S.nuppr.log                             FALSE       FALSE     NA      NA
## H.npnct15.log                           FALSE       FALSE     NA      NA
## H.npnct08.log                           FALSE       FALSE     NA      NA
## H.T.read                                FALSE       FALSE     NA      NA
## A.T.newyork                             FALSE       FALSE     NA      NA
## H.T.polit                               FALSE       FALSE     NA      NA
## H.nuppr.log                             FALSE       FALSE     NA      NA
## H.T.word                                FALSE       FALSE     NA      NA
## PubDate.wkday.fctr                      FALSE       FALSE     NA      NA
## S.ndgts.log                             FALSE       FALSE     NA      NA
## H.P.recap.colon                         FALSE       FALSE     NA      NA
## H.P.no.comment.colon                    FALSE       FALSE     NA      NA
## H.npnct11.log                           FALSE       FALSE     NA      NA
## S.T.make                                FALSE       FALSE     NA      NA
## A.nwrds.unq.log                         FALSE       FALSE     NA      NA
## S.T.state                               FALSE       FALSE     NA      NA
## S.T.report                              FALSE       FALSE     NA      NA
## S.T.one                                 FALSE       FALSE     NA      NA
## S.T.share                               FALSE       FALSE     NA      NA
## S.T.show                                FALSE       FALSE     NA      NA
## PubDate.last10.log                      FALSE       FALSE     NA      NA
## S.npnct04.log                           FALSE       FALSE     NA      NA
## PubDate.second.fctr                     FALSE       FALSE     NA      NA
## S.npnct08.log                            TRUE       FALSE     NA      NA
## A.nwrds.log                             FALSE       FALSE     NA      NA
## A.T.will                                FALSE       FALSE     NA      NA
## .rnorm                                  FALSE       FALSE     NA      NA
## A.T.can                                 FALSE       FALSE     NA      NA
## PubDate.hour.fctr                       FALSE       FALSE     NA      NA
## S.npnct11.log                           FALSE       FALSE     NA      NA
## S.T.said                                FALSE       FALSE     NA      NA
## H.T.week                                FALSE       FALSE     NA      NA
## H.T.say                                 FALSE       FALSE     NA      NA
## PubDate.minute.fctr                     FALSE       FALSE     NA      NA
## H.npnct16.log                           FALSE       FALSE     NA      NA
## H.ndgts.log                             FALSE       FALSE     NA      NA
## H.npnct01.log                           FALSE       FALSE     NA      NA
## A.T.editor                              FALSE       FALSE     NA      NA
## H.T.news                                FALSE       FALSE     NA      NA
## H.T.newyork                             FALSE       FALSE     NA      NA
## H.T.new                                 FALSE       FALSE     NA      NA
## H.npnct07.log                           FALSE       FALSE     NA      NA
## H.T.art                                 FALSE       FALSE     NA      NA
## PubDate.date.fctr                       FALSE       FALSE     NA      NA
## A.T.compani                             FALSE       FALSE     NA      NA
## H.T.get                                 FALSE       FALSE     NA      NA
## S.npnct01.log                           FALSE       FALSE     NA      NA
## A.T.intern                              FALSE       FALSE     NA      NA
## H.npnct12.log                           FALSE       FALSE     NA      NA
## PubDate.last1.log                       FALSE       FALSE     NA      NA
## H.npnct13.log                           FALSE       FALSE     NA      NA
## H.T.china                               FALSE       FALSE     NA      NA
## S.npnct12.log                           FALSE       FALSE     NA      NA
## H.nwrds.log                             FALSE       FALSE     NA      NA
## A.T.take                                FALSE       FALSE     NA      NA
## A.T.time                                FALSE       FALSE     NA      NA
## S.T.day                                 FALSE       FALSE     NA      NA
## H.T.bank                                FALSE       FALSE     NA      NA
## H.T.big                                 FALSE       FALSE     NA      NA
## H.T.take                                 TRUE       FALSE     NA      NA
## A.T.word                                FALSE       FALSE     NA      NA
## H.T.day                                 FALSE       FALSE     NA      NA
## PubDate.wkend                           FALSE       FALSE     NA      NA
## A.T.week                                FALSE       FALSE     NA      NA
## H.T.busi                                FALSE       FALSE     NA      NA
## H.T.X2014                               FALSE       FALSE     NA      NA
## A.nchrs.log                             FALSE       FALSE     NA      NA
## A.T.appear                              FALSE       FALSE     NA      NA
## S.T.highlight                           FALSE       FALSE     NA      NA
## H.T.obama                               FALSE       FALSE     NA      NA
## H.T.billion                             FALSE       FALSE     NA      NA
## S.npnct14.log                           FALSE       FALSE     NA      NA
## H.T.time                                 TRUE       FALSE     NA      NA
## A.T.senat                               FALSE       FALSE     NA      NA
## S.npnct15.log                           FALSE       FALSE     NA      NA
## PubDate.last100.log                      TRUE       FALSE     NA      NA
## S.T.obama                               FALSE       FALSE     NA      NA
## H.T.make                                FALSE       FALSE     NA      NA
## H.npnct28.log                           FALSE       FALSE     NA      NA
## S.T.presid                               TRUE       FALSE     NA      NA
## A.T.new                                 FALSE       FALSE     NA      NA
## A.T.year                                FALSE       FALSE     NA      NA
## H.T.pictur                              FALSE       FALSE     NA      NA
## S.npnct06.log                           FALSE       FALSE     NA      NA
## S.P.metropolitan.diary.colon            FALSE       FALSE     NA      NA
## S.T.first                               FALSE       FALSE     NA      NA
## A.npnct16.log                            TRUE       FALSE     NA      NA
## H.T.ebola                               FALSE       FALSE     NA      NA
## S.T.fashion                             FALSE       FALSE     NA      NA
## A.T.scene                               FALSE       FALSE     NA      NA
## S.T.tribun                              FALSE       FALSE     NA      NA
## H.T.test                                FALSE       FALSE     NA      NA
## H.P.fashion.week                        FALSE       FALSE     NA      NA
## H.npnct14.log                           FALSE       FALSE     NA      NA
## H.T.deal                                FALSE       FALSE     NA      NA
## H.P.first.draft                         FALSE       FALSE     NA      NA
## S.npnct28.log                           FALSE       FALSE     NA      NA
## H.P.daily.clip.report                   FALSE       FALSE     NA      NA
## H.P.today.in.smallbusiness              FALSE       FALSE     NA      NA
## H.P.verbatim.colon                      FALSE       FALSE     NA      NA
## H.npnct02.log                           FALSE       FALSE     NA      NA
## S.P.first.draft                         FALSE       FALSE     NA      NA
## S.npnct03.log                           FALSE       FALSE     NA      NA
## S.npnct20.log                           FALSE       FALSE     NA      NA
## H.P.quandary                            FALSE       FALSE     NA      NA
## A.npnct18.log                           FALSE       FALSE     NA      NA
## S.P.year.colon                          FALSE       FALSE     NA      NA
## H.P.on.this.day                         FALSE       FALSE     NA      NA
## H.npnct05.log                           FALSE       FALSE     NA      NA
## S.npnct07.log                           FALSE       FALSE     NA      NA
## H.P.s.notebook                           TRUE       FALSE     NA      NA
## S.P.fashion.week                        FALSE       FALSE     NA      NA
## A.ndgts.log                             FALSE       FALSE     NA      NA
## A.npnct01.log                           FALSE       FALSE     NA      NA
## A.npnct02.log                           FALSE       FALSE     NA      NA
## A.npnct03.log                           FALSE       FALSE     NA      NA
## A.npnct04.log                           FALSE       FALSE     NA      NA
## A.npnct05.log                              NA       FALSE     NA      NA
## A.npnct06.log                           FALSE       FALSE     NA      NA
## A.npnct07.log                           FALSE       FALSE     NA      NA
## A.npnct08.log                            TRUE       FALSE     NA      NA
## A.npnct09.log                              NA       FALSE     NA      NA
## A.npnct10.log                            TRUE       FALSE     NA      NA
## A.npnct11.log                           FALSE       FALSE     NA      NA
## A.npnct12.log                           FALSE       FALSE     NA      NA
## A.npnct14.log                           FALSE       FALSE     NA      NA
## A.npnct15.log                           FALSE       FALSE     NA      NA
## A.npnct17.log                           FALSE       FALSE     NA      NA
## A.npnct20.log                           FALSE       FALSE     NA      NA
## A.npnct21.log                           FALSE       FALSE     NA      NA
## A.npnct22.log                              NA       FALSE     NA      NA
## A.npnct23.log                           FALSE       FALSE     NA      NA
## A.npnct24.log                            TRUE       FALSE     NA      NA
## A.npnct25.log                            TRUE       FALSE     NA      NA
## A.npnct26.log                              NA       FALSE     NA      NA
## A.npnct27.log                              NA       FALSE     NA      NA
## A.npnct28.log                           FALSE       FALSE     NA      NA
## A.npnct29.log                              NA       FALSE     NA      NA
## A.npnct30.log                              NA       FALSE     NA      NA
## A.nuppr.log                             FALSE       FALSE     NA      NA
## A.P.daily.clip.report                   FALSE       FALSE     NA      NA
## A.P.fashion.week                        FALSE       FALSE     NA      NA
## A.P.first.draft                         FALSE       FALSE     NA      NA
## A.P.http                                FALSE       FALSE     NA      NA
## A.P.metropolitan.diary.colon            FALSE       FALSE     NA      NA
## A.P.year.colon                          FALSE       FALSE     NA      NA
## A.T.day                                 FALSE       FALSE     NA      NA
## A.T.fashion                             FALSE       FALSE     NA      NA
## A.T.obama                               FALSE       FALSE     NA      NA
## A.T.one                                 FALSE       FALSE     NA      NA
## A.T.photo                               FALSE       FALSE     NA      NA
## A.T.report                              FALSE       FALSE     NA      NA
## A.T.said                                FALSE       FALSE     NA      NA
## A.T.state                               FALSE       FALSE     NA      NA
## clusterid                                  NA       FALSE     NA      NA
## H.npnct03.log                           FALSE       FALSE     NA      NA
## H.npnct04.log                           FALSE       FALSE     NA      NA
## H.npnct06.log                           FALSE       FALSE     NA      NA
## H.npnct09.log                              NA       FALSE     NA      NA
## H.npnct10.log                            TRUE       FALSE     NA      NA
## H.npnct17.log                              NA       FALSE     NA      NA
## H.npnct18.log                              NA       FALSE     NA      NA
## H.npnct20.log                            TRUE       FALSE     NA      NA
## H.npnct21.log                              NA       FALSE     NA      NA
## H.npnct22.log                              NA       FALSE     NA      NA
## H.npnct23.log                              NA       FALSE     NA      NA
## H.npnct24.log                            TRUE       FALSE     NA      NA
## H.npnct25.log                              NA       FALSE     NA      NA
## H.npnct26.log                              NA       FALSE     NA      NA
## H.npnct27.log                              NA       FALSE     NA      NA
## H.npnct29.log                              NA       FALSE     NA      NA
## H.npnct30.log                              NA       FALSE     NA      NA
## H.nwrds.unq.log                         FALSE       FALSE     NA      NA
## H.P.http                                   NA       FALSE     NA      NA
## H.P.today.in.politic                    FALSE       FALSE     NA      NA
## H.P.what.we.are                         FALSE       FALSE     NA      NA
## H.P.year.colon                          FALSE       FALSE     NA      NA
## H.T.daili                               FALSE       FALSE     NA      NA
## H.T.fashion                             FALSE       FALSE     NA      NA
## H.T.first                               FALSE       FALSE     NA      NA
## H.T.morn                                FALSE       FALSE     NA      NA
## H.T.report                              FALSE       FALSE     NA      NA
## H.T.today                               FALSE       FALSE     NA      NA
## H.T.X2015                               FALSE       FALSE     NA      NA
## Popular                                 FALSE        TRUE     NA      NA
## Popular.fctr                               NA          NA     NA    TRUE
## PubDate.last1                           FALSE       FALSE     NA      NA
## PubDate.last10                          FALSE       FALSE     NA      NA
## PubDate.last100                         FALSE       FALSE     NA      NA
## PubDate.month.fctr                      FALSE       FALSE     NA      NA
## PubDate.POSIX                           FALSE       FALSE     NA      NA
## PubDate.year.fctr                          NA       FALSE     NA      NA
## PubDate.zoo                             FALSE       FALSE     NA      NA
## S.nchrs.log                             FALSE       FALSE     NA      NA
## S.npnct02.log                            TRUE       FALSE     NA      NA
## S.npnct05.log                              NA       FALSE     NA      NA
## S.npnct09.log                              NA       FALSE     NA      NA
## S.npnct10.log                            TRUE       FALSE     NA      NA
## S.npnct13.log                           FALSE       FALSE     NA      NA
## S.npnct16.log                            TRUE       FALSE     NA      NA
## S.npnct17.log                              NA       FALSE     NA      NA
## S.npnct18.log                              NA       FALSE     NA      NA
## S.npnct19.log                           FALSE       FALSE     NA      NA
## S.npnct21.log                           FALSE       FALSE     NA      NA
## S.npnct22.log                              NA       FALSE     NA      NA
## S.npnct23.log                           FALSE       FALSE     NA      NA
## S.npnct24.log                            TRUE       FALSE     NA      NA
## S.npnct25.log                              NA       FALSE     NA      NA
## S.npnct26.log                              NA       FALSE     NA      NA
## S.npnct27.log                              NA       FALSE     NA      NA
## S.npnct29.log                              NA       FALSE     NA      NA
## S.npnct30.log                              NA       FALSE     NA      NA
## S.nwrds.log                             FALSE       FALSE     NA      NA
## S.nwrds.unq.log                         FALSE       FALSE     NA      NA
## S.P.daily.clip.report                   FALSE       FALSE     NA      NA
## S.P.http                                   NA       FALSE     NA      NA
## S.T.articl                              FALSE       FALSE     NA      NA
## S.T.can                                 FALSE       FALSE     NA      NA
## S.T.compani                             FALSE       FALSE     NA      NA
## S.T.diari                               FALSE       FALSE     NA      NA
## S.T.intern                              FALSE       FALSE     NA      NA
## S.T.new                                 FALSE       FALSE     NA      NA
## S.T.newyork                             FALSE       FALSE     NA      NA
## S.T.past                                FALSE       FALSE     NA      NA
## S.T.photo                               FALSE       FALSE     NA      NA
## S.T.scene                               FALSE       FALSE     NA      NA
## S.T.take                                FALSE       FALSE     NA      NA
## S.T.time                                FALSE       FALSE     NA      NA
## S.T.week                                FALSE       FALSE     NA      NA
## S.T.will                                FALSE       FALSE     NA      NA
## S.T.word                                FALSE       FALSE     NA      NA
## UniqueID                                FALSE       FALSE   TRUE      NA
## WordCount                               FALSE       FALSE     NA      NA
##                                importance Low.cor.X.glm.importance
## WordCount.log                1.000000e+02             1.000000e+02
## myCategory.fctr              7.209376e+01             7.209376e+01
## H.P.readers.respond          5.081905e+01             5.081905e+01
## A.npnct19.log                3.893603e+01             3.893603e+01
## H.nchrs.log                  3.635733e+01             3.635733e+01
## H.npnct19.log                3.367254e+01             3.367254e+01
## A.npnct13.log                3.078002e+01             3.078002e+01
## S.nuppr.log                  2.879518e+01             2.879518e+01
## H.npnct15.log                2.821906e+01             2.821906e+01
## H.npnct08.log                2.401124e+01             2.401124e+01
## H.T.read                     2.378859e+01             2.378859e+01
## A.T.newyork                  2.336015e+01             2.336015e+01
## H.T.polit                    2.244859e+01             2.244859e+01
## H.nuppr.log                  2.205468e+01             2.205468e+01
## H.T.word                     1.954393e+01             1.954393e+01
## PubDate.wkday.fctr           1.903080e+01             1.903080e+01
## S.ndgts.log                  1.850859e+01             1.850859e+01
## H.P.recap.colon              1.836394e+01             1.836394e+01
## H.P.no.comment.colon         1.833180e+01             1.833180e+01
## H.npnct11.log                1.779283e+01             1.779283e+01
## S.T.make                     1.534580e+01             1.534580e+01
## A.nwrds.unq.log              1.490474e+01             1.490474e+01
## S.T.state                    1.443146e+01             1.443146e+01
## S.T.report                   1.398733e+01             1.398733e+01
## S.T.one                      1.374995e+01             1.374995e+01
## S.T.share                    1.356646e+01             1.356646e+01
## S.T.show                     1.315591e+01             1.315591e+01
## PubDate.last10.log           1.314498e+01             1.314498e+01
## S.npnct04.log                1.289273e+01             1.289273e+01
## PubDate.second.fctr          1.202077e+01             1.202077e+01
## S.npnct08.log                1.201425e+01             1.201425e+01
## A.nwrds.log                  1.200285e+01             1.200285e+01
## A.T.will                     1.188661e+01             1.188661e+01
## .rnorm                       1.117476e+01             1.117476e+01
## A.T.can                      1.105762e+01             1.105762e+01
## PubDate.hour.fctr            1.096583e+01             1.096583e+01
## S.npnct11.log                1.085897e+01             1.085897e+01
## S.T.said                     1.064914e+01             1.064914e+01
## H.T.week                     1.043249e+01             1.043249e+01
## H.T.say                      1.025337e+01             1.025337e+01
## PubDate.minute.fctr          1.013617e+01             1.013617e+01
## H.npnct16.log                1.012826e+01             1.012826e+01
## H.ndgts.log                  1.011850e+01             1.011850e+01
## H.npnct01.log                9.941221e+00             9.941221e+00
## A.T.editor                   9.251126e+00             9.251126e+00
## H.T.news                     9.183951e+00             9.183951e+00
## H.T.newyork                  8.989821e+00             8.989821e+00
## H.T.new                      8.839602e+00             8.839602e+00
## H.npnct07.log                8.668352e+00             8.668352e+00
## H.T.art                      8.637437e+00             8.637437e+00
## PubDate.date.fctr            8.224594e+00             8.224594e+00
## A.T.compani                  7.930807e+00             7.930807e+00
## H.T.get                      7.893693e+00             7.893693e+00
## S.npnct01.log                7.444775e+00             7.444775e+00
## A.T.intern                   7.341904e+00             7.341904e+00
## H.npnct12.log                6.907483e+00             6.907483e+00
## PubDate.last1.log            6.802269e+00             6.802269e+00
## H.npnct13.log                6.670123e+00             6.670123e+00
## H.T.china                    6.528943e+00             6.528943e+00
## S.npnct12.log                6.513816e+00             6.513816e+00
## H.nwrds.log                  6.456493e+00             6.456493e+00
## A.T.take                     6.233315e+00             6.233315e+00
## A.T.time                     6.126992e+00             6.126992e+00
## S.T.day                      5.942272e+00             5.942272e+00
## H.T.bank                     5.837283e+00             5.837283e+00
## H.T.big                      5.817428e+00             5.817428e+00
## H.T.take                     5.634194e+00             5.634194e+00
## A.T.word                     5.460452e+00             5.460452e+00
## H.T.day                      5.186099e+00             5.186099e+00
## PubDate.wkend                5.162440e+00             5.162440e+00
## A.T.week                     5.150546e+00             5.150546e+00
## H.T.busi                     5.118511e+00             5.118511e+00
## H.T.X2014                    4.654351e+00             4.654351e+00
## A.nchrs.log                  4.611780e+00             4.611780e+00
## A.T.appear                   4.238918e+00             4.238918e+00
## S.T.highlight                4.226495e+00             4.226495e+00
## H.T.obama                    3.951640e+00             3.951640e+00
## H.T.billion                  3.898066e+00             3.898066e+00
## S.npnct14.log                3.890611e+00             3.890611e+00
## H.T.time                     3.528210e+00             3.528210e+00
## A.T.senat                    3.218199e+00             3.218199e+00
## S.npnct15.log                3.200871e+00             3.200871e+00
## PubDate.last100.log          2.899121e+00             2.899121e+00
## S.T.obama                    2.693325e+00             2.693325e+00
## H.T.make                     2.653710e+00             2.653710e+00
## H.npnct28.log                2.542638e+00             2.542638e+00
## S.T.presid                   2.497619e+00             2.497619e+00
## A.T.new                      2.017300e+00             2.017300e+00
## A.T.year                     1.915103e+00             1.915103e+00
## H.T.pictur                   1.665041e+00             1.665041e+00
## S.npnct06.log                1.563100e+00             1.563100e+00
## S.P.metropolitan.diary.colon 8.185782e-01             8.185782e-01
## S.T.first                    5.931209e-01             5.931209e-01
## A.npnct16.log                5.306992e-01             5.306992e-01
## H.T.ebola                    5.241256e-01             5.241256e-01
## S.T.fashion                  1.501731e-01             1.501731e-01
## A.T.scene                    1.351182e-01             1.351182e-01
## S.T.tribun                   1.203763e-01             1.203763e-01
## H.T.test                     1.077281e-01             1.077281e-01
## H.P.fashion.week             8.950571e-02             8.950571e-02
## H.npnct14.log                7.654941e-02             7.654941e-02
## H.T.deal                     5.179985e-02             5.179985e-02
## H.P.first.draft              4.237400e-02             4.237400e-02
## S.npnct28.log                4.126089e-02             4.126089e-02
## H.P.daily.clip.report        3.602811e-02             3.602811e-02
## H.P.today.in.smallbusiness   2.825282e-02             2.825282e-02
## H.P.verbatim.colon           1.699712e-02             1.699712e-02
## H.npnct02.log                1.350933e-02             1.350933e-02
## S.P.first.draft              1.337679e-02             1.337679e-02
## S.npnct03.log                1.100069e-02             1.100069e-02
## S.npnct20.log                1.087710e-02             1.087710e-02
## H.P.quandary                 1.025645e-02             1.025645e-02
## A.npnct18.log                9.131873e-03             9.131873e-03
## S.P.year.colon               7.061198e-03             7.061198e-03
## H.P.on.this.day              6.324980e-03             6.324980e-03
## H.npnct05.log                3.595486e-03             3.595486e-03
## S.npnct07.log                1.720848e-03             1.720848e-03
## H.P.s.notebook               8.198403e-05             8.198403e-05
## S.P.fashion.week             0.000000e+00             0.000000e+00
## A.ndgts.log                            NA                       NA
## A.npnct01.log                          NA                       NA
## A.npnct02.log                          NA                       NA
## A.npnct03.log                          NA                       NA
## A.npnct04.log                          NA                       NA
## A.npnct05.log                          NA                       NA
## A.npnct06.log                          NA                       NA
## A.npnct07.log                          NA                       NA
## A.npnct08.log                          NA                       NA
## A.npnct09.log                          NA                       NA
## A.npnct10.log                          NA                       NA
## A.npnct11.log                          NA                       NA
## A.npnct12.log                          NA                       NA
## A.npnct14.log                          NA                       NA
## A.npnct15.log                          NA                       NA
## A.npnct17.log                          NA                       NA
## A.npnct20.log                          NA                       NA
## A.npnct21.log                          NA                       NA
## A.npnct22.log                          NA                       NA
## A.npnct23.log                          NA                       NA
## A.npnct24.log                          NA                       NA
## A.npnct25.log                          NA                       NA
## A.npnct26.log                          NA                       NA
## A.npnct27.log                          NA                       NA
## A.npnct28.log                          NA                       NA
## A.npnct29.log                          NA                       NA
## A.npnct30.log                          NA                       NA
## A.nuppr.log                            NA                       NA
## A.P.daily.clip.report                  NA                       NA
## A.P.fashion.week                       NA                       NA
## A.P.first.draft                        NA                       NA
## A.P.http                               NA                       NA
## A.P.metropolitan.diary.colon           NA                       NA
## A.P.year.colon                         NA                       NA
## A.T.day                                NA                       NA
## A.T.fashion                            NA                       NA
## A.T.obama                              NA                       NA
## A.T.one                                NA                       NA
## A.T.photo                              NA                       NA
## A.T.report                             NA                       NA
## A.T.said                               NA                       NA
## A.T.state                              NA                       NA
## clusterid                              NA                       NA
## H.npnct03.log                          NA                       NA
## H.npnct04.log                          NA                       NA
## H.npnct06.log                          NA                       NA
## H.npnct09.log                          NA                       NA
## H.npnct10.log                          NA                       NA
## H.npnct17.log                          NA                       NA
## H.npnct18.log                          NA                       NA
## H.npnct20.log                          NA                       NA
## H.npnct21.log                          NA                       NA
## H.npnct22.log                          NA                       NA
## H.npnct23.log                          NA                       NA
## H.npnct24.log                          NA                       NA
## H.npnct25.log                          NA                       NA
## H.npnct26.log                          NA                       NA
## H.npnct27.log                          NA                       NA
## H.npnct29.log                          NA                       NA
## H.npnct30.log                          NA                       NA
## H.nwrds.unq.log                        NA                       NA
## H.P.http                               NA                       NA
## H.P.today.in.politic                   NA                       NA
## H.P.what.we.are                        NA                       NA
## H.P.year.colon                         NA                       NA
## H.T.daili                              NA                       NA
## H.T.fashion                            NA                       NA
## H.T.first                              NA                       NA
## H.T.morn                               NA                       NA
## H.T.report                             NA                       NA
## H.T.today                              NA                       NA
## H.T.X2015                              NA                       NA
## Popular                                NA                       NA
## Popular.fctr                           NA                       NA
## PubDate.last1                          NA                       NA
## PubDate.last10                         NA                       NA
## PubDate.last100                        NA                       NA
## PubDate.month.fctr                     NA                       NA
## PubDate.POSIX                          NA                       NA
## PubDate.year.fctr                      NA                       NA
## PubDate.zoo                            NA                       NA
## S.nchrs.log                            NA                       NA
## S.npnct02.log                          NA                       NA
## S.npnct05.log                          NA                       NA
## S.npnct09.log                          NA                       NA
## S.npnct10.log                          NA                       NA
## S.npnct13.log                          NA                       NA
## S.npnct16.log                          NA                       NA
## S.npnct17.log                          NA                       NA
## S.npnct18.log                          NA                       NA
## S.npnct19.log                          NA                       NA
## S.npnct21.log                          NA                       NA
## S.npnct22.log                          NA                       NA
## S.npnct23.log                          NA                       NA
## S.npnct24.log                          NA                       NA
## S.npnct25.log                          NA                       NA
## S.npnct26.log                          NA                       NA
## S.npnct27.log                          NA                       NA
## S.npnct29.log                          NA                       NA
## S.npnct30.log                          NA                       NA
## S.nwrds.log                            NA                       NA
## S.nwrds.unq.log                        NA                       NA
## S.P.daily.clip.report                  NA                       NA
## S.P.http                               NA                       NA
## S.T.articl                             NA                       NA
## S.T.can                                NA                       NA
## S.T.compani                            NA                       NA
## S.T.diari                              NA                       NA
## S.T.intern                             NA                       NA
## S.T.new                                NA                       NA
## S.T.newyork                            NA                       NA
## S.T.past                               NA                       NA
## S.T.photo                              NA                       NA
## S.T.scene                              NA                       NA
## S.T.take                               NA                       NA
## S.T.time                               NA                       NA
## S.T.week                               NA                       NA
## S.T.will                               NA                       NA
## S.T.word                               NA                       NA
## UniqueID                               NA                       NA
## WordCount                              NA                       NA
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 118
```

![](NYTBlogs_clusters_files/figure-html/fit.models_2-8.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_2-9.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_2-10.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_2-11.png) ![](NYTBlogs_clusters_files/figure-html/fit.models_2-12.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Low.cor.X.glm.prob
## 6018     6018            N                             0.000281916
## 6370     6370            Y                             0.407680594
##      Popular.fctr.predict.Low.cor.X.glm
## 6018                                  N
## 6370                                  Y
##      Popular.fctr.predict.Low.cor.X.glm.accurate
## 6018                                        TRUE
## 6370                                        TRUE
##      Popular.fctr.predict.Low.cor.X.glm.error .label
## 6018                                        0   6018
## 6370                                        0   6370
## [1] "Inaccurate: "
##      UniqueID Popular.fctr Popular.fctr.predict.Low.cor.X.glm.prob
## 3743     3743            Y                            2.220446e-16
## 5423     5423            Y                            2.220446e-16
## 6387     6387            Y                            4.370417e-11
## 5573     5573            Y                            1.308058e-10
## 2026     2026            Y                            3.349473e-10
## 5486     5486            Y                            6.427948e-10
##      Popular.fctr.predict.Low.cor.X.glm
## 3743                                  N
## 5423                                  N
## 6387                                  N
## 5573                                  N
## 2026                                  N
## 5486                                  N
##      Popular.fctr.predict.Low.cor.X.glm.accurate
## 3743                                       FALSE
## 5423                                       FALSE
## 6387                                       FALSE
## 5573                                       FALSE
## 2026                                       FALSE
## 5486                                       FALSE
##      Popular.fctr.predict.Low.cor.X.glm.error
## 3743                                     -0.4
## 5423                                     -0.4
## 6387                                     -0.4
## 5573                                     -0.4
## 2026                                     -0.4
## 5486                                     -0.4
##      UniqueID Popular.fctr Popular.fctr.predict.Low.cor.X.glm.prob
## 1273     1273            Y                               0.1210278
## 4735     4735            Y                               0.3814447
## 4597     4597            N                               0.7122071
## 214       214            N                               0.7169351
## 5625     5625            N                               0.8050491
## 5523     5523            N                               0.8992834
##      Popular.fctr.predict.Low.cor.X.glm
## 1273                                  N
## 4735                                  N
## 4597                                  Y
## 214                                   Y
## 5625                                  Y
## 5523                                  Y
##      Popular.fctr.predict.Low.cor.X.glm.accurate
## 1273                                       FALSE
## 4735                                       FALSE
## 4597                                       FALSE
## 214                                        FALSE
## 5625                                       FALSE
## 5523                                       FALSE
##      Popular.fctr.predict.Low.cor.X.glm.error
## 1273                              -0.27897223
## 4735                              -0.01855531
## 4597                               0.31220710
## 214                                0.31693511
## 5625                               0.40504906
## 5523                               0.49928338
##      UniqueID Popular.fctr Popular.fctr.predict.Low.cor.X.glm.prob
## 725       725            N                               0.9170724
## 4771     4771            N                               0.9351144
## 4975     4975            N                               0.9507485
## 1667     1667            N                               0.9698354
## 4882     4882            N                               0.9835641
## 770       770            N                               0.9933784
##      Popular.fctr.predict.Low.cor.X.glm
## 725                                   Y
## 4771                                  Y
## 4975                                  Y
## 1667                                  Y
## 4882                                  Y
## 770                                   Y
##      Popular.fctr.predict.Low.cor.X.glm.accurate
## 725                                        FALSE
## 4771                                       FALSE
## 4975                                       FALSE
## 1667                                       FALSE
## 4882                                       FALSE
## 770                                        FALSE
##      Popular.fctr.predict.Low.cor.X.glm.error
## 725                                 0.5170724
## 4771                                0.5351144
## 4975                                0.5507485
## 1667                                0.5698354
## 4882                                0.5835641
## 770                                 0.5933784
```

![](NYTBlogs_clusters_files/figure-html/fit.models_2-13.png) 

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
## [2] Popular.fctr.predict.Low.cor.X.glm.prob    
## [3] Popular.fctr.predict.Low.cor.X.glm         
## [4] Popular.fctr.predict.Low.cor.X.glm.accurate
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
## [1] WordCount.log       myCategory.fctr     H.P.readers.respond
## [4] A.npnct19.log       H.nchrs.log        
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
## 12 fit.models          7          2 290.218 306.917  16.699
## 13 fit.models          7          3 306.917      NA      NA
```


```r
print(setdiff(names(glb_trnent_df), names(glb_entity_df)))
```

```
##  [1] "PubDate.year.fctr" "H.npnct03.log"     "H.npnct09.log"    
##  [4] "H.npnct10.log"     "H.npnct17.log"     "H.npnct18.log"    
##  [7] "H.npnct20.log"     "H.npnct21.log"     "H.npnct22.log"    
## [10] "H.npnct23.log"     "H.npnct24.log"     "H.npnct25.log"    
## [13] "H.npnct26.log"     "H.npnct27.log"     "H.npnct29.log"    
## [16] "H.npnct30.log"     "H.P.http"          "S.npnct02.log"    
## [19] "S.npnct05.log"     "S.npnct09.log"     "S.npnct10.log"    
## [22] "S.npnct17.log"     "S.npnct18.log"     "S.npnct21.log"    
## [25] "S.npnct22.log"     "S.npnct23.log"     "S.npnct24.log"    
## [28] "S.npnct25.log"     "S.npnct26.log"     "S.npnct27.log"    
## [31] "S.npnct29.log"     "S.npnct30.log"     "S.P.http"         
## [34] "A.npnct05.log"     "A.npnct09.log"     "A.npnct10.log"    
## [37] "A.npnct21.log"     "A.npnct22.log"     "A.npnct23.log"    
## [40] "A.npnct24.log"     "A.npnct25.log"     "A.npnct26.log"    
## [43] "A.npnct27.log"     "A.npnct29.log"     "A.npnct30.log"    
## [46] "clusterid"
```

```r
print(setdiff(names(glb_fitent_df), names(glb_entity_df)))
```

```
##  [1] "PubDate.year.fctr" "H.npnct03.log"     "H.npnct09.log"    
##  [4] "H.npnct10.log"     "H.npnct17.log"     "H.npnct18.log"    
##  [7] "H.npnct20.log"     "H.npnct21.log"     "H.npnct22.log"    
## [10] "H.npnct23.log"     "H.npnct24.log"     "H.npnct25.log"    
## [13] "H.npnct26.log"     "H.npnct27.log"     "H.npnct29.log"    
## [16] "H.npnct30.log"     "H.P.http"          "S.npnct02.log"    
## [19] "S.npnct05.log"     "S.npnct09.log"     "S.npnct10.log"    
## [22] "S.npnct17.log"     "S.npnct18.log"     "S.npnct21.log"    
## [25] "S.npnct22.log"     "S.npnct23.log"     "S.npnct24.log"    
## [28] "S.npnct25.log"     "S.npnct26.log"     "S.npnct27.log"    
## [31] "S.npnct29.log"     "S.npnct30.log"     "S.P.http"         
## [34] "A.npnct05.log"     "A.npnct09.log"     "A.npnct10.log"    
## [37] "A.npnct21.log"     "A.npnct22.log"     "A.npnct23.log"    
## [40] "A.npnct24.log"     "A.npnct25.log"     "A.npnct26.log"    
## [43] "A.npnct27.log"     "A.npnct29.log"     "A.npnct30.log"    
## [46] "clusterid"
```

```r
print(setdiff(names(glb_OOBent_df), names(glb_entity_df)))
```

```
##  [1] "PubDate.year.fctr"                          
##  [2] "H.npnct03.log"                              
##  [3] "H.npnct09.log"                              
##  [4] "H.npnct10.log"                              
##  [5] "H.npnct17.log"                              
##  [6] "H.npnct18.log"                              
##  [7] "H.npnct20.log"                              
##  [8] "H.npnct21.log"                              
##  [9] "H.npnct22.log"                              
## [10] "H.npnct23.log"                              
## [11] "H.npnct24.log"                              
## [12] "H.npnct25.log"                              
## [13] "H.npnct26.log"                              
## [14] "H.npnct27.log"                              
## [15] "H.npnct29.log"                              
## [16] "H.npnct30.log"                              
## [17] "H.P.http"                                   
## [18] "S.npnct02.log"                              
## [19] "S.npnct05.log"                              
## [20] "S.npnct09.log"                              
## [21] "S.npnct10.log"                              
## [22] "S.npnct17.log"                              
## [23] "S.npnct18.log"                              
## [24] "S.npnct21.log"                              
## [25] "S.npnct22.log"                              
## [26] "S.npnct23.log"                              
## [27] "S.npnct24.log"                              
## [28] "S.npnct25.log"                              
## [29] "S.npnct26.log"                              
## [30] "S.npnct27.log"                              
## [31] "S.npnct29.log"                              
## [32] "S.npnct30.log"                              
## [33] "S.P.http"                                   
## [34] "A.npnct05.log"                              
## [35] "A.npnct09.log"                              
## [36] "A.npnct10.log"                              
## [37] "A.npnct21.log"                              
## [38] "A.npnct22.log"                              
## [39] "A.npnct23.log"                              
## [40] "A.npnct24.log"                              
## [41] "A.npnct25.log"                              
## [42] "A.npnct26.log"                              
## [43] "A.npnct27.log"                              
## [44] "A.npnct29.log"                              
## [45] "A.npnct30.log"                              
## [46] "clusterid"                                  
## [47] "Popular.fctr.predict.Low.cor.X.glm.prob"    
## [48] "Popular.fctr.predict.Low.cor.X.glm"         
## [49] "Popular.fctr.predict.Low.cor.X.glm.accurate"
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

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
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

![](NYTBlogs_clusters_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn    end elapsed
## 13        fit.models          7          3 306.917 313.16   6.243
## 14 fit.data.training          8          0 313.160     NA      NA
```

## Step `8.0: fit data training`

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
##                                                        id   importance
## WordCount.log                               WordCount.log 1.000000e+02
## myCategory.fctr                           myCategory.fctr 7.209376e+01
## H.P.readers.respond                   H.P.readers.respond 5.081905e+01
## A.npnct19.log                               A.npnct19.log 3.893603e+01
## H.nchrs.log                                   H.nchrs.log 3.635733e+01
## H.npnct19.log                               H.npnct19.log 3.367254e+01
## A.npnct13.log                               A.npnct13.log 3.078002e+01
## S.nuppr.log                                   S.nuppr.log 2.879518e+01
## H.npnct15.log                               H.npnct15.log 2.821906e+01
## H.npnct08.log                               H.npnct08.log 2.401124e+01
## H.T.read                                         H.T.read 2.378859e+01
## A.T.newyork                                   A.T.newyork 2.336015e+01
## H.T.polit                                       H.T.polit 2.244859e+01
## H.nuppr.log                                   H.nuppr.log 2.205468e+01
## H.T.word                                         H.T.word 1.954393e+01
## PubDate.wkday.fctr                     PubDate.wkday.fctr 1.903080e+01
## S.ndgts.log                                   S.ndgts.log 1.850859e+01
## H.P.recap.colon                           H.P.recap.colon 1.836394e+01
## H.P.no.comment.colon                 H.P.no.comment.colon 1.833180e+01
## H.npnct11.log                               H.npnct11.log 1.779283e+01
## S.T.make                                         S.T.make 1.534580e+01
## A.nwrds.unq.log                           A.nwrds.unq.log 1.490474e+01
## S.T.state                                       S.T.state 1.443146e+01
## S.T.report                                     S.T.report 1.398733e+01
## S.T.one                                           S.T.one 1.374995e+01
## S.T.share                                       S.T.share 1.356646e+01
## S.T.show                                         S.T.show 1.315591e+01
## PubDate.last10.log                     PubDate.last10.log 1.314498e+01
## S.npnct04.log                               S.npnct04.log 1.289273e+01
## PubDate.second.fctr                   PubDate.second.fctr 1.202077e+01
## S.npnct08.log                               S.npnct08.log 1.201425e+01
## A.nwrds.log                                   A.nwrds.log 1.200285e+01
## A.T.will                                         A.T.will 1.188661e+01
## .rnorm                                             .rnorm 1.117476e+01
## A.T.can                                           A.T.can 1.105762e+01
## PubDate.hour.fctr                       PubDate.hour.fctr 1.096583e+01
## S.npnct11.log                               S.npnct11.log 1.085897e+01
## S.T.said                                         S.T.said 1.064914e+01
## H.T.week                                         H.T.week 1.043249e+01
## H.T.say                                           H.T.say 1.025337e+01
## PubDate.minute.fctr                   PubDate.minute.fctr 1.013617e+01
## H.npnct16.log                               H.npnct16.log 1.012826e+01
## H.ndgts.log                                   H.ndgts.log 1.011850e+01
## H.npnct01.log                               H.npnct01.log 9.941221e+00
## A.T.editor                                     A.T.editor 9.251126e+00
## H.T.news                                         H.T.news 9.183951e+00
## H.T.newyork                                   H.T.newyork 8.989821e+00
## H.T.new                                           H.T.new 8.839602e+00
## H.npnct07.log                               H.npnct07.log 8.668352e+00
## H.T.art                                           H.T.art 8.637437e+00
## PubDate.date.fctr                       PubDate.date.fctr 8.224594e+00
## A.T.compani                                   A.T.compani 7.930807e+00
## H.T.get                                           H.T.get 7.893693e+00
## S.npnct01.log                               S.npnct01.log 7.444775e+00
## A.T.intern                                     A.T.intern 7.341904e+00
## H.npnct12.log                               H.npnct12.log 6.907483e+00
## PubDate.last1.log                       PubDate.last1.log 6.802269e+00
## H.npnct13.log                               H.npnct13.log 6.670123e+00
## H.T.china                                       H.T.china 6.528943e+00
## S.npnct12.log                               S.npnct12.log 6.513816e+00
## H.nwrds.log                                   H.nwrds.log 6.456493e+00
## A.T.take                                         A.T.take 6.233315e+00
## A.T.time                                         A.T.time 6.126992e+00
## S.T.day                                           S.T.day 5.942272e+00
## H.T.bank                                         H.T.bank 5.837283e+00
## H.T.big                                           H.T.big 5.817428e+00
## H.T.take                                         H.T.take 5.634194e+00
## A.T.word                                         A.T.word 5.460452e+00
## H.T.day                                           H.T.day 5.186099e+00
## PubDate.wkend                               PubDate.wkend 5.162440e+00
## A.T.week                                         A.T.week 5.150546e+00
## H.T.busi                                         H.T.busi 5.118511e+00
## H.T.X2014                                       H.T.X2014 4.654351e+00
## A.nchrs.log                                   A.nchrs.log 4.611780e+00
## A.T.appear                                     A.T.appear 4.238918e+00
## S.T.highlight                               S.T.highlight 4.226495e+00
## H.T.obama                                       H.T.obama 3.951640e+00
## H.T.billion                                   H.T.billion 3.898066e+00
## S.npnct14.log                               S.npnct14.log 3.890611e+00
## H.T.time                                         H.T.time 3.528210e+00
## A.T.senat                                       A.T.senat 3.218199e+00
## S.npnct15.log                               S.npnct15.log 3.200871e+00
## PubDate.last100.log                   PubDate.last100.log 2.899121e+00
## S.T.obama                                       S.T.obama 2.693325e+00
## H.T.make                                         H.T.make 2.653710e+00
## H.npnct28.log                               H.npnct28.log 2.542638e+00
## S.T.presid                                     S.T.presid 2.497619e+00
## A.T.new                                           A.T.new 2.017300e+00
## A.T.year                                         A.T.year 1.915103e+00
## H.T.pictur                                     H.T.pictur 1.665041e+00
## S.npnct06.log                               S.npnct06.log 1.563100e+00
## S.P.metropolitan.diary.colon S.P.metropolitan.diary.colon 8.185782e-01
## S.T.first                                       S.T.first 5.931209e-01
## A.npnct16.log                               A.npnct16.log 5.306992e-01
## H.T.ebola                                       H.T.ebola 5.241256e-01
## S.T.fashion                                   S.T.fashion 1.501731e-01
## A.T.scene                                       A.T.scene 1.351182e-01
## S.T.tribun                                     S.T.tribun 1.203763e-01
## H.T.test                                         H.T.test 1.077281e-01
## H.P.fashion.week                         H.P.fashion.week 8.950571e-02
## H.npnct14.log                               H.npnct14.log 7.654941e-02
## H.T.deal                                         H.T.deal 5.179985e-02
## H.P.first.draft                           H.P.first.draft 4.237400e-02
## S.npnct28.log                               S.npnct28.log 4.126089e-02
## H.P.daily.clip.report               H.P.daily.clip.report 3.602811e-02
## H.P.today.in.smallbusiness     H.P.today.in.smallbusiness 2.825282e-02
## H.P.verbatim.colon                     H.P.verbatim.colon 1.699712e-02
## H.npnct02.log                               H.npnct02.log 1.350933e-02
## S.P.first.draft                           S.P.first.draft 1.337679e-02
## S.npnct03.log                               S.npnct03.log 1.100069e-02
## S.npnct20.log                               S.npnct20.log 1.087710e-02
## H.P.quandary                                 H.P.quandary 1.025645e-02
## A.npnct18.log                               A.npnct18.log 9.131873e-03
## S.P.year.colon                             S.P.year.colon 7.061198e-03
## H.P.on.this.day                           H.P.on.this.day 6.324980e-03
## H.npnct05.log                               H.npnct05.log 3.595486e-03
## S.npnct07.log                               S.npnct07.log 1.720848e-03
## H.P.s.notebook                             H.P.s.notebook 8.198403e-05
## S.P.fashion.week                         S.P.fashion.week 0.000000e+00
## [1] "fitting model: Final.glm"
## [1] "    indep_vars: WordCount.log, myCategory.fctr, H.P.readers.respond, A.npnct19.log, H.nchrs.log, H.npnct19.log, A.npnct13.log, S.nuppr.log, H.npnct15.log, H.npnct08.log, H.T.read, A.T.newyork, H.T.polit, H.nuppr.log, H.T.word, PubDate.wkday.fctr, S.ndgts.log, H.P.recap.colon, H.P.no.comment.colon, H.npnct11.log, S.T.make, A.nwrds.unq.log, S.T.state, S.T.report, S.T.one, S.T.share, S.T.show, PubDate.last10.log, S.npnct04.log, PubDate.second.fctr, S.npnct08.log, A.nwrds.log, A.T.will, .rnorm, A.T.can, PubDate.hour.fctr, S.npnct11.log, S.T.said, H.T.week, H.T.say, PubDate.minute.fctr, H.npnct16.log, H.ndgts.log, H.npnct01.log, A.T.editor, H.T.news, H.T.newyork, H.T.new, H.npnct07.log, H.T.art, PubDate.date.fctr, A.T.compani, H.T.get, S.npnct01.log, A.T.intern, H.npnct12.log, PubDate.last1.log, H.npnct13.log, H.T.china, S.npnct12.log, H.nwrds.log, A.T.take, A.T.time, S.T.day, H.T.bank, H.T.big, H.T.take, A.T.word, H.T.day, PubDate.wkend, A.T.week, H.T.busi, H.T.X2014, A.nchrs.log, A.T.appear, S.T.highlight, H.T.obama, H.T.billion, S.npnct14.log, H.T.time, A.T.senat, S.npnct15.log, PubDate.last100.log, S.T.obama, H.T.make, H.npnct28.log, S.T.presid, A.T.new, A.T.year, H.T.pictur, S.npnct06.log, S.P.metropolitan.diary.colon, S.T.first, A.npnct16.log, H.T.ebola, S.T.fashion, A.T.scene, S.T.tribun, H.T.test, H.P.fashion.week, H.npnct14.log, H.T.deal, H.P.first.draft, S.npnct28.log, H.P.daily.clip.report, H.P.today.in.smallbusiness, H.P.verbatim.colon, H.npnct02.log, S.P.first.draft, S.npnct03.log, S.npnct20.log, H.P.quandary, A.npnct18.log, S.P.year.colon, H.P.on.this.day, H.npnct05.log, S.npnct07.log, H.P.s.notebook, S.P.fashion.week"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](NYTBlogs_clusters_files/figure-html/fit.data.training_0-1.png) ![](NYTBlogs_clusters_files/figure-html/fit.data.training_0-2.png) ![](NYTBlogs_clusters_files/figure-html/fit.data.training_0-3.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](NYTBlogs_clusters_files/figure-html/fit.data.training_0-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.2795  -0.3091  -0.1378   0.0000   3.4982  
## 
## Coefficients:
##                                                         Estimate
## (Intercept)                                           -3.289e+00
## WordCount.log                                          1.164e+00
## `myCategory.fctrForeign#World#Asia Pacific`           -4.408e+00
## `myCategory.fctr#Multimedia#`                         -4.583e+00
## `myCategory.fctrCulture#Arts#`                        -3.006e+00
## `myCategory.fctrBusiness#Business Day#Dealbook`       -2.600e+00
## myCategory.fctrmyOther                                -2.149e+01
## `myCategory.fctrBusiness#Technology#`                 -2.011e+00
## `myCategory.fctrBusiness#Crosswords/Games#`            4.801e-01
## `myCategory.fctrTStyle##`                             -4.419e+00
## `myCategory.fctrForeign#World#`                       -1.810e+01
## `myCategory.fctrOpEd#Opinion#`                         7.598e-01
## `myCategory.fctrStyles##Fashion`                      -5.360e+00
## `myCategory.fctr#Opinion#Room For Debate`             -7.267e+00
## `myCategory.fctr#U.S.#Education`                      -2.221e+01
## `myCategory.fctr##`                                   -2.572e+00
## `myCategory.fctrMetro#N.Y. / Region#`                 -1.738e+00
## `myCategory.fctrBusiness#Business Day#Small Business` -4.009e+00
## `myCategory.fctrStyles#U.S.#`                         -6.032e-01
## `myCategory.fctrTravel#Travel#`                       -4.429e+00
## `myCategory.fctr#Opinion#The Public Editor`            5.302e-01
## H.P.readers.respond                                    7.199e+00
## A.npnct19.log                                          1.678e+00
## H.nchrs.log                                           -1.440e+00
## H.npnct19.log                                          1.258e+00
## A.npnct13.log                                          9.457e-01
## S.nuppr.log                                           -5.480e-01
## H.npnct15.log                                         -9.215e-01
## H.npnct08.log                                          1.184e+00
## H.T.read                                              -1.073e+00
## A.T.newyork                                            2.215e+00
## H.T.polit                                             -7.245e-01
## H.nuppr.log                                            1.030e+00
## H.T.word                                               2.134e+00
## PubDate.wkday.fctr1                                   -5.101e-01
## PubDate.wkday.fctr2                                   -8.549e-01
## PubDate.wkday.fctr3                                   -5.560e-01
## PubDate.wkday.fctr4                                   -6.387e-01
## PubDate.wkday.fctr5                                   -7.448e-01
## PubDate.wkday.fctr6                                   -7.020e-01
## S.ndgts.log                                           -2.897e-01
## H.P.recap.colon                                        1.418e+00
## H.P.no.comment.colon                                   2.319e+00
## H.npnct11.log                                          3.756e-01
## S.T.make                                              -7.949e-01
## A.nwrds.unq.log                                       -1.009e+00
## S.T.state                                              8.735e-01
## S.T.report                                            -9.329e-01
## S.T.one                                               -5.131e-01
## S.T.share                                             -1.580e+00
## S.T.show                                              -9.103e-01
## PubDate.last10.log                                     1.559e-01
## S.npnct04.log                                         -8.749e-01
## `PubDate.second.fctr(14.8,29.5]`                      -6.997e-02
## `PubDate.second.fctr(29.5,44.2]`                      -2.545e-02
## `PubDate.second.fctr(44.2,59.1]`                      -2.118e-01
## S.npnct08.log                                          1.313e-01
## A.nwrds.log                                           -6.736e-01
## A.T.will                                              -9.994e-01
## .rnorm                                                -7.200e-02
## A.T.can                                               -1.016e+00
## `PubDate.hour.fctr(7.67,15.3]`                         1.414e-01
## `PubDate.hour.fctr(15.3,23]`                           2.377e-01
## S.npnct11.log                                         -1.212e-01
## S.T.said                                               1.620e+00
## H.T.week                                              -1.046e+00
## H.T.say                                               -3.402e-01
## `PubDate.minute.fctr(14.8,29.5]`                       1.038e-02
## `PubDate.minute.fctr(29.5,44.2]`                      -2.220e-01
## `PubDate.minute.fctr(44.2,59.1]`                       4.752e-02
## H.npnct16.log                                          5.673e-01
## H.ndgts.log                                            1.229e-01
## H.npnct01.log                                         -1.148e+00
## A.T.editor                                            -1.735e+00
## H.T.news                                              -1.039e+00
## H.T.newyork                                           -9.332e-01
## H.T.new                                               -4.079e-01
## H.npnct07.log                                          2.577e-02
## H.T.art                                               -1.926e-01
## `PubDate.date.fctr(7,13]`                              5.304e-02
## `PubDate.date.fctr(13,19]`                            -1.451e-01
## `PubDate.date.fctr(19,25]`                            -2.887e-02
## `PubDate.date.fctr(25,31]`                            -3.076e-02
## A.T.compani                                           -9.266e-01
## H.T.get                                                6.388e-01
## S.npnct01.log                                          1.953e+00
## A.T.intern                                             6.999e-01
## H.npnct12.log                                          8.289e-02
## PubDate.last1.log                                     -2.250e-02
## H.npnct13.log                                         -9.959e-02
## H.T.china                                             -1.300e-01
## S.npnct12.log                                         -1.676e-01
## H.nwrds.log                                            6.737e-01
## A.T.take                                              -2.844e-01
## A.T.time                                              -8.989e-01
## S.T.day                                               -1.491e-01
## H.T.bank                                               2.664e-01
## H.T.big                                               -1.296e-01
## H.T.take                                              -2.713e-01
## A.T.word                                              -1.038e+00
## H.T.day                                               -1.686e-01
## PubDate.wkend                                         -3.643e-02
## A.T.week                                              -4.013e-01
## H.T.busi                                              -1.047e+00
## H.T.X2014                                             -3.292e-01
## A.nchrs.log                                            4.121e-01
## A.T.appear                                            -3.832e-01
## S.T.highlight                                          6.696e-01
## H.T.obama                                              4.615e-03
## H.T.billion                                            3.371e-01
## S.npnct14.log                                          4.831e-01
## H.T.time                                              -4.543e-03
## A.T.senat                                             -1.769e-01
## S.npnct15.log                                         -3.556e-01
## PubDate.last100.log                                   -5.129e-03
## S.T.obama                                             -2.632e-01
## H.T.make                                              -2.037e-01
## H.npnct28.log                                         -5.424e-01
## S.T.presid                                            -2.753e-01
## A.T.new                                               -3.456e-01
## A.T.year                                              -6.454e-02
## H.T.pictur                                             1.601e-01
## S.npnct06.log                                          1.026e+00
## S.P.metropolitan.diary.colon                          -3.453e-01
## S.T.first                                              3.346e-02
## A.npnct16.log                                          4.353e-01
## H.T.ebola                                             -2.273e-01
## S.T.fashion                                           -1.967e+00
## A.T.scene                                             -3.567e+00
## S.T.tribun                                            -4.100e+01
## H.T.test                                               1.909e-01
## H.P.fashion.week                                      -1.254e+01
## H.npnct14.log                                         -1.982e+01
## H.T.deal                                              -2.969e-01
## H.P.first.draft                                       -1.622e+01
## S.npnct28.log                                         -1.509e+01
## H.P.daily.clip.report                                 -1.707e+01
## H.P.today.in.smallbusiness                            -1.563e+01
## H.P.verbatim.colon                                    -1.489e+01
## H.npnct02.log                                         -1.766e+01
## S.P.first.draft                                       -1.533e+01
## S.npnct03.log                                         -2.993e+01
## S.npnct20.log                                         -2.459e+01
## H.P.quandary                                           2.181e+01
## A.npnct18.log                                         -2.773e+01
## S.P.year.colon                                        -1.004e+01
## H.P.on.this.day                                       -1.611e+01
## H.npnct05.log                                          7.709e-01
## S.npnct07.log                                         -2.381e+01
## H.P.s.notebook                                         1.848e+00
## S.P.fashion.week                                      -1.263e+01
##                                                       Std. Error z value
## (Intercept)                                            2.363e+00  -1.392
## WordCount.log                                          7.829e-02  14.862
## `myCategory.fctrForeign#World#Asia Pacific`            6.536e-01  -6.745
## `myCategory.fctr#Multimedia#`                          7.738e-01  -5.923
## `myCategory.fctrCulture#Arts#`                         3.420e-01  -8.791
## `myCategory.fctrBusiness#Business Day#Dealbook`        2.589e-01 -10.044
## myCategory.fctrmyOther                                 2.398e+03  -0.009
## `myCategory.fctrBusiness#Technology#`                  2.678e-01  -7.512
## `myCategory.fctrBusiness#Crosswords/Games#`            3.906e-01   1.229
## `myCategory.fctrTStyle##`                              4.183e-01 -10.563
## `myCategory.fctrForeign#World#`                        9.670e+02  -0.019
## `myCategory.fctrOpEd#Opinion#`                         2.485e-01   3.057
## `myCategory.fctrStyles##Fashion`                       1.102e+00  -4.865
## `myCategory.fctr#Opinion#Room For Debate`              8.077e-01  -8.997
## `myCategory.fctr#U.S.#Education`                       8.131e+02  -0.027
## `myCategory.fctr##`                                    2.392e-01 -10.752
## `myCategory.fctrMetro#N.Y. / Region#`                  4.878e-01  -3.564
## `myCategory.fctrBusiness#Business Day#Small Business`  5.473e-01  -7.325
## `myCategory.fctrStyles#U.S.#`                          2.797e-01  -2.156
## `myCategory.fctrTravel#Travel#`                        1.035e+00  -4.280
## `myCategory.fctr#Opinion#The Public Editor`            7.150e-01   0.741
## H.P.readers.respond                                    9.988e-01   7.207
## A.npnct19.log                                          2.777e-01   6.043
## H.nchrs.log                                            2.983e-01  -4.826
## H.npnct19.log                                          2.681e-01   4.693
## A.npnct13.log                                          2.202e-01   4.296
## S.nuppr.log                                            1.325e-01  -4.135
## H.npnct15.log                                          2.733e-01  -3.372
## H.npnct08.log                                          3.934e-01   3.010
## H.T.read                                               3.645e-01  -2.944
## A.T.newyork                                            9.045e-01   2.448
## H.T.polit                                              2.357e-01  -3.074
## H.nuppr.log                                            3.467e-01   2.972
## H.T.word                                               8.199e-01   2.603
## PubDate.wkday.fctr1                                    4.320e-01  -1.181
## PubDate.wkday.fctr2                                    4.739e-01  -1.804
## PubDate.wkday.fctr3                                    4.681e-01  -1.188
## PubDate.wkday.fctr4                                    4.615e-01  -1.384
## PubDate.wkday.fctr5                                    4.678e-01  -1.592
## PubDate.wkday.fctr6                                    3.952e-01  -1.776
## S.ndgts.log                                            1.266e-01  -2.287
## H.P.recap.colon                                        4.804e-01   2.952
## H.P.no.comment.colon                                   9.615e-01   2.412
## H.npnct11.log                                          1.740e-01   2.159
## S.T.make                                               5.180e-01  -1.535
## A.nwrds.unq.log                                        4.532e-01  -2.226
## S.T.state                                              6.903e-01   1.265
## S.T.report                                             8.219e-01  -1.135
## S.T.one                                                5.139e-01  -0.998
## S.T.share                                              8.964e-01  -1.763
## S.T.show                                               7.591e-01  -1.199
## PubDate.last10.log                                     9.631e-02   1.619
## S.npnct04.log                                          5.099e-01  -1.716
## `PubDate.second.fctr(14.8,29.5]`                       1.457e-01  -0.480
## `PubDate.second.fctr(29.5,44.2]`                       1.445e-01  -0.176
## `PubDate.second.fctr(44.2,59.1]`                       1.464e-01  -1.447
## S.npnct08.log                                          5.910e-01   0.222
## A.nwrds.log                                            6.672e-01  -1.010
## A.T.will                                               6.210e-01  -1.609
## .rnorm                                                 5.196e-02  -1.386
## A.T.can                                                5.926e-01  -1.715
## `PubDate.hour.fctr(7.67,15.3]`                         1.966e-01   0.719
## `PubDate.hour.fctr(15.3,23]`                           1.986e-01   1.197
## S.npnct11.log                                          1.202e-01  -1.009
## S.T.said                                               6.862e-01   2.361
## H.T.week                                               6.789e-01  -1.541
## H.T.say                                                3.655e-01  -0.931
## `PubDate.minute.fctr(14.8,29.5]`                       1.503e-01   0.069
## `PubDate.minute.fctr(29.5,44.2]`                       1.470e-01  -1.510
## `PubDate.minute.fctr(44.2,59.1]`                       1.526e-01   0.311
## H.npnct16.log                                          4.709e-01   1.205
## H.ndgts.log                                            1.970e-01   0.624
## H.npnct01.log                                          9.804e-01  -1.171
## A.T.editor                                             1.340e+00  -1.294
## H.T.news                                               7.907e-01  -1.314
## H.T.newyork                                            5.038e-01  -1.852
## H.T.new                                                3.759e-01  -1.085
## H.npnct07.log                                          1.658e-01   0.155
## H.T.art                                                6.131e-01  -0.314
## `PubDate.date.fctr(7,13]`                              1.615e-01   0.328
## `PubDate.date.fctr(13,19]`                             1.609e-01  -0.902
## `PubDate.date.fctr(19,25]`                             1.582e-01  -0.183
## `PubDate.date.fctr(25,31]`                             1.718e-01  -0.179
## A.T.compani                                            7.052e-01  -1.314
## H.T.get                                                3.828e-01   1.669
## S.npnct01.log                                          1.157e+00   1.689
## A.T.intern                                             1.435e+00   0.488
## H.npnct12.log                                          2.530e-01   0.328
## PubDate.last1.log                                      3.659e-02  -0.615
## H.npnct13.log                                          1.640e-01  -0.607
## H.T.china                                              5.723e-01  -0.227
## S.npnct12.log                                          1.678e-01  -0.999
## H.nwrds.log                                            4.903e-01   1.374
## A.T.take                                               7.889e-01  -0.360
## A.T.time                                               7.370e-01  -1.220
## S.T.day                                                8.561e-01  -0.174
## H.T.bank                                               3.708e-01   0.718
## H.T.big                                                3.856e-01  -0.336
## H.T.take                                               3.283e-01  -0.826
## A.T.word                                               9.886e-01  -1.050
## H.T.day                                                4.844e-01  -0.348
## PubDate.wkend                                          3.598e-01  -0.101
## A.T.week                                               7.289e-01  -0.551
## H.T.busi                                               8.113e-01  -1.291
## H.T.X2014                                              8.100e-01  -0.406
## A.nchrs.log                                            4.077e-01   1.011
## A.T.appear                                             9.884e-01  -0.388
## S.T.highlight                                          2.303e+00   0.291
## H.T.obama                                              4.141e-01   0.011
## H.T.billion                                            5.192e-01   0.649
## S.npnct14.log                                          1.633e+00   0.296
## H.T.time                                               2.893e-01  -0.016
## A.T.senat                                              8.166e-01  -0.217
## S.npnct15.log                                          4.326e-01  -0.822
## PubDate.last100.log                                    3.565e-02  -0.144
## S.T.obama                                              8.170e-01  -0.322
## H.T.make                                               2.910e-01  -0.700
## H.npnct28.log                                          1.623e+00  -0.334
## S.T.presid                                             7.246e-01  -0.380
## A.T.new                                                6.247e-01  -0.553
## A.T.year                                               7.049e-01  -0.092
## H.T.pictur                                             6.725e-01   0.238
## S.npnct06.log                                          8.236e-01   1.246
## S.P.metropolitan.diary.colon                           7.385e-01  -0.468
## S.T.first                                              8.806e-01   0.038
## A.npnct16.log                                          1.002e+00   0.434
## H.T.ebola                                              2.636e-01  -0.862
## S.T.fashion                                            2.749e+00  -0.716
## A.T.scene                                              3.107e+00  -1.148
## S.T.tribun                                             1.978e+03  -0.021
## H.T.test                                               5.580e-01   0.342
## H.P.fashion.week                                       9.414e+02  -0.013
## H.npnct14.log                                          1.627e+03  -0.012
## H.T.deal                                               4.714e-01  -0.630
## H.P.first.draft                                        1.972e+03  -0.008
## S.npnct28.log                                          1.691e+03  -0.009
## H.P.daily.clip.report                                  2.241e+03  -0.008
## H.P.today.in.smallbusiness                             2.218e+03  -0.007
## H.P.verbatim.colon                                     2.919e+03  -0.005
## H.npnct02.log                                          3.460e+03  -0.005
## S.P.first.draft                                        3.643e+03  -0.004
## S.npnct03.log                                          8.779e+03  -0.003
## S.npnct20.log                                          6.430e+03  -0.004
## H.P.quandary                                           5.165e+03   0.004
## A.npnct18.log                                          6.681e+03  -0.004
## S.P.year.colon                                         3.408e+03  -0.003
## H.P.on.this.day                                        4.445e+03  -0.004
## H.npnct05.log                                          1.604e+00   0.481
## S.npnct07.log                                          9.526e+03  -0.002
## H.P.s.notebook                                         9.142e-01   2.022
## S.P.fashion.week                                       1.026e+03  -0.012
##                                                       Pr(>|z|)    
## (Intercept)                                           0.164039    
## WordCount.log                                          < 2e-16 ***
## `myCategory.fctrForeign#World#Asia Pacific`           1.54e-11 ***
## `myCategory.fctr#Multimedia#`                         3.16e-09 ***
## `myCategory.fctrCulture#Arts#`                         < 2e-16 ***
## `myCategory.fctrBusiness#Business Day#Dealbook`        < 2e-16 ***
## myCategory.fctrmyOther                                0.992850    
## `myCategory.fctrBusiness#Technology#`                 5.83e-14 ***
## `myCategory.fctrBusiness#Crosswords/Games#`           0.219018    
## `myCategory.fctrTStyle##`                              < 2e-16 ***
## `myCategory.fctrForeign#World#`                       0.985064    
## `myCategory.fctrOpEd#Opinion#`                        0.002232 ** 
## `myCategory.fctrStyles##Fashion`                      1.15e-06 ***
## `myCategory.fctr#Opinion#Room For Debate`              < 2e-16 ***
## `myCategory.fctr#U.S.#Education`                      0.978210    
## `myCategory.fctr##`                                    < 2e-16 ***
## `myCategory.fctrMetro#N.Y. / Region#`                 0.000366 ***
## `myCategory.fctrBusiness#Business Day#Small Business` 2.38e-13 ***
## `myCategory.fctrStyles#U.S.#`                         0.031055 *  
## `myCategory.fctrTravel#Travel#`                       1.87e-05 ***
## `myCategory.fctr#Opinion#The Public Editor`           0.458403    
## H.P.readers.respond                                   5.71e-13 ***
## A.npnct19.log                                         1.51e-09 ***
## H.nchrs.log                                           1.39e-06 ***
## H.npnct19.log                                         2.69e-06 ***
## A.npnct13.log                                         1.74e-05 ***
## S.nuppr.log                                           3.55e-05 ***
## H.npnct15.log                                         0.000746 ***
## H.npnct08.log                                         0.002616 ** 
## H.T.read                                              0.003237 ** 
## A.T.newyork                                           0.014348 *  
## H.T.polit                                             0.002115 ** 
## H.nuppr.log                                           0.002962 ** 
## H.T.word                                              0.009241 ** 
## PubDate.wkday.fctr1                                   0.237619    
## PubDate.wkday.fctr2                                   0.071223 .  
## PubDate.wkday.fctr3                                   0.234835    
## PubDate.wkday.fctr4                                   0.166376    
## PubDate.wkday.fctr5                                   0.111339    
## PubDate.wkday.fctr6                                   0.075652 .  
## S.ndgts.log                                           0.022176 *  
## H.P.recap.colon                                       0.003158 ** 
## H.P.no.comment.colon                                  0.015876 *  
## H.npnct11.log                                         0.030873 *  
## S.T.make                                              0.124868    
## A.nwrds.unq.log                                       0.026039 *  
## S.T.state                                             0.205724    
## S.T.report                                            0.256329    
## S.T.one                                               0.318068    
## S.T.share                                             0.077957 .  
## S.T.show                                              0.230486    
## PubDate.last10.log                                    0.105506    
## S.npnct04.log                                         0.086206 .  
## `PubDate.second.fctr(14.8,29.5]`                      0.631057    
## `PubDate.second.fctr(29.5,44.2]`                      0.860191    
## `PubDate.second.fctr(44.2,59.1]`                      0.148003    
## S.npnct08.log                                         0.824217    
## A.nwrds.log                                           0.312667    
## A.T.will                                              0.107527    
## .rnorm                                                0.165874    
## A.T.can                                               0.086434 .  
## `PubDate.hour.fctr(7.67,15.3]`                        0.472056    
## `PubDate.hour.fctr(15.3,23]`                          0.231325    
## S.npnct11.log                                         0.313121    
## S.T.said                                              0.018212 *  
## H.T.week                                              0.123203    
## H.T.say                                               0.351917    
## `PubDate.minute.fctr(14.8,29.5]`                      0.944935    
## `PubDate.minute.fctr(29.5,44.2]`                      0.131061    
## `PubDate.minute.fctr(44.2,59.1]`                      0.755474    
## H.npnct16.log                                         0.228329    
## H.ndgts.log                                           0.532817    
## H.npnct01.log                                         0.241504    
## A.T.editor                                            0.195598    
## H.T.news                                              0.188957    
## H.T.newyork                                           0.063994 .  
## H.T.new                                               0.277896    
## H.npnct07.log                                         0.876451    
## H.T.art                                               0.753450    
## `PubDate.date.fctr(7,13]`                             0.742675    
## `PubDate.date.fctr(13,19]`                            0.367058    
## `PubDate.date.fctr(19,25]`                            0.855189    
## `PubDate.date.fctr(25,31]`                            0.857941    
## A.T.compani                                           0.188848    
## H.T.get                                               0.095127 .  
## S.npnct01.log                                         0.091278 .  
## A.T.intern                                            0.625796    
## H.npnct12.log                                         0.743184    
## PubDate.last1.log                                     0.538595    
## H.npnct13.log                                         0.543595    
## H.T.china                                             0.820320    
## S.npnct12.log                                         0.317926    
## H.nwrds.log                                           0.169368    
## A.T.take                                              0.718477    
## A.T.time                                              0.222624    
## S.T.day                                               0.861694    
## H.T.bank                                              0.472465    
## H.T.big                                               0.736883    
## H.T.take                                              0.408697    
## A.T.word                                              0.293938    
## H.T.day                                               0.727752    
## PubDate.wkend                                         0.919355    
## A.T.week                                              0.581964    
## H.T.busi                                              0.196728    
## H.T.X2014                                             0.684430    
## A.nchrs.log                                           0.312146    
## A.T.appear                                            0.698201    
## S.T.highlight                                         0.771218    
## H.T.obama                                             0.991108    
## H.T.billion                                           0.516144    
## S.npnct14.log                                         0.767333    
## H.T.time                                              0.987471    
## A.T.senat                                             0.828483    
## S.npnct15.log                                         0.410987    
## PubDate.last100.log                                   0.885593    
## S.T.obama                                             0.747344    
## H.T.make                                              0.483889    
## H.npnct28.log                                         0.738181    
## S.T.presid                                            0.703961    
## A.T.new                                               0.580106    
## A.T.year                                              0.927042    
## H.T.pictur                                            0.811882    
## S.npnct06.log                                         0.212756    
## S.P.metropolitan.diary.colon                          0.640119    
## S.T.first                                             0.969688    
## A.npnct16.log                                         0.664010    
## H.T.ebola                                             0.388555    
## S.T.fashion                                           0.474162    
## A.T.scene                                             0.250920    
## S.T.tribun                                            0.983464    
## H.T.test                                              0.732254    
## H.P.fashion.week                                      0.989374    
## H.npnct14.log                                         0.990278    
## H.T.deal                                              0.528868    
## H.P.first.draft                                       0.993436    
## S.npnct28.log                                         0.992880    
## H.P.daily.clip.report                                 0.993920    
## H.P.today.in.smallbusiness                            0.994378    
## H.P.verbatim.colon                                    0.995928    
## H.npnct02.log                                         0.995927    
## S.P.first.draft                                       0.996644    
## S.npnct03.log                                         0.997280    
## S.npnct20.log                                         0.996949    
## H.P.quandary                                          0.996630    
## A.npnct18.log                                         0.996688    
## S.P.year.colon                                        0.997649    
## H.P.on.this.day                                       0.997108    
## H.npnct05.log                                         0.630849    
## S.npnct07.log                                         0.998005    
## H.P.s.notebook                                        0.043184 *  
## S.P.fashion.week                                      0.990176    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5900.1  on 6531  degrees of freedom
## Residual deviance: 2644.0  on 6381  degrees of freedom
## AIC: 2946
## 
## Number of Fisher Scoring iterations: 19
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_clusters_files/figure-html/fit.data.training_0-5.png) 

```
##    threshold   f.score
## 1        0.0 0.2866885
## 2        0.1 0.6699346
## 3        0.2 0.7377882
## 4        0.3 0.7522620
## 5        0.4 0.7493113
## 6        0.5 0.7369458
## 7        0.6 0.7140633
## 8        0.7 0.6636821
## 9        0.8 0.5770654
## 10       0.9 0.4036958
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Final.glm.N
## 1            N                             5084
## 2            Y                              220
##   Popular.fctr.predict.Final.glm.Y
## 1                              355
## 2                              873
##          Prediction
## Reference    N    Y
##         N 5084  355
##         Y  220  873
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.119718e-01   6.989588e-01   9.048389e-01   9.187344e-01   8.326699e-01 
## AccuracyPValue  McnemarPValue 
##   3.713017e-77   2.294534e-08
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](NYTBlogs_clusters_files/figure-html/fit.data.training_0-6.png) 

```
##    model_id model_method
## 1 Final.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    feats
## 1 WordCount.log, myCategory.fctr, H.P.readers.respond, A.npnct19.log, H.nchrs.log, H.npnct19.log, A.npnct13.log, S.nuppr.log, H.npnct15.log, H.npnct08.log, H.T.read, A.T.newyork, H.T.polit, H.nuppr.log, H.T.word, PubDate.wkday.fctr, S.ndgts.log, H.P.recap.colon, H.P.no.comment.colon, H.npnct11.log, S.T.make, A.nwrds.unq.log, S.T.state, S.T.report, S.T.one, S.T.share, S.T.show, PubDate.last10.log, S.npnct04.log, PubDate.second.fctr, S.npnct08.log, A.nwrds.log, A.T.will, .rnorm, A.T.can, PubDate.hour.fctr, S.npnct11.log, S.T.said, H.T.week, H.T.say, PubDate.minute.fctr, H.npnct16.log, H.ndgts.log, H.npnct01.log, A.T.editor, H.T.news, H.T.newyork, H.T.new, H.npnct07.log, H.T.art, PubDate.date.fctr, A.T.compani, H.T.get, S.npnct01.log, A.T.intern, H.npnct12.log, PubDate.last1.log, H.npnct13.log, H.T.china, S.npnct12.log, H.nwrds.log, A.T.take, A.T.time, S.T.day, H.T.bank, H.T.big, H.T.take, A.T.word, H.T.day, PubDate.wkend, A.T.week, H.T.busi, H.T.X2014, A.nchrs.log, A.T.appear, S.T.highlight, H.T.obama, H.T.billion, S.npnct14.log, H.T.time, A.T.senat, S.npnct15.log, PubDate.last100.log, S.T.obama, H.T.make, H.npnct28.log, S.T.presid, A.T.new, A.T.year, H.T.pictur, S.npnct06.log, S.P.metropolitan.diary.colon, S.T.first, A.npnct16.log, H.T.ebola, S.T.fashion, A.T.scene, S.T.tribun, H.T.test, H.P.fashion.week, H.npnct14.log, H.T.deal, H.P.first.draft, S.npnct28.log, H.P.daily.clip.report, H.P.today.in.smallbusiness, H.P.verbatim.colon, H.npnct02.log, S.P.first.draft, S.npnct03.log, S.npnct20.log, H.P.quandary, A.npnct18.log, S.P.year.colon, H.P.on.this.day, H.npnct05.log, S.npnct07.log, H.P.s.notebook, S.P.fashion.week
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     12.109                 6.196
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9516157                    0.3        0.752262        0.9087556
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit min.aic.fit
## 1             0.9048389             0.9187344     0.6510039    2946.001
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0113989      0.05091766
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 313.160 332.371  19.211
## 15 fit.data.training          8          1 332.371      NA      NA
```


```r
glb_trnent_df <- glb_get_predictions(df=glb_trnent_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(df = glb_trnent_df, mdl_id =
## glb_fin_mdl_id, : Using default probability threshold: 0.4
```

```r
glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
                                               entity_df=glb_trnent_df)
glb_feats_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_feats_df$importance
print(glb_feats_df)
```

```
##                                                        id   importance
## WordCount.log                               WordCount.log 1.000000e+02
## myCategory.fctr                           myCategory.fctr 7.209376e+01
## H.P.readers.respond                   H.P.readers.respond 5.081905e+01
## A.npnct19.log                               A.npnct19.log 3.893603e+01
## H.nchrs.log                                   H.nchrs.log 3.635733e+01
## H.npnct19.log                               H.npnct19.log 3.367254e+01
## A.npnct13.log                               A.npnct13.log 3.078002e+01
## S.nuppr.log                                   S.nuppr.log 2.879518e+01
## H.npnct15.log                               H.npnct15.log 2.821906e+01
## H.npnct08.log                               H.npnct08.log 2.401124e+01
## H.T.read                                         H.T.read 2.378859e+01
## A.T.newyork                                   A.T.newyork 2.336015e+01
## H.T.polit                                       H.T.polit 2.244859e+01
## H.nuppr.log                                   H.nuppr.log 2.205468e+01
## H.T.word                                         H.T.word 1.954393e+01
## PubDate.wkday.fctr                     PubDate.wkday.fctr 1.903080e+01
## S.ndgts.log                                   S.ndgts.log 1.850859e+01
## H.P.recap.colon                           H.P.recap.colon 1.836394e+01
## H.P.no.comment.colon                 H.P.no.comment.colon 1.833180e+01
## H.npnct11.log                               H.npnct11.log 1.779283e+01
## S.T.make                                         S.T.make 1.534580e+01
## A.nwrds.unq.log                           A.nwrds.unq.log 1.490474e+01
## S.T.state                                       S.T.state 1.443146e+01
## S.T.report                                     S.T.report 1.398733e+01
## S.T.one                                           S.T.one 1.374995e+01
## S.T.share                                       S.T.share 1.356646e+01
## S.T.show                                         S.T.show 1.315591e+01
## PubDate.last10.log                     PubDate.last10.log 1.314498e+01
## S.npnct04.log                               S.npnct04.log 1.289273e+01
## PubDate.second.fctr                   PubDate.second.fctr 1.202077e+01
## S.npnct08.log                               S.npnct08.log 1.201425e+01
## A.nwrds.log                                   A.nwrds.log 1.200285e+01
## A.T.will                                         A.T.will 1.188661e+01
## .rnorm                                             .rnorm 1.117476e+01
## A.T.can                                           A.T.can 1.105762e+01
## PubDate.hour.fctr                       PubDate.hour.fctr 1.096583e+01
## S.npnct11.log                               S.npnct11.log 1.085897e+01
## S.T.said                                         S.T.said 1.064914e+01
## H.T.week                                         H.T.week 1.043249e+01
## H.T.say                                           H.T.say 1.025337e+01
## PubDate.minute.fctr                   PubDate.minute.fctr 1.013617e+01
## H.npnct16.log                               H.npnct16.log 1.012826e+01
## H.ndgts.log                                   H.ndgts.log 1.011850e+01
## H.npnct01.log                               H.npnct01.log 9.941221e+00
## A.T.editor                                     A.T.editor 9.251126e+00
## H.T.news                                         H.T.news 9.183951e+00
## H.T.newyork                                   H.T.newyork 8.989821e+00
## H.T.new                                           H.T.new 8.839602e+00
## H.npnct07.log                               H.npnct07.log 8.668352e+00
## H.T.art                                           H.T.art 8.637437e+00
## PubDate.date.fctr                       PubDate.date.fctr 8.224594e+00
## A.T.compani                                   A.T.compani 7.930807e+00
## H.T.get                                           H.T.get 7.893693e+00
## S.npnct01.log                               S.npnct01.log 7.444775e+00
## A.T.intern                                     A.T.intern 7.341904e+00
## H.npnct12.log                               H.npnct12.log 6.907483e+00
## PubDate.last1.log                       PubDate.last1.log 6.802269e+00
## H.npnct13.log                               H.npnct13.log 6.670123e+00
## H.T.china                                       H.T.china 6.528943e+00
## S.npnct12.log                               S.npnct12.log 6.513816e+00
## H.nwrds.log                                   H.nwrds.log 6.456493e+00
## A.T.take                                         A.T.take 6.233315e+00
## A.T.time                                         A.T.time 6.126992e+00
## S.T.day                                           S.T.day 5.942272e+00
## H.T.bank                                         H.T.bank 5.837283e+00
## H.T.big                                           H.T.big 5.817428e+00
## H.T.take                                         H.T.take 5.634194e+00
## A.T.word                                         A.T.word 5.460452e+00
## H.T.day                                           H.T.day 5.186099e+00
## PubDate.wkend                               PubDate.wkend 5.162440e+00
## A.T.week                                         A.T.week 5.150546e+00
## H.T.busi                                         H.T.busi 5.118511e+00
## H.T.X2014                                       H.T.X2014 4.654351e+00
## A.nchrs.log                                   A.nchrs.log 4.611780e+00
## A.T.appear                                     A.T.appear 4.238918e+00
## S.T.highlight                               S.T.highlight 4.226495e+00
## H.T.obama                                       H.T.obama 3.951640e+00
## H.T.billion                                   H.T.billion 3.898066e+00
## S.npnct14.log                               S.npnct14.log 3.890611e+00
## H.T.time                                         H.T.time 3.528210e+00
## A.T.senat                                       A.T.senat 3.218199e+00
## S.npnct15.log                               S.npnct15.log 3.200871e+00
## PubDate.last100.log                   PubDate.last100.log 2.899121e+00
## S.T.obama                                       S.T.obama 2.693325e+00
## H.T.make                                         H.T.make 2.653710e+00
## H.npnct28.log                               H.npnct28.log 2.542638e+00
## S.T.presid                                     S.T.presid 2.497619e+00
## A.T.new                                           A.T.new 2.017300e+00
## A.T.year                                         A.T.year 1.915103e+00
## H.T.pictur                                     H.T.pictur 1.665041e+00
## S.npnct06.log                               S.npnct06.log 1.563100e+00
## S.P.metropolitan.diary.colon S.P.metropolitan.diary.colon 8.185782e-01
## S.T.first                                       S.T.first 5.931209e-01
## A.npnct16.log                               A.npnct16.log 5.306992e-01
## H.T.ebola                                       H.T.ebola 5.241256e-01
## S.T.fashion                                   S.T.fashion 1.501731e-01
## A.T.scene                                       A.T.scene 1.351182e-01
## S.T.tribun                                     S.T.tribun 1.203763e-01
## H.T.test                                         H.T.test 1.077281e-01
## H.P.fashion.week                         H.P.fashion.week 8.950571e-02
## H.npnct14.log                               H.npnct14.log 7.654941e-02
## H.T.deal                                         H.T.deal 5.179985e-02
## H.P.first.draft                           H.P.first.draft 4.237400e-02
## S.npnct28.log                               S.npnct28.log 4.126089e-02
## H.P.daily.clip.report               H.P.daily.clip.report 3.602811e-02
## H.P.today.in.smallbusiness     H.P.today.in.smallbusiness 2.825282e-02
## H.P.verbatim.colon                     H.P.verbatim.colon 1.699712e-02
## H.npnct02.log                               H.npnct02.log 1.350933e-02
## S.P.first.draft                           S.P.first.draft 1.337679e-02
## S.npnct03.log                               S.npnct03.log 1.100069e-02
## S.npnct20.log                               S.npnct20.log 1.087710e-02
## H.P.quandary                                 H.P.quandary 1.025645e-02
## A.npnct18.log                               A.npnct18.log 9.131873e-03
## S.P.year.colon                             S.P.year.colon 7.061198e-03
## H.P.on.this.day                           H.P.on.this.day 6.324980e-03
## H.npnct05.log                               H.npnct05.log 3.595486e-03
## S.npnct07.log                               S.npnct07.log 1.720848e-03
## H.P.s.notebook                             H.P.s.notebook 8.198403e-05
## S.P.fashion.week                         S.P.fashion.week 0.000000e+00
## A.ndgts.log                                   A.ndgts.log           NA
## A.npnct01.log                               A.npnct01.log           NA
## A.npnct02.log                               A.npnct02.log           NA
## A.npnct03.log                               A.npnct03.log           NA
## A.npnct04.log                               A.npnct04.log           NA
## A.npnct05.log                               A.npnct05.log           NA
## A.npnct06.log                               A.npnct06.log           NA
## A.npnct07.log                               A.npnct07.log           NA
## A.npnct08.log                               A.npnct08.log           NA
## A.npnct09.log                               A.npnct09.log           NA
## A.npnct10.log                               A.npnct10.log           NA
## A.npnct11.log                               A.npnct11.log           NA
## A.npnct12.log                               A.npnct12.log           NA
## A.npnct14.log                               A.npnct14.log           NA
## A.npnct15.log                               A.npnct15.log           NA
## A.npnct17.log                               A.npnct17.log           NA
## A.npnct20.log                               A.npnct20.log           NA
## A.npnct21.log                               A.npnct21.log           NA
## A.npnct22.log                               A.npnct22.log           NA
## A.npnct23.log                               A.npnct23.log           NA
## A.npnct24.log                               A.npnct24.log           NA
## A.npnct25.log                               A.npnct25.log           NA
## A.npnct26.log                               A.npnct26.log           NA
## A.npnct27.log                               A.npnct27.log           NA
## A.npnct28.log                               A.npnct28.log           NA
## A.npnct29.log                               A.npnct29.log           NA
## A.npnct30.log                               A.npnct30.log           NA
## A.nuppr.log                                   A.nuppr.log           NA
## A.P.daily.clip.report               A.P.daily.clip.report           NA
## A.P.fashion.week                         A.P.fashion.week           NA
## A.P.first.draft                           A.P.first.draft           NA
## A.P.http                                         A.P.http           NA
## A.P.metropolitan.diary.colon A.P.metropolitan.diary.colon           NA
## A.P.year.colon                             A.P.year.colon           NA
## A.T.day                                           A.T.day           NA
## A.T.fashion                                   A.T.fashion           NA
## A.T.obama                                       A.T.obama           NA
## A.T.one                                           A.T.one           NA
## A.T.photo                                       A.T.photo           NA
## A.T.report                                     A.T.report           NA
## A.T.said                                         A.T.said           NA
## A.T.state                                       A.T.state           NA
## clusterid                                       clusterid           NA
## H.npnct03.log                               H.npnct03.log           NA
## H.npnct04.log                               H.npnct04.log           NA
## H.npnct06.log                               H.npnct06.log           NA
## H.npnct09.log                               H.npnct09.log           NA
## H.npnct10.log                               H.npnct10.log           NA
## H.npnct17.log                               H.npnct17.log           NA
## H.npnct18.log                               H.npnct18.log           NA
## H.npnct20.log                               H.npnct20.log           NA
## H.npnct21.log                               H.npnct21.log           NA
## H.npnct22.log                               H.npnct22.log           NA
## H.npnct23.log                               H.npnct23.log           NA
## H.npnct24.log                               H.npnct24.log           NA
## H.npnct25.log                               H.npnct25.log           NA
## H.npnct26.log                               H.npnct26.log           NA
## H.npnct27.log                               H.npnct27.log           NA
## H.npnct29.log                               H.npnct29.log           NA
## H.npnct30.log                               H.npnct30.log           NA
## H.nwrds.unq.log                           H.nwrds.unq.log           NA
## H.P.http                                         H.P.http           NA
## H.P.today.in.politic                 H.P.today.in.politic           NA
## H.P.what.we.are                           H.P.what.we.are           NA
## H.P.year.colon                             H.P.year.colon           NA
## H.T.daili                                       H.T.daili           NA
## H.T.fashion                                   H.T.fashion           NA
## H.T.first                                       H.T.first           NA
## H.T.morn                                         H.T.morn           NA
## H.T.report                                     H.T.report           NA
## H.T.today                                       H.T.today           NA
## H.T.X2015                                       H.T.X2015           NA
## Popular                                           Popular           NA
## Popular.fctr                                 Popular.fctr           NA
## PubDate.last1                               PubDate.last1           NA
## PubDate.last10                             PubDate.last10           NA
## PubDate.last100                           PubDate.last100           NA
## PubDate.month.fctr                     PubDate.month.fctr           NA
## PubDate.POSIX                               PubDate.POSIX           NA
## PubDate.year.fctr                       PubDate.year.fctr           NA
## PubDate.zoo                                   PubDate.zoo           NA
## S.nchrs.log                                   S.nchrs.log           NA
## S.npnct02.log                               S.npnct02.log           NA
## S.npnct05.log                               S.npnct05.log           NA
## S.npnct09.log                               S.npnct09.log           NA
## S.npnct10.log                               S.npnct10.log           NA
## S.npnct13.log                               S.npnct13.log           NA
## S.npnct16.log                               S.npnct16.log           NA
## S.npnct17.log                               S.npnct17.log           NA
## S.npnct18.log                               S.npnct18.log           NA
## S.npnct19.log                               S.npnct19.log           NA
## S.npnct21.log                               S.npnct21.log           NA
## S.npnct22.log                               S.npnct22.log           NA
## S.npnct23.log                               S.npnct23.log           NA
## S.npnct24.log                               S.npnct24.log           NA
## S.npnct25.log                               S.npnct25.log           NA
## S.npnct26.log                               S.npnct26.log           NA
## S.npnct27.log                               S.npnct27.log           NA
## S.npnct29.log                               S.npnct29.log           NA
## S.npnct30.log                               S.npnct30.log           NA
## S.nwrds.log                                   S.nwrds.log           NA
## S.nwrds.unq.log                           S.nwrds.unq.log           NA
## S.P.daily.clip.report               S.P.daily.clip.report           NA
## S.P.http                                         S.P.http           NA
## S.T.articl                                     S.T.articl           NA
## S.T.can                                           S.T.can           NA
## S.T.compani                                   S.T.compani           NA
## S.T.diari                                       S.T.diari           NA
## S.T.intern                                     S.T.intern           NA
## S.T.new                                           S.T.new           NA
## S.T.newyork                                   S.T.newyork           NA
## S.T.past                                         S.T.past           NA
## S.T.photo                                       S.T.photo           NA
## S.T.scene                                       S.T.scene           NA
## S.T.take                                         S.T.take           NA
## S.T.time                                         S.T.time           NA
## S.T.week                                         S.T.week           NA
## S.T.will                                         S.T.will           NA
## S.T.word                                         S.T.word           NA
## UniqueID                                         UniqueID           NA
## WordCount                                       WordCount           NA
##                                      cor.y exclude.as.feat    cor.y.abs
## WordCount.log                 2.656836e-01           FALSE 2.656836e-01
## myCategory.fctr               1.234541e-02           FALSE 1.234541e-02
## H.P.readers.respond           4.432886e-02           FALSE 4.432886e-02
## A.npnct19.log                 5.482747e-02           FALSE 5.482747e-02
## H.nchrs.log                  -1.710624e-01           FALSE 1.710624e-01
## H.npnct19.log                 1.283641e-01           FALSE 1.283641e-01
## A.npnct13.log                -4.999563e-02           FALSE 4.999563e-02
## S.nuppr.log                  -2.718459e-01           FALSE 2.718459e-01
## H.npnct15.log                -8.273237e-02           FALSE 8.273237e-02
## H.npnct08.log                 5.375262e-02           FALSE 5.375262e-02
## H.T.read                     -3.467043e-02           FALSE 3.467043e-02
## A.T.newyork                  -5.769443e-02           FALSE 5.769443e-02
## H.T.polit                    -3.062866e-02           FALSE 3.062866e-02
## H.nuppr.log                  -1.278085e-01           FALSE 1.278085e-01
## H.T.word                     -1.382927e-02           FALSE 1.382927e-02
## PubDate.wkday.fctr           -3.980129e-02           FALSE 3.980129e-02
## S.ndgts.log                  -1.242046e-01           FALSE 1.242046e-01
## H.P.recap.colon               9.008096e-02           FALSE 9.008096e-02
## H.P.no.comment.colon          6.074669e-02           FALSE 6.074669e-02
## H.npnct11.log                 1.333613e-02           FALSE 1.333613e-02
## S.T.make                      3.924228e-02           FALSE 3.924228e-02
## A.nwrds.unq.log              -2.261526e-01           FALSE 2.261526e-01
## S.T.state                     9.894250e-03           FALSE 9.894250e-03
## S.T.report                   -4.746396e-02           FALSE 4.746396e-02
## S.T.one                       9.592876e-03           FALSE 9.592876e-03
## S.T.share                    -5.076046e-02           FALSE 5.076046e-02
## S.T.show                     -4.218975e-02           FALSE 4.218975e-02
## PubDate.last10.log            4.931702e-02           FALSE 4.931702e-02
## S.npnct04.log                -6.294642e-02           FALSE 6.294642e-02
## PubDate.second.fctr          -1.187946e-02           FALSE 1.187946e-02
## S.npnct08.log                -3.372706e-03           FALSE 3.372706e-03
## A.nwrds.log                   1.216578e-01           FALSE 1.216578e-01
## A.T.will                     -3.859405e-02           FALSE 3.859405e-02
## .rnorm                       -8.244230e-03           FALSE 8.244230e-03
## A.T.can                       3.032288e-02           FALSE 3.032288e-02
## PubDate.hour.fctr             1.354368e-01           FALSE 1.354368e-01
## S.npnct11.log                -9.158156e-02           FALSE 9.158156e-02
## S.T.said                      1.801374e-02           FALSE 1.801374e-02
## H.T.week                     -6.991953e-02           FALSE 6.991953e-02
## H.T.say                      -9.960773e-03           FALSE 9.960773e-03
## PubDate.minute.fctr          -3.407385e-02           FALSE 3.407385e-02
## H.npnct16.log                 3.039622e-02           FALSE 3.039622e-02
## H.ndgts.log                  -1.196633e-01           FALSE 1.196633e-01
## H.npnct01.log                 2.271577e-02           FALSE 2.271577e-02
## A.T.editor                   -4.875955e-02           FALSE 4.875955e-02
## H.T.news                     -4.415284e-02           FALSE 4.415284e-02
## H.T.newyork                  -5.717575e-02           FALSE 5.717575e-02
## H.T.new                      -4.327803e-02           FALSE 4.327803e-02
## H.npnct07.log                -1.201741e-02           FALSE 1.201741e-02
## H.T.art                      -3.280483e-02           FALSE 3.280483e-02
## PubDate.date.fctr            -1.164756e-02           FALSE 1.164756e-02
## A.T.compani                  -4.732205e-02           FALSE 4.732205e-02
## H.T.get                       3.312638e-02           FALSE 3.312638e-02
## S.npnct01.log                 3.093101e-02           FALSE 3.093101e-02
## A.T.intern                   -6.065381e-02           FALSE 6.065381e-02
## H.npnct12.log                -1.305305e-02           FALSE 1.305305e-02
## PubDate.last1.log             4.635751e-02           FALSE 4.635751e-02
## H.npnct13.log                -2.524770e-02           FALSE 2.524770e-02
## H.T.china                    -3.144808e-02           FALSE 3.144808e-02
## S.npnct12.log                -3.638891e-02           FALSE 3.638891e-02
## H.nwrds.log                   1.404401e-01           FALSE 1.404401e-01
## A.T.take                     -2.287870e-02           FALSE 2.287870e-02
## A.T.time                     -5.456053e-02           FALSE 5.456053e-02
## S.T.day                      -4.209616e-02           FALSE 4.209616e-02
## H.T.bank                     -9.989139e-03           FALSE 9.989139e-03
## H.T.big                      -1.390748e-02           FALSE 1.390748e-02
## H.T.take                     -9.276608e-04           FALSE 9.276608e-04
## A.T.word                     -4.818014e-02           FALSE 4.818014e-02
## H.T.day                      -6.029849e-02           FALSE 6.029849e-02
## PubDate.wkend                 1.067288e-01           FALSE 1.067288e-01
## A.T.week                     -8.570201e-02           FALSE 8.570201e-02
## H.T.busi                     -4.901905e-02           FALSE 4.901905e-02
## H.T.X2014                    -4.497745e-02           FALSE 4.497745e-02
## A.nchrs.log                  -2.245488e-01           FALSE 2.245488e-01
## A.T.appear                   -3.949035e-02           FALSE 3.949035e-02
## S.T.highlight                -6.283750e-02           FALSE 6.283750e-02
## H.T.obama                    -9.907543e-03           FALSE 9.907543e-03
## H.T.billion                  -2.949817e-02           FALSE 2.949817e-02
## S.npnct14.log                -2.121844e-02           FALSE 2.121844e-02
## H.T.time                     -2.527450e-03           FALSE 2.527450e-03
## A.T.senat                    -4.140312e-02           FALSE 4.140312e-02
## S.npnct15.log                -6.770952e-02           FALSE 6.770952e-02
## PubDate.last100.log          -7.663322e-03           FALSE 7.663322e-03
## S.T.obama                    -1.933422e-02           FALSE 1.933422e-02
## H.T.make                      1.430572e-02           FALSE 1.430572e-02
## H.npnct28.log                -8.917338e-02           FALSE 8.917338e-02
## S.T.presid                   -2.581591e-03           FALSE 2.581591e-03
## A.T.new                      -2.779223e-02           FALSE 2.779223e-02
## A.T.year                     -3.497471e-02           FALSE 3.497471e-02
## H.T.pictur                   -3.993172e-02           FALSE 3.993172e-02
## S.npnct06.log                -2.389145e-02           FALSE 2.389145e-02
## S.P.metropolitan.diary.colon -2.841404e-02           FALSE 2.841404e-02
## S.T.first                    -4.494610e-02           FALSE 4.494610e-02
## A.npnct16.log                -1.587454e-03           FALSE 1.587454e-03
## H.T.ebola                     2.728582e-02           FALSE 2.728582e-02
## S.T.fashion                  -8.344900e-02           FALSE 8.344900e-02
## A.T.scene                    -6.091747e-02           FALSE 6.091747e-02
## S.T.tribun                   -6.880320e-02           FALSE 6.880320e-02
## H.T.test                     -2.057181e-02           FALSE 2.057181e-02
## H.P.fashion.week             -7.632046e-02           FALSE 7.632046e-02
## H.npnct14.log                -6.158577e-02           FALSE 6.158577e-02
## H.T.deal                     -2.559418e-02           FALSE 2.559418e-02
## H.P.first.draft              -4.316253e-02           FALSE 4.316253e-02
## S.npnct28.log                -4.370037e-02           FALSE 4.370037e-02
## H.P.daily.clip.report        -4.388279e-02           FALSE 4.388279e-02
## H.P.today.in.smallbusiness   -4.243051e-02           FALSE 4.243051e-02
## H.P.verbatim.colon           -3.194363e-02           FALSE 3.194363e-02
## H.npnct02.log                -2.001851e-02           FALSE 2.001851e-02
## S.P.first.draft              -2.150663e-02           FALSE 2.150663e-02
## S.npnct03.log                -1.240734e-02           FALSE 1.240734e-02
## S.npnct20.log                -1.923169e-02           FALSE 1.923169e-02
## H.P.quandary                  8.734922e-02           FALSE 8.734922e-02
## A.npnct18.log                -1.271661e-02           FALSE 1.271661e-02
## S.P.year.colon               -1.755336e-02           FALSE 1.755336e-02
## H.P.on.this.day              -2.150663e-02           FALSE 2.150663e-02
## H.npnct05.log                -9.653967e-03           FALSE 9.653967e-03
## S.npnct07.log                -1.214357e-02           FALSE 1.214357e-02
## H.P.s.notebook                7.755542e-03           FALSE 7.755542e-03
## S.P.fashion.week             -7.080716e-02           FALSE 7.080716e-02
## A.ndgts.log                  -1.249484e-01           FALSE 1.249484e-01
## A.npnct01.log                 3.093101e-02           FALSE 3.093101e-02
## A.npnct02.log                -1.451467e-02           FALSE 1.451467e-02
## A.npnct03.log                -1.359260e-02           FALSE 1.359260e-02
## A.npnct04.log                -6.294642e-02           FALSE 6.294642e-02
## A.npnct05.log                           NA           FALSE           NA
## A.npnct06.log                -2.389145e-02           FALSE 2.389145e-02
## A.npnct07.log                -1.214357e-02           FALSE 1.214357e-02
## A.npnct08.log                -4.193476e-03           FALSE 4.193476e-03
## A.npnct09.log                           NA           FALSE           NA
## A.npnct10.log                -5.547032e-03           FALSE 5.547032e-03
## A.npnct11.log                -9.183870e-02           FALSE 9.183870e-02
## A.npnct12.log                -3.760012e-02           FALSE 3.760012e-02
## A.npnct14.log                -2.407715e-02           FALSE 2.407715e-02
## A.npnct15.log                -6.893301e-02           FALSE 6.893301e-02
## A.npnct17.log                -1.457558e-02           FALSE 1.457558e-02
## A.npnct20.log                -1.923169e-02           FALSE 1.923169e-02
## A.npnct21.log                 1.537569e-02           FALSE 1.537569e-02
## A.npnct22.log                           NA           FALSE           NA
## A.npnct23.log                 1.537569e-02           FALSE 1.537569e-02
## A.npnct24.log                -9.890046e-19           FALSE 9.890046e-19
## A.npnct25.log                -5.547032e-03           FALSE 5.547032e-03
## A.npnct26.log                           NA           FALSE           NA
## A.npnct27.log                           NA           FALSE           NA
## A.npnct28.log                -4.373349e-02           FALSE 4.373349e-02
## A.npnct29.log                           NA           FALSE           NA
## A.npnct30.log                           NA           FALSE           NA
## A.nuppr.log                  -2.720962e-01           FALSE 2.720962e-01
## A.P.daily.clip.report        -4.388279e-02           FALSE 4.388279e-02
## A.P.fashion.week             -7.080716e-02           FALSE 7.080716e-02
## A.P.first.draft              -2.150663e-02           FALSE 2.150663e-02
## A.P.http                     -1.294748e-02           FALSE 1.294748e-02
## A.P.metropolitan.diary.colon -2.841404e-02           FALSE 2.841404e-02
## A.P.year.colon               -1.755336e-02           FALSE 1.755336e-02
## A.T.day                      -4.400791e-02           FALSE 4.400791e-02
## A.T.fashion                  -8.349527e-02           FALSE 8.349527e-02
## A.T.obama                    -2.020436e-02           FALSE 2.020436e-02
## A.T.one                       1.048320e-02           FALSE 1.048320e-02
## A.T.photo                    -6.984501e-02           FALSE 6.984501e-02
## A.T.report                   -4.847952e-02           FALSE 4.847952e-02
## A.T.said                      1.831840e-02           FALSE 1.831840e-02
## A.T.state                     1.007193e-02           FALSE 1.007193e-02
## clusterid                               NA           FALSE           NA
## H.npnct03.log                 9.533020e-03           FALSE 9.533020e-03
## H.npnct04.log                -5.126277e-02           FALSE 5.126277e-02
## H.npnct06.log                 3.190718e-02           FALSE 3.190718e-02
## H.npnct09.log                           NA           FALSE           NA
## H.npnct10.log                -5.547032e-03           FALSE 5.547032e-03
## H.npnct17.log                           NA           FALSE           NA
## H.npnct18.log                           NA           FALSE           NA
## H.npnct20.log                -5.547032e-03           FALSE 5.547032e-03
## H.npnct21.log                           NA           FALSE           NA
## H.npnct22.log                           NA           FALSE           NA
## H.npnct23.log                           NA           FALSE           NA
## H.npnct24.log                -9.890046e-19           FALSE 9.890046e-19
## H.npnct25.log                           NA           FALSE           NA
## H.npnct26.log                           NA           FALSE           NA
## H.npnct27.log                           NA           FALSE           NA
## H.npnct29.log                           NA           FALSE           NA
## H.npnct30.log                           NA           FALSE           NA
## H.nwrds.unq.log              -1.957553e-01           FALSE 1.957553e-01
## H.P.http                                NA           FALSE           NA
## H.P.today.in.politic         -3.733661e-02           FALSE 3.733661e-02
## H.P.what.we.are              -3.775209e-02           FALSE 3.775209e-02
## H.P.year.colon               -7.842875e-02           FALSE 7.842875e-02
## H.T.daili                    -5.852819e-02           FALSE 5.852819e-02
## H.T.fashion                  -8.000421e-02           FALSE 8.000421e-02
## H.T.first                    -4.458885e-02           FALSE 4.458885e-02
## H.T.morn                     -4.838380e-02           FALSE 4.838380e-02
## H.T.report                   -5.934795e-02           FALSE 5.934795e-02
## H.T.today                    -5.831308e-02           FALSE 5.831308e-02
## H.T.X2015                    -6.570743e-02           FALSE 6.570743e-02
## Popular                       1.000000e+00            TRUE 1.000000e+00
## Popular.fctr                            NA            TRUE           NA
## PubDate.last1                 3.592267e-02            TRUE 3.592267e-02
## PubDate.last10                5.398093e-02            TRUE 5.398093e-02
## PubDate.last100               3.989229e-02            TRUE 3.989229e-02
## PubDate.month.fctr            1.914874e-02            TRUE 1.914874e-02
## PubDate.POSIX                 1.568326e-02            TRUE 1.568326e-02
## PubDate.year.fctr                       NA           FALSE           NA
## PubDate.zoo                   1.568326e-02            TRUE 1.568326e-02
## S.nchrs.log                  -2.246930e-01           FALSE 2.246930e-01
## S.npnct02.log                -5.547032e-03           FALSE 5.547032e-03
## S.npnct05.log                           NA           FALSE           NA
## S.npnct09.log                           NA           FALSE           NA
## S.npnct10.log                -5.547032e-03           FALSE 5.547032e-03
## S.npnct13.log                -5.332519e-02           FALSE 5.332519e-02
## S.npnct16.log                -1.587454e-03           FALSE 1.587454e-03
## S.npnct17.log                           NA           FALSE           NA
## S.npnct18.log                           NA           FALSE           NA
## S.npnct19.log                 5.503894e-02           FALSE 5.503894e-02
## S.npnct21.log                 2.760321e-02           FALSE 2.760321e-02
## S.npnct22.log                           NA           FALSE           NA
## S.npnct23.log                 2.760321e-02           FALSE 2.760321e-02
## S.npnct24.log                -9.890046e-19           FALSE 9.890046e-19
## S.npnct25.log                           NA           FALSE           NA
## S.npnct26.log                           NA           FALSE           NA
## S.npnct27.log                           NA           FALSE           NA
## S.npnct29.log                           NA           FALSE           NA
## S.npnct30.log                           NA           FALSE           NA
## S.nwrds.log                   1.262344e-01           FALSE 1.262344e-01
## S.nwrds.unq.log              -2.343044e-01           FALSE 2.343044e-01
## S.P.daily.clip.report        -4.388279e-02           FALSE 4.388279e-02
## S.P.http                                NA           FALSE           NA
## S.T.articl                   -5.446081e-02           FALSE 5.446081e-02
## S.T.can                       3.062115e-02           FALSE 3.062115e-02
## S.T.compani                  -4.739476e-02           FALSE 4.739476e-02
## S.T.diari                    -6.227998e-02           FALSE 6.227998e-02
## S.T.intern                   -6.932606e-02           FALSE 6.932606e-02
## S.T.new                      -2.833575e-02           FALSE 2.833575e-02
## S.T.newyork                  -5.943875e-02           FALSE 5.943875e-02
## S.T.past                     -4.562700e-02           FALSE 4.562700e-02
## S.T.photo                    -4.269774e-02           FALSE 4.269774e-02
## S.T.scene                    -6.098895e-02           FALSE 6.098895e-02
## S.T.take                     -2.307456e-02           FALSE 2.307456e-02
## S.T.time                     -5.574436e-02           FALSE 5.574436e-02
## S.T.week                     -8.627682e-02           FALSE 8.627682e-02
## S.T.will                     -3.931556e-02           FALSE 3.931556e-02
## S.T.word                     -4.876372e-02           FALSE 4.876372e-02
## UniqueID                      1.182492e-02            TRUE 1.182492e-02
## WordCount                     2.575265e-01            TRUE 2.575265e-01
##                                                cor.high.X   freqRatio
## WordCount.log                                        <NA>    1.300000
## myCategory.fctr                                      <NA>    1.337185
## H.P.readers.respond                                  <NA>  342.789474
## A.npnct19.log                                        <NA>   12.798715
## H.nchrs.log                                          <NA>    1.023810
## H.npnct19.log                                        <NA>   14.995098
## A.npnct13.log                                        <NA>    4.603330
## S.nuppr.log                                          <NA>    1.152620
## H.npnct15.log                                        <NA>    3.914910
## H.npnct08.log                                        <NA>  111.620690
## H.T.read                                             <NA>  179.388889
## A.T.newyork                                          <NA>  103.762712
## H.T.polit                                            <NA>  126.254902
## H.nuppr.log                                          <NA>    1.033930
## H.T.word                                             <NA>  104.096774
## PubDate.wkday.fctr                                   <NA>    1.003268
## S.ndgts.log                                          <NA>   10.511247
## H.P.recap.colon                                      <NA>   93.666667
## H.P.no.comment.colon                                 <NA>  724.777778
## H.npnct11.log                                        <NA>    4.937442
## S.T.make                                             <NA>  273.782609
## A.nwrds.unq.log                                      <NA>    1.128405
## S.T.state                                            <NA>  315.750000
## S.T.report                                           <NA>   78.362500
## S.T.one                                              <NA>  222.892857
## S.T.share                                            <NA>  218.448276
## S.T.show                                             <NA>  274.608696
## PubDate.last10.log                                   <NA>    1.666667
## S.npnct04.log                                        <NA>   28.536364
## PubDate.second.fctr                                  <NA>    1.018204
## S.npnct08.log                                        <NA>  175.486486
## A.nwrds.log                                          <NA>    2.583333
## A.T.will                                             <NA>  112.547170
## .rnorm                                               <NA>    2.000000
## A.T.can                                              <NA>  241.538462
## PubDate.hour.fctr                                    <NA>    1.835040
## S.npnct11.log                                        <NA>    1.660473
## S.T.said                                             <NA>  174.388889
## H.T.week                                             <NA>   53.666667
## H.T.say                                              <NA>  247.461538
## PubDate.minute.fctr                                  <NA>    1.483365
## H.npnct16.log                                        <NA>   96.104478
## H.ndgts.log                                          <NA>   13.616137
## H.npnct01.log                                        <NA>  282.913043
## A.T.editor                                           <NA>  160.225000
## H.T.news                                             <NA>  238.518519
## H.T.newyork                                          <NA>   95.409091
## H.T.new                                              <NA>  116.333333
## H.npnct07.log                                        <NA>    5.437234
## H.T.art                                              <NA>  307.333333
## PubDate.date.fctr                                    <NA>    1.021394
## A.T.compani                                          <NA>  128.541667
## H.T.get                                              <NA>  430.866667
## S.npnct01.log                                        <NA>  309.952381
## A.T.intern                                           <NA>  157.950000
## H.npnct12.log                                        <NA>   13.126638
## PubDate.last1.log                                    <NA>    1.142857
## H.npnct13.log                                        <NA>   22.802326
## H.T.china                                            <NA>  238.555556
## S.npnct12.log                                        <NA>    5.706263
## H.nwrds.log                                          <NA>    1.127273
## A.T.take                                             <NA>  263.125000
## A.T.time                                             <NA>   68.011236
## S.T.day                                              <NA>   88.338028
## H.T.bank                                             <NA>  221.689655
## H.T.big                                              <NA>  403.562500
## H.T.take                                             <NA>  322.250000
## A.T.word                                             <NA>  133.145833
## H.T.day                                              <NA>   86.547945
## PubDate.wkend                                        <NA>    9.095827
## A.T.week                                             <NA>   47.273438
## H.T.busi                                             <NA>  229.428571
## H.T.X2014                                            <NA>  112.824561
## A.nchrs.log                                          <NA>    1.328571
## A.T.appear                                           <NA>  228.821429
## S.T.highlight                                        <NA>  187.500000
## H.T.obama                                            <NA>  229.750000
## H.T.billion                                          <NA>  229.892857
## S.npnct14.log                                        <NA>  203.062500
## H.T.time                                             <NA>  247.538462
## A.T.senat                                            <NA>  316.450000
## S.npnct15.log                                        <NA>   13.647191
## PubDate.last100.log                                  <NA>   25.000000
## S.T.obama                                            <NA>  335.684211
## H.T.make                                             <NA>  322.200000
## H.npnct28.log                                        <NA>   24.123077
## S.T.presid                                           <NA>  241.692308
## A.T.new                                              <NA>  104.052632
## A.T.year                                             <NA>  138.727273
## H.T.pictur                                           <NA>  104.032258
## S.npnct06.log                                        <NA>  115.642857
## S.P.metropolitan.diary.colon                         <NA>   99.492308
## S.T.first                                            <NA>  217.724138
## A.npnct16.log                                        <NA>  434.133333
## H.T.ebola                                            <NA>  293.000000
## S.T.fashion                                          <NA>   59.809524
## A.T.scene                                            <NA>   71.921348
## S.T.tribun                                           <NA>  138.456522
## H.T.test                                             <NA>  280.000000
## H.P.fashion.week                                     <NA>   34.500000
## H.npnct14.log                                        <NA>   52.983471
## H.T.deal                                             <NA>  258.080000
## H.P.first.draft                                      <NA>  107.866667
## S.npnct28.log                                        <NA>  134.791667
## H.P.daily.clip.report                                <NA>  104.354839
## H.P.today.in.smallbusiness                           <NA>  111.620690
## H.P.verbatim.colon                                   <NA>  196.939394
## H.npnct02.log                                        <NA>  501.461538
## S.P.first.draft                                      <NA>  434.466667
## S.npnct03.log                                        <NA> 1305.400000
## S.npnct20.log                                        <NA>  543.333333
## H.P.quandary                                         <NA>  652.200000
## A.npnct18.log                                        <NA> 1631.500000
## S.P.year.colon                                       <NA>  652.200000
## H.P.on.this.day                                      <NA>  434.466667
## H.npnct05.log                                        <NA>  543.333333
## S.npnct07.log                                        <NA> 1631.750000
## H.P.s.notebook                                       <NA>  815.500000
## S.P.fashion.week                                     <NA>   40.081761
## A.ndgts.log                                   S.ndgts.log   10.501022
## A.npnct01.log                               S.npnct01.log  309.952381
## A.npnct02.log                                    A.P.http 1087.500000
## A.npnct03.log                               S.npnct03.log 1087.666667
## A.npnct04.log                               S.npnct04.log   28.536364
## A.npnct05.log                                        <NA>    0.000000
## A.npnct06.log                               S.npnct06.log  115.642857
## A.npnct07.log                               S.npnct07.log 1631.750000
## A.npnct08.log                                        <NA>  170.842105
## A.npnct09.log                                        <NA>    0.000000
## A.npnct10.log                                        <NA> 6531.000000
## A.npnct11.log                               S.npnct11.log    1.660473
## A.npnct12.log                               S.npnct12.log    5.715368
## A.npnct14.log                               A.npnct17.log  196.696970
## A.npnct15.log                               S.npnct15.log   13.482222
## A.npnct17.log                               A.npnct02.log 1087.500000
## A.npnct20.log                               S.npnct20.log  543.333333
## A.npnct21.log                               A.npnct23.log 3264.500000
## A.npnct22.log                                        <NA>    0.000000
## A.npnct23.log                                        <NA> 3264.500000
## A.npnct24.log                                        <NA>    0.000000
## A.npnct25.log                                        <NA> 6531.000000
## A.npnct26.log                                        <NA>    0.000000
## A.npnct27.log                                        <NA>    0.000000
## A.npnct28.log                               S.npnct28.log  126.862745
## A.npnct29.log                                        <NA>    0.000000
## A.npnct30.log                                        <NA>    0.000000
## A.nuppr.log                                   S.nuppr.log    1.151308
## A.P.daily.clip.report               S.P.daily.clip.report  104.354839
## A.P.fashion.week                         S.P.fashion.week   40.081761
## A.P.first.draft                           S.P.first.draft  434.466667
## A.P.http                                    A.npnct18.log 1305.200000
## A.P.metropolitan.diary.colon S.P.metropolitan.diary.colon   99.492308
## A.P.year.colon                             S.P.year.colon  652.200000
## A.T.day                                           S.T.day   85.904110
## A.T.fashion                                     H.T.X2015   66.105263
## A.T.obama                                       S.T.obama  354.333333
## A.T.one                                           S.T.one  231.111111
## A.T.photo                                       S.T.photo   63.360000
## A.T.report                                     S.T.report   88.295775
## A.T.said                                         S.T.said  202.516129
## A.T.state                                       S.T.state  332.315789
## clusterid                                            <NA>    0.000000
## H.npnct03.log                                        <NA> 2176.333333
## H.npnct04.log                                 H.T.billion   38.325301
## H.npnct06.log                               H.npnct16.log   68.935484
## H.npnct09.log                                        <NA>    0.000000
## H.npnct10.log                                        <NA> 6531.000000
## H.npnct17.log                                        <NA>    0.000000
## H.npnct18.log                                        <NA>    0.000000
## H.npnct20.log                                        <NA> 6531.000000
## H.npnct21.log                                        <NA>    0.000000
## H.npnct22.log                                        <NA>    0.000000
## H.npnct23.log                                        <NA>    0.000000
## H.npnct24.log                                        <NA>    0.000000
## H.npnct25.log                                        <NA>    0.000000
## H.npnct26.log                                        <NA>    0.000000
## H.npnct27.log                                        <NA>    0.000000
## H.npnct29.log                                        <NA>    0.000000
## H.npnct30.log                                        <NA>    0.000000
## H.nwrds.unq.log                               H.nuppr.log    1.008878
## H.P.http                                             <NA>    0.000000
## H.P.today.in.politic                            H.T.polit  144.155556
## H.P.what.we.are                                  H.T.read  141.000000
## H.P.year.colon                                 S.T.intern   32.670103
## H.T.daili                           A.P.daily.clip.report  102.903226
## H.T.fashion                              H.P.fashion.week   71.681818
## H.T.first                                 H.P.first.draft  194.727273
## H.T.morn                                    A.npnct28.log  165.205128
## H.T.report                                      H.T.daili  102.000000
## H.T.today                            H.P.today.in.politic  138.239130
## H.T.X2015                                       S.T.diari  101.444444
## Popular                                              <NA>    4.976212
## Popular.fctr                                         <NA>          NA
## PubDate.last1                                        <NA>    1.142857
## PubDate.last10                                       <NA>    1.666667
## PubDate.last100                                      <NA>   25.000000
## PubDate.month.fctr                                   <NA>    1.017514
## PubDate.POSIX                                        <NA>    1.000000
## PubDate.year.fctr                                    <NA>    0.000000
## PubDate.zoo                                          <NA>    1.000000
## S.nchrs.log                                   A.nchrs.log    1.328571
## S.npnct02.log                                        <NA> 6531.000000
## S.npnct05.log                                        <NA>    0.000000
## S.npnct09.log                                        <NA>    0.000000
## S.npnct10.log                                        <NA> 6531.000000
## S.npnct13.log                               A.npnct13.log    4.672000
## S.npnct16.log                                        <NA>  434.133333
## S.npnct17.log                                        <NA>    0.000000
## S.npnct18.log                                        <NA>    0.000000
## S.npnct19.log                               A.npnct19.log   12.862366
## S.npnct21.log                               A.npnct21.log 6531.000000
## S.npnct22.log                                        <NA>    0.000000
## S.npnct23.log                                        <NA> 6531.000000
## S.npnct24.log                                        <NA>    0.000000
## S.npnct25.log                                        <NA>    0.000000
## S.npnct26.log                                        <NA>    0.000000
## S.npnct27.log                                        <NA>    0.000000
## S.npnct29.log                                        <NA>    0.000000
## S.npnct30.log                                        <NA>    0.000000
## S.nwrds.log                                   A.nwrds.log    2.583333
## S.nwrds.unq.log                               S.nchrs.log    1.092486
## S.P.daily.clip.report                                <NA>  104.354839
## S.P.http                                             <NA>    0.000000
## S.T.articl                                       S.T.past   85.500000
## S.T.can                                           A.T.can  261.666667
## S.T.compani                                   A.T.compani  140.227273
## S.T.diari                                       S.T.scene   64.959184
## S.T.intern                                     S.T.tribun  131.625000
## S.T.new                                           A.T.new  100.559322
## S.T.newyork                                   A.T.newyork  145.761905
## S.T.past                                       A.T.appear  229.285714
## S.T.photo                                      H.T.pictur  248.038462
## S.T.scene                                       A.T.scene   65.316327
## S.T.take                                         A.T.take  263.166667
## S.T.time                                         A.T.time   65.804348
## S.T.week                                         A.T.week   48.408000
## S.T.will                                         A.T.will  112.584906
## S.T.word                                         A.T.word  133.145833
## UniqueID                                             <NA>    1.000000
## WordCount                                            <NA>    2.315789
##                              percentUnique zeroVar   nzv myNearZV
## WordCount.log                  24.14268218   FALSE FALSE    FALSE
## myCategory.fctr                 0.30618494   FALSE FALSE    FALSE
## H.P.readers.respond             0.03061849   FALSE  TRUE    FALSE
## A.npnct19.log                   0.07654623   FALSE FALSE    FALSE
## H.nchrs.log                     1.57685242   FALSE FALSE    FALSE
## H.npnct19.log                   0.06123699   FALSE FALSE    FALSE
## A.npnct13.log                   0.16840171   FALSE FALSE    FALSE
## S.nuppr.log                     0.33680343   FALSE FALSE    FALSE
## H.npnct15.log                   0.04592774   FALSE FALSE    FALSE
## H.npnct08.log                   0.03061849   FALSE  TRUE    FALSE
## H.T.read                        0.16840171   FALSE  TRUE    FALSE
## A.T.newyork                     0.42865891   FALSE  TRUE    FALSE
## H.T.polit                       0.13778322   FALSE  TRUE    FALSE
## H.nuppr.log                     0.29087569   FALSE FALSE    FALSE
## H.T.word                        0.13778322   FALSE  TRUE    FALSE
## PubDate.wkday.fctr              0.10716473   FALSE FALSE    FALSE
## S.ndgts.log                     0.26025720   FALSE FALSE    FALSE
## H.P.recap.colon                 0.03061849   FALSE  TRUE    FALSE
## H.P.no.comment.colon            0.03061849   FALSE  TRUE    FALSE
## H.npnct11.log                   0.07654623   FALSE FALSE    FALSE
## S.T.make                        0.41334966   FALSE  TRUE    FALSE
## A.nwrds.unq.log                 0.55113288   FALSE FALSE    FALSE
## S.T.state                       0.39804042   FALSE  TRUE    FALSE
## S.T.report                      0.35211268   FALSE  TRUE    FALSE
## S.T.one                         0.47458665   FALSE  TRUE    FALSE
## S.T.share                       0.36742192   FALSE  TRUE    FALSE
## S.T.show                        0.39804042   FALSE  TRUE    FALSE
## PubDate.last10.log             79.05695040   FALSE FALSE    FALSE
## S.npnct04.log                   0.07654623   FALSE  TRUE    FALSE
## PubDate.second.fctr             0.06123699   FALSE FALSE    FALSE
## S.npnct08.log                   0.06123699   FALSE  TRUE    FALSE
## A.nwrds.log                    93.50887936   FALSE FALSE    FALSE
## A.T.will                        0.62767912   FALSE  TRUE    FALSE
## .rnorm                         99.98469075   FALSE FALSE    FALSE
## A.T.can                         0.48989590   FALSE  TRUE    FALSE
## PubDate.hour.fctr               0.04592774   FALSE FALSE    FALSE
## S.npnct11.log                   0.13778322   FALSE FALSE    FALSE
## S.T.said                        0.36742192   FALSE  TRUE    FALSE
## H.T.week                        0.16840171   FALSE  TRUE    FALSE
## H.T.say                         0.16840171   FALSE  TRUE    FALSE
## PubDate.minute.fctr             0.06123699   FALSE FALSE    FALSE
## H.npnct16.log                   0.06123699   FALSE  TRUE    FALSE
## H.ndgts.log                     0.18371096   FALSE FALSE    FALSE
## H.npnct01.log                   0.04592774   FALSE  TRUE    FALSE
## A.T.editor                      0.29087569   FALSE  TRUE    FALSE
## H.T.news                        0.16840171   FALSE  TRUE    FALSE
## H.T.newyork                     0.15309247   FALSE  TRUE    FALSE
## H.T.new                         0.19902021   FALSE  TRUE    FALSE
## H.npnct07.log                   0.12247397   FALSE FALSE    FALSE
## H.T.art                         0.19902021   FALSE  TRUE    FALSE
## PubDate.date.fctr               0.07654623   FALSE FALSE    FALSE
## A.T.compani                     0.45927740   FALSE  TRUE    FALSE
## H.T.get                         0.18371096   FALSE  TRUE    FALSE
## S.npnct01.log                   0.06123699   FALSE  TRUE    FALSE
## A.T.intern                      0.35211268   FALSE  TRUE    FALSE
## H.npnct12.log                   0.09185548   FALSE FALSE    FALSE
## PubDate.last1.log              36.49724434   FALSE FALSE    FALSE
## H.npnct13.log                   0.12247397   FALSE  TRUE    FALSE
## H.T.china                       0.18371096   FALSE  TRUE    FALSE
## S.npnct12.log                   0.09185548   FALSE FALSE    FALSE
## H.nwrds.log                    84.15492958   FALSE FALSE    FALSE
## A.T.take                        0.42865891   FALSE  TRUE    FALSE
## A.T.time                        0.42865891   FALSE  TRUE    FALSE
## S.T.day                         0.39804042   FALSE  TRUE    FALSE
## H.T.bank                        0.13778322   FALSE  TRUE    FALSE
## H.T.big                         0.19902021   FALSE  TRUE    FALSE
## H.T.take                        0.15309247   FALSE  TRUE    FALSE
## A.T.word                        0.32149418   FALSE  TRUE    FALSE
## H.T.day                         0.18371096   FALSE  TRUE    FALSE
## PubDate.wkend                   0.03061849   FALSE FALSE    FALSE
## A.T.week                        0.50520514   FALSE  TRUE    FALSE
## H.T.busi                        0.18371096   FALSE  TRUE    FALSE
## H.T.X2014                       0.13778322   FALSE  TRUE    FALSE
## A.nchrs.log                     4.39375383   FALSE FALSE    FALSE
## A.T.appear                      0.30618494   FALSE  TRUE    FALSE
## S.T.highlight                   0.27556644   FALSE  TRUE    FALSE
## H.T.obama                       0.16840171   FALSE  TRUE    FALSE
## H.T.billion                     0.13778322   FALSE  TRUE    FALSE
## S.npnct14.log                   0.04592774   FALSE  TRUE    FALSE
## H.T.time                        0.16840171   FALSE  TRUE    FALSE
## A.T.senat                       0.52051439   FALSE  TRUE    FALSE
## S.npnct15.log                   0.04592774   FALSE FALSE    FALSE
## PubDate.last100.log            92.19228414   FALSE FALSE    FALSE
## S.T.obama                       0.36742192   FALSE  TRUE    FALSE
## H.T.make                        0.13778322   FALSE  TRUE    FALSE
## H.npnct28.log                   0.03061849   FALSE  TRUE    FALSE
## S.T.presid                      0.41334966   FALSE  TRUE    FALSE
## A.T.new                         0.55113288   FALSE  TRUE    FALSE
## A.T.year                        0.48989590   FALSE  TRUE    FALSE
## H.T.pictur                      0.10716473   FALSE  TRUE    FALSE
## S.npnct06.log                   0.03061849   FALSE  TRUE    FALSE
## S.P.metropolitan.diary.colon    0.03061849   FALSE  TRUE    FALSE
## S.T.first                       0.39804042   FALSE  TRUE    FALSE
## A.npnct16.log                   0.04592774   FALSE  TRUE    FALSE
## H.T.ebola                       0.16840171   FALSE  TRUE    FALSE
## S.T.fashion                     0.41334966   FALSE  TRUE    FALSE
## A.T.scene                       0.26025720   FALSE  TRUE    FALSE
## S.T.tribun                      0.21432945   FALSE  TRUE    FALSE
## H.T.test                        0.13778322   FALSE  TRUE    FALSE
## H.P.fashion.week                0.03061849   FALSE  TRUE    FALSE
## H.npnct14.log                   0.03061849   FALSE  TRUE    FALSE
## H.T.deal                        0.13778322   FALSE  TRUE    FALSE
## H.P.first.draft                 0.03061849   FALSE  TRUE    FALSE
## S.npnct28.log                   0.04592774   FALSE  TRUE    FALSE
## H.P.daily.clip.report           0.03061849   FALSE  TRUE    FALSE
## H.P.today.in.smallbusiness      0.03061849   FALSE  TRUE    FALSE
## H.P.verbatim.colon              0.03061849   FALSE  TRUE    FALSE
## H.npnct02.log                   0.03061849   FALSE  TRUE    FALSE
## S.P.first.draft                 0.03061849   FALSE  TRUE    FALSE
## S.npnct03.log                   0.03061849   FALSE  TRUE    FALSE
## S.npnct20.log                   0.03061849   FALSE  TRUE    FALSE
## H.P.quandary                    0.03061849   FALSE  TRUE    FALSE
## A.npnct18.log                   0.06123699   FALSE  TRUE    FALSE
## S.P.year.colon                  0.03061849   FALSE  TRUE    FALSE
## H.P.on.this.day                 0.03061849   FALSE  TRUE    FALSE
## H.npnct05.log                   0.03061849   FALSE  TRUE    FALSE
## S.npnct07.log                   0.04592774   FALSE  TRUE    FALSE
## H.P.s.notebook                  0.03061849   FALSE  TRUE    FALSE
## S.P.fashion.week                0.03061849   FALSE  TRUE    FALSE
## A.ndgts.log                     0.29087569   FALSE FALSE    FALSE
## A.npnct01.log                   0.06123699   FALSE  TRUE    FALSE
## A.npnct02.log                   0.04592774   FALSE  TRUE    FALSE
## A.npnct03.log                   0.03061849   FALSE  TRUE    FALSE
## A.npnct04.log                   0.07654623   FALSE  TRUE    FALSE
## A.npnct05.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct06.log                   0.03061849   FALSE  TRUE    FALSE
## A.npnct07.log                   0.04592774   FALSE  TRUE    FALSE
## A.npnct08.log                   0.06123699   FALSE  TRUE    FALSE
## A.npnct09.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct10.log                   0.03061849   FALSE  TRUE     TRUE
## A.npnct11.log                   0.13778322   FALSE FALSE    FALSE
## A.npnct12.log                   0.12247397   FALSE FALSE    FALSE
## A.npnct14.log                   0.10716473   FALSE  TRUE    FALSE
## A.npnct15.log                   0.04592774   FALSE FALSE    FALSE
## A.npnct17.log                   0.04592774   FALSE  TRUE    FALSE
## A.npnct20.log                   0.03061849   FALSE  TRUE    FALSE
## A.npnct21.log                   0.04592774   FALSE  TRUE     TRUE
## A.npnct22.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct23.log                   0.04592774   FALSE  TRUE     TRUE
## A.npnct24.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct25.log                   0.03061849   FALSE  TRUE     TRUE
## A.npnct26.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct27.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct28.log                   0.04592774   FALSE  TRUE    FALSE
## A.npnct29.log                   0.01530925    TRUE  TRUE     TRUE
## A.npnct30.log                   0.01530925    TRUE  TRUE     TRUE
## A.nuppr.log                     0.33680343   FALSE FALSE    FALSE
## A.P.daily.clip.report           0.03061849   FALSE  TRUE    FALSE
## A.P.fashion.week                0.03061849   FALSE  TRUE    FALSE
## A.P.first.draft                 0.03061849   FALSE  TRUE    FALSE
## A.P.http                        0.04592774   FALSE  TRUE    FALSE
## A.P.metropolitan.diary.colon    0.03061849   FALSE  TRUE    FALSE
## A.P.year.colon                  0.03061849   FALSE  TRUE    FALSE
## A.T.day                         0.42865891   FALSE  TRUE    FALSE
## A.T.fashion                     0.41334966   FALSE  TRUE    FALSE
## A.T.obama                       0.36742192   FALSE  TRUE    FALSE
## A.T.one                         0.52051439   FALSE  TRUE    FALSE
## A.T.photo                       0.29087569   FALSE  TRUE    FALSE
## A.T.report                      0.36742192   FALSE  TRUE    FALSE
## A.T.said                        0.39804042   FALSE  TRUE    FALSE
## A.T.state                       0.44396816   FALSE  TRUE    FALSE
## clusterid                       0.01530925    TRUE  TRUE     TRUE
## H.npnct03.log                   0.03061849   FALSE  TRUE     TRUE
## H.npnct04.log                   0.04592774   FALSE  TRUE    FALSE
## H.npnct06.log                   0.06123699   FALSE  TRUE    FALSE
## H.npnct09.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct10.log                   0.03061849   FALSE  TRUE     TRUE
## H.npnct17.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct18.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct20.log                   0.03061849   FALSE  TRUE     TRUE
## H.npnct21.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct22.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct23.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct24.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct25.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct26.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct27.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct29.log                   0.01530925    TRUE  TRUE     TRUE
## H.npnct30.log                   0.01530925    TRUE  TRUE     TRUE
## H.nwrds.unq.log                 0.21432945   FALSE FALSE    FALSE
## H.P.http                        0.01530925    TRUE  TRUE     TRUE
## H.P.today.in.politic            0.03061849   FALSE  TRUE    FALSE
## H.P.what.we.are                 0.03061849   FALSE  TRUE    FALSE
## H.P.year.colon                  0.03061849   FALSE  TRUE    FALSE
## H.T.daili                       0.16840171   FALSE  TRUE    FALSE
## H.T.fashion                     0.19902021   FALSE  TRUE    FALSE
## H.T.first                       0.15309247   FALSE  TRUE    FALSE
## H.T.morn                        0.07654623   FALSE  TRUE    FALSE
## H.T.report                      0.16840171   FALSE  TRUE    FALSE
## H.T.today                       0.13778322   FALSE  TRUE    FALSE
## H.T.X2015                       0.12247397   FALSE  TRUE    FALSE
## Popular                         0.03061849   FALSE FALSE    FALSE
## Popular.fctr                            NA      NA    NA       NA
## PubDate.last1                  36.49724434   FALSE FALSE    FALSE
## PubDate.last10                 79.05695040   FALSE FALSE    FALSE
## PubDate.last100                92.52908757   FALSE FALSE    FALSE
## PubDate.month.fctr              0.04592774   FALSE FALSE    FALSE
## PubDate.POSIX                  99.86221678   FALSE FALSE    FALSE
## PubDate.year.fctr               0.01530925    TRUE  TRUE     TRUE
## PubDate.zoo                    99.86221678   FALSE FALSE    FALSE
## S.nchrs.log                     3.72014697   FALSE FALSE    FALSE
## S.npnct02.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct05.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct09.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct10.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct13.log                   0.16840171   FALSE FALSE    FALSE
## S.npnct16.log                   0.04592774   FALSE  TRUE    FALSE
## S.npnct17.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct18.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct19.log                   0.07654623   FALSE FALSE    FALSE
## S.npnct21.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct22.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct23.log                   0.03061849   FALSE  TRUE     TRUE
## S.npnct24.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct25.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct26.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct27.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct29.log                   0.01530925    TRUE  TRUE     TRUE
## S.npnct30.log                   0.01530925    TRUE  TRUE     TRUE
## S.nwrds.log                    93.50887936   FALSE FALSE    FALSE
## S.nwrds.unq.log                 0.44396816   FALSE FALSE    FALSE
## S.P.daily.clip.report           0.03061849   FALSE  TRUE    FALSE
## S.P.http                        0.01530925    TRUE  TRUE     TRUE
## S.T.articl                      0.32149418   FALSE  TRUE    FALSE
## S.T.can                         0.41334966   FALSE  TRUE    FALSE
## S.T.compani                     0.44396816   FALSE  TRUE    FALSE
## S.T.diari                       0.18371096   FALSE  TRUE    FALSE
## S.T.intern                      0.32149418   FALSE  TRUE    FALSE
## S.T.new                         0.48989590   FALSE  TRUE    FALSE
## S.T.newyork                     0.41334966   FALSE  TRUE    FALSE
## S.T.past                        0.29087569   FALSE  TRUE    FALSE
## S.T.photo                       0.27556644   FALSE  TRUE    FALSE
## S.T.scene                       0.26025720   FALSE  TRUE    FALSE
## S.T.take                        0.38273117   FALSE  TRUE    FALSE
## S.T.time                        0.44396816   FALSE  TRUE    FALSE
## S.T.week                        0.44396816   FALSE  TRUE    FALSE
## S.T.will                        0.55113288   FALSE  TRUE    FALSE
## S.T.word                        0.30618494   FALSE  TRUE    FALSE
## UniqueID                      100.00000000   FALSE FALSE    FALSE
## WordCount                      24.15799143   FALSE FALSE    FALSE
##                              is.cor.y.abs.low rsp_var_raw id_var rsp_var
## WordCount.log                           FALSE       FALSE     NA      NA
## myCategory.fctr                         FALSE       FALSE     NA      NA
## H.P.readers.respond                     FALSE       FALSE     NA      NA
## A.npnct19.log                           FALSE       FALSE     NA      NA
## H.nchrs.log                             FALSE       FALSE     NA      NA
## H.npnct19.log                           FALSE       FALSE     NA      NA
## A.npnct13.log                           FALSE       FALSE     NA      NA
## S.nuppr.log                             FALSE       FALSE     NA      NA
## H.npnct15.log                           FALSE       FALSE     NA      NA
## H.npnct08.log                           FALSE       FALSE     NA      NA
## H.T.read                                FALSE       FALSE     NA      NA
## A.T.newyork                             FALSE       FALSE     NA      NA
## H.T.polit                               FALSE       FALSE     NA      NA
## H.nuppr.log                             FALSE       FALSE     NA      NA
## H.T.word                                FALSE       FALSE     NA      NA
## PubDate.wkday.fctr                      FALSE       FALSE     NA      NA
## S.ndgts.log                             FALSE       FALSE     NA      NA
## H.P.recap.colon                         FALSE       FALSE     NA      NA
## H.P.no.comment.colon                    FALSE       FALSE     NA      NA
## H.npnct11.log                           FALSE       FALSE     NA      NA
## S.T.make                                FALSE       FALSE     NA      NA
## A.nwrds.unq.log                         FALSE       FALSE     NA      NA
## S.T.state                               FALSE       FALSE     NA      NA
## S.T.report                              FALSE       FALSE     NA      NA
## S.T.one                                 FALSE       FALSE     NA      NA
## S.T.share                               FALSE       FALSE     NA      NA
## S.T.show                                FALSE       FALSE     NA      NA
## PubDate.last10.log                      FALSE       FALSE     NA      NA
## S.npnct04.log                           FALSE       FALSE     NA      NA
## PubDate.second.fctr                     FALSE       FALSE     NA      NA
## S.npnct08.log                            TRUE       FALSE     NA      NA
## A.nwrds.log                             FALSE       FALSE     NA      NA
## A.T.will                                FALSE       FALSE     NA      NA
## .rnorm                                  FALSE       FALSE     NA      NA
## A.T.can                                 FALSE       FALSE     NA      NA
## PubDate.hour.fctr                       FALSE       FALSE     NA      NA
## S.npnct11.log                           FALSE       FALSE     NA      NA
## S.T.said                                FALSE       FALSE     NA      NA
## H.T.week                                FALSE       FALSE     NA      NA
## H.T.say                                 FALSE       FALSE     NA      NA
## PubDate.minute.fctr                     FALSE       FALSE     NA      NA
## H.npnct16.log                           FALSE       FALSE     NA      NA
## H.ndgts.log                             FALSE       FALSE     NA      NA
## H.npnct01.log                           FALSE       FALSE     NA      NA
## A.T.editor                              FALSE       FALSE     NA      NA
## H.T.news                                FALSE       FALSE     NA      NA
## H.T.newyork                             FALSE       FALSE     NA      NA
## H.T.new                                 FALSE       FALSE     NA      NA
## H.npnct07.log                           FALSE       FALSE     NA      NA
## H.T.art                                 FALSE       FALSE     NA      NA
## PubDate.date.fctr                       FALSE       FALSE     NA      NA
## A.T.compani                             FALSE       FALSE     NA      NA
## H.T.get                                 FALSE       FALSE     NA      NA
## S.npnct01.log                           FALSE       FALSE     NA      NA
## A.T.intern                              FALSE       FALSE     NA      NA
## H.npnct12.log                           FALSE       FALSE     NA      NA
## PubDate.last1.log                       FALSE       FALSE     NA      NA
## H.npnct13.log                           FALSE       FALSE     NA      NA
## H.T.china                               FALSE       FALSE     NA      NA
## S.npnct12.log                           FALSE       FALSE     NA      NA
## H.nwrds.log                             FALSE       FALSE     NA      NA
## A.T.take                                FALSE       FALSE     NA      NA
## A.T.time                                FALSE       FALSE     NA      NA
## S.T.day                                 FALSE       FALSE     NA      NA
## H.T.bank                                FALSE       FALSE     NA      NA
## H.T.big                                 FALSE       FALSE     NA      NA
## H.T.take                                 TRUE       FALSE     NA      NA
## A.T.word                                FALSE       FALSE     NA      NA
## H.T.day                                 FALSE       FALSE     NA      NA
## PubDate.wkend                           FALSE       FALSE     NA      NA
## A.T.week                                FALSE       FALSE     NA      NA
## H.T.busi                                FALSE       FALSE     NA      NA
## H.T.X2014                               FALSE       FALSE     NA      NA
## A.nchrs.log                             FALSE       FALSE     NA      NA
## A.T.appear                              FALSE       FALSE     NA      NA
## S.T.highlight                           FALSE       FALSE     NA      NA
## H.T.obama                               FALSE       FALSE     NA      NA
## H.T.billion                             FALSE       FALSE     NA      NA
## S.npnct14.log                           FALSE       FALSE     NA      NA
## H.T.time                                 TRUE       FALSE     NA      NA
## A.T.senat                               FALSE       FALSE     NA      NA
## S.npnct15.log                           FALSE       FALSE     NA      NA
## PubDate.last100.log                      TRUE       FALSE     NA      NA
## S.T.obama                               FALSE       FALSE     NA      NA
## H.T.make                                FALSE       FALSE     NA      NA
## H.npnct28.log                           FALSE       FALSE     NA      NA
## S.T.presid                               TRUE       FALSE     NA      NA
## A.T.new                                 FALSE       FALSE     NA      NA
## A.T.year                                FALSE       FALSE     NA      NA
## H.T.pictur                              FALSE       FALSE     NA      NA
## S.npnct06.log                           FALSE       FALSE     NA      NA
## S.P.metropolitan.diary.colon            FALSE       FALSE     NA      NA
## S.T.first                               FALSE       FALSE     NA      NA
## A.npnct16.log                            TRUE       FALSE     NA      NA
## H.T.ebola                               FALSE       FALSE     NA      NA
## S.T.fashion                             FALSE       FALSE     NA      NA
## A.T.scene                               FALSE       FALSE     NA      NA
## S.T.tribun                              FALSE       FALSE     NA      NA
## H.T.test                                FALSE       FALSE     NA      NA
## H.P.fashion.week                        FALSE       FALSE     NA      NA
## H.npnct14.log                           FALSE       FALSE     NA      NA
## H.T.deal                                FALSE       FALSE     NA      NA
## H.P.first.draft                         FALSE       FALSE     NA      NA
## S.npnct28.log                           FALSE       FALSE     NA      NA
## H.P.daily.clip.report                   FALSE       FALSE     NA      NA
## H.P.today.in.smallbusiness              FALSE       FALSE     NA      NA
## H.P.verbatim.colon                      FALSE       FALSE     NA      NA
## H.npnct02.log                           FALSE       FALSE     NA      NA
## S.P.first.draft                         FALSE       FALSE     NA      NA
## S.npnct03.log                           FALSE       FALSE     NA      NA
## S.npnct20.log                           FALSE       FALSE     NA      NA
## H.P.quandary                            FALSE       FALSE     NA      NA
## A.npnct18.log                           FALSE       FALSE     NA      NA
## S.P.year.colon                          FALSE       FALSE     NA      NA
## H.P.on.this.day                         FALSE       FALSE     NA      NA
## H.npnct05.log                           FALSE       FALSE     NA      NA
## S.npnct07.log                           FALSE       FALSE     NA      NA
## H.P.s.notebook                           TRUE       FALSE     NA      NA
## S.P.fashion.week                        FALSE       FALSE     NA      NA
## A.ndgts.log                             FALSE       FALSE     NA      NA
## A.npnct01.log                           FALSE       FALSE     NA      NA
## A.npnct02.log                           FALSE       FALSE     NA      NA
## A.npnct03.log                           FALSE       FALSE     NA      NA
## A.npnct04.log                           FALSE       FALSE     NA      NA
## A.npnct05.log                              NA       FALSE     NA      NA
## A.npnct06.log                           FALSE       FALSE     NA      NA
## A.npnct07.log                           FALSE       FALSE     NA      NA
## A.npnct08.log                            TRUE       FALSE     NA      NA
## A.npnct09.log                              NA       FALSE     NA      NA
## A.npnct10.log                            TRUE       FALSE     NA      NA
## A.npnct11.log                           FALSE       FALSE     NA      NA
## A.npnct12.log                           FALSE       FALSE     NA      NA
## A.npnct14.log                           FALSE       FALSE     NA      NA
## A.npnct15.log                           FALSE       FALSE     NA      NA
## A.npnct17.log                           FALSE       FALSE     NA      NA
## A.npnct20.log                           FALSE       FALSE     NA      NA
## A.npnct21.log                           FALSE       FALSE     NA      NA
## A.npnct22.log                              NA       FALSE     NA      NA
## A.npnct23.log                           FALSE       FALSE     NA      NA
## A.npnct24.log                            TRUE       FALSE     NA      NA
## A.npnct25.log                            TRUE       FALSE     NA      NA
## A.npnct26.log                              NA       FALSE     NA      NA
## A.npnct27.log                              NA       FALSE     NA      NA
## A.npnct28.log                           FALSE       FALSE     NA      NA
## A.npnct29.log                              NA       FALSE     NA      NA
## A.npnct30.log                              NA       FALSE     NA      NA
## A.nuppr.log                             FALSE       FALSE     NA      NA
## A.P.daily.clip.report                   FALSE       FALSE     NA      NA
## A.P.fashion.week                        FALSE       FALSE     NA      NA
## A.P.first.draft                         FALSE       FALSE     NA      NA
## A.P.http                                FALSE       FALSE     NA      NA
## A.P.metropolitan.diary.colon            FALSE       FALSE     NA      NA
## A.P.year.colon                          FALSE       FALSE     NA      NA
## A.T.day                                 FALSE       FALSE     NA      NA
## A.T.fashion                             FALSE       FALSE     NA      NA
## A.T.obama                               FALSE       FALSE     NA      NA
## A.T.one                                 FALSE       FALSE     NA      NA
## A.T.photo                               FALSE       FALSE     NA      NA
## A.T.report                              FALSE       FALSE     NA      NA
## A.T.said                                FALSE       FALSE     NA      NA
## A.T.state                               FALSE       FALSE     NA      NA
## clusterid                                  NA       FALSE     NA      NA
## H.npnct03.log                           FALSE       FALSE     NA      NA
## H.npnct04.log                           FALSE       FALSE     NA      NA
## H.npnct06.log                           FALSE       FALSE     NA      NA
## H.npnct09.log                              NA       FALSE     NA      NA
## H.npnct10.log                            TRUE       FALSE     NA      NA
## H.npnct17.log                              NA       FALSE     NA      NA
## H.npnct18.log                              NA       FALSE     NA      NA
## H.npnct20.log                            TRUE       FALSE     NA      NA
## H.npnct21.log                              NA       FALSE     NA      NA
## H.npnct22.log                              NA       FALSE     NA      NA
## H.npnct23.log                              NA       FALSE     NA      NA
## H.npnct24.log                            TRUE       FALSE     NA      NA
## H.npnct25.log                              NA       FALSE     NA      NA
## H.npnct26.log                              NA       FALSE     NA      NA
## H.npnct27.log                              NA       FALSE     NA      NA
## H.npnct29.log                              NA       FALSE     NA      NA
## H.npnct30.log                              NA       FALSE     NA      NA
## H.nwrds.unq.log                         FALSE       FALSE     NA      NA
## H.P.http                                   NA       FALSE     NA      NA
## H.P.today.in.politic                    FALSE       FALSE     NA      NA
## H.P.what.we.are                         FALSE       FALSE     NA      NA
## H.P.year.colon                          FALSE       FALSE     NA      NA
## H.T.daili                               FALSE       FALSE     NA      NA
## H.T.fashion                             FALSE       FALSE     NA      NA
## H.T.first                               FALSE       FALSE     NA      NA
## H.T.morn                                FALSE       FALSE     NA      NA
## H.T.report                              FALSE       FALSE     NA      NA
## H.T.today                               FALSE       FALSE     NA      NA
## H.T.X2015                               FALSE       FALSE     NA      NA
## Popular                                 FALSE        TRUE     NA      NA
## Popular.fctr                               NA          NA     NA    TRUE
## PubDate.last1                           FALSE       FALSE     NA      NA
## PubDate.last10                          FALSE       FALSE     NA      NA
## PubDate.last100                         FALSE       FALSE     NA      NA
## PubDate.month.fctr                      FALSE       FALSE     NA      NA
## PubDate.POSIX                           FALSE       FALSE     NA      NA
## PubDate.year.fctr                          NA       FALSE     NA      NA
## PubDate.zoo                             FALSE       FALSE     NA      NA
## S.nchrs.log                             FALSE       FALSE     NA      NA
## S.npnct02.log                            TRUE       FALSE     NA      NA
## S.npnct05.log                              NA       FALSE     NA      NA
## S.npnct09.log                              NA       FALSE     NA      NA
## S.npnct10.log                            TRUE       FALSE     NA      NA
## S.npnct13.log                           FALSE       FALSE     NA      NA
## S.npnct16.log                            TRUE       FALSE     NA      NA
## S.npnct17.log                              NA       FALSE     NA      NA
## S.npnct18.log                              NA       FALSE     NA      NA
## S.npnct19.log                           FALSE       FALSE     NA      NA
## S.npnct21.log                           FALSE       FALSE     NA      NA
## S.npnct22.log                              NA       FALSE     NA      NA
## S.npnct23.log                           FALSE       FALSE     NA      NA
## S.npnct24.log                            TRUE       FALSE     NA      NA
## S.npnct25.log                              NA       FALSE     NA      NA
## S.npnct26.log                              NA       FALSE     NA      NA
## S.npnct27.log                              NA       FALSE     NA      NA
## S.npnct29.log                              NA       FALSE     NA      NA
## S.npnct30.log                              NA       FALSE     NA      NA
## S.nwrds.log                             FALSE       FALSE     NA      NA
## S.nwrds.unq.log                         FALSE       FALSE     NA      NA
## S.P.daily.clip.report                   FALSE       FALSE     NA      NA
## S.P.http                                   NA       FALSE     NA      NA
## S.T.articl                              FALSE       FALSE     NA      NA
## S.T.can                                 FALSE       FALSE     NA      NA
## S.T.compani                             FALSE       FALSE     NA      NA
## S.T.diari                               FALSE       FALSE     NA      NA
## S.T.intern                              FALSE       FALSE     NA      NA
## S.T.new                                 FALSE       FALSE     NA      NA
## S.T.newyork                             FALSE       FALSE     NA      NA
## S.T.past                                FALSE       FALSE     NA      NA
## S.T.photo                               FALSE       FALSE     NA      NA
## S.T.scene                               FALSE       FALSE     NA      NA
## S.T.take                                FALSE       FALSE     NA      NA
## S.T.time                                FALSE       FALSE     NA      NA
## S.T.week                                FALSE       FALSE     NA      NA
## S.T.will                                FALSE       FALSE     NA      NA
## S.T.word                                FALSE       FALSE     NA      NA
## UniqueID                                FALSE       FALSE   TRUE      NA
## WordCount                               FALSE       FALSE     NA      NA
##                              Low.cor.X.glm.importance Final.glm.importance
## WordCount.log                            1.000000e+02         1.000000e+02
## myCategory.fctr                          7.209376e+01         7.209376e+01
## H.P.readers.respond                      5.081905e+01         5.081905e+01
## A.npnct19.log                            3.893603e+01         3.893603e+01
## H.nchrs.log                              3.635733e+01         3.635733e+01
## H.npnct19.log                            3.367254e+01         3.367254e+01
## A.npnct13.log                            3.078002e+01         3.078002e+01
## S.nuppr.log                              2.879518e+01         2.879518e+01
## H.npnct15.log                            2.821906e+01         2.821906e+01
## H.npnct08.log                            2.401124e+01         2.401124e+01
## H.T.read                                 2.378859e+01         2.378859e+01
## A.T.newyork                              2.336015e+01         2.336015e+01
## H.T.polit                                2.244859e+01         2.244859e+01
## H.nuppr.log                              2.205468e+01         2.205468e+01
## H.T.word                                 1.954393e+01         1.954393e+01
## PubDate.wkday.fctr                       1.903080e+01         1.903080e+01
## S.ndgts.log                              1.850859e+01         1.850859e+01
## H.P.recap.colon                          1.836394e+01         1.836394e+01
## H.P.no.comment.colon                     1.833180e+01         1.833180e+01
## H.npnct11.log                            1.779283e+01         1.779283e+01
## S.T.make                                 1.534580e+01         1.534580e+01
## A.nwrds.unq.log                          1.490474e+01         1.490474e+01
## S.T.state                                1.443146e+01         1.443146e+01
## S.T.report                               1.398733e+01         1.398733e+01
## S.T.one                                  1.374995e+01         1.374995e+01
## S.T.share                                1.356646e+01         1.356646e+01
## S.T.show                                 1.315591e+01         1.315591e+01
## PubDate.last10.log                       1.314498e+01         1.314498e+01
## S.npnct04.log                            1.289273e+01         1.289273e+01
## PubDate.second.fctr                      1.202077e+01         1.202077e+01
## S.npnct08.log                            1.201425e+01         1.201425e+01
## A.nwrds.log                              1.200285e+01         1.200285e+01
## A.T.will                                 1.188661e+01         1.188661e+01
## .rnorm                                   1.117476e+01         1.117476e+01
## A.T.can                                  1.105762e+01         1.105762e+01
## PubDate.hour.fctr                        1.096583e+01         1.096583e+01
## S.npnct11.log                            1.085897e+01         1.085897e+01
## S.T.said                                 1.064914e+01         1.064914e+01
## H.T.week                                 1.043249e+01         1.043249e+01
## H.T.say                                  1.025337e+01         1.025337e+01
## PubDate.minute.fctr                      1.013617e+01         1.013617e+01
## H.npnct16.log                            1.012826e+01         1.012826e+01
## H.ndgts.log                              1.011850e+01         1.011850e+01
## H.npnct01.log                            9.941221e+00         9.941221e+00
## A.T.editor                               9.251126e+00         9.251126e+00
## H.T.news                                 9.183951e+00         9.183951e+00
## H.T.newyork                              8.989821e+00         8.989821e+00
## H.T.new                                  8.839602e+00         8.839602e+00
## H.npnct07.log                            8.668352e+00         8.668352e+00
## H.T.art                                  8.637437e+00         8.637437e+00
## PubDate.date.fctr                        8.224594e+00         8.224594e+00
## A.T.compani                              7.930807e+00         7.930807e+00
## H.T.get                                  7.893693e+00         7.893693e+00
## S.npnct01.log                            7.444775e+00         7.444775e+00
## A.T.intern                               7.341904e+00         7.341904e+00
## H.npnct12.log                            6.907483e+00         6.907483e+00
## PubDate.last1.log                        6.802269e+00         6.802269e+00
## H.npnct13.log                            6.670123e+00         6.670123e+00
## H.T.china                                6.528943e+00         6.528943e+00
## S.npnct12.log                            6.513816e+00         6.513816e+00
## H.nwrds.log                              6.456493e+00         6.456493e+00
## A.T.take                                 6.233315e+00         6.233315e+00
## A.T.time                                 6.126992e+00         6.126992e+00
## S.T.day                                  5.942272e+00         5.942272e+00
## H.T.bank                                 5.837283e+00         5.837283e+00
## H.T.big                                  5.817428e+00         5.817428e+00
## H.T.take                                 5.634194e+00         5.634194e+00
## A.T.word                                 5.460452e+00         5.460452e+00
## H.T.day                                  5.186099e+00         5.186099e+00
## PubDate.wkend                            5.162440e+00         5.162440e+00
## A.T.week                                 5.150546e+00         5.150546e+00
## H.T.busi                                 5.118511e+00         5.118511e+00
## H.T.X2014                                4.654351e+00         4.654351e+00
## A.nchrs.log                              4.611780e+00         4.611780e+00
## A.T.appear                               4.238918e+00         4.238918e+00
## S.T.highlight                            4.226495e+00         4.226495e+00
## H.T.obama                                3.951640e+00         3.951640e+00
## H.T.billion                              3.898066e+00         3.898066e+00
## S.npnct14.log                            3.890611e+00         3.890611e+00
## H.T.time                                 3.528210e+00         3.528210e+00
## A.T.senat                                3.218199e+00         3.218199e+00
## S.npnct15.log                            3.200871e+00         3.200871e+00
## PubDate.last100.log                      2.899121e+00         2.899121e+00
## S.T.obama                                2.693325e+00         2.693325e+00
## H.T.make                                 2.653710e+00         2.653710e+00
## H.npnct28.log                            2.542638e+00         2.542638e+00
## S.T.presid                               2.497619e+00         2.497619e+00
## A.T.new                                  2.017300e+00         2.017300e+00
## A.T.year                                 1.915103e+00         1.915103e+00
## H.T.pictur                               1.665041e+00         1.665041e+00
## S.npnct06.log                            1.563100e+00         1.563100e+00
## S.P.metropolitan.diary.colon             8.185782e-01         8.185782e-01
## S.T.first                                5.931209e-01         5.931209e-01
## A.npnct16.log                            5.306992e-01         5.306992e-01
## H.T.ebola                                5.241256e-01         5.241256e-01
## S.T.fashion                              1.501731e-01         1.501731e-01
## A.T.scene                                1.351182e-01         1.351182e-01
## S.T.tribun                               1.203763e-01         1.203763e-01
## H.T.test                                 1.077281e-01         1.077281e-01
## H.P.fashion.week                         8.950571e-02         8.950571e-02
## H.npnct14.log                            7.654941e-02         7.654941e-02
## H.T.deal                                 5.179985e-02         5.179985e-02
## H.P.first.draft                          4.237400e-02         4.237400e-02
## S.npnct28.log                            4.126089e-02         4.126089e-02
## H.P.daily.clip.report                    3.602811e-02         3.602811e-02
## H.P.today.in.smallbusiness               2.825282e-02         2.825282e-02
## H.P.verbatim.colon                       1.699712e-02         1.699712e-02
## H.npnct02.log                            1.350933e-02         1.350933e-02
## S.P.first.draft                          1.337679e-02         1.337679e-02
## S.npnct03.log                            1.100069e-02         1.100069e-02
## S.npnct20.log                            1.087710e-02         1.087710e-02
## H.P.quandary                             1.025645e-02         1.025645e-02
## A.npnct18.log                            9.131873e-03         9.131873e-03
## S.P.year.colon                           7.061198e-03         7.061198e-03
## H.P.on.this.day                          6.324980e-03         6.324980e-03
## H.npnct05.log                            3.595486e-03         3.595486e-03
## S.npnct07.log                            1.720848e-03         1.720848e-03
## H.P.s.notebook                           8.198403e-05         8.198403e-05
## S.P.fashion.week                         0.000000e+00         0.000000e+00
## A.ndgts.log                                        NA                   NA
## A.npnct01.log                                      NA                   NA
## A.npnct02.log                                      NA                   NA
## A.npnct03.log                                      NA                   NA
## A.npnct04.log                                      NA                   NA
## A.npnct05.log                                      NA                   NA
## A.npnct06.log                                      NA                   NA
## A.npnct07.log                                      NA                   NA
## A.npnct08.log                                      NA                   NA
## A.npnct09.log                                      NA                   NA
## A.npnct10.log                                      NA                   NA
## A.npnct11.log                                      NA                   NA
## A.npnct12.log                                      NA                   NA
## A.npnct14.log                                      NA                   NA
## A.npnct15.log                                      NA                   NA
## A.npnct17.log                                      NA                   NA
## A.npnct20.log                                      NA                   NA
## A.npnct21.log                                      NA                   NA
## A.npnct22.log                                      NA                   NA
## A.npnct23.log                                      NA                   NA
## A.npnct24.log                                      NA                   NA
## A.npnct25.log                                      NA                   NA
## A.npnct26.log                                      NA                   NA
## A.npnct27.log                                      NA                   NA
## A.npnct28.log                                      NA                   NA
## A.npnct29.log                                      NA                   NA
## A.npnct30.log                                      NA                   NA
## A.nuppr.log                                        NA                   NA
## A.P.daily.clip.report                              NA                   NA
## A.P.fashion.week                                   NA                   NA
## A.P.first.draft                                    NA                   NA
## A.P.http                                           NA                   NA
## A.P.metropolitan.diary.colon                       NA                   NA
## A.P.year.colon                                     NA                   NA
## A.T.day                                            NA                   NA
## A.T.fashion                                        NA                   NA
## A.T.obama                                          NA                   NA
## A.T.one                                            NA                   NA
## A.T.photo                                          NA                   NA
## A.T.report                                         NA                   NA
## A.T.said                                           NA                   NA
## A.T.state                                          NA                   NA
## clusterid                                          NA                   NA
## H.npnct03.log                                      NA                   NA
## H.npnct04.log                                      NA                   NA
## H.npnct06.log                                      NA                   NA
## H.npnct09.log                                      NA                   NA
## H.npnct10.log                                      NA                   NA
## H.npnct17.log                                      NA                   NA
## H.npnct18.log                                      NA                   NA
## H.npnct20.log                                      NA                   NA
## H.npnct21.log                                      NA                   NA
## H.npnct22.log                                      NA                   NA
## H.npnct23.log                                      NA                   NA
## H.npnct24.log                                      NA                   NA
## H.npnct25.log                                      NA                   NA
## H.npnct26.log                                      NA                   NA
## H.npnct27.log                                      NA                   NA
## H.npnct29.log                                      NA                   NA
## H.npnct30.log                                      NA                   NA
## H.nwrds.unq.log                                    NA                   NA
## H.P.http                                           NA                   NA
## H.P.today.in.politic                               NA                   NA
## H.P.what.we.are                                    NA                   NA
## H.P.year.colon                                     NA                   NA
## H.T.daili                                          NA                   NA
## H.T.fashion                                        NA                   NA
## H.T.first                                          NA                   NA
## H.T.morn                                           NA                   NA
## H.T.report                                         NA                   NA
## H.T.today                                          NA                   NA
## H.T.X2015                                          NA                   NA
## Popular                                            NA                   NA
## Popular.fctr                                       NA                   NA
## PubDate.last1                                      NA                   NA
## PubDate.last10                                     NA                   NA
## PubDate.last100                                    NA                   NA
## PubDate.month.fctr                                 NA                   NA
## PubDate.POSIX                                      NA                   NA
## PubDate.year.fctr                                  NA                   NA
## PubDate.zoo                                        NA                   NA
## S.nchrs.log                                        NA                   NA
## S.npnct02.log                                      NA                   NA
## S.npnct05.log                                      NA                   NA
## S.npnct09.log                                      NA                   NA
## S.npnct10.log                                      NA                   NA
## S.npnct13.log                                      NA                   NA
## S.npnct16.log                                      NA                   NA
## S.npnct17.log                                      NA                   NA
## S.npnct18.log                                      NA                   NA
## S.npnct19.log                                      NA                   NA
## S.npnct21.log                                      NA                   NA
## S.npnct22.log                                      NA                   NA
## S.npnct23.log                                      NA                   NA
## S.npnct24.log                                      NA                   NA
## S.npnct25.log                                      NA                   NA
## S.npnct26.log                                      NA                   NA
## S.npnct27.log                                      NA                   NA
## S.npnct29.log                                      NA                   NA
## S.npnct30.log                                      NA                   NA
## S.nwrds.log                                        NA                   NA
## S.nwrds.unq.log                                    NA                   NA
## S.P.daily.clip.report                              NA                   NA
## S.P.http                                           NA                   NA
## S.T.articl                                         NA                   NA
## S.T.can                                            NA                   NA
## S.T.compani                                        NA                   NA
## S.T.diari                                          NA                   NA
## S.T.intern                                         NA                   NA
## S.T.new                                            NA                   NA
## S.T.newyork                                        NA                   NA
## S.T.past                                           NA                   NA
## S.T.photo                                          NA                   NA
## S.T.scene                                          NA                   NA
## S.T.take                                           NA                   NA
## S.T.time                                           NA                   NA
## S.T.week                                           NA                   NA
## S.T.will                                           NA                   NA
## S.T.word                                           NA                   NA
## UniqueID                                           NA                   NA
## WordCount                                          NA                   NA
```

```r
glb_analytics_diag_plots(obs_df=glb_trnent_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnent_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 118
```

![](NYTBlogs_clusters_files/figure-html/fit.data.training_1-1.png) ![](NYTBlogs_clusters_files/figure-html/fit.data.training_1-2.png) ![](NYTBlogs_clusters_files/figure-html/fit.data.training_1-3.png) ![](NYTBlogs_clusters_files/figure-html/fit.data.training_1-4.png) ![](NYTBlogs_clusters_files/figure-html/fit.data.training_1-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 1507     1507            N                        0.0002195785
## 6370     6370            Y                        0.5529049874
##      Popular.fctr.predict.Final.glm
## 1507                              N
## 6370                              Y
##      Popular.fctr.predict.Final.glm.accurate
## 1507                                    TRUE
## 6370                                    TRUE
##      Popular.fctr.predict.Final.glm.error .label
## 1507                                    0   1507
## 6370                                    0   6370
## [1] "Inaccurate: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 4020     4020            Y                         0.002201096
## 2182     2182            Y                         0.003670196
## 4721     4721            Y                         0.004008113
## 5486     5486            Y                         0.004614868
## 3554     3554            Y                         0.005855854
## 4352     4352            Y                         0.006412768
##      Popular.fctr.predict.Final.glm
## 4020                              N
## 2182                              N
## 4721                              N
## 5486                              N
## 3554                              N
## 4352                              N
##      Popular.fctr.predict.Final.glm.accurate
## 4020                                   FALSE
## 2182                                   FALSE
## 4721                                   FALSE
## 5486                                   FALSE
## 3554                                   FALSE
## 4352                                   FALSE
##      Popular.fctr.predict.Final.glm.error
## 4020                           -0.3977989
## 2182                           -0.3963298
## 4721                           -0.3959919
## 5486                           -0.3953851
## 3554                           -0.3941441
## 4352                           -0.3935872
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 5486     5486            Y                         0.004614868
## 4265     4265            Y                         0.120729013
## 888       888            Y                         0.146951127
## 4364     4364            Y                         0.192549759
## 685       685            Y                         0.393882692
## 5317     5317            N                         0.755157378
##      Popular.fctr.predict.Final.glm
## 5486                              N
## 4265                              N
## 888                               N
## 4364                              N
## 685                               N
## 5317                              Y
##      Popular.fctr.predict.Final.glm.accurate
## 5486                                   FALSE
## 4265                                   FALSE
## 888                                    FALSE
## 4364                                   FALSE
## 685                                    FALSE
## 5317                                   FALSE
##      Popular.fctr.predict.Final.glm.error
## 5486                         -0.395385132
## 4265                         -0.279270987
## 888                          -0.253048873
## 4364                         -0.207450241
## 685                          -0.006117308
## 5317                          0.355157378
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 1667     1667            N                           0.9676284
## 4882     4882            N                           0.9802327
## 1448     1448            N                           0.9808366
## 59         59            N                           0.9862851
## 770       770            N                           0.9910303
## 2018     2018            N                           0.9953812
##      Popular.fctr.predict.Final.glm
## 1667                              Y
## 4882                              Y
## 1448                              Y
## 59                                Y
## 770                               Y
## 2018                              Y
##      Popular.fctr.predict.Final.glm.accurate
## 1667                                   FALSE
## 4882                                   FALSE
## 1448                                   FALSE
## 59                                     FALSE
## 770                                    FALSE
## 2018                                   FALSE
##      Popular.fctr.predict.Final.glm.error
## 1667                            0.5676284
## 4882                            0.5802327
## 1448                            0.5808366
## 59                              0.5862851
## 770                             0.5910303
## 2018                            0.5953812
```

![](NYTBlogs_clusters_files/figure-html/fit.data.training_1-6.png) 

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
## 92              Y                         0.052448559
## 693             Y                         0.086370372
## 4020            Y                         0.002201096
## 4721            Y                         0.004008113
##      Popular.fctr.predict.Final.glm
## 92                                N
## 693                               N
## 4020                              N
## 4721                              N
```

```r
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

![](NYTBlogs_clusters_files/figure-html/fit.data.training_1-7.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 332.371 343.572  11.201
## 16  predict.data.new          9          0 343.573      NA      NA
```

## Step `9.0: predict data new`

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
## rsp_var_out = glb_rsp_var_out, : Using default probability threshold: 0.4
```

```r
glb_analytics_diag_plots(obs_df=glb_newent_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_newent_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 118
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

![](NYTBlogs_clusters_files/figure-html/predict.data.new-1.png) 

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

![](NYTBlogs_clusters_files/figure-html/predict.data.new-2.png) 

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

![](NYTBlogs_clusters_files/figure-html/predict.data.new-3.png) 

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

![](NYTBlogs_clusters_files/figure-html/predict.data.new-4.png) 

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

![](NYTBlogs_clusters_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 6753     6753         <NA>                        0.8528333541
## 7309     7309         <NA>                        0.0002628103
##      Popular.fctr.predict.Final.glm
## 6753                              Y
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

![](NYTBlogs_clusters_files/figure-html/predict.data.new-6.png) 

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
## [1] 0.4
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: Low.cor.X.glm"
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
## [1] 4475  249
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 8              Low.cor.X.glm        0.9105493   0.9200352     0.6727694
## 9                  All.X.glm        0.8974234   0.9150986     0.6523231
## 10      All.X.no.rnorm.rpart        0.8862421   0.7084504     0.5054039
## 1          MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.8327662   0.5000000     0.0000000
## 5            Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 7    Interact.High.cor.Y.glm        0.7987360   0.7859240     0.3416531
## 6              Max.cor.Y.glm        0.7316480   0.7102060     0.2283681
## 2    Random.myrandom_classfr        0.1672338   0.4909227     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 8     2045.192                    0.4
## 9     2078.788                    0.3
## 10          NA                    0.7
## 1           NA                    0.5
## 3           NA                    0.5
## 4           NA                    0.5
## 5           NA                    0.5
## 7     3317.941                    0.3
## 6     3714.601                    0.2
## 2           NA                    0.1
```

```r
print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
```

```
## [1] "Low.cor.X.glm OOB confusion matrix & accuracy: "
```

```r
print(t(confusionMatrix(glb_OOBent_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                        glb_OOBent_df[, glb_rsp_var])$table))
```

```
##          Prediction
## Reference    N    Y
##         N 1629   84
##         Y  100  244
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
## 15                        OpEd#Opinion#    154    164    0.087700535
## 6        Business#Business Day#Dealbook    312    304    0.162566845
## 18                         Styles#U.S.#     54     62    0.033155080
## 16                      Science#Health#     66     57    0.030481283
## 10                        Culture#Arts#    225    244    0.130481283
## 9                  Business#Technology#    114    113    0.060427807
## 8            Business#Crosswords/Games#     40     42    0.022459893
## 13                 Metro#N.Y. / Region#     60     67    0.035828877
## 4            #Opinion#The Public Editor     10     10    0.005347594
## 7  Business#Business Day#Small Business     45     42    0.022459893
## 20                             TStyle##    221    105    0.056149733
## 3              #Opinion#Room For Debate     21     24    0.012834225
## 17                      Styles##Fashion     41     15    0.008021390
## 2                          #Multimedia#     42     52    0.027807487
## 5                       #U.S.#Education     93     90    0.048128342
## 11                       Foreign#World#     47     47    0.025133690
## 12           Foreign#World#Asia Pacific     61     56    0.029946524
## 14                              myOther     13      3    0.001604278
## 19                       Travel#Travel#     31     35    0.018716578
##    .freqRatio.OOB accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 1     0.197860963                 36               371        0.9115479
## 15    0.074866310                 33               121        0.7857143
## 6     0.151677200                 32               280        0.8974359
## 18    0.026251823                 22                32        0.5925926
## 16    0.032085561                 19                47        0.7121212
## 10    0.109382596                 13               212        0.9422222
## 9     0.055420515                 12               102        0.8947368
## 8     0.019445795                  5                35        0.8750000
## 13    0.029168692                  4                56        0.9333333
## 4     0.004861449                  2                 8        0.8000000
## 7     0.021876519                  2                43        0.9555556
## 20    0.107438017                  2               219        0.9909502
## 3     0.010209042                  1                20        0.9523810
## 17    0.019931940                  1                40        0.9756098
## 2     0.020418085                  0                42        1.0000000
## 5     0.045211473                  0                93        1.0000000
## 11    0.022848809                  0                47        1.0000000
## 12    0.029654837                  0                61        1.0000000
## 14    0.006319883                  0                13        1.0000000
## 19    0.015070491                  0                31        1.0000000
```

```r
dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBent_df[glb_OOBent_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBent_df[glb_OOBent_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBent_df[glb_OOBent_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBent_df[glb_OOBent_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBent_df[(glb_OOBent_df$myCategory == myCategory) & 
                             (!glb_OOBent_df[, predct_accurate_var_name]), glb_id_vars]
    OOBerr_df <- glb_OOBent_df[(glb_OOBent_df$UniqueID %in% err_ids) & 
                               (glb_OOBent_df$Popular == 1), 
                        c("clusterid", "Headline", "Popular")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOBerr_df)))
    print(OOBerr_df)
}
dsp_myCategory_conf_mtrx(myCategory="Science#Health#")
```

```
## [1] "Low.cor.X.glm OOB::myCategory=Science#Health# confusion matrix & accuracy: "
##          Prediction
## Reference  N  Y
##         N  8 15
##         Y  4 39
## [1] 0.7121212
## [1] "Low.cor.X.glm OOB::myCategory=Science#Health# FN errors: 4"
##      clusterid
## 327          1
## 1053         1
## 4090         1
## 5701         1
##                                                           Headline Popular
## 327                 Think Like a Doctor: Weaker and Weaker Solved!       1
## 1053                           Ask Well: Plantar Fasciitis Relief        1
## 4090                       Milk Choice May Affect Vitamin D Levels       1
## 5701 Vegetarian Thanksgiving: Caramelized Onion and Fennel Risotto       1
```

```r
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
## [2] Popular.fctr.predict.Low.cor.X.glm.prob    
## [3] Popular.fctr.predict.Low.cor.X.glm         
## [4] Popular.fctr.predict.Low.cor.X.glm.accurate
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
##                PubDate.POSIX                    H.T.X2014 
##                            0                            0 
##                    H.T.X2015                      H.T.art 
##                            0                            0 
##                     H.T.bank                      H.T.big 
##                            0                            0 
##                  H.T.billion                     H.T.busi 
##                            0                            0 
##                    H.T.china                    H.T.daili 
##                            0                            0 
##                      H.T.day                     H.T.deal 
##                            0                            0 
##                  H.T.fashion                    H.T.first 
##                            0                            0 
##                     H.T.make                     H.T.morn 
##                            0                            0 
##                      H.T.new                     H.T.news 
##                            0                            0 
##                  H.T.newyork                    H.T.obama 
##                            0                            0 
##                   H.T.pictur                    H.T.polit 
##                            0                            0 
##                   H.T.report                      H.T.say 
##                            0                            0 
##                     H.T.take                     H.T.test 
##                            0                            0 
##                     H.T.time                    H.T.today 
##                            0                            0 
##                     H.T.week                   S.T.articl 
##                            0                            0 
##                      S.T.can                  S.T.compani 
##                            0                            0 
##                      S.T.day                  S.T.fashion 
##                            0                            0 
##                    S.T.first                   S.T.intern 
##                            0                            0 
##                     S.T.make                      S.T.new 
##                            0                            0 
##                  S.T.newyork                      S.T.one 
##                            0                            0 
##                   S.T.presid                   S.T.report 
##                            0                            0 
##                     S.T.said                    S.T.share 
##                            0                            0 
##                     S.T.show                    S.T.state 
##                            0                            0 
##                     S.T.take                     S.T.time 
##                            0                            0 
##                     S.T.week                     S.T.will 
##                            0                            0 
##                      A.T.can                  A.T.compani 
##                            0                            0 
##                      A.T.day                  A.T.fashion 
##                            0                            0 
##                   A.T.intern                      A.T.new 
##                            0                            0 
##                  A.T.newyork                      A.T.one 
##                            0                            0 
##                   A.T.report                     A.T.said 
##                            0                            0 
##                    A.T.state                     A.T.take 
##                            0                            0 
##                     A.T.time                     A.T.week 
##                            0                            0 
##                     A.T.will                     A.T.year 
##                            0                            0 
##                    H.T.ebola                      H.T.get 
##                            0                            0 
##                     H.T.read                     H.T.word 
##                            0                            0 
##                  H.nwrds.log              H.nwrds.unq.log 
##                            0                            0 
##                  H.nchrs.log                  H.nuppr.log 
##                            0                            0 
##                  H.ndgts.log                H.npnct01.log 
##                            0                            0 
##                H.npnct02.log                H.npnct03.log 
##                            0                            0 
##                H.npnct04.log                H.npnct05.log 
##                            0                            0 
##                H.npnct06.log                H.npnct07.log 
##                            0                            0 
##                H.npnct08.log                H.npnct09.log 
##                            0                            0 
##                H.npnct10.log                H.npnct11.log 
##                            0                            0 
##                H.npnct12.log                H.npnct13.log 
##                            0                            0 
##                H.npnct14.log                H.npnct15.log 
##                            0                            0 
##                H.npnct16.log                H.npnct17.log 
##                            0                            0 
##                H.npnct18.log                H.npnct19.log 
##                            0                            0 
##                H.npnct20.log                H.npnct21.log 
##                            0                            0 
##                H.npnct22.log                H.npnct23.log 
##                            0                            0 
##                H.npnct24.log                H.npnct25.log 
##                            0                            0 
##                H.npnct26.log                H.npnct27.log 
##                            0                            0 
##                H.npnct28.log                H.npnct29.log 
##                            0                            0 
##                H.npnct30.log                     H.P.http 
##                            0                            0 
##               H.P.year.colon        H.P.daily.clip.report 
##                            0                            0 
##             H.P.fashion.week              H.P.first.draft 
##                            0                            0 
##                 H.P.quandary              H.P.recap.colon 
##                            0                            0 
##         H.P.no.comment.colon         H.P.today.in.politic 
##                            0                            0 
##   H.P.today.in.smallbusiness              H.P.what.we.are 
##                            0                            0 
##          H.P.readers.respond               H.P.s.notebook 
##                            0                            0 
##           H.P.verbatim.colon              H.P.on.this.day 
##                            0                            0 
##                    S.T.diari                S.T.highlight 
##                            0                            0 
##                    S.T.obama                     S.T.past 
##                            0                            0 
##                    S.T.photo                    S.T.scene 
##                            0                            0 
##                   S.T.tribun                     S.T.word 
##                            0                            0 
##                  S.nwrds.log              S.nwrds.unq.log 
##                            0                            0 
##                  S.nchrs.log                  S.nuppr.log 
##                            0                            0 
##                  S.ndgts.log                S.npnct01.log 
##                            0                            0 
##                S.npnct02.log                S.npnct03.log 
##                            0                            0 
##                S.npnct04.log                S.npnct05.log 
##                            0                            0 
##                S.npnct06.log                S.npnct07.log 
##                            0                            0 
##                S.npnct08.log                S.npnct09.log 
##                            0                            0 
##                S.npnct10.log                S.npnct11.log 
##                            0                            0 
##                S.npnct12.log                S.npnct13.log 
##                            0                            0 
##                S.npnct14.log                S.npnct15.log 
##                            0                            0 
##                S.npnct16.log                S.npnct17.log 
##                            0                            0 
##                S.npnct18.log                S.npnct19.log 
##                            0                            0 
##                S.npnct20.log                S.npnct21.log 
##                            0                            0 
##                S.npnct22.log                S.npnct23.log 
##                            0                            0 
##                S.npnct24.log                S.npnct25.log 
##                            0                            0 
##                S.npnct26.log                S.npnct27.log 
##                            0                            0 
##                S.npnct28.log                S.npnct29.log 
##                            0                            0 
##                S.npnct30.log                     S.P.http 
##                            0                            0 
##               S.P.year.colon        S.P.daily.clip.report 
##                            0                            0 
##             S.P.fashion.week              S.P.first.draft 
##                            0                            0 
## S.P.metropolitan.diary.colon                   A.T.appear 
##                            0                            0 
##                   A.T.editor                    A.T.obama 
##                            0                            0 
##                    A.T.photo                    A.T.scene 
##                            0                            0 
##                    A.T.senat                     A.T.word 
##                            0                            0 
##                  A.nwrds.log              A.nwrds.unq.log 
##                            0                            0 
##                  A.nchrs.log                  A.nuppr.log 
##                            0                            0 
##                  A.ndgts.log                A.npnct01.log 
##                            0                            0 
##                A.npnct02.log                A.npnct03.log 
##                            0                            0 
##                A.npnct04.log                A.npnct05.log 
##                            0                            0 
##                A.npnct06.log                A.npnct07.log 
##                            0                            0 
##                A.npnct08.log                A.npnct09.log 
##                            0                            0 
##                A.npnct10.log                A.npnct11.log 
##                            0                            0 
##                A.npnct12.log                A.npnct13.log 
##                            0                            0 
##                A.npnct14.log                A.npnct15.log 
##                            0                            0 
##                A.npnct16.log                A.npnct17.log 
##                            0                            0 
##                A.npnct18.log                A.npnct19.log 
##                            0                            0 
##                A.npnct20.log                A.npnct21.log 
##                            0                            0 
##                A.npnct22.log                A.npnct23.log 
##                            0                            0 
##                A.npnct24.log                A.npnct25.log 
##                            0                            0 
##                A.npnct26.log                A.npnct27.log 
##                            0                            0 
##                A.npnct28.log                A.npnct29.log 
##                            0                            0 
##                A.npnct30.log                     A.P.http 
##                            0                            0 
##               A.P.year.colon        A.P.daily.clip.report 
##                            0                            0 
##             A.P.fashion.week              A.P.first.draft 
##                            0                            0 
## A.P.metropolitan.diary.colon 
##                            0
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
##                              zeroVar   nzv myNearZV   importance
## WordCount.log                  FALSE FALSE    FALSE 1.000000e+02
## myCategory.fctr                FALSE FALSE    FALSE 7.209376e+01
## H.P.readers.respond            FALSE  TRUE    FALSE 5.081905e+01
## A.npnct19.log                  FALSE FALSE    FALSE 3.893603e+01
## H.nchrs.log                    FALSE FALSE    FALSE 3.635733e+01
## H.npnct19.log                  FALSE FALSE    FALSE 3.367254e+01
## A.npnct13.log                  FALSE FALSE    FALSE 3.078002e+01
## S.nuppr.log                    FALSE FALSE    FALSE 2.879518e+01
## H.npnct15.log                  FALSE FALSE    FALSE 2.821906e+01
## H.npnct08.log                  FALSE  TRUE    FALSE 2.401124e+01
## H.T.read                       FALSE  TRUE    FALSE 2.378859e+01
## A.T.newyork                    FALSE  TRUE    FALSE 2.336015e+01
## H.T.polit                      FALSE  TRUE    FALSE 2.244859e+01
## H.nuppr.log                    FALSE FALSE    FALSE 2.205468e+01
## H.T.word                       FALSE  TRUE    FALSE 1.954393e+01
## PubDate.wkday.fctr             FALSE FALSE    FALSE 1.903080e+01
## S.ndgts.log                    FALSE FALSE    FALSE 1.850859e+01
## H.P.recap.colon                FALSE  TRUE    FALSE 1.836394e+01
## H.P.no.comment.colon           FALSE  TRUE    FALSE 1.833180e+01
## H.npnct11.log                  FALSE FALSE    FALSE 1.779283e+01
## S.T.make                       FALSE  TRUE    FALSE 1.534580e+01
## A.nwrds.unq.log                FALSE FALSE    FALSE 1.490474e+01
## S.T.state                      FALSE  TRUE    FALSE 1.443146e+01
## S.T.report                     FALSE  TRUE    FALSE 1.398733e+01
## S.T.one                        FALSE  TRUE    FALSE 1.374995e+01
## S.T.share                      FALSE  TRUE    FALSE 1.356646e+01
## S.T.show                       FALSE  TRUE    FALSE 1.315591e+01
## PubDate.last10.log             FALSE FALSE    FALSE 1.314498e+01
## S.npnct04.log                  FALSE  TRUE    FALSE 1.289273e+01
## PubDate.second.fctr            FALSE FALSE    FALSE 1.202077e+01
## S.npnct08.log                  FALSE  TRUE    FALSE 1.201425e+01
## A.nwrds.log                    FALSE FALSE    FALSE 1.200285e+01
## A.T.will                       FALSE  TRUE    FALSE 1.188661e+01
## .rnorm                         FALSE FALSE    FALSE 1.117476e+01
## A.T.can                        FALSE  TRUE    FALSE 1.105762e+01
## PubDate.hour.fctr              FALSE FALSE    FALSE 1.096583e+01
## S.npnct11.log                  FALSE FALSE    FALSE 1.085897e+01
## S.T.said                       FALSE  TRUE    FALSE 1.064914e+01
## H.T.week                       FALSE  TRUE    FALSE 1.043249e+01
## H.T.say                        FALSE  TRUE    FALSE 1.025337e+01
## PubDate.minute.fctr            FALSE FALSE    FALSE 1.013617e+01
## H.npnct16.log                  FALSE  TRUE    FALSE 1.012826e+01
## H.ndgts.log                    FALSE FALSE    FALSE 1.011850e+01
## H.npnct01.log                  FALSE  TRUE    FALSE 9.941221e+00
## A.T.editor                     FALSE  TRUE    FALSE 9.251126e+00
## H.T.news                       FALSE  TRUE    FALSE 9.183951e+00
## H.T.newyork                    FALSE  TRUE    FALSE 8.989821e+00
## H.T.new                        FALSE  TRUE    FALSE 8.839602e+00
## H.npnct07.log                  FALSE FALSE    FALSE 8.668352e+00
## H.T.art                        FALSE  TRUE    FALSE 8.637437e+00
## PubDate.date.fctr              FALSE FALSE    FALSE 8.224594e+00
## A.T.compani                    FALSE  TRUE    FALSE 7.930807e+00
## H.T.get                        FALSE  TRUE    FALSE 7.893693e+00
## S.npnct01.log                  FALSE  TRUE    FALSE 7.444775e+00
## A.T.intern                     FALSE  TRUE    FALSE 7.341904e+00
## H.npnct12.log                  FALSE FALSE    FALSE 6.907483e+00
## PubDate.last1.log              FALSE FALSE    FALSE 6.802269e+00
## H.npnct13.log                  FALSE  TRUE    FALSE 6.670123e+00
## H.T.china                      FALSE  TRUE    FALSE 6.528943e+00
## S.npnct12.log                  FALSE FALSE    FALSE 6.513816e+00
## H.nwrds.log                    FALSE FALSE    FALSE 6.456493e+00
## A.T.take                       FALSE  TRUE    FALSE 6.233315e+00
## A.T.time                       FALSE  TRUE    FALSE 6.126992e+00
## S.T.day                        FALSE  TRUE    FALSE 5.942272e+00
## H.T.bank                       FALSE  TRUE    FALSE 5.837283e+00
## H.T.big                        FALSE  TRUE    FALSE 5.817428e+00
## H.T.take                       FALSE  TRUE    FALSE 5.634194e+00
## A.T.word                       FALSE  TRUE    FALSE 5.460452e+00
## H.T.day                        FALSE  TRUE    FALSE 5.186099e+00
## PubDate.wkend                  FALSE FALSE    FALSE 5.162440e+00
## A.T.week                       FALSE  TRUE    FALSE 5.150546e+00
## H.T.busi                       FALSE  TRUE    FALSE 5.118511e+00
## H.T.X2014                      FALSE  TRUE    FALSE 4.654351e+00
## A.nchrs.log                    FALSE FALSE    FALSE 4.611780e+00
## A.T.appear                     FALSE  TRUE    FALSE 4.238918e+00
## S.T.highlight                  FALSE  TRUE    FALSE 4.226495e+00
## H.T.obama                      FALSE  TRUE    FALSE 3.951640e+00
## H.T.billion                    FALSE  TRUE    FALSE 3.898066e+00
## S.npnct14.log                  FALSE  TRUE    FALSE 3.890611e+00
## H.T.time                       FALSE  TRUE    FALSE 3.528210e+00
## A.T.senat                      FALSE  TRUE    FALSE 3.218199e+00
## S.npnct15.log                  FALSE FALSE    FALSE 3.200871e+00
## PubDate.last100.log            FALSE FALSE    FALSE 2.899121e+00
## S.T.obama                      FALSE  TRUE    FALSE 2.693325e+00
## H.T.make                       FALSE  TRUE    FALSE 2.653710e+00
## H.npnct28.log                  FALSE  TRUE    FALSE 2.542638e+00
## S.T.presid                     FALSE  TRUE    FALSE 2.497619e+00
## A.T.new                        FALSE  TRUE    FALSE 2.017300e+00
## A.T.year                       FALSE  TRUE    FALSE 1.915103e+00
## H.T.pictur                     FALSE  TRUE    FALSE 1.665041e+00
## S.npnct06.log                  FALSE  TRUE    FALSE 1.563100e+00
## S.P.metropolitan.diary.colon   FALSE  TRUE    FALSE 8.185782e-01
## S.T.first                      FALSE  TRUE    FALSE 5.931209e-01
## A.npnct16.log                  FALSE  TRUE    FALSE 5.306992e-01
## H.T.ebola                      FALSE  TRUE    FALSE 5.241256e-01
## S.T.fashion                    FALSE  TRUE    FALSE 1.501731e-01
## A.T.scene                      FALSE  TRUE    FALSE 1.351182e-01
## S.T.tribun                     FALSE  TRUE    FALSE 1.203763e-01
## H.T.test                       FALSE  TRUE    FALSE 1.077281e-01
## H.P.fashion.week               FALSE  TRUE    FALSE 8.950571e-02
## H.npnct14.log                  FALSE  TRUE    FALSE 7.654941e-02
## H.T.deal                       FALSE  TRUE    FALSE 5.179985e-02
## H.P.first.draft                FALSE  TRUE    FALSE 4.237400e-02
## S.npnct28.log                  FALSE  TRUE    FALSE 4.126089e-02
## H.P.daily.clip.report          FALSE  TRUE    FALSE 3.602811e-02
## H.P.today.in.smallbusiness     FALSE  TRUE    FALSE 2.825282e-02
## H.P.verbatim.colon             FALSE  TRUE    FALSE 1.699712e-02
## H.npnct02.log                  FALSE  TRUE    FALSE 1.350933e-02
## S.P.first.draft                FALSE  TRUE    FALSE 1.337679e-02
## S.npnct03.log                  FALSE  TRUE    FALSE 1.100069e-02
## S.npnct20.log                  FALSE  TRUE    FALSE 1.087710e-02
## H.P.quandary                   FALSE  TRUE    FALSE 1.025645e-02
## A.npnct18.log                  FALSE  TRUE    FALSE 9.131873e-03
## S.P.year.colon                 FALSE  TRUE    FALSE 7.061198e-03
## H.P.on.this.day                FALSE  TRUE    FALSE 6.324980e-03
## H.npnct05.log                  FALSE  TRUE    FALSE 3.595486e-03
## S.npnct07.log                  FALSE  TRUE    FALSE 1.720848e-03
## H.P.s.notebook                 FALSE  TRUE    FALSE 8.198403e-05
## S.P.fashion.week               FALSE  TRUE    FALSE 0.000000e+00
##                              Low.cor.X.glm.importance Final.glm.importance
## WordCount.log                            1.000000e+02         1.000000e+02
## myCategory.fctr                          7.209376e+01         7.209376e+01
## H.P.readers.respond                      5.081905e+01         5.081905e+01
## A.npnct19.log                            3.893603e+01         3.893603e+01
## H.nchrs.log                              3.635733e+01         3.635733e+01
## H.npnct19.log                            3.367254e+01         3.367254e+01
## A.npnct13.log                            3.078002e+01         3.078002e+01
## S.nuppr.log                              2.879518e+01         2.879518e+01
## H.npnct15.log                            2.821906e+01         2.821906e+01
## H.npnct08.log                            2.401124e+01         2.401124e+01
## H.T.read                                 2.378859e+01         2.378859e+01
## A.T.newyork                              2.336015e+01         2.336015e+01
## H.T.polit                                2.244859e+01         2.244859e+01
## H.nuppr.log                              2.205468e+01         2.205468e+01
## H.T.word                                 1.954393e+01         1.954393e+01
## PubDate.wkday.fctr                       1.903080e+01         1.903080e+01
## S.ndgts.log                              1.850859e+01         1.850859e+01
## H.P.recap.colon                          1.836394e+01         1.836394e+01
## H.P.no.comment.colon                     1.833180e+01         1.833180e+01
## H.npnct11.log                            1.779283e+01         1.779283e+01
## S.T.make                                 1.534580e+01         1.534580e+01
## A.nwrds.unq.log                          1.490474e+01         1.490474e+01
## S.T.state                                1.443146e+01         1.443146e+01
## S.T.report                               1.398733e+01         1.398733e+01
## S.T.one                                  1.374995e+01         1.374995e+01
## S.T.share                                1.356646e+01         1.356646e+01
## S.T.show                                 1.315591e+01         1.315591e+01
## PubDate.last10.log                       1.314498e+01         1.314498e+01
## S.npnct04.log                            1.289273e+01         1.289273e+01
## PubDate.second.fctr                      1.202077e+01         1.202077e+01
## S.npnct08.log                            1.201425e+01         1.201425e+01
## A.nwrds.log                              1.200285e+01         1.200285e+01
## A.T.will                                 1.188661e+01         1.188661e+01
## .rnorm                                   1.117476e+01         1.117476e+01
## A.T.can                                  1.105762e+01         1.105762e+01
## PubDate.hour.fctr                        1.096583e+01         1.096583e+01
## S.npnct11.log                            1.085897e+01         1.085897e+01
## S.T.said                                 1.064914e+01         1.064914e+01
## H.T.week                                 1.043249e+01         1.043249e+01
## H.T.say                                  1.025337e+01         1.025337e+01
## PubDate.minute.fctr                      1.013617e+01         1.013617e+01
## H.npnct16.log                            1.012826e+01         1.012826e+01
## H.ndgts.log                              1.011850e+01         1.011850e+01
## H.npnct01.log                            9.941221e+00         9.941221e+00
## A.T.editor                               9.251126e+00         9.251126e+00
## H.T.news                                 9.183951e+00         9.183951e+00
## H.T.newyork                              8.989821e+00         8.989821e+00
## H.T.new                                  8.839602e+00         8.839602e+00
## H.npnct07.log                            8.668352e+00         8.668352e+00
## H.T.art                                  8.637437e+00         8.637437e+00
## PubDate.date.fctr                        8.224594e+00         8.224594e+00
## A.T.compani                              7.930807e+00         7.930807e+00
## H.T.get                                  7.893693e+00         7.893693e+00
## S.npnct01.log                            7.444775e+00         7.444775e+00
## A.T.intern                               7.341904e+00         7.341904e+00
## H.npnct12.log                            6.907483e+00         6.907483e+00
## PubDate.last1.log                        6.802269e+00         6.802269e+00
## H.npnct13.log                            6.670123e+00         6.670123e+00
## H.T.china                                6.528943e+00         6.528943e+00
## S.npnct12.log                            6.513816e+00         6.513816e+00
## H.nwrds.log                              6.456493e+00         6.456493e+00
## A.T.take                                 6.233315e+00         6.233315e+00
## A.T.time                                 6.126992e+00         6.126992e+00
## S.T.day                                  5.942272e+00         5.942272e+00
## H.T.bank                                 5.837283e+00         5.837283e+00
## H.T.big                                  5.817428e+00         5.817428e+00
## H.T.take                                 5.634194e+00         5.634194e+00
## A.T.word                                 5.460452e+00         5.460452e+00
## H.T.day                                  5.186099e+00         5.186099e+00
## PubDate.wkend                            5.162440e+00         5.162440e+00
## A.T.week                                 5.150546e+00         5.150546e+00
## H.T.busi                                 5.118511e+00         5.118511e+00
## H.T.X2014                                4.654351e+00         4.654351e+00
## A.nchrs.log                              4.611780e+00         4.611780e+00
## A.T.appear                               4.238918e+00         4.238918e+00
## S.T.highlight                            4.226495e+00         4.226495e+00
## H.T.obama                                3.951640e+00         3.951640e+00
## H.T.billion                              3.898066e+00         3.898066e+00
## S.npnct14.log                            3.890611e+00         3.890611e+00
## H.T.time                                 3.528210e+00         3.528210e+00
## A.T.senat                                3.218199e+00         3.218199e+00
## S.npnct15.log                            3.200871e+00         3.200871e+00
## PubDate.last100.log                      2.899121e+00         2.899121e+00
## S.T.obama                                2.693325e+00         2.693325e+00
## H.T.make                                 2.653710e+00         2.653710e+00
## H.npnct28.log                            2.542638e+00         2.542638e+00
## S.T.presid                               2.497619e+00         2.497619e+00
## A.T.new                                  2.017300e+00         2.017300e+00
## A.T.year                                 1.915103e+00         1.915103e+00
## H.T.pictur                               1.665041e+00         1.665041e+00
## S.npnct06.log                            1.563100e+00         1.563100e+00
## S.P.metropolitan.diary.colon             8.185782e-01         8.185782e-01
## S.T.first                                5.931209e-01         5.931209e-01
## A.npnct16.log                            5.306992e-01         5.306992e-01
## H.T.ebola                                5.241256e-01         5.241256e-01
## S.T.fashion                              1.501731e-01         1.501731e-01
## A.T.scene                                1.351182e-01         1.351182e-01
## S.T.tribun                               1.203763e-01         1.203763e-01
## H.T.test                                 1.077281e-01         1.077281e-01
## H.P.fashion.week                         8.950571e-02         8.950571e-02
## H.npnct14.log                            7.654941e-02         7.654941e-02
## H.T.deal                                 5.179985e-02         5.179985e-02
## H.P.first.draft                          4.237400e-02         4.237400e-02
## S.npnct28.log                            4.126089e-02         4.126089e-02
## H.P.daily.clip.report                    3.602811e-02         3.602811e-02
## H.P.today.in.smallbusiness               2.825282e-02         2.825282e-02
## H.P.verbatim.colon                       1.699712e-02         1.699712e-02
## H.npnct02.log                            1.350933e-02         1.350933e-02
## S.P.first.draft                          1.337679e-02         1.337679e-02
## S.npnct03.log                            1.100069e-02         1.100069e-02
## S.npnct20.log                            1.087710e-02         1.087710e-02
## H.P.quandary                             1.025645e-02         1.025645e-02
## A.npnct18.log                            9.131873e-03         9.131873e-03
## S.P.year.colon                           7.061198e-03         7.061198e-03
## H.P.on.this.day                          6.324980e-03         6.324980e-03
## H.npnct05.log                            3.595486e-03         3.595486e-03
## S.npnct07.log                            1.720848e-03         1.720848e-03
## H.P.s.notebook                           8.198403e-05         8.198403e-05
## S.P.fashion.week                         0.000000e+00         0.000000e+00
```

```r
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])

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

rm(submit_df, tmp_OOBent_df)

# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor     bgn     end elapsed
## 16     predict.data.new          9          0 343.573 352.604   9.032
## 17 display.session.info         10          0 352.605      NA      NA
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
## 8          select.features          5          0 125.885 210.291  84.406
## 6         extract.features          3          0  42.079 122.711  80.632
## 10              fit.models          7          0 211.670 252.083  40.413
## 11              fit.models          7          1 252.084 290.217  38.133
## 14       fit.data.training          8          0 313.160 332.371  19.211
## 2             inspect.data          2          0  13.437  32.417  18.980
## 12              fit.models          7          2 290.218 306.917  16.699
## 15       fit.data.training          8          1 332.371 343.572  11.201
## 16        predict.data.new          9          0 343.573 352.604   9.032
## 13              fit.models          7          3 306.917 313.160   6.243
## 4      manage.missing.data          2          2  36.578  42.019   5.441
## 3             cleanse.data          2          1  32.417  36.577   4.160
## 7             cluster.data          4          0 122.711 125.885   3.174
## 9  partition.data.training          6          0 210.292 211.669   1.377
## 1              import.data          1          0  12.361  13.436   1.075
## 5              encode.data          2          3  42.019  42.079   0.060
##    duration
## 8    84.406
## 6    80.632
## 10   40.413
## 11   38.133
## 14   19.211
## 2    18.980
## 12   16.699
## 15   11.201
## 16    9.031
## 13    6.243
## 4     5.441
## 3     4.160
## 7     3.174
## 9     1.377
## 1     1.075
## 5     0.060
## [1] "Total Elapsed Time: 352.604 secs"
```

![](NYTBlogs_clusters_files/figure-html/display.session.info-1.png) 

```
##                label step_major step_minor     bgn     end elapsed
## 2   fit.models_1_glm          2          0 256.239 276.330  20.091
## 3 fit.models_1_rpart          3          0 276.330 290.210  13.880
## 1   fit.models_1_bgn          1          0 256.226 256.239   0.013
##   duration
## 2   20.091
## 3   13.880
## 1    0.013
## [1] "Total Elapsed Time: 290.21 secs"
```

![](NYTBlogs_clusters_files/figure-html/display.session.info-2.png) 

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
##  [1] rpart.plot_1.5.2    rpart_4.1-9         ROCR_1.0-7         
##  [4] gplots_2.16.0       caTools_1.17.1      caret_6.0-41       
##  [7] dynamicTreeCut_1.62 proxy_0.4-14        tm_0.6             
## [10] NLP_0.1-6           mice_2.22           lattice_0.20-31    
## [13] Rcpp_0.11.5         plyr_1.8.1          zoo_1.7-12         
## [16] sqldf_0.4-10        RSQLite_1.0.0       DBI_0.3.1          
## [19] gsubfn_0.6-6        proto_0.3-10        reshape2_1.4.1     
## [22] doMC_1.3.3          iterators_1.0.7     foreach_1.4.2      
## [25] doBy_4.5-13         survival_2.38-1     ggplot2_1.0.1      
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
## [31] quantreg_5.11       randomForest_4.6-10 RColorBrewer_1.1-2 
## [34] rmarkdown_0.5.1     scales_0.2.4        slam_0.1-32        
## [37] SparseM_1.6         splines_3.1.3       stringr_0.6.2      
## [40] tools_3.1.3         yaml_2.1.13
```
