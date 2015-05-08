# NYTimes:Blogs:: Popular classification:: Metro
bdanalytics  

**  **    
**Date: (Sun) May 10, 2015**    

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
#suppressPackageStartupMessages(require())
#packageVersion("snow")

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTrain.csv"
glb_newdt_url <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTest.csv"
glb_out_pfx <- "NYTBlogs_Metro_"

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
# NewsDesk = the New York Times desk that produced the story (Business, Culture, Foreign, etc.)
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
    #data.frame(parameter="mtry", min=2, max=4, by=1),
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

![](NYTBlogs_Metro_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 8.904  NA      NA
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
##      NewsDesk SectionName SubsectionName
## 226    Styles                           
## 995                                     
## 2124   TStyle                           
## 3326   TStyle                           
## 4752 Business  Technology               
## 6462  Foreign                           
##                                                   Headline
## 226  For Tavi Gevinson, Fashion Takes a Back Seat, for Now
## 995          Reconsidering What to Call an Extremist Group
## 2124          Paris Fashion Week: Kenzo Spring/Summer 2015
## 3326                              The Portable Blue Bottle
## 4752     Monster Moves to Restore a Faded Job Search Brand
## 6462      1889: Priest Questions the Meridian of Greenwich
##                                                                                                                                                                                 Snippet
## 226                                                     Tavi Gevinson, the teenage fashion star turned Broadway actress, wont be much of a player at New York Fashion Week this season.
## 995                                                         Editors have decided to adjust how The Times refer to an Islamic extremist group that controls territory in Syria and Iraq.
## 2124                                                                                                                 Scenes from the Paris Fashion Week photo diary of Nina Westervelt.
## 3326                                                                           The coffee purveyor has teamed up with its fellow Bay Area-based company Timbuk2 to create a travel kit.
## 4752 Monster, which revolutionized online job hunting in the 1990s, is trying to reinvent itself for the era of Twitter and Facebook with new products that capitalize on social media.
## 6462                                                                                From the International Herald Tribune archives: Priest Questions the Meridian of Greenwich in 1889.
##                                                                                                                                                                                Abstract
## 226                                                     Tavi Gevinson, the teenage fashion star turned Broadway actress, wont be much of a player at New York Fashion Week this season.
## 995                                                         Editors have decided to adjust how The Times refer to an Islamic extremist group that controls territory in Syria and Iraq.
## 2124                                                                                                                 Scenes from the Paris Fashion Week photo diary of Nina Westervelt.
## 3326                                                                           The coffee purveyor has teamed up with its fellow Bay Area-based company Timbuk2 to create a travel kit.
## 4752 Monster, which revolutionized online job hunting in the 1990s, is trying to reinvent itself for the era of Twitter and Facebook with new products that capitalize on social media.
## 6462                                                                                From the International Herald Tribune archives: Priest Questions the Meridian of Greenwich in 1889.
##      WordCount             PubDate Popular UniqueID
## 226        459 2014-09-04 16:55:57       0      226
## 995        301 2014-09-15 16:05:13       0      995
## 2124        59 2014-09-28 11:20:02       0     2124
## 3326       248 2014-10-14 14:45:55       0     3326
## 4752       995 2014-11-02 07:00:31       0     4752
## 6462       110 2014-11-27 12:00:34       0     6462
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
##      NewsDesk      SectionName SubsectionName
## 3    Business Crosswords/Games               
## 725    TStyle                                
## 731  Business     Business Day       Dealbook
## 751    TStyle                                
## 864                                          
## 1376 Business     Business Day Small Business
##                                                                           Headline
## 3                                                      Drinking Buddy For Falstaff
## 725                                              Ansel Elgort Buttons Up in Brioni
## 731                    Didi Dache, a Chinese Ride-Hailing App, Raises $700 Million
## 751        The Daily Gift: A Soft, Colorful Quilt From a Brooklyn Fashion Favorite
## 864                                                              Today in Politics
## 1376 As Health Insurance Evolves, Traditional Brokers Claim They Still Have a Role
##                                                                                                                                                                                            Snippet
## 3                                                                                                                                                  In which Timothy Polin reveals his potty mouth.
## 725                                                                                                   The actor brought a tinge of youthfulness to the classic Italian houses retro-tailored look.
## 731  The Singapore investor Temasek and the Chinese social network operator Tencent are among the leaders of the fund-raising round for a company that says it has 10 times the ridership of Uber.
## 751                                                                                                                      Each day until Christmas, the editors of T share a new holiday gift idea.
## 864                                                     The 113th Congress is concluding with partisan brinksmanship and one last mad scramble for votes to pass a $1.1 trillion spending package.
## 1376                       Its complex picking insurance for yourself and your family, said a health care policy director for a small-business organization. Its even more complex for a business.
##                                                                                                                                                                                           Abstract
## 3                                                                                                                                                  In which Timothy Polin reveals his potty mouth.
## 725                                                                                                   The actor brought a tinge of youthfulness to the classic Italian houses retro-tailored look.
## 731  The Singapore investor Temasek and the Chinese social network operator Tencent are among the leaders of the fund-raising round for a company that says it has 10 times the ridership of Uber.
## 751                                                                                                                      Each day until Christmas, the editors of T share a new holiday gift idea.
## 864                                                     The 113th Congress is concluding with partisan brinksmanship and one last mad scramble for votes to pass a $1.1 trillion spending package.
## 1376                       Its complex picking insurance for yourself and your family, said a health care policy director for a small-business organization. Its even more complex for a business.
##      WordCount             PubDate UniqueID
## 3          788 2014-12-01 22:00:26     6535
## 725         89 2014-12-10 12:30:47     7257
## 731        724 2014-12-10 12:06:32     7263
## 751         85 2014-12-10 09:00:38     7283
## 864       1544 2014-12-11 07:09:25     7396
## 1376      1250 2014-12-18 07:00:05     7908
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
## 1  import.data          1          0  8.904 10.224   1.321
## 2 inspect.data          2          0 10.225     NA      NA
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

![](NYTBlogs_Metro_files/figure-html/inspect.data-1.png) 

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

![](NYTBlogs_Metro_files/figure-html/inspect.data-2.png) 

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
myextract_dates_df <- function(df, vars) {
    for (var in vars) {
        dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df[, paste0(var, ".year")] <- as.numeric(format(dates_df$.date, "%Y"))
        dates_df[, paste0(var, ".month.fctr")] <- as.factor(format(dates_df$.date, "%m")) 
        dates_df[, paste0(var, ".date.fctr")] <- cut(as.numeric(format(dates_df$.date, "%d")), 5) # by week    
        dates_df[, paste0(var, ".wkday.fctr")] <- as.factor(format(dates_df$.date, "%w")) 
        dates_df[, paste0(var, ".hour")] <- as.numeric(format(dates_df$.date, "%H"))
        dates_df[, paste0(var, ".apm.fctr")] <- as.factor(ifelse(dates_df[, paste0(var, ".hour")] < 12, "am", "pm"))                                        
        dates_df[, paste0(var, ".minute")] <- as.numeric(format(dates_df$.date, "%M"))                                                                    
        dates_df[, paste0(var, ".second")] <- as.numeric(format(dates_df$.date, "%S")) 
    }
    #myprint_df(dates_df)
    return(subset(dates_df, select=-.date))
}

if (!is.null(glb_date_vars)) {
    glb_entity_df <- cbind(glb_entity_df, 
                           myextract_dates_df(glb_entity_df, glb_date_vars))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_date_vars)
}

# check distribution of all numeric data
dsp_numeric_vars_dstrb <- function(vars_lst) {
    for (var in vars_lst) {
        gp <- myplot_box(df=glb_entity_df, ycol_names=var, xcol_name=glb_rsp_var)
        if (inherits(glb_entity_df[, var], "factor"))
            gp <- gp + facet_wrap(reformulate(var))
        print(gp)
    }    
}
dsp_numeric_vars_dstrb(setdiff(names(glb_entity_df), 
                                union(myfind_chr_cols_df(glb_entity_df), 
                                      c(glb_rsp_var_raw, glb_rsp_var))))                                      
```

![](NYTBlogs_Metro_files/figure-html/inspect.data-3.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-4.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-5.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-6.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-7.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-8.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-9.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-10.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-11.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-12.png) 

```r
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

![](NYTBlogs_Metro_files/figure-html/inspect.data-13.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-14.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-15.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-16.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-17.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-18.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-19.png) ![](NYTBlogs_Metro_files/figure-html/inspect.data-20.png) 

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

glb_chunks_df <- myadd_chunk(glb_chunks_df, "cleanse.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0 10.225 28.893  18.668
## 3 cleanse.data          2          1 28.894     NA      NA
```

### Step `2.1: cleanse data`

```r
dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##          WordCount            Popular           UniqueID 
##                  0               1870                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##               1870                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ 0s in : "
##          WordCount            Popular           UniqueID 
##                109               5439                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                378                159 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0               1344                141 
##      WordCount.log             .rnorm 
##                109                  0 
## [1] "numeric data w/ Infs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ NaNs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
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
##          WordCount            Popular           UniqueID 
##                  0               1870                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##               1870                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                109                  0 
## [1] "numeric data w/ 0s in : "
##          WordCount            Popular           UniqueID 
##                109               5439                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                378                159 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0               1344                141 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ Infs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ NaNs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
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
#dsp_tbl(NewsDesk="", SectionName="", Headline.contains="Boehner")
dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_entity_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
dsp_hdlxtab("(1914)|(1939)")
```

```
##     "Headline.pfx"
## 1     Headline.pfx
## 2     Headline.pfx
## 3     Headline.pfx
## 4     Headline.pfx
## 5     Headline.pfx
## 6     Headline.pfx
## 7     Headline.pfx
## 8     Headline.pfx
## 9     Headline.pfx
## 10    Headline.pfx
## 11    Headline.pfx
## 12    Headline.pfx
## 13    Headline.pfx
## 14    Headline.pfx
## 15    Headline.pfx
## 16    Headline.pfx
## 17    Headline.pfx
## 18    Headline.pfx
## 19    Headline.pfx
## 20    Headline.pfx
## 21    Headline.pfx
## 22    Headline.pfx
## 23    Headline.pfx
## 24    Headline.pfx
## 25    Headline.pfx
## 26    Headline.pfx
## 27    Headline.pfx
## 28    Headline.pfx
## 29    Headline.pfx
## 30    Headline.pfx
## 31    Headline.pfx
## 32    Headline.pfx
## 33    Headline.pfx
## 34    Headline.pfx
## 35    Headline.pfx
## 36    Headline.pfx
## 37    Headline.pfx
## 38    Headline.pfx
## 39    Headline.pfx
## 40    Headline.pfx
## 41    Headline.pfx
## 42    Headline.pfx
## 43    Headline.pfx
## 44    Headline.pfx
## 45    Headline.pfx
## 46    Headline.pfx
## 47    Headline.pfx
## 48    Headline.pfx
## 49    Headline.pfx
## 50    Headline.pfx
## 51    Headline.pfx
## 52    Headline.pfx
## 53    Headline.pfx
## 54    Headline.pfx
## 55    Headline.pfx
## 56    Headline.pfx
## 57    Headline.pfx
## 58    Headline.pfx
## 59    Headline.pfx
## 60    Headline.pfx
## 61    Headline.pfx
## 62    Headline.pfx
## 63    Headline.pfx
## 64    Headline.pfx
## 65    Headline.pfx
## 66    Headline.pfx
## 67    Headline.pfx
## 68    Headline.pfx
## 69    Headline.pfx
## 70    Headline.pfx
## 71    Headline.pfx
## 72    Headline.pfx
## 73    Headline.pfx
## 74    Headline.pfx
## 75    Headline.pfx
## 76    Headline.pfx
## 77    Headline.pfx
## 78    Headline.pfx
## 79    Headline.pfx
## 80    Headline.pfx
## 81    Headline.pfx
## 82    Headline.pfx
## 83    Headline.pfx
## 84    Headline.pfx
## 85    Headline.pfx
## 86    Headline.pfx
## 87    Headline.pfx
## 88    Headline.pfx
## 89    Headline.pfx
## 90    Headline.pfx
## 91    Headline.pfx
## 92    Headline.pfx
## 93    Headline.pfx
## 94    Headline.pfx
## 95    Headline.pfx
## 96    Headline.pfx
## 97    Headline.pfx
## 98    Headline.pfx
## 99    Headline.pfx
## 100   Headline.pfx
## 101   Headline.pfx
## 102   Headline.pfx
## 103   Headline.pfx
## 104   Headline.pfx
## 105   Headline.pfx
## 106   Headline.pfx
## 107   Headline.pfx
## 108   Headline.pfx
## 109   Headline.pfx
## 110   Headline.pfx
## 111   Headline.pfx
## 112   Headline.pfx
## 113   Headline.pfx
## 114   Headline.pfx
## 115   Headline.pfx
## 116   Headline.pfx
## 117   Headline.pfx
## 118   Headline.pfx
## 119   Headline.pfx
## 120   Headline.pfx
## 121   Headline.pfx
##                                                                           Headline
## 1                                                     1914: Turkish Desire for War
## 2                                                 1939: Letters Reaffirm City Ties
## 3                                                 1939: Nazis' War Production Hurt
## 4                                                  1914: 'City of Light' to Return
## 5                                              1914: 79 Die in Brutal German Raid 
## 6                                             1914: Allies Advance in West Africa 
## 7                                                 1914: Antwerp Is Left in Flames 
## 8                                                     1914: Armored Train Surprise
## 9                                                 1914: Belgians Flood Battlefield
## 10                             1914: Big Guns From Pola to Defend Austrian Capital
## 11                                      1914: Bomb-Dropping Warplane Attacks Paris
## 12                              1914: British Prime Minister Urges Irish to Enlist
## 13                                                 1914: British Take Orange River
## 14                                             1914: British War Funds and Troops 
## 15                                                 1914: Christmas Shows in London
## 16                                                    1914: Christmas at the Front
## 17                             1914: Churches Of Ypres Are Targets For German Guns
## 18                                             1914: City Prepares for War Wounded
## 19                          1914: Despite War, 'New York Herald' to Stay in Paris 
## 20          1914: Fearing Espionage, Officials in London Search Travelers' Luggage
## 21                                     1914: France Silences Enemy Guns Near Arras
## 22                                       1914: France&rsquo;s Champagne Prospects 
## 23                                   1914: French Press Misjudges U.S. Ambassadors
## 24                                                1914: General De Wet Is Captured
## 25                                            1914: German Flags Hung in Invalides
## 26                                       1914: German Offensive Checked in Belgium
## 27                                              1914: Germans Attack Antwerp Forts
## 28                                                     1914: Germans Bring Up Guns
## 29                                      1914: Germans Piloting Turkish Aeroplanes 
## 30                                                  1914: Germans Waste Ammunition
## 31                                                    1914: Germany Attacks Tahiti
## 32                                              1914: Hospital-Ship Lost Off Coast
## 33                                         1914: Hotel Seizes Princess&rsquo;s Art
## 34                                            1914: India's Millions Loyal to Core
## 35                                               1914: Indian Infantry Routs Turks
## 36                                              1914: Is Hairdresser a German Spy?
## 37                                            1914: Italian Mobilization Expected 
## 38                                         1914: Italy's Foreign Minister Is Dead 
## 39                                               1914: King George Addresses Army 
## 40                                             1914: King Opens 'Khaki' Parliament
## 41                        1914: M. Henri Pol Will Go On Caring for Paris Sparrows 
## 42                                             1914: Naval Squadron Shells Belgium
## 43                                                   1914: Paris Auto Trade Stalls
## 44                                                  1914: Paris Theatres to Reopen
## 45    1914: Parisians Flock to Cemeteries on All Souls' Day to Honor Dead Soldiers
## 46                              1914: Prince of Wales Passes Busy Day at the Front
## 47                                              1914: Prisoners Escape Paris Crowd
## 48                                               1914: Republicans Sweep Elections
## 49                   1914: Royal Navy Stages Joint Sea and Air Strike on Cuxhaven 
## 50                                               1914: Russian Army Scores Victory
## 51                                          1914: Russians Dominate in East Poland
## 52                                              1914: Scandinavian Alliance Formed
## 53                           1914: Seizure of Oil Steamer Interests United States 
## 54                                                    1914: Sinking of the Nrnberg
## 55                         1914: Sir Ernest Shackleton Outlines His Polar Projects
## 56                                                 1914: Tipperary Song Is a Hit  
## 57                                                  1914: Turcos Drive Germans Out
## 58                                     1914: War May Rise Price of Hats in America
## 59                                                 1914: Wounded May Go to Riviera
## 60                                                  1914: Zeppelin Danger a Bluff 
## 61                                  1939: "Stanley and Livingstone" Opens in Paris
## 62                                                1939: 'Nazi Spy' Named Best Film
## 63                                                1939: 5 Convicted in Stock Fraud
## 64                            1939: 7,000,000 More Cars Predicted on U.S. Highways
## 65                                                 1939: Advice on Heating Issued 
## 66                                                  1939: Allies Seize Contraband 
## 67                1939: American Ships Hasten to Sail Before New Bill Takes Effect
## 68                                                 1939: Australian Flyers Arrive 
## 69                                            1939: British Unmask Reich Defenses 
## 70                                                  1939: Convict Who Fled Returns
## 71                                                   1939: Crowds Return to Paris 
## 72                                        1939: Eleanor Roosevelt Ready to Testify
## 73                                               1939: Empire Air Accord Is Signed
## 74                                                1939: Fighting on Western Front 
## 75                                            1939: Film Industry Revives in Paris
## 76                                           1939: Finns Resist Soviet Aggression 
## 77          1939: France Aims for &lsquo;Total Peace&rsquo;, Says Finance Minister
## 78                                             1939: France Extends Fortifications
## 79                             1939: France Recalls War Premier Georges Clemenceau
## 80                                               1939: French Army in High Spirits
## 81                                                      1939: French Ban Communism
## 82                               1939: French Join Americans in Thanksgiving Rites
## 83                                                 1939: French Occupy German Soil
## 84                    1939: German Battleship Is Badly Damaged by British Cruisers
## 85                                              1939: German Troops Invade Poland 
## 86                                               1939: Ginsberg Name Change Denied
## 87                                            1939: Greatest Opera Fears Eviction 
## 88                                                     1939: Hitler Appeals to God
## 89                                            1939: Hospital Designated Auxiliary 
## 90                                                 1939: Hungary to Defend Europe 
## 91                                            1939: Jamming of BBC Radio Addressed
## 92                                        1939: Light at Night in Paris criticized
## 93                                              1939: Line of Demarcation Decided 
## 94                                                1939: Louvre Hides Art in Vaults
## 95                                                 1939: Mine Sinks Japanese Liner
## 96                                            1939: More Britons Called to Service
## 97                                                 1939: Nazi Raiders Stir London 
## 98                                              1939: Nazi Squadron Flees British 
## 99                                                  1939: Neither King Nor Soldier
## 100                                           1939: Poles Die Under Sovietization 
## 101                                                1939: Polish Gold Reaches Paris
## 102                                          1939: Pont St. Louis Falls Into Seine
## 103                                          1939: Princess Louise Dies in London 
## 104                                           1939: Radio Play Terrifies Hundreds 
## 105                                                    1939: Radio Station Charged
## 106                                            1939: Rain Quiets the Western Front
## 107                        1939: Reich Maps Show France Partitioned, Says Daladier
## 108 1939: Ribbentrop Will Bear Guilt for Tragedy of War, Neville Chamberlain Says 
## 109                                              1939: Roosevelt Signs Neutrality 
## 110                                             1939: Second Meatless Day Is Named
## 111              1939: Sigmund Freud, Psychoanalyst, Dies Refugee in England at 83
## 112                                                 1939: Sir Thomas Cullinan Dies
## 113                                                   1939: Slovaks Are Terrorized
## 114                                                   1939: Soviets Invade Poland 
## 115                                                 1939: Soviets Push Into China 
## 116                                           1939: Textile Rationing Cards Issued
## 117                                  1939: Turks Sign Mutual Aid Pact With Allies 
## 118                                                 1939: Veterans Miss Fox Hunts 
## 119                                             1939: War Inspires Letter-Writing 
## 120                                                  1939: War on Germany Declared
## 121                                              1939: Women Adopt Military Style 
##     Popular.fctr .n
## 1              N  1
## 2              N  1
## 3              N  1
## 4              N  1
## 5           <NA>  1
## 6              N  1
## 7              N  1
## 8              N  1
## 9              N  1
## 10          <NA>  1
## 11             N  1
## 12             N  1
## 13             N  1
## 14             N  1
## 15          <NA>  1
## 16          <NA>  1
## 17             N  1
## 18             N  1
## 19             N  1
## 20             N  1
## 21          <NA>  1
## 22             N  1
## 23             N  1
## 24          <NA>  1
## 25             N  1
## 26             N  1
## 27             N  1
## 28             N  1
## 29             N  1
## 30             N  1
## 31             N  1
## 32             N  1
## 33          <NA>  1
## 34             N  1
## 35             N  1
## 36             N  1
## 37             N  1
## 38             N  1
## 39          <NA>  1
## 40             N  1
## 41             N  1
## 42             N  1
## 43             N  1
## 44             N  1
## 45             N  1
## 46             N  1
## 47             N  1
## 48             N  1
## 49          <NA>  1
## 50             N  1
## 51             N  1
## 52          <NA>  1
## 53             N  1
## 54          <NA>  1
## 55             N  1
## 56          <NA>  1
## 57             N  1
## 58             N  1
## 59          <NA>  1
## 60             N  1
## 61          <NA>  1
## 62          <NA>  1
## 63          <NA>  1
## 64             N  1
## 65             N  1
## 66             N  1
## 67             N  1
## 68          <NA>  1
## 69             N  1
## 70             N  1
## 71             N  1
## 72             N  1
## 73          <NA>  1
## 74             N  1
## 75             N  1
## 76             N  1
## 77             N  1
## 78          <NA>  1
## 79             N  1
## 80             N  1
## 81             N  1
## 82             N  1
## 83             N  1
## 84          <NA>  1
## 85             N  1
## 86          <NA>  1
## 87          <NA>  1
## 88          <NA>  1
## 89             N  1
## 90             N  1
## 91             N  1
## 92             N  1
## 93             N  1
## 94          <NA>  1
## 95             N  1
## 96             N  1
## 97             N  1
## 98             N  1
## 99          <NA>  1
## 100            N  1
## 101            N  1
## 102         <NA>  1
## 103         <NA>  1
## 104            N  1
## 105            N  1
## 106            N  1
## 107            N  1
## 108            N  1
## 109            N  1
## 110         <NA>  1
## 111            N  1
## 112            N  1
## 113            N  1
## 114            N  1
## 115            N  1
## 116            N  1
## 117            N  1
## 118            N  1
## 119            N  1
## 120            N  1
## 121            N  1
```

```r
dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_entity_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
dsp_catxtab("1914)|(1939)")
```

```
##   "Headline.pfx" NewsDesk SectionName SubsectionName Popular.fctr .n
## 1   Headline.pfx  Foreign                                       N 48
## 2   Headline.pfx  Foreign                                    <NA> 13
## 3   Headline.pfx                                             <NA>  2
```

```r
dsp_catxtab("19(14|39|64):")
```

```
##   "Headline.pfx" NewsDesk SectionName SubsectionName Popular.fctr  .n
## 1   Headline.pfx  Foreign                                       N 138
## 2   Headline.pfx  Foreign                                    <NA>  39
## 3   Headline.pfx                                             <NA>   4
## 4   Headline.pfx                                                N   1
```

```r
dsp_catxtab("19..:")
```

```
##   "Headline.pfx" NewsDesk SectionName SubsectionName Popular.fctr  .n
## 1   Headline.pfx  Foreign                                       N 141
## 2   Headline.pfx  Foreign                                    <NA>  39
## 3   Headline.pfx                                                N   9
## 4   Headline.pfx                                             <NA>   4
```

```r
make_prefix <- function(row_ix) {
    # 1/17 is Popular
    #dsp_obs(Headline.contains="Quiz(.*)([?=|]|[?=:])", cols=c("NewsDesk.nb"))
    if (grepl("Quiz(.*)([?=|]|[?=:])", glb_entity_df[row_ix, "Headline"])) 
        return("Quiz(.*)([?=|]|[?=:]::")
    
    words <- unlist(strsplit(glb_entity_df[row_ix, "Headline"], "\\s+"))
    words <- words[words != ""]
    
    # Although 10/10 in trnent is Popular, all of them are contained in
    #   NewsDesk.nb == "Styles" & does not reduce the number of blogs
    #   with NewsDesk.nb == "myMisc::" & Popular = 1
    #dsp_obs(Headline.contains="Quandary(.*)[?=:]", all=TRUE)
    
    # ^ forces match only at the beginning of ths string; [0-9] matches any number
    # All are matched to NewsDesk=[Foreign|]; SectionName=""; SubsectionName=""
    # None are Popular  
    if (grepl("^19[0-9][0-9]:", words[1])) return("19[0-9][0-9]::")
    
    # Only 9 of these & none are Popular
    #if (grepl("NYTLNreads", words[1])) return("NYTLNreads::")
    
    # 6/14 are Popular
    if (grepl("Readers Respond:", glb_entity_df[row_ix, "Headline"])) 
        return("Readers Respond::")
    
    # 4/16 are Popular
    if (grepl("Your Turn:", glb_entity_df[row_ix, "Headline"])) return("Your Turn::")
    
    # 9/18 are Popular
    if (grepl("Ask Well:", glb_entity_df[row_ix, "Headline"])) return("Ask Well::")
    
    # Consolidate all ".*Fashion Week::" since all are Popular=0; & 
    #      NewsDesk="TStyle|Styles|.*|Culture|Metro"; 
    #   SectionName=              ".*|Arts   |N.Y. / Region"; SubsectionName="";
    if (grepl("Fashion Week", glb_entity_df[row_ix, "Headline"])) 
        return(".*Fashion Week::")
    
    # Keep "Daily Clip Report" & "Daily Report" separate"
    #   None are Popular
    #   "Daily Clip Report" -> ""::""::""
    #   "Daily Report" -> "Business"::"Technology"::""
    if (grepl("Daily Clip Report", glb_entity_df[row_ix, "Headline"])) 
        return("Daily Clip Report::")
    if (grepl("Daily Report", glb_entity_df[row_ix, "Headline"])) 
        return("Daily Report::")
    
    # Keep Today in Politics & Today in Small Business separate b/c
    #   Today in Small Business belongs to NewsDesk.nb=Business
    if (grepl("Today in Politics", glb_entity_df[row_ix, "Headline"])) 
        return("Today in Politics::")
    if (grepl("Today in Small Business", glb_entity_df[row_ix, "Headline"])) 
        return("Today in Small Business::")
    
    if (grepl("Pictures of the (Day|Year|.)", glb_entity_df[row_ix, "Headline"])) 
        return("Pictures of the (Day|Year|.)::")
    
    if (words[1] %in% c("Verbatim:")) 
        return(paste0(words[1], ":"))
    if (words[1] %in% c("6", "A", "An", "At", "Daily", "First", "For", "From", 
                        "How", "In", 
                        "Morning", "Milan", "New", "Obama", "On",
                        "Paris", "Pictures", "Q.",
                        "Test", "The", "'The", "Today", 
                        "What", "When", "Why", "Word")) {
        words12 <- paste(words[1], words[2], collapse=" ")
        if (words12 %in% c("Morning Agenda:")) 
            return(paste0(words12, ":"))
        if (words12 %in% c("First Draft", "Test Yourself", "What We're")) 
            return(paste0(words12, "::"))
        if (words12 %in% c("Word of")) return(paste0(words12, " the Day::"))
        if (words12 %in% c("6 Q's")) return(paste0(words12, " About the News::"))

        words123 <- paste(words12, words[3], collapse=" ")
        if (words12 %in% c("New York")) {
            if (words[3] == "Today:") return(paste0(words123, ":"))
            return(words123)
        }
        if (words12 %in% c("The Daily")) {
            if (words[3] %in% c("Gift:")) return(paste0(words12, " Gift::"))
            stop("should not happen")                                                         
        }    
        return(words12)
    } 
    return(words[1])    
}
make_prefix(187)
```

```
## [1] "19[0-9][0-9]::"
```

```r
make_prefix(7984)
```

```
## [1] "Your Turn::"
```

```r
# make_prefix(91)
glb_entity_df$Headline.pfx <- sapply(1:nrow(glb_entity_df), function(row_ix) make_prefix(row_ix))
#myprint_df(glb_entity_df[, c("Headline", "Headline.pfx")])
headline_pfx_df <- mycreate_sqlxtab_df(glb_entity_df[], c("Headline.pfx", glb_rsp_var))
#print(myplot_histogram(headline_pfx_df, ".n"))
print(myplot_hbar(head(headline_pfx_df, 15), "Headline.pfx", ".n", 
                  colorcol_name=glb_rsp_var))
```

![](NYTBlogs_Metro_files/figure-html/cleanse.data-1.png) 

```r
#print(head(orderBy(~-.n + Headline.pfx, headline_pfx_df), 20))
print(head(headline_pfx_df, 20))
```

```
##                      Headline.pfx Popular.fctr  .n
## 1                .*Fashion Week::            N 184
## 2                  19[0-9][0-9]::            N 150
## 3             Daily Clip Report::            N  62
## 4                Morning Agenda::            N  62
## 5          6 Q's About the News::            N  61
## 6                 Test Yourself::            N  61
## 7               Word of the Day::            N  61
## 8                  Daily Report::            N  60
## 9                New York Today::            N  59
## 10                  First Draft::            N  58
## 11      Today in Small Business::            N  58
## 12 Pictures of the (Day|Year|.)::            N  53
## 13                   What We're::            N  45
## 14            Today in Politics::            N  44
## 15                 19[0-9][0-9]::         <NA>  43
## 16                     Verbatim::            N  33
## 17               The Daily Gift::            N  26
## 18               The Daily Gift::         <NA>  24
## 19 Pictures of the (Day|Year|.)::         <NA>  23
## 20            Daily Clip Report::         <NA>  22
```

```r
dsp_catxtab("Today in (Politics|Small Business)")
```

```
##                Headline.pfx NewsDesk  SectionName SubsectionName
## 1 Today in Small Business:: Business Business Day Small Business
## 2       Today in Politics::                                     
## 3       Today in Politics::                                     
## 4 Today in Small Business:: Business Business Day Small Business
## 5 Today in Small Business::          Business Day Small Business
## 6 Today in Small Business:: Business                            
##   Popular.fctr .n
## 1            N 58
## 2            N 44
## 3         <NA> 21
## 4         <NA> 13
## 5         <NA>  1
## 6         <NA>  1
```

```r
dsp_obs(Headline.contains="Today in .", all=TRUE)
```

```
##      UniqueID Popular
## 73         73       0
## 162       162       0
## 260       260       0
## 356       356       0
## 510       510       0
## 607       607       0
## 719       719       0
## 800       800       0
## 883       883       0
## 1024     1024       0
## 1111     1111       0
## 1184     1184       0
## 1281     1281       0
## 1379     1379       0
## 1559     1559       0
## 1654     1654       0
## 1661     1661       0
## 1801     1801       0
## 1829     1829       0
## 1913     1913       0
## 2051     2051       0
## 2194     2194       0
## 2220     2220       0
## 2286     2286       0
## 2324     2324       0
## 2397     2397       0
## 2429     2429       0
## 2501     2501       0
## 2509     2509       0
## 2537     2537       0
## 2605     2605       0
## 2638     2638       0
## 2744     2744       0
## 2773     2773       0
## 2850     2850       0
## 2878     2878       0
## 2939     2939       0
## 2973     2973       0
## 3028     3028       0
## 3071     3071       0
## 3110     3110       0
## 3166     3166       0
## 3251     3251       0
## 3282     3282       0
## 3351     3351       0
## 3374     3374       0
## 3469     3469       0
## 3548     3548       0
## 3569     3569       0
## 3648     3648       0
## 3666     3666       0
## 3791     3791       0
## 3804     3804       0
## 3874     3874       0
## 3905     3905       0
## 3970     3970       0
## 4003     4003       0
## 4057     4057       0
## 4107     4107       0
## 4151     4151       0
## 4203     4203       0
## 4293     4293       0
## 4326     4326       0
## 4392     4392       0
## 4418     4418       0
## 4487     4487       0
## 4520     4520       0
## 4567     4567       0
## 4616     4616       0
## 4678     4678       0
## 4708     4708       0
## 4805     4805       0
## 4829     4829       0
## 4886     4886       0
## 4915     4915       0
## 4974     4974       0
## 4999     4999       0
## 5068     5068       0
## 5107     5107       0
## 5162     5162       0
## 5195     5195       0
## 5288     5288       0
## 5315     5315       0
## 5366     5366       0
## 5396     5396       0
## 5449     5449       0
## 5488     5488       0
## 5547     5547       0
## 5582     5582       0
## 5635     5635       0
## 5676     5676       0
## 5768     5768       0
## 5803     5803       0
## 5862     5862       0
## 5890     5890       0
## 5954     5954       0
## 5985     5985       0
## 6045     6045       0
## 6083     6083       0
## 6155     6155       0
## 6186     6186       0
## 6296     6296       0
## 6371     6371       0
## 6431     6431       0
## 6585     6585      NA
## 6617     6617      NA
## 6668     6668      NA
## 6705     6705      NA
## 6762     6762      NA
## 6797     6797      NA
## 6849     6849      NA
## 6889     6889      NA
## 6954     6954      NA
## 6986     6986      NA
## 7076     7076      NA
## 7104     7104      NA
## 7160     7160      NA
## 7191     7191      NA
## 7261     7261      NA
## 7294     7294      NA
## 7354     7354      NA
## 7396     7396      NA
## 7453     7453      NA
## 7483     7483      NA
## 7563     7563      NA
## 7597     7597      NA
## 7667     7667      NA
## 7688     7688      NA
## 7787     7787      NA
## 7813     7813      NA
## 7880     7880      NA
## 7906     7906      NA
## 7973     7973      NA
## 8002     8002      NA
## 8090     8090      NA
## 8147     8147      NA
## 8191     8191      NA
## 8309     8309      NA
## 8353     8353      NA
## 8397     8397      NA
##                                                                             Headline
## 73                                       Today in Small Business: Made in the U.S.A.
## 162                  Today in Small Business: The Coolest New Businesses in New York
## 260                       Today in Small Business: Suppose Your Company Name Is Isis
## 356                           Today in Small Business: Target and Starbucks Go Small
## 510                            Today in Small Business: Twitter Tests a 'Buy' Button
## 607        Today in Small Business: Best and Worst Cities for Hispanic Entrepreneurs
## 719                                       Today in Small Business: Internet Slowdown
## 800         Today in Small Business: For New S.B.A. Chief, the Honeymoon May Be Over
## 883                                             Today in Small Business: Dying Malls
## 1024                                  Today in Small Business: 30 Start-Ups to Watch
## 1111                               Today in Small Business: The Case Against Tipping
## 1184                  Today in Small Business: When You Don't Love Your Company Name
## 1281           Today in Small Business: The World's Most Mysterious Nutella Emporium
## 1379                                         Today in Small Business: The Bacon Bowl
## 1559                                     Today in Small Business: How a Store Smells
## 1654             Today in Congressional Instagram: The Majority Leader Finds Bigfoot
## 1661                 Today in Small Business: Why Jewelry Stores Hide the Price Tags
## 1801                   Today in Small Business: A Positive Review on Yelp Goes Viral
## 1829                                                               Today in Politics
## 1913                               Today in Small Business: Mobile Is Not a Priority
## 2051                                     Today in Small Business: Unlimited Vacation
## 2194                       Today in Small Business: Facebook Expands Its Ad Platform
## 2220                                                               Today in Politics
## 2286                                      Today in Small Business: Paper or Plastic?
## 2324                                                               Today in Politics
## 2397      Today in Small Business: 'Bloodletting' at Tony Hsieh's Start-Up Community
## 2429                                                               Today in Politics
## 2501                 Today in Small Business: The Coolest New Businesses in Brooklyn
## 2509                         Today in Political #ThrowBackThursday: Bloomberg on Ice
## 2537                                                               Today in Politics
## 2605                                        Today in Small Business: Hiring Picks Up
## 2638                                                               Today in Politics
## 2744                    Today in Small Business: Is the S.B.A. Going Silicon Valley?
## 2773                                                               Today in Politics
## 2850                                Today in Small Business: A Perfect Yelp Response
## 2878                                                               Today in Politics
## 2939                                         Today in Small Business: The Bacon Boom
## 2973                                                               Today in Politics
## 3028                                 Today in Small Business: When Hashtags Backfire
## 3071                                                               Today in Politics
## 3110                                      Today in Small Business: the Rookie Cookie
## 3166                                                               Today in Politics
## 3251                             Today in Small Business: Why Amazon Must Be Stopped
## 3282                                                               Today in Politics
## 3351                              Today in Small Business: Business Travel and Ebola
## 3374                                                               Today in Politics
## 3469                                                               Today in Politics
## 3548               Today in Small Business: Forget R&eacute;sum&eacute;s. Try Videos
## 3569                                                               Today in Politics
## 3648                 Today in Small Business: Paying Retail Employees $50,000 a Year
## 3666                                                               Today in Politics
## 3791 Today in Small Business: How Hackers Can Stick Businesses With Huge Phone Bills
## 3804                                                               Today in Politics
## 3874                      Today in Small Business: Is Apple Pay the Future of Money?
## 3905                                                               Today in Politics
## 3970                                    Today in Small Business: A Lesson in Pricing
## 4003                                                               Today in Politics
## 4057                          Today in Small Business: 'We're the Uber of Whatever!'
## 4107                                                               Today in Politics
## 4151                    Today in Small Business: Dubious Excuses for Calling in Sick
## 4203                                                               Today in Politics
## 4293                Today in Small Business: When the Ebola Virus Touches a Business
## 4326                                                               Today in Politics
## 4392                        Today in Small Business: Start-Ups With a Social Mission
## 4418                                                               Today in Politics
## 4487                        Today in Small Business: Daring to Close on Thanksgiving
## 4520                                                               Today in Politics
## 4567                      Today in Small Business: Jimmy Kimmel Pitches 'Shark Tank'
## 4616                                                               Today in Politics
## 4678                       Today in Small Business: The Halloween Industrial Complex
## 4708                                                               Today in Politics
## 4805                        Today in Small Business: 'The Yelp of Business Software'
## 4829                                                               Today in Politics
## 4886                             Today in Small Business: Minimum Wage and Marijuana
## 4915                                                               Today in Politics
## 4974                                       Today in Small Business: Election Fallout
## 4999                                                               Today in Politics
## 5068                               Today in Small Business: Veteran-Owned Businesses
## 5107                                                               Today in Politics
## 5162                                        Today in Small Business: Paternity Leave
## 5195                                                               Today in Politics
## 5288                             Today in Small Business: Start-Ups Founded by Women
## 5315                                                               Today in Politics
## 5366                                    Today in Small Business: An S.E.O. Challenge
## 5396                                                               Today in Politics
## 5449                       Today in Small Business: Demise of the Internet Sales Tax
## 5488                                                               Today in Politics
## 5547                              Today in Small Business: Avoiding Bad Yelp Reviews
## 5582                                                               Today in Politics
## 5635            Today in Small Business: 'Next Generation of Lender or Boiler Room?'
## 5676                                                               Today in Politics
## 5768                            Today in Small Business: How Costco Codes Its Prices
## 5803                                                               Today in Politics
## 5862                       Today in Small Business: 'Unrealistic Value Expectations'
## 5890                                                               Today in Politics
## 5954                                Today in Small Business: Pastry, Coffee and Cats
## 5985                                                               Today in Politics
## 6045                        Today in Small Business: Why Typewriters Are Coming Back
## 6083                                                               Today in Politics
## 6155                               Today in Small Business: 'Big Cannabis' Is Coming
## 6186                                                               Today in Politics
## 6296                                                               Today in Politics
## 6371                                                               Today in Politics
## 6431                                                               Today in Politics
## 6585                                     Today in Small Business: 'Mean People Fail'
## 6617                                                               Today in Politics
## 6668                      Today in Small Business: Advance Ticketing for Restaurants
## 6705                                                               Today in Politics
## 6762                               Today in Small Business: Pregnancy Discrimination
## 6797                                                               Today in Politics
## 6849                                                  Today in Small Business: Wages
## 6889                                                               Today in Politics
## 6954                        Today in Small Business: The Best Jobs Numbers in Years?
## 6986                                                               Today in Politics
## 7076                               Today in Small Business: Problems With Apple Pay?
## 7104                                                               Today in Politics
## 7160                                    Today in Small Business: The Mistletoe Drone
## 7191                                                               Today in Politics
## 7261                         Today in Small Business: 'Um, I'm Selling the Business'
## 7294                                                               Today in Politics
## 7354                             Today in Small Business: The Best Start-Ups of 2014
## 7396                                                               Today in Politics
## 7453                                 Today in Small Business: The Future of Payments
## 7483                                                               Today in Politics
## 7563                         Today in Small Business: A Retail Success for Instagram
## 7597                                                               Today in Politics
## 7667                         Today in Small Business: Yelp's Gift to Business Owners
## 7688                                                               Today in Politics
## 7787                             Today in Small Business: The Year's Best Franchises
## 7813                                                               Today in Politics
## 7880                       Today in Small Business: The Best Content Marketing Blogs
## 7906                                                               Today in Politics
## 7973                                  Today in Small Business: Fracking and Gambling
## 8002                                                               Today in Politics
## 8090                                                               Today in Politics
## 8147                                                               Today in Politics
## 8191                                                               Today in Politics
## 8309                                                               Today in Politics
## 8353                                                               Today in Politics
## 8397                                                               Today in Politics
```

```r
sav_entity_df <- glb_entity_df
#glb_entity_df <- sav_entity_df

### Mine Headlines with myMisc for patterns & create a separate Headline.pfx category
glb_entity_df$Headline.pfx <- sapply(1:nrow(glb_entity_df), function(row_ix) {
    if (all(tail(unlist(strsplit(glb_entity_df[row_ix, "Headline.pfx"], "")), 2) 
           == c(":", ":"))) return(glb_entity_df[row_ix, "Headline.pfx"])
    
    # myFood:: has to come before myTech:: because of "Apple" conflict
    if (grepl("Thanksgiving", glb_entity_df[row_ix, "Headline"])) return("myFood::")
    if (grepl("Tech |Tech$|Techn[^i]", glb_entity_df[row_ix, "Headline"]))
        return("myTech::")
    if (grepl("Apple|Firefox|Google|Microsoft|Robot|Yahoo", 
              glb_entity_df[row_ix, "Headline"]))
        return("myTech::")
    
    return("myMisc::")
})
#print(glb_entity_df[sel_obs(Headline.pfx="myTech::"), c("UniqueID", "Headline")])
#nrow(glb_entity_df[sel_obs(Headline.pfx="myTech::"), c("UniqueID", "Headline")])
print(mycreate_sqlxtab_df(glb_entity_df, "Headline.pfx"))    
```

```
##                      Headline.pfx   .n
## 1                        myMisc:: 6771
## 2                  19[0-9][0-9]::  193
## 3                .*Fashion Week::  186
## 4                        myTech::  155
## 5             Daily Clip Report::   84
## 6                New York Today::   83
## 7                  Daily Report::   78
## 8                Morning Agenda::   78
## 9                 Test Yourself::   78
## 10              Word of the Day::   78
## 11         6 Q's About the News::   76
## 12 Pictures of the (Day|Year|.)::   76
## 13      Today in Small Business::   73
## 14                  First Draft::   72
## 15            Today in Politics::   65
## 16                   What We're::   57
## 17               The Daily Gift::   50
## 18                     Verbatim::   45
## 19                       myFood::   35
## 20                     Ask Well::   18
## 21              Readers Respond::   18
## 22         Quiz(.*)([?=|]|[?=:]::   17
## 23                    Your Turn::   16
```

```r
dsp_datagrp <- function(..., from=1, to=10, all=FALSE) {
    print((dsp_df <- orderBy(~-.n +Headline.pfx+NewsDesk+SectionName+SubsectionName,
                       mycreate_sqlxtab_df(glb_entity_df[sel_obs(...), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", 
          glb_rsp_var))))[ifelse(all, 1, from):
                          ifelse(all, nrow(dsp_df), min(to, nrow(dsp_df))), ])
}

dsp_datagrp(all=TRUE)
```

```
##                       Headline.pfx NewsDesk      SectionName
## 1                         myMisc::                          
## 2                         myMisc:: Business     Business Day
## 3                         myMisc::  Culture             Arts
## 4                         myMisc::   TStyle                 
## 5                         myMisc::     OpEd          Opinion
## 6                         myMisc:: Business     Business Day
## 7                         myMisc::                          
## 8                         myMisc::  Foreign            World
## 9                         myMisc::  Culture             Arts
## 10                        myMisc:: Business       Technology
## 11                  19[0-9][0-9]::  Foreign                 
## 12                        myMisc::     OpEd          Opinion
## 13                .*Fashion Week::   TStyle                 
## 14                        myMisc::                      U.S.
## 15                        myMisc::    Metro    N.Y. / Region
## 16                        myMisc::   Travel           Travel
## 17                        myMisc::                          
## 18                        myMisc::  Science           Health
## 19                        myMisc::     OpEd          Opinion
## 20                        myMisc:: Business Crosswords/Games
## 21                        myMisc::   Styles             U.S.
## 22                        myMisc::                Multimedia
## 23                        myMisc:: Business     Business Day
## 24                        myMisc::   TStyle                 
## 25                        myMisc:: Business     Business Day
## 26                        myMisc:: Business       Technology
## 27                        myMisc::  Culture                 
## 28                        myMisc::   Styles                 
## 29                        myMisc::   Styles             U.S.
## 30                        myTech:: Business       Technology
## 31             Daily Clip Report::                          
## 32                Morning Agenda:: Business     Business Day
## 33          6 Q's About the News::                      U.S.
## 34                 Test Yourself::                      U.S.
## 35               Word of the Day::                      U.S.
## 36                  Daily Report:: Business       Technology
## 38                        myMisc::                   Opinion
## 39                        myMisc::   Styles             U.S.
## 37                New York Today::    Metro    N.Y. / Region
## 40                   First Draft::                          
## 41       Today in Small Business:: Business     Business Day
## 42                        myMisc::  Foreign            World
## 43  Pictures of the (Day|Year|.)::                Multimedia
## 44                        myMisc::  Science           Health
## 45                        myMisc::  Science           Health
## 46                        myMisc::  Culture             Arts
## 47                .*Fashion Week::   Styles                 
## 48             Today in Politics::                          
## 49                        myMisc::    Metro    N.Y. / Region
## 50                    What We're::                          
## 51                  19[0-9][0-9]::  Foreign                 
## 52                        myMisc:: Business Crosswords/Games
## 53                        myMisc::                      U.S.
## 54                      Verbatim::                          
## 55                        myMisc:: Business       Technology
## 56                        myMisc:: Magazine         Magazine
## 57                        myMisc::   Travel           Travel
## 58                        myMisc::                Multimedia
## 59                The Daily Gift::   TStyle                 
## 60                        myMisc:: Business     Business Day
## 61             Daily Clip Report::                          
## 64                        myMisc::  Foreign                 
## 62  Pictures of the (Day|Year|.)::                Multimedia
## 63                The Daily Gift::   TStyle                 
## 65             Today in Politics::                          
## 67                        myMisc::                   Opinion
## 66                New York Today::    Metro    N.Y. / Region
## 68                        myMisc:: Business Crosswords/Games
## 69                  Daily Report:: Business       Technology
## 70                        myTech:: Business       Technology
## 71                        myTech:: Business       Technology
## 74                        myMisc::     OpEd                 
## 72                 Test Yourself::                      U.S.
## 73               Word of the Day::                      U.S.
## 75                Morning Agenda:: Business     Business Day
## 76                        myMisc::                   Opinion
## 77          6 Q's About the News::                      U.S.
## 78                        myFood::  Science           Health
## 79                   First Draft::                          
## 80                        myMisc::    Metro    N.Y. / Region
## 81                        myTech:: Business     Business Day
## 83                        myMisc::   Styles                 
## 82       Today in Small Business:: Business     Business Day
## 86                        myMisc::              Business Day
## 84          Quiz(.*)([?=|]|[?=:]::                      U.S.
## 85                      Verbatim::                          
## 88                        myMisc::                      Arts
## 87                    What We're::                          
## 89                        myMisc::                   Opinion
## 90                  19[0-9][0-9]::                          
## 91                      Ask Well::  Science           Health
## 93                        myMisc::  Foreign            World
## 92                     Your Turn::   Styles             U.S.
## 94                        myMisc::   TStyle                 
## 95                        myMisc::  Foreign                 
## 96                      Ask Well::  Science           Health
## 97               Readers Respond::                          
## 99                        myMisc::                   Opinion
## 100                       myMisc:: Business     Business Day
## 101                       myTech:: Business     Business Day
## 98               Readers Respond::                          
## 102                 19[0-9][0-9]::                          
## 106                       myMisc::          Crosswords/Games
## 107                       myMisc::                      Open
## 108                       myMisc::                   Opinion
## 109                       myMisc::                    Travel
## 110                       myTech::                          
## 111                       myTech:: Business                 
## 103              Readers Respond::                          
## 104                   What We're::     OpEd          Opinion
## 105                    Your Turn::   Styles             U.S.
## 112                     Ask Well::  Science           Health
## 115                       myFood::                      U.S.
## 116                       myFood::   Styles             U.S.
## 117                       myMisc::              Business Day
## 118                       myMisc::  Foreign            World
## 119                       myMisc:: Magazine         Magazine
## 120                       myTech::                      U.S.
## 121                       myTech:: Business     Business Day
## 113               New York Today::    Metro    N.Y. / Region
## 114         Quiz(.*)([?=|]|[?=:]::                      U.S.
## 125                       myFood::                          
## 126                       myFood::    Metro    N.Y. / Region
## 127                       myFood::   Styles             U.S.
## 128                       myMisc::                Multimedia
## 129                       myMisc::             N.Y. / Region
## 130                       myMisc::                   Opinion
## 131                       myMisc:: National                 
## 132                       myMisc:: National             U.S.
## 133                       myMisc::  Science                 
## 134                       myMisc::  Science                 
## 135                       myMisc::   Styles            Style
## 136                       myTech::                   Opinion
## 137                       myTech::  Culture             Arts
## 138                       myTech::  Culture             Arts
## 139                       myTech::     OpEd          Opinion
## 140                       myTech::   Styles                 
## 122              Readers Respond::                   Opinion
## 123               The Daily Gift::                          
## 124                    Your Turn::   Styles             U.S.
## 141               .*Fashion Week::                          
## 142               .*Fashion Week::  Culture             Arts
## 143               .*Fashion Week::    Metro    N.Y. / Region
## 144               .*Fashion Week::   Styles                 
## 155                       myFood:: Business Crosswords/Games
## 156                       myFood:: Business       Technology
## 157                       myFood::  Culture             Arts
## 158                       myFood::     OpEd          Opinion
## 159                       myFood::     OpEd          Opinion
## 160                       myFood::  Science           Health
## 162                       myFood::   Travel           Travel
## 161                       myFood::   TStyle                 
## 163                       myMisc::              Business Day
## 164                       myMisc::          Crosswords/Games
## 165                       myMisc::                    Health
## 166                       myMisc::                      Open
## 167                       myMisc::                   Opinion
## 168                       myMisc::                Technology
## 169                       myMisc::                    Travel
## 170                       myMisc::                      U.S.
## 171                       myMisc::                      U.S.
## 172                       myMisc::                     World
## 173                       myMisc::  Culture                 
## 174                       myMisc::  Foreign            World
## 175                       myMisc::     OpEd                 
## 176                       myMisc::   Sports                 
## 177                       myMisc::   Sports           Sports
## 178                       myMisc::   Styles                 
## 179                       myMisc::   Styles           Health
## 180                       myMisc::   Travel           Travel
## 181                       myTech::              Business Day
## 182                       myTech::                      U.S.
## 183                       myTech:: Business                 
## 184                       myTech:: Business     Business Day
## 185                       myTech:: Business     Business Day
## 186                       myTech:: Business Crosswords/Games
## 187                       myTech::     OpEd                 
## 188                       myTech::     OpEd          Opinion
## 189                       myTech::  Science           Health
## 190                       myTech::   Styles                 
## 191                       myTech::   TStyle                 
## 192                       myTech::   TStyle                 
## 145               New York Today::             N.Y. / Region
## 146 Pictures of the (Day|Year|.)::    Metro    N.Y. / Region
## 147         Quiz(.*)([?=|]|[?=:]::                          
## 148         Quiz(.*)([?=|]|[?=:]::                          
## 149              Readers Respond::                   Opinion
## 150               The Daily Gift::                          
## 151      Today in Small Business::              Business Day
## 152      Today in Small Business:: Business                 
## 153                   What We're::     OpEd          Opinion
## 154                    Your Turn::                      U.S.
##        SubsectionName Popular.fctr  .n
## 1                                N 908
## 2            Dealbook            N 788
## 3                                N 622
## 4                                N 552
## 5                                Y 404
## 6            Dealbook         <NA> 272
## 7                             <NA> 251
## 8        Asia Pacific            N 200
## 9                             <NA> 160
## 10                               N 155
## 11                               N 141
## 12                            <NA> 140
## 13                               N 136
## 14          Education            N 124
## 15                               N 119
## 16                               N 114
## 17                               Y 109
## 18                               Y 109
## 19                               N 107
## 20                               Y 101
## 21                               Y  94
## 22                               N  86
## 23           Dealbook            Y  83
## 24                            <NA>  83
## 25     Small Business            N  75
## 26                            <NA>  75
## 27                            <NA>  70
## 28                               N  68
## 29                               N  65
## 30                               N  64
## 31                               N  62
## 32           Dealbook            N  62
## 33          Education            N  61
## 34          Education            N  61
## 35          Education            N  61
## 36                               N  60
## 38    Room For Debate            N  59
## 39                            <NA>  59
## 37                               N  59
## 40                               N  58
## 41     Small Business            N  58
## 42       Asia Pacific         <NA>  55
## 43                               N  53
## 44                            <NA>  52
## 45                               N  51
## 46                               Y  50
## 47                               N  46
## 48                               N  44
## 49                            <NA>  43
## 50                               N  41
## 51                            <NA>  39
## 52                            <NA>  38
## 53          Education         <NA>  36
## 54                               N  33
## 55                               Y  32
## 56                               N  31
## 57                            <NA>  31
## 58                            <NA>  30
## 59                               N  25
## 60     Small Business         <NA>  23
## 61                            <NA>  22
## 64                               N  22
## 62                            <NA>  22
## 63                            <NA>  22
## 65                            <NA>  21
## 67    Room For Debate         <NA>  20
## 66                            <NA>  20
## 68                               N  19
## 69                            <NA>  18
## 70                            <NA>  18
## 71                               Y  18
## 74                            <NA>  17
## 72          Education         <NA>  17
## 73          Education         <NA>  17
## 75           Dealbook         <NA>  16
## 76  The Public Editor            Y  16
## 77          Education         <NA>  15
## 78                               N  15
## 79                            <NA>  14
## 80                               Y  14
## 81           Dealbook            N  14
## 83                            <NA>  13
## 82     Small Business         <NA>  13
## 86           Dealbook         <NA>  12
## 84          Education            N  12
## 85                            <NA>  12
## 88                            <NA>  11
## 87                            <NA>  11
## 89  The Public Editor         <NA>  10
## 90                               N   9
## 91                               Y   9
## 93                               N   9
## 92                               N   9
## 94                               Y   8
## 95                            <NA>   7
## 96                               N   6
## 97                               N   6
## 99                            <NA>   5
## 100    Small Business            Y   5
## 101          Dealbook            Y   5
## 98                               Y   5
## 102                           <NA>   4
## 106                           <NA>   4
## 107                              N   4
## 108 The Public Editor            N   4
## 109                           <NA>   4
## 110                              N   4
## 111                              N   4
## 103                           <NA>   4
## 104                              N   4
## 105                              Y   4
## 112                           <NA>   3
## 115         Education            N   3
## 116                              N   3
## 117    Small Business         <NA>   3
## 118      Asia Pacific            Y   3
## 119                           <NA>   3
## 120         Education            N   3
## 121          Dealbook         <NA>   3
## 113                              Y   3
## 114         Education         <NA>   3
## 125                              N   2
## 126                              N   2
## 127                              Y   2
## 128                              Y   2
## 129                           <NA>   2
## 130                              N   2
## 131                              N   2
## 132          Politics            N   2
## 133                           <NA>   2
## 134                              Y   2
## 135   Fashion & Style            N   2
## 136   Room For Debate            N   2
## 137                           <NA>   2
## 138                              N   2
## 139                              Y   2
## 140                              N   2
## 122                              N   2
## 123                           <NA>   2
## 124                           <NA>   2
## 141                              N   1
## 142                           <NA>   1
## 143                              N   1
## 144                           <NA>   1
## 155                              Y   1
## 156                              N   1
## 157                              N   1
## 158                              N   1
## 159                              Y   1
## 160                              Y   1
## 162                              N   1
## 161                              N   1
## 163    Small Business            N   1
## 164                              N   1
## 165                              Y   1
## 166                           <NA>   1
## 167   Room For Debate            Y   1
## 168                           <NA>   1
## 169                              N   1
## 170                           <NA>   1
## 171                              N   1
## 172      Asia Pacific         <NA>   1
## 173                              N   1
## 174                           <NA>   1
## 175                              Y   1
## 176                              N   1
## 177                              N   1
## 178                              Y   1
## 179                              N   1
## 180                              Y   1
## 181          Dealbook         <NA>   1
## 182         Education         <NA>   1
## 183                              Y   1
## 184    Small Business         <NA>   1
## 185    Small Business            N   1
## 186                              Y   1
## 187                           <NA>   1
## 188                              N   1
## 189                              N   1
## 190                           <NA>   1
## 191                              N   1
## 192                              Y   1
## 145                           <NA>   1
## 146                           <NA>   1
## 147                           <NA>   1
## 148                              Y   1
## 149                              Y   1
## 150                              N   1
## 151    Small Business         <NA>   1
## 152                           <NA>   1
## 153                           <NA>   1
## 154                           <NA>   1
```

```r
glb_entity_df[, "NewsDesk.nb"] <- sapply(1:nrow(glb_entity_df), function(rix) {
    if (glb_entity_df[rix, "Headline.pfx"] == ".*Fashion Week::") return("TStyle")
    if (glb_entity_df[rix, "Headline.pfx"] == "Pictures of the (Day|Year|.)::") 
        return("myMultimedia")
        
    if (glb_entity_df[rix, "SectionName"] %in% 
            c("Business Day", "Crosswords/Games", "Technology")) return("Business")
    if (glb_entity_df[rix, "SectionName"] == "Arts") return("Culture")
    if ((glb_entity_df[rix, "SectionName"] == "N.Y. / Region") &
        !(glb_entity_df[rix, "Headline.pfx"] %in% 
              c(".*Fashion Week::", "Pictures of the (Day|Year|.)::")))    
        return("Metro")    
    if (glb_entity_df[rix, "SectionName"] == "Multimedia") return("myMultimedia")
    if (glb_entity_df[rix, "SectionName"] == "Opinion") return("OpEd")
    if (glb_entity_df[rix, "SectionName"] == "Health") return("Science")
    
    if ((str <- glb_entity_df[rix, "NewsDesk"]) != "") return(str)
    
    if (glb_entity_df[rix, "Headline.pfx"] == "What We're::") return("OpEd")    
    if (glb_entity_df[rix, "Headline.pfx"] == "Your Turn::") return("Styles")
    return(glb_entity_df[rix, "Headline.pfx"])
})
mycheck_map_results(glb_entity_df, "NewsDesk", "NewsDesk.nb", print.all=TRUE)
```

```
##    NewsDesk            NewsDesk.nb   .n
## 1  Business               Business 2026
## 2                         myMisc:: 1441
## 3   Culture                Culture  908
## 4    TStyle                 TStyle  829
## 5      OpEd                   OpEd  680
## 6   Foreign                Foreign  477
## 7    Styles                 Styles  325
## 8     Metro                  Metro  260
## 9   Science                Science  251
## 10                    myMultimedia  193
## 11                            OpEd  174
## 12   Travel                 Travel  147
## 13             Daily Clip Report::   84
## 14                 Test Yourself::   78
## 15               Word of the Day::   78
## 16          6 Q's About the News::   76
## 17                   First Draft::   72
## 18             Today in Politics::   65
## 19   Styles                 TStyle   47
## 20                      Verbatim::   45
## 21 Magazine               Magazine   34
## 22                        Business   24
## 23          Quiz(.*)([?=|]|[?=:]::   17
## 24               Readers Respond::   15
## 25                  19[0-9][0-9]::   13
## 26                         Culture   11
## 27                        myTech::    8
## 28                        myFood::    5
## 29 National               National    4
## 30                           Metro    3
## 31                The Daily Gift::    3
## 32   Sports                 Sports    2
## 33                         Science    1
## 34                          Styles    1
## 35                          TStyle    1
## 36  Culture                 TStyle    1
## 37    Metro                 TStyle    1
## 38    Metro           myMultimedia    1
## 39   Styles                Science    1
```

![](NYTBlogs_Metro_files/figure-html/cleanse.data-2.png) 

```r
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, "NewsDesk")

glb_entity_df[, "SectionName.nb"] <- sapply(1:nrow(glb_entity_df), function(rix) {
    if (glb_entity_df[rix, "Headline.pfx"] == ".*Fashion Week::") return("TStyle")
    
    str <- glb_entity_df[rix, "SectionName"]

    if (glb_entity_df[rix, "Headline.pfx"] == "Today in Small Business::")
        return("Business Day")
    
    if (str == "Style") return("Styles")
    if (str != "") return (str)
    
    if (glb_entity_df[rix, "NewsDesk"] == "OpEd") return("Opinion")
    if (glb_entity_df[rix, "NewsDesk.nb"] == "OpEd") return("Opinion")    
    
    if (glb_entity_df[rix, "NewsDesk"] == "Science") return("Health")
    
    return(glb_entity_df[rix, "NewsDesk.nb"])
})
mycheck_map_results(glb_entity_df, "SectionName", "SectionName.nb", print.all=TRUE)
```

```
##         SectionName         SectionName.nb   .n
## 1      Business Day           Business Day 1437
## 2                                 myMisc:: 1268
## 3                                   TStyle  877
## 4              Arts                   Arts  848
## 5           Opinion                Opinion  783
## 6              U.S.                   U.S.  657
## 7        Technology             Technology  442
## 8             World                  World  269
## 9     N.Y. / Region          N.Y. / Region  264
## 10           Health                 Health  249
## 11                                 Foreign  209
## 12       Multimedia             Multimedia  193
## 13 Crosswords/Games       Crosswords/Games  165
## 14           Travel                 Travel  152
## 15                                  Styles   85
## 16                     Daily Clip Report::   84
## 17                           First Draft::   72
## 18                                 Culture   71
## 19                                 Opinion   71
## 20                     Today in Politics::   65
## 21                              Verbatim::   45
## 22         Magazine               Magazine   34
## 23                       Readers Respond::   15
## 24                          19[0-9][0-9]::   13
## 25                                Business    5
## 26             Open                   Open    5
## 27                                  Health    4
## 28                                myTech::    4
## 29                        The Daily Gift::    3
## 30                                National    2
## 31                  Quiz(.*)([?=|]|[?=:]::    2
## 32                                myFood::    2
## 33            Style                 Styles    2
## 34                            Business Day    1
## 35                                  Sports    1
## 36             Arts                 TStyle    1
## 37    N.Y. / Region                 TStyle    1
## 38           Sports                 Sports    1
```

![](NYTBlogs_Metro_files/figure-html/cleanse.data-3.png) 

```r
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, "SectionName")

glb_entity_df[, "SubsectionName.nb"] <- sapply(1:nrow(glb_entity_df), function(rix) {
    if ((str <- glb_entity_df[rix, "SubsectionName"]) != "") return(str)
    
    if (glb_entity_df[rix, "NewsDesk.nb"] == "Styles") return("Fashion & Style")
    
    if (glb_entity_df[rix, "Headline.pfx"] == "Today in Small Business::")
        return("Small Business")
    
    str <- paste(glb_entity_df[rix, "NewsDesk.nb"], 
                 glb_entity_df[rix, "SectionName.nb"], 
        sep=ifelse(grepl("::$", glb_entity_df[rix, "NewsDesk.nb"]), "", "::"))
    return(str)
})
mycheck_map_results(glb_entity_df, "SubsectionName", "SubsectionName.nb")
```

```
##   SubsectionName    SubsectionName.nb   .n
## 1                    myMisc::myMisc:: 1268
## 2       Dealbook             Dealbook 1256
## 3                      TStyle::TStyle  879
## 4                       Culture::Arts  848
## 5                       OpEd::Opinion  742
## 6                Business::Technology  442
##       SubsectionName                      SubsectionName.nb  .n
## 15                               Business::Crosswords/Games 165
## 21                   Today in Politics::Today in Politics::  65
## 24 The Public Editor                      The Public Editor  30
## 32                         The Daily Gift::The Daily Gift::   3
## 36                                         myFood::myFood::   2
## 41                              myMultimedia::N.Y. / Region   1
##     SubsectionName           SubsectionName.nb .n
## 36                            myFood::myFood::  2
## 37                                myMisc::U.S.  2
## 38 Fashion & Style             Fashion & Style  2
## 39        Politics                    Politics  2
## 40                              Small Business  1
## 41                 myMultimedia::N.Y. / Region  1
```

![](NYTBlogs_Metro_files/figure-html/cleanse.data-4.png) 

```r
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, "SubsectionName")

hdlpfx_xtab_df <- orderBy(reformulate(c("Headline.pfx", glb_rsp_var, "-", ".n")),
                          mycreate_sqlxtab_df(glb_entity_df,
                                                c("Headline.pfx", glb_rsp_var)))
myprint_df(hdlpfx_xtab_df)
```

```
##              Headline.pfx Popular.fctr  .n
## 4        .*Fashion Week::            N 184
## 52       .*Fashion Week::         <NA>   2
## 5          19[0-9][0-9]::            N 150
## 19         19[0-9][0-9]::         <NA>  43
## 9  6 Q's About the News::            N  61
## 34 6 Q's About the News::         <NA>  15
##                      Headline.pfx Popular.fctr .n
## 7             Daily Clip Report::            N 62
## 12                 Daily Report::            N 60
## 23                       myTech::            Y 28
## 16 Pictures of the (Day|Year|.)::            N 53
## 24               The Daily Gift::            N 26
## 32              Word of the Day::         <NA> 17
##         Headline.pfx Popular.fctr .n
## 39      What We're::         <NA> 12
## 11 Word of the Day::            N 61
## 32 Word of the Day::         <NA> 17
## 41       Your Turn::            N  9
## 48       Your Turn::            Y  4
## 51       Your Turn::         <NA>  3
```

```r
newsdesk_xtab_df <- orderBy(reformulate(
    c("NewsDesk.nb", "NewsDesk",
      "Headline.pfx", glb_rsp_var, "-", ".n")),
                          mycreate_sqlxtab_df(glb_entity_df,
    c("NewsDesk.nb", "NewsDesk", 
      "Headline.pfx", glb_rsp_var)))
myprint_df(newsdesk_xtab_df)
```

```
##               NewsDesk.nb NewsDesk           Headline.pfx Popular.fctr .n
## 73         19[0-9][0-9]::                  19[0-9][0-9]::            N  9
## 82         19[0-9][0-9]::                  19[0-9][0-9]::         <NA>  4
## 29 6 Q's About the News::          6 Q's About the News::            N 61
## 64 6 Q's About the News::          6 Q's About the News::         <NA> 15
## 93               Business                        myMisc::            N  2
## 57               Business                        myMisc::         <NA> 20
##           NewsDesk.nb NewsDesk      Headline.pfx Popular.fctr  .n
## 32           Business Business    Daily Report::            N  60
## 115              OpEd     OpEd          myFood::            N   1
## 11               OpEd     OpEd          myMisc::         <NA> 157
## 84               OpEd     OpEd      What We're::            N   4
## 80  Readers Respond::          Readers Respond::            Y   5
## 101            Sports   Sports          myMisc::            N   2
##          NewsDesk.nb NewsDesk      Headline.pfx Popular.fctr .n
## 51            TStyle   TStyle  The Daily Gift::            N 25
## 54            TStyle   TStyle  The Daily Gift::         <NA> 22
## 46        Verbatim::                 Verbatim::            N 33
## 70        Verbatim::                 Verbatim::         <NA> 12
## 31 Word of the Day::          Word of the Day::            N 61
## 62 Word of the Day::          Word of the Day::         <NA> 17
```

```r
ndsection_xtab_df <- orderBy(reformulate(
    c("NewsDesk.nb", "NewsDesk", "SectionName.nb", "SectionName", 
      "Headline.pfx", glb_rsp_var, "-", ".n")),
                          mycreate_sqlxtab_df(glb_entity_df,
    c("NewsDesk.nb", "NewsDesk", "SectionName.nb", "SectionName", 
      "Headline.pfx", glb_rsp_var)))
print(ndsection_xtab_df)
```

```
##                NewsDesk.nb NewsDesk         SectionName.nb
## 87          19[0-9][0-9]::                  19[0-9][0-9]::
## 96          19[0-9][0-9]::                  19[0-9][0-9]::
## 33  6 Q's About the News::                            U.S.
## 75  6 Q's About the News::                            U.S.
## 133               Business                    Business Day
## 76                Business                    Business Day
## 134               Business                    Business Day
## 132               Business                    Business Day
## 135               Business                Crosswords/Games
## 97                Business                Crosswords/Games
## 136               Business                      Technology
## 98                Business Business               Business
## 137               Business Business               Business
## 138               Business Business           Business Day
## 31                Business Business           Business Day
## 74                Business Business           Business Day
## 2                 Business Business           Business Day
## 22                Business Business           Business Day
## 6                 Business Business           Business Day
## 77                Business Business           Business Day
## 94                Business Business           Business Day
## 99                Business Business           Business Day
## 39                Business Business           Business Day
## 81                Business Business           Business Day
## 139               Business Business       Crosswords/Games
## 66                Business Business       Crosswords/Games
## 20                Business Business       Crosswords/Games
## 51                Business Business       Crosswords/Games
## 140               Business Business       Crosswords/Games
## 36                Business Business             Technology
## 67                Business Business             Technology
## 141               Business Business             Technology
## 10                Business Business             Technology
## 55                Business Business             Technology
## 25                Business Business             Technology
## 30                Business Business             Technology
## 69                Business Business             Technology
## 68                Business Business             Technology
## 85                 Culture                            Arts
## 142                Culture  Culture                   Arts
## 3                  Culture  Culture                   Arts
## 45                 Culture  Culture                   Arts
## 9                  Culture  Culture                   Arts
## 115                Culture  Culture                   Arts
## 114                Culture  Culture                   Arts
## 143                Culture  Culture                Culture
## 26                 Culture  Culture                Culture
## 32     Daily Clip Report::             Daily Clip Report::
## 60     Daily Clip Report::             Daily Clip Report::
## 40           First Draft::                   First Draft::
## 79           First Draft::                   First Draft::
## 11                 Foreign  Foreign                Foreign
## 50                 Foreign  Foreign                Foreign
## 61                 Foreign  Foreign                Foreign
## 91                 Foreign  Foreign                Foreign
## 8                  Foreign  Foreign                  World
## 106                Foreign  Foreign                  World
## 41                 Foreign  Foreign                  World
## 56                Magazine Magazine               Magazine
## 107               Magazine Magazine               Magazine
## 116                  Metro                   N.Y. / Region
## 144                  Metro                   N.Y. / Region
## 117                  Metro    Metro          N.Y. / Region
## 15                   Metro    Metro          N.Y. / Region
## 80                   Metro    Metro          N.Y. / Region
## 48                   Metro    Metro          N.Y. / Region
## 37                   Metro    Metro          N.Y. / Region
## 108                  Metro    Metro          N.Y. / Region
## 65                   Metro    Metro          N.Y. / Region
## 130               myFood::                        myFood::
## 112               myFood::                            U.S.
## 1                 myMisc::                        myMisc::
## 18                myMisc::                        myMisc::
## 7                 myMisc::                        myMisc::
## 103               myMisc::                            Open
## 173               myMisc::                            Open
## 174               myMisc::                          Travel
## 104               myMisc::                          Travel
## 14                myMisc::                            U.S.
## 52                myMisc::                            U.S.
## 175               myMisc::                           World
## 23            myMultimedia                      Multimedia
## 131           myMultimedia                      Multimedia
## 58            myMultimedia                      Multimedia
## 42            myMultimedia                      Multimedia
## 63            myMultimedia                      Multimedia
## 176           myMultimedia    Metro          N.Y. / Region
## 105               myTech::                        myTech::
## 113               myTech::                            U.S.
## 177               myTech::                            U.S.
## 118               National National               National
## 119               National National                   U.S.
## 49                    OpEd                         Opinion
## 86                    OpEd                         Opinion
## 28                    OpEd                         Opinion
## 70                    OpEd                         Opinion
## 53                    OpEd                         Opinion
## 121                   OpEd                         Opinion
## 120                   OpEd                         Opinion
## 145                   OpEd                         Opinion
## 146                   OpEd     OpEd                Opinion
## 71                    OpEd     OpEd                Opinion
## 147                   OpEd     OpEd                Opinion
## 149                   OpEd     OpEd                Opinion
## 150                   OpEd     OpEd                Opinion
## 19                    OpEd     OpEd                Opinion
## 5                     OpEd     OpEd                Opinion
## 12                    OpEd     OpEd                Opinion
## 151                   OpEd     OpEd                Opinion
## 122                   OpEd     OpEd                Opinion
## 100                   OpEd     OpEd                Opinion
## 148                   OpEd     OpEd                Opinion
## 153 Quiz(.*)([?=|]|[?=:]::          Quiz(.*)([?=|]|[?=:]::
## 152 Quiz(.*)([?=|]|[?=:]::          Quiz(.*)([?=|]|[?=:]::
## 83  Quiz(.*)([?=|]|[?=:]::                            U.S.
## 109 Quiz(.*)([?=|]|[?=:]::                            U.S.
## 92       Readers Respond::               Readers Respond::
## 95       Readers Respond::               Readers Respond::
## 101      Readers Respond::               Readers Respond::
## 154                Science                          Health
## 124                Science  Science                 Health
## 123                Science  Science                 Health
## 93                 Science  Science                 Health
## 88                 Science  Science                 Health
## 110                Science  Science                 Health
## 78                 Science  Science                 Health
## 155                Science  Science                 Health
## 44                 Science  Science                 Health
## 17                 Science  Science                 Health
## 43                 Science  Science                 Health
## 156                Science  Science                 Health
## 157                Science   Styles                 Health
## 158                 Sports   Sports                 Sports
## 159                 Sports   Sports                 Sports
## 160                 Styles                            U.S.
## 27                  Styles   Styles                 Styles
## 161                 Styles   Styles                 Styles
## 82                  Styles   Styles                 Styles
## 125                 Styles   Styles                 Styles
## 162                 Styles   Styles                 Styles
## 126                 Styles   Styles                 Styles
## 111                 Styles   Styles                   U.S.
## 128                 Styles   Styles                   U.S.
## 29                  Styles   Styles                   U.S.
## 21                  Styles   Styles                   U.S.
## 38                  Styles   Styles                   U.S.
## 89                  Styles   Styles                   U.S.
## 102                 Styles   Styles                   U.S.
## 127                 Styles   Styles                   U.S.
## 34         Test Yourself::                            U.S.
## 72         Test Yourself::                            U.S.
## 170       The Daily Gift::                The Daily Gift::
## 129       The Daily Gift::                The Daily Gift::
## 47     Today in Politics::             Today in Politics::
## 64     Today in Politics::             Today in Politics::
## 171                 Travel   Travel                 Travel
## 16                  Travel   Travel                 Travel
## 172                 Travel   Travel                 Travel
## 57                  Travel   Travel                 Travel
## 163                 TStyle                          TStyle
## 164                 TStyle  Culture                 TStyle
## 165                 TStyle    Metro                 TStyle
## 46                  TStyle   Styles                 TStyle
## 166                 TStyle   Styles                 TStyle
## 13                  TStyle   TStyle                 TStyle
## 167                 TStyle   TStyle                 TStyle
## 4                   TStyle   TStyle                 TStyle
## 90                  TStyle   TStyle                 TStyle
## 24                  TStyle   TStyle                 TStyle
## 168                 TStyle   TStyle                 TStyle
## 169                 TStyle   TStyle                 TStyle
## 59                  TStyle   TStyle                 TStyle
## 62                  TStyle   TStyle                 TStyle
## 54              Verbatim::                      Verbatim::
## 84              Verbatim::                      Verbatim::
## 35       Word of the Day::                            U.S.
## 73       Word of the Day::                            U.S.
##          SectionName                   Headline.pfx Popular.fctr  .n
## 87                                   19[0-9][0-9]::            N   9
## 96                                   19[0-9][0-9]::         <NA>   4
## 33              U.S.         6 Q's About the News::            N  61
## 75              U.S.         6 Q's About the News::         <NA>  15
## 133     Business Day                       myMisc::            N   1
## 76      Business Day                       myMisc::         <NA>  15
## 134     Business Day                       myTech::         <NA>   1
## 132     Business Day      Today in Small Business::         <NA>   1
## 135 Crosswords/Games                       myMisc::            N   1
## 97  Crosswords/Games                       myMisc::         <NA>   4
## 136       Technology                       myMisc::         <NA>   1
## 98                                         myTech::            N   4
## 137                                        myTech::            Y   1
## 138                       Today in Small Business::         <NA>   1
## 31      Business Day               Morning Agenda::            N  62
## 74      Business Day               Morning Agenda::         <NA>  16
## 2       Business Day                       myMisc::            N 863
## 22      Business Day                       myMisc::            Y  88
## 6       Business Day                       myMisc::         <NA> 295
## 77      Business Day                       myTech::            N  15
## 94      Business Day                       myTech::            Y   5
## 99      Business Day                       myTech::         <NA>   4
## 39      Business Day      Today in Small Business::            N  58
## 81      Business Day      Today in Small Business::         <NA>  13
## 139 Crosswords/Games                       myFood::            Y   1
## 66  Crosswords/Games                       myMisc::            N  19
## 20  Crosswords/Games                       myMisc::            Y 101
## 51  Crosswords/Games                       myMisc::         <NA>  38
## 140 Crosswords/Games                       myTech::            Y   1
## 36        Technology                 Daily Report::            N  60
## 67        Technology                 Daily Report::         <NA>  18
## 141       Technology                       myFood::            N   1
## 10        Technology                       myMisc::            N 155
## 55        Technology                       myMisc::            Y  32
## 25        Technology                       myMisc::         <NA>  75
## 30        Technology                       myTech::            N  64
## 69        Technology                       myTech::            Y  18
## 68        Technology                       myTech::         <NA>  18
## 85              Arts                       myMisc::         <NA>  11
## 142             Arts                       myFood::            N   1
## 3               Arts                       myMisc::            N 622
## 45              Arts                       myMisc::            Y  50
## 9               Arts                       myMisc::         <NA> 160
## 115             Arts                       myTech::            N   2
## 114             Arts                       myTech::         <NA>   2
## 143                                        myMisc::            N   1
## 26                                         myMisc::         <NA>  70
## 32                              Daily Clip Report::            N  62
## 60                              Daily Clip Report::         <NA>  22
## 40                                    First Draft::            N  58
## 79                                    First Draft::         <NA>  14
## 11                                   19[0-9][0-9]::            N 141
## 50                                   19[0-9][0-9]::         <NA>  39
## 61                                         myMisc::            N  22
## 91                                         myMisc::         <NA>   7
## 8              World                       myMisc::            N 209
## 106            World                       myMisc::            Y   3
## 41             World                       myMisc::         <NA>  56
## 56          Magazine                       myMisc::            N  31
## 107         Magazine                       myMisc::         <NA>   3
## 116    N.Y. / Region                       myMisc::         <NA>   2
## 144    N.Y. / Region               New York Today::         <NA>   1
## 117    N.Y. / Region                       myFood::            N   2
## 15     N.Y. / Region                       myMisc::            N 119
## 80     N.Y. / Region                       myMisc::            Y  14
## 48     N.Y. / Region                       myMisc::         <NA>  43
## 37     N.Y. / Region               New York Today::            N  59
## 108    N.Y. / Region               New York Today::            Y   3
## 65     N.Y. / Region               New York Today::         <NA>  20
## 130                                        myFood::            N   2
## 112             U.S.                       myFood::            N   3
## 1                                          myMisc::            N 908
## 18                                         myMisc::            Y 109
## 7                                          myMisc::         <NA> 251
## 103             Open                       myMisc::            N   4
## 173             Open                       myMisc::         <NA>   1
## 174           Travel                       myMisc::            N   1
## 104           Travel                       myMisc::         <NA>   4
## 14              U.S.                       myMisc::            N 125
## 52              U.S.                       myMisc::         <NA>  37
## 175            World                       myMisc::         <NA>   1
## 23        Multimedia                       myMisc::            N  86
## 131       Multimedia                       myMisc::            Y   2
## 58        Multimedia                       myMisc::         <NA>  30
## 42        Multimedia Pictures of the (Day|Year|.)::            N  53
## 63        Multimedia Pictures of the (Day|Year|.)::         <NA>  22
## 176    N.Y. / Region Pictures of the (Day|Year|.)::         <NA>   1
## 105                                        myTech::            N   4
## 113             U.S.                       myTech::            N   3
## 177             U.S.                       myTech::         <NA>   1
## 118                                        myMisc::            N   2
## 119             U.S.                       myMisc::            N   2
## 49                                     What We're::            N  41
## 86                                     What We're::         <NA>  11
## 28           Opinion                       myMisc::            N  65
## 70           Opinion                       myMisc::            Y  17
## 53           Opinion                       myMisc::         <NA>  35
## 121          Opinion                       myTech::            N   2
## 120          Opinion              Readers Respond::            N   2
## 145          Opinion              Readers Respond::            Y   1
## 146                                        myMisc::            Y   1
## 71                                         myMisc::         <NA>  17
## 147                                        myTech::         <NA>   1
## 149          Opinion                       myFood::            N   1
## 150          Opinion                       myFood::            Y   1
## 19           Opinion                       myMisc::            N 107
## 5            Opinion                       myMisc::            Y 404
## 12           Opinion                       myMisc::         <NA> 140
## 151          Opinion                       myTech::            N   1
## 122          Opinion                       myTech::            Y   2
## 100          Opinion                   What We're::            N   4
## 148          Opinion                   What We're::         <NA>   1
## 153                          Quiz(.*)([?=|]|[?=:]::            Y   1
## 152                          Quiz(.*)([?=|]|[?=:]::         <NA>   1
## 83              U.S.         Quiz(.*)([?=|]|[?=:]::            N  12
## 109             U.S.         Quiz(.*)([?=|]|[?=:]::         <NA>   3
## 92                                Readers Respond::            N   6
## 95                                Readers Respond::            Y   5
## 101                               Readers Respond::         <NA>   4
## 154           Health                       myMisc::            Y   1
## 124                                        myMisc::            Y   2
## 123                                        myMisc::         <NA>   2
## 93            Health                     Ask Well::            N   6
## 88            Health                     Ask Well::            Y   9
## 110           Health                     Ask Well::         <NA>   3
## 78            Health                       myFood::            N  15
## 155           Health                       myFood::            Y   1
## 44            Health                       myMisc::            N  51
## 17            Health                       myMisc::            Y 109
## 43            Health                       myMisc::         <NA>  52
## 156           Health                       myTech::            N   1
## 157           Health                       myMisc::            N   1
## 158                                        myMisc::            N   1
## 159           Sports                       myMisc::            N   1
## 160             U.S.                    Your Turn::         <NA>   1
## 27                                         myMisc::            N  68
## 161                                        myMisc::            Y   1
## 82                                         myMisc::         <NA>  13
## 125                                        myTech::            N   2
## 162                                        myTech::         <NA>   1
## 126            Style                       myMisc::            N   2
## 111             U.S.                       myFood::            N   3
## 128             U.S.                       myFood::            Y   2
## 29              U.S.                       myMisc::            N  65
## 21              U.S.                       myMisc::            Y  94
## 38              U.S.                       myMisc::         <NA>  59
## 89              U.S.                    Your Turn::            N   9
## 102             U.S.                    Your Turn::            Y   4
## 127             U.S.                    Your Turn::         <NA>   2
## 34              U.S.                Test Yourself::            N  61
## 72              U.S.                Test Yourself::         <NA>  17
## 170                                The Daily Gift::            N   1
## 129                                The Daily Gift::         <NA>   2
## 47                              Today in Politics::            N  44
## 64                              Today in Politics::         <NA>  21
## 171           Travel                       myFood::            N   1
## 16            Travel                       myMisc::            N 114
## 172           Travel                       myMisc::            Y   1
## 57            Travel                       myMisc::         <NA>  31
## 163                                .*Fashion Week::            N   1
## 164             Arts               .*Fashion Week::         <NA>   1
## 165    N.Y. / Region               .*Fashion Week::            N   1
## 46                                 .*Fashion Week::            N  46
## 166                                .*Fashion Week::         <NA>   1
## 13                                 .*Fashion Week::            N 136
## 167                                        myFood::            N   1
## 4                                          myMisc::            N 552
## 90                                         myMisc::            Y   8
## 24                                         myMisc::         <NA>  83
## 168                                        myTech::            N   1
## 169                                        myTech::            Y   1
## 59                                 The Daily Gift::            N  25
## 62                                 The Daily Gift::         <NA>  22
## 54                                       Verbatim::            N  33
## 84                                       Verbatim::         <NA>  12
## 35              U.S.              Word of the Day::            N  61
## 73              U.S.              Word of the Day::         <NA>  17
```

```r
ndsecsub_xtab_df <- orderBy(reformulate(
    c("NewsDesk.nb", "NewsDesk", 
      "SectionName.nb", "SectionName", "SubsectionName.nb", "SubsectionName", 
      "Headline.pfx", glb_rsp_var, "-", ".n")),
                          mycreate_sqlxtab_df(glb_entity_df,
    c("NewsDesk.nb", "NewsDesk", 
      "SectionName.nb", "SectionName", "SubsectionName.nb", "SubsectionName", 
      "Headline.pfx", glb_rsp_var)))
myprint_df(ndsecsub_xtab_df)
```

```
##                NewsDesk.nb NewsDesk SectionName.nb  SectionName
## 90          19[0-9][0-9]::          19[0-9][0-9]::             
## 102         19[0-9][0-9]::          19[0-9][0-9]::             
## 33  6 Q's About the News::                    U.S.         U.S.
## 77  6 Q's About the News::                    U.S.         U.S.
## 84                Business            Business Day Business Day
## 141               Business            Business Day Business Day
##                SubsectionName.nb SubsectionName           Headline.pfx
## 90  19[0-9][0-9]::19[0-9][0-9]::                        19[0-9][0-9]::
## 102 19[0-9][0-9]::19[0-9][0-9]::                        19[0-9][0-9]::
## 33                     Education      Education 6 Q's About the News::
## 77                     Education      Education 6 Q's About the News::
## 84                      Dealbook       Dealbook               myMisc::
## 141                     Dealbook       Dealbook               myTech::
##     Popular.fctr .n
## 90             N  9
## 102         <NA>  4
## 33             N 61
## 77          <NA> 15
## 84          <NA> 12
## 141         <NA>  1
##     NewsDesk.nb NewsDesk SectionName.nb   SectionName    SubsectionName.nb
## 124       Metro           N.Y. / Region N.Y. / Region Metro::N.Y. / Region
## 186    myMisc::                    Open          Open         myMisc::Open
## 158        OpEd                 Opinion       Opinion      Room For Debate
## 164        OpEd     OpEd        Opinion       Opinion        OpEd::Opinion
## 59       TStyle   TStyle         TStyle                     TStyle::TStyle
## 54   Verbatim::              Verbatim::               Verbatim::Verbatim::
##      SubsectionName     Headline.pfx Popular.fctr .n
## 124                         myMisc::         <NA>  2
## 186                         myMisc::         <NA>  1
## 158 Room For Debate         myMisc::            Y  1
## 164                         myTech::            N  1
## 59                  The Daily Gift::            N 25
## 54                        Verbatim::            N 33
##          NewsDesk.nb NewsDesk SectionName.nb SectionName
## 59            TStyle   TStyle         TStyle            
## 63            TStyle   TStyle         TStyle            
## 54        Verbatim::              Verbatim::            
## 86        Verbatim::              Verbatim::            
## 35 Word of the Day::                    U.S.        U.S.
## 74 Word of the Day::                    U.S.        U.S.
##       SubsectionName.nb SubsectionName      Headline.pfx Popular.fctr .n
## 59       TStyle::TStyle                 The Daily Gift::            N 25
## 63       TStyle::TStyle                 The Daily Gift::         <NA> 22
## 54 Verbatim::Verbatim::                       Verbatim::            N 33
## 86 Verbatim::Verbatim::                       Verbatim::         <NA> 12
## 35            Education      Education Word of the Day::            N 61
## 74            Education      Education Word of the Day::         <NA> 17
```

```r
print(nrow(ndsecsub_xtab_df))
```

```
## [1] 192
```

```r
#print(subset(ndsecsub_xtab_df, NewsDesk == "OpEd"))
#print(subset(ndsecsub_xtab_df, SectionName.nb == "Health"))
write.table(ndsecsub_xtab_df, paste0(glb_out_pfx, "ndsecsub_xtab.csv"), 
            row.names=FALSE)
#stop("here")

# dsp_datagrp(Headline.pfx="What We're::", all=TRUE)
# #dsp_obs(Headline.pfx="What We're::")
# dsp_datagrp(all=TRUE)

# ## May we group all 1914::Foreign into Education(history) for SubsectionName ?
# ### Will it will clash with other entries in Education for SubsectionName ?
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(SubsectionName="Education"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# #### No all other Education entries have U.S. as SectionName
# ### May we group all 1914::Foreign into World for SectionName ?
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(SectionName="World"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# #### Yes, all other World entries don't have Education in SubsectionName    
# print(head(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#                    mycreate_sqlxtab_df(glb_entity_df[sel_obs(), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))),
#     10))
# glb_entity_df[glb_entity_df$Headline.pfx == "1914::", "SectionName"] <- "World"
# glb_entity_df[glb_entity_df$Headline.pfx == "1914::", "SubsectionName"] <- "Education"
# 
# ## What are the headlines for 1939::blank::blank::blank ?
# dsp_obs(Headline.pfx="1939::", NewsDesk="")
# ### It is Foreign stuff
# glb_entity_df[glb_entity_df$Headline.pfx == "1939::", "NewsDesk"] <- "Foreign"
# glb_entity_df[glb_entity_df$Headline.pfx == "1939::", "SectionName"] <- "World"
# glb_entity_df[glb_entity_df$Headline.pfx == "1939::", "SubsectionName"] <- "Education"
# 
# ## What are the headlines for 1964::blank::blank::blank ?
# dsp_obs(Headline.pfx="1964::", NewsDesk="")
# ### It is Foreign stuff
# glb_entity_df[glb_entity_df$Headline.pfx == "1964::", "NewsDesk"] <- "Foreign"
# glb_entity_df[glb_entity_df$Headline.pfx == "1964::", "SectionName"] <- "World"
# glb_entity_df[glb_entity_df$Headline.pfx == "1964::", "SubsectionName"] <- "Education"
# 
# ## Grouping 6 Q's About the News:: into NewsDesk=myEducation - A new category
# glb_entity_df[glb_entity_df$Headline.pfx == "6 Q's About the News::", "NewsDesk"] <-
#     "myEducation"
# dsp_datagrp(7, 17)
# 
# ## Sample headlines from Daily Clip Report:: ?
# dsp_obs(Headline.pfx="Daily Clip Report::")
# ## Grouping Daily Clip Report:: into NewsDesk=myCollection - A new category
# ###                                 SectionName=myCollection - A new category
# ###                                 SubsectionName=myCollection - A new category
# glb_entity_df[glb_entity_df$Headline.pfx == "Daily Clip Report::", "NewsDesk"] <- "myCollection"
# glb_entity_df[glb_entity_df$Headline.pfx == "Daily Clip Report::", "SectionName"] <- "myCollection"
# glb_entity_df[glb_entity_df$Headline.pfx == "Daily Clip Report::", "SubsectionName"] <- "myCollection"
# 
# ## Sample headlines from Daily Report:: ?
# dsp_obs(Headline.pfx="Daily Report::")
# ### What are the SubsectionNames for <>::Business::Technology ?
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(
#             NewsDesk="Business", SectionName="Technology"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# 
# dsp_obs(Headline.pfx="Daily Report::")
# ## Grouping Daily Report:: into SubsectionName=myBus::Tech - A new category
# glb_entity_df[glb_entity_df$Headline.pfx == "Daily Report::", "SubsectionName"] <- "myBus::Tech"
# dsp_datagrp(7, 17)
# 
# ## Sample headlines from First Draft:: ?
# dsp_obs(Headline.pfx="First Draft::")
# ## Grouping First Draft:: into NewsDesk=myCollection - A new category
# ###                                 SectionName=myCollection - A new category
# ###                                 SubsectionName=myCollection - A new category
# glb_entity_df[glb_entity_df$Headline.pfx == "First Draft::", "NewsDesk"] <- "myCollection"
# glb_entity_df[glb_entity_df$Headline.pfx == "First Draft::", "SectionName"] <- "myCollection"
# glb_entity_df[glb_entity_df$Headline.pfx == "First Draft::", "SubsectionName"] <- "myCollection"
# dsp_datagrp(1, 20)
# 
# ## How are the Milan Fashion Week:: blogs categorized ?
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(
#             Headline.contains="Fashion Week"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(
#             NewsDesk="Styles"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# dsp_obs(Popular=1, NewsDesk="Styles")
# 
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(
#             NewsDesk="TStyle"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# #dsp_xtab("Fashion Week")
# 
# ## Sample headlines from myMisc:: ?
# dsp_obs(Popular=1, Headline.pfx="myMisc::")
# dsp_obs(Headline.contains="Saturday Morning Music") # only 1 obs
# dsp_obs(Headline.pfx="myMisc::", Headline.contains=":")
# dsp_obs(Headline.contains="Charities That Inspire Kids")

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

#dsp_NewsDesk_SectionName_obs("", "U.S.")

# print(table(glb_entity_df$NewsDesk, glb_entity_df$SectionName))
# print(table(glb_entity_df$SectionName, glb_entity_df$SubsectionName))
# print(table(glb_entity_df$NewsDesk, glb_entity_df$SectionName, glb_entity_df$SubsectionName))

# Copy Headline into Snipper & Abstract if they are empty
print(glb_entity_df[nchar(glb_entity_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
```

```
##                                                            Headline
## 2838            First Draft Focus: Off to Raise Money for Democrats
## 3728                      Verbatim: Obama as Supreme Court Justice?
## 4904                                   Election 2014: Live Coverage
## 4994                                   Election 2014: Live Coverage
## 5029                        First Draft Focus: Perry's Day in Court
## 5065                   First Draft Focus: Honoring a Civil War Hero
## 5160                 Supreme Court to Hear New Health Law Challenge
## 5254                                 Verbatim: Will Rick Perry Run?
## 5472                        First Draft Focus: A Red Carpet Welcome
## 7129                                 First Draft Focus: Pass a Bill
## 7164 Does Torture Work? C.I.A.'s Claims vs. Senate Panel's Findings
## 7364                              First Draft Focus: Three Wise Men
## 7368                              Verbatim: The People's Priorities
##      Snippet
## 2838        
## 3728        
## 4904        
## 4994        
## 5029        
## 5065        
## 5160        
## 5254        
## 5472        
## 7129        
## 7164        
## 7364        
## 7368
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
## 5029                        First Draft Focus: Perry's Day in Court
## 5065                   First Draft Focus: Honoring a Civil War Hero
## 5160                 Supreme Court to Hear New Health Law Challenge
## 5254                                 Verbatim: Will Rick Perry Run?
## 5472                        First Draft Focus: A Red Carpet Welcome
## 7129                                 First Draft Focus: Pass a Bill
## 7164 Does Torture Work? C.I.A.'s Claims vs. Senate Panel's Findings
## 7309             Spending Bill Passes House With Democratic Support
## 7310                   Funding Bill Hangs in Balance as House Votes
## 7315              House Democrats Vent Frustration With White House
## 7329                Obama Works the Phones to Get Funding Deal Done
## 7364                              First Draft Focus: Three Wise Men
## 7368                              Verbatim: The People's Priorities
##      Abstract
## 2838         
## 3728         
## 4904         
## 4994         
## 5029         
## 5065         
## 5160         
## 5254         
## 5472         
## 7129         
## 7164         
## 7309         
## 7310         
## 7315         
## 7329         
## 7364         
## 7368
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
## 3        cleanse.data          2          1 28.894 52.664   23.77
## 4 manage.missing.data          2          2 52.664     NA      NA
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
##          WordCount            Popular           UniqueID 
##                  0               1870                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##               1870                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                109                  0 
## [1] "numeric data w/ 0s in : "
##          WordCount            Popular           UniqueID 
##                109               5439                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                378                159 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0               1344                141 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ Infs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ NaNs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "string data missing in : "
##          NewsDesk       SectionName    SubsectionName          Headline 
##              2408              2899              6176                 0 
##           Snippet          Abstract           PubDate      Headline.pfx 
##                 0                 0                 0                 0 
##       NewsDesk.nb    SectionName.nb SubsectionName.nb 
##                 0                 0                 0
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
##  PubDate.date.fctr PubDate.wkday.fctr  PubDate.hour   PubDate.apm.fctr
##  (0.97,7]:1981     0: 378             Min.   : 0.00   am:3636         
##  (7,13]  :1757     1:1605             1st Qu.: 9.00   pm:4766         
##  (13,19] :1808     2:1559             Median :12.00                   
##  (19,25] :1650     3:1614             Mean   :12.22                   
##  (25,31] :1206     4:1539             3rd Qu.:16.00                   
##                    5:1470             Max.   :23.00                   
##                    6: 237                                             
##  PubDate.minute  PubDate.second  WordCount.log        .rnorm         
##  Min.   : 0.00   Min.   : 0.00   Min.   :0.6932   Min.   :-3.881663  
##  1st Qu.: 5.00   1st Qu.:14.00   1st Qu.:5.2679   1st Qu.:-0.665043  
##  Median :24.00   Median :30.00   Median :5.9480   Median :-0.004510  
##  Mean   :24.11   Mean   :29.49   Mean   :5.8263   Mean   :-0.006807  
##  3rd Qu.:40.00   3rd Qu.:44.00   3rd Qu.:6.6067   3rd Qu.: 0.664125  
##  Max.   :59.00   Max.   :59.00   Max.   :9.2977   Max.   : 3.356092  
##                                  NA's   :109                         
##  Headline.pfx       NewsDesk.nb        SectionName.nb    
##  Length:8402        Length:8402        Length:8402       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##  SubsectionName.nb 
##  Length:8402       
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
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
##  PubDate.date.fctr PubDate.wkday.fctr  PubDate.hour   PubDate.apm.fctr
##  (0.97,7]:1981     0: 378             Min.   : 0.00   am:3636         
##  (7,13]  :1757     1:1605             1st Qu.: 9.00   pm:4766         
##  (13,19] :1808     2:1559             Median :12.00                   
##  (19,25] :1650     3:1614             Mean   :12.22                   
##  (25,31] :1206     4:1539             3rd Qu.:16.00                   
##                    5:1470             Max.   :23.00                   
##                    6: 237                                             
##  PubDate.minute  PubDate.second  WordCount.log        .rnorm         
##  Min.   : 0.00   Min.   : 0.00   Min.   :0.6931   Min.   :-3.881663  
##  1st Qu.: 5.00   1st Qu.:14.00   1st Qu.:5.2730   1st Qu.:-0.665043  
##  Median :24.00   Median :30.00   Median :5.9467   Median :-0.004510  
##  Mean   :24.11   Mean   :29.49   Mean   :5.8263   Mean   :-0.006807  
##  3rd Qu.:40.00   3rd Qu.:44.00   3rd Qu.:6.6067   3rd Qu.: 0.664125  
##  Max.   :59.00   Max.   :59.00   Max.   :9.2977   Max.   : 3.356092  
##                                                                      
##  Headline.pfx       NewsDesk.nb        SectionName.nb    
##  Length:8402        Length:8402        Length:8402       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##  SubsectionName.nb 
##  Length:8402       
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

```r
dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##          WordCount            Popular           UniqueID 
##                  0               1870                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##               1870                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ 0s in : "
##          WordCount            Popular           UniqueID 
##                109               5439                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                378                159 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0               1344                141 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ Infs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ NaNs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "string data missing in : "
##          NewsDesk       SectionName    SubsectionName          Headline 
##              2408              2899              6176                 0 
##           Snippet          Abstract           PubDate      Headline.pfx 
##                 0                 0                 0                 0 
##       NewsDesk.nb    SectionName.nb SubsectionName.nb 
##                 0                 0                 0
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "encode.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 4 manage.missing.data          2          2 52.664 58.729   6.065
## 5         encode.data          2          3 58.729     NA      NA
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
## 5      encode.data          2          3 58.729 58.784   0.055
## 6 extract.features          3          0 58.784     NA      NA
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
str_vars <- sapply(names(glb_entity_df), function(var)  
                    ifelse(class(glb_entity_df[, var]) == "character", var, ""))
print(str_vars <- str_vars[str_vars != ""])
```

```
##            NewsDesk         SectionName      SubsectionName 
##          "NewsDesk"       "SectionName"    "SubsectionName" 
##            Headline             Snippet            Abstract 
##          "Headline"           "Snippet"          "Abstract" 
##             PubDate                .src        Headline.pfx 
##           "PubDate"              ".src"      "Headline.pfx" 
##         NewsDesk.nb      SectionName.nb   SubsectionName.nb 
##       "NewsDesk.nb"    "SectionName.nb" "SubsectionName.nb"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               glb_exclude_vars_as_features)) > 0) {
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
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
}
```

```
## Warning: Creating factors of string variable: Headline.pfx: # of unique
## values: 23
```

```
## Warning: Creating factors of string variable: NewsDesk.nb: # of unique
## values: 27
```

```
## Warning: Creating factors of string variable: SectionName.nb: # of unique
## values: 31
```

```
## Warning: Creating factors of string variable: SubsectionName.nb: # of
## unique values: 39
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
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(toupper(substr(txt_var, 1, 1)), ".",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_entity_df) # warning otherwise
        glb_entity_df <- cbind(glb_entity_df, txt_X_df)
        
        # Create <txt_var>.has.http
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".has.http", sep="")] <- 
            sapply(1:nrow(glb_entity_df), 
    function(row_ix) ifelse(grepl("http", glb_entity_df[row_ix, txt_var], fixed=TRUE), 
                            1, 0))
    
        # Create user-specified term vectors 
        #   UniqueID == 4020, H.has.ebola
#         dsp_chisq.test(Headline.contains="[Ee]bola")                            
#         dsp_chisq.test( Snippet.contains="[Ee]bola")
#         dsp_chisq.test(Abstract.contains="[Ee]bola")
        if (txt_var == "Headline") {
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".has.ebola", sep="")] <- 
            sapply(1:nrow(glb_entity_df), 
    function(row_ix) ifelse(grepl("[Ee]bola", glb_entity_df[row_ix, txt_var]), 
                            1, 0))            
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".is.question", sep="")] <- 
            sapply(1:nrow(glb_entity_df), 
    function(row_ix) ifelse(grepl("\\?", glb_entity_df[row_ix, txt_var]), 
                            1, 0))           
        }
    
        # Create <txt_var>.num.chars
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".num.chars", sep="")] <- 
            sapply(1:nrow(glb_entity_df), 
                    function(row_ix) nchar(glb_entity_df[row_ix, txt_var]))

        # Create <txt_var>.num.words & .num.words.unq
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".num.words", sep="")] <- 
            rowSums(as.matrix(glb_full_DTM_lst[[txt_var]]))
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".num.words.unq", sep="")] <- 
            rowSums(as.matrix(glb_full_DTM_lst[[txt_var]]) != 0)
    
        for (feat in paste(toupper(substr(txt_var, 1, 1)), 
                            c(".num.chars", ".num.words", ".num.words.unq"), sep="")) {
            glb_entity_df[, paste0(feat, ".log")] <- log(1 + glb_entity_df[, feat])
            print(myplot_box(glb_entity_df, paste0(feat, ".log"), glb_rsp_var))
            glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features,
                                                  feat)
        }            
    }        

    # Generate summaries
#     print(summary(glb_entity_df))
#     print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
#     print(summary(glb_trnent_df))
#     print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
#     print(summary(glb_newent_df))
#     print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
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

![](NYTBlogs_Metro_files/figure-html/extract.features-1.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-2.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-3.png) 

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

![](NYTBlogs_Metro_files/figure-html/extract.features-4.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-5.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-6.png) 

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

![](NYTBlogs_Metro_files/figure-html/extract.features-7.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-8.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-9.png) 

```
## [1] "Binding DTM for Headline..."
```

![](NYTBlogs_Metro_files/figure-html/extract.features-10.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-11.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-12.png) 

```
## [1] "Binding DTM for Snippet..."
```

![](NYTBlogs_Metro_files/figure-html/extract.features-13.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-14.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-15.png) 

```
## [1] "Binding DTM for Abstract..."
```

![](NYTBlogs_Metro_files/figure-html/extract.features-16.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-17.png) ![](NYTBlogs_Metro_files/figure-html/extract.features-18.png) 

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

![](NYTBlogs_Metro_files/figure-html/extract.features-19.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 6 extract.features          3          0  58.784 148.121  89.337
## 7  select.features          4          0 148.121      NA      NA
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
##                                            id        cor.y exclude.as.feat
## Popular                               Popular  1.000000000               1
## WordCount.log                   WordCount.log  0.265952699               0
## WordCount                           WordCount  0.257526549               1
## S.num.words.unq.log       S.num.words.unq.log -0.250796919               0
## A.num.words.unq.log       A.num.words.unq.log -0.250601203               0
## S.num.words.log               S.num.words.log -0.245354135               0
## A.num.words.log               A.num.words.log -0.245073324               0
## S.num.chars.log               S.num.chars.log -0.224692967               0
## A.num.chars.log               A.num.chars.log -0.224548821               0
## SubsectionName.nb.fctr SubsectionName.nb.fctr -0.213669898               0
## S.num.words.unq               S.num.words.unq -0.212102717               1
## A.num.words.unq               A.num.words.unq -0.210242145               1
## S.num.words                       S.num.words -0.206385049               1
## H.num.words.unq.log       H.num.words.unq.log -0.204496360               0
## A.num.words                       A.num.words -0.204211072               1
## H.num.words.log               H.num.words.log -0.200686356               0
## H.num.words.unq               H.num.words.unq -0.189702157               1
## H.num.words                       H.num.words -0.186036895               1
## S.num.chars                       S.num.chars -0.179331806               1
## A.num.chars                       A.num.chars -0.177037425               1
## H.num.chars.log               H.num.chars.log -0.171062360               0
## NewsDesk.nb.fctr             NewsDesk.nb.fctr -0.170890512               0
## PubDate.hour                     PubDate.hour  0.159167673               0
## SectionName.nb.fctr       SectionName.nb.fctr -0.155578018               0
## H.num.chars                       H.num.chars -0.147211183               1
## H.is.question                   H.is.question  0.129154799               0
## Headline.pfx.fctr           Headline.pfx.fctr -0.102262465               0
## PubDate.apm.fctr             PubDate.apm.fctr  0.101472715               0
## S.fashion                           S.fashion -0.086446251               0
## A.fashion                           A.fashion -0.086446251               0
## S.week                                 S.week -0.084814939               0
## A.week                                 A.week -0.084814939               0
## H.fashion                           H.fashion -0.081708612               0
## H.week                                 H.week -0.075105216               0
## H.daili                               H.daili -0.069192975               0
## S.intern                             S.intern -0.068485701               0
## A.intern                             A.intern -0.068485701               0
## H.X2015                               H.X2015 -0.066584892               0
## H.report                             H.report -0.064948102               0
## H.today                               H.today -0.063723058               0
## S.newyork                           S.newyork -0.062117105               0
## A.newyork                           A.newyork -0.062117105               0
## H.day                                   H.day -0.061669687               0
## A.will                                 A.will -0.061025004               0
## S.will                                 S.will -0.060575493               0
## S.articl                             S.articl -0.059520554               0
## A.articl                             A.articl -0.059520554               0
## H.newyork                           H.newyork -0.057970095               0
## A.time                                 A.time -0.057790617               0
## S.time                                 S.time -0.057595102               0
## S.first                               S.first -0.053388178               0
## A.first                               A.first -0.053388178               0
## H.new                                   H.new -0.053121542               0
## A.compani                           A.compani -0.053099633               0
## S.compani                           S.compani -0.053012962               0
## S.year                                 S.year -0.051146178               0
## A.year                                 A.year -0.051146178               0
## S.share                               S.share -0.050329686               0
## A.share                               A.share -0.050329686               0
## S.report                             S.report -0.050211524               0
## A.report                             A.report -0.050211524               0
## S.show                                 S.show -0.048801740               0
## A.show                                 A.show -0.048801740               0
## H.X2014                               H.X2014 -0.046206380               0
## A.day                                   A.day -0.045909684               0
## S.day                                   S.day -0.045649185               0
## PubDate.wkday.fctr         PubDate.wkday.fctr -0.039801288               0
## A.new                                   A.new -0.035359447               0
## S.new                                   S.new -0.034948520               0
## A.can                                   A.can  0.031498867               0
## PubDate.minute                 PubDate.minute -0.031469083               0
## S.can                                   S.can  0.029999780               0
## A.take                                 A.take -0.026086108               0
## H.has.ebola                       H.has.ebola  0.025881397               0
## S.take                                 S.take -0.025762398               0
## S.make                                 S.make  0.023138853               0
## A.make                                 A.make  0.023138853               0
## S.presid                             S.presid -0.019828826               0
## A.presid                             A.presid -0.019828826               0
## PubDate.month.fctr         PubDate.month.fctr  0.019148739               1
## A.has.http                         A.has.http -0.013592603               0
## PubDate.second                 PubDate.second -0.012253600               0
## UniqueID                             UniqueID  0.011824920               1
## PubDate.date.fctr           PubDate.date.fctr -0.011647558               0
## .rnorm                                 .rnorm -0.008703337               0
## S.one                                   S.one  0.006342094               0
## S.state                               S.state  0.006069626               0
## A.state                               A.state  0.005702163               0
## A.one                                   A.one  0.005696039               0
## S.said                                 S.said  0.001363226               0
## A.said                                 A.said  0.001363226               0
## PubDate.year                     PubDate.year           NA               1
## H.has.http                         H.has.http           NA               0
## S.has.http                         S.has.http           NA               0
##                          cor.y.abs
## Popular                1.000000000
## WordCount.log          0.265952699
## WordCount              0.257526549
## S.num.words.unq.log    0.250796919
## A.num.words.unq.log    0.250601203
## S.num.words.log        0.245354135
## A.num.words.log        0.245073324
## S.num.chars.log        0.224692967
## A.num.chars.log        0.224548821
## SubsectionName.nb.fctr 0.213669898
## S.num.words.unq        0.212102717
## A.num.words.unq        0.210242145
## S.num.words            0.206385049
## H.num.words.unq.log    0.204496360
## A.num.words            0.204211072
## H.num.words.log        0.200686356
## H.num.words.unq        0.189702157
## H.num.words            0.186036895
## S.num.chars            0.179331806
## A.num.chars            0.177037425
## H.num.chars.log        0.171062360
## NewsDesk.nb.fctr       0.170890512
## PubDate.hour           0.159167673
## SectionName.nb.fctr    0.155578018
## H.num.chars            0.147211183
## H.is.question          0.129154799
## Headline.pfx.fctr      0.102262465
## PubDate.apm.fctr       0.101472715
## S.fashion              0.086446251
## A.fashion              0.086446251
## S.week                 0.084814939
## A.week                 0.084814939
## H.fashion              0.081708612
## H.week                 0.075105216
## H.daili                0.069192975
## S.intern               0.068485701
## A.intern               0.068485701
## H.X2015                0.066584892
## H.report               0.064948102
## H.today                0.063723058
## S.newyork              0.062117105
## A.newyork              0.062117105
## H.day                  0.061669687
## A.will                 0.061025004
## S.will                 0.060575493
## S.articl               0.059520554
## A.articl               0.059520554
## H.newyork              0.057970095
## A.time                 0.057790617
## S.time                 0.057595102
## S.first                0.053388178
## A.first                0.053388178
## H.new                  0.053121542
## A.compani              0.053099633
## S.compani              0.053012962
## S.year                 0.051146178
## A.year                 0.051146178
## S.share                0.050329686
## A.share                0.050329686
## S.report               0.050211524
## A.report               0.050211524
## S.show                 0.048801740
## A.show                 0.048801740
## H.X2014                0.046206380
## A.day                  0.045909684
## S.day                  0.045649185
## PubDate.wkday.fctr     0.039801288
## A.new                  0.035359447
## S.new                  0.034948520
## A.can                  0.031498867
## PubDate.minute         0.031469083
## S.can                  0.029999780
## A.take                 0.026086108
## H.has.ebola            0.025881397
## S.take                 0.025762398
## S.make                 0.023138853
## A.make                 0.023138853
## S.presid               0.019828826
## A.presid               0.019828826
## PubDate.month.fctr     0.019148739
## A.has.http             0.013592603
## PubDate.second         0.012253600
## UniqueID               0.011824920
## PubDate.date.fctr      0.011647558
## .rnorm                 0.008703337
## S.one                  0.006342094
## S.state                0.006069626
## A.state                0.005702163
## A.one                  0.005696039
## S.said                 0.001363226
## A.said                 0.001363226
## PubDate.year                    NA
## H.has.http                      NA
## S.has.http                      NA
```

```r
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, entity_df=glb_trnent_df, 
                              rsp_var=glb_rsp_var, 
                            checkConditionalX=(glb_is_classification && glb_is_binomial))))
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
## [1] "cor(A.articl, S.articl)=1.0000"
## [1] "cor(Popular.fctr, A.articl)=-0.0595"
## [1] "cor(Popular.fctr, S.articl)=-0.0595"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.articl as highly correlated with A.articl
```

```
## [1] "cor(A.fashion, S.fashion)=1.0000"
## [1] "cor(Popular.fctr, A.fashion)=-0.0864"
## [1] "cor(Popular.fctr, S.fashion)=-0.0864"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.fashion as highly correlated with A.fashion
```

```
## [1] "cor(A.first, S.first)=1.0000"
## [1] "cor(Popular.fctr, A.first)=-0.0534"
## [1] "cor(Popular.fctr, S.first)=-0.0534"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.first as highly correlated with A.first
```

```
## [1] "cor(A.intern, S.intern)=1.0000"
## [1] "cor(Popular.fctr, A.intern)=-0.0685"
## [1] "cor(Popular.fctr, S.intern)=-0.0685"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.intern as highly correlated with A.intern
```

```
## [1] "cor(A.make, S.make)=1.0000"
## [1] "cor(Popular.fctr, A.make)=0.0231"
## [1] "cor(Popular.fctr, S.make)=0.0231"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.make as highly correlated with A.make
```

```
## [1] "cor(A.newyork, S.newyork)=1.0000"
## [1] "cor(Popular.fctr, A.newyork)=-0.0621"
## [1] "cor(Popular.fctr, S.newyork)=-0.0621"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.newyork as highly correlated with A.newyork
```

```
## [1] "cor(A.presid, S.presid)=1.0000"
## [1] "cor(Popular.fctr, A.presid)=-0.0198"
## [1] "cor(Popular.fctr, S.presid)=-0.0198"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.presid as highly correlated with A.presid
```

```
## [1] "cor(A.report, S.report)=1.0000"
## [1] "cor(Popular.fctr, A.report)=-0.0502"
## [1] "cor(Popular.fctr, S.report)=-0.0502"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.report as highly correlated with A.report
```

```
## [1] "cor(A.share, S.share)=1.0000"
## [1] "cor(Popular.fctr, A.share)=-0.0503"
## [1] "cor(Popular.fctr, S.share)=-0.0503"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.share as highly correlated with A.share
```

```
## [1] "cor(A.show, S.show)=1.0000"
## [1] "cor(Popular.fctr, A.show)=-0.0488"
## [1] "cor(Popular.fctr, S.show)=-0.0488"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.show as highly correlated with A.show
```

```
## [1] "cor(A.week, S.week)=1.0000"
## [1] "cor(Popular.fctr, A.week)=-0.0848"
## [1] "cor(Popular.fctr, S.week)=-0.0848"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.week as highly correlated with A.week
```

```
## [1] "cor(A.year, S.year)=1.0000"
## [1] "cor(Popular.fctr, A.year)=-0.0511"
## [1] "cor(Popular.fctr, S.year)=-0.0511"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.year as highly correlated with A.year
```

```
## [1] "cor(A.time, S.time)=0.9991"
## [1] "cor(Popular.fctr, A.time)=-0.0578"
## [1] "cor(Popular.fctr, S.time)=-0.0576"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.time as highly correlated with A.time
```

```
## [1] "cor(A.num.words.unq.log, S.num.words.unq.log)=0.9989"
## [1] "cor(Popular.fctr, A.num.words.unq.log)=-0.2506"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2508"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.words.unq.log as highly correlated with
## S.num.words.unq.log
```

```
## [1] "cor(A.num.words.log, S.num.words.log)=0.9988"
## [1] "cor(Popular.fctr, A.num.words.log)=-0.2451"
## [1] "cor(Popular.fctr, S.num.words.log)=-0.2454"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.words.log as highly correlated with
## S.num.words.log
```

```
## [1] "cor(A.compani, S.compani)=0.9988"
## [1] "cor(Popular.fctr, A.compani)=-0.0531"
## [1] "cor(Popular.fctr, S.compani)=-0.0530"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.compani as highly correlated with A.compani
```

```
## [1] "cor(A.num.chars.log, S.num.chars.log)=0.9986"
## [1] "cor(Popular.fctr, A.num.chars.log)=-0.2245"
## [1] "cor(Popular.fctr, S.num.chars.log)=-0.2247"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.chars.log as highly correlated with
## S.num.chars.log
```

```
## [1] "cor(A.new, S.new)=0.9983"
## [1] "cor(Popular.fctr, A.new)=-0.0354"
## [1] "cor(Popular.fctr, S.new)=-0.0349"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.new as highly correlated with A.new
```

```
## [1] "cor(A.can, S.can)=0.9982"
## [1] "cor(Popular.fctr, A.can)=0.0315"
## [1] "cor(Popular.fctr, S.can)=0.0300"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.can as highly correlated with A.can
```

```
## [1] "cor(A.day, S.day)=0.9981"
## [1] "cor(Popular.fctr, A.day)=-0.0459"
## [1] "cor(Popular.fctr, S.day)=-0.0456"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.day as highly correlated with A.day
```

```
## [1] "cor(A.take, S.take)=0.9976"
## [1] "cor(Popular.fctr, A.take)=-0.0261"
## [1] "cor(Popular.fctr, S.take)=-0.0258"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.take as highly correlated with A.take
```

```
## [1] "cor(A.will, S.will)=0.9976"
## [1] "cor(Popular.fctr, A.will)=-0.0610"
## [1] "cor(Popular.fctr, S.will)=-0.0606"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.will as highly correlated with A.will
```

```
## [1] "cor(H.num.words.log, H.num.words.unq.log)=0.9967"
## [1] "cor(Popular.fctr, H.num.words.log)=-0.2007"
## [1] "cor(Popular.fctr, H.num.words.unq.log)=-0.2045"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.num.words.log as highly correlated with
## H.num.words.unq.log
```

```
## [1] "cor(S.num.words.log, S.num.words.unq.log)=0.9954"
## [1] "cor(Popular.fctr, S.num.words.log)=-0.2454"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2508"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.num.words.log as highly correlated with
## S.num.words.unq.log
```

```
## [1] "cor(S.num.chars.log, S.num.words.unq.log)=0.9543"
## [1] "cor(Popular.fctr, S.num.chars.log)=-0.2247"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2508"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.num.chars.log as highly correlated with
## S.num.words.unq.log
```

```
## [1] "cor(SectionName.nb.fctr, SubsectionName.nb.fctr)=0.8965"
## [1] "cor(Popular.fctr, SectionName.nb.fctr)=-0.1556"
## [1] "cor(Popular.fctr, SubsectionName.nb.fctr)=-0.2137"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified SectionName.nb.fctr as highly correlated with
## SubsectionName.nb.fctr
```

```
## [1] "cor(H.num.chars.log, H.num.words.unq.log)=0.8881"
## [1] "cor(Popular.fctr, H.num.chars.log)=-0.1711"
## [1] "cor(Popular.fctr, H.num.words.unq.log)=-0.2045"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.num.chars.log as highly correlated with
## H.num.words.unq.log
```

```
## [1] "cor(NewsDesk.nb.fctr, SubsectionName.nb.fctr)=0.8339"
## [1] "cor(Popular.fctr, NewsDesk.nb.fctr)=-0.1709"
## [1] "cor(Popular.fctr, SubsectionName.nb.fctr)=-0.2137"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified NewsDesk.nb.fctr as highly correlated with
## SubsectionName.nb.fctr
```

```
## [1] "cor(PubDate.apm.fctr, PubDate.hour)=0.8156"
## [1] "cor(Popular.fctr, PubDate.apm.fctr)=0.1015"
## [1] "cor(Popular.fctr, PubDate.hour)=0.1592"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified PubDate.apm.fctr as highly correlated with
## PubDate.hour
```

```
## [1] "cor(H.fashion, H.week)=0.7616"
## [1] "cor(Popular.fctr, H.fashion)=-0.0817"
## [1] "cor(Popular.fctr, H.week)=-0.0751"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.week as highly correlated with H.fashion
```

```
##                                            id        cor.y exclude.as.feat
## Popular                               Popular  1.000000000               1
## WordCount.log                   WordCount.log  0.265952699               0
## WordCount                           WordCount  0.257526549               1
## PubDate.hour                     PubDate.hour  0.159167673               0
## H.is.question                   H.is.question  0.129154799               0
## PubDate.apm.fctr             PubDate.apm.fctr  0.101472715               0
## A.can                                   A.can  0.031498867               0
## S.can                                   S.can  0.029999780               0
## H.has.ebola                       H.has.ebola  0.025881397               0
## S.make                                 S.make  0.023138853               0
## A.make                                 A.make  0.023138853               0
## PubDate.month.fctr         PubDate.month.fctr  0.019148739               1
## UniqueID                             UniqueID  0.011824920               1
## S.one                                   S.one  0.006342094               0
## S.state                               S.state  0.006069626               0
## A.state                               A.state  0.005702163               0
## A.one                                   A.one  0.005696039               0
## S.said                                 S.said  0.001363226               0
## A.said                                 A.said  0.001363226               0
## .rnorm                                 .rnorm -0.008703337               0
## PubDate.date.fctr           PubDate.date.fctr -0.011647558               0
## PubDate.second                 PubDate.second -0.012253600               0
## A.has.http                         A.has.http -0.013592603               0
## S.presid                             S.presid -0.019828826               0
## A.presid                             A.presid -0.019828826               0
## S.take                                 S.take -0.025762398               0
## A.take                                 A.take -0.026086108               0
## PubDate.minute                 PubDate.minute -0.031469083               0
## S.new                                   S.new -0.034948520               0
## A.new                                   A.new -0.035359447               0
## PubDate.wkday.fctr         PubDate.wkday.fctr -0.039801288               0
## S.day                                   S.day -0.045649185               0
## A.day                                   A.day -0.045909684               0
## H.X2014                               H.X2014 -0.046206380               0
## S.show                                 S.show -0.048801740               0
## A.show                                 A.show -0.048801740               0
## S.report                             S.report -0.050211524               0
## A.report                             A.report -0.050211524               0
## S.share                               S.share -0.050329686               0
## A.share                               A.share -0.050329686               0
## S.year                                 S.year -0.051146178               0
## A.year                                 A.year -0.051146178               0
## S.compani                           S.compani -0.053012962               0
## A.compani                           A.compani -0.053099633               0
## H.new                                   H.new -0.053121542               0
## S.first                               S.first -0.053388178               0
## A.first                               A.first -0.053388178               0
## S.time                                 S.time -0.057595102               0
## A.time                                 A.time -0.057790617               0
## H.newyork                           H.newyork -0.057970095               0
## S.articl                             S.articl -0.059520554               0
## A.articl                             A.articl -0.059520554               0
## S.will                                 S.will -0.060575493               0
## A.will                                 A.will -0.061025004               0
## H.day                                   H.day -0.061669687               0
## S.newyork                           S.newyork -0.062117105               0
## A.newyork                           A.newyork -0.062117105               0
## H.today                               H.today -0.063723058               0
## H.report                             H.report -0.064948102               0
## H.X2015                               H.X2015 -0.066584892               0
## S.intern                             S.intern -0.068485701               0
## A.intern                             A.intern -0.068485701               0
## H.daili                               H.daili -0.069192975               0
## H.week                                 H.week -0.075105216               0
## H.fashion                           H.fashion -0.081708612               0
## S.week                                 S.week -0.084814939               0
## A.week                                 A.week -0.084814939               0
## S.fashion                           S.fashion -0.086446251               0
## A.fashion                           A.fashion -0.086446251               0
## Headline.pfx.fctr           Headline.pfx.fctr -0.102262465               0
## H.num.chars                       H.num.chars -0.147211183               1
## SectionName.nb.fctr       SectionName.nb.fctr -0.155578018               0
## NewsDesk.nb.fctr             NewsDesk.nb.fctr -0.170890512               0
## H.num.chars.log               H.num.chars.log -0.171062360               0
## A.num.chars                       A.num.chars -0.177037425               1
## S.num.chars                       S.num.chars -0.179331806               1
## H.num.words                       H.num.words -0.186036895               1
## H.num.words.unq               H.num.words.unq -0.189702157               1
## H.num.words.log               H.num.words.log -0.200686356               0
## A.num.words                       A.num.words -0.204211072               1
## H.num.words.unq.log       H.num.words.unq.log -0.204496360               0
## S.num.words                       S.num.words -0.206385049               1
## A.num.words.unq               A.num.words.unq -0.210242145               1
## S.num.words.unq               S.num.words.unq -0.212102717               1
## SubsectionName.nb.fctr SubsectionName.nb.fctr -0.213669898               0
## A.num.chars.log               A.num.chars.log -0.224548821               0
## S.num.chars.log               S.num.chars.log -0.224692967               0
## A.num.words.log               A.num.words.log -0.245073324               0
## S.num.words.log               S.num.words.log -0.245354135               0
## A.num.words.unq.log       A.num.words.unq.log -0.250601203               0
## S.num.words.unq.log       S.num.words.unq.log -0.250796919               0
## PubDate.year                     PubDate.year           NA               1
## H.has.http                         H.has.http           NA               0
## S.has.http                         S.has.http           NA               0
##                          cor.y.abs       cor.high.X is.ConditionalX.y
## Popular                1.000000000             <NA>                NA
## WordCount.log          0.265952699             <NA>              TRUE
## WordCount              0.257526549             <NA>                NA
## PubDate.hour           0.159167673 PubDate.apm.fctr              TRUE
## H.is.question          0.129154799             <NA>              TRUE
## PubDate.apm.fctr       0.101472715             <NA>              TRUE
## A.can                  0.031498867            S.can              TRUE
## S.can                  0.029999780             <NA>              TRUE
## H.has.ebola            0.025881397             <NA>              TRUE
## S.make                 0.023138853             <NA>              TRUE
## A.make                 0.023138853           S.make              TRUE
## PubDate.month.fctr     0.019148739             <NA>                NA
## UniqueID               0.011824920             <NA>                NA
## S.one                  0.006342094             <NA>              TRUE
## S.state                0.006069626             <NA>              TRUE
## A.state                0.005702163             <NA>              TRUE
## A.one                  0.005696039             <NA>              TRUE
## S.said                 0.001363226             <NA>              TRUE
## A.said                 0.001363226             <NA>              TRUE
## .rnorm                 0.008703337             <NA>              TRUE
## PubDate.date.fctr      0.011647558             <NA>              TRUE
## PubDate.second         0.012253600             <NA>              TRUE
## A.has.http             0.013592603             <NA>             FALSE
## S.presid               0.019828826             <NA>              TRUE
## A.presid               0.019828826         S.presid              TRUE
## S.take                 0.025762398             <NA>              TRUE
## A.take                 0.026086108           S.take              TRUE
## PubDate.minute         0.031469083             <NA>              TRUE
## S.new                  0.034948520             <NA>              TRUE
## A.new                  0.035359447            S.new              TRUE
## PubDate.wkday.fctr     0.039801288             <NA>              TRUE
## S.day                  0.045649185             <NA>              TRUE
## A.day                  0.045909684            S.day              TRUE
## H.X2014                0.046206380             <NA>              TRUE
## S.show                 0.048801740             <NA>              TRUE
## A.show                 0.048801740           S.show              TRUE
## S.report               0.050211524             <NA>              TRUE
## A.report               0.050211524         S.report              TRUE
## S.share                0.050329686             <NA>              TRUE
## A.share                0.050329686          S.share              TRUE
## S.year                 0.051146178             <NA>              TRUE
## A.year                 0.051146178           S.year              TRUE
## S.compani              0.053012962             <NA>              TRUE
## A.compani              0.053099633        S.compani              TRUE
## H.new                  0.053121542             <NA>              TRUE
## S.first                0.053388178             <NA>              TRUE
## A.first                0.053388178          S.first              TRUE
## S.time                 0.057595102             <NA>              TRUE
## A.time                 0.057790617           S.time              TRUE
## H.newyork              0.057970095             <NA>              TRUE
## S.articl               0.059520554             <NA>              TRUE
## A.articl               0.059520554         S.articl              TRUE
## S.will                 0.060575493             <NA>              TRUE
## A.will                 0.061025004           S.will              TRUE
## H.day                  0.061669687             <NA>              TRUE
## S.newyork              0.062117105             <NA>              TRUE
## A.newyork              0.062117105        S.newyork              TRUE
## H.today                0.063723058             <NA>              TRUE
## H.report               0.064948102             <NA>              TRUE
## H.X2015                0.066584892             <NA>             FALSE
## S.intern               0.068485701             <NA>              TRUE
## A.intern               0.068485701         S.intern              TRUE
## H.daili                0.069192975             <NA>             FALSE
## H.week                 0.075105216             <NA>              TRUE
## H.fashion              0.081708612           H.week              TRUE
## S.week                 0.084814939             <NA>              TRUE
## A.week                 0.084814939           S.week              TRUE
## S.fashion              0.086446251             <NA>              TRUE
## A.fashion              0.086446251        S.fashion              TRUE
## Headline.pfx.fctr      0.102262465             <NA>              TRUE
## H.num.chars            0.147211183             <NA>                NA
## SectionName.nb.fctr    0.155578018             <NA>              TRUE
## NewsDesk.nb.fctr       0.170890512             <NA>              TRUE
## H.num.chars.log        0.171062360             <NA>              TRUE
## A.num.chars            0.177037425             <NA>                NA
## S.num.chars            0.179331806             <NA>                NA
## H.num.words            0.186036895             <NA>                NA
## H.num.words.unq        0.189702157             <NA>                NA
## H.num.words.log        0.200686356             <NA>              TRUE
## A.num.words            0.204211072             <NA>                NA
## H.num.words.unq.log    0.204496360  H.num.chars.log              TRUE
## S.num.words            0.206385049             <NA>                NA
## A.num.words.unq        0.210242145             <NA>                NA
## S.num.words.unq        0.212102717             <NA>                NA
## SubsectionName.nb.fctr 0.213669898 NewsDesk.nb.fctr              TRUE
## A.num.chars.log        0.224548821             <NA>              TRUE
## S.num.chars.log        0.224692967  A.num.chars.log              TRUE
## A.num.words.log        0.245073324             <NA>              TRUE
## S.num.words.log        0.245354135  A.num.words.log              TRUE
## A.num.words.unq.log    0.250601203             <NA>              TRUE
## S.num.words.unq.log    0.250796919  S.num.chars.log              TRUE
## PubDate.year                    NA             <NA>                NA
## H.has.http                      NA             <NA>             FALSE
## S.has.http                      NA             <NA>             FALSE
##                        is.cor.y.abs.low
## Popular                           FALSE
## WordCount.log                     FALSE
## WordCount                         FALSE
## PubDate.hour                      FALSE
## H.is.question                     FALSE
## PubDate.apm.fctr                  FALSE
## A.can                             FALSE
## S.can                             FALSE
## H.has.ebola                       FALSE
## S.make                            FALSE
## A.make                            FALSE
## PubDate.month.fctr                FALSE
## UniqueID                          FALSE
## S.one                              TRUE
## S.state                            TRUE
## A.state                            TRUE
## A.one                              TRUE
## S.said                             TRUE
## A.said                             TRUE
## .rnorm                            FALSE
## PubDate.date.fctr                 FALSE
## PubDate.second                    FALSE
## A.has.http                        FALSE
## S.presid                          FALSE
## A.presid                          FALSE
## S.take                            FALSE
## A.take                            FALSE
## PubDate.minute                    FALSE
## S.new                             FALSE
## A.new                             FALSE
## PubDate.wkday.fctr                FALSE
## S.day                             FALSE
## A.day                             FALSE
## H.X2014                           FALSE
## S.show                            FALSE
## A.show                            FALSE
## S.report                          FALSE
## A.report                          FALSE
## S.share                           FALSE
## A.share                           FALSE
## S.year                            FALSE
## A.year                            FALSE
## S.compani                         FALSE
## A.compani                         FALSE
## H.new                             FALSE
## S.first                           FALSE
## A.first                           FALSE
## S.time                            FALSE
## A.time                            FALSE
## H.newyork                         FALSE
## S.articl                          FALSE
## A.articl                          FALSE
## S.will                            FALSE
## A.will                            FALSE
## H.day                             FALSE
## S.newyork                         FALSE
## A.newyork                         FALSE
## H.today                           FALSE
## H.report                          FALSE
## H.X2015                           FALSE
## S.intern                          FALSE
## A.intern                          FALSE
## H.daili                           FALSE
## H.week                            FALSE
## H.fashion                         FALSE
## S.week                            FALSE
## A.week                            FALSE
## S.fashion                         FALSE
## A.fashion                         FALSE
## Headline.pfx.fctr                 FALSE
## H.num.chars                       FALSE
## SectionName.nb.fctr               FALSE
## NewsDesk.nb.fctr                  FALSE
## H.num.chars.log                   FALSE
## A.num.chars                       FALSE
## S.num.chars                       FALSE
## H.num.words                       FALSE
## H.num.words.unq                   FALSE
## H.num.words.log                   FALSE
## A.num.words                       FALSE
## H.num.words.unq.log               FALSE
## S.num.words                       FALSE
## A.num.words.unq                   FALSE
## S.num.words.unq                   FALSE
## SubsectionName.nb.fctr            FALSE
## A.num.chars.log                   FALSE
## S.num.chars.log                   FALSE
## A.num.words.log                   FALSE
## S.num.words.log                   FALSE
## A.num.words.unq.log               FALSE
## S.num.words.unq.log               FALSE
## PubDate.year                         NA
## H.has.http                           NA
## S.has.http                           NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor     bgn    end elapsed
## 7         select.features          4          0 148.121 158.48  10.359
## 8 partition.data.training          5          0 158.481     NA      NA
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
                                       "NewsDesk.nb")
OOBent_ctgry_df <- mycreate_sqlxtab_df(subset(glb_entity_df, .lcn == "OOB"), 
                                       "NewsDesk.nb")
glb_ctgry_df <- merge(newent_ctgry_df, OOBent_ctgry_df, by="NewsDesk.nb", all=TRUE, 
                      suffixes=c(".Tst", ".OOB"))
glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
```

```
##               NewsDesk.nb .n.Tst .n.OOB .freqRatio.Tst .freqRatio.OOB
## 3                Business    500    513   0.2673796791   0.2493923189
## 11               myMisc::    294    350   0.1572192513   0.1701507049
## 4                 Culture    243    203   0.1299465241   0.0986874088
## 14                   OpEd    205    217   0.1096256684   0.1054934370
## 24                 TStyle    107    239   0.0572192513   0.1161886242
## 7                 Foreign    102    119   0.0545454545   0.0578512397
## 19                 Styles     76     75   0.0406417112   0.0364608653
## 9                   Metro     66     57   0.0352941176   0.0277102577
## 17                Science     57     66   0.0304812834   0.0320855615
## 12           myMultimedia     53     38   0.0283422460   0.0184735051
## 23                 Travel     31     34   0.0165775401   0.0165289256
## 5     Daily Clip Report::     22     18   0.0117647059   0.0087506077
## 22    Today in Politics::     21     14   0.0112299465   0.0068060282
## 20        Test Yourself::     17     21   0.0090909091   0.0102090423
## 26      Word of the Day::     17     18   0.0090909091   0.0087506077
## 2  6 Q's About the News::     15     22   0.0080213904   0.0106951872
## 6           First Draft::     14     18   0.0074866310   0.0087506077
## 25             Verbatim::     12     11   0.0064171123   0.0053475936
## 15 Quiz(.*)([?=|]|[?=:]::      4      3   0.0021390374   0.0014584346
## 1          19[0-9][0-9]::      4      2   0.0021390374   0.0009722897
## 16      Readers Respond::      4      2   0.0021390374   0.0009722897
## 8                Magazine      3     10   0.0016042781   0.0048614487
## 21       The Daily Gift::      2      1   0.0010695187   0.0004861449
## 13               myTech::      1      4   0.0005347594   0.0019445795
## 10               myFood::     NA      1             NA   0.0004861449
## 18                 Sports     NA      1             NA   0.0004861449
```

```r
# dsp_class_dstrb(glb_entity_df, ".src", "NewsDesk.nb")
# dsp_class_dstrb(glb_entity_df, ".lcn", "NewsDesk.nb")

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 94  7
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
##              is.ConditionalX.y is.cor.y.abs.low rsp_var_raw id_var rsp_var
## Popular                     NA            FALSE        TRUE     NA      NA
## UniqueID                    NA            FALSE       FALSE   TRUE      NA
## Popular.fctr                NA               NA          NA     NA    TRUE
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
## character(0)
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
## [1] 8402  108
```

```r
print("glb_trnent_df: "); print(dim(glb_trnent_df))
```

```
## [1] "glb_trnent_df: "
```

```
## [1] 6532  107
```

```r
print("glb_fitent_df: "); print(dim(glb_fitent_df))
```

```
## [1] "glb_fitent_df: "
```

```
## [1] 4475  107
```

```r
print("glb_OOBent_df: "); print(dim(glb_OOBent_df))
```

```
## [1] "glb_OOBent_df: "
```

```
## [1] 2057  107
```

```r
print("glb_newent_df: "); print(dim(glb_newent_df))
```

```
## [1] "glb_newent_df: "
```

```
## [1] 1870  107
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
## 8 partition.data.training          5          0 158.481 159.885   1.404
## 9              fit.models          6          0 159.885      NA      NA
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
## 1                      0.665                 0.003         0.5
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

![](NYTBlogs_Metro_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.2867534
## 3        0.2 0.1683938
## 4        0.3 0.1683938
## 5        0.4 0.1683938
## 6        0.5 0.1683938
## 7        0.6 0.1683938
## 8        0.7 0.1683938
## 9        0.8 0.1683938
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-2.png) 

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

![](NYTBlogs_Metro_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.2865473
## 3        0.2 0.1404011
## 4        0.3 0.1404011
## 5        0.4 0.1404011
## 6        0.5 0.1404011
## 7        0.6 0.1404011
## 8        0.7 0.1404011
## 9        0.8 0.1404011
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-4.png) 

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
## 1                      0.325                 0.002   0.4975446
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.1       0.2867534        0.1673743
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.1565447             0.1786398             0   0.4821958
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
## [1] "    indep_vars: WordCount.log"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.00223 on full training set
```

```
## Loading required package: rpart.plot
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##            CP nsplit rel error
## 1 0.002225189      0         1
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
##               model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart WordCount.log               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.708                  0.07         0.5
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
## [1] "    indep_vars: WordCount.log"
## Fitting cp = 0 on full training set
```

```
## Warning: labs do not fit even at cex 0.15, there may be some overplotting
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##             CP nsplit rel error
## 1 0.0022251891      0 1.0000000
## 2 0.0020026702     13 0.9666222
## 3 0.0013351135     19 0.9519359
## 4 0.0008900757     39 0.9158879
## 5 0.0005340454     50 0.9052069
## 6 0.0002225189     55 0.9025367
## 7 0.0000000000     61 0.9012016
## 
## Variable importance
## WordCount.log 
##           100 
## 
## Node number 1: 4475 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (3276 obs) right son=3 (1199 obs)
##   Primary splits:
##       WordCount.log < 6.528688 to the left,  improve=109.5997, (0 missing)
## 
## Node number 2: 3276 observations
##   predicted class=N  expected loss=0.1004274  P(node) =0.732067
##     class counts:  2947   329
##    probabilities: 0.900 0.100 
## 
## Node number 3: 1199 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3502919  P(node) =0.267933
##     class counts:   779   420
##    probabilities: 0.650 0.350 
##   left son=6 (193 obs) right son=7 (1006 obs)
##   Primary splits:
##       WordCount.log < 6.663771 to the left,  improve=3.008125, (0 missing)
## 
## Node number 6: 193 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2694301  P(node) =0.04312849
##     class counts:   141    52
##    probabilities: 0.731 0.269 
##   left son=12 (62 obs) right son=13 (131 obs)
##   Primary splits:
##       WordCount.log < 6.631343 to the right, improve=2.136379, (0 missing)
## 
## Node number 7: 1006 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3658052  P(node) =0.2248045
##     class counts:   638   368
##    probabilities: 0.634 0.366 
##   left son=14 (85 obs) right son=15 (921 obs)
##   Primary splits:
##       WordCount.log < 7.57327  to the right, improve=3.162874, (0 missing)
## 
## Node number 12: 62 observations
##   predicted class=N  expected loss=0.1612903  P(node) =0.01385475
##     class counts:    52    10
##    probabilities: 0.839 0.161 
## 
## Node number 13: 131 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3206107  P(node) =0.02927374
##     class counts:    89    42
##    probabilities: 0.679 0.321 
##   left son=26 (121 obs) right son=27 (10 obs)
##   Primary splits:
##       WordCount.log < 6.535966 to the right, improve=0.6968015, (0 missing)
## 
## Node number 14: 85 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.2352941  P(node) =0.01899441
##     class counts:    65    20
##    probabilities: 0.765 0.235 
##   left son=28 (77 obs) right son=29 (8 obs)
##   Primary splits:
##       WordCount.log < 8.229096 to the left,  improve=4.679144, (0 missing)
## 
## Node number 15: 921 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3778502  P(node) =0.2058101
##     class counts:   573   348
##    probabilities: 0.622 0.378 
##   left son=30 (734 obs) right son=31 (187 obs)
##   Primary splits:
##       WordCount.log < 6.775937 to the right, improve=1.435366, (0 missing)
## 
## Node number 26: 121 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3057851  P(node) =0.02703911
##     class counts:    84    37
##    probabilities: 0.694 0.306 
##   left son=52 (15 obs) right son=53 (106 obs)
##   Primary splits:
##       WordCount.log < 6.548935 to the left,  improve=1.018442, (0 missing)
## 
## Node number 27: 10 observations
##   predicted class=N  expected loss=0.5  P(node) =0.002234637
##     class counts:     5     5
##    probabilities: 0.500 0.500 
## 
## Node number 28: 77 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.0172067
##     class counts:    63    14
##    probabilities: 0.818 0.182 
## 
## Node number 29: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001787709
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## Node number 30: 734 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3637602  P(node) =0.1640223
##     class counts:   467   267
##    probabilities: 0.636 0.364 
##   left son=60 (11 obs) right son=61 (723 obs)
##   Primary splits:
##       WordCount.log < 6.782759 to the left,  improve=2.955363, (0 missing)
## 
## Node number 31: 187 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4331551  P(node) =0.04178771
##     class counts:   106    81
##    probabilities: 0.567 0.433 
##   left son=62 (177 obs) right son=63 (10 obs)
##   Primary splits:
##       WordCount.log < 6.771362 to the left,  improve=2.843566, (0 missing)
## 
## Node number 52: 15 observations
##   predicted class=N  expected loss=0.1333333  P(node) =0.003351955
##     class counts:    13     2
##    probabilities: 0.867 0.133 
## 
## Node number 53: 106 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3301887  P(node) =0.02368715
##     class counts:    71    35
##    probabilities: 0.670 0.330 
##   left son=106 (87 obs) right son=107 (19 obs)
##   Primary splits:
##       WordCount.log < 6.566671 to the right, improve=1.780924, (0 missing)
## 
## Node number 60: 11 observations
##   predicted class=N  expected loss=0  P(node) =0.002458101
##     class counts:    11     0
##    probabilities: 1.000 0.000 
## 
## Node number 61: 723 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3692946  P(node) =0.1615642
##     class counts:   456   267
##    probabilities: 0.631 0.369 
##   left son=122 (515 obs) right son=123 (208 obs)
##   Primary splits:
##       WordCount.log < 7.162785 to the left,  improve=1.689287, (0 missing)
## 
## Node number 62: 177 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4124294  P(node) =0.03955307
##     class counts:   104    73
##    probabilities: 0.588 0.412 
##   left son=124 (125 obs) right son=125 (52 obs)
##   Primary splits:
##       WordCount.log < 6.736373 to the left,  improve=0.6877723, (0 missing)
## 
## Node number 63: 10 observations
##   predicted class=Y  expected loss=0.2  P(node) =0.002234637
##     class counts:     2     8
##    probabilities: 0.200 0.800 
## 
## Node number 106: 87 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2873563  P(node) =0.01944134
##     class counts:    62    25
##    probabilities: 0.713 0.287 
##   left son=212 (41 obs) right son=213 (46 obs)
##   Primary splits:
##       WordCount.log < 6.597826 to the left,  improve=1.319353, (0 missing)
## 
## Node number 107: 19 observations
##   predicted class=Y  expected loss=0.4736842  P(node) =0.00424581
##     class counts:     9    10
##    probabilities: 0.474 0.526 
## 
## Node number 122: 515 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3475728  P(node) =0.1150838
##     class counts:   336   179
##    probabilities: 0.652 0.348 
##   left son=244 (190 obs) right son=245 (325 obs)
##   Primary splits:
##       WordCount.log < 6.982399 to the right, improve=2.835815, (0 missing)
## 
## Node number 123: 208 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4230769  P(node) =0.04648045
##     class counts:   120    88
##    probabilities: 0.577 0.423 
##   left son=246 (199 obs) right son=247 (9 obs)
##   Primary splits:
##       WordCount.log < 7.17434  to the right, improve=2.367049, (0 missing)
## 
## Node number 124: 125 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.384  P(node) =0.02793296
##     class counts:    77    48
##    probabilities: 0.616 0.384 
##   left son=248 (40 obs) right son=249 (85 obs)
##   Primary splits:
##       WordCount.log < 6.713563 to the right, improve=1.397765, (0 missing)
## 
## Node number 125: 52 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4807692  P(node) =0.01162011
##     class counts:    27    25
##    probabilities: 0.519 0.481 
##   left son=250 (40 obs) right son=251 (12 obs)
##   Primary splits:
##       WordCount.log < 6.745823 to the right, improve=2.261538, (0 missing)
## 
## Node number 212: 41 observations
##   predicted class=N  expected loss=0.195122  P(node) =0.009162011
##     class counts:    33     8
##    probabilities: 0.805 0.195 
## 
## Node number 213: 46 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3695652  P(node) =0.01027933
##     class counts:    29    17
##    probabilities: 0.630 0.370 
##   left son=426 (33 obs) right son=427 (13 obs)
##   Primary splits:
##       WordCount.log < 6.605974 to the right, improve=2.190027, (0 missing)
## 
## Node number 244: 190 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2789474  P(node) =0.0424581
##     class counts:   137    53
##    probabilities: 0.721 0.279 
##   left son=488 (8 obs) right son=489 (182 obs)
##   Primary splits:
##       WordCount.log < 7.158125 to the right, improve=1.299711, (0 missing)
## 
## Node number 245: 325 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3876923  P(node) =0.0726257
##     class counts:   199   126
##    probabilities: 0.612 0.388 
##   left son=490 (231 obs) right son=491 (94 obs)
##   Primary splits:
##       WordCount.log < 6.912741 to the left,  improve=0.9243624, (0 missing)
## 
## Node number 246: 199 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4070352  P(node) =0.04446927
##     class counts:   118    81
##    probabilities: 0.593 0.407 
##   left son=492 (114 obs) right son=493 (85 obs)
##   Primary splits:
##       WordCount.log < 7.275172 to the right, improve=0.7959052, (0 missing)
## 
## Node number 247: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.002011173
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 248: 40 observations
##   predicted class=N  expected loss=0.275  P(node) =0.008938547
##     class counts:    29    11
##    probabilities: 0.725 0.275 
## 
## Node number 249: 85 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4352941  P(node) =0.01899441
##     class counts:    48    37
##    probabilities: 0.565 0.435 
##   left son=498 (56 obs) right son=499 (29 obs)
##   Primary splits:
##       WordCount.log < 6.698884 to the left,  improve=1.193408, (0 missing)
## 
## Node number 250: 40 observations
##   predicted class=N  expected loss=0.4  P(node) =0.008938547
##     class counts:    24    16
##    probabilities: 0.600 0.400 
## 
## Node number 251: 12 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.002681564
##     class counts:     3     9
##    probabilities: 0.250 0.750 
## 
## Node number 426: 33 observations
##   predicted class=N  expected loss=0.2727273  P(node) =0.007374302
##     class counts:    24     9
##    probabilities: 0.727 0.273 
## 
## Node number 427: 13 observations
##   predicted class=Y  expected loss=0.3846154  P(node) =0.002905028
##     class counts:     5     8
##    probabilities: 0.385 0.615 
## 
## Node number 488: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.001787709
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 489: 182 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2912088  P(node) =0.04067039
##     class counts:   129    53
##    probabilities: 0.709 0.291 
##   left son=978 (127 obs) right son=979 (55 obs)
##   Primary splits:
##       WordCount.log < 7.088408 to the left,  improve=1.865726, (0 missing)
## 
## Node number 490: 231 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3636364  P(node) =0.05162011
##     class counts:   147    84
##    probabilities: 0.636 0.364 
##   left son=980 (36 obs) right son=981 (195 obs)
##   Primary splits:
##       WordCount.log < 6.892134 to the right, improve=1.705672, (0 missing)
## 
## Node number 491: 94 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4468085  P(node) =0.02100559
##     class counts:    52    42
##    probabilities: 0.553 0.447 
##   left son=982 (63 obs) right son=983 (31 obs)
##   Primary splits:
##       WordCount.log < 6.935857 to the right, improve=0.9545162, (0 missing)
## 
## Node number 492: 114 observations,    complexity param=0.0005340454
##   predicted class=N  expected loss=0.3684211  P(node) =0.02547486
##     class counts:    72    42
##    probabilities: 0.632 0.368 
##   left son=984 (35 obs) right son=985 (79 obs)
##   Primary splits:
##       WordCount.log < 7.345687 to the left,  improve=1.250823, (0 missing)
## 
## Node number 493: 85 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4588235  P(node) =0.01899441
##     class counts:    46    39
##    probabilities: 0.541 0.459 
##   left son=986 (69 obs) right son=987 (16 obs)
##   Primary splits:
##       WordCount.log < 7.257355 to the left,  improve=1.088576, (0 missing)
## 
## Node number 498: 56 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.375  P(node) =0.01251397
##     class counts:    35    21
##    probabilities: 0.625 0.375 
##   left son=996 (12 obs) right son=997 (44 obs)
##   Primary splits:
##       WordCount.log < 6.691463 to the right, improve=1.325758, (0 missing)
## 
## Node number 499: 29 observations
##   predicted class=Y  expected loss=0.4482759  P(node) =0.006480447
##     class counts:    13    16
##    probabilities: 0.448 0.552 
## 
## Node number 978: 127 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2440945  P(node) =0.02837989
##     class counts:    96    31
##    probabilities: 0.756 0.244 
##   left son=1956 (9 obs) right son=1957 (118 obs)
##   Primary splits:
##       WordCount.log < 7.080026 to the right, improve=1.154277, (0 missing)
## 
## Node number 979: 55 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.4  P(node) =0.0122905
##     class counts:    33    22
##    probabilities: 0.600 0.400 
##   left son=1958 (43 obs) right son=1959 (12 obs)
##   Primary splits:
##       WordCount.log < 7.100027 to the right, improve=1.031783, (0 missing)
## 
## Node number 980: 36 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.008044693
##     class counts:    28     8
##    probabilities: 0.778 0.222 
## 
## Node number 981: 195 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3897436  P(node) =0.04357542
##     class counts:   119    76
##    probabilities: 0.610 0.390 
##   left son=1962 (185 obs) right son=1963 (10 obs)
##   Primary splits:
##       WordCount.log < 6.884486 to the left,  improve=0.9319473, (0 missing)
## 
## Node number 982: 63 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3968254  P(node) =0.01407821
##     class counts:    38    25
##    probabilities: 0.603 0.397 
##   left son=1964 (10 obs) right son=1965 (53 obs)
##   Primary splits:
##       WordCount.log < 6.942156 to the left,  improve=2.094579, (0 missing)
## 
## Node number 983: 31 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.4516129  P(node) =0.006927374
##     class counts:    14    17
##    probabilities: 0.452 0.548 
##   left son=1966 (24 obs) right son=1967 (7 obs)
##   Primary splits:
##       WordCount.log < 6.928048 to the left,  improve=0.4976959, (0 missing)
## 
## Node number 984: 35 observations
##   predicted class=N  expected loss=0.2571429  P(node) =0.007821229
##     class counts:    26     9
##    probabilities: 0.743 0.257 
## 
## Node number 985: 79 observations,    complexity param=0.0005340454
##   predicted class=N  expected loss=0.4177215  P(node) =0.01765363
##     class counts:    46    33
##    probabilities: 0.582 0.418 
##   left son=1970 (67 obs) right son=1971 (12 obs)
##   Primary splits:
##       WordCount.log < 7.543801 to the left,  improve=0.1915738, (0 missing)
## 
## Node number 986: 69 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4202899  P(node) =0.01541899
##     class counts:    40    29
##    probabilities: 0.580 0.420 
##   left son=1972 (14 obs) right son=1973 (55 obs)
##   Primary splits:
##       WordCount.log < 7.191805 to the left,  improve=0.6361754, (0 missing)
## 
## Node number 987: 16 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.003575419
##     class counts:     6    10
##    probabilities: 0.375 0.625 
## 
## Node number 996: 12 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.002681564
##     class counts:    10     2
##    probabilities: 0.833 0.167 
## 
## Node number 997: 44 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4318182  P(node) =0.009832402
##     class counts:    25    19
##    probabilities: 0.568 0.432 
##   left son=1994 (35 obs) right son=1995 (9 obs)
##   Primary splits:
##       WordCount.log < 6.685236 to the left,  improve=2.708369, (0 missing)
## 
## Node number 1956: 9 observations
##   predicted class=N  expected loss=0  P(node) =0.002011173
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 1957: 118 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2627119  P(node) =0.02636872
##     class counts:    87    31
##    probabilities: 0.737 0.263 
##   left son=3914 (98 obs) right son=3915 (20 obs)
##   Primary splits:
##       WordCount.log < 7.060476 to the left,  improve=0.9077828, (0 missing)
## 
## Node number 1958: 43 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3488372  P(node) =0.009608939
##     class counts:    28    15
##    probabilities: 0.651 0.349 
##   left son=3916 (11 obs) right son=3917 (32 obs)
##   Primary splits:
##       WordCount.log < 7.11192  to the left,  improve=0.8246564, (0 missing)
## 
## Node number 1959: 12 observations
##   predicted class=Y  expected loss=0.4166667  P(node) =0.002681564
##     class counts:     5     7
##    probabilities: 0.417 0.583 
## 
## Node number 1962: 185 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3783784  P(node) =0.04134078
##     class counts:   115    70
##    probabilities: 0.622 0.378 
##   left son=3924 (164 obs) right son=3925 (21 obs)
##   Primary splits:
##       WordCount.log < 6.790659 to the right, improve=1.002056, (0 missing)
## 
## Node number 1963: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.002234637
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 1964: 10 observations
##   predicted class=N  expected loss=0.1  P(node) =0.002234637
##     class counts:     9     1
##    probabilities: 0.900 0.100 
## 
## Node number 1965: 53 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4528302  P(node) =0.01184358
##     class counts:    29    24
##    probabilities: 0.547 0.453 
##   left son=3930 (21 obs) right son=3931 (32 obs)
##   Primary splits:
##       WordCount.log < 6.956069 to the left,  improve=0.359389, (0 missing)
## 
## Node number 1966: 24 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.5  P(node) =0.005363128
##     class counts:    12    12
##    probabilities: 0.500 0.500 
##   left son=3932 (9 obs) right son=3933 (15 obs)
##   Primary splits:
##       WordCount.log < 6.920178 to the right, improve=0.8, (0 missing)
## 
## Node number 1967: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.001564246
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 1970: 67 observations,    complexity param=0.0005340454
##   predicted class=N  expected loss=0.4029851  P(node) =0.01497207
##     class counts:    40    27
##    probabilities: 0.597 0.403 
##   left son=3940 (8 obs) right son=3941 (59 obs)
##   Primary splits:
##       WordCount.log < 7.506042 to the right, improve=0.4252466, (0 missing)
## 
## Node number 1971: 12 observations
##   predicted class=N  expected loss=0.5  P(node) =0.002681564
##     class counts:     6     6
##    probabilities: 0.500 0.500 
## 
## Node number 1972: 14 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.003128492
##     class counts:    10     4
##    probabilities: 0.714 0.286 
## 
## Node number 1973: 55 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4545455  P(node) =0.0122905
##     class counts:    30    25
##    probabilities: 0.545 0.455 
##   left son=3946 (40 obs) right son=3947 (15 obs)
##   Primary splits:
##       WordCount.log < 7.20897  to the right, improve=0.8727273, (0 missing)
## 
## Node number 1994: 35 observations
##   predicted class=N  expected loss=0.3428571  P(node) =0.007821229
##     class counts:    23    12
##    probabilities: 0.657 0.343 
## 
## Node number 1995: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.002011173
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 3914: 98 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2346939  P(node) =0.02189944
##     class counts:    75    23
##    probabilities: 0.765 0.235 
##   left son=7828 (27 obs) right son=7829 (71 obs)
##   Primary splits:
##       WordCount.log < 7.044469 to the right, improve=0.5582809, (0 missing)
## 
## Node number 3915: 20 observations
##   predicted class=N  expected loss=0.4  P(node) =0.004469274
##     class counts:    12     8
##    probabilities: 0.600 0.400 
## 
## Node number 3916: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 3917: 32 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.40625  P(node) =0.007150838
##     class counts:    19    13
##    probabilities: 0.594 0.406 
##   left son=7834 (22 obs) right son=7835 (10 obs)
##   Primary splits:
##       WordCount.log < 7.127693 to the right, improve=1.092045, (0 missing)
## 
## Node number 3924: 164 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3597561  P(node) =0.03664804
##     class counts:   105    59
##    probabilities: 0.640 0.360 
##   left son=7848 (18 obs) right son=7849 (146 obs)
##   Primary splits:
##       WordCount.log < 6.873163 to the right, improve=0.7649144, (0 missing)
## 
## Node number 3925: 21 observations
##   predicted class=Y  expected loss=0.4761905  P(node) =0.004692737
##     class counts:    10    11
##    probabilities: 0.476 0.524 
## 
## Node number 3930: 21 observations
##   predicted class=N  expected loss=0.3809524  P(node) =0.004692737
##     class counts:    13     8
##    probabilities: 0.619 0.381 
## 
## Node number 3931: 32 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.5  P(node) =0.007150838
##     class counts:    16    16
##    probabilities: 0.500 0.500 
##   left son=7862 (23 obs) right son=7863 (9 obs)
##   Primary splits:
##       WordCount.log < 6.96319  to the right, improve=1.932367, (0 missing)
## 
## Node number 3932: 9 observations
##   predicted class=N  expected loss=0.3333333  P(node) =0.002011173
##     class counts:     6     3
##    probabilities: 0.667 0.333 
## 
## Node number 3933: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 3940: 8 observations
##   predicted class=N  expected loss=0.25  P(node) =0.001787709
##     class counts:     6     2
##    probabilities: 0.750 0.250 
## 
## Node number 3941: 59 observations,    complexity param=0.0005340454
##   predicted class=N  expected loss=0.4237288  P(node) =0.01318436
##     class counts:    34    25
##    probabilities: 0.576 0.424 
##   left son=7882 (27 obs) right son=7883 (32 obs)
##   Primary splits:
##       WordCount.log < 7.405491 to the left,  improve=0.2834667, (0 missing)
## 
## Node number 3946: 40 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4  P(node) =0.008938547
##     class counts:    24    16
##    probabilities: 0.600 0.400 
##   left son=7892 (7 obs) right son=7893 (33 obs)
##   Primary splits:
##       WordCount.log < 7.220371 to the left,  improve=1.122078, (0 missing)
## 
## Node number 3947: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 7828: 27 observations
##   predicted class=N  expected loss=0.1481481  P(node) =0.00603352
##     class counts:    23     4
##    probabilities: 0.852 0.148 
## 
## Node number 7829: 71 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2676056  P(node) =0.01586592
##     class counts:    52    19
##    probabilities: 0.732 0.268 
##   left son=15658 (52 obs) right son=15659 (19 obs)
##   Primary splits:
##       WordCount.log < 7.025094 to the left,  improve=0.5273422, (0 missing)
## 
## Node number 7834: 22 observations
##   predicted class=N  expected loss=0.3181818  P(node) =0.004916201
##     class counts:    15     7
##    probabilities: 0.682 0.318 
## 
## Node number 7835: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.002234637
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 7848: 18 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.004022346
##     class counts:    14     4
##    probabilities: 0.778 0.222 
## 
## Node number 7849: 146 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3767123  P(node) =0.0326257
##     class counts:    91    55
##    probabilities: 0.623 0.377 
##   left son=15698 (130 obs) right son=15699 (16 obs)
##   Primary splits:
##       WordCount.log < 6.862757 to the left,  improve=2.21549, (0 missing)
## 
## Node number 7862: 23 observations
##   predicted class=N  expected loss=0.3913043  P(node) =0.005139665
##     class counts:    14     9
##    probabilities: 0.609 0.391 
## 
## Node number 7863: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.002011173
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 7882: 27 observations
##   predicted class=N  expected loss=0.3703704  P(node) =0.00603352
##     class counts:    17    10
##    probabilities: 0.630 0.370 
## 
## Node number 7883: 32 observations,    complexity param=0.0005340454
##   predicted class=N  expected loss=0.46875  P(node) =0.007150838
##     class counts:    17    15
##    probabilities: 0.531 0.469 
##   left son=15766 (16 obs) right son=15767 (16 obs)
##   Primary splits:
##       WordCount.log < 7.444526 to the right, improve=0.5625, (0 missing)
## 
## Node number 7892: 7 observations
##   predicted class=N  expected loss=0.1428571  P(node) =0.001564246
##     class counts:     6     1
##    probabilities: 0.857 0.143 
## 
## Node number 7893: 33 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4545455  P(node) =0.007374302
##     class counts:    18    15
##    probabilities: 0.545 0.455 
##   left son=15786 (15 obs) right son=15787 (18 obs)
##   Primary splits:
##       WordCount.log < 7.238497 to the right, improve=0.8080808, (0 missing)
## 
## Node number 15658: 52 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2307692  P(node) =0.01162011
##     class counts:    40    12
##    probabilities: 0.769 0.231 
##   left son=31316 (11 obs) right son=31317 (41 obs)
##   Primary splits:
##       WordCount.log < 7.013015 to the right, improve=1.485929, (0 missing)
## 
## Node number 15659: 19 observations
##   predicted class=N  expected loss=0.3684211  P(node) =0.00424581
##     class counts:    12     7
##    probabilities: 0.632 0.368 
## 
## Node number 15698: 130 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3461538  P(node) =0.02905028
##     class counts:    85    45
##    probabilities: 0.654 0.346 
##   left son=31396 (32 obs) right son=31397 (98 obs)
##   Primary splits:
##       WordCount.log < 6.843217 to the right, improve=0.7849294, (0 missing)
## 
## Node number 15699: 16 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.003575419
##     class counts:     6    10
##    probabilities: 0.375 0.625 
## 
## Node number 15766: 16 observations
##   predicted class=N  expected loss=0.375  P(node) =0.003575419
##     class counts:    10     6
##    probabilities: 0.625 0.375 
## 
## Node number 15767: 16 observations
##   predicted class=Y  expected loss=0.4375  P(node) =0.003575419
##     class counts:     7     9
##    probabilities: 0.437 0.562 
## 
## Node number 15786: 15 observations
##   predicted class=N  expected loss=0.3333333  P(node) =0.003351955
##     class counts:    10     5
##    probabilities: 0.667 0.333 
## 
## Node number 15787: 18 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.004022346
##     class counts:     8    10
##    probabilities: 0.444 0.556 
## 
## Node number 31316: 11 observations
##   predicted class=N  expected loss=0  P(node) =0.002458101
##     class counts:    11     0
##    probabilities: 1.000 0.000 
## 
## Node number 31317: 41 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2926829  P(node) =0.009162011
##     class counts:    29    12
##    probabilities: 0.707 0.293 
##   left son=62634 (30 obs) right son=62635 (11 obs)
##   Primary splits:
##       WordCount.log < 7.004882 to the left,  improve=1.921064, (0 missing)
## 
## Node number 31396: 32 observations
##   predicted class=N  expected loss=0.25  P(node) =0.007150838
##     class counts:    24     8
##    probabilities: 0.750 0.250 
## 
## Node number 31397: 98 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.377551  P(node) =0.02189944
##     class counts:    61    37
##    probabilities: 0.622 0.378 
##   left son=62794 (83 obs) right son=62795 (15 obs)
##   Primary splits:
##       WordCount.log < 6.836796 to the left,  improve=1.752791, (0 missing)
## 
## Node number 62634: 30 observations
##   predicted class=N  expected loss=0.2  P(node) =0.006703911
##     class counts:    24     6
##    probabilities: 0.800 0.200 
## 
## Node number 62635: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.002458101
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 62794: 83 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3373494  P(node) =0.01854749
##     class counts:    55    28
##    probabilities: 0.663 0.337 
##   left son=125588 (11 obs) right son=125589 (72 obs)
##   Primary splits:
##       WordCount.log < 6.829253 to the right, improve=0.6134842, (0 missing)
## 
## Node number 62795: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 125588: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 125589: 72 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3611111  P(node) =0.01608939
##     class counts:    46    26
##    probabilities: 0.639 0.361 
##   left son=251178 (29 obs) right son=251179 (43 obs)
##   Primary splits:
##       WordCount.log < 6.807382 to the left,  improve=0.7057828, (0 missing)
## 
## Node number 251178: 29 observations
##   predicted class=N  expected loss=0.2758621  P(node) =0.006480447
##     class counts:    21     8
##    probabilities: 0.724 0.276 
## 
## Node number 251179: 43 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4186047  P(node) =0.009608939
##     class counts:    25    18
##    probabilities: 0.581 0.419 
##   left son=502358 (35 obs) right son=502359 (8 obs)
##   Primary splits:
##       WordCount.log < 6.810693 to the right, improve=2.158804, (0 missing)
## 
## Node number 502358: 35 observations
##   predicted class=N  expected loss=0.3428571  P(node) =0.007821229
##     class counts:    23    12
##    probabilities: 0.657 0.343 
## 
## Node number 502359: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001787709
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##      1) root 4475 749 N (0.8326257 0.1673743)  
##        2) WordCount.log< 6.528688 3276 329 N (0.8995726 0.1004274) *
##        3) WordCount.log>=6.528688 1199 420 N (0.6497081 0.3502919)  
##          6) WordCount.log< 6.663771 193  52 N (0.7305699 0.2694301)  
##           12) WordCount.log>=6.631343 62  10 N (0.8387097 0.1612903) *
##           13) WordCount.log< 6.631343 131  42 N (0.6793893 0.3206107)  
##             26) WordCount.log>=6.535966 121  37 N (0.6942149 0.3057851)  
##               52) WordCount.log< 6.548935 15   2 N (0.8666667 0.1333333) *
##               53) WordCount.log>=6.548935 106  35 N (0.6698113 0.3301887)  
##                106) WordCount.log>=6.566671 87  25 N (0.7126437 0.2873563)  
##                  212) WordCount.log< 6.597826 41   8 N (0.8048780 0.1951220) *
##                  213) WordCount.log>=6.597826 46  17 N (0.6304348 0.3695652)  
##                    426) WordCount.log>=6.605974 33   9 N (0.7272727 0.2727273) *
##                    427) WordCount.log< 6.605974 13   5 Y (0.3846154 0.6153846) *
##                107) WordCount.log< 6.566671 19   9 Y (0.4736842 0.5263158) *
##             27) WordCount.log< 6.535966 10   5 N (0.5000000 0.5000000) *
##          7) WordCount.log>=6.663771 1006 368 N (0.6341948 0.3658052)  
##           14) WordCount.log>=7.57327 85  20 N (0.7647059 0.2352941)  
##             28) WordCount.log< 8.229096 77  14 N (0.8181818 0.1818182) *
##             29) WordCount.log>=8.229096 8   2 Y (0.2500000 0.7500000) *
##           15) WordCount.log< 7.57327 921 348 N (0.6221498 0.3778502)  
##             30) WordCount.log>=6.775937 734 267 N (0.6362398 0.3637602)  
##               60) WordCount.log< 6.782759 11   0 N (1.0000000 0.0000000) *
##               61) WordCount.log>=6.782759 723 267 N (0.6307054 0.3692946)  
##                122) WordCount.log< 7.162785 515 179 N (0.6524272 0.3475728)  
##                  244) WordCount.log>=6.982399 190  53 N (0.7210526 0.2789474)  
##                    488) WordCount.log>=7.158125 8   0 N (1.0000000 0.0000000) *
##                    489) WordCount.log< 7.158125 182  53 N (0.7087912 0.2912088)  
##                      978) WordCount.log< 7.088408 127  31 N (0.7559055 0.2440945)  
##                       1956) WordCount.log>=7.080026 9   0 N (1.0000000 0.0000000) *
##                       1957) WordCount.log< 7.080026 118  31 N (0.7372881 0.2627119)  
##                         3914) WordCount.log< 7.060476 98  23 N (0.7653061 0.2346939)  
##                           7828) WordCount.log>=7.044469 27   4 N (0.8518519 0.1481481) *
##                           7829) WordCount.log< 7.044469 71  19 N (0.7323944 0.2676056)  
##                            15658) WordCount.log< 7.025094 52  12 N (0.7692308 0.2307692)  
##                              31316) WordCount.log>=7.013015 11   0 N (1.0000000 0.0000000) *
##                              31317) WordCount.log< 7.013015 41  12 N (0.7073171 0.2926829)  
##                                62634) WordCount.log< 7.004882 30   6 N (0.8000000 0.2000000) *
##                                62635) WordCount.log>=7.004882 11   5 Y (0.4545455 0.5454545) *
##                            15659) WordCount.log>=7.025094 19   7 N (0.6315789 0.3684211) *
##                         3915) WordCount.log>=7.060476 20   8 N (0.6000000 0.4000000) *
##                      979) WordCount.log>=7.088408 55  22 N (0.6000000 0.4000000)  
##                       1958) WordCount.log>=7.100027 43  15 N (0.6511628 0.3488372)  
##                         3916) WordCount.log< 7.11192 11   2 N (0.8181818 0.1818182) *
##                         3917) WordCount.log>=7.11192 32  13 N (0.5937500 0.4062500)  
##                           7834) WordCount.log>=7.127693 22   7 N (0.6818182 0.3181818) *
##                           7835) WordCount.log< 7.127693 10   4 Y (0.4000000 0.6000000) *
##                       1959) WordCount.log< 7.100027 12   5 Y (0.4166667 0.5833333) *
##                  245) WordCount.log< 6.982399 325 126 N (0.6123077 0.3876923)  
##                    490) WordCount.log< 6.912741 231  84 N (0.6363636 0.3636364)  
##                      980) WordCount.log>=6.892134 36   8 N (0.7777778 0.2222222) *
##                      981) WordCount.log< 6.892134 195  76 N (0.6102564 0.3897436)  
##                       1962) WordCount.log< 6.884486 185  70 N (0.6216216 0.3783784)  
##                         3924) WordCount.log>=6.790659 164  59 N (0.6402439 0.3597561)  
##                           7848) WordCount.log>=6.873163 18   4 N (0.7777778 0.2222222) *
##                           7849) WordCount.log< 6.873163 146  55 N (0.6232877 0.3767123)  
##                            15698) WordCount.log< 6.862757 130  45 N (0.6538462 0.3461538)  
##                              31396) WordCount.log>=6.843217 32   8 N (0.7500000 0.2500000) *
##                              31397) WordCount.log< 6.843217 98  37 N (0.6224490 0.3775510)  
##                                62794) WordCount.log< 6.836796 83  28 N (0.6626506 0.3373494)  
##                                 125588) WordCount.log>=6.829253 11   2 N (0.8181818 0.1818182) *
##                                 125589) WordCount.log< 6.829253 72  26 N (0.6388889 0.3611111)  
##                                   251178) WordCount.log< 6.807382 29   8 N (0.7241379 0.2758621) *
##                                   251179) WordCount.log>=6.807382 43  18 N (0.5813953 0.4186047)  
##                                     502358) WordCount.log>=6.810693 35  12 N (0.6571429 0.3428571) *
##                                     502359) WordCount.log< 6.810693 8   2 Y (0.2500000 0.7500000) *
##                                62795) WordCount.log>=6.836796 15   6 Y (0.4000000 0.6000000) *
##                            15699) WordCount.log>=6.862757 16   6 Y (0.3750000 0.6250000) *
##                         3925) WordCount.log< 6.790659 21  10 Y (0.4761905 0.5238095) *
##                       1963) WordCount.log>=6.884486 10   4 Y (0.4000000 0.6000000) *
##                    491) WordCount.log>=6.912741 94  42 N (0.5531915 0.4468085)  
##                      982) WordCount.log>=6.935857 63  25 N (0.6031746 0.3968254)  
##                       1964) WordCount.log< 6.942156 10   1 N (0.9000000 0.1000000) *
##                       1965) WordCount.log>=6.942156 53  24 N (0.5471698 0.4528302)  
##                         3930) WordCount.log< 6.956069 21   8 N (0.6190476 0.3809524) *
##                         3931) WordCount.log>=6.956069 32  16 N (0.5000000 0.5000000)  
##                           7862) WordCount.log>=6.96319 23   9 N (0.6086957 0.3913043) *
##                           7863) WordCount.log< 6.96319 9   2 Y (0.2222222 0.7777778) *
##                      983) WordCount.log< 6.935857 31  14 Y (0.4516129 0.5483871)  
##                       1966) WordCount.log< 6.928048 24  12 N (0.5000000 0.5000000)  
##                         3932) WordCount.log>=6.920178 9   3 N (0.6666667 0.3333333) *
##                         3933) WordCount.log< 6.920178 15   6 Y (0.4000000 0.6000000) *
##                       1967) WordCount.log>=6.928048 7   2 Y (0.2857143 0.7142857) *
##                123) WordCount.log>=7.162785 208  88 N (0.5769231 0.4230769)  
##                  246) WordCount.log>=7.17434 199  81 N (0.5929648 0.4070352)  
##                    492) WordCount.log>=7.275172 114  42 N (0.6315789 0.3684211)  
##                      984) WordCount.log< 7.345687 35   9 N (0.7428571 0.2571429) *
##                      985) WordCount.log>=7.345687 79  33 N (0.5822785 0.4177215)  
##                       1970) WordCount.log< 7.543801 67  27 N (0.5970149 0.4029851)  
##                         3940) WordCount.log>=7.506042 8   2 N (0.7500000 0.2500000) *
##                         3941) WordCount.log< 7.506042 59  25 N (0.5762712 0.4237288)  
##                           7882) WordCount.log< 7.405491 27  10 N (0.6296296 0.3703704) *
##                           7883) WordCount.log>=7.405491 32  15 N (0.5312500 0.4687500)  
##                            15766) WordCount.log>=7.444526 16   6 N (0.6250000 0.3750000) *
##                            15767) WordCount.log< 7.444526 16   7 Y (0.4375000 0.5625000) *
##                       1971) WordCount.log>=7.543801 12   6 N (0.5000000 0.5000000) *
##                    493) WordCount.log< 7.275172 85  39 N (0.5411765 0.4588235)  
##                      986) WordCount.log< 7.257355 69  29 N (0.5797101 0.4202899)  
##                       1972) WordCount.log< 7.191805 14   4 N (0.7142857 0.2857143) *
##                       1973) WordCount.log>=7.191805 55  25 N (0.5454545 0.4545455)  
##                         3946) WordCount.log>=7.20897 40  16 N (0.6000000 0.4000000)  
##                           7892) WordCount.log< 7.220371 7   1 N (0.8571429 0.1428571) *
##                           7893) WordCount.log>=7.220371 33  15 N (0.5454545 0.4545455)  
##                            15786) WordCount.log>=7.238497 15   5 N (0.6666667 0.3333333) *
##                            15787) WordCount.log< 7.238497 18   8 Y (0.4444444 0.5555556) *
##                         3947) WordCount.log< 7.20897 15   6 Y (0.4000000 0.6000000) *
##                      987) WordCount.log>=7.257355 16   6 Y (0.3750000 0.6250000) *
##                  247) WordCount.log< 7.17434 9   2 Y (0.2222222 0.7777778) *
##             31) WordCount.log< 6.775937 187  81 N (0.5668449 0.4331551)  
##               62) WordCount.log< 6.771362 177  73 N (0.5875706 0.4124294)  
##                124) WordCount.log< 6.736373 125  48 N (0.6160000 0.3840000)  
##                  248) WordCount.log>=6.713563 40  11 N (0.7250000 0.2750000) *
##                  249) WordCount.log< 6.713563 85  37 N (0.5647059 0.4352941)  
##                    498) WordCount.log< 6.698884 56  21 N (0.6250000 0.3750000)  
##                      996) WordCount.log>=6.691463 12   2 N (0.8333333 0.1666667) *
##                      997) WordCount.log< 6.691463 44  19 N (0.5681818 0.4318182)  
##                       1994) WordCount.log< 6.685236 35  12 N (0.6571429 0.3428571) *
##                       1995) WordCount.log>=6.685236 9   2 Y (0.2222222 0.7777778) *
##                    499) WordCount.log>=6.698884 29  13 Y (0.4482759 0.5517241) *
##                125) WordCount.log>=6.736373 52  25 N (0.5192308 0.4807692)  
##                  250) WordCount.log>=6.745823 40  16 N (0.6000000 0.4000000) *
##                  251) WordCount.log< 6.745823 12   3 Y (0.2500000 0.7500000) *
##               63) WordCount.log>=6.771362 10   2 Y (0.2000000 0.8000000) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-7.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.2890821
## 3        0.2 0.4572127
## 4        0.3 0.4481999
## 5        0.4 0.3744208
## 6        0.5 0.3744208
## 7        0.6 0.2414929
## 8        0.7 0.1339829
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3213
## 2            Y                                              375
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              513
## 2                                              374
##          Prediction
## Reference    N    Y
##         N 3213  513
##         Y  375  374
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.015642e-01   3.368571e-01   7.895723e-01   8.131613e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   4.277569e-06 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-9.png) 

```
##    threshold    f.score
## 1        0.0 0.28654727
## 2        0.1 0.28307434
## 3        0.2 0.37994723
## 4        0.3 0.32692308
## 5        0.4 0.19793814
## 6        0.5 0.19793814
## 7        0.6 0.13365155
## 8        0.7 0.07894737
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1443
## 2            Y                                              200
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              270
## 2                                              144
##          Prediction
## Reference    N    Y
##         N 1443  270
##         Y  200  144
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##    0.771511911    0.241360856    0.752744918    0.789501902    0.832766164 
## AccuracyPValue  McnemarPValue 
##    1.000000000    0.001458922 
##                    model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart WordCount.log               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.583                 0.058   0.7074274
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4572127        0.8015642
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7895723             0.8131613     0.3368571   0.6504263
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3799472        0.7715119
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7527449             0.7895019     0.2413609
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
## [1] "    indep_vars: WordCount.log"
## + Fold1: cp=0.001335 
## - Fold1: cp=0.001335 
## + Fold2: cp=0.001335 
## - Fold2: cp=0.001335 
## + Fold3: cp=0.001335 
## - Fold3: cp=0.001335 
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00223 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-11.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##            CP nsplit rel error
## 1 0.002225189      0         1
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
##          model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart WordCount.log               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                       1.32                 0.067         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8174308
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553    0.06210715         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002468564      0.01888548
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
## [1] "    indep_vars: WordCount.log"
## + Fold1: parameter=none 
## - Fold1: parameter=none 
## + Fold2: parameter=none 
## - Fold2: parameter=none 
## + Fold3: parameter=none 
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-13.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-14.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-15.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4353  -0.6490  -0.4807  -0.2734   3.2543  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -7.02059    0.33309  -21.08   <2e-16 ***
## WordCount.log  0.88928    0.05231   17.00   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 3670.9  on 4473  degrees of freedom
## AIC: 3674.9
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-17.png) 

```
##    threshold     f.score
## 1        0.0 0.286753446
## 2        0.1 0.353155973
## 3        0.2 0.422222222
## 4        0.3 0.287937743
## 5        0.4 0.080367394
## 6        0.5 0.023225806
## 7        0.6 0.013227513
## 8        0.7 0.005326232
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-18.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 2700
## 2            Y                                  274
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                 1026
## 2                                  475
##          Prediction
## Reference    N    Y
##         N 2700 1026
##         Y  274  475
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.094972e-01   2.560981e-01   6.959502e-01   7.227703e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.363819e-96 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-19.png) 

```
##    threshold     f.score
## 1        0.0 0.286547272
## 2        0.1 0.352486188
## 3        0.2 0.396172249
## 4        0.3 0.333868379
## 5        0.4 0.118483412
## 6        0.5 0.016713092
## 7        0.6 0.011527378
## 8        0.7 0.005797101
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-20.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1219
## 2            Y                                  137
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  494
## 2                                  207
##          Prediction
## Reference    N    Y
##         N 1219  494
##         Y  137  207
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   6.932426e-01   2.215049e-01   6.728061e-01   7.131270e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.362906e-45 
##        model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm WordCount.log               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.152                 0.076   0.7301738
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4222222        0.8306149
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.6959502             0.7227703    0.01169498   0.7342331
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3961722        0.6932426
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.6728061              0.713127     0.2215049    3674.923
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0016274     0.007870559
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
## [1] "    indep_vars: WordCount.log, WordCount.log:PubDate.apm.fctr, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:NewsDesk.nb.fctr, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log"
## + Fold1: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold1: parameter=none 
## + Fold2: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold2: parameter=none 
## + Fold3: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-21.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-22.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-23.png) 

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
## -2.6629  -0.4736  -0.2754  -0.0614   3.2523  
## 
## Coefficients: (1 not defined because of singularities)
##                                                          Estimate
## (Intercept)                                             -5.949751
## WordCount.log                                            1.986405
## `WordCount.log:PubDate.apm.fctrpm`                       0.012161
## `WordCount.log:S.can`                                   -0.060695
## `WordCount.log:S.make`                                   0.013242
## `WordCount.log:S.presid`                                 0.056105
## `WordCount.log:S.take`                                  -0.043158
## `WordCount.log:S.new`                                   -0.036109
## `WordCount.log:S.day`                                    0.022648
## `WordCount.log:S.show`                                  -0.084193
## `WordCount.log:S.report`                                -0.120921
## `WordCount.log:S.share`                                 -0.105246
## `WordCount.log:S.year`                                  -0.037969
## `WordCount.log:S.compani`                               -0.073189
## `WordCount.log:S.first`                                 -0.050030
## `WordCount.log:S.time`                                  -0.028166
## `WordCount.log:S.articl`                                 0.004321
## `WordCount.log:S.will`                                  -0.075063
## `WordCount.log:S.newyork`                               -0.031838
## `WordCount.log:S.intern`                                -0.108284
## `WordCount.log:H.week`                                  -0.090362
## `WordCount.log:S.week`                                  -0.030879
## `WordCount.log:S.fashion`                               -0.446053
## `WordCount.log:H.num.chars.log`                         -0.103766
## `WordCount.log:NewsDesk.nb.fctrCulture`                 -0.093376
## `WordCount.log:NewsDesk.nb.fctrScience`                  0.353980
## `WordCount.log:NewsDesk.nb.fctrOpEd`                     0.216276
## `WordCount.log:NewsDesk.nb.fctrForeign`                 -0.420056
## `WordCount.log:NewsDesk.nb.fctrStyles`                   0.263795
## `WordCount.log:NewsDesk.nb.fctrTStyle`                  -0.307173
## `WordCount.log:NewsDesk.nb.fctrMagazine`                -3.459692
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia`            -0.428762
## `WordCount.log:NewsDesk.nb.fctrmyMisc::`                -0.068352
## `WordCount.log:NewsDesk.nb.fctrTravel`                  -0.433302
## `WordCount.log:NewsDesk.nb.fctrMetro`                   -0.157217
## `WordCount.log:NewsDesk.nb.fctrDaily Clip Report::`     -3.050037
## `WordCount.log:NewsDesk.nb.fctr6 Q's About the News::`  -3.217485
## `WordCount.log:NewsDesk.nb.fctrTest Yourself::`         -3.923014
## `WordCount.log:NewsDesk.nb.fctrWord of the Day::`       -3.113394
## `WordCount.log:NewsDesk.nb.fctr19[0-9][0-9]::`          -3.072256
## `WordCount.log:NewsDesk.nb.fctrReaders Respond::`        0.241438
## `WordCount.log:NewsDesk.nb.fctrNational`                -3.052473
## `WordCount.log:NewsDesk.nb.fctrSports`                  -2.182129
## `WordCount.log:NewsDesk.nb.fctrQuiz(.*)([?=|]|[?=:]::`   0.293949
## `WordCount.log:NewsDesk.nb.fctrmyTech::`                -2.547105
## `WordCount.log:NewsDesk.nb.fctrVerbatim::`              -4.261874
## `WordCount.log:NewsDesk.nb.fctrFirst Draft::`           -4.728823
## `WordCount.log:NewsDesk.nb.fctrToday in Politics::`     -2.577965
## `WordCount.log:NewsDesk.nb.fctrmyFood::`                -3.292997
## `WordCount.log:NewsDesk.nb.fctrThe Daily Gift::`               NA
## `WordCount.log:A.num.chars.log`                         -1.128772
## `WordCount.log:A.num.words.log`                          0.017209
## `WordCount.log:S.num.chars.log`                          0.940070
##                                                        Std. Error z value
## (Intercept)                                              0.414935 -14.339
## WordCount.log                                            0.177548  11.188
## `WordCount.log:PubDate.apm.fctrpm`                       0.017386   0.699
## `WordCount.log:S.can`                                    0.037065  -1.638
## `WordCount.log:S.make`                                   0.036963   0.358
## `WordCount.log:S.presid`                                 0.043263   1.297
## `WordCount.log:S.take`                                   0.050770  -0.850
## `WordCount.log:S.new`                                    0.029354  -1.230
## `WordCount.log:S.day`                                    0.047246   0.479
## `WordCount.log:S.show`                                   0.056365  -1.494
## `WordCount.log:S.report`                                 0.050886  -2.376
## `WordCount.log:S.share`                                  0.060791  -1.731
## `WordCount.log:S.year`                                   0.041145  -0.923
## `WordCount.log:S.compani`                                0.038061  -1.923
## `WordCount.log:S.first`                                  0.058537  -0.855
## `WordCount.log:S.time`                                   0.035896  -0.785
## `WordCount.log:S.articl`                                 0.088441   0.049
## `WordCount.log:S.will`                                   0.031614  -2.374
## `WordCount.log:S.newyork`                                0.048423  -0.657
## `WordCount.log:S.intern`                                 0.111136  -0.974
## `WordCount.log:H.week`                                   0.084717  -1.067
## `WordCount.log:S.week`                                   0.048740  -0.634
## `WordCount.log:S.fashion`                                0.163253  -2.732
## `WordCount.log:H.num.chars.log`                          0.021716  -4.778
## `WordCount.log:NewsDesk.nb.fctrCulture`                  0.032136  -2.906
## `WordCount.log:NewsDesk.nb.fctrScience`                  0.037004   9.566
## `WordCount.log:NewsDesk.nb.fctrOpEd`                     0.025245   8.567
## `WordCount.log:NewsDesk.nb.fctrForeign`                  0.115275  -3.644
## `WordCount.log:NewsDesk.nb.fctrStyles`                   0.031927   8.262
## `WordCount.log:NewsDesk.nb.fctrTStyle`                   0.057181  -5.372
## `WordCount.log:NewsDesk.nb.fctrMagazine`               252.723901  -0.014
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia`             0.165143  -2.596
## `WordCount.log:NewsDesk.nb.fctrmyMisc::`                 0.025689  -2.661
## `WordCount.log:NewsDesk.nb.fctrTravel`                   0.175500  -2.469
## `WordCount.log:NewsDesk.nb.fctrMetro`                    0.055573  -2.829
## `WordCount.log:NewsDesk.nb.fctrDaily Clip Report::`    185.792426  -0.016
## `WordCount.log:NewsDesk.nb.fctr6 Q's About the News::` 202.111959  -0.016
## `WordCount.log:NewsDesk.nb.fctrTest Yourself::`        256.990476  -0.015
## `WordCount.log:NewsDesk.nb.fctrWord of the Day::`      183.621811  -0.017
## `WordCount.log:NewsDesk.nb.fctr19[0-9][0-9]::`         452.382505  -0.007
## `WordCount.log:NewsDesk.nb.fctrReaders Respond::`        0.107444   2.247
## `WordCount.log:NewsDesk.nb.fctrNational`               579.557714  -0.005
## `WordCount.log:NewsDesk.nb.fctrSports`                 885.376736  -0.002
## `WordCount.log:NewsDesk.nb.fctrQuiz(.*)([?=|]|[?=:]::`   0.259260   1.134
## `WordCount.log:NewsDesk.nb.fctrmyTech::`               554.567585  -0.005
## `WordCount.log:NewsDesk.nb.fctrVerbatim::`             368.440255  -0.012
## `WordCount.log:NewsDesk.nb.fctrFirst Draft::`          242.781963  -0.019
## `WordCount.log:NewsDesk.nb.fctrToday in Politics::`    159.312219  -0.016
## `WordCount.log:NewsDesk.nb.fctrmyFood::`               570.386417  -0.006
## `WordCount.log:NewsDesk.nb.fctrThe Daily Gift::`               NA      NA
## `WordCount.log:A.num.chars.log`                          0.858642  -1.315
## `WordCount.log:A.num.words.log`                          0.069043   0.249
## `WordCount.log:S.num.chars.log`                          0.858514   1.095
##                                                        Pr(>|z|)    
## (Intercept)                                             < 2e-16 ***
## WordCount.log                                           < 2e-16 ***
## `WordCount.log:PubDate.apm.fctrpm`                     0.484271    
## `WordCount.log:S.can`                                  0.101518    
## `WordCount.log:S.make`                                 0.720159    
## `WordCount.log:S.presid`                               0.194687    
## `WordCount.log:S.take`                                 0.395277    
## `WordCount.log:S.new`                                  0.218647    
## `WordCount.log:S.day`                                  0.631682    
## `WordCount.log:S.show`                                 0.135250    
## `WordCount.log:S.report`                               0.017487 *  
## `WordCount.log:S.share`                                0.083403 .  
## `WordCount.log:S.year`                                 0.356113    
## `WordCount.log:S.compani`                              0.054488 .  
## `WordCount.log:S.first`                                0.392733    
## `WordCount.log:S.time`                                 0.432666    
## `WordCount.log:S.articl`                               0.961038    
## `WordCount.log:S.will`                                 0.017578 *  
## `WordCount.log:S.newyork`                              0.510865    
## `WordCount.log:S.intern`                               0.329891    
## `WordCount.log:H.week`                                 0.286134    
## `WordCount.log:S.week`                                 0.526381    
## `WordCount.log:S.fashion`                              0.006290 ** 
## `WordCount.log:H.num.chars.log`                        1.77e-06 ***
## `WordCount.log:NewsDesk.nb.fctrCulture`                0.003665 ** 
## `WordCount.log:NewsDesk.nb.fctrScience`                 < 2e-16 ***
## `WordCount.log:NewsDesk.nb.fctrOpEd`                    < 2e-16 ***
## `WordCount.log:NewsDesk.nb.fctrForeign`                0.000268 ***
## `WordCount.log:NewsDesk.nb.fctrStyles`                  < 2e-16 ***
## `WordCount.log:NewsDesk.nb.fctrTStyle`                 7.79e-08 ***
## `WordCount.log:NewsDesk.nb.fctrMagazine`               0.989078    
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia`           0.009423 ** 
## `WordCount.log:NewsDesk.nb.fctrmyMisc::`               0.007797 ** 
## `WordCount.log:NewsDesk.nb.fctrTravel`                 0.013551 *  
## `WordCount.log:NewsDesk.nb.fctrMetro`                  0.004669 ** 
## `WordCount.log:NewsDesk.nb.fctrDaily Clip Report::`    0.986902    
## `WordCount.log:NewsDesk.nb.fctr6 Q's About the News::` 0.987299    
## `WordCount.log:NewsDesk.nb.fctrTest Yourself::`        0.987821    
## `WordCount.log:NewsDesk.nb.fctrWord of the Day::`      0.986472    
## `WordCount.log:NewsDesk.nb.fctr19[0-9][0-9]::`         0.994581    
## `WordCount.log:NewsDesk.nb.fctrReaders Respond::`      0.024633 *  
## `WordCount.log:NewsDesk.nb.fctrNational`               0.995798    
## `WordCount.log:NewsDesk.nb.fctrSports`                 0.998034    
## `WordCount.log:NewsDesk.nb.fctrQuiz(.*)([?=|]|[?=:]::` 0.256878    
## `WordCount.log:NewsDesk.nb.fctrmyTech::`               0.996335    
## `WordCount.log:NewsDesk.nb.fctrVerbatim::`             0.990771    
## `WordCount.log:NewsDesk.nb.fctrFirst Draft::`          0.984460    
## `WordCount.log:NewsDesk.nb.fctrToday in Politics::`    0.987089    
## `WordCount.log:NewsDesk.nb.fctrmyFood::`               0.995394    
## `WordCount.log:NewsDesk.nb.fctrThe Daily Gift::`             NA    
## `WordCount.log:A.num.chars.log`                        0.188644    
## `WordCount.log:A.num.words.log`                        0.803172    
## `WordCount.log:S.num.chars.log`                        0.273518    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 2596.7  on 4423  degrees of freedom
## AIC: 2700.7
## 
## Number of Fisher Scoring iterations: 17
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

![](NYTBlogs_Metro_files/figure-html/fit.models_0-24.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-25.png) 

```
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.51897279
## 3        0.2 0.64858882
## 4        0.3 0.64928292
## 5        0.4 0.62955032
## 6        0.5 0.56624606
## 7        0.6 0.46631206
## 8        0.7 0.37907206
## 9        0.8 0.26030369
## 10       0.9 0.05350318
## 11       1.0 0.00000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3439
## 2            Y                                            251
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            287
## 2                                            498
##          Prediction
## Reference    N    Y
##         N 3439  287
##         Y  251  498
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.797765e-01   5.767853e-01   8.698876e-01   8.891668e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   7.600180e-19   1.313097e-01 
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

![](NYTBlogs_Metro_files/figure-html/fit.models_0-26.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-27.png) 

```
##    threshold    f.score
## 1        0.0 0.28654727
## 2        0.1 0.51033386
## 3        0.2 0.63255814
## 4        0.3 0.65667575
## 5        0.4 0.67166417
## 6        0.5 0.61487603
## 7        0.6 0.51962617
## 8        0.7 0.44444444
## 9        0.8 0.25238095
## 10       0.9 0.07142857
## 11       1.0 0.00000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-28.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1614
## 2            Y                                            120
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             99
## 2                                            224
##          Prediction
## Reference    N    Y
##         N 1614   99
##         Y  120  224
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.935343e-01   6.082058e-01   8.793971e-01   9.065382e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   3.705874e-15   1.765434e-01 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## 1 WordCount.log, WordCount.log:PubDate.apm.fctr, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:NewsDesk.nb.fctr, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      4.105                     1
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8958855                    0.3       0.6492829        0.8737453
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8698876             0.8891668     0.4854243   0.9062453
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.6716642        0.8935343
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8793971             0.9065382     0.6082058    2700.718
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01035211      0.04919088
```

```r
# Low.cor.X
if (glb_is_classification && glb_is_binomial)
    indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
                                            is.ConditionalX.y & 
                                            (exclude.as.feat != 1))[, "id"] else
    indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
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
## [1] "    indep_vars: WordCount.log, H.is.question, PubDate.apm.fctr, S.can, H.has.ebola, S.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, S.take, PubDate.minute, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log"
## + Fold1: parameter=none
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold1: parameter=none 
## + Fold2: parameter=none
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold2: parameter=none 
## + Fold3: parameter=none
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold3: parameter=none 
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
##   297, 3058, 3256
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-29.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-30.png) 

```
## Warning: not plotting observations with leverage one:
##   297, 3058, 3256
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-31.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -8.49    0.00    0.00    0.00    8.49  
## 
## Coefficients: (30 not defined because of singularities)
##                                                     Estimate Std. Error
## (Intercept)                                        8.693e+14  2.318e+07
## WordCount.log                                      1.454e+14  1.249e+06
## H.is.question                                     -3.783e+14  4.679e+06
## PubDate.apm.fctrpm                                -5.988e+13  2.343e+06
## S.can                                             -3.936e+14  5.741e+06
## H.has.ebola                                       -7.818e+12  9.134e+06
## S.make                                            -7.973e+13  5.277e+06
## S.one                                             -7.476e+14  4.875e+07
## S.state                                            1.017e+14  5.575e+06
## A.state                                                   NA         NA
## A.one                                              7.756e+14  4.837e+07
## S.said                                             1.143e+14  5.259e+06
## A.said                                                    NA         NA
## .rnorm                                             8.161e+12  1.008e+06
## `PubDate.date.fctr(7,13]`                          8.588e+13  3.141e+06
## `PubDate.date.fctr(13,19]`                         1.996e+13  3.115e+06
## `PubDate.date.fctr(19,25]`                        -1.936e+13  3.029e+06
## `PubDate.date.fctr(25,31]`                         1.532e+13  3.376e+06
## PubDate.second                                    -7.451e+10  5.837e+04
## S.presid                                           5.829e+13  5.112e+06
## S.take                                             3.926e+13  6.016e+06
## PubDate.minute                                     1.461e+11  5.802e+04
## S.new                                             -1.074e+13  3.699e+06
## PubDate.wkday.fctr1                               -4.176e+13  5.305e+06
## PubDate.wkday.fctr2                               -1.418e+14  5.311e+06
## PubDate.wkday.fctr3                               -1.107e+14  5.291e+06
## PubDate.wkday.fctr4                               -1.903e+14  5.296e+06
## PubDate.wkday.fctr5                               -1.438e+14  5.330e+06
## PubDate.wkday.fctr6                               -2.132e+14  7.858e+06
## S.day                                             -3.399e+13  6.247e+06
## H.X2014                                           -1.331e+15  1.011e+07
## S.show                                            -2.419e+14  5.407e+06
## S.report                                           1.033e+14  6.019e+06
## S.share                                           -7.693e+14  6.348e+06
## S.year                                            -1.950e+14  4.721e+06
## S.compani                                         -1.530e+14  4.475e+06
## H.new                                             -5.626e+14  5.574e+06
## S.first                                           -5.002e+13  5.757e+06
## S.time                                             4.700e+13  4.383e+06
## H.newyork                                         -4.579e+14  7.660e+06
## S.articl                                          -2.244e+14  9.945e+06
## S.will                                            -2.651e+14  3.466e+06
## H.day                                             -1.455e+13  8.613e+06
## S.newyork                                          6.173e+12  5.021e+06
## H.today                                           -3.338e+15  2.763e+07
## H.report                                          -1.619e+14  9.782e+06
## S.intern                                          -1.880e+13  1.076e+07
## H.week                                            -8.401e+13  1.178e+07
## S.week                                            -2.796e+14  5.005e+06
## S.fashion                                          4.390e+14  7.054e+06
## `Headline.pfx.fctr19[0-9][0-9]::`                 -3.536e+15  1.926e+07
## `Headline.pfx.fctrDaily Report::`                 -3.555e+15  1.571e+07
## `Headline.pfx.fctr.*Fashion Week::`                1.079e+15  1.414e+07
## `Headline.pfx.fctrWhat We're::`                   -4.625e+15  1.468e+07
## `Headline.pfx.fctrPictures of the (Day|Year|.)::` -1.503e+15  1.732e+07
## `Headline.pfx.fctrToday in Small Business::`       2.537e+15  2.968e+07
## `Headline.pfx.fctrDaily Clip Report::`            -5.352e+15  2.013e+07
## `Headline.pfx.fctrMorning Agenda::`               -4.182e+15  1.137e+07
## `Headline.pfx.fctrNew York Today::`                2.081e+15  3.102e+07
## `Headline.pfx.fctr6 Q's About the News::`         -7.055e+14  5.147e+07
## `Headline.pfx.fctrTest Yourself::`                -7.260e+13  5.155e+07
## `Headline.pfx.fctrWord of the Day::`               1.670e+15  5.253e+07
## `Headline.pfx.fctrmyTech::`                       -4.765e+14  8.948e+06
## `Headline.pfx.fctrYour Turn::`                    -2.682e+14  2.471e+07
## `Headline.pfx.fctrReaders Respond::`              -1.224e+15  3.920e+07
## `Headline.pfx.fctrAsk Well::`                      2.414e+14  2.888e+07
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`          1.133e+15  5.781e+07
## `Headline.pfx.fctrVerbatim::`                     -2.491e+15  1.683e+07
## `Headline.pfx.fctrFirst Draft::`                  -5.593e+15  1.369e+07
## `Headline.pfx.fctrToday in Politics::`            -2.475e+15  3.134e+07
## `Headline.pfx.fctrmyFood::`                       -6.023e+14  1.561e+07
## `Headline.pfx.fctrThe Daily Gift::`                2.206e+15  1.828e+07
## SectionName.nb.fctrArts                           -1.918e+15  8.818e+06
## `SectionName.nb.fctrBusiness Day`                 -1.654e+15  8.972e+06
## SectionName.nb.fctrHealth                         -9.940e+14  1.013e+07
## SectionName.nb.fctrOpinion                        -5.753e+13  8.427e+06
## SectionName.nb.fctrForeign                        -2.119e+15  2.222e+07
## SectionName.nb.fctrStyles                         -7.915e+15  5.211e+07
## SectionName.nb.fctrTStyle                         -3.723e+15  8.884e+06
## SectionName.nb.fctrWorld                          -3.887e+15  1.049e+07
## SectionName.nb.fctrTechnology                     -1.023e+15  1.019e+07
## SectionName.nb.fctrMagazine                       -5.509e+15  1.712e+07
## SectionName.nb.fctrMultimedia                     -2.920e+15  1.183e+07
## `SectionName.nb.fctrmyMisc::`                     -1.513e+15  5.143e+07
## SectionName.nb.fctrTravel                         -5.019e+15  1.087e+07
## SectionName.nb.fctrU.S.                           -4.875e+15  5.091e+07
## `SectionName.nb.fctrN.Y. / Region`                -1.676e+15  1.077e+07
## `SectionName.nb.fctrDaily Clip Report::`                  NA         NA
## SectionName.nb.fctrOpen                           -3.106e+15  7.028e+07
## `SectionName.nb.fctr19[0-9][0-9]::`               -2.148e+15  3.380e+07
## `SectionName.nb.fctrReaders Respond::`             1.579e+15  4.579e+07
## SectionName.nb.fctrSports                         -4.456e+15  6.973e+07
## SectionName.nb.fctrNational                       -2.643e+15  8.498e+07
## `SectionName.nb.fctrVerbatim::`                           NA         NA
## `SectionName.nb.fctrFirst Draft::`                        NA         NA
## `SectionName.nb.fctrToday in Politics::`                  NA         NA
## `SectionName.nb.fctrmyTech::`                     -6.623e+14  9.719e+07
## `SectionName.nb.fctrmyFood::`                     -5.189e+15  5.089e+07
## SectionName.nb.fctrCulture                                NA         NA
## `SectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::`       -2.182e+15  8.903e+07
## SectionName.nb.fctrBusiness                       -1.881e+15  3.592e+07
## `SectionName.nb.fctrThe Daily Gift::`                     NA         NA
## NewsDesk.nb.fctrCulture                                   NA         NA
## NewsDesk.nb.fctrScience                                   NA         NA
## NewsDesk.nb.fctrOpEd                                      NA         NA
## NewsDesk.nb.fctrForeign                                   NA         NA
## NewsDesk.nb.fctrStyles                             3.569e+15  5.058e+07
## NewsDesk.nb.fctrTStyle                                    NA         NA
## NewsDesk.nb.fctrMagazine                                  NA         NA
## NewsDesk.nb.fctrmyMultimedia                              NA         NA
## `NewsDesk.nb.fctrmyMisc::`                        -3.606e+14  5.080e+07
## NewsDesk.nb.fctrTravel                                    NA         NA
## NewsDesk.nb.fctrMetro                                     NA         NA
## `NewsDesk.nb.fctrDaily Clip Report::`                     NA         NA
## `NewsDesk.nb.fctr6 Q's About the News::`                  NA         NA
## `NewsDesk.nb.fctrTest Yourself::`                         NA         NA
## `NewsDesk.nb.fctrWord of the Day::`                       NA         NA
## `NewsDesk.nb.fctr19[0-9][0-9]::`                          NA         NA
## `NewsDesk.nb.fctrReaders Respond::`                       NA         NA
## NewsDesk.nb.fctrNational                          -4.790e+14  7.009e+07
## NewsDesk.nb.fctrSports                                    NA         NA
## `NewsDesk.nb.fctrQuiz(.*)([?=|]|[?=:]::`                  NA         NA
## `NewsDesk.nb.fctrmyTech::`                        -1.624e+14  6.972e+07
## `NewsDesk.nb.fctrVerbatim::`                              NA         NA
## `NewsDesk.nb.fctrFirst Draft::`                           NA         NA
## `NewsDesk.nb.fctrToday in Politics::`                     NA         NA
## `NewsDesk.nb.fctrmyFood::`                                NA         NA
## `NewsDesk.nb.fctrThe Daily Gift::`                        NA         NA
## H.num.chars.log                                   -7.110e+12  6.487e+06
## H.num.words.log                                    1.263e+14  7.656e+06
## A.num.chars.log                                   -3.120e+14  8.794e+06
## A.num.words.log                                    3.008e+14  2.854e+07
## A.num.words.unq.log                                7.045e+13  2.751e+07
##                                                      z value Pr(>|z|)    
## (Intercept)                                         37507284   <2e-16 ***
## WordCount.log                                      116410486   <2e-16 ***
## H.is.question                                      -80864091   <2e-16 ***
## PubDate.apm.fctrpm                                 -25554899   <2e-16 ***
## S.can                                              -68554334   <2e-16 ***
## H.has.ebola                                          -855941   <2e-16 ***
## S.make                                             -15109941   <2e-16 ***
## S.one                                              -15336363   <2e-16 ***
## S.state                                             18234154   <2e-16 ***
## A.state                                                   NA       NA    
## A.one                                               16036057   <2e-16 ***
## S.said                                              21731132   <2e-16 ***
## A.said                                                    NA       NA    
## .rnorm                                               8099058   <2e-16 ***
## `PubDate.date.fctr(7,13]`                           27338669   <2e-16 ***
## `PubDate.date.fctr(13,19]`                           6406456   <2e-16 ***
## `PubDate.date.fctr(19,25]`                          -6391256   <2e-16 ***
## `PubDate.date.fctr(25,31]`                           4537684   <2e-16 ***
## PubDate.second                                      -1276463   <2e-16 ***
## S.presid                                            11402505   <2e-16 ***
## S.take                                               6525739   <2e-16 ***
## PubDate.minute                                       2518938   <2e-16 ***
## S.new                                               -2904246   <2e-16 ***
## PubDate.wkday.fctr1                                 -7870741   <2e-16 ***
## PubDate.wkday.fctr2                                -26698873   <2e-16 ***
## PubDate.wkday.fctr3                                -20927771   <2e-16 ***
## PubDate.wkday.fctr4                                -35924768   <2e-16 ***
## PubDate.wkday.fctr5                                -26986546   <2e-16 ***
## PubDate.wkday.fctr6                                -27134988   <2e-16 ***
## S.day                                               -5440087   <2e-16 ***
## H.X2014                                           -131685174   <2e-16 ***
## S.show                                             -44747580   <2e-16 ***
## S.report                                            17155178   <2e-16 ***
## S.share                                           -121191314   <2e-16 ***
## S.year                                             -41300850   <2e-16 ***
## S.compani                                          -34178189   <2e-16 ***
## H.new                                             -100933799   <2e-16 ***
## S.first                                             -8689009   <2e-16 ***
## S.time                                              10722546   <2e-16 ***
## H.newyork                                          -59777355   <2e-16 ***
## S.articl                                           -22560813   <2e-16 ***
## S.will                                             -76473282   <2e-16 ***
## H.day                                               -1689871   <2e-16 ***
## S.newyork                                            1229313   <2e-16 ***
## H.today                                           -120793853   <2e-16 ***
## H.report                                           -16553278   <2e-16 ***
## S.intern                                            -1746198   <2e-16 ***
## H.week                                              -7132884   <2e-16 ***
## S.week                                             -55853731   <2e-16 ***
## S.fashion                                           62244168   <2e-16 ***
## `Headline.pfx.fctr19[0-9][0-9]::`                 -183562097   <2e-16 ***
## `Headline.pfx.fctrDaily Report::`                 -226329562   <2e-16 ***
## `Headline.pfx.fctr.*Fashion Week::`                 76315064   <2e-16 ***
## `Headline.pfx.fctrWhat We're::`                   -315001073   <2e-16 ***
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`  -86802876   <2e-16 ***
## `Headline.pfx.fctrToday in Small Business::`        85464160   <2e-16 ***
## `Headline.pfx.fctrDaily Clip Report::`            -265861078   <2e-16 ***
## `Headline.pfx.fctrMorning Agenda::`               -367664889   <2e-16 ***
## `Headline.pfx.fctrNew York Today::`                 67111077   <2e-16 ***
## `Headline.pfx.fctr6 Q's About the News::`          -13706859   <2e-16 ***
## `Headline.pfx.fctrTest Yourself::`                  -1408174   <2e-16 ***
## `Headline.pfx.fctrWord of the Day::`                31789226   <2e-16 ***
## `Headline.pfx.fctrmyTech::`                        -53249505   <2e-16 ***
## `Headline.pfx.fctrYour Turn::`                     -10852580   <2e-16 ***
## `Headline.pfx.fctrReaders Respond::`               -31215821   <2e-16 ***
## `Headline.pfx.fctrAsk Well::`                        8357882   <2e-16 ***
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`           19600234   <2e-16 ***
## `Headline.pfx.fctrVerbatim::`                     -148075537   <2e-16 ***
## `Headline.pfx.fctrFirst Draft::`                  -408473833   <2e-16 ***
## `Headline.pfx.fctrToday in Politics::`             -78977330   <2e-16 ***
## `Headline.pfx.fctrmyFood::`                        -38570073   <2e-16 ***
## `Headline.pfx.fctrThe Daily Gift::`                120683647   <2e-16 ***
## SectionName.nb.fctrArts                           -217556980   <2e-16 ***
## `SectionName.nb.fctrBusiness Day`                 -184345209   <2e-16 ***
## SectionName.nb.fctrHealth                          -98132747   <2e-16 ***
## SectionName.nb.fctrOpinion                          -6827355   <2e-16 ***
## SectionName.nb.fctrForeign                         -95352879   <2e-16 ***
## SectionName.nb.fctrStyles                         -151879702   <2e-16 ***
## SectionName.nb.fctrTStyle                         -419059604   <2e-16 ***
## SectionName.nb.fctrWorld                          -370687412   <2e-16 ***
## SectionName.nb.fctrTechnology                     -100397600   <2e-16 ***
## SectionName.nb.fctrMagazine                       -321833840   <2e-16 ***
## SectionName.nb.fctrMultimedia                     -246772847   <2e-16 ***
## `SectionName.nb.fctrmyMisc::`                      -29413246   <2e-16 ***
## SectionName.nb.fctrTravel                         -461602784   <2e-16 ***
## SectionName.nb.fctrU.S.                            -95748236   <2e-16 ***
## `SectionName.nb.fctrN.Y. / Region`                -155655963   <2e-16 ***
## `SectionName.nb.fctrDaily Clip Report::`                  NA       NA    
## SectionName.nb.fctrOpen                            -44197043   <2e-16 ***
## `SectionName.nb.fctr19[0-9][0-9]::`                -63541420   <2e-16 ***
## `SectionName.nb.fctrReaders Respond::`              34476082   <2e-16 ***
## SectionName.nb.fctrSports                          -63904980   <2e-16 ***
## SectionName.nb.fctrNational                        -31105027   <2e-16 ***
## `SectionName.nb.fctrVerbatim::`                           NA       NA    
## `SectionName.nb.fctrFirst Draft::`                        NA       NA    
## `SectionName.nb.fctrToday in Politics::`                  NA       NA    
## `SectionName.nb.fctrmyTech::`                       -6814494   <2e-16 ***
## `SectionName.nb.fctrmyFood::`                     -101974556   <2e-16 ***
## SectionName.nb.fctrCulture                                NA       NA    
## `SectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::`        -24507368   <2e-16 ***
## SectionName.nb.fctrBusiness                        -52385778   <2e-16 ***
## `SectionName.nb.fctrThe Daily Gift::`                     NA       NA    
## NewsDesk.nb.fctrCulture                                   NA       NA    
## NewsDesk.nb.fctrScience                                   NA       NA    
## NewsDesk.nb.fctrOpEd                                      NA       NA    
## NewsDesk.nb.fctrForeign                                   NA       NA    
## NewsDesk.nb.fctrStyles                              70553408   <2e-16 ***
## NewsDesk.nb.fctrTStyle                                    NA       NA    
## NewsDesk.nb.fctrMagazine                                  NA       NA    
## NewsDesk.nb.fctrmyMultimedia                              NA       NA    
## `NewsDesk.nb.fctrmyMisc::`                          -7098350   <2e-16 ***
## NewsDesk.nb.fctrTravel                                    NA       NA    
## NewsDesk.nb.fctrMetro                                     NA       NA    
## `NewsDesk.nb.fctrDaily Clip Report::`                     NA       NA    
## `NewsDesk.nb.fctr6 Q's About the News::`                  NA       NA    
## `NewsDesk.nb.fctrTest Yourself::`                         NA       NA    
## `NewsDesk.nb.fctrWord of the Day::`                       NA       NA    
## `NewsDesk.nb.fctr19[0-9][0-9]::`                          NA       NA    
## `NewsDesk.nb.fctrReaders Respond::`                       NA       NA    
## NewsDesk.nb.fctrNational                            -6833632   <2e-16 ***
## NewsDesk.nb.fctrSports                                    NA       NA    
## `NewsDesk.nb.fctrQuiz(.*)([?=|]|[?=:]::`                  NA       NA    
## `NewsDesk.nb.fctrmyTech::`                          -2329576   <2e-16 ***
## `NewsDesk.nb.fctrVerbatim::`                              NA       NA    
## `NewsDesk.nb.fctrFirst Draft::`                           NA       NA    
## `NewsDesk.nb.fctrToday in Politics::`                     NA       NA    
## `NewsDesk.nb.fctrmyFood::`                                NA       NA    
## `NewsDesk.nb.fctrThe Daily Gift::`                        NA       NA    
## H.num.chars.log                                     -1096056   <2e-16 ***
## H.num.words.log                                     16495583   <2e-16 ***
## A.num.chars.log                                    -35473899   <2e-16 ***
## A.num.words.log                                     10538360   <2e-16 ***
## A.num.words.unq.log                                  2560425   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance:  4042.7  on 4474  degrees of freedom
## Residual deviance: 38855.1  on 4372  degrees of freedom
## AIC: 39061
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

![](NYTBlogs_Metro_files/figure-html/fit.models_0-32.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-33.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6365475
## 3        0.2 0.6365475
## 4        0.3 0.6365475
## 5        0.4 0.6365475
## 6        0.5 0.6365475
## 7        0.6 0.6365475
## 8        0.7 0.6365475
## 9        0.8 0.6365475
## 10       0.9 0.6365475
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3464
## 2            Y                                  277
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  262
## 2                                  472
##          Prediction
## Reference    N    Y
##         N 3464  262
##         Y  277  472
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.795531e-01   5.643721e-01   8.696566e-01   8.889511e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.120844e-18   5.464936e-01 
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

![](NYTBlogs_Metro_files/figure-html/fit.models_0-34.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_0-35.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6378066
## 3        0.2 0.6378066
## 4        0.3 0.6378066
## 5        0.4 0.6378066
## 6        0.5 0.6378066
## 7        0.6 0.6378066
## 8        0.7 0.6378066
## 9        0.8 0.6378066
## 10       0.9 0.6378066
## 11       1.0 0.0000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_0-36.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1585
## 2            Y                                  123
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  128
## 2                                  221
##          Prediction
## Reference    N    Y
##         N 1585  128
##         Y  123  221
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.779776e-01   5.644409e-01   8.630492e-01   8.918192e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   6.646565e-09   8.006718e-01 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1 WordCount.log, H.is.question, PubDate.apm.fctr, S.can, H.has.ebola, S.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, S.take, PubDate.minute, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     14.457                 4.372
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7799284                    0.9       0.6365475        0.8616763
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8696566             0.8889511     0.4363828   0.7838596
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.6378066        0.8779776
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8630492             0.8918192     0.5644409    39061.06
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.03119338       0.1765255
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 9  fit.models          6          0 159.885 208.673  48.789
## 10 fit.models          6          1 208.674      NA      NA
```


```r
# All X that is not user excluded
if (glb_is_classification && glb_is_binomial) {
    model_id_pfx <- "Conditional.X"
# indep_vars_vctr <- setdiff(names(glb_fitent_df), union(glb_rsp_var, glb_exclude_vars_as_features))
    indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
                                            (exclude.as.feat != 1))[, "id"]
} else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, 
                                            (exclude.as.feat != 1))[, "id"]
}
for (method in glb_models_method_vctr) {
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
## [1] "fitting model: Conditional.X.glm"
## [1] "    indep_vars: WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
## + Fold1: parameter=none
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold1: parameter=none 
## + Fold2: parameter=none
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold2: parameter=none 
## + Fold3: parameter=none
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold3: parameter=none 
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
##   297, 1693, 3058, 3256, 3596, 3794, 4229, 4338, 4394
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-1.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_1-2.png) 

```
## Warning: not plotting observations with leverage one:
##   297, 1693, 3058, 3256, 3596, 3794, 4229, 4338, 4394
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -8.49    0.00    0.00    0.00    8.49  
## 
## Coefficients: (77 not defined because of singularities)
##                                                                        Estimate
## (Intercept)                                                          -4.275e+14
## WordCount.log                                                         2.932e+14
## PubDate.hour                                                         -4.992e+12
## H.is.question                                                         1.093e+14
## PubDate.apm.fctrpm                                                   -1.124e+14
## A.can                                                                 2.433e+15
## S.can                                                                -2.603e+15
## H.has.ebola                                                          -2.756e+14
## S.make                                                                2.991e+13
## A.make                                                                       NA
## S.one                                                                -3.172e+15
## S.state                                                              -1.953e+13
## A.state                                                                      NA
## A.one                                                                 3.196e+15
## S.said                                                                1.863e+14
## A.said                                                                       NA
## .rnorm                                                                1.682e+13
## `PubDate.date.fctr(7,13]`                                            -5.496e+13
## `PubDate.date.fctr(13,19]`                                           -5.636e+13
## `PubDate.date.fctr(19,25]`                                           -6.020e+13
## `PubDate.date.fctr(25,31]`                                            1.328e+13
## PubDate.second                                                       -5.851e+11
## S.presid                                                              1.241e+14
## A.presid                                                                     NA
## S.take                                                               -4.409e+15
## A.take                                                                4.541e+15
## PubDate.minute                                                        1.625e+11
## S.new                                                                -9.656e+14
## A.new                                                                 9.293e+14
## PubDate.wkday.fctr1                                                  -1.201e+13
## PubDate.wkday.fctr2                                                   2.015e+13
## PubDate.wkday.fctr3                                                   4.351e+13
## PubDate.wkday.fctr4                                                   1.025e+13
## PubDate.wkday.fctr5                                                  -7.189e+12
## PubDate.wkday.fctr6                                                  -1.169e+14
## S.day                                                                -3.009e+14
## A.day                                                                 2.732e+14
## H.X2014                                                               9.443e+12
## S.show                                                               -1.047e+14
## A.show                                                                       NA
## S.report                                                              2.030e+14
## A.report                                                                     NA
## S.share                                                              -3.592e+14
## A.share                                                                      NA
## S.year                                                               -9.394e+13
## A.year                                                                       NA
## S.compani                                                             2.710e+14
## A.compani                                                                    NA
## H.new                                                                -4.246e+14
## S.first                                                              -4.472e+12
## A.first                                                                      NA
## S.time                                                                5.452e+15
## A.time                                                               -5.433e+15
## H.newyork                                                            -1.264e+14
## S.articl                                                             -2.778e+14
## A.articl                                                                     NA
## S.will                                                                1.304e+15
## A.will                                                               -1.278e+15
## H.day                                                                -2.102e+12
## S.newyork                                                            -4.125e+13
## A.newyork                                                                    NA
## H.today                                                              -1.462e+15
## H.report                                                             -2.260e+13
## S.intern                                                              3.762e+12
## A.intern                                                                     NA
## H.week                                                                1.542e+13
## H.fashion                                                             8.378e+14
## S.week                                                               -3.924e+14
## A.week                                                                       NA
## S.fashion                                                            -2.784e+14
## A.fashion                                                                    NA
## `Headline.pfx.fctr19[0-9][0-9]::`                                    -2.432e+15
## `Headline.pfx.fctrDaily Report::`                                    -2.598e+15
## `Headline.pfx.fctr.*Fashion Week::`                                  -2.538e+15
## `Headline.pfx.fctrWhat We're::`                                      -5.616e+15
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                    -2.241e+15
## `Headline.pfx.fctrToday in Small Business::`                         -1.688e+15
## `Headline.pfx.fctrDaily Clip Report::`                               -5.213e+15
## `Headline.pfx.fctrMorning Agenda::`                                  -3.285e+15
## `Headline.pfx.fctrNew York Today::`                                   2.505e+14
## `Headline.pfx.fctr6 Q's About the News::`                            -1.691e+15
## `Headline.pfx.fctrTest Yourself::`                                   -8.463e+14
## `Headline.pfx.fctrWord of the Day::`                                 -1.220e+15
## `Headline.pfx.fctrmyTech::`                                           2.995e+14
## `Headline.pfx.fctrYour Turn::`                                        1.458e+15
## `Headline.pfx.fctrReaders Respond::`                                 -1.028e+15
## `Headline.pfx.fctrAsk Well::`                                        -8.446e+14
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                            -9.633e+14
## `Headline.pfx.fctrVerbatim::`                                        -4.935e+15
## `Headline.pfx.fctrFirst Draft::`                                     -5.030e+15
## `Headline.pfx.fctrToday in Politics::`                               -4.292e+15
## `Headline.pfx.fctrmyFood::`                                          -1.594e+15
## `Headline.pfx.fctrThe Daily Gift::`                                   7.688e+14
## SectionName.nb.fctrArts                                              -1.844e+15
## `SectionName.nb.fctrBusiness Day`                                    -2.368e+15
## SectionName.nb.fctrHealth                                             6.573e+14
## SectionName.nb.fctrOpinion                                           -5.263e+14
## SectionName.nb.fctrForeign                                           -2.789e+15
## SectionName.nb.fctrStyles                                            -7.035e+15
## SectionName.nb.fctrTStyle                                            -2.639e+15
## SectionName.nb.fctrWorld                                             -4.386e+15
## SectionName.nb.fctrTechnology                                        -2.774e+15
## SectionName.nb.fctrMagazine                                          -5.283e+15
## SectionName.nb.fctrMultimedia                                        -2.261e+15
## `SectionName.nb.fctrmyMisc::`                                        -4.180e+13
## SectionName.nb.fctrTravel                                            -4.573e+15
## SectionName.nb.fctrU.S.                                              -3.658e+15
## `SectionName.nb.fctrN.Y. / Region`                                   -1.424e+15
## `SectionName.nb.fctrDaily Clip Report::`                                     NA
## SectionName.nb.fctrOpen                                              -3.716e+15
## `SectionName.nb.fctr19[0-9][0-9]::`                                  -2.911e+15
## `SectionName.nb.fctrReaders Respond::`                                4.356e+14
## SectionName.nb.fctrSports                                            -1.135e+15
## SectionName.nb.fctrNational                                          -1.129e+15
## `SectionName.nb.fctrVerbatim::`                                              NA
## `SectionName.nb.fctrFirst Draft::`                                           NA
## `SectionName.nb.fctrToday in Politics::`                                     NA
## `SectionName.nb.fctrmyTech::`                                         4.425e+14
## `SectionName.nb.fctrmyFood::`                                        -1.911e+15
## SectionName.nb.fctrCulture                                                   NA
## `SectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::`                           4.910e+15
## SectionName.nb.fctrBusiness                                          -3.728e+15
## `SectionName.nb.fctrThe Daily Gift::`                                        NA
## NewsDesk.nb.fctrCulture                                                      NA
## NewsDesk.nb.fctrScience                                                      NA
## NewsDesk.nb.fctrOpEd                                                         NA
## NewsDesk.nb.fctrForeign                                                      NA
## NewsDesk.nb.fctrStyles                                                3.894e+15
## NewsDesk.nb.fctrTStyle                                                       NA
## NewsDesk.nb.fctrMagazine                                                     NA
## NewsDesk.nb.fctrmyMultimedia                                                 NA
## `NewsDesk.nb.fctrmyMisc::`                                           -1.962e+15
## NewsDesk.nb.fctrTravel                                                       NA
## NewsDesk.nb.fctrMetro                                                        NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                        NA
## `NewsDesk.nb.fctr6 Q's About the News::`                                     NA
## `NewsDesk.nb.fctrTest Yourself::`                                            NA
## `NewsDesk.nb.fctrWord of the Day::`                                          NA
## `NewsDesk.nb.fctr19[0-9][0-9]::`                                             NA
## `NewsDesk.nb.fctrReaders Respond::`                                          NA
## NewsDesk.nb.fctrNational                                             -1.810e+15
## NewsDesk.nb.fctrSports                                                       NA
## `NewsDesk.nb.fctrQuiz(.*)([?=|]|[?=:]::`                                     NA
## `NewsDesk.nb.fctrmyTech::`                                           -2.048e+15
## `NewsDesk.nb.fctrVerbatim::`                                                 NA
## `NewsDesk.nb.fctrFirst Draft::`                                              NA
## `NewsDesk.nb.fctrToday in Politics::`                                        NA
## `NewsDesk.nb.fctrmyFood::`                                                   NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                           NA
## H.num.chars.log                                                       7.990e+13
## H.num.words.log                                                       4.970e+14
## H.num.words.unq.log                                                  -5.696e+14
## `SubsectionName.nb.fctrCulture::Arts`                                        NA
## SubsectionName.nb.fctrDealbook                                        2.785e+13
## `SubsectionName.nb.fctrScience::Health`                                      NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                 6.000e+14
## `SubsectionName.nb.fctrRoom For Debate`                              -2.470e+15
## `SubsectionName.nb.fctrForeign::Foreign`                                     NA
## `SubsectionName.nb.fctrFashion & Style`                                      NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                       NA
## `SubsectionName.nb.fctrAsia Pacific`                                  1.588e+15
## `SubsectionName.nb.fctrBusiness::Technology`                                 NA
## `SubsectionName.nb.fctrMagazine::Magazine`                                   NA
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                             NA
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                     NA
## `SubsectionName.nb.fctrTravel::Travel`                                       NA
## SubsectionName.nb.fctrEducation                                              NA
## `SubsectionName.nb.fctrThe Public Editor`                                    NA
## `SubsectionName.nb.fctrSmall Business`                                       NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                                 NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`               NA
## `SubsectionName.nb.fctrmyMisc::Open`                                         NA
## `SubsectionName.nb.fctrForeign::World`                                       NA
## `SubsectionName.nb.fctr19[0-9][0-9]::19[0-9][0-9]::`                         NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                   NA
## SubsectionName.nb.fctrPolitics                                               NA
## `SubsectionName.nb.fctrSports::Sports`                                       NA
## `SubsectionName.nb.fctrNational::National`                                   NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                                 NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                           NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`               NA
## `SubsectionName.nb.fctrmyTech::myTech::`                                     NA
## `SubsectionName.nb.fctrmyFood::myFood::`                                     NA
## `SubsectionName.nb.fctrCulture::Culture`                                     NA
## `SubsectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::Quiz(.*)([?=|]|[?=:]::`         NA
## `SubsectionName.nb.fctrBusiness::Business`                                   NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                         NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                     NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                       NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                          NA
## A.num.chars.log                                                      -7.007e+15
## S.num.chars.log                                                       6.908e+15
## A.num.words.log                                                      -4.260e+16
## S.num.words.log                                                       4.236e+16
## A.num.words.unq.log                                                   4.474e+16
## S.num.words.unq.log                                                  -4.445e+16
##                                                                      Std. Error
## (Intercept)                                                           2.413e+07
## WordCount.log                                                         1.270e+06
## PubDate.hour                                                          3.913e+05
## H.is.question                                                         4.705e+06
## PubDate.apm.fctrpm                                                    3.669e+06
## A.can                                                                 7.817e+07
## S.can                                                                 7.873e+07
## H.has.ebola                                                           9.138e+06
## S.make                                                                5.284e+06
## A.make                                                                       NA
## S.one                                                                 1.129e+08
## S.state                                                               5.581e+06
## A.state                                                                      NA
## A.one                                                                 1.130e+08
## S.said                                                                5.265e+06
## A.said                                                                       NA
## .rnorm                                                                1.009e+06
## `PubDate.date.fctr(7,13]`                                             3.148e+06
## `PubDate.date.fctr(13,19]`                                            3.119e+06
## `PubDate.date.fctr(19,25]`                                            3.035e+06
## `PubDate.date.fctr(25,31]`                                            3.382e+06
## PubDate.second                                                        5.847e+04
## S.presid                                                              5.118e+06
## A.presid                                                                     NA
## S.take                                                                9.747e+07
## A.take                                                                9.733e+07
## PubDate.minute                                                        5.850e+04
## S.new                                                                 5.272e+07
## A.new                                                                 5.260e+07
## PubDate.wkday.fctr1                                                   5.345e+06
## PubDate.wkday.fctr2                                                   5.358e+06
## PubDate.wkday.fctr3                                                   5.342e+06
## PubDate.wkday.fctr4                                                   5.337e+06
## PubDate.wkday.fctr5                                                   5.371e+06
## PubDate.wkday.fctr6                                                   7.926e+06
## S.day                                                                 7.462e+07
## A.day                                                                 7.439e+07
## H.X2014                                                               1.012e+07
## S.show                                                                5.409e+06
## A.show                                                                       NA
## S.report                                                              6.025e+06
## A.report                                                                     NA
## S.share                                                               6.356e+06
## A.share                                                                      NA
## S.year                                                                4.734e+06
## A.year                                                                       NA
## S.compani                                                             4.488e+06
## A.compani                                                                    NA
## H.new                                                                 5.586e+06
## S.first                                                               5.785e+06
## A.first                                                                      NA
## S.time                                                                7.017e+07
## A.time                                                                6.999e+07
## H.newyork                                                             7.684e+06
## S.articl                                                              1.002e+07
## A.articl                                                                     NA
## S.will                                                                7.907e+07
## A.will                                                                7.905e+07
## H.day                                                                 8.637e+06
## S.newyork                                                             5.027e+06
## A.newyork                                                                    NA
## H.today                                                               2.764e+07
## H.report                                                              9.797e+06
## S.intern                                                              1.079e+07
## A.intern                                                                     NA
## H.week                                                                1.179e+07
## H.fashion                                                             1.437e+07
## S.week                                                                5.026e+06
## A.week                                                                       NA
## S.fashion                                                             7.068e+06
## A.fashion                                                                    NA
## `Headline.pfx.fctr19[0-9][0-9]::`                                     1.927e+07
## `Headline.pfx.fctrDaily Report::`                                     1.576e+07
## `Headline.pfx.fctr.*Fashion Week::`                                   2.012e+07
## `Headline.pfx.fctrWhat We're::`                                       1.477e+07
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                     1.739e+07
## `Headline.pfx.fctrToday in Small Business::`                          3.105e+07
## `Headline.pfx.fctrDaily Clip Report::`                                2.024e+07
## `Headline.pfx.fctrMorning Agenda::`                                   1.147e+07
## `Headline.pfx.fctrNew York Today::`                                   3.105e+07
## `Headline.pfx.fctr6 Q's About the News::`                             5.149e+07
## `Headline.pfx.fctrTest Yourself::`                                    5.158e+07
## `Headline.pfx.fctrWord of the Day::`                                  5.267e+07
## `Headline.pfx.fctrmyTech::`                                           8.965e+06
## `Headline.pfx.fctrYour Turn::`                                        2.483e+07
## `Headline.pfx.fctrReaders Respond::`                                  3.924e+07
## `Headline.pfx.fctrAsk Well::`                                         2.890e+07
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                             5.782e+07
## `Headline.pfx.fctrVerbatim::`                                         1.691e+07
## `Headline.pfx.fctrFirst Draft::`                                      1.377e+07
## `Headline.pfx.fctrToday in Politics::`                                3.137e+07
## `Headline.pfx.fctrmyFood::`                                           1.562e+07
## `Headline.pfx.fctrThe Daily Gift::`                                   1.830e+07
## SectionName.nb.fctrArts                                               8.902e+06
## `SectionName.nb.fctrBusiness Day`                                     1.246e+07
## SectionName.nb.fctrHealth                                             1.022e+07
## SectionName.nb.fctrOpinion                                            2.001e+07
## SectionName.nb.fctrForeign                                            2.227e+07
## SectionName.nb.fctrStyles                                             5.226e+07
## SectionName.nb.fctrTStyle                                             8.969e+06
## SectionName.nb.fctrWorld                                              3.978e+07
## SectionName.nb.fctrTechnology                                         1.030e+07
## SectionName.nb.fctrMagazine                                           1.717e+07
## SectionName.nb.fctrMultimedia                                         1.204e+07
## `SectionName.nb.fctrmyMisc::`                                         5.146e+07
## SectionName.nb.fctrTravel                                             1.093e+07
## SectionName.nb.fctrU.S.                                               5.094e+07
## `SectionName.nb.fctrN.Y. / Region`                                    1.081e+07
## `SectionName.nb.fctrDaily Clip Report::`                                     NA
## SectionName.nb.fctrOpen                                               7.030e+07
## `SectionName.nb.fctr19[0-9][0-9]::`                                   3.382e+07
## `SectionName.nb.fctrReaders Respond::`                                4.583e+07
## SectionName.nb.fctrSports                                             6.978e+07
## SectionName.nb.fctrNational                                           8.501e+07
## `SectionName.nb.fctrVerbatim::`                                              NA
## `SectionName.nb.fctrFirst Draft::`                                           NA
## `SectionName.nb.fctrToday in Politics::`                                     NA
## `SectionName.nb.fctrmyTech::`                                         9.723e+07
## `SectionName.nb.fctrmyFood::`                                         5.091e+07
## SectionName.nb.fctrCulture                                                   NA
## `SectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::`                           8.905e+07
## SectionName.nb.fctrBusiness                                           3.594e+07
## `SectionName.nb.fctrThe Daily Gift::`                                        NA
## NewsDesk.nb.fctrCulture                                                      NA
## NewsDesk.nb.fctrScience                                                      NA
## NewsDesk.nb.fctrOpEd                                                         NA
## NewsDesk.nb.fctrForeign                                                      NA
## NewsDesk.nb.fctrStyles                                                5.060e+07
## NewsDesk.nb.fctrTStyle                                                       NA
## NewsDesk.nb.fctrMagazine                                                     NA
## NewsDesk.nb.fctrmyMultimedia                                                 NA
## `NewsDesk.nb.fctrmyMisc::`                                            5.081e+07
## NewsDesk.nb.fctrTravel                                                       NA
## NewsDesk.nb.fctrMetro                                                        NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                        NA
## `NewsDesk.nb.fctr6 Q's About the News::`                                     NA
## `NewsDesk.nb.fctrTest Yourself::`                                            NA
## `NewsDesk.nb.fctrWord of the Day::`                                          NA
## `NewsDesk.nb.fctr19[0-9][0-9]::`                                             NA
## `NewsDesk.nb.fctrReaders Respond::`                                          NA
## NewsDesk.nb.fctrNational                                              7.011e+07
## NewsDesk.nb.fctrSports                                                       NA
## `NewsDesk.nb.fctrQuiz(.*)([?=|]|[?=:]::`                                     NA
## `NewsDesk.nb.fctrmyTech::`                                            6.976e+07
## `NewsDesk.nb.fctrVerbatim::`                                                 NA
## `NewsDesk.nb.fctrFirst Draft::`                                              NA
## `NewsDesk.nb.fctrToday in Politics::`                                        NA
## `NewsDesk.nb.fctrmyFood::`                                                   NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                           NA
## H.num.chars.log                                                       6.548e+06
## H.num.words.log                                                       3.801e+07
## H.num.words.unq.log                                                   3.752e+07
## `SubsectionName.nb.fctrCulture::Arts`                                        NA
## SubsectionName.nb.fctrDealbook                                        9.744e+06
## `SubsectionName.nb.fctrScience::Health`                                      NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                 1.860e+07
## `SubsectionName.nb.fctrRoom For Debate`                               2.103e+07
## `SubsectionName.nb.fctrForeign::Foreign`                                     NA
## `SubsectionName.nb.fctrFashion & Style`                                      NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                       NA
## `SubsectionName.nb.fctrAsia Pacific`                                  3.928e+07
## `SubsectionName.nb.fctrBusiness::Technology`                                 NA
## `SubsectionName.nb.fctrMagazine::Magazine`                                   NA
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                             NA
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                     NA
## `SubsectionName.nb.fctrTravel::Travel`                                       NA
## SubsectionName.nb.fctrEducation                                              NA
## `SubsectionName.nb.fctrThe Public Editor`                                    NA
## `SubsectionName.nb.fctrSmall Business`                                       NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                                 NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`               NA
## `SubsectionName.nb.fctrmyMisc::Open`                                         NA
## `SubsectionName.nb.fctrForeign::World`                                       NA
## `SubsectionName.nb.fctr19[0-9][0-9]::19[0-9][0-9]::`                         NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                   NA
## SubsectionName.nb.fctrPolitics                                               NA
## `SubsectionName.nb.fctrSports::Sports`                                       NA
## `SubsectionName.nb.fctrNational::National`                                   NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                                 NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                           NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`               NA
## `SubsectionName.nb.fctrmyTech::myTech::`                                     NA
## `SubsectionName.nb.fctrmyFood::myFood::`                                     NA
## `SubsectionName.nb.fctrCulture::Culture`                                     NA
## `SubsectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::Quiz(.*)([?=|]|[?=:]::`         NA
## `SubsectionName.nb.fctrBusiness::Business`                                   NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                         NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                     NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                       NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                          NA
## A.num.chars.log                                                       1.136e+08
## S.num.chars.log                                                       1.135e+08
## A.num.words.log                                                       5.195e+08
## S.num.words.log                                                       5.195e+08
## A.num.words.unq.log                                                   5.233e+08
## S.num.words.unq.log                                                   5.228e+08
##                                                                         z value
## (Intercept)                                                           -17712534
## WordCount.log                                                         230783960
## PubDate.hour                                                          -12756532
## H.is.question                                                          23221958
## PubDate.apm.fctrpm                                                    -30627122
## A.can                                                                  31121307
## S.can                                                                 -33058261
## H.has.ebola                                                           -30153129
## S.make                                                                  5660075
## A.make                                                                       NA
## S.one                                                                 -28104318
## S.state                                                                -3499328
## A.state                                                                      NA
## A.one                                                                  28286655
## S.said                                                                 35377776
## A.said                                                                       NA
## .rnorm                                                                 16668352
## `PubDate.date.fctr(7,13]`                                             -17459646
## `PubDate.date.fctr(13,19]`                                            -18069049
## `PubDate.date.fctr(19,25]`                                            -19832200
## `PubDate.date.fctr(25,31]`                                              3925055
## PubDate.second                                                        -10006455
## S.presid                                                               24245362
## A.presid                                                                     NA
## S.take                                                                -45233621
## A.take                                                                 46653321
## PubDate.minute                                                          2777672
## S.new                                                                 -18315318
## A.new                                                                  17668762
## PubDate.wkday.fctr1                                                    -2247858
## PubDate.wkday.fctr2                                                     3761321
## PubDate.wkday.fctr3                                                     8145048
## PubDate.wkday.fctr4                                                     1919692
## PubDate.wkday.fctr5                                                    -1338390
## PubDate.wkday.fctr6                                                   -14754645
## S.day                                                                  -4032648
## A.day                                                                   3672581
## H.X2014                                                                  933262
## S.show                                                                -19349679
## A.show                                                                       NA
## S.report                                                               33685848
## A.report                                                                     NA
## S.share                                                               -56515728
## A.share                                                                      NA
## S.year                                                                -19843192
## A.year                                                                       NA
## S.compani                                                              60375529
## A.compani                                                                    NA
## H.new                                                                 -76011325
## S.first                                                                 -772969
## A.first                                                                      NA
## S.time                                                                 77704433
## A.time                                                                -77623327
## H.newyork                                                             -16451112
## S.articl                                                              -27730605
## A.articl                                                                     NA
## S.will                                                                 16489609
## A.will                                                                -16162422
## H.day                                                                   -243368
## S.newyork                                                              -8206379
## A.newyork                                                                    NA
## H.today                                                               -52898947
## H.report                                                               -2306632
## S.intern                                                                 348706
## A.intern                                                                     NA
## H.week                                                                  1308215
## H.fashion                                                              58300328
## S.week                                                                -78088920
## A.week                                                                       NA
## S.fashion                                                             -39386061
## A.fashion                                                                    NA
## `Headline.pfx.fctr19[0-9][0-9]::`                                    -126205596
## `Headline.pfx.fctrDaily Report::`                                    -164826074
## `Headline.pfx.fctr.*Fashion Week::`                                  -126143828
## `Headline.pfx.fctrWhat We're::`                                      -380341963
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                    -128864375
## `Headline.pfx.fctrToday in Small Business::`                          -54368867
## `Headline.pfx.fctrDaily Clip Report::`                               -257626543
## `Headline.pfx.fctrMorning Agenda::`                                  -286457073
## `Headline.pfx.fctrNew York Today::`                                     8065739
## `Headline.pfx.fctr6 Q's About the News::`                             -32851127
## `Headline.pfx.fctrTest Yourself::`                                    -16408633
## `Headline.pfx.fctrWord of the Day::`                                  -23156795
## `Headline.pfx.fctrmyTech::`                                            33405997
## `Headline.pfx.fctrYour Turn::`                                         58706347
## `Headline.pfx.fctrReaders Respond::`                                  -26207738
## `Headline.pfx.fctrAsk Well::`                                         -29229304
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                             -16660309
## `Headline.pfx.fctrVerbatim::`                                        -291724887
## `Headline.pfx.fctrFirst Draft::`                                     -365389940
## `Headline.pfx.fctrToday in Politics::`                               -136827515
## `Headline.pfx.fctrmyFood::`                                          -102021095
## `Headline.pfx.fctrThe Daily Gift::`                                    42012702
## SectionName.nb.fctrArts                                              -207168996
## `SectionName.nb.fctrBusiness Day`                                    -190037087
## SectionName.nb.fctrHealth                                              64294342
## SectionName.nb.fctrOpinion                                            -26298638
## SectionName.nb.fctrForeign                                           -125253857
## SectionName.nb.fctrStyles                                            -134616034
## SectionName.nb.fctrTStyle                                            -294188478
## SectionName.nb.fctrWorld                                             -110255552
## SectionName.nb.fctrTechnology                                        -269246528
## SectionName.nb.fctrMagazine                                          -307774927
## SectionName.nb.fctrMultimedia                                        -187802408
## `SectionName.nb.fctrmyMisc::`                                           -812322
## SectionName.nb.fctrTravel                                            -418376165
## SectionName.nb.fctrU.S.                                               -71810345
## `SectionName.nb.fctrN.Y. / Region`                                   -131726918
## `SectionName.nb.fctrDaily Clip Report::`                                     NA
## SectionName.nb.fctrOpen                                               -52856512
## `SectionName.nb.fctr19[0-9][0-9]::`                                   -86054268
## `SectionName.nb.fctrReaders Respond::`                                  9505311
## SectionName.nb.fctrSports                                             -16268903
## SectionName.nb.fctrNational                                           -13280788
## `SectionName.nb.fctrVerbatim::`                                              NA
## `SectionName.nb.fctrFirst Draft::`                                           NA
## `SectionName.nb.fctrToday in Politics::`                                     NA
## `SectionName.nb.fctrmyTech::`                                           4551210
## `SectionName.nb.fctrmyFood::`                                         -37532036
## SectionName.nb.fctrCulture                                                   NA
## `SectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::`                            55140092
## SectionName.nb.fctrBusiness                                          -103740788
## `SectionName.nb.fctrThe Daily Gift::`                                        NA
## NewsDesk.nb.fctrCulture                                                      NA
## NewsDesk.nb.fctrScience                                                      NA
## NewsDesk.nb.fctrOpEd                                                         NA
## NewsDesk.nb.fctrForeign                                                      NA
## NewsDesk.nb.fctrStyles                                                 76959306
## NewsDesk.nb.fctrTStyle                                                       NA
## NewsDesk.nb.fctrMagazine                                                     NA
## NewsDesk.nb.fctrmyMultimedia                                                 NA
## `NewsDesk.nb.fctrmyMisc::`                                            -38618418
## NewsDesk.nb.fctrTravel                                                       NA
## NewsDesk.nb.fctrMetro                                                        NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                        NA
## `NewsDesk.nb.fctr6 Q's About the News::`                                     NA
## `NewsDesk.nb.fctrTest Yourself::`                                            NA
## `NewsDesk.nb.fctrWord of the Day::`                                          NA
## `NewsDesk.nb.fctr19[0-9][0-9]::`                                             NA
## `NewsDesk.nb.fctrReaders Respond::`                                          NA
## NewsDesk.nb.fctrNational                                              -25819643
## NewsDesk.nb.fctrSports                                                       NA
## `NewsDesk.nb.fctrQuiz(.*)([?=|]|[?=:]::`                                     NA
## `NewsDesk.nb.fctrmyTech::`                                            -29364662
## `NewsDesk.nb.fctrVerbatim::`                                                 NA
## `NewsDesk.nb.fctrFirst Draft::`                                              NA
## `NewsDesk.nb.fctrToday in Politics::`                                        NA
## `NewsDesk.nb.fctrmyFood::`                                                   NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                           NA
## H.num.chars.log                                                        12202679
## H.num.words.log                                                        13072564
## H.num.words.unq.log                                                   -15181840
## `SubsectionName.nb.fctrCulture::Arts`                                        NA
## SubsectionName.nb.fctrDealbook                                          2858615
## `SubsectionName.nb.fctrScience::Health`                                      NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                  32258246
## `SubsectionName.nb.fctrRoom For Debate`                              -117439524
## `SubsectionName.nb.fctrForeign::Foreign`                                     NA
## `SubsectionName.nb.fctrFashion & Style`                                      NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                       NA
## `SubsectionName.nb.fctrAsia Pacific`                                   40430730
## `SubsectionName.nb.fctrBusiness::Technology`                                 NA
## `SubsectionName.nb.fctrMagazine::Magazine`                                   NA
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                             NA
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                     NA
## `SubsectionName.nb.fctrTravel::Travel`                                       NA
## SubsectionName.nb.fctrEducation                                              NA
## `SubsectionName.nb.fctrThe Public Editor`                                    NA
## `SubsectionName.nb.fctrSmall Business`                                       NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                                 NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`               NA
## `SubsectionName.nb.fctrmyMisc::Open`                                         NA
## `SubsectionName.nb.fctrForeign::World`                                       NA
## `SubsectionName.nb.fctr19[0-9][0-9]::19[0-9][0-9]::`                         NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                   NA
## SubsectionName.nb.fctrPolitics                                               NA
## `SubsectionName.nb.fctrSports::Sports`                                       NA
## `SubsectionName.nb.fctrNational::National`                                   NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                                 NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                           NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`               NA
## `SubsectionName.nb.fctrmyTech::myTech::`                                     NA
## `SubsectionName.nb.fctrmyFood::myFood::`                                     NA
## `SubsectionName.nb.fctrCulture::Culture`                                     NA
## `SubsectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::Quiz(.*)([?=|]|[?=:]::`         NA
## `SubsectionName.nb.fctrBusiness::Business`                                   NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                         NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                     NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                       NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                          NA
## A.num.chars.log                                                       -61708601
## S.num.chars.log                                                        60843186
## A.num.words.log                                                       -82004244
## S.num.words.log                                                        81544399
## A.num.words.unq.log                                                    85501936
## S.num.words.unq.log                                                   -85025887
##                                                                      Pr(>|z|)
## (Intercept)                                                            <2e-16
## WordCount.log                                                          <2e-16
## PubDate.hour                                                           <2e-16
## H.is.question                                                          <2e-16
## PubDate.apm.fctrpm                                                     <2e-16
## A.can                                                                  <2e-16
## S.can                                                                  <2e-16
## H.has.ebola                                                            <2e-16
## S.make                                                                 <2e-16
## A.make                                                                     NA
## S.one                                                                  <2e-16
## S.state                                                                <2e-16
## A.state                                                                    NA
## A.one                                                                  <2e-16
## S.said                                                                 <2e-16
## A.said                                                                     NA
## .rnorm                                                                 <2e-16
## `PubDate.date.fctr(7,13]`                                              <2e-16
## `PubDate.date.fctr(13,19]`                                             <2e-16
## `PubDate.date.fctr(19,25]`                                             <2e-16
## `PubDate.date.fctr(25,31]`                                             <2e-16
## PubDate.second                                                         <2e-16
## S.presid                                                               <2e-16
## A.presid                                                                   NA
## S.take                                                                 <2e-16
## A.take                                                                 <2e-16
## PubDate.minute                                                         <2e-16
## S.new                                                                  <2e-16
## A.new                                                                  <2e-16
## PubDate.wkday.fctr1                                                    <2e-16
## PubDate.wkday.fctr2                                                    <2e-16
## PubDate.wkday.fctr3                                                    <2e-16
## PubDate.wkday.fctr4                                                    <2e-16
## PubDate.wkday.fctr5                                                    <2e-16
## PubDate.wkday.fctr6                                                    <2e-16
## S.day                                                                  <2e-16
## A.day                                                                  <2e-16
## H.X2014                                                                <2e-16
## S.show                                                                 <2e-16
## A.show                                                                     NA
## S.report                                                               <2e-16
## A.report                                                                   NA
## S.share                                                                <2e-16
## A.share                                                                    NA
## S.year                                                                 <2e-16
## A.year                                                                     NA
## S.compani                                                              <2e-16
## A.compani                                                                  NA
## H.new                                                                  <2e-16
## S.first                                                                <2e-16
## A.first                                                                    NA
## S.time                                                                 <2e-16
## A.time                                                                 <2e-16
## H.newyork                                                              <2e-16
## S.articl                                                               <2e-16
## A.articl                                                                   NA
## S.will                                                                 <2e-16
## A.will                                                                 <2e-16
## H.day                                                                  <2e-16
## S.newyork                                                              <2e-16
## A.newyork                                                                  NA
## H.today                                                                <2e-16
## H.report                                                               <2e-16
## S.intern                                                               <2e-16
## A.intern                                                                   NA
## H.week                                                                 <2e-16
## H.fashion                                                              <2e-16
## S.week                                                                 <2e-16
## A.week                                                                     NA
## S.fashion                                                              <2e-16
## A.fashion                                                                  NA
## `Headline.pfx.fctr19[0-9][0-9]::`                                      <2e-16
## `Headline.pfx.fctrDaily Report::`                                      <2e-16
## `Headline.pfx.fctr.*Fashion Week::`                                    <2e-16
## `Headline.pfx.fctrWhat We're::`                                        <2e-16
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                      <2e-16
## `Headline.pfx.fctrToday in Small Business::`                           <2e-16
## `Headline.pfx.fctrDaily Clip Report::`                                 <2e-16
## `Headline.pfx.fctrMorning Agenda::`                                    <2e-16
## `Headline.pfx.fctrNew York Today::`                                    <2e-16
## `Headline.pfx.fctr6 Q's About the News::`                              <2e-16
## `Headline.pfx.fctrTest Yourself::`                                     <2e-16
## `Headline.pfx.fctrWord of the Day::`                                   <2e-16
## `Headline.pfx.fctrmyTech::`                                            <2e-16
## `Headline.pfx.fctrYour Turn::`                                         <2e-16
## `Headline.pfx.fctrReaders Respond::`                                   <2e-16
## `Headline.pfx.fctrAsk Well::`                                          <2e-16
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                              <2e-16
## `Headline.pfx.fctrVerbatim::`                                          <2e-16
## `Headline.pfx.fctrFirst Draft::`                                       <2e-16
## `Headline.pfx.fctrToday in Politics::`                                 <2e-16
## `Headline.pfx.fctrmyFood::`                                            <2e-16
## `Headline.pfx.fctrThe Daily Gift::`                                    <2e-16
## SectionName.nb.fctrArts                                                <2e-16
## `SectionName.nb.fctrBusiness Day`                                      <2e-16
## SectionName.nb.fctrHealth                                              <2e-16
## SectionName.nb.fctrOpinion                                             <2e-16
## SectionName.nb.fctrForeign                                             <2e-16
## SectionName.nb.fctrStyles                                              <2e-16
## SectionName.nb.fctrTStyle                                              <2e-16
## SectionName.nb.fctrWorld                                               <2e-16
## SectionName.nb.fctrTechnology                                          <2e-16
## SectionName.nb.fctrMagazine                                            <2e-16
## SectionName.nb.fctrMultimedia                                          <2e-16
## `SectionName.nb.fctrmyMisc::`                                          <2e-16
## SectionName.nb.fctrTravel                                              <2e-16
## SectionName.nb.fctrU.S.                                                <2e-16
## `SectionName.nb.fctrN.Y. / Region`                                     <2e-16
## `SectionName.nb.fctrDaily Clip Report::`                                   NA
## SectionName.nb.fctrOpen                                                <2e-16
## `SectionName.nb.fctr19[0-9][0-9]::`                                    <2e-16
## `SectionName.nb.fctrReaders Respond::`                                 <2e-16
## SectionName.nb.fctrSports                                              <2e-16
## SectionName.nb.fctrNational                                            <2e-16
## `SectionName.nb.fctrVerbatim::`                                            NA
## `SectionName.nb.fctrFirst Draft::`                                         NA
## `SectionName.nb.fctrToday in Politics::`                                   NA
## `SectionName.nb.fctrmyTech::`                                          <2e-16
## `SectionName.nb.fctrmyFood::`                                          <2e-16
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::`                            <2e-16
## SectionName.nb.fctrBusiness                                            <2e-16
## `SectionName.nb.fctrThe Daily Gift::`                                      NA
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrScience                                                    NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrStyles                                                 <2e-16
## NewsDesk.nb.fctrTStyle                                                     NA
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## `NewsDesk.nb.fctrmyMisc::`                                             <2e-16
## NewsDesk.nb.fctrTravel                                                     NA
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## `NewsDesk.nb.fctr6 Q's About the News::`                                   NA
## `NewsDesk.nb.fctrTest Yourself::`                                          NA
## `NewsDesk.nb.fctrWord of the Day::`                                        NA
## `NewsDesk.nb.fctr19[0-9][0-9]::`                                           NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                               <2e-16
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrQuiz(.*)([?=|]|[?=:]::`                                   NA
## `NewsDesk.nb.fctrmyTech::`                                             <2e-16
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrmyFood::`                                                 NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## H.num.chars.log                                                        <2e-16
## H.num.words.log                                                        <2e-16
## H.num.words.unq.log                                                    <2e-16
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                         <2e-16
## `SubsectionName.nb.fctrScience::Health`                                    NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                  <2e-16
## `SubsectionName.nb.fctrRoom For Debate`                                <2e-16
## `SubsectionName.nb.fctrForeign::Foreign`                                   NA
## `SubsectionName.nb.fctrFashion & Style`                                    NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrAsia Pacific`                                   <2e-16
## `SubsectionName.nb.fctrBusiness::Technology`                               NA
## `SubsectionName.nb.fctrMagazine::Magazine`                                 NA
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                           NA
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                   NA
## `SubsectionName.nb.fctrTravel::Travel`                                     NA
## SubsectionName.nb.fctrEducation                                            NA
## `SubsectionName.nb.fctrThe Public Editor`                                  NA
## `SubsectionName.nb.fctrSmall Business`                                     NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                               NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`             NA
## `SubsectionName.nb.fctrmyMisc::Open`                                       NA
## `SubsectionName.nb.fctrForeign::World`                                     NA
## `SubsectionName.nb.fctr19[0-9][0-9]::19[0-9][0-9]::`                       NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                             NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrmyTech::myTech::`                                   NA
## `SubsectionName.nb.fctrmyFood::myFood::`                                   NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::Quiz(.*)([?=|]|[?=:]::`       NA
## `SubsectionName.nb.fctrBusiness::Business`                                 NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## A.num.chars.log                                                        <2e-16
## S.num.chars.log                                                        <2e-16
## A.num.words.log                                                        <2e-16
## S.num.words.log                                                        <2e-16
## A.num.words.unq.log                                                    <2e-16
## S.num.words.unq.log                                                    <2e-16
##                                                                         
## (Intercept)                                                          ***
## WordCount.log                                                        ***
## PubDate.hour                                                         ***
## H.is.question                                                        ***
## PubDate.apm.fctrpm                                                   ***
## A.can                                                                ***
## S.can                                                                ***
## H.has.ebola                                                          ***
## S.make                                                               ***
## A.make                                                                  
## S.one                                                                ***
## S.state                                                              ***
## A.state                                                                 
## A.one                                                                ***
## S.said                                                               ***
## A.said                                                                  
## .rnorm                                                               ***
## `PubDate.date.fctr(7,13]`                                            ***
## `PubDate.date.fctr(13,19]`                                           ***
## `PubDate.date.fctr(19,25]`                                           ***
## `PubDate.date.fctr(25,31]`                                           ***
## PubDate.second                                                       ***
## S.presid                                                             ***
## A.presid                                                                
## S.take                                                               ***
## A.take                                                               ***
## PubDate.minute                                                       ***
## S.new                                                                ***
## A.new                                                                ***
## PubDate.wkday.fctr1                                                  ***
## PubDate.wkday.fctr2                                                  ***
## PubDate.wkday.fctr3                                                  ***
## PubDate.wkday.fctr4                                                  ***
## PubDate.wkday.fctr5                                                  ***
## PubDate.wkday.fctr6                                                  ***
## S.day                                                                ***
## A.day                                                                ***
## H.X2014                                                              ***
## S.show                                                               ***
## A.show                                                                  
## S.report                                                             ***
## A.report                                                                
## S.share                                                              ***
## A.share                                                                 
## S.year                                                               ***
## A.year                                                                  
## S.compani                                                            ***
## A.compani                                                               
## H.new                                                                ***
## S.first                                                              ***
## A.first                                                                 
## S.time                                                               ***
## A.time                                                               ***
## H.newyork                                                            ***
## S.articl                                                             ***
## A.articl                                                                
## S.will                                                               ***
## A.will                                                               ***
## H.day                                                                ***
## S.newyork                                                            ***
## A.newyork                                                               
## H.today                                                              ***
## H.report                                                             ***
## S.intern                                                             ***
## A.intern                                                                
## H.week                                                               ***
## H.fashion                                                            ***
## S.week                                                               ***
## A.week                                                                  
## S.fashion                                                            ***
## A.fashion                                                               
## `Headline.pfx.fctr19[0-9][0-9]::`                                    ***
## `Headline.pfx.fctrDaily Report::`                                    ***
## `Headline.pfx.fctr.*Fashion Week::`                                  ***
## `Headline.pfx.fctrWhat We're::`                                      ***
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                    ***
## `Headline.pfx.fctrToday in Small Business::`                         ***
## `Headline.pfx.fctrDaily Clip Report::`                               ***
## `Headline.pfx.fctrMorning Agenda::`                                  ***
## `Headline.pfx.fctrNew York Today::`                                  ***
## `Headline.pfx.fctr6 Q's About the News::`                            ***
## `Headline.pfx.fctrTest Yourself::`                                   ***
## `Headline.pfx.fctrWord of the Day::`                                 ***
## `Headline.pfx.fctrmyTech::`                                          ***
## `Headline.pfx.fctrYour Turn::`                                       ***
## `Headline.pfx.fctrReaders Respond::`                                 ***
## `Headline.pfx.fctrAsk Well::`                                        ***
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                            ***
## `Headline.pfx.fctrVerbatim::`                                        ***
## `Headline.pfx.fctrFirst Draft::`                                     ***
## `Headline.pfx.fctrToday in Politics::`                               ***
## `Headline.pfx.fctrmyFood::`                                          ***
## `Headline.pfx.fctrThe Daily Gift::`                                  ***
## SectionName.nb.fctrArts                                              ***
## `SectionName.nb.fctrBusiness Day`                                    ***
## SectionName.nb.fctrHealth                                            ***
## SectionName.nb.fctrOpinion                                           ***
## SectionName.nb.fctrForeign                                           ***
## SectionName.nb.fctrStyles                                            ***
## SectionName.nb.fctrTStyle                                            ***
## SectionName.nb.fctrWorld                                             ***
## SectionName.nb.fctrTechnology                                        ***
## SectionName.nb.fctrMagazine                                          ***
## SectionName.nb.fctrMultimedia                                        ***
## `SectionName.nb.fctrmyMisc::`                                        ***
## SectionName.nb.fctrTravel                                            ***
## SectionName.nb.fctrU.S.                                              ***
## `SectionName.nb.fctrN.Y. / Region`                                   ***
## `SectionName.nb.fctrDaily Clip Report::`                                
## SectionName.nb.fctrOpen                                              ***
## `SectionName.nb.fctr19[0-9][0-9]::`                                  ***
## `SectionName.nb.fctrReaders Respond::`                               ***
## SectionName.nb.fctrSports                                            ***
## SectionName.nb.fctrNational                                          ***
## `SectionName.nb.fctrVerbatim::`                                         
## `SectionName.nb.fctrFirst Draft::`                                      
## `SectionName.nb.fctrToday in Politics::`                                
## `SectionName.nb.fctrmyTech::`                                        ***
## `SectionName.nb.fctrmyFood::`                                        ***
## SectionName.nb.fctrCulture                                              
## `SectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::`                          ***
## SectionName.nb.fctrBusiness                                          ***
## `SectionName.nb.fctrThe Daily Gift::`                                   
## NewsDesk.nb.fctrCulture                                                 
## NewsDesk.nb.fctrScience                                                 
## NewsDesk.nb.fctrOpEd                                                    
## NewsDesk.nb.fctrForeign                                                 
## NewsDesk.nb.fctrStyles                                               ***
## NewsDesk.nb.fctrTStyle                                                  
## NewsDesk.nb.fctrMagazine                                                
## NewsDesk.nb.fctrmyMultimedia                                            
## `NewsDesk.nb.fctrmyMisc::`                                           ***
## NewsDesk.nb.fctrTravel                                                  
## NewsDesk.nb.fctrMetro                                                   
## `NewsDesk.nb.fctrDaily Clip Report::`                                   
## `NewsDesk.nb.fctr6 Q's About the News::`                                
## `NewsDesk.nb.fctrTest Yourself::`                                       
## `NewsDesk.nb.fctrWord of the Day::`                                     
## `NewsDesk.nb.fctr19[0-9][0-9]::`                                        
## `NewsDesk.nb.fctrReaders Respond::`                                     
## NewsDesk.nb.fctrNational                                             ***
## NewsDesk.nb.fctrSports                                                  
## `NewsDesk.nb.fctrQuiz(.*)([?=|]|[?=:]::`                                
## `NewsDesk.nb.fctrmyTech::`                                           ***
## `NewsDesk.nb.fctrVerbatim::`                                            
## `NewsDesk.nb.fctrFirst Draft::`                                         
## `NewsDesk.nb.fctrToday in Politics::`                                   
## `NewsDesk.nb.fctrmyFood::`                                              
## `NewsDesk.nb.fctrThe Daily Gift::`                                      
## H.num.chars.log                                                      ***
## H.num.words.log                                                      ***
## H.num.words.unq.log                                                  ***
## `SubsectionName.nb.fctrCulture::Arts`                                   
## SubsectionName.nb.fctrDealbook                                       ***
## `SubsectionName.nb.fctrScience::Health`                                 
## `SubsectionName.nb.fctrOpEd::Opinion`                                ***
## `SubsectionName.nb.fctrRoom For Debate`                              ***
## `SubsectionName.nb.fctrForeign::Foreign`                                
## `SubsectionName.nb.fctrFashion & Style`                                 
## `SubsectionName.nb.fctrTStyle::TStyle`                                  
## `SubsectionName.nb.fctrAsia Pacific`                                 ***
## `SubsectionName.nb.fctrBusiness::Technology`                            
## `SubsectionName.nb.fctrMagazine::Magazine`                              
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                        
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                
## `SubsectionName.nb.fctrTravel::Travel`                                  
## SubsectionName.nb.fctrEducation                                         
## `SubsectionName.nb.fctrThe Public Editor`                               
## `SubsectionName.nb.fctrSmall Business`                                  
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                            
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`          
## `SubsectionName.nb.fctrmyMisc::Open`                                    
## `SubsectionName.nb.fctrForeign::World`                                  
## `SubsectionName.nb.fctr19[0-9][0-9]::19[0-9][0-9]::`                    
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`              
## SubsectionName.nb.fctrPolitics                                          
## `SubsectionName.nb.fctrSports::Sports`                                  
## `SubsectionName.nb.fctrNational::National`                              
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                            
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                      
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`          
## `SubsectionName.nb.fctrmyTech::myTech::`                                
## `SubsectionName.nb.fctrmyFood::myFood::`                                
## `SubsectionName.nb.fctrCulture::Culture`                                
## `SubsectionName.nb.fctrQuiz(.*)([?=|]|[?=:]::Quiz(.*)([?=|]|[?=:]::`    
## `SubsectionName.nb.fctrBusiness::Business`                              
## `SubsectionName.nb.fctrmyMisc::U.S.`                                    
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                
## `SubsectionName.nb.fctrmyMisc::Travel`                                  
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                     
## A.num.chars.log                                                      ***
## S.num.chars.log                                                      ***
## A.num.words.log                                                      ***
## S.num.words.log                                                      ***
## A.num.words.unq.log                                                  ***
## S.num.words.unq.log                                                  ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance:  4042.7  on 4474  degrees of freedom
## Residual deviance: 30565.0  on 4356  degrees of freedom
## AIC: 30803
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

![](NYTBlogs_Metro_files/figure-html/fit.models_1-4.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.7059639
## 3        0.2 0.7059639
## 4        0.3 0.7059639
## 5        0.4 0.7059639
## 6        0.5 0.7059639
## 7        0.6 0.7059639
## 8        0.7 0.7059639
## 9        0.8 0.7059639
## 10       0.9 0.7059639
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3542
## 2            Y                                      240
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      184
## 2                                      509
##          Prediction
## Reference    N    Y
##         N 3542  184
##         Y  240  509
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.052514e-01   6.495923e-01   8.962925e-01   9.136773e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.879318e-44   7.561751e-03 
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

![](NYTBlogs_Metro_files/figure-html/fit.models_1-6.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.7083333
## 3        0.2 0.7083333
## 4        0.3 0.7083333
## 5        0.4 0.7083333
## 6        0.5 0.7083333
## 7        0.6 0.7083333
## 8        0.7 0.7083333
## 9        0.8 0.7083333
## 10       0.9 0.7083333
## 11       1.0 0.0000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-8.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1623
## 2            Y                                      106
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       90
## 2                                      238
##          Prediction
## Reference    N    Y
##         N 1623   90
##         Y  106  238
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.047156e-01   6.514283e-01   8.912012e-01   9.170632e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.055033e-21   2.839768e-01 
##            model_id model_method
## 1 Conditional.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     feats
## 1 WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     28.612                  8.54
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.815095                    0.9       0.7059639        0.9025717
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8962925             0.9136773     0.6374782   0.8196605
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.7083333        0.9047156
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8912012             0.9170632     0.6514283    30803.02
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.00731871      0.03073687
## [1] "fitting model: Conditional.X.no.rnorm.rpart"
## [1] "    indep_vars: WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
## + Fold1: cp=0.03738 
## - Fold1: cp=0.03738 
## + Fold2: cp=0.03738 
## - Fold2: cp=0.03738 
## + Fold3: cp=0.03738 
## - Fold3: cp=0.03738 
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0394 on full training set
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-9.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_1-10.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##           CP nsplit rel error
## 1 0.22162884      0 1.0000000
## 2 0.03938585      1 0.7783712
## 
## Variable importance
## SubsectionName.nb.fctrOpEd::Opinion                NewsDesk.nb.fctrOpEd 
##                                  33                                  28 
##          SectionName.nb.fctrOpinion                     A.num.chars.log 
##                                  28                                   4 
##                     S.num.chars.log                 A.num.words.unq.log 
##                                   4                                   4 
## 
## Node number 1: 4475 observations,    complexity param=0.2216288
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (4101 obs) right son=3 (374 obs)
##   Primary splits:
##       SubsectionName.nb.fctrOpEd::Opinion < 0.5      to the left,  improve=251.00800, (0 missing)
##       SectionName.nb.fctrOpinion          < 0.5      to the left,  improve=223.18070, (0 missing)
##       NewsDesk.nb.fctrOpEd                < 0.5      to the left,  improve=223.18070, (0 missing)
##       WordCount.log                       < 6.528688 to the left,  improve=109.59970, (0 missing)
##       A.num.chars.log                     < 3.795426 to the right, improve= 95.79591, (0 missing)
##   Surrogate splits:
##       SectionName.nb.fctrOpinion < 0.5      to the left,  agree=0.987, adj=0.845, (0 split)
##       NewsDesk.nb.fctrOpEd       < 0.5      to the left,  agree=0.987, adj=0.845, (0 split)
##       A.num.chars.log            < 3.725621 to the right, agree=0.927, adj=0.123, (0 split)
##       S.num.chars.log            < 3.725621 to the right, agree=0.927, adj=0.123, (0 split)
##       A.num.words.unq.log        < 1.497866 to the right, agree=0.926, adj=0.115, (0 split)
## 
## Node number 2: 4101 observations
##   predicted class=N  expected loss=0.1168008  P(node) =0.9164246
##     class counts:  3622   479
##    probabilities: 0.883 0.117 
## 
## Node number 3: 374 observations
##   predicted class=Y  expected loss=0.2780749  P(node) =0.08357542
##     class counts:   104   270
##    probabilities: 0.278 0.722 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 4475 749 N (0.8326257 0.1673743)  
##   2) SubsectionName.nb.fctrOpEd::Opinion< 0.5 4101 479 N (0.8831992 0.1168008) *
##   3) SubsectionName.nb.fctrOpEd::Opinion>=0.5 374 104 Y (0.2780749 0.7219251) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.2867534
## 3        0.2 0.4808549
## 4        0.3 0.4808549
## 5        0.4 0.4808549
## 6        0.5 0.4808549
## 7        0.6 0.4808549
## 8        0.7 0.4808549
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rpart.N
## 1            N                                                3622
## 2            Y                                                 479
##   Popular.fctr.predict.Conditional.X.no.rnorm.rpart.Y
## 1                                                 104
## 2                                                 270
##          Prediction
## Reference    N    Y
##         N 3622  104
##         Y  479  270
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.697207e-01   4.157169e-01   8.595050e-01   8.794511e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   3.957808e-12   4.084715e-54 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-13.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.2865473
## 3        0.2 0.5176909
## 4        0.3 0.5176909
## 5        0.4 0.5176909
## 6        0.5 0.5176909
## 7        0.6 0.5176909
## 8        0.7 0.5176909
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rpart.N
## 1            N                                                1659
## 2            Y                                                 205
##   Popular.fctr.predict.Conditional.X.no.rnorm.rpart.Y
## 1                                                  54
## 2                                                 139
##          Prediction
## Reference    N    Y
##         N 1659   54
##         Y  205  139
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.740885e-01   4.517912e-01   8.589743e-01   8.881272e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.169537e-07   1.157426e-20 
##                       model_id model_method
## 1 Conditional.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             feats
## 1 WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     11.161                 1.944
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.6662843                    0.7       0.4808549        0.8744112
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.859505             0.8794511     0.4405605   0.6862731
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.5176909        0.8740885
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8589743             0.8881272     0.4517912
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01425467      0.07384068
## [1] "fitting model: Conditional.X.no.rnorm.rf"
## [1] "    indep_vars: WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-14.png) 

```
## + : mtry=  2 
## - : mtry=  2 
## + : mtry= 98 
## - : mtry= 98 
## + : mtry=194 
## - : mtry=194 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 98 on full training set
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-15.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_1-16.png) 

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
## importance       194   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           194   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.7993597
## 3        0.2 0.9190184
## 4        0.3 0.9758958
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 0.9993320
## 8        0.7 0.9338078
## 9        0.8 0.8139351
## 10       0.9 0.6528777
## 11       1.0 0.0237467
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3726
## 2            Y                                               NA
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               NA
## 2                                              749
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

![](NYTBlogs_Metro_files/figure-html/fit.models_1-19.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6026365
## 3        0.2 0.6748879
## 4        0.3 0.7174194
## 5        0.4 0.7357955
## 6        0.5 0.6968750
## 7        0.6 0.6734349
## 8        0.7 0.6206897
## 9        0.8 0.5132383
## 10       0.9 0.3364929
## 11       1.0 0.0000000
```

![](NYTBlogs_Metro_files/figure-html/fit.models_1-20.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1612
## 2            Y                                               85
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                              101
## 2                                              259
##          Prediction
## Reference    N    Y
##         N 1612  101
##         Y   85  259
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.095771e-01   6.812840e-01   8.963496e-01   9.216231e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.768804e-24   2.713960e-01 
##                    model_id model_method
## 1 Conditional.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             feats
## 1 WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                    311.255                82.151
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.9101676
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.999176                     1     0.6579964   0.9337691
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7357955        0.9095771
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8963496             0.9216231      0.681284
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
##                        model_id     model_method
## 1             MFO.myMFO_classfr    myMFO_classfr
## 2       Random.myrandom_classfr myrandom_classfr
## 3          Max.cor.Y.cv.0.rpart            rpart
## 4     Max.cor.Y.cv.0.cp.0.rpart            rpart
## 5               Max.cor.Y.rpart            rpart
## 6                 Max.cor.Y.glm              glm
## 7       Interact.High.cor.Y.glm              glm
## 8                 Low.cor.X.glm              glm
## 9             Conditional.X.glm              glm
## 10 Conditional.X.no.rnorm.rpart            rpart
## 11    Conditional.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            WordCount.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            WordCount.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            WordCount.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            WordCount.log
## 7                                                                                                                                                                                                        WordCount.log, WordCount.log:PubDate.apm.fctr, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:NewsDesk.nb.fctr, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
## 8                                                                                                                                                                                                                                                                                                       WordCount.log, H.is.question, PubDate.apm.fctr, S.can, H.has.ebola, S.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, S.take, PubDate.minute, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
## 9  WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 10         WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 11         WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##    max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1                0                      0.665                 0.003
## 2                0                      0.325                 0.002
## 3                0                      0.708                 0.070
## 4                0                      0.583                 0.058
## 5                3                      1.320                 0.067
## 6                1                      1.152                 0.076
## 7                1                      4.105                 1.000
## 8                1                     14.457                 4.372
## 9                1                     28.612                 8.540
## 10               3                     11.161                 1.944
## 11               3                    311.255                82.151
##    max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.5000000                    0.5       0.0000000        0.8326257
## 2    0.4975446                    0.1       0.2867534        0.1673743
## 3    0.5000000                    0.5       0.0000000        0.8326257
## 4    0.7074274                    0.2       0.4572127        0.8015642
## 5    0.5000000                    0.5       0.0000000        0.8174308
## 6    0.7301738                    0.2       0.4222222        0.8306149
## 7    0.8958855                    0.3       0.6492829        0.8737453
## 8    0.7799284                    0.9       0.6365475        0.8616763
## 9    0.8150950                    0.9       0.7059639        0.9025717
## 10   0.6662843                    0.7       0.4808549        0.8744112
## 11   1.0000000                    0.5       1.0000000        0.9101676
##    max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.8213602             0.8434553    0.00000000   0.5000000
## 2              0.1565447             0.1786398    0.00000000   0.4821958
## 3              0.8213602             0.8434553    0.00000000   0.5000000
## 4              0.7895723             0.8131613    0.33685715   0.6504263
## 5              0.8213602             0.8434553    0.06210715   0.5000000
## 6              0.6959502             0.7227703    0.01169498   0.7342331
## 7              0.8698876             0.8891668    0.48542426   0.9062453
## 8              0.8696566             0.8889511    0.43638280   0.7838596
## 9              0.8962925             0.9136773    0.63747818   0.8196605
## 10             0.8595050             0.8794511    0.44056055   0.6862731
## 11             0.9991760             1.0000000    0.65799638   0.9337691
##    opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                     0.5       0.0000000        0.8327662
## 2                     0.1       0.2865473        0.1672338
## 3                     0.5       0.0000000        0.8327662
## 4                     0.2       0.3799472        0.7715119
## 5                     0.5       0.0000000        0.8327662
## 6                     0.2       0.3961722        0.6932426
## 7                     0.4       0.6716642        0.8935343
## 8                     0.9       0.6378066        0.8779776
## 9                     0.9       0.7083333        0.9047156
## 10                    0.7       0.5176909        0.8740885
## 11                    0.4       0.7357955        0.9095771
##    max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.8159247             0.8486533     0.0000000
## 2              0.1513467             0.1840753     0.0000000
## 3              0.8159247             0.8486533     0.0000000
## 4              0.7527449             0.7895019     0.2413609
## 5              0.8159247             0.8486533     0.0000000
## 6              0.6728061             0.7131270     0.2215049
## 7              0.8793971             0.9065382     0.6082058
## 8              0.8630492             0.8918192     0.5644409
## 9              0.8912012             0.9170632     0.6514283
## 10             0.8589743             0.8881272     0.4517912
## 11             0.8963496             0.9216231     0.6812840
##    max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## 1                  NA              NA          NA
## 2                  NA              NA          NA
## 3                  NA              NA          NA
## 4                  NA              NA          NA
## 5         0.002468564     0.018885476          NA
## 6         0.001627400     0.007870559    3674.923
## 7         0.010352108     0.049190879    2700.718
## 8         0.031193380     0.176525463   39061.058
## 9         0.007318710     0.030736865   30803.018
## 10        0.014254671     0.073840685          NA
## 11                 NA              NA          NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          6          1 208.674 579.295 370.621
## 11 fit.models          6          2 579.296      NA      NA
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
##                        model_id     model_method
## 1             MFO.myMFO_classfr    myMFO_classfr
## 2       Random.myrandom_classfr myrandom_classfr
## 3          Max.cor.Y.cv.0.rpart            rpart
## 4     Max.cor.Y.cv.0.cp.0.rpart            rpart
## 5               Max.cor.Y.rpart            rpart
## 6                 Max.cor.Y.glm              glm
## 7       Interact.High.cor.Y.glm              glm
## 8                 Low.cor.X.glm              glm
## 9             Conditional.X.glm              glm
## 10 Conditional.X.no.rnorm.rpart            rpart
## 11    Conditional.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            WordCount.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            WordCount.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            WordCount.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            WordCount.log
## 7                                                                                                                                                                                                        WordCount.log, WordCount.log:PubDate.apm.fctr, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:NewsDesk.nb.fctr, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
## 8                                                                                                                                                                                                                                                                                                       WordCount.log, H.is.question, PubDate.apm.fctr, S.can, H.has.ebola, S.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, S.take, PubDate.minute, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
## 9  WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 10         WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 11         WordCount.log, PubDate.hour, H.is.question, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##    max.nTuningRuns max.auc.fit opt.prob.threshold.fit max.f.score.fit
## 1                0   0.5000000                    0.5       0.0000000
## 2                0   0.4975446                    0.1       0.2867534
## 3                0   0.5000000                    0.5       0.0000000
## 4                0   0.7074274                    0.2       0.4572127
## 5                3   0.5000000                    0.5       0.0000000
## 6                1   0.7301738                    0.2       0.4222222
## 7                1   0.8958855                    0.3       0.6492829
## 8                1   0.7799284                    0.9       0.6365475
## 9                1   0.8150950                    0.9       0.7059639
## 10               3   0.6662843                    0.7       0.4808549
## 11               3   1.0000000                    0.5       1.0000000
##    max.Accuracy.fit max.Kappa.fit max.auc.OOB opt.prob.threshold.OOB
## 1         0.8326257    0.00000000   0.5000000                    0.5
## 2         0.1673743    0.00000000   0.4821958                    0.1
## 3         0.8326257    0.00000000   0.5000000                    0.5
## 4         0.8015642    0.33685715   0.6504263                    0.2
## 5         0.8174308    0.06210715   0.5000000                    0.5
## 6         0.8306149    0.01169498   0.7342331                    0.2
## 7         0.8737453    0.48542426   0.9062453                    0.4
## 8         0.8616763    0.43638280   0.7838596                    0.9
## 9         0.9025717    0.63747818   0.8196605                    0.9
## 10        0.8744112    0.44056055   0.6862731                    0.7
## 11        0.9101676    0.65799638   0.9337691                    0.4
##    max.f.score.OOB max.Accuracy.OOB max.Kappa.OOB
## 1        0.0000000        0.8327662     0.0000000
## 2        0.2865473        0.1672338     0.0000000
## 3        0.0000000        0.8327662     0.0000000
## 4        0.3799472        0.7715119     0.2413609
## 5        0.0000000        0.8327662     0.0000000
## 6        0.3961722        0.6932426     0.2215049
## 7        0.6716642        0.8935343     0.6082058
## 8        0.6378066        0.8779776     0.5644409
## 9        0.7083333        0.9047156     0.6514283
## 10       0.5176909        0.8740885     0.4517912
## 11       0.7357955        0.9095771     0.6812840
##    inv.elapsedtime.everything inv.elapsedtime.final  inv.aic.fit
## 1                  1.50375940          333.33333333           NA
## 2                  3.07692308          500.00000000           NA
## 3                  1.41242938           14.28571429           NA
## 4                  1.71526587           17.24137931           NA
## 5                  0.75757576           14.92537313           NA
## 6                  0.86805556           13.15789474 2.721146e-04
## 7                  0.24360536            1.00000000 3.702718e-04
## 8                  0.06917064            0.22872827 2.560094e-05
## 9                  0.03495037            0.11709602 3.246435e-05
## 10                 0.08959771            0.51440329           NA
## 11                 0.00321280            0.01217271           NA
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

![](NYTBlogs_Metro_files/figure-html/fit.models_2-1.png) 

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
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

![](NYTBlogs_Metro_files/figure-html/fit.models_2-2.png) 

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
##                        model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 11    Conditional.X.no.rnorm.rf        0.9095771   0.9337691     0.6812840
## 9             Conditional.X.glm        0.9047156   0.8196605     0.6514283
## 7       Interact.High.cor.Y.glm        0.8935343   0.9062453     0.6082058
## 8                 Low.cor.X.glm        0.8779776   0.7838596     0.5644409
## 10 Conditional.X.no.rnorm.rpart        0.8740885   0.6862731     0.4517912
## 1             MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3          Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 5               Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 4     Max.cor.Y.cv.0.cp.0.rpart        0.7715119   0.6504263     0.2413609
## 6                 Max.cor.Y.glm        0.6932426   0.7342331     0.2215049
## 2       Random.myrandom_classfr        0.1672338   0.4821958     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 11          NA                    0.4
## 9    30803.018                    0.9
## 7     2700.718                    0.4
## 8    39061.058                    0.9
## 10          NA                    0.7
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 4           NA                    0.2
## 6     3674.923                    0.2
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

![](NYTBlogs_Metro_files/figure-html/fit.models_2-3.png) 

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
## [1] "Best model id: Conditional.X.no.rnorm.rf"
```

```r
if (is.null(glb_sel_mdl_id)) 
    { glb_sel_mdl_id <- dsp_models_df[1, "model_id"] } else 
        print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](NYTBlogs_Metro_files/figure-html/fit.models_2-4.png) 

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
## importance       194   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           194   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
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
##                                            id        cor.y exclude.as.feat
## WordCount.log                   WordCount.log  0.265952699           FALSE
## SubsectionName.nb.fctr SubsectionName.nb.fctr -0.213669898           FALSE
## PubDate.hour                     PubDate.hour  0.159167673           FALSE
## NewsDesk.nb.fctr             NewsDesk.nb.fctr -0.170890512           FALSE
## PubDate.minute                 PubDate.minute -0.031469083           FALSE
## H.num.chars.log               H.num.chars.log -0.171062360           FALSE
## SectionName.nb.fctr       SectionName.nb.fctr -0.155578018           FALSE
## A.num.chars.log               A.num.chars.log -0.224548821           FALSE
## S.num.chars.log               S.num.chars.log -0.224692967           FALSE
## PubDate.second                 PubDate.second -0.012253600           FALSE
## H.num.words.log               H.num.words.log -0.200686356           FALSE
## H.num.words.unq.log       H.num.words.unq.log -0.204496360           FALSE
## Headline.pfx.fctr           Headline.pfx.fctr -0.102262465           FALSE
## S.num.words.unq.log       S.num.words.unq.log -0.250796919           FALSE
## A.num.words.unq.log       A.num.words.unq.log -0.250601203           FALSE
## A.num.words.log               A.num.words.log -0.245073324           FALSE
## S.num.words.log               S.num.words.log -0.245354135           FALSE
## H.is.question                   H.is.question  0.129154799           FALSE
## PubDate.wkday.fctr         PubDate.wkday.fctr -0.039801288           FALSE
## PubDate.date.fctr           PubDate.date.fctr -0.011647558           FALSE
## PubDate.apm.fctr             PubDate.apm.fctr  0.101472715           FALSE
## S.one                                   S.one  0.006342094           FALSE
## S.time                                 S.time -0.057595102           FALSE
## A.time                                 A.time -0.057790617           FALSE
## A.one                                   A.one  0.005696039           FALSE
## S.new                                   S.new -0.034948520           FALSE
## A.new                                   A.new -0.035359447           FALSE
## A.year                                 A.year -0.051146178           FALSE
## S.year                                 S.year -0.051146178           FALSE
## H.day                                   H.day -0.061669687           FALSE
## S.can                                   S.can  0.029999780           FALSE
## A.can                                   A.can  0.031498867           FALSE
## S.report                             S.report -0.050211524           FALSE
## A.will                                 A.will -0.061025004           FALSE
## S.will                                 S.will -0.060575493           FALSE
## A.week                                 A.week -0.084814939           FALSE
## A.report                             A.report -0.050211524           FALSE
## A.said                                 A.said  0.001363226           FALSE
## S.state                               S.state  0.006069626           FALSE
## A.state                               A.state  0.005702163           FALSE
## S.week                                 S.week -0.084814939           FALSE
## S.said                                 S.said  0.001363226           FALSE
## S.newyork                           S.newyork -0.062117105           FALSE
## A.newyork                           A.newyork -0.062117105           FALSE
## S.compani                           S.compani -0.053012962           FALSE
## A.compani                           A.compani -0.053099633           FALSE
## A.take                                 A.take -0.026086108           FALSE
## S.take                                 S.take -0.025762398           FALSE
## A.make                                 A.make  0.023138853           FALSE
## S.make                                 S.make  0.023138853           FALSE
## A.show                                 A.show -0.048801740           FALSE
## S.presid                             S.presid -0.019828826           FALSE
## A.share                               A.share -0.050329686           FALSE
## S.show                                 S.show -0.048801740           FALSE
## H.has.ebola                       H.has.ebola  0.025881397           FALSE
## S.share                               S.share -0.050329686           FALSE
## A.presid                             A.presid -0.019828826           FALSE
## S.day                                   S.day -0.045649185           FALSE
## A.day                                   A.day -0.045909684           FALSE
## H.new                                   H.new -0.053121542           FALSE
## H.report                             H.report -0.064948102           FALSE
## A.first                               A.first -0.053388178           FALSE
## S.intern                             S.intern -0.068485701           FALSE
## A.intern                             A.intern -0.068485701           FALSE
## S.first                               S.first -0.053388178           FALSE
## H.week                                 H.week -0.075105216           FALSE
## H.newyork                           H.newyork -0.057970095           FALSE
## H.X2014                               H.X2014 -0.046206380           FALSE
## H.today                               H.today -0.063723058           FALSE
## A.articl                             A.articl -0.059520554           FALSE
## S.articl                             S.articl -0.059520554           FALSE
## H.fashion                           H.fashion -0.081708612           FALSE
## A.fashion                           A.fashion -0.086446251           FALSE
## S.fashion                           S.fashion -0.086446251           FALSE
## .rnorm                                 .rnorm -0.008703337           FALSE
## A.has.http                         A.has.http -0.013592603           FALSE
## A.num.chars                       A.num.chars -0.177037425            TRUE
## A.num.words                       A.num.words -0.204211072            TRUE
## A.num.words.unq               A.num.words.unq -0.210242145            TRUE
## H.daili                               H.daili -0.069192975           FALSE
## H.has.http                         H.has.http           NA           FALSE
## H.num.chars                       H.num.chars -0.147211183            TRUE
## H.num.words                       H.num.words -0.186036895            TRUE
## H.num.words.unq               H.num.words.unq -0.189702157            TRUE
## H.X2015                               H.X2015 -0.066584892           FALSE
## Popular                               Popular  1.000000000            TRUE
## Popular.fctr                     Popular.fctr           NA            TRUE
## PubDate.month.fctr         PubDate.month.fctr  0.019148739            TRUE
## PubDate.year                     PubDate.year           NA            TRUE
## S.has.http                         S.has.http           NA           FALSE
## S.num.chars                       S.num.chars -0.179331806            TRUE
## S.num.words                       S.num.words -0.206385049            TRUE
## S.num.words.unq               S.num.words.unq -0.212102717            TRUE
## UniqueID                             UniqueID  0.011824920            TRUE
## WordCount                           WordCount  0.257526549            TRUE
##                          cor.y.abs       cor.high.X is.ConditionalX.y
## WordCount.log          0.265952699             <NA>              TRUE
## SubsectionName.nb.fctr 0.213669898 NewsDesk.nb.fctr              TRUE
## PubDate.hour           0.159167673 PubDate.apm.fctr              TRUE
## NewsDesk.nb.fctr       0.170890512             <NA>              TRUE
## PubDate.minute         0.031469083             <NA>              TRUE
## H.num.chars.log        0.171062360             <NA>              TRUE
## SectionName.nb.fctr    0.155578018             <NA>              TRUE
## A.num.chars.log        0.224548821             <NA>              TRUE
## S.num.chars.log        0.224692967  A.num.chars.log              TRUE
## PubDate.second         0.012253600             <NA>              TRUE
## H.num.words.log        0.200686356             <NA>              TRUE
## H.num.words.unq.log    0.204496360  H.num.chars.log              TRUE
## Headline.pfx.fctr      0.102262465             <NA>              TRUE
## S.num.words.unq.log    0.250796919  S.num.chars.log              TRUE
## A.num.words.unq.log    0.250601203             <NA>              TRUE
## A.num.words.log        0.245073324             <NA>              TRUE
## S.num.words.log        0.245354135  A.num.words.log              TRUE
## H.is.question          0.129154799             <NA>              TRUE
## PubDate.wkday.fctr     0.039801288             <NA>              TRUE
## PubDate.date.fctr      0.011647558             <NA>              TRUE
## PubDate.apm.fctr       0.101472715             <NA>              TRUE
## S.one                  0.006342094             <NA>              TRUE
## S.time                 0.057595102             <NA>              TRUE
## A.time                 0.057790617           S.time              TRUE
## A.one                  0.005696039             <NA>              TRUE
## S.new                  0.034948520             <NA>              TRUE
## A.new                  0.035359447            S.new              TRUE
## A.year                 0.051146178           S.year              TRUE
## S.year                 0.051146178             <NA>              TRUE
## H.day                  0.061669687             <NA>              TRUE
## S.can                  0.029999780             <NA>              TRUE
## A.can                  0.031498867            S.can              TRUE
## S.report               0.050211524             <NA>              TRUE
## A.will                 0.061025004           S.will              TRUE
## S.will                 0.060575493             <NA>              TRUE
## A.week                 0.084814939           S.week              TRUE
## A.report               0.050211524         S.report              TRUE
## A.said                 0.001363226             <NA>              TRUE
## S.state                0.006069626             <NA>              TRUE
## A.state                0.005702163             <NA>              TRUE
## S.week                 0.084814939             <NA>              TRUE
## S.said                 0.001363226             <NA>              TRUE
## S.newyork              0.062117105             <NA>              TRUE
## A.newyork              0.062117105        S.newyork              TRUE
## S.compani              0.053012962             <NA>              TRUE
## A.compani              0.053099633        S.compani              TRUE
## A.take                 0.026086108           S.take              TRUE
## S.take                 0.025762398             <NA>              TRUE
## A.make                 0.023138853           S.make              TRUE
## S.make                 0.023138853             <NA>              TRUE
## A.show                 0.048801740           S.show              TRUE
## S.presid               0.019828826             <NA>              TRUE
## A.share                0.050329686          S.share              TRUE
## S.show                 0.048801740             <NA>              TRUE
## H.has.ebola            0.025881397             <NA>              TRUE
## S.share                0.050329686             <NA>              TRUE
## A.presid               0.019828826         S.presid              TRUE
## S.day                  0.045649185             <NA>              TRUE
## A.day                  0.045909684            S.day              TRUE
## H.new                  0.053121542             <NA>              TRUE
## H.report               0.064948102             <NA>              TRUE
## A.first                0.053388178          S.first              TRUE
## S.intern               0.068485701             <NA>              TRUE
## A.intern               0.068485701         S.intern              TRUE
## S.first                0.053388178             <NA>              TRUE
## H.week                 0.075105216             <NA>              TRUE
## H.newyork              0.057970095             <NA>              TRUE
## H.X2014                0.046206380             <NA>              TRUE
## H.today                0.063723058             <NA>              TRUE
## A.articl               0.059520554         S.articl              TRUE
## S.articl               0.059520554             <NA>              TRUE
## H.fashion              0.081708612           H.week              TRUE
## A.fashion              0.086446251        S.fashion              TRUE
## S.fashion              0.086446251             <NA>              TRUE
## .rnorm                 0.008703337             <NA>              TRUE
## A.has.http             0.013592603             <NA>             FALSE
## A.num.chars            0.177037425             <NA>                NA
## A.num.words            0.204211072             <NA>                NA
## A.num.words.unq        0.210242145             <NA>                NA
## H.daili                0.069192975             <NA>             FALSE
## H.has.http                      NA             <NA>             FALSE
## H.num.chars            0.147211183             <NA>                NA
## H.num.words            0.186036895             <NA>                NA
## H.num.words.unq        0.189702157             <NA>                NA
## H.X2015                0.066584892             <NA>             FALSE
## Popular                1.000000000             <NA>                NA
## Popular.fctr                    NA             <NA>                NA
## PubDate.month.fctr     0.019148739             <NA>                NA
## PubDate.year                    NA             <NA>                NA
## S.has.http                      NA             <NA>             FALSE
## S.num.chars            0.179331806             <NA>                NA
## S.num.words            0.206385049             <NA>                NA
## S.num.words.unq        0.212102717             <NA>                NA
## UniqueID               0.011824920             <NA>                NA
## WordCount              0.257526549             <NA>                NA
##                        is.cor.y.abs.low rsp_var_raw id_var rsp_var
## WordCount.log                     FALSE       FALSE     NA      NA
## SubsectionName.nb.fctr            FALSE       FALSE     NA      NA
## PubDate.hour                      FALSE       FALSE     NA      NA
## NewsDesk.nb.fctr                  FALSE       FALSE     NA      NA
## PubDate.minute                    FALSE       FALSE     NA      NA
## H.num.chars.log                   FALSE       FALSE     NA      NA
## SectionName.nb.fctr               FALSE       FALSE     NA      NA
## A.num.chars.log                   FALSE       FALSE     NA      NA
## S.num.chars.log                   FALSE       FALSE     NA      NA
## PubDate.second                    FALSE       FALSE     NA      NA
## H.num.words.log                   FALSE       FALSE     NA      NA
## H.num.words.unq.log               FALSE       FALSE     NA      NA
## Headline.pfx.fctr                 FALSE       FALSE     NA      NA
## S.num.words.unq.log               FALSE       FALSE     NA      NA
## A.num.words.unq.log               FALSE       FALSE     NA      NA
## A.num.words.log                   FALSE       FALSE     NA      NA
## S.num.words.log                   FALSE       FALSE     NA      NA
## H.is.question                     FALSE       FALSE     NA      NA
## PubDate.wkday.fctr                FALSE       FALSE     NA      NA
## PubDate.date.fctr                 FALSE       FALSE     NA      NA
## PubDate.apm.fctr                  FALSE       FALSE     NA      NA
## S.one                              TRUE       FALSE     NA      NA
## S.time                            FALSE       FALSE     NA      NA
## A.time                            FALSE       FALSE     NA      NA
## A.one                              TRUE       FALSE     NA      NA
## S.new                             FALSE       FALSE     NA      NA
## A.new                             FALSE       FALSE     NA      NA
## A.year                            FALSE       FALSE     NA      NA
## S.year                            FALSE       FALSE     NA      NA
## H.day                             FALSE       FALSE     NA      NA
## S.can                             FALSE       FALSE     NA      NA
## A.can                             FALSE       FALSE     NA      NA
## S.report                          FALSE       FALSE     NA      NA
## A.will                            FALSE       FALSE     NA      NA
## S.will                            FALSE       FALSE     NA      NA
## A.week                            FALSE       FALSE     NA      NA
## A.report                          FALSE       FALSE     NA      NA
## A.said                             TRUE       FALSE     NA      NA
## S.state                            TRUE       FALSE     NA      NA
## A.state                            TRUE       FALSE     NA      NA
## S.week                            FALSE       FALSE     NA      NA
## S.said                             TRUE       FALSE     NA      NA
## S.newyork                         FALSE       FALSE     NA      NA
## A.newyork                         FALSE       FALSE     NA      NA
## S.compani                         FALSE       FALSE     NA      NA
## A.compani                         FALSE       FALSE     NA      NA
## A.take                            FALSE       FALSE     NA      NA
## S.take                            FALSE       FALSE     NA      NA
## A.make                            FALSE       FALSE     NA      NA
## S.make                            FALSE       FALSE     NA      NA
## A.show                            FALSE       FALSE     NA      NA
## S.presid                          FALSE       FALSE     NA      NA
## A.share                           FALSE       FALSE     NA      NA
## S.show                            FALSE       FALSE     NA      NA
## H.has.ebola                       FALSE       FALSE     NA      NA
## S.share                           FALSE       FALSE     NA      NA
## A.presid                          FALSE       FALSE     NA      NA
## S.day                             FALSE       FALSE     NA      NA
## A.day                             FALSE       FALSE     NA      NA
## H.new                             FALSE       FALSE     NA      NA
## H.report                          FALSE       FALSE     NA      NA
## A.first                           FALSE       FALSE     NA      NA
## S.intern                          FALSE       FALSE     NA      NA
## A.intern                          FALSE       FALSE     NA      NA
## S.first                           FALSE       FALSE     NA      NA
## H.week                            FALSE       FALSE     NA      NA
## H.newyork                         FALSE       FALSE     NA      NA
## H.X2014                           FALSE       FALSE     NA      NA
## H.today                           FALSE       FALSE     NA      NA
## A.articl                          FALSE       FALSE     NA      NA
## S.articl                          FALSE       FALSE     NA      NA
## H.fashion                         FALSE       FALSE     NA      NA
## A.fashion                         FALSE       FALSE     NA      NA
## S.fashion                         FALSE       FALSE     NA      NA
## .rnorm                            FALSE       FALSE     NA      NA
## A.has.http                        FALSE       FALSE     NA      NA
## A.num.chars                       FALSE       FALSE     NA      NA
## A.num.words                       FALSE       FALSE     NA      NA
## A.num.words.unq                   FALSE       FALSE     NA      NA
## H.daili                           FALSE       FALSE     NA      NA
## H.has.http                           NA       FALSE     NA      NA
## H.num.chars                       FALSE       FALSE     NA      NA
## H.num.words                       FALSE       FALSE     NA      NA
## H.num.words.unq                   FALSE       FALSE     NA      NA
## H.X2015                           FALSE       FALSE     NA      NA
## Popular                           FALSE        TRUE     NA      NA
## Popular.fctr                         NA          NA     NA    TRUE
## PubDate.month.fctr                FALSE       FALSE     NA      NA
## PubDate.year                         NA       FALSE     NA      NA
## S.has.http                           NA       FALSE     NA      NA
## S.num.chars                       FALSE       FALSE     NA      NA
## S.num.words                       FALSE       FALSE     NA      NA
## S.num.words.unq                   FALSE       FALSE     NA      NA
## UniqueID                          FALSE       FALSE   TRUE      NA
## WordCount                         FALSE       FALSE     NA      NA
##                          importance Conditional.X.no.rnorm.rf.importance
## WordCount.log          100.00000000                         100.00000000
## SubsectionName.nb.fctr  75.94943243                          75.94943243
## PubDate.hour            51.77902587                          51.77902587
## NewsDesk.nb.fctr        35.88460826                          35.88460826
## PubDate.minute          31.83753233                          31.83753233
## H.num.chars.log         31.82815922                          31.82815922
## SectionName.nb.fctr     30.15727347                          30.15727347
## A.num.chars.log         28.73445312                          28.73445312
## S.num.chars.log         28.64600145                          28.64600145
## PubDate.second          24.97368401                          24.97368401
## H.num.words.log         11.93904244                          11.93904244
## H.num.words.unq.log     11.77933121                          11.77933121
## Headline.pfx.fctr       11.58021109                          11.58021109
## S.num.words.unq.log     11.43976670                          11.43976670
## A.num.words.unq.log     11.29529950                          11.29529950
## A.num.words.log         10.96589442                          10.96589442
## S.num.words.log         10.88606595                          10.88606595
## H.is.question            7.72406831                           7.72406831
## PubDate.wkday.fctr       5.11743562                           5.11743562
## PubDate.date.fctr        3.42556063                           3.42556063
## PubDate.apm.fctr         3.41399600                           3.41399600
## S.one                    2.62187190                           2.62187190
## S.time                   2.58414555                           2.58414555
## A.time                   2.34815137                           2.34815137
## A.one                    2.33323297                           2.33323297
## S.new                    2.13899035                           2.13899035
## A.new                    2.09801769                           2.09801769
## A.year                   1.68387596                           1.68387596
## S.year                   1.68337992                           1.68337992
## H.day                    1.63512203                           1.63512203
## S.can                    1.61371956                           1.61371956
## A.can                    1.44862975                           1.44862975
## S.report                 1.43918550                           1.43918550
## A.will                   1.38730172                           1.38730172
## S.will                   1.36680498                           1.36680498
## A.week                   1.34663990                           1.34663990
## A.report                 1.33186398                           1.33186398
## A.said                   1.27009593                           1.27009593
## S.state                  1.26959882                           1.26959882
## A.state                  1.26682812                           1.26682812
## S.week                   1.21473174                           1.21473174
## S.said                   1.21457535                           1.21457535
## S.newyork                1.19464000                           1.19464000
## A.newyork                1.17716672                           1.17716672
## S.compani                1.13502270                           1.13502270
## A.compani                1.13159684                           1.13159684
## A.take                   1.10547591                           1.10547591
## S.take                   1.04952996                           1.04952996
## A.make                   0.91890089                           0.91890089
## S.make                   0.90600935                           0.90600935
## A.show                   0.89981034                           0.89981034
## S.presid                 0.89652394                           0.89652394
## A.share                  0.88055003                           0.88055003
## S.show                   0.87585858                           0.87585858
## H.has.ebola              0.87326832                           0.87326832
## S.share                  0.86634396                           0.86634396
## A.presid                 0.79566039                           0.79566039
## S.day                    0.70412014                           0.70412014
## A.day                    0.66104799                           0.66104799
## H.new                    0.59499600                           0.59499600
## H.report                 0.50235040                           0.50235040
## A.first                  0.49412212                           0.49412212
## S.intern                 0.49060159                           0.49060159
## A.intern                 0.40615745                           0.40615745
## S.first                  0.39434669                           0.39434669
## H.week                   0.36306631                           0.36306631
## H.newyork                0.34885977                           0.34885977
## H.X2014                  0.30421277                           0.30421277
## H.today                  0.27455637                           0.27455637
## A.articl                 0.27058395                           0.27058395
## S.articl                 0.20426541                           0.20426541
## H.fashion                0.11912548                           0.11912548
## A.fashion                0.10168751                           0.10168751
## S.fashion                0.09431399                           0.09431399
## .rnorm                           NA                                   NA
## A.has.http                       NA                                   NA
## A.num.chars                      NA                                   NA
## A.num.words                      NA                                   NA
## A.num.words.unq                  NA                                   NA
## H.daili                          NA                                   NA
## H.has.http                       NA                                   NA
## H.num.chars                      NA                                   NA
## H.num.words                      NA                                   NA
## H.num.words.unq                  NA                                   NA
## H.X2015                          NA                                   NA
## Popular                          NA                                   NA
## Popular.fctr                     NA                                   NA
## PubDate.month.fctr               NA                                   NA
## PubDate.year                     NA                                   NA
## S.has.http                       NA                                   NA
## S.num.chars                      NA                                   NA
## S.num.words                      NA                                   NA
## S.num.words.unq                  NA                                   NA
## UniqueID                         NA                                   NA
## WordCount                        NA                                   NA
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 74
```

![](NYTBlogs_Metro_files/figure-html/fit.models_2-5.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_2-6.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_2-7.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_2-8.png) ![](NYTBlogs_Metro_files/figure-html/fit.models_2-9.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr
## 1654     1654            N
## 6370     6370            Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 1654                                               0.004
## 6370                                               0.440
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 1654                                              N
## 6370                                              Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 1654                                                    TRUE
## 6370                                                    TRUE
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.error .label
## 1654                                                    0   1654
## 6370                                                    0   6370
## [1] "Inaccurate: "
##      UniqueID Popular.fctr
## 92         92            Y
## 2527     2527            Y
## 3554     3554            Y
## 4721     4721            Y
## 693       693            Y
## 3635     3635            Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 92                                                 0.000
## 2527                                               0.002
## 3554                                               0.002
## 4721                                               0.002
## 693                                                0.006
## 3635                                               0.008
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 92                                                N
## 2527                                              N
## 3554                                              N
## 4721                                              N
## 693                                               N
## 3635                                              N
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 92                                                     FALSE
## 2527                                                   FALSE
## 3554                                                   FALSE
## 4721                                                   FALSE
## 693                                                    FALSE
## 3635                                                   FALSE
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.error
## 92                                                 -0.400
## 2527                                               -0.398
## 3554                                               -0.398
## 4721                                               -0.398
## 693                                                -0.394
## 3635                                               -0.392
##      UniqueID Popular.fctr
## 3231     3231            Y
## 2991     2991            Y
## 4387     4387            Y
## 4736     4736            N
## 6479     6479            N
## 6046     6046            N
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 3231                                               0.162
## 2991                                               0.286
## 4387                                               0.392
## 4736                                               0.476
## 6479                                               0.554
## 6046                                               0.618
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 3231                                              N
## 2991                                              N
## 4387                                              N
## 4736                                              Y
## 6479                                              Y
## 6046                                              Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 3231                                                   FALSE
## 2991                                                   FALSE
## 4387                                                   FALSE
## 4736                                                   FALSE
## 6479                                                   FALSE
## 6046                                                   FALSE
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.error
## 3231                                               -0.238
## 2991                                               -0.114
## 4387                                               -0.008
## 4736                                                0.076
## 6479                                                0.154
## 6046                                                0.218
##      UniqueID Popular.fctr
## 679       679            N
## 4763     4763            N
## 4771     4771            N
## 2510     2510            N
## 17         17            N
## 4929     4929            N
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 679                                                0.918
## 4763                                               0.946
## 4771                                               0.948
## 2510                                               0.950
## 17                                                 0.970
## 4929                                               0.972
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 679                                               Y
## 4763                                              Y
## 4771                                              Y
## 2510                                              Y
## 17                                                Y
## 4929                                              Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 679                                                    FALSE
## 4763                                                   FALSE
## 4771                                                   FALSE
## 2510                                                   FALSE
## 17                                                     FALSE
## 4929                                                   FALSE
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.error
## 679                                                 0.518
## 4763                                                0.546
## 4771                                                0.548
## 2510                                                0.550
## 17                                                  0.570
## 4929                                                0.572
```

![](NYTBlogs_Metro_files/figure-html/fit.models_2-10.png) 

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
##      Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 92              Y                                               0.000
## 693             Y                                               0.006
## 4020            Y                                               0.028
## 4721            Y                                               0.002
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 92                                                N
## 693                                               N
## 4020                                              N
## 4721                                              N
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 92                                                     FALSE
## 693                                                    FALSE
## 4020                                                   FALSE
## 4721                                                   FALSE
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
##      WordCount.log SubsectionName.nb.fctr PubDate.hour NewsDesk.nb.fctr
## 92        5.723585               Dealbook            8         Business
## 693       5.641907         Small Business           14         Business
## 4020      3.610918   Metro::N.Y. / Region           21            Metro
## 4721      6.030685           Asia Pacific            4          Foreign
##      PubDate.minute
## 92                2
## 693              10
## 4020             13
## 4721             23
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_txt_vars])
```

```
##                                                                   Headline
## 92   Moelis & Co. Hires Cantor, Ex-House Majority Leader, as Vice Chairman
## 693                                Do You Hire Employees on a Trial Basis?
## 4020       Video: News Conference About Ebola Patient at Bellevue Hospital
## 4721     Hong Kong Politician Likens Protesters to African-American Slaves
##                                                                                                                                                                                                                         Snippet
## 92                                                                            Eric Cantor, who suffered a surprising electoral defeat this year, will be joining Moelis & Company as vice chairman and a director on its board.
## 693                                                                                                                Do you think job candidates are willing to work for three months on a contract before being hired full-time?
## 4020                                                                                                                    A news conference about Dr. Craig Spencer at Bellevue Hospital who tested positive for the Ebola virus.
## 4721 A prominent businesswoman and politician has come under fire for saying, erroneously, that black Americans did not get voting rights for 107 years after the countrys slaves were freed, so Hong Kongers should also wait.
##                                                                                                                                                                                                                        Abstract
## 92                                                                            Eric Cantor, who suffered a surprising electoral defeat this year, will be joining Moelis & Company as vice chairman and a director on its board.
## 693                                                                                                                Do you think job candidates are willing to work for three months on a contract before being hired full-time?
## 4020                                                                                                                    A news conference about Dr. Craig Spencer at Bellevue Hospital who tested positive for the Ebola virus.
## 4721 A prominent businesswoman and politician has come under fire for saying, erroneously, that black Americans did not get voting rights for 107 years after the countrys slaves were freed, so Hong Kongers should also wait.
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
## 11 fit.models          6          2 579.296 595.631  16.335
## 12 fit.models          6          3 595.631      NA      NA
```


```r
sav_entity_df <- glb_entity_df
print(setdiff(names(glb_trnent_df), names(glb_entity_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitent_df), names(glb_entity_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBent_df), names(glb_entity_df)))
```

```
## [1] "Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob"    
## [2] "Popular.fctr.predict.Conditional.X.no.rnorm.rf"         
## [3] "Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate"
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

![](NYTBlogs_Metro_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 12        fit.models          6          3 595.631 665.194  69.563
## 13 fit.data.training          7          0 665.194      NA      NA
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
##                                            id   importance
## WordCount.log                   WordCount.log 100.00000000
## SubsectionName.nb.fctr SubsectionName.nb.fctr  75.94943243
## PubDate.hour                     PubDate.hour  51.77902587
## NewsDesk.nb.fctr             NewsDesk.nb.fctr  35.88460826
## PubDate.minute                 PubDate.minute  31.83753233
## H.num.chars.log               H.num.chars.log  31.82815922
## SectionName.nb.fctr       SectionName.nb.fctr  30.15727347
## A.num.chars.log               A.num.chars.log  28.73445312
## S.num.chars.log               S.num.chars.log  28.64600145
## PubDate.second                 PubDate.second  24.97368401
## H.num.words.log               H.num.words.log  11.93904244
## H.num.words.unq.log       H.num.words.unq.log  11.77933121
## Headline.pfx.fctr           Headline.pfx.fctr  11.58021109
## S.num.words.unq.log       S.num.words.unq.log  11.43976670
## A.num.words.unq.log       A.num.words.unq.log  11.29529950
## A.num.words.log               A.num.words.log  10.96589442
## S.num.words.log               S.num.words.log  10.88606595
## H.is.question                   H.is.question   7.72406831
## PubDate.wkday.fctr         PubDate.wkday.fctr   5.11743562
## PubDate.date.fctr           PubDate.date.fctr   3.42556063
## PubDate.apm.fctr             PubDate.apm.fctr   3.41399600
## S.one                                   S.one   2.62187190
## S.time                                 S.time   2.58414555
## A.time                                 A.time   2.34815137
## A.one                                   A.one   2.33323297
## S.new                                   S.new   2.13899035
## A.new                                   A.new   2.09801769
## A.year                                 A.year   1.68387596
## S.year                                 S.year   1.68337992
## H.day                                   H.day   1.63512203
## S.can                                   S.can   1.61371956
## A.can                                   A.can   1.44862975
## S.report                             S.report   1.43918550
## A.will                                 A.will   1.38730172
## S.will                                 S.will   1.36680498
## A.week                                 A.week   1.34663990
## A.report                             A.report   1.33186398
## A.said                                 A.said   1.27009593
## S.state                               S.state   1.26959882
## A.state                               A.state   1.26682812
## S.week                                 S.week   1.21473174
## S.said                                 S.said   1.21457535
## S.newyork                           S.newyork   1.19464000
## A.newyork                           A.newyork   1.17716672
## S.compani                           S.compani   1.13502270
## A.compani                           A.compani   1.13159684
## A.take                                 A.take   1.10547591
## S.take                                 S.take   1.04952996
## A.make                                 A.make   0.91890089
## S.make                                 S.make   0.90600935
## A.show                                 A.show   0.89981034
## S.presid                             S.presid   0.89652394
## A.share                               A.share   0.88055003
## S.show                                 S.show   0.87585858
## H.has.ebola                       H.has.ebola   0.87326832
## S.share                               S.share   0.86634396
## A.presid                             A.presid   0.79566039
## S.day                                   S.day   0.70412014
## A.day                                   A.day   0.66104799
## H.new                                   H.new   0.59499600
## H.report                             H.report   0.50235040
## A.first                               A.first   0.49412212
## S.intern                             S.intern   0.49060159
## A.intern                             A.intern   0.40615745
## S.first                               S.first   0.39434669
## H.week                                 H.week   0.36306631
## H.newyork                           H.newyork   0.34885977
## H.X2014                               H.X2014   0.30421277
## H.today                               H.today   0.27455637
## A.articl                             A.articl   0.27058395
## S.articl                             S.articl   0.20426541
## H.fashion                           H.fashion   0.11912548
## A.fashion                           A.fashion   0.10168751
## S.fashion                           S.fashion   0.09431399
## [1] "fitting model: Final.rf"
## [1] "    indep_vars: WordCount.log, SubsectionName.nb.fctr, PubDate.hour, NewsDesk.nb.fctr, PubDate.minute, H.num.chars.log, SectionName.nb.fctr, A.num.chars.log, S.num.chars.log, PubDate.second, H.num.words.log, H.num.words.unq.log, Headline.pfx.fctr, S.num.words.unq.log, A.num.words.unq.log, A.num.words.log, S.num.words.log, H.is.question, PubDate.wkday.fctr, PubDate.date.fctr, PubDate.apm.fctr, S.one, S.time, A.time, A.one, S.new, A.new, A.year, S.year, H.day, S.can, A.can, S.report, A.will, S.will, A.week, A.report, A.said, S.state, A.state, S.week, S.said, S.newyork, A.newyork, S.compani, A.compani, A.take, S.take, A.make, S.make, A.show, S.presid, A.share, S.show, H.has.ebola, S.share, A.presid, S.day, A.day, H.new, H.report, A.first, S.intern, A.intern, S.first, H.week, H.newyork, H.X2014, H.today, A.articl, S.articl, H.fashion, A.fashion, S.fashion"
## + : mtry=98 
## - : mtry=98 
## Aggregating results
## Fitting final model on full training set
```

![](NYTBlogs_Metro_files/figure-html/fit.data.training_0-1.png) 

```
##                 Length Class      Mode     
## call                4  -none-     call     
## type                1  -none-     character
## predicted        6532  factor     numeric  
## err.rate         1500  -none-     numeric  
## confusion           6  -none-     numeric  
## votes           13064  matrix     numeric  
## oob.times        6532  -none-     numeric  
## classes             2  -none-     character
## importance        194  -none-     numeric  
## importanceSD        0  -none-     NULL     
## localImportance     0  -none-     NULL     
## proximity           0  -none-     NULL     
## ntree               1  -none-     numeric  
## mtry                1  -none-     numeric  
## forest             14  -none-     list     
## y                6532  factor     numeric  
## test                0  -none-     NULL     
## inbag               0  -none-     NULL     
## xNames            194  -none-     character
## problemType         1  -none-     character
## tuneValue           1  data.frame list     
## obsLevels           2  -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_Metro_files/figure-html/fit.data.training_0-2.png) 

```
##    threshold    f.score
## 1        0.0 0.28668852
## 2        0.1 0.80102602
## 3        0.2 0.92314189
## 4        0.3 0.97720161
## 5        0.4 1.00000000
## 6        0.5 1.00000000
## 7        0.6 0.99954233
## 8        0.7 0.93521676
## 9        0.8 0.82219828
## 10       0.9 0.63375000
## 11       1.0 0.01633394
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                              NA
##   Popular.fctr.predict.Final.rf.Y
## 1                              NA
## 2                            1093
##          Prediction
## Reference    N    Y
##         N 5439    0
##         Y    0 1093
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      1.0000000      1.0000000      0.9994354      1.0000000      0.8326699 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](NYTBlogs_Metro_files/figure-html/fit.data.training_0-3.png) 

```
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             feats
## 1 WordCount.log, SubsectionName.nb.fctr, PubDate.hour, NewsDesk.nb.fctr, PubDate.minute, H.num.chars.log, SectionName.nb.fctr, A.num.chars.log, S.num.chars.log, PubDate.second, H.num.words.log, H.num.words.unq.log, Headline.pfx.fctr, S.num.words.unq.log, A.num.words.unq.log, A.num.words.log, S.num.words.log, H.is.question, PubDate.wkday.fctr, PubDate.date.fctr, PubDate.apm.fctr, S.one, S.time, A.time, A.one, S.new, A.new, A.year, S.year, H.day, S.can, A.can, S.report, A.will, S.will, A.week, A.report, A.said, S.state, A.state, S.week, S.said, S.newyork, A.newyork, S.compani, A.compani, A.take, S.take, A.make, S.make, A.show, S.presid, A.share, S.show, H.has.ebola, S.share, A.presid, S.day, A.day, H.new, H.report, A.first, S.intern, A.intern, S.first, H.week, H.newyork, H.X2014, H.today, A.articl, S.articl, H.fashion, A.fashion, S.fashion
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                    276.474               135.992
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1         0.912278
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.9994354                     1     0.6662956
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13 fit.data.training          7          0 665.194 948.527 283.333
## 14 fit.data.training          7          1 948.527      NA      NA
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
##                                            id   importance        cor.y
## WordCount.log                   WordCount.log 100.00000000  0.265952699
## SubsectionName.nb.fctr SubsectionName.nb.fctr  75.94943243 -0.213669898
## PubDate.hour                     PubDate.hour  51.77902587  0.159167673
## NewsDesk.nb.fctr             NewsDesk.nb.fctr  35.88460826 -0.170890512
## PubDate.minute                 PubDate.minute  31.83753233 -0.031469083
## H.num.chars.log               H.num.chars.log  31.82815922 -0.171062360
## SectionName.nb.fctr       SectionName.nb.fctr  30.15727347 -0.155578018
## A.num.chars.log               A.num.chars.log  28.73445312 -0.224548821
## S.num.chars.log               S.num.chars.log  28.64600145 -0.224692967
## PubDate.second                 PubDate.second  24.97368401 -0.012253600
## H.num.words.log               H.num.words.log  11.93904244 -0.200686356
## H.num.words.unq.log       H.num.words.unq.log  11.77933121 -0.204496360
## Headline.pfx.fctr           Headline.pfx.fctr  11.58021109 -0.102262465
## S.num.words.unq.log       S.num.words.unq.log  11.43976670 -0.250796919
## A.num.words.unq.log       A.num.words.unq.log  11.29529950 -0.250601203
## A.num.words.log               A.num.words.log  10.96589442 -0.245073324
## S.num.words.log               S.num.words.log  10.88606595 -0.245354135
## H.is.question                   H.is.question   7.72406831  0.129154799
## PubDate.wkday.fctr         PubDate.wkday.fctr   5.11743562 -0.039801288
## PubDate.date.fctr           PubDate.date.fctr   3.42556063 -0.011647558
## PubDate.apm.fctr             PubDate.apm.fctr   3.41399600  0.101472715
## S.one                                   S.one   2.62187190  0.006342094
## S.time                                 S.time   2.58414555 -0.057595102
## A.time                                 A.time   2.34815137 -0.057790617
## A.one                                   A.one   2.33323297  0.005696039
## S.new                                   S.new   2.13899035 -0.034948520
## A.new                                   A.new   2.09801769 -0.035359447
## A.year                                 A.year   1.68387596 -0.051146178
## S.year                                 S.year   1.68337992 -0.051146178
## H.day                                   H.day   1.63512203 -0.061669687
## S.can                                   S.can   1.61371956  0.029999780
## A.can                                   A.can   1.44862975  0.031498867
## S.report                             S.report   1.43918550 -0.050211524
## A.will                                 A.will   1.38730172 -0.061025004
## S.will                                 S.will   1.36680498 -0.060575493
## A.week                                 A.week   1.34663990 -0.084814939
## A.report                             A.report   1.33186398 -0.050211524
## A.said                                 A.said   1.27009593  0.001363226
## S.state                               S.state   1.26959882  0.006069626
## A.state                               A.state   1.26682812  0.005702163
## S.week                                 S.week   1.21473174 -0.084814939
## S.said                                 S.said   1.21457535  0.001363226
## S.newyork                           S.newyork   1.19464000 -0.062117105
## A.newyork                           A.newyork   1.17716672 -0.062117105
## S.compani                           S.compani   1.13502270 -0.053012962
## A.compani                           A.compani   1.13159684 -0.053099633
## A.take                                 A.take   1.10547591 -0.026086108
## S.take                                 S.take   1.04952996 -0.025762398
## A.make                                 A.make   0.91890089  0.023138853
## S.make                                 S.make   0.90600935  0.023138853
## A.show                                 A.show   0.89981034 -0.048801740
## S.presid                             S.presid   0.89652394 -0.019828826
## A.share                               A.share   0.88055003 -0.050329686
## S.show                                 S.show   0.87585858 -0.048801740
## H.has.ebola                       H.has.ebola   0.87326832  0.025881397
## S.share                               S.share   0.86634396 -0.050329686
## A.presid                             A.presid   0.79566039 -0.019828826
## S.day                                   S.day   0.70412014 -0.045649185
## A.day                                   A.day   0.66104799 -0.045909684
## H.new                                   H.new   0.59499600 -0.053121542
## H.report                             H.report   0.50235040 -0.064948102
## A.first                               A.first   0.49412212 -0.053388178
## S.intern                             S.intern   0.49060159 -0.068485701
## A.intern                             A.intern   0.40615745 -0.068485701
## S.first                               S.first   0.39434669 -0.053388178
## H.week                                 H.week   0.36306631 -0.075105216
## H.newyork                           H.newyork   0.34885977 -0.057970095
## H.X2014                               H.X2014   0.30421277 -0.046206380
## H.today                               H.today   0.27455637 -0.063723058
## A.articl                             A.articl   0.27058395 -0.059520554
## S.articl                             S.articl   0.20426541 -0.059520554
## H.fashion                           H.fashion   0.11912548 -0.081708612
## A.fashion                           A.fashion   0.10168751 -0.086446251
## S.fashion                           S.fashion   0.09431399 -0.086446251
## .rnorm                                 .rnorm           NA -0.008703337
## A.has.http                         A.has.http           NA -0.013592603
## A.num.chars                       A.num.chars           NA -0.177037425
## A.num.words                       A.num.words           NA -0.204211072
## A.num.words.unq               A.num.words.unq           NA -0.210242145
## H.daili                               H.daili           NA -0.069192975
## H.has.http                         H.has.http           NA           NA
## H.num.chars                       H.num.chars           NA -0.147211183
## H.num.words                       H.num.words           NA -0.186036895
## H.num.words.unq               H.num.words.unq           NA -0.189702157
## H.X2015                               H.X2015           NA -0.066584892
## Popular                               Popular           NA  1.000000000
## Popular.fctr                     Popular.fctr           NA           NA
## PubDate.month.fctr         PubDate.month.fctr           NA  0.019148739
## PubDate.year                     PubDate.year           NA           NA
## S.has.http                         S.has.http           NA           NA
## S.num.chars                       S.num.chars           NA -0.179331806
## S.num.words                       S.num.words           NA -0.206385049
## S.num.words.unq               S.num.words.unq           NA -0.212102717
## UniqueID                             UniqueID           NA  0.011824920
## WordCount                           WordCount           NA  0.257526549
##                        exclude.as.feat   cor.y.abs       cor.high.X
## WordCount.log                    FALSE 0.265952699             <NA>
## SubsectionName.nb.fctr           FALSE 0.213669898 NewsDesk.nb.fctr
## PubDate.hour                     FALSE 0.159167673 PubDate.apm.fctr
## NewsDesk.nb.fctr                 FALSE 0.170890512             <NA>
## PubDate.minute                   FALSE 0.031469083             <NA>
## H.num.chars.log                  FALSE 0.171062360             <NA>
## SectionName.nb.fctr              FALSE 0.155578018             <NA>
## A.num.chars.log                  FALSE 0.224548821             <NA>
## S.num.chars.log                  FALSE 0.224692967  A.num.chars.log
## PubDate.second                   FALSE 0.012253600             <NA>
## H.num.words.log                  FALSE 0.200686356             <NA>
## H.num.words.unq.log              FALSE 0.204496360  H.num.chars.log
## Headline.pfx.fctr                FALSE 0.102262465             <NA>
## S.num.words.unq.log              FALSE 0.250796919  S.num.chars.log
## A.num.words.unq.log              FALSE 0.250601203             <NA>
## A.num.words.log                  FALSE 0.245073324             <NA>
## S.num.words.log                  FALSE 0.245354135  A.num.words.log
## H.is.question                    FALSE 0.129154799             <NA>
## PubDate.wkday.fctr               FALSE 0.039801288             <NA>
## PubDate.date.fctr                FALSE 0.011647558             <NA>
## PubDate.apm.fctr                 FALSE 0.101472715             <NA>
## S.one                            FALSE 0.006342094             <NA>
## S.time                           FALSE 0.057595102             <NA>
## A.time                           FALSE 0.057790617           S.time
## A.one                            FALSE 0.005696039             <NA>
## S.new                            FALSE 0.034948520             <NA>
## A.new                            FALSE 0.035359447            S.new
## A.year                           FALSE 0.051146178           S.year
## S.year                           FALSE 0.051146178             <NA>
## H.day                            FALSE 0.061669687             <NA>
## S.can                            FALSE 0.029999780             <NA>
## A.can                            FALSE 0.031498867            S.can
## S.report                         FALSE 0.050211524             <NA>
## A.will                           FALSE 0.061025004           S.will
## S.will                           FALSE 0.060575493             <NA>
## A.week                           FALSE 0.084814939           S.week
## A.report                         FALSE 0.050211524         S.report
## A.said                           FALSE 0.001363226             <NA>
## S.state                          FALSE 0.006069626             <NA>
## A.state                          FALSE 0.005702163             <NA>
## S.week                           FALSE 0.084814939             <NA>
## S.said                           FALSE 0.001363226             <NA>
## S.newyork                        FALSE 0.062117105             <NA>
## A.newyork                        FALSE 0.062117105        S.newyork
## S.compani                        FALSE 0.053012962             <NA>
## A.compani                        FALSE 0.053099633        S.compani
## A.take                           FALSE 0.026086108           S.take
## S.take                           FALSE 0.025762398             <NA>
## A.make                           FALSE 0.023138853           S.make
## S.make                           FALSE 0.023138853             <NA>
## A.show                           FALSE 0.048801740           S.show
## S.presid                         FALSE 0.019828826             <NA>
## A.share                          FALSE 0.050329686          S.share
## S.show                           FALSE 0.048801740             <NA>
## H.has.ebola                      FALSE 0.025881397             <NA>
## S.share                          FALSE 0.050329686             <NA>
## A.presid                         FALSE 0.019828826         S.presid
## S.day                            FALSE 0.045649185             <NA>
## A.day                            FALSE 0.045909684            S.day
## H.new                            FALSE 0.053121542             <NA>
## H.report                         FALSE 0.064948102             <NA>
## A.first                          FALSE 0.053388178          S.first
## S.intern                         FALSE 0.068485701             <NA>
## A.intern                         FALSE 0.068485701         S.intern
## S.first                          FALSE 0.053388178             <NA>
## H.week                           FALSE 0.075105216             <NA>
## H.newyork                        FALSE 0.057970095             <NA>
## H.X2014                          FALSE 0.046206380             <NA>
## H.today                          FALSE 0.063723058             <NA>
## A.articl                         FALSE 0.059520554         S.articl
## S.articl                         FALSE 0.059520554             <NA>
## H.fashion                        FALSE 0.081708612           H.week
## A.fashion                        FALSE 0.086446251        S.fashion
## S.fashion                        FALSE 0.086446251             <NA>
## .rnorm                           FALSE 0.008703337             <NA>
## A.has.http                       FALSE 0.013592603             <NA>
## A.num.chars                       TRUE 0.177037425             <NA>
## A.num.words                       TRUE 0.204211072             <NA>
## A.num.words.unq                   TRUE 0.210242145             <NA>
## H.daili                          FALSE 0.069192975             <NA>
## H.has.http                       FALSE          NA             <NA>
## H.num.chars                       TRUE 0.147211183             <NA>
## H.num.words                       TRUE 0.186036895             <NA>
## H.num.words.unq                   TRUE 0.189702157             <NA>
## H.X2015                          FALSE 0.066584892             <NA>
## Popular                           TRUE 1.000000000             <NA>
## Popular.fctr                      TRUE          NA             <NA>
## PubDate.month.fctr                TRUE 0.019148739             <NA>
## PubDate.year                      TRUE          NA             <NA>
## S.has.http                       FALSE          NA             <NA>
## S.num.chars                       TRUE 0.179331806             <NA>
## S.num.words                       TRUE 0.206385049             <NA>
## S.num.words.unq                   TRUE 0.212102717             <NA>
## UniqueID                          TRUE 0.011824920             <NA>
## WordCount                         TRUE 0.257526549             <NA>
##                        is.ConditionalX.y is.cor.y.abs.low rsp_var_raw
## WordCount.log                       TRUE            FALSE       FALSE
## SubsectionName.nb.fctr              TRUE            FALSE       FALSE
## PubDate.hour                        TRUE            FALSE       FALSE
## NewsDesk.nb.fctr                    TRUE            FALSE       FALSE
## PubDate.minute                      TRUE            FALSE       FALSE
## H.num.chars.log                     TRUE            FALSE       FALSE
## SectionName.nb.fctr                 TRUE            FALSE       FALSE
## A.num.chars.log                     TRUE            FALSE       FALSE
## S.num.chars.log                     TRUE            FALSE       FALSE
## PubDate.second                      TRUE            FALSE       FALSE
## H.num.words.log                     TRUE            FALSE       FALSE
## H.num.words.unq.log                 TRUE            FALSE       FALSE
## Headline.pfx.fctr                   TRUE            FALSE       FALSE
## S.num.words.unq.log                 TRUE            FALSE       FALSE
## A.num.words.unq.log                 TRUE            FALSE       FALSE
## A.num.words.log                     TRUE            FALSE       FALSE
## S.num.words.log                     TRUE            FALSE       FALSE
## H.is.question                       TRUE            FALSE       FALSE
## PubDate.wkday.fctr                  TRUE            FALSE       FALSE
## PubDate.date.fctr                   TRUE            FALSE       FALSE
## PubDate.apm.fctr                    TRUE            FALSE       FALSE
## S.one                               TRUE             TRUE       FALSE
## S.time                              TRUE            FALSE       FALSE
## A.time                              TRUE            FALSE       FALSE
## A.one                               TRUE             TRUE       FALSE
## S.new                               TRUE            FALSE       FALSE
## A.new                               TRUE            FALSE       FALSE
## A.year                              TRUE            FALSE       FALSE
## S.year                              TRUE            FALSE       FALSE
## H.day                               TRUE            FALSE       FALSE
## S.can                               TRUE            FALSE       FALSE
## A.can                               TRUE            FALSE       FALSE
## S.report                            TRUE            FALSE       FALSE
## A.will                              TRUE            FALSE       FALSE
## S.will                              TRUE            FALSE       FALSE
## A.week                              TRUE            FALSE       FALSE
## A.report                            TRUE            FALSE       FALSE
## A.said                              TRUE             TRUE       FALSE
## S.state                             TRUE             TRUE       FALSE
## A.state                             TRUE             TRUE       FALSE
## S.week                              TRUE            FALSE       FALSE
## S.said                              TRUE             TRUE       FALSE
## S.newyork                           TRUE            FALSE       FALSE
## A.newyork                           TRUE            FALSE       FALSE
## S.compani                           TRUE            FALSE       FALSE
## A.compani                           TRUE            FALSE       FALSE
## A.take                              TRUE            FALSE       FALSE
## S.take                              TRUE            FALSE       FALSE
## A.make                              TRUE            FALSE       FALSE
## S.make                              TRUE            FALSE       FALSE
## A.show                              TRUE            FALSE       FALSE
## S.presid                            TRUE            FALSE       FALSE
## A.share                             TRUE            FALSE       FALSE
## S.show                              TRUE            FALSE       FALSE
## H.has.ebola                         TRUE            FALSE       FALSE
## S.share                             TRUE            FALSE       FALSE
## A.presid                            TRUE            FALSE       FALSE
## S.day                               TRUE            FALSE       FALSE
## A.day                               TRUE            FALSE       FALSE
## H.new                               TRUE            FALSE       FALSE
## H.report                            TRUE            FALSE       FALSE
## A.first                             TRUE            FALSE       FALSE
## S.intern                            TRUE            FALSE       FALSE
## A.intern                            TRUE            FALSE       FALSE
## S.first                             TRUE            FALSE       FALSE
## H.week                              TRUE            FALSE       FALSE
## H.newyork                           TRUE            FALSE       FALSE
## H.X2014                             TRUE            FALSE       FALSE
## H.today                             TRUE            FALSE       FALSE
## A.articl                            TRUE            FALSE       FALSE
## S.articl                            TRUE            FALSE       FALSE
## H.fashion                           TRUE            FALSE       FALSE
## A.fashion                           TRUE            FALSE       FALSE
## S.fashion                           TRUE            FALSE       FALSE
## .rnorm                              TRUE            FALSE       FALSE
## A.has.http                         FALSE            FALSE       FALSE
## A.num.chars                           NA            FALSE       FALSE
## A.num.words                           NA            FALSE       FALSE
## A.num.words.unq                       NA            FALSE       FALSE
## H.daili                            FALSE            FALSE       FALSE
## H.has.http                         FALSE               NA       FALSE
## H.num.chars                           NA            FALSE       FALSE
## H.num.words                           NA            FALSE       FALSE
## H.num.words.unq                       NA            FALSE       FALSE
## H.X2015                            FALSE            FALSE       FALSE
## Popular                               NA            FALSE        TRUE
## Popular.fctr                          NA               NA          NA
## PubDate.month.fctr                    NA            FALSE       FALSE
## PubDate.year                          NA               NA       FALSE
## S.has.http                         FALSE               NA       FALSE
## S.num.chars                           NA            FALSE       FALSE
## S.num.words                           NA            FALSE       FALSE
## S.num.words.unq                       NA            FALSE       FALSE
## UniqueID                              NA            FALSE       FALSE
## WordCount                             NA            FALSE       FALSE
##                        id_var rsp_var Conditional.X.no.rnorm.rf.importance
## WordCount.log              NA      NA                         100.00000000
## SubsectionName.nb.fctr     NA      NA                          75.94943243
## PubDate.hour               NA      NA                          51.77902587
## NewsDesk.nb.fctr           NA      NA                          35.88460826
## PubDate.minute             NA      NA                          31.83753233
## H.num.chars.log            NA      NA                          31.82815922
## SectionName.nb.fctr        NA      NA                          30.15727347
## A.num.chars.log            NA      NA                          28.73445312
## S.num.chars.log            NA      NA                          28.64600145
## PubDate.second             NA      NA                          24.97368401
## H.num.words.log            NA      NA                          11.93904244
## H.num.words.unq.log        NA      NA                          11.77933121
## Headline.pfx.fctr          NA      NA                          11.58021109
## S.num.words.unq.log        NA      NA                          11.43976670
## A.num.words.unq.log        NA      NA                          11.29529950
## A.num.words.log            NA      NA                          10.96589442
## S.num.words.log            NA      NA                          10.88606595
## H.is.question              NA      NA                           7.72406831
## PubDate.wkday.fctr         NA      NA                           5.11743562
## PubDate.date.fctr          NA      NA                           3.42556063
## PubDate.apm.fctr           NA      NA                           3.41399600
## S.one                      NA      NA                           2.62187190
## S.time                     NA      NA                           2.58414555
## A.time                     NA      NA                           2.34815137
## A.one                      NA      NA                           2.33323297
## S.new                      NA      NA                           2.13899035
## A.new                      NA      NA                           2.09801769
## A.year                     NA      NA                           1.68387596
## S.year                     NA      NA                           1.68337992
## H.day                      NA      NA                           1.63512203
## S.can                      NA      NA                           1.61371956
## A.can                      NA      NA                           1.44862975
## S.report                   NA      NA                           1.43918550
## A.will                     NA      NA                           1.38730172
## S.will                     NA      NA                           1.36680498
## A.week                     NA      NA                           1.34663990
## A.report                   NA      NA                           1.33186398
## A.said                     NA      NA                           1.27009593
## S.state                    NA      NA                           1.26959882
## A.state                    NA      NA                           1.26682812
## S.week                     NA      NA                           1.21473174
## S.said                     NA      NA                           1.21457535
## S.newyork                  NA      NA                           1.19464000
## A.newyork                  NA      NA                           1.17716672
## S.compani                  NA      NA                           1.13502270
## A.compani                  NA      NA                           1.13159684
## A.take                     NA      NA                           1.10547591
## S.take                     NA      NA                           1.04952996
## A.make                     NA      NA                           0.91890089
## S.make                     NA      NA                           0.90600935
## A.show                     NA      NA                           0.89981034
## S.presid                   NA      NA                           0.89652394
## A.share                    NA      NA                           0.88055003
## S.show                     NA      NA                           0.87585858
## H.has.ebola                NA      NA                           0.87326832
## S.share                    NA      NA                           0.86634396
## A.presid                   NA      NA                           0.79566039
## S.day                      NA      NA                           0.70412014
## A.day                      NA      NA                           0.66104799
## H.new                      NA      NA                           0.59499600
## H.report                   NA      NA                           0.50235040
## A.first                    NA      NA                           0.49412212
## S.intern                   NA      NA                           0.49060159
## A.intern                   NA      NA                           0.40615745
## S.first                    NA      NA                           0.39434669
## H.week                     NA      NA                           0.36306631
## H.newyork                  NA      NA                           0.34885977
## H.X2014                    NA      NA                           0.30421277
## H.today                    NA      NA                           0.27455637
## A.articl                   NA      NA                           0.27058395
## S.articl                   NA      NA                           0.20426541
## H.fashion                  NA      NA                           0.11912548
## A.fashion                  NA      NA                           0.10168751
## S.fashion                  NA      NA                           0.09431399
## .rnorm                     NA      NA                                   NA
## A.has.http                 NA      NA                                   NA
## A.num.chars                NA      NA                                   NA
## A.num.words                NA      NA                                   NA
## A.num.words.unq            NA      NA                                   NA
## H.daili                    NA      NA                                   NA
## H.has.http                 NA      NA                                   NA
## H.num.chars                NA      NA                                   NA
## H.num.words                NA      NA                                   NA
## H.num.words.unq            NA      NA                                   NA
## H.X2015                    NA      NA                                   NA
## Popular                    NA      NA                                   NA
## Popular.fctr               NA    TRUE                                   NA
## PubDate.month.fctr         NA      NA                                   NA
## PubDate.year               NA      NA                                   NA
## S.has.http                 NA      NA                                   NA
## S.num.chars                NA      NA                                   NA
## S.num.words                NA      NA                                   NA
## S.num.words.unq            NA      NA                                   NA
## UniqueID                 TRUE      NA                                   NA
## WordCount                  NA      NA                                   NA
##                        Final.rf.importance
## WordCount.log                 100.00000000
## SubsectionName.nb.fctr         75.94943243
## PubDate.hour                   51.77902587
## NewsDesk.nb.fctr               35.88460826
## PubDate.minute                 31.83753233
## H.num.chars.log                31.82815922
## SectionName.nb.fctr            30.15727347
## A.num.chars.log                28.73445312
## S.num.chars.log                28.64600145
## PubDate.second                 24.97368401
## H.num.words.log                11.93904244
## H.num.words.unq.log            11.77933121
## Headline.pfx.fctr              11.58021109
## S.num.words.unq.log            11.43976670
## A.num.words.unq.log            11.29529950
## A.num.words.log                10.96589442
## S.num.words.log                10.88606595
## H.is.question                   7.72406831
## PubDate.wkday.fctr              5.11743562
## PubDate.date.fctr               3.42556063
## PubDate.apm.fctr                3.41399600
## S.one                           2.62187190
## S.time                          2.58414555
## A.time                          2.34815137
## A.one                           2.33323297
## S.new                           2.13899035
## A.new                           2.09801769
## A.year                          1.68387596
## S.year                          1.68337992
## H.day                           1.63512203
## S.can                           1.61371956
## A.can                           1.44862975
## S.report                        1.43918550
## A.will                          1.38730172
## S.will                          1.36680498
## A.week                          1.34663990
## A.report                        1.33186398
## A.said                          1.27009593
## S.state                         1.26959882
## A.state                         1.26682812
## S.week                          1.21473174
## S.said                          1.21457535
## S.newyork                       1.19464000
## A.newyork                       1.17716672
## S.compani                       1.13502270
## A.compani                       1.13159684
## A.take                          1.10547591
## S.take                          1.04952996
## A.make                          0.91890089
## S.make                          0.90600935
## A.show                          0.89981034
## S.presid                        0.89652394
## A.share                         0.88055003
## S.show                          0.87585858
## H.has.ebola                     0.87326832
## S.share                         0.86634396
## A.presid                        0.79566039
## S.day                           0.70412014
## A.day                           0.66104799
## H.new                           0.59499600
## H.report                        0.50235040
## A.first                         0.49412212
## S.intern                        0.49060159
## A.intern                        0.40615745
## S.first                         0.39434669
## H.week                          0.36306631
## H.newyork                       0.34885977
## H.X2014                         0.30421277
## H.today                         0.27455637
## A.articl                        0.27058395
## S.articl                        0.20426541
## H.fashion                       0.11912548
## A.fashion                       0.10168751
## S.fashion                       0.09431399
## .rnorm                                  NA
## A.has.http                              NA
## A.num.chars                             NA
## A.num.words                             NA
## A.num.words.unq                         NA
## H.daili                                 NA
## H.has.http                              NA
## H.num.chars                             NA
## H.num.words                             NA
## H.num.words.unq                         NA
## H.X2015                                 NA
## Popular                                 NA
## Popular.fctr                            NA
## PubDate.month.fctr                      NA
## PubDate.year                            NA
## S.has.http                              NA
## S.num.chars                             NA
## S.num.words                             NA
## S.num.words.unq                         NA
## UniqueID                                NA
## WordCount                               NA
```

```r
glb_analytics_diag_plots(obs_df=glb_trnent_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnent_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 74
```

![](NYTBlogs_Metro_files/figure-html/fit.data.training_1-1.png) ![](NYTBlogs_Metro_files/figure-html/fit.data.training_1-2.png) ![](NYTBlogs_Metro_files/figure-html/fit.data.training_1-3.png) ![](NYTBlogs_Metro_files/figure-html/fit.data.training_1-4.png) ![](NYTBlogs_Metro_files/figure-html/fit.data.training_1-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## 1507     1507            N                               0.00
## 6370     6370            Y                               0.75
##      Popular.fctr.predict.Final.rf Popular.fctr.predict.Final.rf.accurate
## 1507                             N                                   TRUE
## 6370                             Y                                   TRUE
##      Popular.fctr.predict.Final.rf.error .label
## 1507                                   0   1507
## 6370                                   0   6370
## [1] "Inaccurate: "
## [1] UniqueID                              
## [2] Popular.fctr                          
## [3] Popular.fctr.predict.Final.rf.prob    
## [4] Popular.fctr.predict.Final.rf         
## [5] Popular.fctr.predict.Final.rf.accurate
## [6] Popular.fctr.predict.Final.rf.error   
## <0 rows> (or 0-length row.names)
```

![](NYTBlogs_Metro_files/figure-html/fit.data.training_1-6.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

print(glb_trnent_df[glb_trnent_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_trnent_df), value=TRUE)])
```

```
##      Popular.fctr Popular.fctr.predict.Final.rf.prob
## 92              Y                              0.636
## 693             Y                              0.646
## 4020            Y                              0.684
## 4721            Y                              0.660
##      Popular.fctr.predict.Final.rf
## 92                               Y
## 693                              Y
## 4020                             Y
## 4721                             Y
```

```r
sav_entity_df <- glb_entity_df
print(setdiff(names(glb_trnent_df), names(glb_entity_df)))
```

```
## [1] "Popular.fctr.predict.Final.rf.prob"
## [2] "Popular.fctr.predict.Final.rf"
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

![](NYTBlogs_Metro_files/figure-html/fit.data.training_1-7.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor      bgn      end elapsed
## 14 fit.data.training          7          1  948.527 1036.129  87.602
## 15  predict.data.new          8          0 1036.129       NA      NA
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 74
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

![](NYTBlogs_Metro_files/figure-html/predict.data.new-1.png) 

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

![](NYTBlogs_Metro_files/figure-html/predict.data.new-2.png) 

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

![](NYTBlogs_Metro_files/figure-html/predict.data.new-3.png) 

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

![](NYTBlogs_Metro_files/figure-html/predict.data.new-4.png) 

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

![](NYTBlogs_Metro_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## 6753     6753         <NA>                              0.528
## 7309     7309         <NA>                              0.078
##      Popular.fctr.predict.Final.rf Popular.fctr.predict.Final.rf.accurate
## 6753                             Y                                     NA
## 7309                             N                                     NA
##      Popular.fctr.predict.Final.rf.error .label
## 6753                                   0   6753
## 7309                                   0   7309
## [1] "Inaccurate: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## NA         NA         <NA>                                 NA
## NA.1       NA         <NA>                                 NA
## NA.2       NA         <NA>                                 NA
## NA.3       NA         <NA>                                 NA
## NA.4       NA         <NA>                                 NA
## NA.5       NA         <NA>                                 NA
##      Popular.fctr.predict.Final.rf Popular.fctr.predict.Final.rf.accurate
## NA                            <NA>                                     NA
## NA.1                          <NA>                                     NA
## NA.2                          <NA>                                     NA
## NA.3                          <NA>                                     NA
## NA.4                          <NA>                                     NA
## NA.5                          <NA>                                     NA
##      Popular.fctr.predict.Final.rf.error
## NA                                    NA
## NA.1                                  NA
## NA.2                                  NA
## NA.3                                  NA
## NA.4                                  NA
## NA.5                                  NA
##         UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## NA.293        NA         <NA>                                 NA
## NA.959        NA         <NA>                                 NA
## NA.1490       NA         <NA>                                 NA
## NA.1593       NA         <NA>                                 NA
## NA.1740       NA         <NA>                                 NA
## NA.1832       NA         <NA>                                 NA
##         Popular.fctr.predict.Final.rf
## NA.293                           <NA>
## NA.959                           <NA>
## NA.1490                          <NA>
## NA.1593                          <NA>
## NA.1740                          <NA>
## NA.1832                          <NA>
##         Popular.fctr.predict.Final.rf.accurate
## NA.293                                      NA
## NA.959                                      NA
## NA.1490                                     NA
## NA.1593                                     NA
## NA.1740                                     NA
## NA.1832                                     NA
##         Popular.fctr.predict.Final.rf.error
## NA.293                                   NA
## NA.959                                   NA
## NA.1490                                  NA
## NA.1593                                  NA
## NA.1740                                  NA
## NA.1832                                  NA
##         UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## NA.1864       NA         <NA>                                 NA
## NA.1865       NA         <NA>                                 NA
## NA.1866       NA         <NA>                                 NA
## NA.1867       NA         <NA>                                 NA
## NA.1868       NA         <NA>                                 NA
## NA.1869       NA         <NA>                                 NA
##         Popular.fctr.predict.Final.rf
## NA.1864                          <NA>
## NA.1865                          <NA>
## NA.1866                          <NA>
## NA.1867                          <NA>
## NA.1868                          <NA>
## NA.1869                          <NA>
##         Popular.fctr.predict.Final.rf.accurate
## NA.1864                                     NA
## NA.1865                                     NA
## NA.1866                                     NA
## NA.1867                                     NA
## NA.1868                                     NA
## NA.1869                                     NA
##         Popular.fctr.predict.Final.rf.error
## NA.1864                                  NA
## NA.1865                                  NA
## NA.1866                                  NA
## NA.1867                                  NA
## NA.1868                                  NA
## NA.1869                                  NA
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_Metro_files/figure-html/predict.data.new-6.png) 

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
## [1] "glb_sel_mdl_id: Conditional.X.no.rnorm.rf"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.rf"
```

```r
print(dim(glb_fitent_df))
```

```
## [1] 4475  107
```

```r
print(dsp_models_df)
```

```
##                        model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 11    Conditional.X.no.rnorm.rf        0.9095771   0.9337691     0.6812840
## 9             Conditional.X.glm        0.9047156   0.8196605     0.6514283
## 7       Interact.High.cor.Y.glm        0.8935343   0.9062453     0.6082058
## 8                 Low.cor.X.glm        0.8779776   0.7838596     0.5644409
## 10 Conditional.X.no.rnorm.rpart        0.8740885   0.6862731     0.4517912
## 1             MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3          Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 5               Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 4     Max.cor.Y.cv.0.cp.0.rpart        0.7715119   0.6504263     0.2413609
## 6                 Max.cor.Y.glm        0.6932426   0.7342331     0.2215049
## 2       Random.myrandom_classfr        0.1672338   0.4821958     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 11          NA                    0.4
## 9    30803.018                    0.9
## 7     2700.718                    0.4
## 8    39061.058                    0.9
## 10          NA                    0.7
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 4           NA                    0.2
## 6     3674.923                    0.2
## 2           NA                    0.1
```

```r
print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
```

```
## [1] "Conditional.X.no.rnorm.rf OOB confusion matrix & accuracy: "
```

```r
print(t(confusionMatrix(glb_OOBent_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                        glb_OOBent_df[, glb_rsp_var])$table))
```

```
##          Prediction
## Reference    N    Y
##         N 1612  101
##         Y   85  259
```

```r
# nOOB_ctgry_df <- mycreate_sqlxtab_df(glb_OOBent_df, c("NewsDesk.nb"))
tmp_OOBent_df <- glb_OOBent_df[, c("NewsDesk.nb", predct_accurate_var_name)]
names(tmp_OOBent_df)[2] <- "accurate.OOB"
aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBent_df, names(tmp_OOBent_df)) 
aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                        .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                        max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
```

```
## [1] "NewsDesk.nb" ".n.OOB"
```

```r
glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
```

```
##               NewsDesk.nb .n.OOB .n.Tst .freqRatio.Tst .freqRatio.OOB
## 3                Business    513    500   0.2673796791   0.2493923189
## 11               myMisc::    350    294   0.1572192513   0.1701507049
## 14                   OpEd    217    205   0.1096256684   0.1054934370
## 19                 Styles     75     76   0.0406417112   0.0364608653
## 17                Science     66     57   0.0304812834   0.0320855615
## 4                 Culture    203    243   0.1299465241   0.0986874088
## 9                   Metro     57     66   0.0352941176   0.0277102577
## 7                 Foreign    119    102   0.0545454545   0.0578512397
## 16      Readers Respond::      2      4   0.0021390374   0.0009722897
## 24                 TStyle    239    107   0.0572192513   0.1161886242
## 12           myMultimedia     38     53   0.0283422460   0.0184735051
## 13               myTech::      4      1   0.0005347594   0.0019445795
## 1          19[0-9][0-9]::      2      4   0.0021390374   0.0009722897
## 2  6 Q's About the News::     22     15   0.0080213904   0.0106951872
## 5     Daily Clip Report::     18     22   0.0117647059   0.0087506077
## 6           First Draft::     18     14   0.0074866310   0.0087506077
## 8                Magazine     10      3   0.0016042781   0.0048614487
## 10               myFood::      1     NA             NA   0.0004861449
## 15 Quiz(.*)([?=|]|[?=:]::      3      4   0.0021390374   0.0014584346
## 18                 Sports      1     NA             NA   0.0004861449
## 20        Test Yourself::     21     17   0.0090909091   0.0102090423
## 21       The Daily Gift::      1      2   0.0010695187   0.0004861449
## 22    Today in Politics::     14     21   0.0112299465   0.0068060282
## 23                 Travel     34     31   0.0165775401   0.0165289256
## 25             Verbatim::     11     12   0.0064171123   0.0053475936
## 26      Word of the Day::     18     17   0.0090909091   0.0087506077
##    accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 3                  56               457        0.8908382
## 11                 36               314        0.8971429
## 14                 36               181        0.8341014
## 19                 21                54        0.7200000
## 17                 16                50        0.7575758
## 4                   8               195        0.9605911
## 9                   5                52        0.9122807
## 7                   2               117        0.9831933
## 16                  2                 0        0.0000000
## 24                  2               237        0.9916318
## 12                  1                37        0.9736842
## 13                  1                 3        0.7500000
## 1                   0                 2        1.0000000
## 2                   0                22        1.0000000
## 5                   0                18        1.0000000
## 6                   0                18        1.0000000
## 8                   0                10        1.0000000
## 10                  0                 1        1.0000000
## 15                  0                 3        1.0000000
## 18                  0                 1        1.0000000
## 20                  0                21        1.0000000
## 21                  0                 1        1.0000000
## 22                  0                14        1.0000000
## 23                  0                34        1.0000000
## 25                  0                11        1.0000000
## 26                  0                18        1.0000000
```

```r
# OOB_ctgry_df <- mycreate_sqlxtab_df(glb_OOBent_df, c("NewsDesk.nb"))
# print(sum(glb_OOBent_df[, predct_accurate_var_name]) / nrow(glb_OOBent_df))
# print(tapply(glb_OOBent_df[, predct_accurate_var_name], 
#              glb_OOBent_df[, "NewsDesk.nb"], sum) / 
#       tapply(glb_OOBent_df[, predct_accurate_var_name], 
#              glb_OOBent_df[, "NewsDesk.nb"], length))
# acc_df <- as.data.frame(
#       tapply(glb_OOBent_df[, predct_accurate_var_name], 
#              glb_OOBent_df[, "NewsDesk.nb"], sum) / 
#       tapply(glb_OOBent_df[, predct_accurate_var_name], 
#              glb_OOBent_df[, "NewsDesk.nb"], length))
# names(acc_df) <- c("max.accuracy.OOB")
# print(orderBy(~-max.accuracy.OOB, acc_df))

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
#dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="Culture")
dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="Foreign")
```

```
## [1] "Conditional.X.no.rnorm.rf OOB::NewsDesk.nb=Foreign confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 117   1
##         Y   1   0
## [1] 0.9831933
## [1] "Conditional.X.no.rnorm.rf OOB::NewsDesk.nb=Foreign errors: "
##      Headline.pfx
## 395      myMisc::
## 4721     myMisc::
##                                                               Headline
## 395                   Wealthy Chinese Travelers Lining Up to Blast Off
## 4721 Hong Kong Politician Likens Protesters to African-American Slaves
##      Popular
## 395        0
## 4721       1
```

```r
dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="Metro")
```

```
## [1] "Conditional.X.no.rnorm.rf OOB::NewsDesk.nb=Metro confusion matrix & accuracy: "
##          Prediction
## Reference  N  Y
##         N 52  0
##         Y  5  0
## [1] 0.9122807
## [1] "Conditional.X.no.rnorm.rf OOB::NewsDesk.nb=Metro errors: "
##          Headline.pfx
## 828  New York Today::
## 2348         myMisc::
## 4020         myMisc::
## 4178         myMisc::
## 5461         myMisc::
##                                                                  Headline
## 828                                  New York Today: The City Remembers  
## 2348 Woman With 3 Jobs Died From Gas Fumes While Napping, Authorities Say
## 4020      Video: News Conference About Ebola Patient at Bellevue Hospital
## 4178                        Friday Updates on New York's First Ebola Case
## 5461                    Back for More: Return of the Pulp Fiction Contest
##      Popular
## 828        1
## 2348       1
## 4020       1
## 4178       1
## 5461       1
```

```r
#dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="Science")
# dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="Styles")
# dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="TStyle")

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
##      Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 92              Y                                               0.000
## 693             Y                                               0.006
## 4020            Y                                               0.028
## 4721            Y                                               0.002
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 92                                                N
## 693                                               N
## 4020                                              N
## 4721                                              N
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 92                                                     FALSE
## 693                                                    FALSE
## 4020                                                   FALSE
## 4721                                                   FALSE
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_txt_vars])
```

```
##                                                                   Headline
## 92   Moelis & Co. Hires Cantor, Ex-House Majority Leader, as Vice Chairman
## 693                                Do You Hire Employees on a Trial Basis?
## 4020       Video: News Conference About Ebola Patient at Bellevue Hospital
## 4721     Hong Kong Politician Likens Protesters to African-American Slaves
##                                                                                                                                                                                                                         Snippet
## 92                                                                            Eric Cantor, who suffered a surprising electoral defeat this year, will be joining Moelis & Company as vice chairman and a director on its board.
## 693                                                                                                                Do you think job candidates are willing to work for three months on a contract before being hired full-time?
## 4020                                                                                                                    A news conference about Dr. Craig Spencer at Bellevue Hospital who tested positive for the Ebola virus.
## 4721 A prominent businesswoman and politician has come under fire for saying, erroneously, that black Americans did not get voting rights for 107 years after the countrys slaves were freed, so Hong Kongers should also wait.
##                                                                                                                                                                                                                        Abstract
## 92                                                                            Eric Cantor, who suffered a surprising electoral defeat this year, will be joining Moelis & Company as vice chairman and a director on its board.
## 693                                                                                                                Do you think job candidates are willing to work for three months on a contract before being hired full-time?
## 4020                                                                                                                    A news conference about Dr. Craig Spencer at Bellevue Hospital who tested positive for the Ebola virus.
## 4721 A prominent businesswoman and politician has come under fire for saying, erroneously, that black Americans did not get voting rights for 107 years after the countrys slaves were freed, so Hong Kongers should also wait.
```

```r
print(dsp_vctr <- colSums(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    setdiff(grep("[HSA].", names(glb_OOBent_df), value=TRUE),
                            union(myfind_chr_cols_df(glb_OOBent_df),
                grep(".fctr", names(glb_OOBent_df), fixed=TRUE, value=TRUE)))]))
```

```
##             H.X2014             H.X2015             H.daili 
##            0.000000            0.000000            0.000000 
##               H.day           H.fashion               H.new 
##            0.000000            0.000000            0.000000 
##           H.newyork            H.report             H.today 
##            0.000000            0.000000            0.000000 
##              H.week          H.has.http         H.has.ebola 
##            0.000000            0.000000            1.000000 
##       H.is.question         H.num.chars         H.num.words 
##            1.000000          236.000000           26.000000 
##     H.num.words.unq     H.num.chars.log     H.num.words.log 
##           26.000000           16.285913            7.965546 
## H.num.words.unq.log            S.articl               S.can 
##            7.965546            0.000000            0.000000 
##           S.compani               S.day           S.fashion 
##            1.000000            0.000000            0.000000 
##             S.first            S.intern              S.make 
##            0.000000            0.000000            0.000000 
##               S.new           S.newyork               S.one 
##            0.000000            0.000000            0.000000 
##            S.presid            S.report              S.said 
##            0.000000            0.000000            0.000000 
##             S.share              S.show             S.state 
##            0.000000            0.000000            0.000000 
##              S.take              S.time              S.week 
##            0.000000            0.000000            0.000000 
##              S.will              S.year          S.has.http 
##            2.000000            2.000000            0.000000 
##         S.num.chars         S.num.words     S.num.words.unq 
##          574.000000           56.000000           56.000000 
##     S.num.chars.log     S.num.words.log S.num.words.unq.log 
##           19.708417           10.659422           10.659422 
##            A.articl               A.can           A.compani 
##            0.000000            0.000000            1.000000 
##               A.day           A.fashion             A.first 
##            0.000000            0.000000            0.000000 
##            A.intern              A.make               A.new 
##            0.000000            0.000000            0.000000 
##           A.newyork               A.one            A.presid 
##            0.000000            0.000000            0.000000 
##            A.report              A.said             A.share 
##            0.000000            0.000000            0.000000 
##              A.show             A.state              A.take 
##            0.000000            0.000000            0.000000 
##              A.time              A.week              A.will 
##            0.000000            0.000000            2.000000 
##              A.year          A.has.http         A.num.chars 
##            2.000000            0.000000          574.000000 
##         A.num.words     A.num.words.unq     A.num.chars.log 
##           56.000000           56.000000           19.708417 
##     A.num.words.log A.num.words.unq.log 
##           10.659422           10.659422
```

```r
dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBent_df[glb_OOBent_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBent_df), value=TRUE)])
    print(glb_newent_df[glb_newent_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newent_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newent_df[glb_newent_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA].", names(glb_newent_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newent_df),
                    grep(".fctr", names(glb_newent_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newent_df[glb_newent_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newent_df))])
}
dsp_hdlpfx_results("Ask Well::")
```

```
## [1] "Ask Well::"
##      Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 1053            Y                                               0.642
## 3228            Y                                               0.536
## 3437            Y                                               0.356
## 3602            N                                               0.594
## 4134            N                                               0.518
## 4217            N                                               0.532
## 4387            Y                                               0.392
## 5244            N                                               0.702
## 5658            Y                                               0.458
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 1053                                              Y
## 3228                                              Y
## 3437                                              N
## 3602                                              Y
## 4134                                              Y
## 4217                                              Y
## 4387                                              N
## 5244                                              Y
## 5658                                              Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 1053                                                    TRUE
## 3228                                                    TRUE
## 3437                                                   FALSE
## 3602                                                   FALSE
## 4134                                                   FALSE
## 4217                                                   FALSE
## 4387                                                   FALSE
## 5244                                                   FALSE
## 5658                                                    TRUE
##      Popular.fctr Popular.fctr.predict.Final.rf.prob
## 6558         <NA>                              0.356
## 7535         <NA>                              0.546
## 7864         <NA>                              0.520
##      Popular.fctr.predict.Final.rf
## 6558                             N
## 7535                             Y
## 7864                             Y
##             H.X2014             H.X2015             H.daili 
##            0.000000            0.000000            0.000000 
##               H.day           H.fashion               H.new 
##            0.000000            0.000000            0.000000 
##           H.newyork            H.report             H.today 
##            0.000000            0.000000            0.000000 
##              H.week          H.has.http         H.has.ebola 
##            0.000000            0.000000            0.000000 
##       H.is.question         H.num.chars         H.num.words 
##            0.000000          108.000000           17.000000 
##     H.num.words.unq     H.num.chars.log     H.num.words.log 
##           16.000000           10.653204            5.634790 
## H.num.words.unq.log            S.articl               S.can 
##            5.480639            0.000000            0.000000 
##           S.compani               S.day           S.fashion 
##            0.000000            0.000000            0.000000 
##             S.first            S.intern              S.make 
##            0.000000            0.000000            1.000000 
##               S.new           S.newyork               S.one 
##            0.000000            0.000000            0.000000 
##            S.presid            S.report              S.said 
##            0.000000            0.000000            0.000000 
##             S.share              S.show             S.state 
##            0.000000            0.000000            0.000000 
##              S.take              S.time              S.week 
##            0.000000            0.000000            0.000000 
##              S.will              S.year          S.has.http 
##            0.000000            0.000000            0.000000 
##         S.num.chars         S.num.words     S.num.words.unq 
##          193.000000           22.000000           20.000000 
##     S.num.chars.log     S.num.words.log S.num.words.unq.log 
##           12.507177            6.340359            6.089045 
##            A.articl               A.can           A.compani 
##            0.000000            0.000000            0.000000 
##               A.day           A.fashion             A.first 
##            0.000000            0.000000            0.000000 
##            A.intern              A.make               A.new 
##            0.000000            1.000000            0.000000 
##           A.newyork               A.one            A.presid 
##            0.000000            0.000000            0.000000 
##            A.report              A.said             A.share 
##            0.000000            0.000000            0.000000 
##              A.show             A.state              A.take 
##            0.000000            0.000000            0.000000 
##              A.time              A.week              A.will 
##            0.000000            0.000000            0.000000 
##              A.year          A.has.http         A.num.chars 
##            0.000000            0.000000          193.000000 
##         A.num.words     A.num.words.unq     A.num.chars.log 
##           22.000000           20.000000           12.507177 
##     A.num.words.log A.num.words.unq.log 
##            6.340359            6.089045 
##         H.num.chars         H.num.words     H.num.words.unq 
##          108.000000           17.000000           16.000000 
##     H.num.chars.log     H.num.words.log H.num.words.unq.log 
##           10.653204            5.634790            5.480639 
##              S.make         S.num.chars         S.num.words 
##            1.000000          193.000000           22.000000 
##     S.num.words.unq     S.num.chars.log     S.num.words.log 
##           20.000000           12.507177            6.340359 
## S.num.words.unq.log              A.make         A.num.chars 
##            6.089045            1.000000          193.000000 
##         A.num.words     A.num.words.unq     A.num.chars.log 
##           22.000000           20.000000           12.507177 
##     A.num.words.log A.num.words.unq.log 
##            6.340359            6.089045 
##      H.num.chars H.num.words H.num.words.unq H.num.chars.log
## 6558          51           7               7        3.951244
## 7535          21           4               4        3.091042
## 7864          36           6               5        3.610918
##      H.num.words.log H.num.words.unq.log S.make S.num.chars S.num.words
## 6558        2.079442            2.079442      0          64           8
## 7535        1.609438            1.609438      1          53           6
## 7864        1.945910            1.791759      0          76           8
##      S.num.words.unq S.num.chars.log S.num.words.log S.num.words.unq.log
## 6558               8        4.174387        2.197225            2.197225
## 7535               6        3.988984        1.945910            1.945910
## 7864               6        4.343805        2.197225            1.945910
##      A.make A.num.chars A.num.words A.num.words.unq A.num.chars.log
## 6558      0          64           8               8        4.174387
## 7535      1          53           6               6        3.988984
## 7864      0          76           8               6        4.343805
##      A.num.words.log A.num.words.unq.log NewsDesk SectionName
## 6558        2.197225            2.197225  Science      Health
## 7535        1.945910            1.945910  Science      Health
## 7864        2.197225            1.945910  Science      Health
##      SubsectionName                                            Headline
## 6558                Ask Well: Eating Fat to Boost Vitamin D and Calcium
## 7535                                              Ask Well: Noisy Knees
## 7864                               Ask Well: Wild Fish vs. Farmed Fish 
##                                                                           Snippet
## 6558             A reader asks: Must you eat fat to absorb calcium and vitamin D?
## 7535                        A reader asks: Why do my knees make a cracking sound?
## 7864 A reader asks: Is eating farm-raised fish better than eating no fish at all?
##                                                                          Abstract
## 6558             A reader asks: Must you eat fat to absorb calcium and vitamin D?
## 7535                        A reader asks: Why do my knees make a cracking sound?
## 7864 A reader asks: Is eating farm-raised fish better than eating no fish at all?
##                  PubDate .src Headline.pfx NewsDesk.nb SectionName.nb
## 6558 2014-12-01 16:27:30 Test   Ask Well::     Science         Health
## 7535 2014-12-15 18:53:06 Test   Ask Well::     Science         Health
## 7864 2014-12-18 13:42:52 Test   Ask Well::     Science         Health
##      SubsectionName.nb
## 6558   Science::Health
## 7535   Science::Health
## 7864   Science::Health
```

```r
print("myMisc::|OpEd|blank|blank|1:")
```

```
## [1] "myMisc::|OpEd|blank|blank|1:"
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% c(6446), 
                    grep(glb_rsp_var, names(glb_OOBent_df), value=TRUE)])
```

```
##      Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 6446            Y                                                0.76
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 6446                                              Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 6446                                                    TRUE
```

```r
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
print(subset(glb_feats_df, !is.na(importance))[,
    c("is.ConditionalX.y", 
      grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
```

```
##                        is.ConditionalX.y   importance
## WordCount.log                       TRUE 100.00000000
## SubsectionName.nb.fctr              TRUE  75.94943243
## PubDate.hour                        TRUE  51.77902587
## NewsDesk.nb.fctr                    TRUE  35.88460826
## PubDate.minute                      TRUE  31.83753233
## H.num.chars.log                     TRUE  31.82815922
## SectionName.nb.fctr                 TRUE  30.15727347
## A.num.chars.log                     TRUE  28.73445312
## S.num.chars.log                     TRUE  28.64600145
## PubDate.second                      TRUE  24.97368401
## H.num.words.log                     TRUE  11.93904244
## H.num.words.unq.log                 TRUE  11.77933121
## Headline.pfx.fctr                   TRUE  11.58021109
## S.num.words.unq.log                 TRUE  11.43976670
## A.num.words.unq.log                 TRUE  11.29529950
## A.num.words.log                     TRUE  10.96589442
## S.num.words.log                     TRUE  10.88606595
## H.is.question                       TRUE   7.72406831
## PubDate.wkday.fctr                  TRUE   5.11743562
## PubDate.date.fctr                   TRUE   3.42556063
## PubDate.apm.fctr                    TRUE   3.41399600
## S.one                               TRUE   2.62187190
## S.time                              TRUE   2.58414555
## A.time                              TRUE   2.34815137
## A.one                               TRUE   2.33323297
## S.new                               TRUE   2.13899035
## A.new                               TRUE   2.09801769
## A.year                              TRUE   1.68387596
## S.year                              TRUE   1.68337992
## H.day                               TRUE   1.63512203
## S.can                               TRUE   1.61371956
## A.can                               TRUE   1.44862975
## S.report                            TRUE   1.43918550
## A.will                              TRUE   1.38730172
## S.will                              TRUE   1.36680498
## A.week                              TRUE   1.34663990
## A.report                            TRUE   1.33186398
## A.said                              TRUE   1.27009593
## S.state                             TRUE   1.26959882
## A.state                             TRUE   1.26682812
## S.week                              TRUE   1.21473174
## S.said                              TRUE   1.21457535
## S.newyork                           TRUE   1.19464000
## A.newyork                           TRUE   1.17716672
## S.compani                           TRUE   1.13502270
## A.compani                           TRUE   1.13159684
## A.take                              TRUE   1.10547591
## S.take                              TRUE   1.04952996
## A.make                              TRUE   0.91890089
## S.make                              TRUE   0.90600935
## A.show                              TRUE   0.89981034
## S.presid                            TRUE   0.89652394
## A.share                             TRUE   0.88055003
## S.show                              TRUE   0.87585858
## H.has.ebola                         TRUE   0.87326832
## S.share                             TRUE   0.86634396
## A.presid                            TRUE   0.79566039
## S.day                               TRUE   0.70412014
## A.day                               TRUE   0.66104799
## H.new                               TRUE   0.59499600
## H.report                            TRUE   0.50235040
## A.first                             TRUE   0.49412212
## S.intern                            TRUE   0.49060159
## A.intern                            TRUE   0.40615745
## S.first                             TRUE   0.39434669
## H.week                              TRUE   0.36306631
## H.newyork                           TRUE   0.34885977
## H.X2014                             TRUE   0.30421277
## H.today                             TRUE   0.27455637
## A.articl                            TRUE   0.27058395
## S.articl                            TRUE   0.20426541
## H.fashion                           TRUE   0.11912548
## A.fashion                           TRUE   0.10168751
## S.fashion                           TRUE   0.09431399
##                        Conditional.X.no.rnorm.rf.importance
## WordCount.log                                  100.00000000
## SubsectionName.nb.fctr                          75.94943243
## PubDate.hour                                    51.77902587
## NewsDesk.nb.fctr                                35.88460826
## PubDate.minute                                  31.83753233
## H.num.chars.log                                 31.82815922
## SectionName.nb.fctr                             30.15727347
## A.num.chars.log                                 28.73445312
## S.num.chars.log                                 28.64600145
## PubDate.second                                  24.97368401
## H.num.words.log                                 11.93904244
## H.num.words.unq.log                             11.77933121
## Headline.pfx.fctr                               11.58021109
## S.num.words.unq.log                             11.43976670
## A.num.words.unq.log                             11.29529950
## A.num.words.log                                 10.96589442
## S.num.words.log                                 10.88606595
## H.is.question                                    7.72406831
## PubDate.wkday.fctr                               5.11743562
## PubDate.date.fctr                                3.42556063
## PubDate.apm.fctr                                 3.41399600
## S.one                                            2.62187190
## S.time                                           2.58414555
## A.time                                           2.34815137
## A.one                                            2.33323297
## S.new                                            2.13899035
## A.new                                            2.09801769
## A.year                                           1.68387596
## S.year                                           1.68337992
## H.day                                            1.63512203
## S.can                                            1.61371956
## A.can                                            1.44862975
## S.report                                         1.43918550
## A.will                                           1.38730172
## S.will                                           1.36680498
## A.week                                           1.34663990
## A.report                                         1.33186398
## A.said                                           1.27009593
## S.state                                          1.26959882
## A.state                                          1.26682812
## S.week                                           1.21473174
## S.said                                           1.21457535
## S.newyork                                        1.19464000
## A.newyork                                        1.17716672
## S.compani                                        1.13502270
## A.compani                                        1.13159684
## A.take                                           1.10547591
## S.take                                           1.04952996
## A.make                                           0.91890089
## S.make                                           0.90600935
## A.show                                           0.89981034
## S.presid                                         0.89652394
## A.share                                          0.88055003
## S.show                                           0.87585858
## H.has.ebola                                      0.87326832
## S.share                                          0.86634396
## A.presid                                         0.79566039
## S.day                                            0.70412014
## A.day                                            0.66104799
## H.new                                            0.59499600
## H.report                                         0.50235040
## A.first                                          0.49412212
## S.intern                                         0.49060159
## A.intern                                         0.40615745
## S.first                                          0.39434669
## H.week                                           0.36306631
## H.newyork                                        0.34885977
## H.X2014                                          0.30421277
## H.today                                          0.27455637
## A.articl                                         0.27058395
## S.articl                                         0.20426541
## H.fashion                                        0.11912548
## A.fashion                                        0.10168751
## S.fashion                                        0.09431399
##                        Final.rf.importance
## WordCount.log                 100.00000000
## SubsectionName.nb.fctr         75.94943243
## PubDate.hour                   51.77902587
## NewsDesk.nb.fctr               35.88460826
## PubDate.minute                 31.83753233
## H.num.chars.log                31.82815922
## SectionName.nb.fctr            30.15727347
## A.num.chars.log                28.73445312
## S.num.chars.log                28.64600145
## PubDate.second                 24.97368401
## H.num.words.log                11.93904244
## H.num.words.unq.log            11.77933121
## Headline.pfx.fctr              11.58021109
## S.num.words.unq.log            11.43976670
## A.num.words.unq.log            11.29529950
## A.num.words.log                10.96589442
## S.num.words.log                10.88606595
## H.is.question                   7.72406831
## PubDate.wkday.fctr              5.11743562
## PubDate.date.fctr               3.42556063
## PubDate.apm.fctr                3.41399600
## S.one                           2.62187190
## S.time                          2.58414555
## A.time                          2.34815137
## A.one                           2.33323297
## S.new                           2.13899035
## A.new                           2.09801769
## A.year                          1.68387596
## S.year                          1.68337992
## H.day                           1.63512203
## S.can                           1.61371956
## A.can                           1.44862975
## S.report                        1.43918550
## A.will                          1.38730172
## S.will                          1.36680498
## A.week                          1.34663990
## A.report                        1.33186398
## A.said                          1.27009593
## S.state                         1.26959882
## A.state                         1.26682812
## S.week                          1.21473174
## S.said                          1.21457535
## S.newyork                       1.19464000
## A.newyork                       1.17716672
## S.compani                       1.13502270
## A.compani                       1.13159684
## A.take                          1.10547591
## S.take                          1.04952996
## A.make                          0.91890089
## S.make                          0.90600935
## A.show                          0.89981034
## S.presid                        0.89652394
## A.share                         0.88055003
## S.show                          0.87585858
## H.has.ebola                     0.87326832
## S.share                         0.86634396
## A.presid                        0.79566039
## S.day                           0.70412014
## A.day                           0.66104799
## H.new                           0.59499600
## H.report                        0.50235040
## A.first                         0.49412212
## S.intern                        0.49060159
## A.intern                        0.40615745
## S.first                         0.39434669
## H.week                          0.36306631
## H.newyork                       0.34885977
## H.X2014                         0.30421277
## H.today                         0.27455637
## A.articl                        0.27058395
## S.articl                        0.20426541
## H.fashion                       0.11912548
## A.fashion                       0.10168751
## S.fashion                       0.09431399
```

```r
print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
    c("is.ConditionalX.y", 
      grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
```

```
##        is.ConditionalX.y importance Conditional.X.no.rnorm.rf.importance
## .rnorm              TRUE         NA                                   NA
##        Final.rf.importance
## .rnorm                  NA
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
##                   label step_major step_minor      bgn      end elapsed
## 15     predict.data.new          8          0 1036.129 1119.763  83.634
## 16 display.session.info          9          0 1119.764       NA      NA
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
##                      label step_major step_minor      bgn      end elapsed
## 10              fit.models          6          1  208.674  579.295 370.621
## 13       fit.data.training          7          0  665.194  948.527 283.333
## 6         extract.features          3          0   58.784  148.121  89.337
## 14       fit.data.training          7          1  948.527 1036.129  87.602
## 15        predict.data.new          8          0 1036.129 1119.763  83.634
## 12              fit.models          6          3  595.631  665.194  69.563
## 9               fit.models          6          0  159.885  208.673  48.789
## 3             cleanse.data          2          1   28.894   52.664  23.770
## 2             inspect.data          2          0   10.225   28.893  18.668
## 11              fit.models          6          2  579.296  595.631  16.335
## 7          select.features          4          0  148.121  158.480  10.359
## 4      manage.missing.data          2          2   52.664   58.729   6.065
## 8  partition.data.training          5          0  158.481  159.885   1.404
## 1              import.data          1          0    8.904   10.224   1.321
## 5              encode.data          2          3   58.729   58.784   0.055
##    duration
## 10  370.621
## 13  283.333
## 6    89.337
## 14   87.602
## 15   83.634
## 12   69.563
## 9    48.788
## 3    23.770
## 2    18.668
## 11   16.335
## 7    10.359
## 4     6.065
## 8     1.404
## 1     1.320
## 5     0.055
```

```
## [1] "Total Elapsed Time: 1,119.763 secs"
```

![](NYTBlogs_Metro_files/figure-html/display.session.info-1.png) 

```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] tcltk     grid      stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] randomForest_4.6-10 rpart.plot_1.5.2    rpart_4.1-9        
##  [4] ROCR_1.0-7          gplots_2.16.0       caTools_1.17.1     
##  [7] caret_6.0-41        tm_0.6              NLP_0.1-6          
## [10] mice_2.22           lattice_0.20-31     Rcpp_0.11.5        
## [13] plyr_1.8.1          sqldf_0.4-10        RSQLite_1.0.0      
## [16] DBI_0.3.1           gsubfn_0.6-6        proto_0.3-10       
## [19] reshape2_1.4.1      doBy_4.5-13         survival_2.38-1    
## [22] ggplot2_1.0.1      
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6        BradleyTerry2_1.0-6 brglm_0.5-9        
##  [4] car_2.0-25          chron_2.3-45        class_7.3-12       
##  [7] codetools_0.2-11    colorspace_1.2-6    compiler_3.1.3     
## [10] digest_0.6.8        e1071_1.6-4         evaluate_0.5.5     
## [13] foreach_1.4.2       formatR_1.1         gdata_2.13.3       
## [16] gtable_0.1.2        gtools_3.4.1        htmltools_0.2.6    
## [19] iterators_1.0.7     KernSmooth_2.23-14  knitr_1.9          
## [22] labeling_0.3        lme4_1.1-7          MASS_7.3-40        
## [25] Matrix_1.2-0        mgcv_1.8-6          minqa_1.2.4        
## [28] munsell_0.4.2       nlme_3.1-120        nloptr_1.0.4       
## [31] nnet_7.3-9          parallel_3.1.3      pbkrtest_0.4-2     
## [34] quantreg_5.11       RColorBrewer_1.1-2  rmarkdown_0.5.1    
## [37] scales_0.2.4        slam_0.1-32         SparseM_1.6        
## [40] splines_3.1.3       stringr_0.6.2       tools_3.1.3        
## [43] yaml_2.1.13
```
