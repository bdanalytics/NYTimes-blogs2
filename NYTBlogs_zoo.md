# NYTimes:Blogs:: Popular classification:: zoo
bdanalytics  

**  **    
**Date: (Tue) May 12, 2015**    

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
glb_out_pfx <- "NYTBlogs_zoo_"

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

![](NYTBlogs_zoo_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor    bgn end elapsed
## 1 import.data          1          0 11.965  NA      NA
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
##          label step_major step_minor    bgn   end elapsed
## 1  import.data          1          0 11.965 13.07   1.105
## 2 inspect.data          2          0 13.071    NA      NA
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

![](NYTBlogs_zoo_files/figure-html/inspect.data-1.png) 

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

![](NYTBlogs_zoo_files/figure-html/inspect.data-2.png) 

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
# stop("here")
# sav_entity_df <- glb_entity_df
# glb_entity_df <- sav_entity_df
myextract_dates_df <- function(df, vars, rsp_var) {
    keep_feats <- c(NULL)
    for (var in vars) {
        dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df[, rsp_var] <- df[, rsp_var]
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
            c(".year.fctr", ".month.fctr", ".date.fctr", ".wkday.fctr", 
              ".wkend", ".hour.fctr", ".minute.fctr", ".second.fctr"), sep=""))        
    }
    #myprint_df(dates_df)
    return(dates_df[, keep_feats])
}

if (!is.null(glb_date_vars)) {
    glb_entity_df <- cbind(glb_entity_df, 
        myextract_dates_df(df=glb_entity_df, vars=glb_date_vars, rsp_var=glb_rsp_var))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_date_vars)
}
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-3.png) 

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

![](NYTBlogs_zoo_files/figure-html/inspect.data-4.png) 

```r
print(head(glb_entity_df))
```

```
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
##   WordCount             PubDate Popular UniqueID  .src Popular.fctr
## 1       508 2014-09-01 22:00:09       1        1 Train            Y
## 2       285 2014-09-01 21:14:07       0        2 Train            N
## 3      1211 2014-09-01 21:05:36       0        3 Train            N
## 4      1405 2014-09-01 20:43:34       1        4 Train            Y
## 5       181 2014-09-01 18:58:51       1        5 Train            Y
## 6       245 2014-09-01 18:52:22       1        6 Train            Y
##   PubDate.year.fctr PubDate.month.fctr PubDate.date.fctr
## 1              2014                 09          (0.97,7]
## 2              2014                 09          (0.97,7]
## 3              2014                 09          (0.97,7]
## 4              2014                 09          (0.97,7]
## 5              2014                 09          (0.97,7]
## 6              2014                 09          (0.97,7]
##   PubDate.wkday.fctr PubDate.wkend PubDate.hour.fctr PubDate.minute.fctr
## 1                  1             1         (15.3,23]       (-0.059,14.8]
## 2                  1             1         (15.3,23]       (-0.059,14.8]
## 3                  1             1         (15.3,23]       (-0.059,14.8]
## 4                  1             1         (15.3,23]         (29.5,44.2]
## 5                  1             1         (15.3,23]         (44.2,59.1]
## 6                  1             1         (15.3,23]         (44.2,59.1]
##   PubDate.second.fctr
## 1       (-0.059,14.8]
## 2       (-0.059,14.8]
## 3         (29.5,44.2]
## 4         (29.5,44.2]
## 5         (44.2,59.1]
## 6         (14.8,29.5]
```

```r
# Create features that measure the gap between previous timestamp in the data
# pd = as.POSIXlt(glb_entity_df$PubDate )
# z = zoo(as.numeric(pd))
# plot(z)
# n = nrow(glb_entity_df)
# b = zoo(, seq(n))
# 
# last1 = as.numeric(merge(z-lag(z, -1), b, all = TRUE))
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
dsp_numeric_vars_dstrb(setdiff(names(glb_entity_df), 
                                union(myfind_chr_cols_df(glb_entity_df), 
                                      c(glb_rsp_var_raw, glb_rsp_var))))                                      
```

```
## [1] "var: WordCount"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-5.png) 

```
## [1] "var: UniqueID"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-6.png) 

```
## [1] "var: PubDate.year.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-7.png) 

```
## [1] "var: PubDate.month.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-8.png) 

```
## [1] "var: PubDate.date.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-9.png) 

```
## [1] "var: PubDate.wkday.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-10.png) 

```
## [1] "var: PubDate.wkend"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-11.png) 

```
## [1] "var: PubDate.hour.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-12.png) 

```
## [1] "var: PubDate.minute.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-13.png) 

```
## [1] "var: PubDate.second.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-14.png) 

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

```
## [1] "var: PubDate.year.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-15.png) 

```
## [1] "var: PubDate.date.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-16.png) 

```
## [1] "var: PubDate.wkday.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-17.png) 

```
## [1] "var: PubDate.wkend"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-18.png) 

```
## [1] "var: PubDate.hour.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-19.png) 

```
## [1] "var: PubDate.minute.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-20.png) 

```
## [1] "var: PubDate.second.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-21.png) 

```
## [1] "var: WordCount.log"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-22.png) 

```
## [1] "var: .rnorm"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-23.png) 

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
## 2 inspect.data          2          0 13.071 36.823  23.752
## 3 cleanse.data          2          1 36.823     NA      NA
```

### Step `2.1: cleanse data`

```r
dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##           WordCount             Popular            UniqueID 
##                   0                1870                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                1870                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
## [1] "numeric data w/ 0s in : "
##           WordCount             Popular            UniqueID 
##                 109                5439                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                 378                7624 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                 109                   0 
## [1] "numeric data w/ Infs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
## [1] "numeric data w/ NaNs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
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
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                1870                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                 109                   0 
## [1] "numeric data w/ 0s in : "
##           WordCount             Popular            UniqueID 
##                 109                5439                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                 378                7624 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
## [1] "numeric data w/ Infs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
## [1] "numeric data w/ NaNs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
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
    if (grepl("On This Day:", glb_entity_df[row_ix, "Headline"])) 
        return("On This Day::")
    
    if (grepl("Reporter('*)s Notebook", glb_entity_df[row_ix, "Headline"])) 
        return("Reporter's Notebook::")
    
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
    if (grepl("Readers Respond", glb_entity_df[row_ix, "Headline"])) 
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
make_prefix(187) # First char is blank
```

```
## [1] "19[0-9][0-9]::"
```

```r
make_prefix(3882)
```

```
## [1] "Reporter's Notebook::"
```

```r
glb_entity_df$Headline.pfx <- sapply(1:nrow(glb_entity_df), function(row_ix) make_prefix(row_ix))
#myprint_df(glb_entity_df[, c("Headline", "Headline.pfx")])
headline_pfx_df <- mycreate_sqlxtab_df(glb_entity_df[], c("Headline.pfx", glb_rsp_var))
#print(myplot_histogram(headline_pfx_df, ".n"))
print(myplot_hbar(head(headline_pfx_df, 15), "Headline.pfx", ".n", 
                  colorcol_name=glb_rsp_var))
```

![](NYTBlogs_zoo_files/figure-html/cleanse.data-1.png) 

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
    if (grepl("Cook|Food|Thanksgiving", glb_entity_df[row_ix, "Headline"])) 
        return("myFood::")
    if (grepl("Tech |Tech$|Techn[^i]", glb_entity_df[row_ix, "Headline"]))
        return("myTech::")
    if (grepl("Apple|Firefox|Google|Microsoft|Robot|Yahoo", 
              glb_entity_df[row_ix, "Headline"]))
        return("myTech::")
    
    if (grepl("College|School", 
              glb_entity_df[row_ix, "Headline"]))
        return("myEducation::")
    if (grepl("Obama|President", 
              glb_entity_df[row_ix, "Headline"]))
        return("myPolitics::")

    # This should be last b/c topics are probably more important than media type
    if (grepl("Video", 
              glb_entity_df[row_ix, "Headline"]))
        return("myMultimedia::")
    
    return("myMisc::")
})
#print(glb_entity_df[sel_obs(Headline.pfx="myTech::"), c("UniqueID", "Headline")])
#nrow(glb_entity_df[sel_obs(Headline.pfx="myTech::"), c("UniqueID", "Headline")])
print(mycreate_sqlxtab_df(glb_entity_df, "Headline.pfx"))    
```

```
##                      Headline.pfx   .n
## 1                        myMisc:: 6383
## 2                  19[0-9][0-9]::  193
## 3                .*Fashion Week::  186
## 4                    myPolitics::  176
## 5                        myTech::  149
## 6                        myFood::   90
## 7             Daily Clip Report::   84
## 8                New York Today::   83
## 9                  Daily Report::   78
## 10               Morning Agenda::   78
## 11                Test Yourself::   78
## 12              Word of the Day::   78
## 13         6 Q's About the News::   76
## 14 Pictures of the (Day|Year|.)::   76
## 15      Today in Small Business::   73
## 16                  First Draft::   72
## 17                  myEducation::   69
## 18            Today in Politics::   65
## 19                   What We're::   57
## 20                 myMultimedia::   56
## 21               The Daily Gift::   50
## 22                     Verbatim::   45
## 23              Readers Respond::   23
## 24                     Ask Well::   18
## 25                  On This Day::   18
## 26         Quiz(.*)([?=|]|[?=:]::   17
## 27                    Your Turn::   16
## 28          Reporter's Notebook::   15
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
## 14                        myMisc::    Metro    N.Y. / Region
## 15                        myMisc::                      U.S.
## 16                        myMisc::  Science           Health
## 17                        myMisc::   Travel           Travel
## 18                        myMisc::     OpEd          Opinion
## 19                        myMisc:: Business Crosswords/Games
## 20                        myMisc::                          
## 21                        myMisc::                Multimedia
## 22                    myPolitics::                          
## 23                        myMisc::   Styles             U.S.
## 24                        myMisc:: Business     Business Day
## 25                        myMisc::   TStyle                 
## 26                        myMisc:: Business       Technology
## 27                        myMisc:: Business     Business Day
## 28                        myMisc::  Culture                 
## 29                        myMisc::   Styles                 
## 30             Daily Clip Report::                          
## 31                Morning Agenda:: Business     Business Day
## 32                        myTech:: Business       Technology
## 33          6 Q's About the News::                      U.S.
## 34                 Test Yourself::                      U.S.
## 35               Word of the Day::                      U.S.
## 36                  Daily Report:: Business       Technology
## 37                        myMisc::   Styles             U.S.
## 38                New York Today::    Metro    N.Y. / Region
## 39                   First Draft::                          
## 40       Today in Small Business:: Business     Business Day
## 41                        myMisc::                   Opinion
## 42                        myMisc::   Styles             U.S.
## 43                        myMisc::  Foreign            World
## 44  Pictures of the (Day|Year|.)::                Multimedia
## 45                        myMisc::  Culture             Arts
## 46                        myMisc::  Science           Health
## 47                        myMisc::  Science           Health
## 48                .*Fashion Week::   Styles                 
## 49             Today in Politics::                          
## 50                        myMisc::    Metro    N.Y. / Region
## 52                    myPolitics::                          
## 51                    What We're::                          
## 53                  19[0-9][0-9]::  Foreign                 
## 54                        myMisc:: Business Crosswords/Games
## 56                        myMisc::                      U.S.
## 55                      Verbatim::                          
## 57                        myMisc:: Business       Technology
## 58                        myMisc::   Travel           Travel
## 59                        myMisc::                Multimedia
## 60                        myMisc:: Magazine         Magazine
## 61                The Daily Gift::   TStyle                 
## 62             Daily Clip Report::                          
## 65                        myMisc:: Business     Business Day
## 66                        myMisc::  Foreign                 
## 63  Pictures of the (Day|Year|.)::                Multimedia
## 64                The Daily Gift::   TStyle                 
## 67             Today in Politics::                          
## 69                        myMisc::                   Opinion
## 68                New York Today::    Metro    N.Y. / Region
## 70                  Daily Report:: Business       Technology
## 71                        myMisc:: Business Crosswords/Games
## 72                        myTech:: Business       Technology
## 75                        myTech:: Business       Technology
## 73                 Test Yourself::                      U.S.
## 74               Word of the Day::                      U.S.
## 76                Morning Agenda:: Business     Business Day
## 77                        myMisc::     OpEd                 
## 78                    myPolitics::     OpEd          Opinion
## 79          6 Q's About the News::                      U.S.
## 81                        myFood::  Science           Health
## 80                   On This Day::                          
## 82                   First Draft::                          
## 83                        myMisc::                   Opinion
## 85                        myFood::   TStyle                 
## 86                        myTech:: Business     Business Day
## 84       Today in Small Business:: Business     Business Day
## 89                        myMisc::              Business Day
## 90                  myMultimedia::  Culture             Arts
## 87          Quiz(.*)([?=|]|[?=:]::                      U.S.
## 88                      Verbatim::                          
## 92                        myMisc::    Metro    N.Y. / Region
## 93                        myMisc::   Styles                 
## 91                    What We're::                          
## 94                   myEducation::                      U.S.
## 95                        myMisc::                      Arts
## 96                        myMisc::                   Opinion
## 97                  19[0-9][0-9]::                          
## 98                      Ask Well::  Science           Health
## 100                  myEducation::   Styles             U.S.
## 101                       myFood::   Travel           Travel
## 102                       myMisc::  Foreign            World
## 99                     Your Turn::   Styles             U.S.
## 104                       myMisc::  Foreign                 
## 105                       myMisc::   TStyle                 
## 106                 myMultimedia::   TStyle                 
## 103          Reporter's Notebook::                          
## 107                     Ask Well::  Science           Health
## 111                  myEducation::  Culture             Arts
## 112                       myFood:: Business     Business Day
## 113                   myPolitics::  Culture             Arts
## 108              Readers Respond::                          
## 109              Readers Respond::                          
## 110          Reporter's Notebook::                          
## 114                  myEducation::  Foreign            World
## 115                       myFood::                          
## 116                       myFood::                      U.S.
## 117                       myFood::   Styles             U.S.
## 118                       myMisc::                   Opinion
## 119                       myMisc:: Business     Business Day
## 120                   myPolitics::                          
## 121                       myTech:: Business     Business Day
## 122                 19[0-9][0-9]::                          
## 126                  myEducation::                          
## 127                  myEducation::  Culture             Arts
## 128                       myFood::    Metro    N.Y. / Region
## 129                       myFood::   Styles             U.S.
## 130                       myMisc::          Crosswords/Games
## 131                       myMisc::                      Open
## 132                       myMisc::                   Opinion
## 133                 myMultimedia::                          
## 134                 myMultimedia::                      U.S.
## 135                 myMultimedia::  Foreign            World
## 136                 myMultimedia::   TStyle                 
## 137                       myTech:: Business                 
## 123              Readers Respond::                          
## 124                   What We're::     OpEd          Opinion
## 125                    Your Turn::   Styles             U.S.
## 138                     Ask Well::  Science           Health
## 142                  myEducation:: Business     Business Day
## 143                  myEducation::   Styles             U.S.
## 144                  myEducation::   Styles             U.S.
## 145                       myFood:: Business       Technology
## 146                       myFood::  Science           Health
## 147                       myMisc::              Business Day
## 148                       myMisc::                    Travel
## 149                       myMisc:: Magazine         Magazine
## 150                 myMultimedia::                          
## 151                 myMultimedia:: Business       Technology
## 152                 myMultimedia::  Culture             Arts
## 153                   myPolitics::     OpEd          Opinion
## 154                       myTech::                          
## 155                       myTech::                      U.S.
## 156                       myTech:: Business     Business Day
## 139               New York Today::    Metro    N.Y. / Region
## 140                  On This Day::                          
## 141         Quiz(.*)([?=|]|[?=:]::                      U.S.
## 161                  myEducation::                          
## 162                  myEducation::                      U.S.
## 163                  myEducation:: Business Crosswords/Games
## 164                  myEducation:: Business       Technology
## 165                       myFood:: Business     Business Day
## 166                       myFood::  Foreign            World
## 167                       myFood:: Magazine         Magazine
## 168                       myFood::  Science           Health
## 169                       myMisc::                Multimedia
## 170                       myMisc::             N.Y. / Region
## 171                       myMisc::                   Opinion
## 172                       myMisc::  Foreign            World
## 173                       myMisc:: National                 
## 174                       myMisc:: National             U.S.
## 175                       myMisc::  Science                 
## 176                       myMisc::  Science                 
## 177                       myMisc::   Styles            Style
## 178                 myMultimedia::   Styles             U.S.
## 179                   myPolitics::                   Opinion
## 180                   myPolitics:: Business     Business Day
## 181                   myPolitics::     OpEd          Opinion
## 182                   myPolitics::   Styles                 
## 183                   myPolitics::   Styles                 
## 184                       myTech::                   Opinion
## 185                       myTech::  Culture             Arts
## 186                       myTech::  Culture             Arts
## 187                       myTech::     OpEd          Opinion
## 188                       myTech::   Styles                 
## 157              Readers Respond::                   Opinion
## 158          Reporter's Notebook::                          
## 159               The Daily Gift::                          
## 160                    Your Turn::   Styles             U.S.
## 189               .*Fashion Week::                          
## 190               .*Fashion Week::  Culture             Arts
## 191               .*Fashion Week::    Metro    N.Y. / Region
## 192               .*Fashion Week::   Styles                 
## 207                  myEducation::                          
## 208                  myEducation::                    Travel
## 209                  myEducation::                      U.S.
## 210                  myEducation:: Business     Business Day
## 211                  myEducation:: Business     Business Day
## 212                  myEducation:: Business     Business Day
## 213                  myEducation:: Business     Business Day
## 214                  myEducation:: Business Crosswords/Games
## 215                  myEducation:: Business       Technology
## 216                  myEducation::    Metro    N.Y. / Region
## 217                  myEducation::     OpEd                 
## 218                  myEducation::  Science           Health
## 219                  myEducation::   TStyle                 
## 220                  myEducation::   TStyle                 
## 221                       myFood::                          
## 222                       myFood:: Business     Business Day
## 223                       myFood:: Business Crosswords/Games
## 224                       myFood:: Business       Technology
## 225                       myFood::  Culture             Arts
## 226                       myFood::    Metro    N.Y. / Region
## 227                       myFood::     OpEd          Opinion
## 228                       myFood::     OpEd          Opinion
## 230                       myFood::   Travel           Travel
## 229                       myFood::   TStyle                 
## 231                       myMisc::              Business Day
## 232                       myMisc::          Crosswords/Games
## 233                       myMisc::                    Health
## 234                       myMisc::                      Open
## 235                       myMisc::                   Opinion
## 236                       myMisc::                Technology
## 237                       myMisc::                    Travel
## 238                       myMisc::                      U.S.
## 239                       myMisc::                     World
## 240                       myMisc::  Culture                 
## 241                       myMisc::  Foreign            World
## 242                       myMisc::     OpEd                 
## 243                       myMisc::   Sports                 
## 244                       myMisc::   Sports           Sports
## 245                       myMisc::   Styles           Health
## 246                       myMisc::   Travel           Travel
## 247                 myMultimedia::                      Arts
## 248                 myMultimedia::                Multimedia
## 249                 myMultimedia::                   Opinion
## 250                 myMultimedia::                      U.S.
## 251                 myMultimedia:: Business     Business Day
## 252                 myMultimedia:: Business     Business Day
## 253                 myMultimedia::  Culture                 
## 254                 myMultimedia::    Metro    N.Y. / Region
## 255                 myMultimedia::    Metro    N.Y. / Region
## 256                 myMultimedia::   Styles             U.S.
## 257                   myPolitics::                   Opinion
## 258                   myPolitics:: Business     Business Day
## 259                   myPolitics:: Business     Business Day
## 260                   myPolitics:: Business       Technology
## 261                   myPolitics::  Culture             Arts
## 262                   myPolitics::  Foreign            World
## 263                   myPolitics::  Foreign            World
## 264                   myPolitics::  Foreign            World
## 265                   myPolitics::   Styles                 
## 266                   myPolitics::   TStyle                 
## 267                       myTech::              Business Day
## 268                       myTech::                      U.S.
## 269                       myTech:: Business                 
## 270                       myTech:: Business     Business Day
## 271                       myTech:: Business     Business Day
## 272                       myTech:: Business Crosswords/Games
## 273                       myTech::     OpEd                 
## 274                       myTech::     OpEd          Opinion
## 275                       myTech::  Science           Health
## 276                       myTech::   Styles                 
## 277                       myTech::   TStyle                 
## 193               New York Today::             N.Y. / Region
## 194 Pictures of the (Day|Year|.)::    Metro    N.Y. / Region
## 195         Quiz(.*)([?=|]|[?=:]::                          
## 196         Quiz(.*)([?=|]|[?=:]::                          
## 197              Readers Respond::                   Opinion
## 198              Readers Respond:: Business     Business Day
## 199              Readers Respond:: Magazine         Magazine
## 200              Readers Respond::    Metro    N.Y. / Region
## 201              Readers Respond::     OpEd          Opinion
## 202               The Daily Gift::                          
## 203      Today in Small Business::              Business Day
## 204      Today in Small Business:: Business                 
## 205                   What We're::     OpEd          Opinion
## 206                    Your Turn::                      U.S.
##        SubsectionName Popular.fctr  .n
## 1                                N 794
## 2            Dealbook            N 779
## 3                                N 598
## 4                                N 532
## 5                                Y 387
## 6            Dealbook         <NA> 265
## 7                             <NA> 195
## 8        Asia Pacific            N 188
## 9                             <NA> 152
## 10                               N 149
## 11                               N 141
## 12                            <NA> 138
## 13                               N 136
## 14                               N 116
## 15          Education            N 108
## 16                               Y 108
## 17                               N 106
## 18                               N 104
## 19                               Y  99
## 20                               Y  97
## 21                               N  86
## 22                               N  85
## 23                               Y  81
## 24           Dealbook            Y  80
## 25                            <NA>  78
## 26                            <NA>  74
## 27     Small Business            N  73
## 28                            <NA>  69
## 29                               N  66
## 30                               N  62
## 31           Dealbook            N  62
## 32                               N  62
## 33          Education            N  61
## 34          Education            N  61
## 35          Education            N  61
## 36                               N  60
## 37                               N  60
## 38                               N  59
## 39                               N  58
## 40     Small Business            N  58
## 41    Room For Debate            N  57
## 42                            <NA>  55
## 43       Asia Pacific         <NA>  54
## 44                               N  53
## 45                               Y  50
## 46                               N  50
## 47                            <NA>  49
## 48                               N  46
## 49                               N  44
## 50                            <NA>  42
## 52                            <NA>  41
## 51                               N  41
## 53                            <NA>  39
## 54                            <NA>  38
## 56          Education         <NA>  33
## 55                               N  33
## 57                               Y  32
## 58                            <NA>  30
## 59                            <NA>  29
## 60                               N  28
## 61                               N  25
## 62                            <NA>  22
## 65     Small Business         <NA>  22
## 66                               N  22
## 63                            <NA>  22
## 64                            <NA>  22
## 67                            <NA>  21
## 69    Room For Debate         <NA>  20
## 68                            <NA>  20
## 70                            <NA>  18
## 71                               N  18
## 72                            <NA>  18
## 75                               Y  17
## 73          Education         <NA>  17
## 74          Education         <NA>  17
## 76           Dealbook         <NA>  16
## 77                            <NA>  16
## 78                               Y  16
## 79          Education         <NA>  15
## 81                               N  15
## 80                               N  15
## 82                            <NA>  14
## 83  The Public Editor            Y  14
## 85                               N  13
## 86           Dealbook            N  13
## 84     Small Business         <NA>  13
## 89           Dealbook         <NA>  12
## 90                               N  12
## 87          Education            N  12
## 88                            <NA>  12
## 92                               Y  11
## 93                            <NA>  11
## 91                            <NA>  11
## 94          Education            N  10
## 95                            <NA>  10
## 96  The Public Editor         <NA>  10
## 97                               N   9
## 98                               Y   9
## 100                              Y   9
## 101                              N   9
## 102                              N   9
## 99                               N   9
## 104                           <NA>   7
## 105                              Y   7
## 106                              N   7
## 103                           <NA>   7
## 107                              N   6
## 111                              N   6
## 112          Dealbook            N   6
## 113                              N   6
## 108                              N   6
## 109                              Y   6
## 110                              N   6
## 114      Asia Pacific            N   5
## 115                              N   5
## 116         Education            N   5
## 117                              N   5
## 118                           <NA>   5
## 119    Small Business            Y   5
## 120                              Y   5
## 121          Dealbook            Y   5
## 122                           <NA>   4
## 126                              Y   4
## 127                           <NA>   4
## 128                              N   4
## 129                              Y   4
## 130                           <NA>   4
## 131                              N   4
## 132 The Public Editor            N   4
## 133                              N   4
## 134         Education            N   4
## 135      Asia Pacific            N   4
## 136                           <NA>   4
## 137                              N   4
## 123                           <NA>   4
## 124                              N   4
## 125                              Y   4
## 138                           <NA>   3
## 142          Dealbook         <NA>   3
## 143                           <NA>   3
## 144                              N   3
## 145                              N   3
## 146                           <NA>   3
## 147    Small Business         <NA>   3
## 148                           <NA>   3
## 149                           <NA>   3
## 150                           <NA>   3
## 151                              N   3
## 152                           <NA>   3
## 153                              N   3
## 154                              N   3
## 155         Education            N   3
## 156          Dealbook         <NA>   3
## 139                              Y   3
## 140                           <NA>   3
## 141         Education         <NA>   3
## 161                              N   2
## 162         Education         <NA>   2
## 163                              Y   2
## 164                              N   2
## 165          Dealbook         <NA>   2
## 166      Asia Pacific            N   2
## 167                              N   2
## 168                              Y   2
## 169                              Y   2
## 170                           <NA>   2
## 171                              N   2
## 172      Asia Pacific            Y   2
## 173                              N   2
## 174          Politics            N   2
## 175                           <NA>   2
## 176                              Y   2
## 177   Fashion & Style            N   2
## 178                              Y   2
## 179   Room For Debate            N   2
## 180          Dealbook            Y   2
## 181                           <NA>   2
## 182                           <NA>   2
## 183                              N   2
## 184   Room For Debate            N   2
## 185                           <NA>   2
## 186                              N   2
## 187                              Y   2
## 188                              N   2
## 157                              N   2
## 158                              Y   2
## 159                           <NA>   2
## 160                           <NA>   2
## 189                              N   1
## 190                           <NA>   1
## 191                              N   1
## 192                           <NA>   1
## 207                           <NA>   1
## 208                           <NA>   1
## 209                           <NA>   1
## 210          Dealbook            N   1
## 211          Dealbook            Y   1
## 212    Small Business         <NA>   1
## 213    Small Business            N   1
## 214                              N   1
## 215                           <NA>   1
## 216                              N   1
## 217                           <NA>   1
## 218                              N   1
## 219                              N   1
## 220                              Y   1
## 221                           <NA>   1
## 222    Small Business            N   1
## 223                              Y   1
## 224                              Y   1
## 225                              N   1
## 226                              Y   1
## 227                              N   1
## 228                              Y   1
## 230                           <NA>   1
## 229                           <NA>   1
## 231    Small Business            N   1
## 232                              N   1
## 233                              Y   1
## 234                           <NA>   1
## 235   Room For Debate            Y   1
## 236                           <NA>   1
## 237                              N   1
## 238                              N   1
## 239      Asia Pacific         <NA>   1
## 240                              N   1
## 241                           <NA>   1
## 242                              Y   1
## 243                              N   1
## 244                              N   1
## 245                              N   1
## 246                              Y   1
## 247                           <NA>   1
## 248                           <NA>   1
## 249 The Public Editor            Y   1
## 250         Education         <NA>   1
## 251          Dealbook         <NA>   1
## 252          Dealbook            N   1
## 253                           <NA>   1
## 254                           <NA>   1
## 255                              Y   1
## 256                           <NA>   1
## 257 The Public Editor            Y   1
## 258          Dealbook         <NA>   1
## 259          Dealbook            N   1
## 260                              N   1
## 261                           <NA>   1
## 262      Asia Pacific         <NA>   1
## 263      Asia Pacific            N   1
## 264      Asia Pacific            Y   1
## 265                              Y   1
## 266                              N   1
## 267          Dealbook         <NA>   1
## 268         Education         <NA>   1
## 269                              Y   1
## 270    Small Business         <NA>   1
## 271    Small Business            N   1
## 272                              Y   1
## 273                           <NA>   1
## 274                              N   1
## 275                              N   1
## 276                           <NA>   1
## 277                              Y   1
## 193                           <NA>   1
## 194                           <NA>   1
## 195                           <NA>   1
## 196                              Y   1
## 197                              Y   1
## 198          Dealbook            N   1
## 199                              N   1
## 200                              Y   1
## 201                              Y   1
## 202                              N   1
## 203    Small Business         <NA>   1
## 204                           <NA>   1
## 205                           <NA>   1
## 206                           <NA>   1
```

```r
glb_entity_df[, "NewsDesk.nb"] <- sapply(1:nrow(glb_entity_df), function(rix) {
    if (glb_entity_df[rix, "Headline.pfx"] %in% 
            c("Quiz(.*)([?=|]|[?=:]::", "On This Day::")) 
        return("myEducation")
    if (glb_entity_df[rix, "Headline.pfx"] == ".*Fashion Week::") return("TStyle")
    if (glb_entity_df[rix, "Headline.pfx"] == "Pictures of the (Day|Year|.)::") 
        return("myMultimedia")
        
    if (glb_entity_df[rix, "SectionName"] %in% 
            c("Business Day", "Crosswords/Games", "Technology")) return("Business")
    if (glb_entity_df[rix, "SectionName"] == "Arts") return("Culture")
    if (glb_entity_df[rix, "SectionName"] == "World") return("Foreign")    
    if ((glb_entity_df[rix, "SectionName"] == "N.Y. / Region") &
        !(glb_entity_df[rix, "Headline.pfx"] %in% 
              c(".*Fashion Week::", "Pictures of the (Day|Year|.)::")))    
        return("Metro")    
    if (glb_entity_df[rix, "SectionName"] == "Multimedia") return("myMultimedia")
    if (glb_entity_df[rix, "SectionName"] == "Opinion") return("OpEd")
    if (glb_entity_df[rix, "SectionName"] == "Health") return("Science")
    
    if ((str <- glb_entity_df[rix, "NewsDesk"]) != "") return(str)

    if (glb_entity_df[rix, "SubsectionName"] == "Education") return("myEducation")
    
    if (glb_entity_df[rix, "Headline.pfx"] == "19[0-9][0-9]::") return("Foreign")    
    if (glb_entity_df[rix, "Headline.pfx"] == "What We're::") return("OpEd")    
    if (glb_entity_df[rix, "Headline.pfx"] == "Your Turn::") return("Styles")
    
    if (glb_entity_df[rix, "Headline.pfx"] == "myFood::") return("Styles")    
    if (glb_entity_df[rix, "Headline.pfx"] == "myTech::") return("Business")        
    
    return(glb_entity_df[rix, "Headline.pfx"])
})
mycheck_map_results(glb_entity_df, "NewsDesk", "NewsDesk.nb", print.all=TRUE)
```

```
##    NewsDesk           NewsDesk.nb   .n
## 1  Business              Business 2026
## 2                        myMisc:: 1096
## 3   Culture               Culture  908
## 4    TStyle                TStyle  829
## 5      OpEd                  OpEd  680
## 6   Foreign               Foreign  477
## 7                     myEducation  434
## 8    Styles                Styles  325
## 9     Metro                 Metro  260
## 10  Science               Science  251
## 11                   myMultimedia  193
## 12                           OpEd  174
## 13   Travel                Travel  147
## 14                   myPolitics::  131
## 15            Daily Clip Report::   84
## 16                  First Draft::   72
## 17            Today in Politics::   65
## 18   Styles                TStyle   47
## 19                     Verbatim::   45
## 20 Magazine              Magazine   34
## 21                       Business   27
## 22              Readers Respond::   16
## 23          Reporter's Notebook::   15
## 24                        Foreign   14
## 25                        Culture   11
## 26                  myEducation::    9
## 27                         Styles    7
## 28                 myMultimedia::    7
## 29 National              National    4
## 30                          Metro    3
## 31               The Daily Gift::    3
## 32   Sports                Sports    2
## 33                        Science    1
## 34                         TStyle    1
## 35  Culture                TStyle    1
## 36    Metro                TStyle    1
## 37    Metro          myMultimedia    1
## 38   Styles               Science    1
```

![](NYTBlogs_zoo_files/figure-html/cleanse.data-2.png) 

```r
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, "NewsDesk")

glb_entity_df[, "SectionName.nb"] <- sapply(1:nrow(glb_entity_df), function(rix) {
    if (glb_entity_df[rix, "Headline.pfx"] == "Today in Small Business::")
        return("Business Day")
    if (glb_entity_df[rix, "Headline.pfx"] == ".*Fashion Week::") return("TStyle")    
    if (glb_entity_df[rix, "Headline.pfx"] %in% 
            c("On This Day::", "Quiz(.*)([?=|]|[?=:]::")) return("U.S.")            
    if (glb_entity_df[rix, "Headline.pfx"] == "19[0-9][0-9]::") return("World")        
    
    str <- glb_entity_df[rix, "SectionName"]

    if (str == "Style") return("Styles")
    if (str != "") return (str)
    
    if (glb_entity_df[rix, "NewsDesk"] == "OpEd") return("Opinion")
    if (glb_entity_df[rix, "NewsDesk.nb"] == "OpEd") return("Opinion")    
    
    if (glb_entity_df[rix, "NewsDesk"] == "Science") return("Health")
    if (glb_entity_df[rix, "NewsDesk"] == "Foreign") return("World")
    
    if (glb_entity_df[rix, "Headline.pfx"] == "myTech::") return("Technology")    
    
    return(glb_entity_df[rix, "NewsDesk.nb"])
})
mycheck_map_results(glb_entity_df, "SectionName", "SectionName.nb", print.all=TRUE)
```

```
##         SectionName        SectionName.nb   .n
## 1      Business Day          Business Day 1437
## 2                                myMisc:: 1086
## 3                                  TStyle  876
## 4              Arts                  Arts  848
## 5           Opinion               Opinion  783
## 6              U.S.                  U.S.  657
## 7        Technology            Technology  442
## 8             World                 World  269
## 9     N.Y. / Region         N.Y. / Region  264
## 10           Health                Health  249
## 11                                  World  222
## 12       Multimedia            Multimedia  193
## 13 Crosswords/Games      Crosswords/Games  165
## 14           Travel                Travel  152
## 15                           myPolitics::  131
## 16                                 Styles   88
## 17                    Daily Clip Report::   84
## 18                          First Draft::   72
## 19                                Culture   71
## 20                                Opinion   71
## 21                    Today in Politics::   65
## 22                             Verbatim::   45
## 23         Magazine              Magazine   34
## 24                                   U.S.   20
## 25                      Readers Respond::   16
## 26                  Reporter's Notebook::   15
## 27                             Technology   12
## 28                          myEducation::    7
## 29                         myMultimedia::    7
## 30             Open                  Open    5
## 31                                 Health    4
## 32                       The Daily Gift::    3
## 33                               National    2
## 34            Style                Styles    2
## 35                           Business Day    1
## 36                                 Sports    1
## 37             Arts                TStyle    1
## 38    N.Y. / Region                TStyle    1
## 39           Sports                Sports    1
```

![](NYTBlogs_zoo_files/figure-html/cleanse.data-3.png) 

```r
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, "SectionName")

glb_entity_df[, "SubsectionName.nb"] <- sapply(1:nrow(glb_entity_df), function(rix) {
    if ((str <- glb_entity_df[rix, "SubsectionName"]) != "") return(str)
    
    if (glb_entity_df[rix, "NewsDesk.nb"] == "Styles") return("Fashion & Style")
    if (glb_entity_df[rix, "NewsDesk.nb"] == "myEducation") return("Education")    
    
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
## 1       Dealbook             Dealbook 1256
## 2                    myMisc::myMisc:: 1086
## 3                      TStyle::TStyle  878
## 4                       Culture::Arts  848
## 5                       OpEd::Opinion  742
## 6                Business::Technology  450
##    SubsectionName                      SubsectionName.nb  .n
## 16                                        Travel::Travel 147
## 22                Today in Politics::Today in Politics::  65
## 26                                             Education  20
## 34                                    National::National   2
## 38                                        Small Business   1
## 43                           myMultimedia::N.Y. / Region   1
##    SubsectionName           SubsectionName.nb .n
## 38                             Small Business  1
## 39                         TStyle::Technology  1
## 40                        myEducation::Travel  1
## 41                          myEducation::U.S.  1
## 42                               myMisc::U.S.  1
## 43                myMultimedia::N.Y. / Region  1
```

![](NYTBlogs_zoo_files/figure-html/cleanse.data-4.png) 

```r
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, "SubsectionName")

hdlpfx_xtab_df <- orderBy(reformulate(c(glb_rsp_var, "-", ".n")),
                          mycreate_sqlxtab_df(glb_entity_df,
                                                c("Headline.pfx", glb_rsp_var)))
write.table(hdlpfx_xtab_df, paste0(glb_out_pfx, "hdlpfx_xtab.csv"), 
            row.names=FALSE)
myprint_df(hdlpfx_xtab_df)
```

```
##              Headline.pfx Popular.fctr .n
## 56             Ask Well::            N  6
## 57  Reporter's Notebook::            N  6
## 53            Your Turn::            N  9
## 49      Readers Respond::            N 10
## 46 Quiz(.*)([?=|]|[?=:]::            N 12
## 43          On This Day::            N 15
##                      Headline.pfx Popular.fctr .n
## 18 Pictures of the (Day|Year|.)::            N 53
## 15               New York Today::            N 59
## 60                    Your Turn::            Y  4
## 29                   myPolitics::            Y 26
## 48                   What We're::         <NA> 12
## 30               The Daily Gift::         <NA> 24
##                      Headline.pfx Popular.fctr   .n
## 31 Pictures of the (Day|Year|.)::         <NA>   23
## 30               The Daily Gift::         <NA>   24
## 26                       myTech::         <NA>   28
## 22                 19[0-9][0-9]::         <NA>   43
## 19                   myPolitics::         <NA>   48
## 2                        myMisc::         <NA> 1435
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
##     NewsDesk.nb NewsDesk              Headline.pfx Popular.fctr .n
## 133    Business                           myMisc::            N  2
## 61     Business                           myMisc::         <NA> 20
## 120    Business                           myTech::            N  3
## 162    Business                           myTech::         <NA>  1
## 161    Business          Today in Small Business::         <NA>  1
## 34     Business Business            Daily Report::            N 60
##      NewsDesk.nb NewsDesk                   Headline.pfx Popular.fctr  .n
## 135     Business Business                       myFood::            Y   2
## 215 myMultimedia    Metro Pictures of the (Day|Year|.)::         <NA>   1
## 27          OpEd                                myMisc::            N  63
## 182         OpEd                            myPolitics::            Y   1
## 6           OpEd     OpEd                       myMisc::            Y 388
## 191      Science  Science                  myEducation::            N   1
##     NewsDesk.nb NewsDesk     Headline.pfx Popular.fctr .n
## 206      TStyle   TStyle     myPolitics::            N  1
## 207      TStyle   TStyle         myTech::            Y  1
## 54       TStyle   TStyle The Daily Gift::            N 25
## 58       TStyle   TStyle The Daily Gift::         <NA> 22
## 49   Verbatim::                Verbatim::            N 33
## 76   Verbatim::                Verbatim::         <NA> 12
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
##               NewsDesk.nb NewsDesk        SectionName.nb      SectionName
## 182              Business                   Business Day     Business Day
## 77               Business                   Business Day     Business Day
## 183              Business                   Business Day     Business Day
## 181              Business                   Business Day     Business Day
## 184              Business               Crosswords/Games Crosswords/Games
## 116              Business               Crosswords/Games Crosswords/Games
## 133              Business                     Technology                 
## 185              Business                     Technology       Technology
## 186              Business Business          Business Day                 
## 30               Business Business          Business Day     Business Day
## 74               Business Business          Business Day     Business Day
## 149              Business Business          Business Day     Business Day
## 188              Business Business          Business Day     Business Day
## 117              Business Business          Business Day     Business Day
## 99               Business Business          Business Day     Business Day
## 150              Business Business          Business Day     Business Day
## 1                Business Business          Business Day     Business Day
## 22               Business Business          Business Day     Business Day
## 6                Business Business          Business Day     Business Day
## 190              Business Business          Business Day     Business Day
## 189              Business Business          Business Day     Business Day
## 192              Business Business          Business Day     Business Day
## 151              Business Business          Business Day     Business Day
## 191              Business Business          Business Day     Business Day
## 82               Business Business          Business Day     Business Day
## 110              Business Business          Business Day     Business Day
## 118              Business Business          Business Day     Business Day
## 187              Business Business          Business Day     Business Day
## 39               Business Business          Business Day     Business Day
## 84               Business Business          Business Day     Business Day
## 193              Business Business      Crosswords/Games Crosswords/Games
## 152              Business Business      Crosswords/Games Crosswords/Games
## 194              Business Business      Crosswords/Games Crosswords/Games
## 68               Business Business      Crosswords/Games Crosswords/Games
## 19               Business Business      Crosswords/Games Crosswords/Games
## 53               Business Business      Crosswords/Games Crosswords/Games
## 195              Business Business      Crosswords/Games Crosswords/Games
## 119              Business Business            Technology                 
## 196              Business Business            Technology                 
## 36               Business Business            Technology       Technology
## 69               Business Business            Technology       Technology
## 153              Business Business            Technology       Technology
## 197              Business Business            Technology       Technology
## 134              Business Business            Technology       Technology
## 198              Business Business            Technology       Technology
## 10               Business Business            Technology       Technology
## 57               Business Business            Technology       Technology
## 26               Business Business            Technology       Technology
## 135              Business Business            Technology       Technology
## 199              Business Business            Technology       Technology
## 31               Business Business            Technology       Technology
## 71               Business Business            Technology       Technology
## 70               Business Business            Technology       Technology
## 92                Culture                           Arts             Arts
## 200               Culture                           Arts             Arts
## 104               Culture  Culture                  Arts             Arts
## 120               Culture  Culture                  Arts             Arts
## 201               Culture  Culture                  Arts             Arts
## 3                 Culture  Culture                  Arts             Arts
## 44                Culture  Culture                  Arts             Arts
## 9                 Culture  Culture                  Arts             Arts
## 86                Culture  Culture                  Arts             Arts
## 136               Culture  Culture                  Arts             Arts
## 105               Culture  Culture                  Arts             Arts
## 202               Culture  Culture                  Arts             Arts
## 155               Culture  Culture                  Arts             Arts
## 154               Culture  Culture                  Arts             Arts
## 203               Culture  Culture               Culture                 
## 27                Culture  Culture               Culture                 
## 204               Culture  Culture               Culture                 
## 32    Daily Clip Report::            Daily Clip Report::                 
## 62    Daily Clip Report::            Daily Clip Report::                 
## 40          First Draft::                  First Draft::                 
## 83          First Draft::                  First Draft::                 
## 94                Foreign                          World                 
## 121               Foreign                          World                 
## 205               Foreign                          World            World
## 11                Foreign  Foreign                 World                 
## 52                Foreign  Foreign                 World                 
## 63                Foreign  Foreign                 World                 
## 100               Foreign  Foreign                 World                 
## 111               Foreign  Foreign                 World            World
## 156               Foreign  Foreign                 World            World
## 7                 Foreign  Foreign                 World            World
## 157               Foreign  Foreign                 World            World
## 41                Foreign  Foreign                 World            World
## 122               Foreign  Foreign                 World            World
## 207               Foreign  Foreign                 World            World
## 208               Foreign  Foreign                 World            World
## 206               Foreign  Foreign                 World            World
## 158              Magazine Magazine              Magazine         Magazine
## 60               Magazine Magazine              Magazine         Magazine
## 137              Magazine Magazine              Magazine         Magazine
## 209              Magazine Magazine              Magazine         Magazine
## 159                 Metro                  N.Y. / Region    N.Y. / Region
## 210                 Metro                  N.Y. / Region    N.Y. / Region
## 212                 Metro    Metro         N.Y. / Region    N.Y. / Region
## 123                 Metro    Metro         N.Y. / Region    N.Y. / Region
## 213                 Metro    Metro         N.Y. / Region    N.Y. / Region
## 14                  Metro    Metro         N.Y. / Region    N.Y. / Region
## 89                  Metro    Metro         N.Y. / Region    N.Y. / Region
## 49                  Metro    Metro         N.Y. / Region    N.Y. / Region
## 215                 Metro    Metro         N.Y. / Region    N.Y. / Region
## 214                 Metro    Metro         N.Y. / Region    N.Y. / Region
## 38                  Metro    Metro         N.Y. / Region    N.Y. / Region
## 138                 Metro    Metro         N.Y. / Region    N.Y. / Region
## 67                  Metro    Metro         N.Y. / Region    N.Y. / Region
## 211                 Metro    Metro         N.Y. / Region    N.Y. / Region
## 80            myEducation                           U.S.                 
## 144           myEducation                           U.S.                 
## 251           myEducation                           U.S.                 
## 250           myEducation                           U.S.                 
## 33            myEducation                           U.S.             U.S.
## 81            myEducation                           U.S.             U.S.
## 93            myEducation                           U.S.             U.S.
## 178           myEducation                           U.S.             U.S.
## 114           myEducation                           U.S.             U.S.
## 16            myEducation                           U.S.             U.S.
## 56            myEducation                           U.S.             U.S.
## 129           myEducation                           U.S.             U.S.
## 252           myEducation                           U.S.             U.S.
## 146           myEducation                           U.S.             U.S.
## 253           myEducation                           U.S.             U.S.
## 88            myEducation                           U.S.             U.S.
## 145           myEducation                           U.S.             U.S.
## 34            myEducation                           U.S.             U.S.
## 72            myEducation                           U.S.             U.S.
## 35            myEducation                           U.S.             U.S.
## 73            myEducation                           U.S.             U.S.
## 179         myEducation::                  myEducation::                 
## 130         myEducation::                  myEducation::                 
## 256         myEducation::                  myEducation::                 
## 254         myEducation::                         Travel           Travel
## 255         myEducation::                           U.S.             U.S.
## 2                myMisc::                       myMisc::                 
## 20               myMisc::                       myMisc::                 
## 8                myMisc::                       myMisc::                 
## 131              myMisc::                           Open             Open
## 257              myMisc::                           Open             Open
## 258              myMisc::                         Travel           Travel
## 147              myMisc::                         Travel           Travel
## 259              myMisc::                           U.S.             U.S.
## 21           myMultimedia                     Multimedia       Multimedia
## 180          myMultimedia                     Multimedia       Multimedia
## 59           myMultimedia                     Multimedia       Multimedia
## 260          myMultimedia                     Multimedia       Multimedia
## 43           myMultimedia                     Multimedia       Multimedia
## 65           myMultimedia                     Multimedia       Multimedia
## 261          myMultimedia    Metro         N.Y. / Region    N.Y. / Region
## 132        myMultimedia::                 myMultimedia::                 
## 148        myMultimedia::                 myMultimedia::                 
## 23           myPolitics::                   myPolitics::                 
## 115          myPolitics::                   myPolitics::                 
## 51           myPolitics::                   myPolitics::                 
## 160              National National              National                 
## 161              National National                  U.S.             U.S.
## 50                   OpEd                        Opinion                 
## 90                   OpEd                        Opinion                 
## 29                   OpEd                        Opinion          Opinion
## 78                   OpEd                        Opinion          Opinion
## 54                   OpEd                        Opinion          Opinion
## 217                  OpEd                        Opinion          Opinion
## 163                  OpEd                        Opinion          Opinion
## 218                  OpEd                        Opinion          Opinion
## 164                  OpEd                        Opinion          Opinion
## 162                  OpEd                        Opinion          Opinion
## 216                  OpEd                        Opinion          Opinion
## 219                  OpEd     OpEd               Opinion                 
## 220                  OpEd     OpEd               Opinion                 
## 75                   OpEd     OpEd               Opinion                 
## 221                  OpEd     OpEd               Opinion                 
## 224                  OpEd     OpEd               Opinion          Opinion
## 225                  OpEd     OpEd               Opinion          Opinion
## 18                   OpEd     OpEd               Opinion          Opinion
## 5                    OpEd     OpEd               Opinion          Opinion
## 12                   OpEd     OpEd               Opinion          Opinion
## 139                  OpEd     OpEd               Opinion          Opinion
## 76                   OpEd     OpEd               Opinion          Opinion
## 165                  OpEd     OpEd               Opinion          Opinion
## 226                  OpEd     OpEd               Opinion          Opinion
## 166                  OpEd     OpEd               Opinion          Opinion
## 222                  OpEd     OpEd               Opinion          Opinion
## 124                  OpEd     OpEd               Opinion          Opinion
## 223                  OpEd     OpEd               Opinion          Opinion
## 106     Readers Respond::              Readers Respond::                 
## 107     Readers Respond::              Readers Respond::                 
## 125     Readers Respond::              Readers Respond::                 
## 108 Reporter's Notebook::          Reporter's Notebook::                 
## 167 Reporter's Notebook::          Reporter's Notebook::                 
## 101 Reporter's Notebook::          Reporter's Notebook::                 
## 227               Science                         Health           Health
## 169               Science  Science                Health                 
## 168               Science  Science                Health                 
## 109               Science  Science                Health           Health
## 95                Science  Science                Health           Health
## 140               Science  Science                Health           Health
## 228               Science  Science                Health           Health
## 79                Science  Science                Health           Health
## 170               Science  Science                Health           Health
## 141               Science  Science                Health           Health
## 45                Science  Science                Health           Health
## 15                Science  Science                Health           Health
## 46                Science  Science                Health           Health
## 229               Science  Science                Health           Health
## 230               Science   Styles                Health           Health
## 231                Sports   Sports                Sports                 
## 232                Sports   Sports                Sports           Sports
## 112                Styles                         Styles                 
## 233                Styles                         Styles                 
## 234                Styles                           U.S.             U.S.
## 28                 Styles   Styles                Styles                 
## 91                 Styles   Styles                Styles                 
## 172                Styles   Styles                Styles                 
## 235                Styles   Styles                Styles                 
## 171                Styles   Styles                Styles                 
## 173                Styles   Styles                Styles            Style
## 174                Styles   Styles            Technology                 
## 236                Styles   Styles            Technology                 
## 143                Styles   Styles                  U.S.             U.S.
## 97                 Styles   Styles                  U.S.             U.S.
## 142                Styles   Styles                  U.S.             U.S.
## 113                Styles   Styles                  U.S.             U.S.
## 127                Styles   Styles                  U.S.             U.S.
## 37                 Styles   Styles                  U.S.             U.S.
## 24                 Styles   Styles                  U.S.             U.S.
## 42                 Styles   Styles                  U.S.             U.S.
## 176                Styles   Styles                  U.S.             U.S.
## 237                Styles   Styles                  U.S.             U.S.
## 96                 Styles   Styles                  U.S.             U.S.
## 126                Styles   Styles                  U.S.             U.S.
## 175                Styles   Styles                  U.S.             U.S.
## 247      The Daily Gift::               The Daily Gift::                 
## 177      The Daily Gift::               The Daily Gift::                 
## 48    Today in Politics::            Today in Politics::                 
## 66    Today in Politics::            Today in Politics::                 
## 98                 Travel   Travel                Travel           Travel
## 248                Travel   Travel                Travel           Travel
## 17                 Travel   Travel                Travel           Travel
## 249                Travel   Travel                Travel           Travel
## 58                 Travel   Travel                Travel           Travel
## 238                TStyle                         TStyle                 
## 239                TStyle  Culture                TStyle             Arts
## 240                TStyle    Metro                TStyle    N.Y. / Region
## 47                 TStyle   Styles                TStyle                 
## 241                TStyle   Styles                TStyle                 
## 246                TStyle   TStyle            Technology                 
## 13                 TStyle   TStyle                TStyle                 
## 242                TStyle   TStyle                TStyle                 
## 243                TStyle   TStyle                TStyle                 
## 85                 TStyle   TStyle                TStyle                 
## 244                TStyle   TStyle                TStyle                 
## 4                  TStyle   TStyle                TStyle                 
## 102                TStyle   TStyle                TStyle                 
## 25                 TStyle   TStyle                TStyle                 
## 103                TStyle   TStyle                TStyle                 
## 128                TStyle   TStyle                TStyle                 
## 245                TStyle   TStyle                TStyle                 
## 61                 TStyle   TStyle                TStyle                 
## 64                 TStyle   TStyle                TStyle                 
## 55             Verbatim::                     Verbatim::                 
## 87             Verbatim::                     Verbatim::                 
##                       Headline.pfx Popular.fctr  .n
## 182                       myMisc::            N   1
## 77                        myMisc::         <NA>  15
## 183                       myTech::         <NA>   1
## 181      Today in Small Business::         <NA>   1
## 184                       myMisc::            N   1
## 116                       myMisc::         <NA>   4
## 133                       myTech::            N   3
## 185                       myMisc::         <NA>   1
## 186      Today in Small Business::         <NA>   1
## 30                Morning Agenda::            N  62
## 74                Morning Agenda::         <NA>  16
## 149                  myEducation::            N   2
## 188                  myEducation::            Y   1
## 117                  myEducation::         <NA>   4
## 99                        myFood::            N   7
## 150                       myFood::         <NA>   2
## 1                         myMisc::            N 852
## 22                        myMisc::            Y  85
## 6                         myMisc::         <NA> 287
## 190                 myMultimedia::            N   1
## 189                 myMultimedia::         <NA>   1
## 192                   myPolitics::            N   1
## 151                   myPolitics::            Y   2
## 191                   myPolitics::         <NA>   1
## 82                        myTech::            N  14
## 110                       myTech::            Y   5
## 118                       myTech::         <NA>   4
## 187              Readers Respond::            N   1
## 39       Today in Small Business::            N  58
## 84       Today in Small Business::         <NA>  13
## 193                  myEducation::            N   1
## 152                  myEducation::            Y   2
## 194                       myFood::            Y   1
## 68                        myMisc::            N  18
## 19                        myMisc::            Y  99
## 53                        myMisc::         <NA>  38
## 195                       myTech::            Y   1
## 119                       myTech::            N   4
## 196                       myTech::            Y   1
## 36                  Daily Report::            N  60
## 69                  Daily Report::         <NA>  18
## 153                  myEducation::            N   2
## 197                  myEducation::         <NA>   1
## 134                       myFood::            N   3
## 198                       myFood::            Y   1
## 10                        myMisc::            N 149
## 57                        myMisc::            Y  32
## 26                        myMisc::         <NA>  74
## 135                 myMultimedia::            N   3
## 199                   myPolitics::            N   1
## 31                        myTech::            N  62
## 71                        myTech::            Y  17
## 70                        myTech::         <NA>  18
## 92                        myMisc::         <NA>  10
## 200                 myMultimedia::         <NA>   1
## 104                  myEducation::            N   6
## 120                  myEducation::         <NA>   4
## 201                       myFood::            N   1
## 3                         myMisc::            N 598
## 44                        myMisc::            Y  50
## 9                         myMisc::         <NA> 152
## 86                  myMultimedia::            N  12
## 136                 myMultimedia::         <NA>   3
## 105                   myPolitics::            N   6
## 202                   myPolitics::         <NA>   1
## 155                       myTech::            N   2
## 154                       myTech::         <NA>   2
## 203                       myMisc::            N   1
## 27                        myMisc::         <NA>  69
## 204                 myMultimedia::         <NA>   1
## 32             Daily Clip Report::            N  62
## 62             Daily Clip Report::         <NA>  22
## 40                   First Draft::            N  58
## 83                   First Draft::         <NA>  14
## 94                  19[0-9][0-9]::            N   9
## 121                 19[0-9][0-9]::         <NA>   4
## 205                       myMisc::         <NA>   1
## 11                  19[0-9][0-9]::            N 141
## 52                  19[0-9][0-9]::         <NA>  39
## 63                        myMisc::            N  22
## 100                       myMisc::         <NA>   7
## 111                  myEducation::            N   5
## 156                       myFood::            N   2
## 7                         myMisc::            N 197
## 157                       myMisc::            Y   2
## 41                        myMisc::         <NA>  55
## 122                 myMultimedia::            N   4
## 207                   myPolitics::            N   1
## 208                   myPolitics::            Y   1
## 206                   myPolitics::         <NA>   1
## 158                       myFood::            N   2
## 60                        myMisc::            N  28
## 137                       myMisc::         <NA>   3
## 209              Readers Respond::            N   1
## 159                       myMisc::         <NA>   2
## 210               New York Today::         <NA>   1
## 212                  myEducation::            N   1
## 123                       myFood::            N   4
## 213                       myFood::            Y   1
## 14                        myMisc::            N 116
## 89                        myMisc::            Y  11
## 49                        myMisc::         <NA>  42
## 215                 myMultimedia::            Y   1
## 214                 myMultimedia::         <NA>   1
## 38                New York Today::            N  59
## 138               New York Today::            Y   3
## 67                New York Today::         <NA>  20
## 211              Readers Respond::            Y   1
## 80                   On This Day::            N  15
## 144                  On This Day::         <NA>   3
## 251         Quiz(.*)([?=|]|[?=:]::            Y   1
## 250         Quiz(.*)([?=|]|[?=:]::         <NA>   1
## 33          6 Q's About the News::            N  61
## 81          6 Q's About the News::         <NA>  15
## 93                   myEducation::            N  10
## 178                  myEducation::         <NA>   2
## 114                       myFood::            N   5
## 16                        myMisc::            N 108
## 56                        myMisc::         <NA>  33
## 129                 myMultimedia::            N   4
## 252                 myMultimedia::         <NA>   1
## 146                       myTech::            N   3
## 253                       myTech::         <NA>   1
## 88          Quiz(.*)([?=|]|[?=:]::            N  12
## 145         Quiz(.*)([?=|]|[?=:]::         <NA>   3
## 34                 Test Yourself::            N  61
## 72                 Test Yourself::         <NA>  17
## 35               Word of the Day::            N  61
## 73               Word of the Day::         <NA>  17
## 179                  myEducation::            N   2
## 130                  myEducation::            Y   4
## 256                  myEducation::         <NA>   1
## 254                  myEducation::         <NA>   1
## 255                  myEducation::         <NA>   1
## 2                         myMisc::            N 794
## 20                        myMisc::            Y  97
## 8                         myMisc::         <NA> 195
## 131                       myMisc::            N   4
## 257                       myMisc::         <NA>   1
## 258                       myMisc::            N   1
## 147                       myMisc::         <NA>   3
## 259                       myMisc::            N   1
## 21                        myMisc::            N  86
## 180                       myMisc::            Y   2
## 59                        myMisc::         <NA>  29
## 260                 myMultimedia::         <NA>   1
## 43  Pictures of the (Day|Year|.)::            N  53
## 65  Pictures of the (Day|Year|.)::         <NA>  22
## 261 Pictures of the (Day|Year|.)::         <NA>   1
## 132                 myMultimedia::            N   4
## 148                 myMultimedia::         <NA>   3
## 23                    myPolitics::            N  85
## 115                   myPolitics::            Y   5
## 51                    myPolitics::         <NA>  41
## 160                       myMisc::            N   2
## 161                       myMisc::            N   2
## 50                    What We're::            N  41
## 90                    What We're::         <NA>  11
## 29                        myMisc::            N  63
## 78                        myMisc::            Y  15
## 54                        myMisc::         <NA>  35
## 217                 myMultimedia::            Y   1
## 163                   myPolitics::            N   2
## 218                   myPolitics::            Y   1
## 164                       myTech::            N   2
## 162              Readers Respond::            N   2
## 216              Readers Respond::            Y   1
## 219                  myEducation::         <NA>   1
## 220                       myMisc::            Y   1
## 75                        myMisc::         <NA>  16
## 221                       myTech::         <NA>   1
## 224                       myFood::            N   1
## 225                       myFood::            Y   1
## 18                        myMisc::            N 104
## 5                         myMisc::            Y 387
## 12                        myMisc::         <NA> 138
## 139                   myPolitics::            N   3
## 76                    myPolitics::            Y  16
## 165                   myPolitics::         <NA>   2
## 226                       myTech::            N   1
## 166                       myTech::            Y   2
## 222              Readers Respond::            Y   1
## 124                   What We're::            N   4
## 223                   What We're::         <NA>   1
## 106              Readers Respond::            N   6
## 107              Readers Respond::            Y   6
## 125              Readers Respond::         <NA>   4
## 108          Reporter's Notebook::            N   6
## 167          Reporter's Notebook::            Y   2
## 101          Reporter's Notebook::         <NA>   7
## 227                       myMisc::            Y   1
## 169                       myMisc::            Y   2
## 168                       myMisc::         <NA>   2
## 109                     Ask Well::            N   6
## 95                      Ask Well::            Y   9
## 140                     Ask Well::         <NA>   3
## 228                  myEducation::            N   1
## 79                        myFood::            N  15
## 170                       myFood::            Y   2
## 141                       myFood::         <NA>   3
## 45                        myMisc::            N  50
## 15                        myMisc::            Y 108
## 46                        myMisc::         <NA>  49
## 229                       myTech::            N   1
## 230                       myMisc::            N   1
## 231                       myMisc::            N   1
## 232                       myMisc::            N   1
## 112                       myFood::            N   5
## 233                       myFood::         <NA>   1
## 234                    Your Turn::         <NA>   1
## 28                        myMisc::            N  66
## 91                        myMisc::         <NA>  11
## 172                   myPolitics::            N   2
## 235                   myPolitics::            Y   1
## 171                   myPolitics::         <NA>   2
## 173                       myMisc::            N   2
## 174                       myTech::            N   2
## 236                       myTech::         <NA>   1
## 143                  myEducation::            N   3
## 97                   myEducation::            Y   9
## 142                  myEducation::         <NA>   3
## 113                       myFood::            N   5
## 127                       myFood::            Y   4
## 37                        myMisc::            N  60
## 24                        myMisc::            Y  81
## 42                        myMisc::         <NA>  55
## 176                 myMultimedia::            Y   2
## 237                 myMultimedia::         <NA>   1
## 96                     Your Turn::            N   9
## 126                    Your Turn::            Y   4
## 175                    Your Turn::         <NA>   2
## 247               The Daily Gift::            N   1
## 177               The Daily Gift::         <NA>   2
## 48             Today in Politics::            N  44
## 66             Today in Politics::         <NA>  21
## 98                        myFood::            N   9
## 248                       myFood::         <NA>   1
## 17                        myMisc::            N 106
## 249                       myMisc::            Y   1
## 58                        myMisc::         <NA>  30
## 238               .*Fashion Week::            N   1
## 239               .*Fashion Week::         <NA>   1
## 240               .*Fashion Week::            N   1
## 47                .*Fashion Week::            N  46
## 241               .*Fashion Week::         <NA>   1
## 246                       myTech::            Y   1
## 13                .*Fashion Week::            N 136
## 242                  myEducation::            N   1
## 243                  myEducation::            Y   1
## 85                        myFood::            N  13
## 244                       myFood::         <NA>   1
## 4                         myMisc::            N 532
## 102                       myMisc::            Y   7
## 25                        myMisc::         <NA>  78
## 103                 myMultimedia::            N   7
## 128                 myMultimedia::         <NA>   4
## 245                   myPolitics::            N   1
## 61                The Daily Gift::            N  25
## 64                The Daily Gift::         <NA>  22
## 55                      Verbatim::            N  33
## 87                      Verbatim::         <NA>  12
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
##     NewsDesk.nb NewsDesk   SectionName.nb      SectionName
## 87     Business              Business Day     Business Day
## 189    Business              Business Day     Business Day
## 191    Business              Business Day     Business Day
## 138    Business              Business Day     Business Day
## 190    Business              Business Day     Business Day
## 192    Business          Crosswords/Games Crosswords/Games
##              SubsectionName.nb SubsectionName              Headline.pfx
## 87                    Dealbook       Dealbook                  myMisc::
## 189                   Dealbook       Dealbook                  myTech::
## 191             Small Business Small Business                  myMisc::
## 138             Small Business Small Business                  myMisc::
## 190             Small Business Small Business Today in Small Business::
## 192 Business::Crosswords/Games                                 myMisc::
##     Popular.fctr .n
## 87          <NA> 12
## 189         <NA>  1
## 191            N  1
## 138         <NA>  3
## 190         <NA>  1
## 192            N  1
##      NewsDesk.nb NewsDesk SectionName.nb   SectionName
## 220      Foreign  Foreign          World         World
## 152  myEducation                    U.S.              
## 277 myMultimedia    Metro  N.Y. / Region N.Y. / Region
## 83          OpEd                 Opinion       Opinion
## 106       TStyle   TStyle         TStyle              
## 61        TStyle   TStyle         TStyle              
##               SubsectionName.nb    SubsectionName
## 220                Asia Pacific      Asia Pacific
## 152                   Education                  
## 277 myMultimedia::N.Y. / Region                  
## 83            The Public Editor The Public Editor
## 106              TStyle::TStyle                  
## 61               TStyle::TStyle                  
##                       Headline.pfx Popular.fctr .n
## 220                   myPolitics::         <NA>  1
## 152                  On This Day::         <NA>  3
## 277 Pictures of the (Day|Year|.)::         <NA>  1
## 83                        myMisc::            Y 14
## 106                 myMultimedia::            N  7
## 61                The Daily Gift::            N 25
##     NewsDesk.nb NewsDesk SectionName.nb SectionName    SubsectionName.nb
## 133      TStyle   TStyle         TStyle                   TStyle::TStyle
## 261      TStyle   TStyle         TStyle                   TStyle::TStyle
## 61       TStyle   TStyle         TStyle                   TStyle::TStyle
## 65       TStyle   TStyle         TStyle                   TStyle::TStyle
## 55   Verbatim::              Verbatim::             Verbatim::Verbatim::
## 89   Verbatim::              Verbatim::             Verbatim::Verbatim::
##     SubsectionName     Headline.pfx Popular.fctr .n
## 133                  myMultimedia::         <NA>  4
## 261                    myPolitics::            N  1
## 61                 The Daily Gift::            N 25
## 65                 The Daily Gift::         <NA> 22
## 55                       Verbatim::            N 33
## 89                       Verbatim::         <NA> 12
```

```r
print(nrow(ndsecsub_xtab_df))
```

```
## [1] 277
```

```r
write.table(ndsecsub_xtab_df, paste0(glb_out_pfx, "ndsecsub_xtab.csv"), 
            row.names=FALSE)

NDnb_Hdlpfx_xtab_df <- orderBy(reformulate(c("-", ".n", glb_rsp_var, "NewsDesk.nb")),
                          mycreate_sqlxtab_df(glb_entity_df,
                                c("Headline.pfx", "NewsDesk.nb", glb_rsp_var)))
write.table(NDnb_Hdlpfx_xtab_df, paste0(glb_out_pfx, "NDnb_Hdlpfx_xtab.csv"), 
            row.names=FALSE)
myprint_df(NDnb_Hdlpfx_xtab_df)
```

```
##   Headline.pfx NewsDesk.nb Popular.fctr   .n
## 1     myMisc::    Business            N 1021
## 2     myMisc::    myMisc::            N  800
## 3     myMisc::     Culture            N  599
## 4     myMisc::      TStyle            N  532
## 5     myMisc::    Business         <NA>  419
## 6     myMisc::        OpEd            Y  403
##         Headline.pfx NewsDesk.nb Popular.fctr  .n
## 3           myMisc::     Culture            N 599
## 23          myTech::    Business            N  83
## 29  Morning Agenda::    Business            N  62
## 67          myFood::     Science            N  15
## 109   myMultimedia:: myEducation            N   4
## 158    myEducation::      TStyle            Y   1
##       Headline.pfx NewsDesk.nb Popular.fctr .n
## 181       myTech::        OpEd         <NA>  1
## 163       myFood::      Styles         <NA>  1
## 171 myMultimedia::      Styles         <NA>  1
## 183       myTech::      Styles         <NA>  1
## 165       myFood::      Travel         <NA>  1
## 164       myFood::      TStyle         <NA>  1
```

```r
require(tm)
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

```r
# freq of words/symbols in myMisc::Y
print("Patterns in myMisc::Y")
```

```
## [1] "Patterns in myMisc::Y"
```

```r
print(nrow(glb_entity_df[sel_obs(Popular=1, NewsDesk.nb="myMisc::"),]))
```

```
## [1] 97
```

```r
myMisc_hdl <- glb_entity_df[sel_obs(Popular=1, NewsDesk.nb="myMisc::"), "Headline"]
myMisc_hdl_wrds <- c(NULL); rix=1
#myMisc_hdl_wrds <- c(myMisc_hdl_wrds, unlist(strsplit(myMisc_hdl[rix], "\\W+")))
myMisc_hdl_wrds <- unlist(sapply(myMisc_hdl, function(hdl) 
    return(unlist(strsplit(hdl, "\\W+")))))
names(myMisc_hdl_wrds) <- NULL
myMisc_hdl_wrds <- setdiff(tolower(myMisc_hdl_wrds), stopwords("english"))
print(head(sort(myMisc_hdl_wrds_tbl <- table(myMisc_hdl_wrds), decreasing=TRUE), 10))
```

```
## myMisc_hdl_wrds
##              1983     2014     20th      558 academic accusers       ad 
##        1        1        1        1        1        1        1        1 
##    adult affluent 
##        1        1
```

```r
print("Patterns in myMisc::Y|N|NA")
```

```
## [1] "Patterns in myMisc::Y|N|NA"
```

```r
print(nrow(glb_entity_df[sel_obs(NewsDesk.nb="myMisc::"),]))
```

```
## [1] 1096
```

```r
myMisc_hdl <- glb_entity_df[sel_obs(NewsDesk.nb="myMisc::"), "Headline"]
myMisc_hdl_wrds <- c(NULL); rix=1
#myMisc_hdl_wrds <- c(myMisc_hdl_wrds, unlist(strsplit(myMisc_hdl[rix], "\\W+")))
myMisc_hdl_wrds <- unlist(sapply(myMisc_hdl, function(hdl) 
    return(unlist(strsplit(hdl, "\\W+")))))
names(myMisc_hdl_wrds) <- NULL
myMisc_hdl_wrds <- setdiff(tolower(myMisc_hdl_wrds), stopwords("english"))
print(head(sort(myMisc_hdl_wrds_tbl <- table(myMisc_hdl_wrds), decreasing=TRUE), 10))
```

```
## myMisc_hdl_wrds
##         0  000    1   11 12th   16 1600 1868 1874 
##    1    1    1    1    1    1    1    1    1    1
```

```r
# dsp_datagrp(Headline.pfx="What We're::", all=TRUE)
# #dsp_obs(Headline.pfx="What We're::")
# dsp_datagrp(all=TRUE)

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
## 3        cleanse.data          2          1 36.823 68.334  31.511
## 4 manage.missing.data          2          2 68.334     NA      NA
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
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                1870                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                 109                   0 
## [1] "numeric data w/ 0s in : "
##           WordCount             Popular            UniqueID 
##                 109                5439                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                 378                7624 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
## [1] "numeric data w/ Infs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
## [1] "numeric data w/ NaNs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
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
##  WordCount.log        .rnorm          Headline.pfx      
##  Min.   :0.6932   Min.   :-3.881663   Length:8402       
##  1st Qu.:5.2679   1st Qu.:-0.665043   Class :character  
##  Median :5.9480   Median :-0.004510   Mode  :character  
##  Mean   :5.8263   Mean   :-0.006807                     
##  3rd Qu.:6.6067   3rd Qu.: 0.664125                     
##  Max.   :9.2977   Max.   : 3.356092                     
##  NA's   :109                                            
##  NewsDesk.nb        SectionName.nb     SubsectionName.nb 
##  Length:8402        Length:8402        Length:8402       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
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
##  WordCount.log        .rnorm          Headline.pfx      
##  Min.   :0.6931   Min.   :-3.881663   Length:8402       
##  1st Qu.:5.2679   1st Qu.:-0.665043   Class :character  
##  Median :5.9454   Median :-0.004510   Mode  :character  
##  Mean   :5.8239   Mean   :-0.006807                     
##  3rd Qu.:6.6067   3rd Qu.: 0.664125                     
##  Max.   :9.2977   Max.   : 3.356092                     
##                                                         
##  NewsDesk.nb        SectionName.nb     SubsectionName.nb 
##  Length:8402        Length:8402        Length:8402       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
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
##           WordCount             Popular            UniqueID 
##                   0                1870                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                1870                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
## [1] "numeric data w/ 0s in : "
##           WordCount             Popular            UniqueID 
##                 109                5439                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                 378                7624 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
## [1] "numeric data w/ Infs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
## [1] "numeric data w/ NaNs in : "
##           WordCount             Popular            UniqueID 
##                   0                   0                   0 
##        Popular.fctr   PubDate.year.fctr  PubDate.month.fctr 
##                   0                   0                   0 
##   PubDate.date.fctr  PubDate.wkday.fctr       PubDate.wkend 
##                   0                   0                   0 
##   PubDate.hour.fctr PubDate.minute.fctr PubDate.second.fctr 
##                   0                   0                   0 
##       WordCount.log              .rnorm 
##                   0                   0 
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
## 4 manage.missing.data          2          2 68.334 74.572   6.238
## 5         encode.data          2          3 74.573     NA      NA
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
## 5      encode.data          2          3 74.573 74.628   0.055
## 6 extract.features          3          0 74.629     NA      NA
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
## values: 28
```

```
## Warning: Creating factors of string variable: NewsDesk.nb: # of unique
## values: 25
```

```
## Warning: Creating factors of string variable: SectionName.nb: # of unique
## values: 29
```

```
## Warning: Creating factors of string variable: SubsectionName.nb: # of
## unique values: 40
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

![](NYTBlogs_zoo_files/figure-html/extract.features-1.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-2.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-3.png) 

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

![](NYTBlogs_zoo_files/figure-html/extract.features-4.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-5.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-6.png) 

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

![](NYTBlogs_zoo_files/figure-html/extract.features-7.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-8.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-9.png) 

```
## [1] "Binding DTM for Headline..."
```

![](NYTBlogs_zoo_files/figure-html/extract.features-10.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-11.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-12.png) 

```
## [1] "Binding DTM for Snippet..."
```

![](NYTBlogs_zoo_files/figure-html/extract.features-13.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-14.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-15.png) 

```
## [1] "Binding DTM for Abstract..."
```

![](NYTBlogs_zoo_files/figure-html/extract.features-16.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-17.png) ![](NYTBlogs_zoo_files/figure-html/extract.features-18.png) 

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

![](NYTBlogs_zoo_files/figure-html/extract.features-19.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##              label step_major step_minor     bgn    end elapsed
## 6 extract.features          3          0  74.629 163.47  88.841
## 7  select.features          4          0 163.470     NA      NA
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
## WordCount.log                   WordCount.log  0.265800360               0
## WordCount                           WordCount  0.257526549               1
## S.num.words.unq.log       S.num.words.unq.log -0.250796919               0
## A.num.words.unq.log       A.num.words.unq.log -0.250601203               0
## S.num.words.log               S.num.words.log -0.245354135               0
## A.num.words.log               A.num.words.log -0.245073324               0
## S.num.chars.log               S.num.chars.log -0.224692967               0
## A.num.chars.log               A.num.chars.log -0.224548821               0
## S.num.words.unq               S.num.words.unq -0.212102717               1
## A.num.words.unq               A.num.words.unq -0.210242145               1
## SubsectionName.nb.fctr SubsectionName.nb.fctr -0.206432730               0
## S.num.words                       S.num.words -0.206385049               1
## H.num.words.unq.log       H.num.words.unq.log -0.204496360               0
## A.num.words                       A.num.words -0.204211072               1
## H.num.words.log               H.num.words.log -0.200686356               0
## H.num.words.unq               H.num.words.unq -0.189702157               1
## H.num.words                       H.num.words -0.186036895               1
## S.num.chars                       S.num.chars -0.179331806               1
## A.num.chars                       A.num.chars -0.177037425               1
## H.num.chars.log               H.num.chars.log -0.171062360               0
## NewsDesk.nb.fctr             NewsDesk.nb.fctr -0.167013566               0
## H.num.chars                       H.num.chars -0.147211183               1
## SectionName.nb.fctr       SectionName.nb.fctr -0.146001417               0
## PubDate.hour.fctr           PubDate.hour.fctr  0.135436805               0
## H.is.question                   H.is.question  0.129154799               0
## PubDate.wkend                   PubDate.wkend  0.106728760               0
## Headline.pfx.fctr           Headline.pfx.fctr -0.088287365               0
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
## PubDate.minute.fctr       PubDate.minute.fctr -0.034073846               0
## A.can                                   A.can  0.031498867               0
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
## PubDate.second.fctr       PubDate.second.fctr -0.011879458               0
## UniqueID                             UniqueID  0.011824920               1
## PubDate.date.fctr           PubDate.date.fctr -0.011647558               0
## .rnorm                                 .rnorm -0.008703337               0
## S.one                                   S.one  0.006342094               0
## S.state                               S.state  0.006069626               0
## A.state                               A.state  0.005702163               0
## A.one                                   A.one  0.005696039               0
## S.said                                 S.said  0.001363226               0
## A.said                                 A.said  0.001363226               0
## H.has.http                         H.has.http           NA               0
## S.has.http                         S.has.http           NA               0
## PubDate.year.fctr           PubDate.year.fctr           NA               0
##                          cor.y.abs
## Popular                1.000000000
## WordCount.log          0.265800360
## WordCount              0.257526549
## S.num.words.unq.log    0.250796919
## A.num.words.unq.log    0.250601203
## S.num.words.log        0.245354135
## A.num.words.log        0.245073324
## S.num.chars.log        0.224692967
## A.num.chars.log        0.224548821
## S.num.words.unq        0.212102717
## A.num.words.unq        0.210242145
## SubsectionName.nb.fctr 0.206432730
## S.num.words            0.206385049
## H.num.words.unq.log    0.204496360
## A.num.words            0.204211072
## H.num.words.log        0.200686356
## H.num.words.unq        0.189702157
## H.num.words            0.186036895
## S.num.chars            0.179331806
## A.num.chars            0.177037425
## H.num.chars.log        0.171062360
## NewsDesk.nb.fctr       0.167013566
## H.num.chars            0.147211183
## SectionName.nb.fctr    0.146001417
## PubDate.hour.fctr      0.135436805
## H.is.question          0.129154799
## PubDate.wkend          0.106728760
## Headline.pfx.fctr      0.088287365
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
## PubDate.minute.fctr    0.034073846
## A.can                  0.031498867
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
## PubDate.second.fctr    0.011879458
## UniqueID               0.011824920
## PubDate.date.fctr      0.011647558
## .rnorm                 0.008703337
## S.one                  0.006342094
## S.state                0.006069626
## A.state                0.005702163
## A.one                  0.005696039
## S.said                 0.001363226
## A.said                 0.001363226
## H.has.http                      NA
## S.has.http                      NA
## PubDate.year.fctr               NA
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
## [1] "cor(NewsDesk.nb.fctr, SectionName.nb.fctr)=0.9285"
## [1] "cor(Popular.fctr, NewsDesk.nb.fctr)=-0.1670"
## [1] "cor(Popular.fctr, SectionName.nb.fctr)=-0.1460"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified SectionName.nb.fctr as highly correlated with
## NewsDesk.nb.fctr
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
## [1] "cor(NewsDesk.nb.fctr, SubsectionName.nb.fctr)=0.8703"
## [1] "cor(Popular.fctr, NewsDesk.nb.fctr)=-0.1670"
## [1] "cor(Popular.fctr, SubsectionName.nb.fctr)=-0.2064"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified NewsDesk.nb.fctr as highly correlated with
## SubsectionName.nb.fctr
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
## WordCount.log                   WordCount.log  0.265800360               0
## WordCount                           WordCount  0.257526549               1
## PubDate.hour.fctr           PubDate.hour.fctr  0.135436805               0
## H.is.question                   H.is.question  0.129154799               0
## PubDate.wkend                   PubDate.wkend  0.106728760               0
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
## PubDate.second.fctr       PubDate.second.fctr -0.011879458               0
## A.has.http                         A.has.http -0.013592603               0
## S.presid                             S.presid -0.019828826               0
## A.presid                             A.presid -0.019828826               0
## S.take                                 S.take -0.025762398               0
## A.take                                 A.take -0.026086108               0
## PubDate.minute.fctr       PubDate.minute.fctr -0.034073846               0
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
## Headline.pfx.fctr           Headline.pfx.fctr -0.088287365               0
## SectionName.nb.fctr       SectionName.nb.fctr -0.146001417               0
## H.num.chars                       H.num.chars -0.147211183               1
## NewsDesk.nb.fctr             NewsDesk.nb.fctr -0.167013566               0
## H.num.chars.log               H.num.chars.log -0.171062360               0
## A.num.chars                       A.num.chars -0.177037425               1
## S.num.chars                       S.num.chars -0.179331806               1
## H.num.words                       H.num.words -0.186036895               1
## H.num.words.unq               H.num.words.unq -0.189702157               1
## H.num.words.log               H.num.words.log -0.200686356               0
## A.num.words                       A.num.words -0.204211072               1
## H.num.words.unq.log       H.num.words.unq.log -0.204496360               0
## S.num.words                       S.num.words -0.206385049               1
## SubsectionName.nb.fctr SubsectionName.nb.fctr -0.206432730               0
## A.num.words.unq               A.num.words.unq -0.210242145               1
## S.num.words.unq               S.num.words.unq -0.212102717               1
## A.num.chars.log               A.num.chars.log -0.224548821               0
## S.num.chars.log               S.num.chars.log -0.224692967               0
## A.num.words.log               A.num.words.log -0.245073324               0
## S.num.words.log               S.num.words.log -0.245354135               0
## A.num.words.unq.log       A.num.words.unq.log -0.250601203               0
## S.num.words.unq.log       S.num.words.unq.log -0.250796919               0
## H.has.http                         H.has.http           NA               0
## S.has.http                         S.has.http           NA               0
## PubDate.year.fctr           PubDate.year.fctr           NA               0
##                          cor.y.abs          cor.high.X is.ConditionalX.y
## Popular                1.000000000                <NA>                NA
## WordCount.log          0.265800360                <NA>              TRUE
## WordCount              0.257526549                <NA>                NA
## PubDate.hour.fctr      0.135436805                <NA>              TRUE
## H.is.question          0.129154799                <NA>              TRUE
## PubDate.wkend          0.106728760                <NA>              TRUE
## A.can                  0.031498867               S.can              TRUE
## S.can                  0.029999780                <NA>              TRUE
## H.has.ebola            0.025881397                <NA>              TRUE
## S.make                 0.023138853                <NA>              TRUE
## A.make                 0.023138853              S.make              TRUE
## PubDate.month.fctr     0.019148739                <NA>                NA
## UniqueID               0.011824920                <NA>                NA
## S.one                  0.006342094                <NA>              TRUE
## S.state                0.006069626                <NA>              TRUE
## A.state                0.005702163                <NA>              TRUE
## A.one                  0.005696039                <NA>              TRUE
## S.said                 0.001363226                <NA>              TRUE
## A.said                 0.001363226                <NA>              TRUE
## .rnorm                 0.008703337                <NA>              TRUE
## PubDate.date.fctr      0.011647558                <NA>              TRUE
## PubDate.second.fctr    0.011879458                <NA>              TRUE
## A.has.http             0.013592603                <NA>             FALSE
## S.presid               0.019828826                <NA>              TRUE
## A.presid               0.019828826            S.presid              TRUE
## S.take                 0.025762398                <NA>              TRUE
## A.take                 0.026086108              S.take              TRUE
## PubDate.minute.fctr    0.034073846                <NA>              TRUE
## S.new                  0.034948520                <NA>              TRUE
## A.new                  0.035359447               S.new              TRUE
## PubDate.wkday.fctr     0.039801288                <NA>              TRUE
## S.day                  0.045649185                <NA>              TRUE
## A.day                  0.045909684               S.day              TRUE
## H.X2014                0.046206380                <NA>              TRUE
## S.show                 0.048801740                <NA>              TRUE
## A.show                 0.048801740              S.show              TRUE
## S.report               0.050211524                <NA>              TRUE
## A.report               0.050211524            S.report              TRUE
## S.share                0.050329686                <NA>              TRUE
## A.share                0.050329686             S.share              TRUE
## S.year                 0.051146178                <NA>              TRUE
## A.year                 0.051146178              S.year              TRUE
## S.compani              0.053012962                <NA>              TRUE
## A.compani              0.053099633           S.compani              TRUE
## H.new                  0.053121542                <NA>              TRUE
## S.first                0.053388178                <NA>              TRUE
## A.first                0.053388178             S.first              TRUE
## S.time                 0.057595102                <NA>              TRUE
## A.time                 0.057790617              S.time              TRUE
## H.newyork              0.057970095                <NA>              TRUE
## S.articl               0.059520554                <NA>              TRUE
## A.articl               0.059520554            S.articl              TRUE
## S.will                 0.060575493                <NA>              TRUE
## A.will                 0.061025004              S.will              TRUE
## H.day                  0.061669687                <NA>              TRUE
## S.newyork              0.062117105                <NA>              TRUE
## A.newyork              0.062117105           S.newyork              TRUE
## H.today                0.063723058                <NA>              TRUE
## H.report               0.064948102                <NA>              TRUE
## H.X2015                0.066584892                <NA>             FALSE
## S.intern               0.068485701                <NA>              TRUE
## A.intern               0.068485701            S.intern              TRUE
## H.daili                0.069192975                <NA>             FALSE
## H.week                 0.075105216                <NA>              TRUE
## H.fashion              0.081708612              H.week              TRUE
## S.week                 0.084814939                <NA>              TRUE
## A.week                 0.084814939              S.week              TRUE
## S.fashion              0.086446251                <NA>              TRUE
## A.fashion              0.086446251           S.fashion              TRUE
## Headline.pfx.fctr      0.088287365                <NA>              TRUE
## SectionName.nb.fctr    0.146001417                <NA>              TRUE
## H.num.chars            0.147211183                <NA>                NA
## NewsDesk.nb.fctr       0.167013566 SectionName.nb.fctr              TRUE
## H.num.chars.log        0.171062360                <NA>              TRUE
## A.num.chars            0.177037425                <NA>                NA
## S.num.chars            0.179331806                <NA>                NA
## H.num.words            0.186036895                <NA>                NA
## H.num.words.unq        0.189702157                <NA>                NA
## H.num.words.log        0.200686356                <NA>              TRUE
## A.num.words            0.204211072                <NA>                NA
## H.num.words.unq.log    0.204496360     H.num.chars.log              TRUE
## S.num.words            0.206385049                <NA>                NA
## SubsectionName.nb.fctr 0.206432730    NewsDesk.nb.fctr              TRUE
## A.num.words.unq        0.210242145                <NA>                NA
## S.num.words.unq        0.212102717                <NA>                NA
## A.num.chars.log        0.224548821                <NA>              TRUE
## S.num.chars.log        0.224692967     A.num.chars.log              TRUE
## A.num.words.log        0.245073324                <NA>              TRUE
## S.num.words.log        0.245354135     A.num.words.log              TRUE
## A.num.words.unq.log    0.250601203                <NA>              TRUE
## S.num.words.unq.log    0.250796919     S.num.chars.log              TRUE
## H.has.http                      NA                <NA>             FALSE
## S.has.http                      NA                <NA>             FALSE
## PubDate.year.fctr               NA                <NA>             FALSE
##                        is.cor.y.abs.low
## Popular                           FALSE
## WordCount.log                     FALSE
## WordCount                         FALSE
## PubDate.hour.fctr                 FALSE
## H.is.question                     FALSE
## PubDate.wkend                     FALSE
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
## PubDate.second.fctr               FALSE
## A.has.http                        FALSE
## S.presid                          FALSE
## A.presid                          FALSE
## S.take                            FALSE
## A.take                            FALSE
## PubDate.minute.fctr               FALSE
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
## SectionName.nb.fctr               FALSE
## H.num.chars                       FALSE
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
## SubsectionName.nb.fctr            FALSE
## A.num.words.unq                   FALSE
## S.num.words.unq                   FALSE
## A.num.chars.log                   FALSE
## S.num.chars.log                   FALSE
## A.num.words.log                   FALSE
## S.num.words.log                   FALSE
## A.num.words.unq.log               FALSE
## S.num.words.unq.log               FALSE
## H.has.http                           NA
## S.has.http                           NA
## PubDate.year.fctr                    NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn     end elapsed
## 7         select.features          4          0 163.47 174.689   11.22
## 8 partition.data.training          5          0 174.69      NA      NA
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
##              NewsDesk.nb .n.Tst .n.OOB .freqRatio.Tst .freqRatio.OOB
## 1               Business    500    516    0.267379679   0.2508507535
## 2                Culture    243    203    0.129946524   0.0986874088
## 14                  OpEd    205    217    0.109625668   0.1054934370
## 10              myMisc::    199    265    0.106417112   0.1288283909
## 23                TStyle    107    239    0.057219251   0.1161886242
## 5                Foreign    107    121    0.057219251   0.0588235294
## 8            myEducation     93    118    0.049732620   0.0573650948
## 19                Styles     77     75    0.041176471   0.0364608653
## 7                  Metro     66     57    0.035294118   0.0277102577
## 17               Science     57     66    0.030481283   0.0320855615
## 11          myMultimedia     53     38    0.028342246   0.0184735051
## 13          myPolitics::     41     25    0.021925134   0.0121536218
## 22                Travel     31     34    0.016577540   0.0165289256
## 3    Daily Clip Report::     22     18    0.011764706   0.0087506077
## 21   Today in Politics::     21     14    0.011229947   0.0068060282
## 4          First Draft::     14     18    0.007486631   0.0087506077
## 24            Verbatim::     12     11    0.006417112   0.0053475936
## 16 Reporter's Notebook::      7      2    0.003743316   0.0009722897
## 15     Readers Respond::      4      2    0.002139037   0.0009722897
## 6               Magazine      3     10    0.001604278   0.0048614487
## 9          myEducation::      3      4    0.001604278   0.0019445795
## 12        myMultimedia::      3      2    0.001604278   0.0009722897
## 20      The Daily Gift::      2      1    0.001069519   0.0004861449
## 18                Sports     NA      1             NA   0.0004861449
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
## 8 partition.data.training          5          0 174.690 177.934   3.244
## 9              fit.models          6          0 177.935      NA      NA
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
## 1                       0.63                 0.003         0.5
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-1.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-2.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-3.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-4.png) 

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
## 1                      0.351                 0.001   0.4975446
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
## Fitting cp = 0.002 on full training set
```

```
## Loading required package: rpart.plot
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##           CP nsplit rel error
## 1 0.00200267      0         1
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
## 1                       0.75                 0.072         0.5
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##             CP nsplit rel error
## 1 0.0020026702      0 1.0000000
## 2 0.0017801513     19 0.9546061
## 3 0.0013351135     28 0.9385848
## 4 0.0008900757     49 0.9038718
## 5 0.0002225189     54 0.8985314
## 6 0.0000000000     60 0.8971963
## 
## Variable importance
## WordCount.log 
##           100 
## 
## Node number 1: 4475 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (3279 obs) right son=3 (1196 obs)
##   Primary splits:
##       WordCount.log < 6.528688 to the left,  improve=110.2773, (0 missing)
## 
## Node number 2: 3279 observations
##   predicted class=N  expected loss=0.1003355  P(node) =0.7327374
##     class counts:  2950   329
##    probabilities: 0.900 0.100 
## 
## Node number 3: 1196 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.3511706  P(node) =0.2672626
##     class counts:   776   420
##    probabilities: 0.649 0.351 
##   left son=6 (191 obs) right son=7 (1005 obs)
##   Primary splits:
##       WordCount.log < 6.663771 to the left,  improve=2.831356, (0 missing)
## 
## Node number 6: 191 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.2722513  P(node) =0.04268156
##     class counts:   139    52
##    probabilities: 0.728 0.272 
##   left son=12 (74 obs) right son=13 (117 obs)
##   Primary splits:
##       WordCount.log < 6.620739 to the right, improve=2.253431, (0 missing)
## 
## Node number 7: 1005 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.3661692  P(node) =0.224581
##     class counts:   637   368
##    probabilities: 0.634 0.366 
##   left son=14 (86 obs) right son=15 (919 obs)
##   Primary splits:
##       WordCount.log < 7.57327  to the right, improve=3.357867, (0 missing)
## 
## Node number 12: 74 observations
##   predicted class=N  expected loss=0.1756757  P(node) =0.01653631
##     class counts:    61    13
##    probabilities: 0.824 0.176 
## 
## Node number 13: 117 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3333333  P(node) =0.02614525
##     class counts:    78    39
##    probabilities: 0.667 0.333 
##   left son=26 (84 obs) right son=27 (33 obs)
##   Primary splits:
##       WordCount.log < 6.597826 to the left,  improve=0.7597403, (0 missing)
## 
## Node number 14: 86 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.2325581  P(node) =0.01921788
##     class counts:    66    20
##    probabilities: 0.767 0.233 
##   left son=28 (78 obs) right son=29 (8 obs)
##   Primary splits:
##       WordCount.log < 8.229096 to the left,  improve=4.723315, (0 missing)
## 
## Node number 15: 919 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.3786725  P(node) =0.2053631
##     class counts:   571   348
##    probabilities: 0.621 0.379 
##   left son=30 (732 obs) right son=31 (187 obs)
##   Primary splits:
##       WordCount.log < 6.775937 to the right, improve=1.393772, (0 missing)
## 
## Node number 26: 84 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.297619  P(node) =0.01877095
##     class counts:    59    25
##    probabilities: 0.702 0.298 
##   left son=52 (23 obs) right son=53 (61 obs)
##   Primary splits:
##       WordCount.log < 6.579945 to the right, improve=1.770509, (0 missing)
## 
## Node number 27: 33 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.4242424  P(node) =0.007374302
##     class counts:    19    14
##    probabilities: 0.576 0.424 
##   left son=54 (20 obs) right son=55 (13 obs)
##   Primary splits:
##       WordCount.log < 6.605974 to the right, improve=1.567366, (0 missing)
## 
## Node number 28: 78 observations
##   predicted class=N  expected loss=0.1794872  P(node) =0.01743017
##     class counts:    64    14
##    probabilities: 0.821 0.179 
## 
## Node number 29: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001787709
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## Node number 30: 732 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.3647541  P(node) =0.1635754
##     class counts:   465   267
##    probabilities: 0.635 0.365 
##   left son=60 (11 obs) right son=61 (721 obs)
##   Primary splits:
##       WordCount.log < 6.782759 to the left,  improve=2.971658, (0 missing)
## 
## Node number 31: 187 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4331551  P(node) =0.04178771
##     class counts:   106    81
##    probabilities: 0.567 0.433 
##   left son=62 (176 obs) right son=63 (11 obs)
##   Primary splits:
##       WordCount.log < 6.771362 to the left,  improve=2.022059, (0 missing)
## 
## Node number 52: 23 observations
##   predicted class=N  expected loss=0.1304348  P(node) =0.005139665
##     class counts:    20     3
##    probabilities: 0.870 0.130 
## 
## Node number 53: 61 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3606557  P(node) =0.01363128
##     class counts:    39    22
##    probabilities: 0.639 0.361 
##   left son=106 (32 obs) right son=107 (29 obs)
##   Primary splits:
##       WordCount.log < 6.553932 to the left,  improve=0.84882, (0 missing)
## 
## Node number 54: 20 observations
##   predicted class=N  expected loss=0.3  P(node) =0.004469274
##     class counts:    14     6
##    probabilities: 0.700 0.300 
## 
## Node number 55: 13 observations
##   predicted class=Y  expected loss=0.3846154  P(node) =0.002905028
##     class counts:     5     8
##    probabilities: 0.385 0.615 
## 
## Node number 60: 11 observations
##   predicted class=N  expected loss=0  P(node) =0.002458101
##     class counts:    11     0
##    probabilities: 1.000 0.000 
## 
## Node number 61: 721 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.370319  P(node) =0.1611173
##     class counts:   454   267
##    probabilities: 0.630 0.370 
##   left son=122 (516 obs) right son=123 (205 obs)
##   Primary splits:
##       WordCount.log < 7.162785 to the left,  improve=1.990795, (0 missing)
## 
## Node number 62: 176 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4147727  P(node) =0.03932961
##     class counts:   103    73
##    probabilities: 0.585 0.415 
##   left son=124 (125 obs) right son=125 (51 obs)
##   Primary splits:
##       WordCount.log < 6.736373 to the left,  improve=0.8169857, (0 missing)
## 
## Node number 63: 11 observations
##   predicted class=Y  expected loss=0.2727273  P(node) =0.002458101
##     class counts:     3     8
##    probabilities: 0.273 0.727 
## 
## Node number 106: 32 observations
##   predicted class=N  expected loss=0.28125  P(node) =0.007150838
##     class counts:    23     9
##    probabilities: 0.719 0.281 
## 
## Node number 107: 29 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.4482759  P(node) =0.006480447
##     class counts:    16    13
##    probabilities: 0.552 0.448 
##   left son=214 (18 obs) right son=215 (11 obs)
##   Primary splits:
##       WordCount.log < 6.566671 to the right, improve=2.758969, (0 missing)
## 
## Node number 122: 516 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3468992  P(node) =0.1153073
##     class counts:   337   179
##    probabilities: 0.653 0.347 
##   left son=244 (189 obs) right son=245 (327 obs)
##   Primary splits:
##       WordCount.log < 6.982399 to the right, improve=2.635863, (0 missing)
## 
## Node number 123: 205 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4292683  P(node) =0.04581006
##     class counts:   117    88
##    probabilities: 0.571 0.429 
##   left son=246 (196 obs) right son=247 (9 obs)
##   Primary splits:
##       WordCount.log < 7.17434  to the right, improve=2.286649, (0 missing)
## 
## Node number 124: 125 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.384  P(node) =0.02793296
##     class counts:    77    48
##    probabilities: 0.616 0.384 
##   left son=248 (40 obs) right son=249 (85 obs)
##   Primary splits:
##       WordCount.log < 6.713563 to the right, improve=1.397765, (0 missing)
## 
## Node number 125: 51 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4901961  P(node) =0.01139665
##     class counts:    26    25
##    probabilities: 0.510 0.490 
##   left son=250 (39 obs) right son=251 (12 obs)
##   Primary splits:
##       WordCount.log < 6.745823 to the right, improve=2.118401, (0 missing)
## 
## Node number 214: 18 observations
##   predicted class=N  expected loss=0.2777778  P(node) =0.004022346
##     class counts:    13     5
##    probabilities: 0.722 0.278 
## 
## Node number 215: 11 observations
##   predicted class=Y  expected loss=0.2727273  P(node) =0.002458101
##     class counts:     3     8
##    probabilities: 0.273 0.727 
## 
## Node number 244: 189 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2804233  P(node) =0.04223464
##     class counts:   136    53
##    probabilities: 0.720 0.280 
##   left son=488 (8 obs) right son=489 (181 obs)
##   Primary splits:
##       WordCount.log < 7.158125 to the right, improve=1.313806, (0 missing)
## 
## Node number 245: 327 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3853211  P(node) =0.07307263
##     class counts:   201   126
##    probabilities: 0.615 0.385 
##   left son=490 (281 obs) right son=491 (46 obs)
##   Primary splits:
##       WordCount.log < 6.946014 to the left,  improve=0.9247672, (0 missing)
## 
## Node number 246: 196 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4132653  P(node) =0.04379888
##     class counts:   115    81
##    probabilities: 0.587 0.413 
##   left son=492 (111 obs) right son=493 (85 obs)
##   Primary splits:
##       WordCount.log < 7.275172 to the right, improve=0.6230395, (0 missing)
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
##   left son=498 (35 obs) right son=499 (50 obs)
##   Primary splits:
##       WordCount.log < 6.685236 to the left,  improve=1.016807, (0 missing)
## 
## Node number 250: 39 observations
##   predicted class=N  expected loss=0.4102564  P(node) =0.008715084
##     class counts:    23    16
##    probabilities: 0.590 0.410 
## 
## Node number 251: 12 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.002681564
##     class counts:     3     9
##    probabilities: 0.250 0.750 
## 
## Node number 488: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.001787709
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 489: 181 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2928177  P(node) =0.04044693
##     class counts:   128    53
##    probabilities: 0.707 0.293 
##   left son=978 (126 obs) right son=979 (55 obs)
##   Primary splits:
##       WordCount.log < 7.088408 to the left,  improve=1.815294, (0 missing)
## 
## Node number 490: 281 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3701068  P(node) =0.0627933
##     class counts:   177   104
##    probabilities: 0.630 0.370 
##   left son=980 (13 obs) right son=981 (268 obs)
##   Primary splits:
##       WordCount.log < 6.937314 to the right, improve=1.274969, (0 missing)
## 
## Node number 491: 46 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.4782609  P(node) =0.01027933
##     class counts:    24    22
##    probabilities: 0.522 0.478 
##   left son=982 (24 obs) right son=983 (22 obs)
##   Primary splits:
##       WordCount.log < 6.96319  to the right, improve=1.070158, (0 missing)
## 
## Node number 492: 111 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3783784  P(node) =0.02480447
##     class counts:    69    42
##    probabilities: 0.622 0.378 
##   left son=984 (34 obs) right son=985 (77 obs)
##   Primary splits:
##       WordCount.log < 7.345687 to the left,  improve=1.266636, (0 missing)
## 
## Node number 493: 85 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4588235  P(node) =0.01899441
##     class counts:    46    39
##    probabilities: 0.541 0.459 
##   left son=986 (69 obs) right son=987 (16 obs)
##   Primary splits:
##       WordCount.log < 7.257355 to the left,  improve=1.088576, (0 missing)
## 
## Node number 498: 35 observations
##   predicted class=N  expected loss=0.3428571  P(node) =0.007821229
##     class counts:    23    12
##    probabilities: 0.657 0.343 
## 
## Node number 499: 50 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.5  P(node) =0.01117318
##     class counts:    25    25
##    probabilities: 0.500 0.500 
##   left son=998 (41 obs) right son=999 (9 obs)
##   Primary splits:
##       WordCount.log < 6.691463 to the right, improve=1.693767, (0 missing)
## 
## Node number 978: 126 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2460317  P(node) =0.02815642
##     class counts:    95    31
##    probabilities: 0.754 0.246 
##   left son=1956 (9 obs) right son=1957 (117 obs)
##   Primary splits:
##       WordCount.log < 7.080026 to the right, improve=1.173382, (0 missing)
## 
## Node number 979: 55 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.4  P(node) =0.0122905
##     class counts:    33    22
##    probabilities: 0.600 0.400 
##   left son=1958 (43 obs) right son=1959 (12 obs)
##   Primary splits:
##       WordCount.log < 7.100027 to the right, improve=1.031783, (0 missing)
## 
## Node number 980: 13 observations
##   predicted class=N  expected loss=0.1538462  P(node) =0.002905028
##     class counts:    11     2
##    probabilities: 0.846 0.154 
## 
## Node number 981: 268 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.380597  P(node) =0.05988827
##     class counts:   166   102
##    probabilities: 0.619 0.381 
##   left son=1962 (258 obs) right son=1963 (10 obs)
##   Primary splits:
##       WordCount.log < 6.928048 to the left,  improve=1.000069, (0 missing)
## 
## Node number 982: 24 observations
##   predicted class=N  expected loss=0.375  P(node) =0.005363128
##     class counts:    15     9
##    probabilities: 0.625 0.375 
## 
## Node number 983: 22 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.4090909  P(node) =0.004916201
##     class counts:     9    13
##    probabilities: 0.409 0.591 
##   left son=1966 (15 obs) right son=1967 (7 obs)
##   Primary splits:
##       WordCount.log < 6.957497 to the left,  improve=1.455411, (0 missing)
## 
## Node number 984: 34 observations
##   predicted class=N  expected loss=0.2647059  P(node) =0.007597765
##     class counts:    25     9
##    probabilities: 0.735 0.265 
## 
## Node number 985: 77 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4285714  P(node) =0.0172067
##     class counts:    44    33
##    probabilities: 0.571 0.429 
##   left son=1970 (36 obs) right son=1971 (41 obs)
##   Primary splits:
##       WordCount.log < 7.444526 to the right, improve=0.2129307, (0 missing)
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
## Node number 998: 41 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4390244  P(node) =0.009162011
##     class counts:    23    18
##    probabilities: 0.561 0.439 
##   left son=1996 (11 obs) right son=1997 (30 obs)
##   Primary splits:
##       WordCount.log < 6.698884 to the left,  improve=1.989061, (0 missing)
## 
## Node number 999: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.002011173
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 1956: 9 observations
##   predicted class=N  expected loss=0  P(node) =0.002011173
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 1957: 117 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2649573  P(node) =0.02614525
##     class counts:    86    31
##    probabilities: 0.735 0.265 
##   left son=3914 (97 obs) right son=3915 (20 obs)
##   Primary splits:
##       WordCount.log < 7.060476 to the left,  improve=0.8798661, (0 missing)
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
## Node number 1962: 258 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.372093  P(node) =0.05765363
##     class counts:   162    96
##    probabilities: 0.628 0.372 
##   left son=3924 (237 obs) right son=3925 (21 obs)
##   Primary splits:
##       WordCount.log < 6.790659 to the right, improve=1.052413, (0 missing)
## 
## Node number 1963: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.002234637
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 1966: 15 observations
##   predicted class=N  expected loss=0.4666667  P(node) =0.003351955
##     class counts:     8     7
##    probabilities: 0.533 0.467 
## 
## Node number 1967: 7 observations
##   predicted class=Y  expected loss=0.1428571  P(node) =0.001564246
##     class counts:     1     6
##    probabilities: 0.143 0.857 
## 
## Node number 1970: 36 observations
##   predicted class=N  expected loss=0.3888889  P(node) =0.008044693
##     class counts:    22    14
##    probabilities: 0.611 0.389 
## 
## Node number 1971: 41 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4634146  P(node) =0.009162011
##     class counts:    22    19
##    probabilities: 0.537 0.463 
##   left son=3942 (26 obs) right son=3943 (15 obs)
##   Primary splits:
##       WordCount.log < 7.405491 to the left,  improve=0.8825516, (0 missing)
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
## Node number 1996: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 1997: 30 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.4666667  P(node) =0.006703911
##     class counts:    14    16
##    probabilities: 0.467 0.533 
##   left son=3994 (15 obs) right son=3995 (15 obs)
##   Primary splits:
##       WordCount.log < 6.707473 to the left,  improve=0.2666667, (0 missing)
## 
## Node number 3914: 97 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2371134  P(node) =0.02167598
##     class counts:    74    23
##    probabilities: 0.763 0.237 
##   left son=7828 (26 obs) right son=7829 (71 obs)
##   Primary splits:
##       WordCount.log < 7.044469 to the right, improve=0.4925668, (0 missing)
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
## Node number 3924: 237 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3586498  P(node) =0.05296089
##     class counts:   152    85
##    probabilities: 0.641 0.359 
##   left son=7848 (210 obs) right son=7849 (27 obs)
##   Primary splits:
##       WordCount.log < 6.912741 to the left,  improve=0.4485835, (0 missing)
## 
## Node number 3925: 21 observations
##   predicted class=Y  expected loss=0.4761905  P(node) =0.004692737
##     class counts:    10    11
##    probabilities: 0.476 0.524 
## 
## Node number 3942: 26 observations
##   predicted class=N  expected loss=0.3846154  P(node) =0.005810056
##     class counts:    16    10
##    probabilities: 0.615 0.385 
## 
## Node number 3943: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
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
## Node number 3994: 15 observations
##   predicted class=N  expected loss=0.4666667  P(node) =0.003351955
##     class counts:     8     7
##    probabilities: 0.533 0.467 
## 
## Node number 3995: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 7828: 26 observations
##   predicted class=N  expected loss=0.1538462  P(node) =0.005810056
##     class counts:    22     4
##    probabilities: 0.846 0.154 
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
## Node number 7848: 210 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.347619  P(node) =0.04692737
##     class counts:   137    73
##    probabilities: 0.652 0.348 
##   left son=15696 (37 obs) right son=15697 (173 obs)
##   Primary splits:
##       WordCount.log < 6.892134 to the right, improve=1.551009, (0 missing)
## 
## Node number 7849: 27 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4444444  P(node) =0.00603352
##     class counts:    15    12
##    probabilities: 0.556 0.444 
##   left son=15698 (11 obs) right son=15699 (16 obs)
##   Primary splits:
##       WordCount.log < 6.920178 to the right, improve=1.094697, (0 missing)
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
## Node number 15696: 37 observations
##   predicted class=N  expected loss=0.2162162  P(node) =0.008268156
##     class counts:    29     8
##    probabilities: 0.784 0.216 
## 
## Node number 15697: 173 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3757225  P(node) =0.03865922
##     class counts:   108    65
##    probabilities: 0.624 0.376 
##   left son=31394 (163 obs) right son=31395 (10 obs)
##   Primary splits:
##       WordCount.log < 6.884486 to the left,  improve=1.067726, (0 missing)
## 
## Node number 15698: 11 observations
##   predicted class=N  expected loss=0.2727273  P(node) =0.002458101
##     class counts:     8     3
##    probabilities: 0.727 0.273 
## 
## Node number 15699: 16 observations
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
## Node number 31394: 163 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3619632  P(node) =0.03642458
##     class counts:   104    59
##    probabilities: 0.638 0.362 
##   left son=62788 (19 obs) right son=62789 (144 obs)
##   Primary splits:
##       WordCount.log < 6.873163 to the right, improve=0.986443, (0 missing)
## 
## Node number 31395: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.002234637
##     class counts:     4     6
##    probabilities: 0.400 0.600 
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
## Node number 62788: 19 observations
##   predicted class=N  expected loss=0.2105263  P(node) =0.00424581
##     class counts:    15     4
##    probabilities: 0.789 0.211 
## 
## Node number 62789: 144 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3819444  P(node) =0.03217877
##     class counts:    89    55
##    probabilities: 0.618 0.382 
##   left son=125578 (129 obs) right son=125579 (15 obs)
##   Primary splits:
##       WordCount.log < 6.862757 to the left,  improve=2.714793, (0 missing)
## 
## Node number 125578: 129 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3488372  P(node) =0.02882682
##     class counts:    84    45
##    probabilities: 0.651 0.349 
##   left son=251156 (32 obs) right son=251157 (97 obs)
##   Primary splits:
##       WordCount.log < 6.843217 to the right, improve=0.8314553, (0 missing)
## 
## Node number 125579: 15 observations
##   predicted class=Y  expected loss=0.3333333  P(node) =0.003351955
##     class counts:     5    10
##    probabilities: 0.333 0.667 
## 
## Node number 251156: 32 observations
##   predicted class=N  expected loss=0.25  P(node) =0.007150838
##     class counts:    24     8
##    probabilities: 0.750 0.250 
## 
## Node number 251157: 97 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3814433  P(node) =0.02167598
##     class counts:    60    37
##    probabilities: 0.619 0.381 
##   left son=502314 (82 obs) right son=502315 (15 obs)
##   Primary splits:
##       WordCount.log < 6.836796 to the left,  improve=1.695147, (0 missing)
## 
## Node number 502314: 82 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3414634  P(node) =0.01832402
##     class counts:    54    28
##    probabilities: 0.659 0.341 
##   left son=1004628 (11 obs) right son=1004629 (71 obs)
##   Primary splits:
##       WordCount.log < 6.829253 to the right, improve=0.647575, (0 missing)
## 
## Node number 502315: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 1004628: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 1004629: 71 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3661972  P(node) =0.01586592
##     class counts:    45    26
##    probabilities: 0.634 0.366 
##   left son=2009258 (28 obs) right son=2009259 (43 obs)
##   Primary splits:
##       WordCount.log < 6.807382 to the left,  improve=0.5989425, (0 missing)
## 
## Node number 2009258: 28 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.006256983
##     class counts:    20     8
##    probabilities: 0.714 0.286 
## 
## Node number 2009259: 43 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4186047  P(node) =0.009608939
##     class counts:    25    18
##    probabilities: 0.581 0.419 
##   left son=4018518 (35 obs) right son=4018519 (8 obs)
##   Primary splits:
##       WordCount.log < 6.810693 to the right, improve=2.158804, (0 missing)
## 
## Node number 4018518: 35 observations
##   predicted class=N  expected loss=0.3428571  P(node) =0.007821229
##     class counts:    23    12
##    probabilities: 0.657 0.343 
## 
## Node number 4018519: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001787709
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##       1) root 4475 749 N (0.8326257 0.1673743)  
##         2) WordCount.log< 6.528688 3279 329 N (0.8996645 0.1003355) *
##         3) WordCount.log>=6.528688 1196 420 N (0.6488294 0.3511706)  
##           6) WordCount.log< 6.663771 191  52 N (0.7277487 0.2722513)  
##            12) WordCount.log>=6.620739 74  13 N (0.8243243 0.1756757) *
##            13) WordCount.log< 6.620739 117  39 N (0.6666667 0.3333333)  
##              26) WordCount.log< 6.597826 84  25 N (0.7023810 0.2976190)  
##                52) WordCount.log>=6.579945 23   3 N (0.8695652 0.1304348) *
##                53) WordCount.log< 6.579945 61  22 N (0.6393443 0.3606557)  
##                 106) WordCount.log< 6.553932 32   9 N (0.7187500 0.2812500) *
##                 107) WordCount.log>=6.553932 29  13 N (0.5517241 0.4482759)  
##                   214) WordCount.log>=6.566671 18   5 N (0.7222222 0.2777778) *
##                   215) WordCount.log< 6.566671 11   3 Y (0.2727273 0.7272727) *
##              27) WordCount.log>=6.597826 33  14 N (0.5757576 0.4242424)  
##                54) WordCount.log>=6.605974 20   6 N (0.7000000 0.3000000) *
##                55) WordCount.log< 6.605974 13   5 Y (0.3846154 0.6153846) *
##           7) WordCount.log>=6.663771 1005 368 N (0.6338308 0.3661692)  
##            14) WordCount.log>=7.57327 86  20 N (0.7674419 0.2325581)  
##              28) WordCount.log< 8.229096 78  14 N (0.8205128 0.1794872) *
##              29) WordCount.log>=8.229096 8   2 Y (0.2500000 0.7500000) *
##            15) WordCount.log< 7.57327 919 348 N (0.6213275 0.3786725)  
##              30) WordCount.log>=6.775937 732 267 N (0.6352459 0.3647541)  
##                60) WordCount.log< 6.782759 11   0 N (1.0000000 0.0000000) *
##                61) WordCount.log>=6.782759 721 267 N (0.6296810 0.3703190)  
##                 122) WordCount.log< 7.162785 516 179 N (0.6531008 0.3468992)  
##                   244) WordCount.log>=6.982399 189  53 N (0.7195767 0.2804233)  
##                     488) WordCount.log>=7.158125 8   0 N (1.0000000 0.0000000) *
##                     489) WordCount.log< 7.158125 181  53 N (0.7071823 0.2928177)  
##                       978) WordCount.log< 7.088408 126  31 N (0.7539683 0.2460317)  
##                        1956) WordCount.log>=7.080026 9   0 N (1.0000000 0.0000000) *
##                        1957) WordCount.log< 7.080026 117  31 N (0.7350427 0.2649573)  
##                          3914) WordCount.log< 7.060476 97  23 N (0.7628866 0.2371134)  
##                            7828) WordCount.log>=7.044469 26   4 N (0.8461538 0.1538462) *
##                            7829) WordCount.log< 7.044469 71  19 N (0.7323944 0.2676056)  
##                             15658) WordCount.log< 7.025094 52  12 N (0.7692308 0.2307692)  
##                               31316) WordCount.log>=7.013015 11   0 N (1.0000000 0.0000000) *
##                               31317) WordCount.log< 7.013015 41  12 N (0.7073171 0.2926829)  
##                                 62634) WordCount.log< 7.004882 30   6 N (0.8000000 0.2000000) *
##                                 62635) WordCount.log>=7.004882 11   5 Y (0.4545455 0.5454545) *
##                             15659) WordCount.log>=7.025094 19   7 N (0.6315789 0.3684211) *
##                          3915) WordCount.log>=7.060476 20   8 N (0.6000000 0.4000000) *
##                       979) WordCount.log>=7.088408 55  22 N (0.6000000 0.4000000)  
##                        1958) WordCount.log>=7.100027 43  15 N (0.6511628 0.3488372)  
##                          3916) WordCount.log< 7.11192 11   2 N (0.8181818 0.1818182) *
##                          3917) WordCount.log>=7.11192 32  13 N (0.5937500 0.4062500)  
##                            7834) WordCount.log>=7.127693 22   7 N (0.6818182 0.3181818) *
##                            7835) WordCount.log< 7.127693 10   4 Y (0.4000000 0.6000000) *
##                        1959) WordCount.log< 7.100027 12   5 Y (0.4166667 0.5833333) *
##                   245) WordCount.log< 6.982399 327 126 N (0.6146789 0.3853211)  
##                     490) WordCount.log< 6.946014 281 104 N (0.6298932 0.3701068)  
##                       980) WordCount.log>=6.937314 13   2 N (0.8461538 0.1538462) *
##                       981) WordCount.log< 6.937314 268 102 N (0.6194030 0.3805970)  
##                        1962) WordCount.log< 6.928048 258  96 N (0.6279070 0.3720930)  
##                          3924) WordCount.log>=6.790659 237  85 N (0.6413502 0.3586498)  
##                            7848) WordCount.log< 6.912741 210  73 N (0.6523810 0.3476190)  
##                             15696) WordCount.log>=6.892134 37   8 N (0.7837838 0.2162162) *
##                             15697) WordCount.log< 6.892134 173  65 N (0.6242775 0.3757225)  
##                               31394) WordCount.log< 6.884486 163  59 N (0.6380368 0.3619632)  
##                                 62788) WordCount.log>=6.873163 19   4 N (0.7894737 0.2105263) *
##                                 62789) WordCount.log< 6.873163 144  55 N (0.6180556 0.3819444)  
##                                  125578) WordCount.log< 6.862757 129  45 N (0.6511628 0.3488372)  
##                                    251156) WordCount.log>=6.843217 32   8 N (0.7500000 0.2500000) *
##                                    251157) WordCount.log< 6.843217 97  37 N (0.6185567 0.3814433)  
##                                      502314) WordCount.log< 6.836796 82  28 N (0.6585366 0.3414634)  
##                                       1004628) WordCount.log>=6.829253 11   2 N (0.8181818 0.1818182) *
##                                       1004629) WordCount.log< 6.829253 71  26 N (0.6338028 0.3661972)  
##                                         2009258) WordCount.log< 6.807382 28   8 N (0.7142857 0.2857143) *
##                                         2009259) WordCount.log>=6.807382 43  18 N (0.5813953 0.4186047)  
##                                           4018518) WordCount.log>=6.810693 35  12 N (0.6571429 0.3428571) *
##                                           4018519) WordCount.log< 6.810693 8   2 Y (0.2500000 0.7500000) *
##                                      502315) WordCount.log>=6.836796 15   6 Y (0.4000000 0.6000000) *
##                                  125579) WordCount.log>=6.862757 15   5 Y (0.3333333 0.6666667) *
##                               31395) WordCount.log>=6.884486 10   4 Y (0.4000000 0.6000000) *
##                            7849) WordCount.log>=6.912741 27  12 N (0.5555556 0.4444444)  
##                             15698) WordCount.log>=6.920178 11   3 N (0.7272727 0.2727273) *
##                             15699) WordCount.log< 6.920178 16   7 Y (0.4375000 0.5625000) *
##                          3925) WordCount.log< 6.790659 21  10 Y (0.4761905 0.5238095) *
##                        1963) WordCount.log>=6.928048 10   4 Y (0.4000000 0.6000000) *
##                     491) WordCount.log>=6.946014 46  22 N (0.5217391 0.4782609)  
##                       982) WordCount.log>=6.96319 24   9 N (0.6250000 0.3750000) *
##                       983) WordCount.log< 6.96319 22   9 Y (0.4090909 0.5909091)  
##                        1966) WordCount.log< 6.957497 15   7 N (0.5333333 0.4666667) *
##                        1967) WordCount.log>=6.957497 7   1 Y (0.1428571 0.8571429) *
##                 123) WordCount.log>=7.162785 205  88 N (0.5707317 0.4292683)  
##                   246) WordCount.log>=7.17434 196  81 N (0.5867347 0.4132653)  
##                     492) WordCount.log>=7.275172 111  42 N (0.6216216 0.3783784)  
##                       984) WordCount.log< 7.345687 34   9 N (0.7352941 0.2647059) *
##                       985) WordCount.log>=7.345687 77  33 N (0.5714286 0.4285714)  
##                        1970) WordCount.log>=7.444526 36  14 N (0.6111111 0.3888889) *
##                        1971) WordCount.log< 7.444526 41  19 N (0.5365854 0.4634146)  
##                          3942) WordCount.log< 7.405491 26  10 N (0.6153846 0.3846154) *
##                          3943) WordCount.log>=7.405491 15   6 Y (0.4000000 0.6000000) *
##                     493) WordCount.log< 7.275172 85  39 N (0.5411765 0.4588235)  
##                       986) WordCount.log< 7.257355 69  29 N (0.5797101 0.4202899)  
##                        1972) WordCount.log< 7.191805 14   4 N (0.7142857 0.2857143) *
##                        1973) WordCount.log>=7.191805 55  25 N (0.5454545 0.4545455)  
##                          3946) WordCount.log>=7.20897 40  16 N (0.6000000 0.4000000)  
##                            7892) WordCount.log< 7.220371 7   1 N (0.8571429 0.1428571) *
##                            7893) WordCount.log>=7.220371 33  15 N (0.5454545 0.4545455)  
##                             15786) WordCount.log>=7.238497 15   5 N (0.6666667 0.3333333) *
##                             15787) WordCount.log< 7.238497 18   8 Y (0.4444444 0.5555556) *
##                          3947) WordCount.log< 7.20897 15   6 Y (0.4000000 0.6000000) *
##                       987) WordCount.log>=7.257355 16   6 Y (0.3750000 0.6250000) *
##                   247) WordCount.log< 7.17434 9   2 Y (0.2222222 0.7777778) *
##              31) WordCount.log< 6.775937 187  81 N (0.5668449 0.4331551)  
##                62) WordCount.log< 6.771362 176  73 N (0.5852273 0.4147727)  
##                 124) WordCount.log< 6.736373 125  48 N (0.6160000 0.3840000)  
##                   248) WordCount.log>=6.713563 40  11 N (0.7250000 0.2750000) *
##                   249) WordCount.log< 6.713563 85  37 N (0.5647059 0.4352941)  
##                     498) WordCount.log< 6.685236 35  12 N (0.6571429 0.3428571) *
##                     499) WordCount.log>=6.685236 50  25 N (0.5000000 0.5000000)  
##                       998) WordCount.log>=6.691463 41  18 N (0.5609756 0.4390244)  
##                        1996) WordCount.log< 6.698884 11   2 N (0.8181818 0.1818182) *
##                        1997) WordCount.log>=6.698884 30  14 Y (0.4666667 0.5333333)  
##                          3994) WordCount.log< 6.707473 15   7 N (0.5333333 0.4666667) *
##                          3995) WordCount.log>=6.707473 15   6 Y (0.4000000 0.6000000) *
##                       999) WordCount.log< 6.691463 9   2 Y (0.2222222 0.7777778) *
##                 125) WordCount.log>=6.736373 51  25 N (0.5098039 0.4901961)  
##                   250) WordCount.log>=6.745823 39  16 N (0.5897436 0.4102564) *
##                   251) WordCount.log< 6.745823 12   3 Y (0.2500000 0.7500000) *
##                63) WordCount.log>=6.771362 11   3 Y (0.2727273 0.7272727) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-7.png) 

```
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.28891032
## 3        0.2 0.45641646
## 4        0.3 0.44509948
## 5        0.4 0.38371041
## 6        0.5 0.35135135
## 7        0.6 0.26077586
## 8        0.7 0.13834951
## 9        0.8 0.01587302
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3200
## 2            Y                                              372
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              526
## 2                                              377
##          Prediction
## Reference    N    Y
##         N 3200  526
##         Y  372  377
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.993296e-01   3.346778e-01   7.872899e-01   8.109773e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   3.296138e-07 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-9.png) 

```
##    threshold     f.score
## 1        0.0 0.286547272
## 2        0.1 0.282481140
## 3        0.2 0.378590078
## 4        0.3 0.320634921
## 5        0.4 0.200400802
## 6        0.5 0.186836518
## 7        0.6 0.125290023
## 8        0.7 0.077120823
## 9        0.8 0.005747126
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1436
## 2            Y                                              199
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              277
## 2                                              145
##          Prediction
## Reference    N    Y
##         N 1436  277
##         Y  199  145
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   0.7685950413   0.2382228322   0.7497514652   0.7866700226   0.8327661643 
## AccuracyPValue  McnemarPValue 
##   1.0000000000   0.0004166775 
##                    model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart WordCount.log               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.628                 0.061   0.7075116
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4564165        0.7993296
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7872899             0.8109773     0.3346778   0.6459615
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3785901         0.768595
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7497515               0.78667     0.2382228
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
## Fitting cp = 0.002 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-11.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##           CP nsplit rel error
## 1 0.00200267      0         1
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
## 1                      1.866                 0.068         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8149732
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553    0.08110908         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.004766392     0.009878063
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-13.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-14.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-15.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4396  -0.6492  -0.4792  -0.2729   3.2575  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -7.03718    0.33285  -21.14   <2e-16 ***
## WordCount.log  0.89234    0.05228   17.07   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 3667.2  on 4473  degrees of freedom
## AIC: 3671.2
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-17.png) 

```
##    threshold     f.score
## 1        0.0 0.286753446
## 2        0.1 0.354172116
## 3        0.2 0.424053452
## 4        0.3 0.287267081
## 5        0.4 0.080183276
## 6        0.5 0.023225806
## 7        0.6 0.013227513
## 8        0.7 0.005326232
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-18.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 2706
## 2            Y                                  273
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                 1020
## 2                                  476
##          Prediction
## Reference    N    Y
##         N 2706 1020
##         Y  273  476
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.110615e-01   2.586928e-01   6.975347e-01   7.243123e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.325400e-95 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-19.png) 

```
##    threshold     f.score
## 1        0.0 0.286547272
## 2        0.1 0.353071389
## 3        0.2 0.396190476
## 4        0.3 0.329635499
## 5        0.4 0.117924528
## 6        0.5 0.016620499
## 7        0.6 0.011461318
## 8        0.7 0.005797101
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-20.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1215
## 2            Y                                  136
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  498
## 2                                  208
##          Prediction
## Reference    N    Y
##         N 1215  498
##         Y  136  208
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   6.917842e-01   2.210018e-01   6.713234e-01   7.116969e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.282291e-46 
##        model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm WordCount.log               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.233                 0.076   0.7311226
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4240535        0.8308383
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.6975347             0.7243123    0.01384725   0.7322841
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3961905        0.6917842
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.6713234             0.7116969     0.2210018    3671.246
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.001635096     0.008694535
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
## [1] "    indep_vars: WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:SectionName.nb.fctr, WordCount.log:H.num.chars.log, WordCount.log:NewsDesk.nb.fctr, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log"
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
##   297, 3405
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-21.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-22.png) 

```
## Warning: not plotting observations with leverage one:
##   297, 3405
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-23.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -8.49    0.00    0.00    0.00    8.49  
## 
## Coefficients: (23 not defined because of singularities)
##                                                            Estimate
## (Intercept)                                              -2.398e+15
## WordCount.log                                             4.966e+14
## `WordCount.log:S.can`                                    -4.199e+13
## `WordCount.log:S.make`                                   -1.706e+13
## `WordCount.log:S.presid`                                  1.323e+13
## `WordCount.log:S.take`                                    1.157e+12
## `WordCount.log:S.new`                                    -1.318e+13
## `WordCount.log:S.day`                                     2.655e+13
## `WordCount.log:S.show`                                   -6.851e+13
## `WordCount.log:S.report`                                 -6.794e+12
## `WordCount.log:S.share`                                  -5.094e+13
## `WordCount.log:S.year`                                   -2.484e+13
## `WordCount.log:S.compani`                                 8.317e+12
## `WordCount.log:S.first`                                  -3.179e+13
## `WordCount.log:S.time`                                   -2.667e+13
## `WordCount.log:S.articl`                                 -4.887e+13
## `WordCount.log:S.will`                                   -4.735e+13
## `WordCount.log:S.newyork`                                -3.673e+13
## `WordCount.log:S.intern`                                 -4.364e+14
## `WordCount.log:H.week`                                   -1.197e+14
## `WordCount.log:S.week`                                   -1.878e+13
## `WordCount.log:S.fashion`                                -4.130e+13
## `WordCount.log:SectionName.nb.fctrArts`                  -6.246e+13
## `WordCount.log:SectionName.nb.fctrBusiness Day`          -1.003e+14
## `WordCount.log:SectionName.nb.fctrHealth`                 2.271e+13
## `WordCount.log:SectionName.nb.fctrOpinion`               -1.046e+14
## `WordCount.log:SectionName.nb.fctrWorld`                 -2.025e+14
## `WordCount.log:SectionName.nb.fctrStyles`                -7.332e+14
## `WordCount.log:SectionName.nb.fctrTStyle`                -3.634e+14
## `WordCount.log:SectionName.nb.fctrTechnology`            -1.191e+14
## `WordCount.log:SectionName.nb.fctrMagazine`              -7.408e+14
## `WordCount.log:SectionName.nb.fctrMultimedia`            -5.333e+14
## `WordCount.log:SectionName.nb.fctrmyMisc::`              -3.119e+13
## `WordCount.log:SectionName.nb.fctrTravel`                -3.190e+14
## `WordCount.log:SectionName.nb.fctrU.S.`                  -1.193e+14
## `WordCount.log:SectionName.nb.fctrN.Y. / Region`         -1.144e+14
## `WordCount.log:SectionName.nb.fctrDaily Clip Report::`   -3.247e+14
## `WordCount.log:SectionName.nb.fctrOpen`                  -4.618e+14
## `WordCount.log:SectionName.nb.fctrReaders Respond::`     -1.457e+14
## `WordCount.log:SectionName.nb.fctrSports`                 1.415e+13
## `WordCount.log:SectionName.nb.fctrmyEducation::`         -2.192e+14
## `WordCount.log:SectionName.nb.fctrmyPolitics::`          -5.131e+13
## `WordCount.log:SectionName.nb.fctrNational`              -7.182e+14
## `WordCount.log:SectionName.nb.fctrVerbatim::`            -5.014e+14
## `WordCount.log:SectionName.nb.fctrFirst Draft::`         -3.432e+14
## `WordCount.log:SectionName.nb.fctrmyMultimedia::`        -6.103e+14
## `WordCount.log:SectionName.nb.fctrToday in Politics::`   -4.107e+14
## `WordCount.log:SectionName.nb.fctrReporter's Notebook::` -2.547e+14
## `WordCount.log:SectionName.nb.fctrCulture`                       NA
## `WordCount.log:SectionName.nb.fctrThe Daily Gift::`              NA
## `WordCount.log:H.num.chars.log`                          -1.110e+13
## `WordCount.log:NewsDesk.nb.fctrCulture`                          NA
## `WordCount.log:NewsDesk.nb.fctrScience`                          NA
## `WordCount.log:NewsDesk.nb.fctrOpEd`                             NA
## `WordCount.log:NewsDesk.nb.fctrForeign`                          NA
## `WordCount.log:NewsDesk.nb.fctrStyles`                    1.708e+14
## `WordCount.log:NewsDesk.nb.fctrTStyle`                    1.906e+13
## `WordCount.log:NewsDesk.nb.fctrMagazine`                         NA
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia`                     NA
## `WordCount.log:NewsDesk.nb.fctrmyMisc::`                         NA
## `WordCount.log:NewsDesk.nb.fctrTravel`                           NA
## `WordCount.log:NewsDesk.nb.fctrmyEducation`              -2.893e+14
## `WordCount.log:NewsDesk.nb.fctrMetro`                            NA
## `WordCount.log:NewsDesk.nb.fctrDaily Clip Report::`              NA
## `WordCount.log:NewsDesk.nb.fctrReaders Respond::`                NA
## `WordCount.log:NewsDesk.nb.fctrNational`                         NA
## `WordCount.log:NewsDesk.nb.fctrSports`                           NA
## `WordCount.log:NewsDesk.nb.fctrmyEducation::`                    NA
## `WordCount.log:NewsDesk.nb.fctrmyPolitics::`                     NA
## `WordCount.log:NewsDesk.nb.fctrVerbatim::`                       NA
## `WordCount.log:NewsDesk.nb.fctrFirst Draft::`                    NA
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia::`                   NA
## `WordCount.log:NewsDesk.nb.fctrToday in Politics::`              NA
## `WordCount.log:NewsDesk.nb.fctrReporter's Notebook::`            NA
## `WordCount.log:NewsDesk.nb.fctrThe Daily Gift::`                 NA
## `WordCount.log:A.num.chars.log`                          -1.187e+15
## `WordCount.log:A.num.words.log`                          -7.338e+13
## `WordCount.log:S.num.chars.log`                           1.220e+15
##                                                          Std. Error
## (Intercept)                                               6.186e+06
## WordCount.log                                             3.901e+06
## `WordCount.log:S.can`                                     8.375e+05
## `WordCount.log:S.make`                                    8.653e+05
## `WordCount.log:S.presid`                                  9.833e+05
## `WordCount.log:S.take`                                    9.754e+05
## `WordCount.log:S.new`                                     5.972e+05
## `WordCount.log:S.day`                                     9.850e+05
## `WordCount.log:S.show`                                    9.015e+05
## `WordCount.log:S.report`                                  9.280e+05
## `WordCount.log:S.share`                                   9.883e+05
## `WordCount.log:S.year`                                    7.570e+05
## `WordCount.log:S.compani`                                 7.238e+05
## `WordCount.log:S.first`                                   9.629e+05
## `WordCount.log:S.time`                                    6.833e+05
## `WordCount.log:S.articl`                                  1.437e+06
## `WordCount.log:S.will`                                    5.773e+05
## `WordCount.log:S.newyork`                                 7.913e+05
## `WordCount.log:S.intern`                                  1.189e+06
## `WordCount.log:H.week`                                    1.235e+06
## `WordCount.log:S.week`                                    7.930e+05
## `WordCount.log:S.fashion`                                 1.155e+06
## `WordCount.log:SectionName.nb.fctrArts`                   1.363e+06
## `WordCount.log:SectionName.nb.fctrBusiness Day`           1.364e+06
## `WordCount.log:SectionName.nb.fctrHealth`                 1.533e+06
## `WordCount.log:SectionName.nb.fctrOpinion`                1.272e+06
## `WordCount.log:SectionName.nb.fctrWorld`                  1.525e+06
## `WordCount.log:SectionName.nb.fctrStyles`                 8.026e+06
## `WordCount.log:SectionName.nb.fctrTStyle`                 9.065e+06
## `WordCount.log:SectionName.nb.fctrTechnology`             1.502e+06
## `WordCount.log:SectionName.nb.fctrMagazine`               2.849e+06
## `WordCount.log:SectionName.nb.fctrMultimedia`             1.797e+06
## `WordCount.log:SectionName.nb.fctrmyMisc::`               1.323e+06
## `WordCount.log:SectionName.nb.fctrTravel`                 1.828e+06
## `WordCount.log:SectionName.nb.fctrU.S.`                   7.820e+06
## `WordCount.log:SectionName.nb.fctrN.Y. / Region`          1.569e+06
## `WordCount.log:SectionName.nb.fctrDaily Clip Report::`    3.038e+06
## `WordCount.log:SectionName.nb.fctrOpen`                   7.154e+06
## `WordCount.log:SectionName.nb.fctrReaders Respond::`      3.500e+06
## `WordCount.log:SectionName.nb.fctrSports`                 9.388e+06
## `WordCount.log:SectionName.nb.fctrmyEducation::`          7.454e+06
## `WordCount.log:SectionName.nb.fctrmyPolitics::`           2.129e+06
## `WordCount.log:SectionName.nb.fctrNational`               8.839e+06
## `WordCount.log:SectionName.nb.fctrVerbatim::`             4.002e+06
## `WordCount.log:SectionName.nb.fctrFirst Draft::`          2.710e+06
## `WordCount.log:SectionName.nb.fctrmyMultimedia::`         9.691e+06
## `WordCount.log:SectionName.nb.fctrToday in Politics::`    2.104e+06
## `WordCount.log:SectionName.nb.fctrReporter's Notebook::`  4.594e+06
## `WordCount.log:SectionName.nb.fctrCulture`                       NA
## `WordCount.log:SectionName.nb.fctrThe Daily Gift::`              NA
## `WordCount.log:H.num.chars.log`                           5.260e+05
## `WordCount.log:NewsDesk.nb.fctrCulture`                          NA
## `WordCount.log:NewsDesk.nb.fctrScience`                          NA
## `WordCount.log:NewsDesk.nb.fctrOpEd`                             NA
## `WordCount.log:NewsDesk.nb.fctrForeign`                          NA
## `WordCount.log:NewsDesk.nb.fctrStyles`                    7.797e+06
## `WordCount.log:NewsDesk.nb.fctrTStyle`                    8.940e+06
## `WordCount.log:NewsDesk.nb.fctrMagazine`                         NA
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia`                     NA
## `WordCount.log:NewsDesk.nb.fctrmyMisc::`                         NA
## `WordCount.log:NewsDesk.nb.fctrTravel`                           NA
## `WordCount.log:NewsDesk.nb.fctrmyEducation`               7.802e+06
## `WordCount.log:NewsDesk.nb.fctrMetro`                            NA
## `WordCount.log:NewsDesk.nb.fctrDaily Clip Report::`              NA
## `WordCount.log:NewsDesk.nb.fctrReaders Respond::`                NA
## `WordCount.log:NewsDesk.nb.fctrNational`                         NA
## `WordCount.log:NewsDesk.nb.fctrSports`                           NA
## `WordCount.log:NewsDesk.nb.fctrmyEducation::`                    NA
## `WordCount.log:NewsDesk.nb.fctrmyPolitics::`                     NA
## `WordCount.log:NewsDesk.nb.fctrVerbatim::`                       NA
## `WordCount.log:NewsDesk.nb.fctrFirst Draft::`                    NA
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia::`                   NA
## `WordCount.log:NewsDesk.nb.fctrToday in Politics::`              NA
## `WordCount.log:NewsDesk.nb.fctrReporter's Notebook::`            NA
## `WordCount.log:NewsDesk.nb.fctrThe Daily Gift::`                 NA
## `WordCount.log:A.num.chars.log`                           8.503e+06
## `WordCount.log:A.num.words.log`                           1.585e+06
## `WordCount.log:S.num.chars.log`                           8.475e+06
##                                                             z value
## (Intercept)                                              -387743649
## WordCount.log                                             127314563
## `WordCount.log:S.can`                                     -50140855
## `WordCount.log:S.make`                                    -19717227
## `WordCount.log:S.presid`                                   13454255
## `WordCount.log:S.take`                                      1186104
## `WordCount.log:S.new`                                     -22063440
## `WordCount.log:S.day`                                      26958575
## `WordCount.log:S.show`                                    -75989194
## `WordCount.log:S.report`                                   -7321486
## `WordCount.log:S.share`                                   -51543360
## `WordCount.log:S.year`                                    -32821063
## `WordCount.log:S.compani`                                  11490273
## `WordCount.log:S.first`                                   -33016844
## `WordCount.log:S.time`                                    -39040339
## `WordCount.log:S.articl`                                  -34014196
## `WordCount.log:S.will`                                    -82021493
## `WordCount.log:S.newyork`                                 -46414125
## `WordCount.log:S.intern`                                 -367081226
## `WordCount.log:H.week`                                    -96922331
## `WordCount.log:S.week`                                    -23680361
## `WordCount.log:S.fashion`                                 -35760961
## `WordCount.log:SectionName.nb.fctrArts`                   -45816679
## `WordCount.log:SectionName.nb.fctrBusiness Day`           -73548780
## `WordCount.log:SectionName.nb.fctrHealth`                  14815219
## `WordCount.log:SectionName.nb.fctrOpinion`                -82191867
## `WordCount.log:SectionName.nb.fctrWorld`                 -132768936
## `WordCount.log:SectionName.nb.fctrStyles`                 -91356201
## `WordCount.log:SectionName.nb.fctrTStyle`                 -40090500
## `WordCount.log:SectionName.nb.fctrTechnology`             -79242939
## `WordCount.log:SectionName.nb.fctrMagazine`              -260035565
## `WordCount.log:SectionName.nb.fctrMultimedia`            -296854557
## `WordCount.log:SectionName.nb.fctrmyMisc::`               -23570303
## `WordCount.log:SectionName.nb.fctrTravel`                -174535509
## `WordCount.log:SectionName.nb.fctrU.S.`                   -15260031
## `WordCount.log:SectionName.nb.fctrN.Y. / Region`          -72944484
## `WordCount.log:SectionName.nb.fctrDaily Clip Report::`   -106864137
## `WordCount.log:SectionName.nb.fctrOpen`                   -64556595
## `WordCount.log:SectionName.nb.fctrReaders Respond::`      -41626599
## `WordCount.log:SectionName.nb.fctrSports`                   1507275
## `WordCount.log:SectionName.nb.fctrmyEducation::`          -29407541
## `WordCount.log:SectionName.nb.fctrmyPolitics::`           -24102255
## `WordCount.log:SectionName.nb.fctrNational`               -81246401
## `WordCount.log:SectionName.nb.fctrVerbatim::`            -125295583
## `WordCount.log:SectionName.nb.fctrFirst Draft::`         -126642014
## `WordCount.log:SectionName.nb.fctrmyMultimedia::`         -62975152
## `WordCount.log:SectionName.nb.fctrToday in Politics::`   -195232316
## `WordCount.log:SectionName.nb.fctrReporter's Notebook::`  -55436349
## `WordCount.log:SectionName.nb.fctrCulture`                       NA
## `WordCount.log:SectionName.nb.fctrThe Daily Gift::`              NA
## `WordCount.log:H.num.chars.log`                           -21095800
## `WordCount.log:NewsDesk.nb.fctrCulture`                          NA
## `WordCount.log:NewsDesk.nb.fctrScience`                          NA
## `WordCount.log:NewsDesk.nb.fctrOpEd`                             NA
## `WordCount.log:NewsDesk.nb.fctrForeign`                          NA
## `WordCount.log:NewsDesk.nb.fctrStyles`                     21907534
## `WordCount.log:NewsDesk.nb.fctrTStyle`                      2132142
## `WordCount.log:NewsDesk.nb.fctrMagazine`                         NA
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia`                     NA
## `WordCount.log:NewsDesk.nb.fctrmyMisc::`                         NA
## `WordCount.log:NewsDesk.nb.fctrTravel`                           NA
## `WordCount.log:NewsDesk.nb.fctrmyEducation`               -37080859
## `WordCount.log:NewsDesk.nb.fctrMetro`                            NA
## `WordCount.log:NewsDesk.nb.fctrDaily Clip Report::`              NA
## `WordCount.log:NewsDesk.nb.fctrReaders Respond::`                NA
## `WordCount.log:NewsDesk.nb.fctrNational`                         NA
## `WordCount.log:NewsDesk.nb.fctrSports`                           NA
## `WordCount.log:NewsDesk.nb.fctrmyEducation::`                    NA
## `WordCount.log:NewsDesk.nb.fctrmyPolitics::`                     NA
## `WordCount.log:NewsDesk.nb.fctrVerbatim::`                       NA
## `WordCount.log:NewsDesk.nb.fctrFirst Draft::`                    NA
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia::`                   NA
## `WordCount.log:NewsDesk.nb.fctrToday in Politics::`              NA
## `WordCount.log:NewsDesk.nb.fctrReporter's Notebook::`            NA
## `WordCount.log:NewsDesk.nb.fctrThe Daily Gift::`                 NA
## `WordCount.log:A.num.chars.log`                          -139650559
## `WordCount.log:A.num.words.log`                           -46295972
## `WordCount.log:S.num.chars.log`                           143928832
##                                                          Pr(>|z|)    
## (Intercept)                                                <2e-16 ***
## WordCount.log                                              <2e-16 ***
## `WordCount.log:S.can`                                      <2e-16 ***
## `WordCount.log:S.make`                                     <2e-16 ***
## `WordCount.log:S.presid`                                   <2e-16 ***
## `WordCount.log:S.take`                                     <2e-16 ***
## `WordCount.log:S.new`                                      <2e-16 ***
## `WordCount.log:S.day`                                      <2e-16 ***
## `WordCount.log:S.show`                                     <2e-16 ***
## `WordCount.log:S.report`                                   <2e-16 ***
## `WordCount.log:S.share`                                    <2e-16 ***
## `WordCount.log:S.year`                                     <2e-16 ***
## `WordCount.log:S.compani`                                  <2e-16 ***
## `WordCount.log:S.first`                                    <2e-16 ***
## `WordCount.log:S.time`                                     <2e-16 ***
## `WordCount.log:S.articl`                                   <2e-16 ***
## `WordCount.log:S.will`                                     <2e-16 ***
## `WordCount.log:S.newyork`                                  <2e-16 ***
## `WordCount.log:S.intern`                                   <2e-16 ***
## `WordCount.log:H.week`                                     <2e-16 ***
## `WordCount.log:S.week`                                     <2e-16 ***
## `WordCount.log:S.fashion`                                  <2e-16 ***
## `WordCount.log:SectionName.nb.fctrArts`                    <2e-16 ***
## `WordCount.log:SectionName.nb.fctrBusiness Day`            <2e-16 ***
## `WordCount.log:SectionName.nb.fctrHealth`                  <2e-16 ***
## `WordCount.log:SectionName.nb.fctrOpinion`                 <2e-16 ***
## `WordCount.log:SectionName.nb.fctrWorld`                   <2e-16 ***
## `WordCount.log:SectionName.nb.fctrStyles`                  <2e-16 ***
## `WordCount.log:SectionName.nb.fctrTStyle`                  <2e-16 ***
## `WordCount.log:SectionName.nb.fctrTechnology`              <2e-16 ***
## `WordCount.log:SectionName.nb.fctrMagazine`                <2e-16 ***
## `WordCount.log:SectionName.nb.fctrMultimedia`              <2e-16 ***
## `WordCount.log:SectionName.nb.fctrmyMisc::`                <2e-16 ***
## `WordCount.log:SectionName.nb.fctrTravel`                  <2e-16 ***
## `WordCount.log:SectionName.nb.fctrU.S.`                    <2e-16 ***
## `WordCount.log:SectionName.nb.fctrN.Y. / Region`           <2e-16 ***
## `WordCount.log:SectionName.nb.fctrDaily Clip Report::`     <2e-16 ***
## `WordCount.log:SectionName.nb.fctrOpen`                    <2e-16 ***
## `WordCount.log:SectionName.nb.fctrReaders Respond::`       <2e-16 ***
## `WordCount.log:SectionName.nb.fctrSports`                  <2e-16 ***
## `WordCount.log:SectionName.nb.fctrmyEducation::`           <2e-16 ***
## `WordCount.log:SectionName.nb.fctrmyPolitics::`            <2e-16 ***
## `WordCount.log:SectionName.nb.fctrNational`                <2e-16 ***
## `WordCount.log:SectionName.nb.fctrVerbatim::`              <2e-16 ***
## `WordCount.log:SectionName.nb.fctrFirst Draft::`           <2e-16 ***
## `WordCount.log:SectionName.nb.fctrmyMultimedia::`          <2e-16 ***
## `WordCount.log:SectionName.nb.fctrToday in Politics::`     <2e-16 ***
## `WordCount.log:SectionName.nb.fctrReporter's Notebook::`   <2e-16 ***
## `WordCount.log:SectionName.nb.fctrCulture`                     NA    
## `WordCount.log:SectionName.nb.fctrThe Daily Gift::`            NA    
## `WordCount.log:H.num.chars.log`                            <2e-16 ***
## `WordCount.log:NewsDesk.nb.fctrCulture`                        NA    
## `WordCount.log:NewsDesk.nb.fctrScience`                        NA    
## `WordCount.log:NewsDesk.nb.fctrOpEd`                           NA    
## `WordCount.log:NewsDesk.nb.fctrForeign`                        NA    
## `WordCount.log:NewsDesk.nb.fctrStyles`                     <2e-16 ***
## `WordCount.log:NewsDesk.nb.fctrTStyle`                     <2e-16 ***
## `WordCount.log:NewsDesk.nb.fctrMagazine`                       NA    
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia`                   NA    
## `WordCount.log:NewsDesk.nb.fctrmyMisc::`                       NA    
## `WordCount.log:NewsDesk.nb.fctrTravel`                         NA    
## `WordCount.log:NewsDesk.nb.fctrmyEducation`                <2e-16 ***
## `WordCount.log:NewsDesk.nb.fctrMetro`                          NA    
## `WordCount.log:NewsDesk.nb.fctrDaily Clip Report::`            NA    
## `WordCount.log:NewsDesk.nb.fctrReaders Respond::`              NA    
## `WordCount.log:NewsDesk.nb.fctrNational`                       NA    
## `WordCount.log:NewsDesk.nb.fctrSports`                         NA    
## `WordCount.log:NewsDesk.nb.fctrmyEducation::`                  NA    
## `WordCount.log:NewsDesk.nb.fctrmyPolitics::`                   NA    
## `WordCount.log:NewsDesk.nb.fctrVerbatim::`                     NA    
## `WordCount.log:NewsDesk.nb.fctrFirst Draft::`                  NA    
## `WordCount.log:NewsDesk.nb.fctrmyMultimedia::`                 NA    
## `WordCount.log:NewsDesk.nb.fctrToday in Politics::`            NA    
## `WordCount.log:NewsDesk.nb.fctrReporter's Notebook::`          NA    
## `WordCount.log:NewsDesk.nb.fctrThe Daily Gift::`               NA    
## `WordCount.log:A.num.chars.log`                            <2e-16 ***
## `WordCount.log:A.num.words.log`                            <2e-16 ***
## `WordCount.log:S.num.chars.log`                            <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance:  4042.7  on 4474  degrees of freedom
## Residual deviance: 46424.2  on 4420  degrees of freedom
## AIC: 46534
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-24.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-25.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.5098935
## 3        0.2 0.5098935
## 4        0.3 0.5098935
## 5        0.4 0.5098935
## 6        0.5 0.5098935
## 7        0.6 0.5098935
## 8        0.7 0.5098935
## 9        0.8 0.5098935
## 10       0.9 0.5098935
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3496
## 2            Y                                            414
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            230
## 2                                            335
##          Prediction
## Reference    N    Y
##         N 3496  230
##         Y  414  335
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.560894e-01   4.274879e-01   8.454615e-01   8.662502e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.008081e-05   5.545676e-13 
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-26.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-27.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.4752475
## 3        0.2 0.4752475
## 4        0.3 0.4752475
## 5        0.4 0.4752475
## 6        0.5 0.4752475
## 7        0.6 0.4752475
## 8        0.7 0.4752475
## 9        0.8 0.4752475
## 10       0.9 0.4752475
## 11       1.0 0.0000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-28.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1595
## 2            Y                                            200
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            118
## 2                                            144
##          Prediction
## Reference    N    Y
##         N 1595  118
##         Y  200  144
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.454059e-01   3.865379e-01   8.290495e-01   8.607712e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   6.484544e-02   5.565581e-06 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:SectionName.nb.fctr, WordCount.log:H.num.chars.log, WordCount.log:NewsDesk.nb.fctr, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      7.167                 2.122
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.6927673                    0.9       0.5098935        0.8659114
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8454615             0.8662502      0.479082   0.6748598
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.4752475        0.8454059
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8290495             0.8607712     0.3865379    46534.23
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0402338        0.167036
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
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, S.can, H.has.ebola, S.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, SectionName.nb.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log"
## + Fold1: parameter=none
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

```
## Warning: not plotting observations with leverage one:
##   297
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-29.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-30.png) 

```
## Warning: not plotting observations with leverage one:
##   297
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-31.png) 

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
##      Min        1Q    Median        3Q       Max  
## -2.89214  -0.40150  -0.19222  -0.00007   2.99551  
## 
## Coefficients: (9 not defined because of singularities)
##                                                     Estimate Std. Error
## (Intercept)                                        1.101e+00  1.334e+00
## WordCount.log                                      8.677e-01  7.625e-02
## `PubDate.hour.fctr(7.67,15.3]`                     6.472e-01  1.970e-01
## `PubDate.hour.fctr(15.3,23]`                       4.107e-01  2.114e-01
## H.is.question                                      7.685e-01  1.938e-01
## PubDate.wkend                                     -3.588e-01  3.895e-01
## S.can                                             -6.683e-01  2.705e-01
## H.has.ebola                                       -6.697e-01  4.502e-01
## S.make                                            -1.351e-01  2.655e-01
## S.one                                              1.480e+01  6.841e+03
## S.state                                           -1.103e-02  3.080e-01
## A.state                                                   NA         NA
## A.one                                             -1.480e+01  6.841e+03
## S.said                                             6.472e-01  2.584e-01
## A.said                                                    NA         NA
## .rnorm                                            -2.436e-02  5.471e-02
## `PubDate.date.fctr(7,13]`                          6.446e-02  1.711e-01
## `PubDate.date.fctr(13,19]`                        -1.018e-01  1.700e-01
## `PubDate.date.fctr(19,25]`                        -2.650e-01  1.683e-01
## `PubDate.date.fctr(25,31]`                         2.830e-02  1.791e-01
## `PubDate.second.fctr(14.8,29.5]`                   2.322e-02  1.553e-01
## `PubDate.second.fctr(29.5,44.2]`                  -6.190e-02  1.574e-01
## `PubDate.second.fctr(44.2,59.1]`                   3.513e-02  1.566e-01
## S.presid                                           2.679e-02  3.381e-01
## S.take                                            -2.744e-02  3.390e-01
## `PubDate.minute.fctr(14.8,29.5]`                   1.573e-01  1.583e-01
## `PubDate.minute.fctr(29.5,44.2]`                  -4.557e-02  1.535e-01
## `PubDate.minute.fctr(44.2,59.1]`                   1.434e-01  1.643e-01
## S.new                                             -2.128e-01  1.984e-01
## PubDate.wkday.fctr1                               -1.283e+00  4.368e-01
## PubDate.wkday.fctr2                               -1.550e+00  4.677e-01
## PubDate.wkday.fctr3                               -1.427e+00  4.682e-01
## PubDate.wkday.fctr4                               -1.626e+00  4.602e-01
## PubDate.wkday.fctr5                               -1.628e+00  4.706e-01
## PubDate.wkday.fctr6                               -9.772e-01  4.107e-01
## S.day                                              1.297e-01  3.627e-01
## H.X2014                                           -1.618e+00  9.162e-01
## S.show                                            -3.740e-01  3.733e-01
## S.report                                          -8.892e-02  3.257e-01
## S.share                                           -8.758e-01  4.696e-01
## S.year                                            -3.434e-01  2.694e-01
## S.compani                                         -3.276e-01  2.678e-01
## H.new                                             -3.489e-01  3.864e-01
## S.first                                           -7.150e-02  3.737e-01
## S.time                                            -1.928e-01  2.459e-01
## H.newyork                                          7.939e-02  5.953e-01
## S.articl                                          -9.082e-01  6.007e-01
## S.will                                            -4.308e-01  2.113e-01
## H.day                                              7.028e-02  5.201e-01
## S.newyork                                          7.249e-02  3.219e-01
## H.today                                           -1.667e+01  3.913e+03
## H.report                                          -8.461e-01  6.791e-01
## S.intern                                          -3.035e-01  7.124e-01
## H.week                                             2.028e-01  5.510e-01
## S.week                                            -4.157e-01  3.470e-01
## S.fashion                                         -1.920e-01  1.051e+00
## `Headline.pfx.fctrmyMisc::`                        4.990e-02  5.152e-01
## `Headline.pfx.fctr19[0-9][0-9]::`                 -1.436e+01  1.001e+03
## `Headline.pfx.fctrDaily Report::`                 -1.561e+01  1.659e+03
## `Headline.pfx.fctr.*Fashion Week::`               -1.488e+01  9.062e+02
## `Headline.pfx.fctrWhat We're::`                   -1.980e+01  2.003e+03
## `Headline.pfx.fctrPictures of the (Day|Year|.)::` -1.257e+01  1.592e+03
## `Headline.pfx.fctrToday in Small Business::`      -4.061e-01  4.238e+03
## `Headline.pfx.fctrDaily Clip Report::`            -1.792e+01  1.613e+03
## `Headline.pfx.fctrMorning Agenda::`               -1.687e+01  1.576e+03
## `Headline.pfx.fctrNew York Today::`                1.486e+01  3.913e+03
## `Headline.pfx.fctr6 Q's About the News::`         -1.765e+01  1.677e+03
## `Headline.pfx.fctrTest Yourself::`                -1.582e+01  1.639e+03
## `Headline.pfx.fctrWord of the Day::`              -1.705e+01  1.632e+03
## `Headline.pfx.fctrmyMultimedia::`                 -1.196e+00  1.214e+00
## `Headline.pfx.fctrmyTech::`                        2.272e-01  6.237e-01
## `Headline.pfx.fctrmyPolitics::`                    1.267e+00  7.951e-01
## `Headline.pfx.fctrmyFood::`                       -8.066e-01  6.967e-01
## `Headline.pfx.fctrYour Turn::`                     4.153e+00  1.212e+00
## `Headline.pfx.fctrReaders Respond::`               1.667e-01  1.221e+00
## `Headline.pfx.fctrAsk Well::`                      1.639e-01  1.111e+00
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`          1.730e+00  1.563e+00
## `Headline.pfx.fctrOn This Day::`                  -1.864e+01  3.549e+03
## `Headline.pfx.fctrVerbatim::`                     -1.868e+01  2.211e+03
## `Headline.pfx.fctrFirst Draft::`                  -1.949e+01  1.537e+03
## `Headline.pfx.fctrToday in Politics::`            -4.321e+00  4.362e+03
## `Headline.pfx.fctrReporter's Notebook::`          -1.634e+00  1.459e+00
## `Headline.pfx.fctrThe Daily Gift::`               -1.420e+01  2.408e+03
## SectionName.nb.fctrArts                           -3.258e+00  4.008e-01
## `SectionName.nb.fctrBusiness Day`                 -3.207e+00  4.000e-01
## SectionName.nb.fctrHealth                         -2.208e-01  4.046e-01
## SectionName.nb.fctrOpinion                        -5.637e-01  3.570e-01
## SectionName.nb.fctrWorld                          -5.145e+00  8.163e-01
## SectionName.nb.fctrStyles                         -2.052e+01  1.414e+03
## SectionName.nb.fctrTStyle                         -4.538e+00  5.265e-01
## SectionName.nb.fctrTechnology                     -2.195e+00  4.313e-01
## SectionName.nb.fctrMagazine                       -2.010e+01  2.131e+03
## SectionName.nb.fctrMultimedia                     -4.754e+00  1.084e+00
## `SectionName.nb.fctrmyMisc::`                     -2.903e+00  3.747e-01
## SectionName.nb.fctrTravel                         -4.743e+00  1.064e+00
## SectionName.nb.fctrU.S.                           -1.435e+00  4.073e-01
## `SectionName.nb.fctrN.Y. / Region`                -2.417e+00  4.997e-01
## `SectionName.nb.fctrDaily Clip Report::`                  NA         NA
## SectionName.nb.fctrOpen                           -2.080e+01  7.518e+03
## `SectionName.nb.fctrReaders Respond::`            -5.861e-01  1.335e+00
## SectionName.nb.fctrSports                         -2.012e+01  1.075e+04
## `SectionName.nb.fctrmyEducation::`                -7.348e-01  1.778e+00
## `SectionName.nb.fctrmyPolitics::`                 -4.333e+00  8.519e-01
## SectionName.nb.fctrNational                       -1.929e+01  7.445e+03
## `SectionName.nb.fctrVerbatim::`                           NA         NA
## `SectionName.nb.fctrFirst Draft::`                        NA         NA
## `SectionName.nb.fctrmyMultimedia::`               -1.716e+01  7.568e+03
## `SectionName.nb.fctrToday in Politics::`                  NA         NA
## `SectionName.nb.fctrReporter's Notebook::`                NA         NA
## SectionName.nb.fctrCulture                                NA         NA
## `SectionName.nb.fctrThe Daily Gift::`                     NA         NA
## H.num.chars.log                                   -1.588e-01  3.217e-01
## H.num.words.log                                   -1.271e-01  3.771e-01
## A.num.chars.log                                   -9.346e-01  4.324e-01
## A.num.words.log                                    1.891e+00  1.375e+00
## A.num.words.unq.log                               -1.608e+00  1.316e+00
##                                                   z value Pr(>|z|)    
## (Intercept)                                         0.826 0.408982    
## WordCount.log                                      11.378  < 2e-16 ***
## `PubDate.hour.fctr(7.67,15.3]`                      3.285 0.001018 ** 
## `PubDate.hour.fctr(15.3,23]`                        1.942 0.052081 .  
## H.is.question                                       3.966 7.31e-05 ***
## PubDate.wkend                                      -0.921 0.356884    
## S.can                                              -2.470 0.013497 *  
## H.has.ebola                                        -1.487 0.136883    
## S.make                                             -0.509 0.611041    
## S.one                                               0.002 0.998274    
## S.state                                            -0.036 0.971437    
## A.state                                                NA       NA    
## A.one                                              -0.002 0.998274    
## S.said                                              2.505 0.012250 *  
## A.said                                                 NA       NA    
## .rnorm                                             -0.445 0.656069    
## `PubDate.date.fctr(7,13]`                           0.377 0.706418    
## `PubDate.date.fctr(13,19]`                         -0.599 0.549205    
## `PubDate.date.fctr(19,25]`                         -1.575 0.115212    
## `PubDate.date.fctr(25,31]`                          0.158 0.874449    
## `PubDate.second.fctr(14.8,29.5]`                    0.150 0.881156    
## `PubDate.second.fctr(29.5,44.2]`                   -0.393 0.694143    
## `PubDate.second.fctr(44.2,59.1]`                    0.224 0.822498    
## S.presid                                            0.079 0.936842    
## S.take                                             -0.081 0.935473    
## `PubDate.minute.fctr(14.8,29.5]`                    0.994 0.320325    
## `PubDate.minute.fctr(29.5,44.2]`                   -0.297 0.766609    
## `PubDate.minute.fctr(44.2,59.1]`                    0.873 0.382868    
## S.new                                              -1.072 0.283573    
## PubDate.wkday.fctr1                                -2.937 0.003314 ** 
## PubDate.wkday.fctr2                                -3.314 0.000918 ***
## PubDate.wkday.fctr3                                -3.049 0.002297 ** 
## PubDate.wkday.fctr4                                -3.533 0.000410 ***
## PubDate.wkday.fctr5                                -3.460 0.000540 ***
## PubDate.wkday.fctr6                                -2.380 0.017333 *  
## S.day                                               0.358 0.720627    
## H.X2014                                            -1.766 0.077329 .  
## S.show                                             -1.002 0.316454    
## S.report                                           -0.273 0.784818    
## S.share                                            -1.865 0.062169 .  
## S.year                                             -1.274 0.202521    
## S.compani                                          -1.223 0.221264    
## H.new                                              -0.903 0.366636    
## S.first                                            -0.191 0.848272    
## S.time                                             -0.784 0.433005    
## H.newyork                                           0.133 0.893906    
## S.articl                                           -1.512 0.130588    
## S.will                                             -2.038 0.041508 *  
## H.day                                               0.135 0.892502    
## S.newyork                                           0.225 0.821802    
## H.today                                            -0.004 0.996600    
## H.report                                           -1.246 0.212780    
## S.intern                                           -0.426 0.670056    
## H.week                                              0.368 0.712834    
## S.week                                             -1.198 0.230893    
## S.fashion                                          -0.183 0.854953    
## `Headline.pfx.fctrmyMisc::`                         0.097 0.922834    
## `Headline.pfx.fctr19[0-9][0-9]::`                  -0.014 0.988551    
## `Headline.pfx.fctrDaily Report::`                  -0.009 0.992491    
## `Headline.pfx.fctr.*Fashion Week::`                -0.016 0.986895    
## `Headline.pfx.fctrWhat We're::`                    -0.010 0.992112    
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`  -0.008 0.993702    
## `Headline.pfx.fctrToday in Small Business::`        0.000 0.999924    
## `Headline.pfx.fctrDaily Clip Report::`             -0.011 0.991136    
## `Headline.pfx.fctrMorning Agenda::`                -0.011 0.991458    
## `Headline.pfx.fctrNew York Today::`                 0.004 0.996969    
## `Headline.pfx.fctr6 Q's About the News::`          -0.011 0.991603    
## `Headline.pfx.fctrTest Yourself::`                 -0.010 0.992300    
## `Headline.pfx.fctrWord of the Day::`               -0.010 0.991661    
## `Headline.pfx.fctrmyMultimedia::`                  -0.985 0.324649    
## `Headline.pfx.fctrmyTech::`                         0.364 0.715669    
## `Headline.pfx.fctrmyPolitics::`                     1.593 0.111089    
## `Headline.pfx.fctrmyFood::`                        -1.158 0.246973    
## `Headline.pfx.fctrYour Turn::`                      3.425 0.000614 ***
## `Headline.pfx.fctrReaders Respond::`                0.136 0.891439    
## `Headline.pfx.fctrAsk Well::`                       0.148 0.882731    
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`           1.107 0.268303    
## `Headline.pfx.fctrOn This Day::`                   -0.005 0.995809    
## `Headline.pfx.fctrVerbatim::`                      -0.008 0.993257    
## `Headline.pfx.fctrFirst Draft::`                   -0.013 0.989883    
## `Headline.pfx.fctrToday in Politics::`             -0.001 0.999210    
## `Headline.pfx.fctrReporter's Notebook::`           -1.120 0.262652    
## `Headline.pfx.fctrThe Daily Gift::`                -0.006 0.995294    
## SectionName.nb.fctrArts                            -8.128 4.38e-16 ***
## `SectionName.nb.fctrBusiness Day`                  -8.017 1.08e-15 ***
## SectionName.nb.fctrHealth                          -0.546 0.585184    
## SectionName.nb.fctrOpinion                         -1.579 0.114383    
## SectionName.nb.fctrWorld                           -6.303 2.91e-10 ***
## SectionName.nb.fctrStyles                          -0.015 0.988422    
## SectionName.nb.fctrTStyle                          -8.619  < 2e-16 ***
## SectionName.nb.fctrTechnology                      -5.090 3.58e-07 ***
## SectionName.nb.fctrMagazine                        -0.009 0.992474    
## SectionName.nb.fctrMultimedia                      -4.387 1.15e-05 ***
## `SectionName.nb.fctrmyMisc::`                      -7.748 9.33e-15 ***
## SectionName.nb.fctrTravel                          -4.459 8.23e-06 ***
## SectionName.nb.fctrU.S.                            -3.522 0.000428 ***
## `SectionName.nb.fctrN.Y. / Region`                 -4.836 1.32e-06 ***
## `SectionName.nb.fctrDaily Clip Report::`               NA       NA    
## SectionName.nb.fctrOpen                            -0.003 0.997793    
## `SectionName.nb.fctrReaders Respond::`             -0.439 0.660681    
## SectionName.nb.fctrSports                          -0.002 0.998507    
## `SectionName.nb.fctrmyEducation::`                 -0.413 0.679481    
## `SectionName.nb.fctrmyPolitics::`                  -5.087 3.64e-07 ***
## SectionName.nb.fctrNational                        -0.003 0.997932    
## `SectionName.nb.fctrVerbatim::`                        NA       NA    
## `SectionName.nb.fctrFirst Draft::`                     NA       NA    
## `SectionName.nb.fctrmyMultimedia::`                -0.002 0.998191    
## `SectionName.nb.fctrToday in Politics::`               NA       NA    
## `SectionName.nb.fctrReporter's Notebook::`             NA       NA    
## SectionName.nb.fctrCulture                             NA       NA    
## `SectionName.nb.fctrThe Daily Gift::`                  NA       NA    
## H.num.chars.log                                    -0.493 0.621675    
## H.num.words.log                                    -0.337 0.736093    
## A.num.chars.log                                    -2.161 0.030673 *  
## A.num.words.log                                     1.375 0.169124    
## A.num.words.unq.log                                -1.222 0.221647    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 2232.2  on 4368  degrees of freedom
## AIC: 2446.2
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-32.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-33.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6074968
## 3        0.2 0.6840066
## 4        0.3 0.6849657
## 5        0.4 0.6770904
## 6        0.5 0.6490455
## 7        0.6 0.6038339
## 8        0.7 0.5141844
## 9        0.8 0.3658537
## 10       0.9 0.1176471
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3421
## 2            Y                                  200
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  305
## 2                                  549
##          Prediction
## Reference    N    Y
##         N 3421  305
##         Y  200  549
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.871508e-01   6.165891e-01   8.775150e-01   8.962781e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   6.226813e-25   3.693223e-06 
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-34.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-35.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.5784225
## 3        0.2 0.6400000
## 4        0.3 0.6675427
## 5        0.4 0.6800574
## 6        0.5 0.6444444
## 7        0.6 0.5969125
## 8        0.7 0.5313093
## 9        0.8 0.3948498
## 10       0.9 0.1361257
## 11       1.0 0.0000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-36.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1597
## 2            Y                                  107
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  116
## 2                                  237
##          Prediction
## Reference    N    Y
##         N 1597  116
##         Y  107  237
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.915897e-01   6.148086e-01   8.773491e-01   9.047029e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   2.933301e-14   5.921523e-01 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, S.can, H.has.ebola, S.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, SectionName.nb.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      9.685                 2.821
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9296428                    0.3       0.6849657           0.8876
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.877515             0.8962781     0.5644997   0.9130877
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.6800574        0.8915897
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8773491             0.9047029     0.6148086    2446.224
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.008887506      0.03254153
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 9  fit.models          6          0 177.935 230.352  52.417
## 10 fit.models          6          1 230.353      NA      NA
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
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
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
##   1693, 3596, 3794, 4229, 4338, 4394
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-1.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_1-2.png) 

```
## Warning: not plotting observations with leverage one:
##   1693, 3596, 3794, 4229, 4338, 4394
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-3.png) 

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
## Coefficients: (78 not defined because of singularities)
##                                                                      Estimate
## (Intercept)                                                         1.114e+14
## WordCount.log                                                       3.354e+14
## `PubDate.hour.fctr(7.67,15.3]`                                      2.625e+13
## `PubDate.hour.fctr(15.3,23]`                                       -2.864e+13
## H.is.question                                                       9.650e+13
## PubDate.wkend                                                      -1.058e+13
## A.can                                                               7.066e+15
## S.can                                                              -7.485e+15
## H.has.ebola                                                        -2.713e+14
## S.make                                                              2.047e+13
## A.make                                                                     NA
## S.one                                                              -2.194e+15
## S.state                                                             7.238e+13
## A.state                                                                    NA
## A.one                                                               2.245e+15
## S.said                                                              3.240e+14
## A.said                                                                     NA
## .rnorm                                                              1.287e+12
## `PubDate.date.fctr(7,13]`                                           2.527e+13
## `PubDate.date.fctr(13,19]`                                          1.969e+13
## `PubDate.date.fctr(19,25]`                                          4.435e+12
## `PubDate.date.fctr(25,31]`                                          5.884e+13
## `PubDate.second.fctr(14.8,29.5]`                                   -1.141e+14
## `PubDate.second.fctr(29.5,44.2]`                                   -8.437e+13
## `PubDate.second.fctr(44.2,59.1]`                                   -7.397e+13
## S.presid                                                           -4.214e+13
## A.presid                                                                   NA
## S.take                                                             -2.138e+15
## A.take                                                              2.261e+15
## `PubDate.minute.fctr(14.8,29.5]`                                   -2.361e+13
## `PubDate.minute.fctr(29.5,44.2]`                                   -7.693e+13
## `PubDate.minute.fctr(44.2,59.1]`                                    2.236e+13
## S.new                                                               1.037e+15
## A.new                                                              -1.073e+15
## PubDate.wkday.fctr1                                                 8.902e+12
## PubDate.wkday.fctr2                                                -9.685e+13
## PubDate.wkday.fctr3                                                -8.142e+13
## PubDate.wkday.fctr4                                                -8.666e+13
## PubDate.wkday.fctr5                                                -9.711e+13
## PubDate.wkday.fctr6                                                -1.381e+14
## S.day                                                              -1.093e+15
## A.day                                                               1.174e+15
## H.X2014                                                            -2.429e+14
## S.show                                                             -1.055e+14
## A.show                                                                     NA
## S.report                                                            1.418e+14
## A.report                                                                   NA
## S.share                                                            -3.262e+14
## A.share                                                                    NA
## S.year                                                             -1.727e+14
## A.year                                                                     NA
## S.compani                                                           1.596e+14
## A.compani                                                                  NA
## H.new                                                              -1.549e+14
## S.first                                                            -1.015e+14
## A.first                                                                    NA
## S.time                                                              5.302e+15
## A.time                                                             -5.289e+15
## H.newyork                                                           5.379e+13
## S.articl                                                           -4.203e+14
## A.articl                                                                   NA
## S.will                                                             -5.193e+13
## A.will                                                             -1.496e+14
## H.day                                                              -1.229e+14
## S.newyork                                                          -2.172e+13
## A.newyork                                                                  NA
## H.today                                                            -3.220e+15
## H.report                                                           -2.823e+14
## S.intern                                                            1.298e+14
## A.intern                                                                   NA
## H.week                                                             -6.385e+13
## H.fashion                                                          -6.428e+13
## S.week                                                             -1.562e+14
## A.week                                                                     NA
## S.fashion                                                           5.450e+14
## A.fashion                                                                  NA
## `Headline.pfx.fctrmyMisc::`                                        -1.521e+14
## `Headline.pfx.fctr19[0-9][0-9]::`                                   1.252e+14
## `Headline.pfx.fctrDaily Report::`                                  -4.742e+14
## `Headline.pfx.fctr.*Fashion Week::`                                 1.859e+15
## `Headline.pfx.fctrWhat We're::`                                    -5.013e+15
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                  -7.506e+14
## `Headline.pfx.fctrToday in Small Business::`                        3.135e+15
## `Headline.pfx.fctrDaily Clip Report::`                             -5.547e+15
## `Headline.pfx.fctrMorning Agenda::`                                -1.499e+15
## `Headline.pfx.fctrNew York Today::`                                 2.614e+15
## `Headline.pfx.fctr6 Q's About the News::`                           2.765e+15
## `Headline.pfx.fctrTest Yourself::`                                  3.806e+15
## `Headline.pfx.fctrWord of the Day::`                                1.049e+15
## `Headline.pfx.fctrmyMultimedia::`                                  -8.396e+14
## `Headline.pfx.fctrmyTech::`                                         3.577e+14
## `Headline.pfx.fctrmyPolitics::`                                    -3.135e+13
## `Headline.pfx.fctrmyFood::`                                        -9.362e+14
## `Headline.pfx.fctrYour Turn::`                                     -4.220e+14
## `Headline.pfx.fctrReaders Respond::`                               -2.556e+14
## `Headline.pfx.fctrAsk Well::`                                      -7.060e+14
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           4.725e+15
## `Headline.pfx.fctrOn This Day::`                                    4.007e+15
## `Headline.pfx.fctrVerbatim::`                                      -3.499e+15
## `Headline.pfx.fctrFirst Draft::`                                   -5.732e+15
## `Headline.pfx.fctrToday in Politics::`                             -7.407e+14
## `Headline.pfx.fctrReporter's Notebook::`                           -1.557e+15
## `Headline.pfx.fctrThe Daily Gift::`                                -4.210e+14
## SectionName.nb.fctrArts                                            -2.565e+15
## `SectionName.nb.fctrBusiness Day`                                  -4.122e+15
## SectionName.nb.fctrHealth                                          -5.326e+14
## SectionName.nb.fctrOpinion                                         -5.235e+14
## SectionName.nb.fctrWorld                                           -3.531e+15
## SectionName.nb.fctrStyles                                          -9.600e+15
## SectionName.nb.fctrTStyle                                          -1.026e+16
## SectionName.nb.fctrTechnology                                      -3.194e+15
## SectionName.nb.fctrMagazine                                        -3.796e+15
## SectionName.nb.fctrMultimedia                                      -3.547e+15
## `SectionName.nb.fctrmyMisc::`                                      -2.663e+15
## SectionName.nb.fctrTravel                                          -3.120e+15
## SectionName.nb.fctrU.S.                                            -3.614e+15
## `SectionName.nb.fctrN.Y. / Region`                                 -2.667e+15
## `SectionName.nb.fctrDaily Clip Report::`                                   NA
## SectionName.nb.fctrOpen                                            -4.027e+15
## `SectionName.nb.fctrReaders Respond::`                              2.174e+14
## SectionName.nb.fctrSports                                          -1.547e+15
## `SectionName.nb.fctrmyEducation::`                                 -1.699e+15
## `SectionName.nb.fctrmyPolitics::`                                  -2.861e+15
## SectionName.nb.fctrNational                                        -3.510e+15
## `SectionName.nb.fctrVerbatim::`                                            NA
## `SectionName.nb.fctrFirst Draft::`                                         NA
## `SectionName.nb.fctrmyMultimedia::`                                -1.773e+15
## `SectionName.nb.fctrToday in Politics::`                                   NA
## `SectionName.nb.fctrReporter's Notebook::`                                 NA
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                                      NA
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrScience                                                    NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrStyles                                              3.362e+15
## NewsDesk.nb.fctrTStyle                                              5.103e+15
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrTravel                                                     NA
## NewsDesk.nb.fctrmyEducation                                        -2.814e+15
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## H.num.chars.log                                                    -3.555e+13
## H.num.words.log                                                     9.193e+14
## H.num.words.unq.log                                                -7.739e+14
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      1.421e+15
## `SubsectionName.nb.fctrScience::Health`                                    NA
## `SubsectionName.nb.fctrOpEd::Opinion`                              -4.664e+12
## `SubsectionName.nb.fctrRoom For Debate`                            -4.471e+15
## `SubsectionName.nb.fctrForeign::World`                             -2.585e+15
## `SubsectionName.nb.fctrFashion & Style`                                    NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrAsia Pacific`                                       NA
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
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                             NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                                 NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## A.num.chars.log                                                    -5.957e+15
## S.num.chars.log                                                     5.906e+15
## A.num.words.log                                                    -3.865e+16
## S.num.words.log                                                     3.859e+16
## A.num.words.unq.log                                                 3.854e+16
## S.num.words.unq.log                                                -3.852e+16
##                                                                    Std. Error
## (Intercept)                                                         2.822e+07
## WordCount.log                                                       1.268e+06
## `PubDate.hour.fctr(7.67,15.3]`                                      3.649e+06
## `PubDate.hour.fctr(15.3,23]`                                        3.943e+06
## H.is.question                                                       4.716e+06
## PubDate.wkend                                                       7.528e+06
## A.can                                                               7.819e+07
## S.can                                                               7.876e+07
## H.has.ebola                                                         9.135e+06
## S.make                                                              5.285e+06
## A.make                                                                     NA
## S.one                                                               1.076e+08
## S.state                                                             5.586e+06
## A.state                                                                    NA
## A.one                                                               1.077e+08
## S.said                                                              5.267e+06
## A.said                                                                     NA
## .rnorm                                                              1.010e+06
## `PubDate.date.fctr(7,13]`                                           3.162e+06
## `PubDate.date.fctr(13,19]`                                          3.122e+06
## `PubDate.date.fctr(19,25]`                                          3.039e+06
## `PubDate.date.fctr(25,31]`                                          3.384e+06
## `PubDate.second.fctr(14.8,29.5]`                                    2.872e+06
## `PubDate.second.fctr(29.5,44.2]`                                    2.860e+06
## `PubDate.second.fctr(44.2,59.1]`                                    2.873e+06
## S.presid                                                            5.760e+06
## A.presid                                                                   NA
## S.take                                                              9.748e+07
## A.take                                                              9.734e+07
## `PubDate.minute.fctr(14.8,29.5]`                                    2.956e+06
## `PubDate.minute.fctr(29.5,44.2]`                                    2.777e+06
## `PubDate.minute.fctr(44.2,59.1]`                                    3.007e+06
## S.new                                                               5.280e+07
## A.new                                                               5.268e+07
## PubDate.wkday.fctr1                                                 8.681e+06
## PubDate.wkday.fctr2                                                 9.186e+06
## PubDate.wkday.fctr3                                                 9.211e+06
## PubDate.wkday.fctr4                                                 9.067e+06
## PubDate.wkday.fctr5                                                 9.213e+06
## PubDate.wkday.fctr6                                                 7.903e+06
## S.day                                                               7.472e+07
## A.day                                                               7.449e+07
## H.X2014                                                             1.002e+07
## S.show                                                              5.418e+06
## A.show                                                                     NA
## S.report                                                            6.020e+06
## A.report                                                                   NA
## S.share                                                             6.372e+06
## A.share                                                                    NA
## S.year                                                              4.739e+06
## A.year                                                                     NA
## S.compani                                                           4.488e+06
## A.compani                                                                  NA
## H.new                                                               5.590e+06
## S.first                                                             5.700e+06
## A.first                                                                    NA
## S.time                                                              7.022e+07
## A.time                                                              7.006e+07
## H.newyork                                                           7.695e+06
## S.articl                                                            1.008e+07
## A.articl                                                                   NA
## S.will                                                              7.130e+07
## A.will                                                              7.128e+07
## H.day                                                               9.132e+06
## S.newyork                                                           5.027e+06
## A.newyork                                                                  NA
## H.today                                                             2.767e+07
## H.report                                                            1.027e+07
## S.intern                                                            9.825e+06
## A.intern                                                                   NA
## H.week                                                              1.171e+07
## H.fashion                                                           1.432e+07
## S.week                                                              5.020e+06
## A.week                                                                     NA
## S.fashion                                                           7.053e+06
## A.fashion                                                                  NA
## `Headline.pfx.fctrmyMisc::`                                         1.310e+07
## `Headline.pfx.fctr19[0-9][0-9]::`                                   2.205e+07
## `Headline.pfx.fctrDaily Report::`                                   2.070e+07
## `Headline.pfx.fctr.*Fashion Week::`                                 2.362e+07
## `Headline.pfx.fctrWhat We're::`                                     1.979e+07
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                   2.223e+07
## `Headline.pfx.fctrToday in Small Business::`                        3.376e+07
## `Headline.pfx.fctrDaily Clip Report::`                              2.430e+07
## `Headline.pfx.fctrMorning Agenda::`                                 1.746e+07
## `Headline.pfx.fctrNew York Today::`                                 3.381e+07
## `Headline.pfx.fctr6 Q's About the News::`                           1.817e+07
## `Headline.pfx.fctrTest Yourself::`                                  1.925e+07
## `Headline.pfx.fctrWord of the Day::`                                2.292e+07
## `Headline.pfx.fctrmyMultimedia::`                                   1.877e+07
## `Headline.pfx.fctrmyTech::`                                         1.572e+07
## `Headline.pfx.fctrmyPolitics::`                                     1.906e+07
## `Headline.pfx.fctrmyFood::`                                         1.568e+07
## `Headline.pfx.fctrYour Turn::`                                      2.797e+07
## `Headline.pfx.fctrReaders Respond::`                                3.105e+07
## `Headline.pfx.fctrAsk Well::`                                       3.173e+07
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           2.940e+07
## `Headline.pfx.fctrOn This Day::`                                    2.835e+07
## `Headline.pfx.fctrVerbatim::`                                       2.134e+07
## `Headline.pfx.fctrFirst Draft::`                                    1.890e+07
## `Headline.pfx.fctrToday in Politics::`                              3.406e+07
## `Headline.pfx.fctrReporter's Notebook::`                            3.312e+07
## `Headline.pfx.fctrThe Daily Gift::`                                 2.240e+07
## SectionName.nb.fctrArts                                             8.887e+06
## `SectionName.nb.fctrBusiness Day`                                   1.254e+07
## SectionName.nb.fctrHealth                                           1.011e+07
## SectionName.nb.fctrOpinion                                          2.004e+07
## SectionName.nb.fctrWorld                                            1.067e+07
## SectionName.nb.fctrStyles                                           5.057e+07
## SectionName.nb.fctrTStyle                                           6.915e+07
## SectionName.nb.fctrTechnology                                       1.023e+07
## SectionName.nb.fctrMagazine                                         1.720e+07
## SectionName.nb.fctrMultimedia                                       1.205e+07
## `SectionName.nb.fctrmyMisc::`                                       8.672e+06
## SectionName.nb.fctrTravel                                           1.091e+07
## SectionName.nb.fctrU.S.                                             4.915e+07
## `SectionName.nb.fctrN.Y. / Region`                                  1.075e+07
## `SectionName.nb.fctrDaily Clip Report::`                                   NA
## SectionName.nb.fctrOpen                                             4.855e+07
## `SectionName.nb.fctrReaders Respond::`                              3.613e+07
## SectionName.nb.fctrSports                                           6.980e+07
## `SectionName.nb.fctrmyEducation::`                                  5.018e+07
## `SectionName.nb.fctrmyPolitics::`                                   1.814e+07
## SectionName.nb.fctrNational                                         4.850e+07
## `SectionName.nb.fctrVerbatim::`                                            NA
## `SectionName.nb.fctrFirst Draft::`                                         NA
## `SectionName.nb.fctrmyMultimedia::`                                 5.089e+07
## `SectionName.nb.fctrToday in Politics::`                                   NA
## `SectionName.nb.fctrReporter's Notebook::`                                 NA
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                                      NA
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrScience                                                    NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrStyles                                              4.917e+07
## NewsDesk.nb.fctrTStyle                                              6.849e+07
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrTravel                                                     NA
## NewsDesk.nb.fctrmyEducation                                         4.956e+07
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## H.num.chars.log                                                     6.540e+06
## H.num.words.log                                                     3.801e+07
## H.num.words.unq.log                                                 3.752e+07
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      9.845e+06
## `SubsectionName.nb.fctrScience::Health`                                    NA
## `SubsectionName.nb.fctrOpEd::Opinion`                               1.862e+07
## `SubsectionName.nb.fctrRoom For Debate`                             2.109e+07
## `SubsectionName.nb.fctrForeign::World`                              1.904e+07
## `SubsectionName.nb.fctrFashion & Style`                                    NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrAsia Pacific`                                       NA
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
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                             NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                                 NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## A.num.chars.log                                                     1.136e+08
## S.num.chars.log                                                     1.136e+08
## A.num.words.log                                                     5.198e+08
## S.num.words.log                                                     5.198e+08
## A.num.words.unq.log                                                 5.234e+08
## S.num.words.unq.log                                                 5.229e+08
##                                                                       z value
## (Intercept)                                                           3947743
## WordCount.log                                                       264519219
## `PubDate.hour.fctr(7.67,15.3]`                                        7194709
## `PubDate.hour.fctr(15.3,23]`                                         -7264584
## H.is.question                                                        20461200
## PubDate.wkend                                                        -1405835
## A.can                                                                90373556
## S.can                                                               -95036155
## H.has.ebola                                                         -29697089
## S.make                                                                3872169
## A.make                                                                     NA
## S.one                                                               -20393498
## S.state                                                              12958006
## A.state                                                                    NA
## A.one                                                                20842414
## S.said                                                               61512829
## A.said                                                                     NA
## .rnorm                                                                1274900
## `PubDate.date.fctr(7,13]`                                             7991643
## `PubDate.date.fctr(13,19]`                                            6308195
## `PubDate.date.fctr(19,25]`                                            1459470
## `PubDate.date.fctr(25,31]`                                           17390416
## `PubDate.second.fctr(14.8,29.5]`                                    -39737143
## `PubDate.second.fctr(29.5,44.2]`                                    -29503498
## `PubDate.second.fctr(44.2,59.1]`                                    -25744820
## S.presid                                                             -7316072
## A.presid                                                                   NA
## S.take                                                              -21931291
## A.take                                                               23224939
## `PubDate.minute.fctr(14.8,29.5]`                                     -7987132
## `PubDate.minute.fctr(29.5,44.2]`                                    -27702850
## `PubDate.minute.fctr(44.2,59.1]`                                      7436728
## S.new                                                                19644762
## A.new                                                               -20367378
## PubDate.wkday.fctr1                                                   1025467
## PubDate.wkday.fctr2                                                 -10543099
## PubDate.wkday.fctr3                                                  -8839123
## PubDate.wkday.fctr4                                                  -9557947
## PubDate.wkday.fctr5                                                 -10540249
## PubDate.wkday.fctr6                                                 -17477493
## S.day                                                               -14622186
## A.day                                                                15762204
## H.X2014                                                             -24243199
## S.show                                                              -19469395
## A.show                                                                     NA
## S.report                                                             23550578
## A.report                                                                   NA
## S.share                                                             -51185412
## A.share                                                                    NA
## S.year                                                              -36444995
## A.year                                                                     NA
## S.compani                                                            35552880
## A.compani                                                                  NA
## H.new                                                               -27719196
## S.first                                                             -17800703
## A.first                                                                    NA
## S.time                                                               75509509
## A.time                                                              -75491049
## H.newyork                                                             6990821
## S.articl                                                            -41675737
## A.articl                                                                   NA
## S.will                                                                -728279
## A.will                                                               -2098333
## H.day                                                               -13453189
## S.newyork                                                            -4320777
## A.newyork                                                                  NA
## H.today                                                            -116347727
## H.report                                                            -27480234
## S.intern                                                             13209819
## A.intern                                                                   NA
## H.week                                                               -5450904
## H.fashion                                                            -4489520
## S.week                                                              -31121279
## A.week                                                                     NA
## S.fashion                                                            77272818
## A.fashion                                                                  NA
## `Headline.pfx.fctrmyMisc::`                                         -11611520
## `Headline.pfx.fctr19[0-9][0-9]::`                                     5678931
## `Headline.pfx.fctrDaily Report::`                                   -22904049
## `Headline.pfx.fctr.*Fashion Week::`                                  78700838
## `Headline.pfx.fctrWhat We're::`                                    -253350626
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                   -33758318
## `Headline.pfx.fctrToday in Small Business::`                         92861408
## `Headline.pfx.fctrDaily Clip Report::`                             -228250127
## `Headline.pfx.fctrMorning Agenda::`                                 -85827635
## `Headline.pfx.fctrNew York Today::`                                  77309219
## `Headline.pfx.fctr6 Q's About the News::`                           152191715
## `Headline.pfx.fctrTest Yourself::`                                  197659970
## `Headline.pfx.fctrWord of the Day::`                                 45775553
## `Headline.pfx.fctrmyMultimedia::`                                   -44743323
## `Headline.pfx.fctrmyTech::`                                          22745538
## `Headline.pfx.fctrmyPolitics::`                                      -1644898
## `Headline.pfx.fctrmyFood::`                                         -59711688
## `Headline.pfx.fctrYour Turn::`                                      -15083471
## `Headline.pfx.fctrReaders Respond::`                                 -8231907
## `Headline.pfx.fctrAsk Well::`                                       -22250076
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           160733293
## `Headline.pfx.fctrOn This Day::`                                    141339385
## `Headline.pfx.fctrVerbatim::`                                      -163971426
## `Headline.pfx.fctrFirst Draft::`                                   -303347836
## `Headline.pfx.fctrToday in Politics::`                              -21749615
## `Headline.pfx.fctrReporter's Notebook::`                            -47004678
## `Headline.pfx.fctrThe Daily Gift::`                                 -18794758
## SectionName.nb.fctrArts                                            -288575264
## `SectionName.nb.fctrBusiness Day`                                  -328788372
## SectionName.nb.fctrHealth                                           -52673299
## SectionName.nb.fctrOpinion                                          -26127954
## SectionName.nb.fctrWorld                                           -330782272
## SectionName.nb.fctrStyles                                          -189850329
## SectionName.nb.fctrTStyle                                          -148434187
## SectionName.nb.fctrTechnology                                      -312161658
## SectionName.nb.fctrMagazine                                        -220687515
## SectionName.nb.fctrMultimedia                                      -294364209
## `SectionName.nb.fctrmyMisc::`                                      -307138314
## SectionName.nb.fctrTravel                                          -285940989
## SectionName.nb.fctrU.S.                                             -73528838
## `SectionName.nb.fctrN.Y. / Region`                                 -247995938
## `SectionName.nb.fctrDaily Clip Report::`                                   NA
## SectionName.nb.fctrOpen                                             -82933498
## `SectionName.nb.fctrReaders Respond::`                                6017347
## SectionName.nb.fctrSports                                           -22164225
## `SectionName.nb.fctrmyEducation::`                                  -33858273
## `SectionName.nb.fctrmyPolitics::`                                  -157697125
## SectionName.nb.fctrNational                                         -72369852
## `SectionName.nb.fctrVerbatim::`                                            NA
## `SectionName.nb.fctrFirst Draft::`                                         NA
## `SectionName.nb.fctrmyMultimedia::`                                 -34842788
## `SectionName.nb.fctrToday in Politics::`                                   NA
## `SectionName.nb.fctrReporter's Notebook::`                                 NA
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                                      NA
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrScience                                                    NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrStyles                                               68359483
## NewsDesk.nb.fctrTStyle                                               74507190
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrTravel                                                     NA
## NewsDesk.nb.fctrmyEducation                                         -56780192
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## H.num.chars.log                                                      -5435805
## H.num.words.log                                                      24185098
## H.num.words.unq.log                                                 -20627570
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      144300917
## `SubsectionName.nb.fctrScience::Health`                                    NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                 -250546
## `SubsectionName.nb.fctrRoom For Debate`                            -211948331
## `SubsectionName.nb.fctrForeign::World`                             -135744688
## `SubsectionName.nb.fctrFashion & Style`                                    NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrAsia Pacific`                                       NA
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
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                             NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                                 NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## A.num.chars.log                                                     -52418342
## S.num.chars.log                                                      51975168
## A.num.words.log                                                     -74350920
## S.num.words.log                                                      74236876
## A.num.words.unq.log                                                  73631557
## S.num.words.unq.log                                                 -73666001
##                                                                    Pr(>|z|)
## (Intercept)                                                          <2e-16
## WordCount.log                                                        <2e-16
## `PubDate.hour.fctr(7.67,15.3]`                                       <2e-16
## `PubDate.hour.fctr(15.3,23]`                                         <2e-16
## H.is.question                                                        <2e-16
## PubDate.wkend                                                        <2e-16
## A.can                                                                <2e-16
## S.can                                                                <2e-16
## H.has.ebola                                                          <2e-16
## S.make                                                               <2e-16
## A.make                                                                   NA
## S.one                                                                <2e-16
## S.state                                                              <2e-16
## A.state                                                                  NA
## A.one                                                                <2e-16
## S.said                                                               <2e-16
## A.said                                                                   NA
## .rnorm                                                               <2e-16
## `PubDate.date.fctr(7,13]`                                            <2e-16
## `PubDate.date.fctr(13,19]`                                           <2e-16
## `PubDate.date.fctr(19,25]`                                           <2e-16
## `PubDate.date.fctr(25,31]`                                           <2e-16
## `PubDate.second.fctr(14.8,29.5]`                                     <2e-16
## `PubDate.second.fctr(29.5,44.2]`                                     <2e-16
## `PubDate.second.fctr(44.2,59.1]`                                     <2e-16
## S.presid                                                             <2e-16
## A.presid                                                                 NA
## S.take                                                               <2e-16
## A.take                                                               <2e-16
## `PubDate.minute.fctr(14.8,29.5]`                                     <2e-16
## `PubDate.minute.fctr(29.5,44.2]`                                     <2e-16
## `PubDate.minute.fctr(44.2,59.1]`                                     <2e-16
## S.new                                                                <2e-16
## A.new                                                                <2e-16
## PubDate.wkday.fctr1                                                  <2e-16
## PubDate.wkday.fctr2                                                  <2e-16
## PubDate.wkday.fctr3                                                  <2e-16
## PubDate.wkday.fctr4                                                  <2e-16
## PubDate.wkday.fctr5                                                  <2e-16
## PubDate.wkday.fctr6                                                  <2e-16
## S.day                                                                <2e-16
## A.day                                                                <2e-16
## H.X2014                                                              <2e-16
## S.show                                                               <2e-16
## A.show                                                                   NA
## S.report                                                             <2e-16
## A.report                                                                 NA
## S.share                                                              <2e-16
## A.share                                                                  NA
## S.year                                                               <2e-16
## A.year                                                                   NA
## S.compani                                                            <2e-16
## A.compani                                                                NA
## H.new                                                                <2e-16
## S.first                                                              <2e-16
## A.first                                                                  NA
## S.time                                                               <2e-16
## A.time                                                               <2e-16
## H.newyork                                                            <2e-16
## S.articl                                                             <2e-16
## A.articl                                                                 NA
## S.will                                                               <2e-16
## A.will                                                               <2e-16
## H.day                                                                <2e-16
## S.newyork                                                            <2e-16
## A.newyork                                                                NA
## H.today                                                              <2e-16
## H.report                                                             <2e-16
## S.intern                                                             <2e-16
## A.intern                                                                 NA
## H.week                                                               <2e-16
## H.fashion                                                            <2e-16
## S.week                                                               <2e-16
## A.week                                                                   NA
## S.fashion                                                            <2e-16
## A.fashion                                                                NA
## `Headline.pfx.fctrmyMisc::`                                          <2e-16
## `Headline.pfx.fctr19[0-9][0-9]::`                                    <2e-16
## `Headline.pfx.fctrDaily Report::`                                    <2e-16
## `Headline.pfx.fctr.*Fashion Week::`                                  <2e-16
## `Headline.pfx.fctrWhat We're::`                                      <2e-16
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                    <2e-16
## `Headline.pfx.fctrToday in Small Business::`                         <2e-16
## `Headline.pfx.fctrDaily Clip Report::`                               <2e-16
## `Headline.pfx.fctrMorning Agenda::`                                  <2e-16
## `Headline.pfx.fctrNew York Today::`                                  <2e-16
## `Headline.pfx.fctr6 Q's About the News::`                            <2e-16
## `Headline.pfx.fctrTest Yourself::`                                   <2e-16
## `Headline.pfx.fctrWord of the Day::`                                 <2e-16
## `Headline.pfx.fctrmyMultimedia::`                                    <2e-16
## `Headline.pfx.fctrmyTech::`                                          <2e-16
## `Headline.pfx.fctrmyPolitics::`                                      <2e-16
## `Headline.pfx.fctrmyFood::`                                          <2e-16
## `Headline.pfx.fctrYour Turn::`                                       <2e-16
## `Headline.pfx.fctrReaders Respond::`                                 <2e-16
## `Headline.pfx.fctrAsk Well::`                                        <2e-16
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                            <2e-16
## `Headline.pfx.fctrOn This Day::`                                     <2e-16
## `Headline.pfx.fctrVerbatim::`                                        <2e-16
## `Headline.pfx.fctrFirst Draft::`                                     <2e-16
## `Headline.pfx.fctrToday in Politics::`                               <2e-16
## `Headline.pfx.fctrReporter's Notebook::`                             <2e-16
## `Headline.pfx.fctrThe Daily Gift::`                                  <2e-16
## SectionName.nb.fctrArts                                              <2e-16
## `SectionName.nb.fctrBusiness Day`                                    <2e-16
## SectionName.nb.fctrHealth                                            <2e-16
## SectionName.nb.fctrOpinion                                           <2e-16
## SectionName.nb.fctrWorld                                             <2e-16
## SectionName.nb.fctrStyles                                            <2e-16
## SectionName.nb.fctrTStyle                                            <2e-16
## SectionName.nb.fctrTechnology                                        <2e-16
## SectionName.nb.fctrMagazine                                          <2e-16
## SectionName.nb.fctrMultimedia                                        <2e-16
## `SectionName.nb.fctrmyMisc::`                                        <2e-16
## SectionName.nb.fctrTravel                                            <2e-16
## SectionName.nb.fctrU.S.                                              <2e-16
## `SectionName.nb.fctrN.Y. / Region`                                   <2e-16
## `SectionName.nb.fctrDaily Clip Report::`                                 NA
## SectionName.nb.fctrOpen                                              <2e-16
## `SectionName.nb.fctrReaders Respond::`                               <2e-16
## SectionName.nb.fctrSports                                            <2e-16
## `SectionName.nb.fctrmyEducation::`                                   <2e-16
## `SectionName.nb.fctrmyPolitics::`                                    <2e-16
## SectionName.nb.fctrNational                                          <2e-16
## `SectionName.nb.fctrVerbatim::`                                          NA
## `SectionName.nb.fctrFirst Draft::`                                       NA
## `SectionName.nb.fctrmyMultimedia::`                                  <2e-16
## `SectionName.nb.fctrToday in Politics::`                                 NA
## `SectionName.nb.fctrReporter's Notebook::`                               NA
## SectionName.nb.fctrCulture                                               NA
## `SectionName.nb.fctrThe Daily Gift::`                                    NA
## NewsDesk.nb.fctrCulture                                                  NA
## NewsDesk.nb.fctrScience                                                  NA
## NewsDesk.nb.fctrOpEd                                                     NA
## NewsDesk.nb.fctrForeign                                                  NA
## NewsDesk.nb.fctrStyles                                               <2e-16
## NewsDesk.nb.fctrTStyle                                               <2e-16
## NewsDesk.nb.fctrMagazine                                                 NA
## NewsDesk.nb.fctrmyMultimedia                                             NA
## `NewsDesk.nb.fctrmyMisc::`                                               NA
## NewsDesk.nb.fctrTravel                                                   NA
## NewsDesk.nb.fctrmyEducation                                          <2e-16
## NewsDesk.nb.fctrMetro                                                    NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                    NA
## `NewsDesk.nb.fctrReaders Respond::`                                      NA
## NewsDesk.nb.fctrNational                                                 NA
## NewsDesk.nb.fctrSports                                                   NA
## `NewsDesk.nb.fctrmyEducation::`                                          NA
## `NewsDesk.nb.fctrmyPolitics::`                                           NA
## `NewsDesk.nb.fctrVerbatim::`                                             NA
## `NewsDesk.nb.fctrFirst Draft::`                                          NA
## `NewsDesk.nb.fctrmyMultimedia::`                                         NA
## `NewsDesk.nb.fctrToday in Politics::`                                    NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                  NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                       NA
## H.num.chars.log                                                      <2e-16
## H.num.words.log                                                      <2e-16
## H.num.words.unq.log                                                  <2e-16
## `SubsectionName.nb.fctrCulture::Arts`                                    NA
## SubsectionName.nb.fctrDealbook                                       <2e-16
## `SubsectionName.nb.fctrScience::Health`                                  NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                <2e-16
## `SubsectionName.nb.fctrRoom For Debate`                              <2e-16
## `SubsectionName.nb.fctrForeign::World`                               <2e-16
## `SubsectionName.nb.fctrFashion & Style`                                  NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                   NA
## `SubsectionName.nb.fctrAsia Pacific`                                     NA
## `SubsectionName.nb.fctrBusiness::Technology`                             NA
## `SubsectionName.nb.fctrMagazine::Magazine`                               NA
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                         NA
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                 NA
## `SubsectionName.nb.fctrTravel::Travel`                                   NA
## SubsectionName.nb.fctrEducation                                          NA
## `SubsectionName.nb.fctrThe Public Editor`                                NA
## `SubsectionName.nb.fctrSmall Business`                                   NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                             NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`           NA
## `SubsectionName.nb.fctrmyMisc::Open`                                     NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`               NA
## SubsectionName.nb.fctrPolitics                                           NA
## `SubsectionName.nb.fctrSports::Sports`                                   NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                       NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                         NA
## `SubsectionName.nb.fctrNational::National`                               NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                             NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                       NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                     NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`           NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`       NA
## `SubsectionName.nb.fctrCulture::Culture`                                 NA
## `SubsectionName.nb.fctrTStyle::Technology`                               NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                     NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                 NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                   NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                      NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                NA
## `SubsectionName.nb.fctrmyEducation::Travel`                              NA
## A.num.chars.log                                                      <2e-16
## S.num.chars.log                                                      <2e-16
## A.num.words.log                                                      <2e-16
## S.num.words.log                                                      <2e-16
## A.num.words.unq.log                                                  <2e-16
## S.num.words.unq.log                                                  <2e-16
##                                                                       
## (Intercept)                                                        ***
## WordCount.log                                                      ***
## `PubDate.hour.fctr(7.67,15.3]`                                     ***
## `PubDate.hour.fctr(15.3,23]`                                       ***
## H.is.question                                                      ***
## PubDate.wkend                                                      ***
## A.can                                                              ***
## S.can                                                              ***
## H.has.ebola                                                        ***
## S.make                                                             ***
## A.make                                                                
## S.one                                                              ***
## S.state                                                            ***
## A.state                                                               
## A.one                                                              ***
## S.said                                                             ***
## A.said                                                                
## .rnorm                                                             ***
## `PubDate.date.fctr(7,13]`                                          ***
## `PubDate.date.fctr(13,19]`                                         ***
## `PubDate.date.fctr(19,25]`                                         ***
## `PubDate.date.fctr(25,31]`                                         ***
## `PubDate.second.fctr(14.8,29.5]`                                   ***
## `PubDate.second.fctr(29.5,44.2]`                                   ***
## `PubDate.second.fctr(44.2,59.1]`                                   ***
## S.presid                                                           ***
## A.presid                                                              
## S.take                                                             ***
## A.take                                                             ***
## `PubDate.minute.fctr(14.8,29.5]`                                   ***
## `PubDate.minute.fctr(29.5,44.2]`                                   ***
## `PubDate.minute.fctr(44.2,59.1]`                                   ***
## S.new                                                              ***
## A.new                                                              ***
## PubDate.wkday.fctr1                                                ***
## PubDate.wkday.fctr2                                                ***
## PubDate.wkday.fctr3                                                ***
## PubDate.wkday.fctr4                                                ***
## PubDate.wkday.fctr5                                                ***
## PubDate.wkday.fctr6                                                ***
## S.day                                                              ***
## A.day                                                              ***
## H.X2014                                                            ***
## S.show                                                             ***
## A.show                                                                
## S.report                                                           ***
## A.report                                                              
## S.share                                                            ***
## A.share                                                               
## S.year                                                             ***
## A.year                                                                
## S.compani                                                          ***
## A.compani                                                             
## H.new                                                              ***
## S.first                                                            ***
## A.first                                                               
## S.time                                                             ***
## A.time                                                             ***
## H.newyork                                                          ***
## S.articl                                                           ***
## A.articl                                                              
## S.will                                                             ***
## A.will                                                             ***
## H.day                                                              ***
## S.newyork                                                          ***
## A.newyork                                                             
## H.today                                                            ***
## H.report                                                           ***
## S.intern                                                           ***
## A.intern                                                              
## H.week                                                             ***
## H.fashion                                                          ***
## S.week                                                             ***
## A.week                                                                
## S.fashion                                                          ***
## A.fashion                                                             
## `Headline.pfx.fctrmyMisc::`                                        ***
## `Headline.pfx.fctr19[0-9][0-9]::`                                  ***
## `Headline.pfx.fctrDaily Report::`                                  ***
## `Headline.pfx.fctr.*Fashion Week::`                                ***
## `Headline.pfx.fctrWhat We're::`                                    ***
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                  ***
## `Headline.pfx.fctrToday in Small Business::`                       ***
## `Headline.pfx.fctrDaily Clip Report::`                             ***
## `Headline.pfx.fctrMorning Agenda::`                                ***
## `Headline.pfx.fctrNew York Today::`                                ***
## `Headline.pfx.fctr6 Q's About the News::`                          ***
## `Headline.pfx.fctrTest Yourself::`                                 ***
## `Headline.pfx.fctrWord of the Day::`                               ***
## `Headline.pfx.fctrmyMultimedia::`                                  ***
## `Headline.pfx.fctrmyTech::`                                        ***
## `Headline.pfx.fctrmyPolitics::`                                    ***
## `Headline.pfx.fctrmyFood::`                                        ***
## `Headline.pfx.fctrYour Turn::`                                     ***
## `Headline.pfx.fctrReaders Respond::`                               ***
## `Headline.pfx.fctrAsk Well::`                                      ***
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                          ***
## `Headline.pfx.fctrOn This Day::`                                   ***
## `Headline.pfx.fctrVerbatim::`                                      ***
## `Headline.pfx.fctrFirst Draft::`                                   ***
## `Headline.pfx.fctrToday in Politics::`                             ***
## `Headline.pfx.fctrReporter's Notebook::`                           ***
## `Headline.pfx.fctrThe Daily Gift::`                                ***
## SectionName.nb.fctrArts                                            ***
## `SectionName.nb.fctrBusiness Day`                                  ***
## SectionName.nb.fctrHealth                                          ***
## SectionName.nb.fctrOpinion                                         ***
## SectionName.nb.fctrWorld                                           ***
## SectionName.nb.fctrStyles                                          ***
## SectionName.nb.fctrTStyle                                          ***
## SectionName.nb.fctrTechnology                                      ***
## SectionName.nb.fctrMagazine                                        ***
## SectionName.nb.fctrMultimedia                                      ***
## `SectionName.nb.fctrmyMisc::`                                      ***
## SectionName.nb.fctrTravel                                          ***
## SectionName.nb.fctrU.S.                                            ***
## `SectionName.nb.fctrN.Y. / Region`                                 ***
## `SectionName.nb.fctrDaily Clip Report::`                              
## SectionName.nb.fctrOpen                                            ***
## `SectionName.nb.fctrReaders Respond::`                             ***
## SectionName.nb.fctrSports                                          ***
## `SectionName.nb.fctrmyEducation::`                                 ***
## `SectionName.nb.fctrmyPolitics::`                                  ***
## SectionName.nb.fctrNational                                        ***
## `SectionName.nb.fctrVerbatim::`                                       
## `SectionName.nb.fctrFirst Draft::`                                    
## `SectionName.nb.fctrmyMultimedia::`                                ***
## `SectionName.nb.fctrToday in Politics::`                              
## `SectionName.nb.fctrReporter's Notebook::`                            
## SectionName.nb.fctrCulture                                            
## `SectionName.nb.fctrThe Daily Gift::`                                 
## NewsDesk.nb.fctrCulture                                               
## NewsDesk.nb.fctrScience                                               
## NewsDesk.nb.fctrOpEd                                                  
## NewsDesk.nb.fctrForeign                                               
## NewsDesk.nb.fctrStyles                                             ***
## NewsDesk.nb.fctrTStyle                                             ***
## NewsDesk.nb.fctrMagazine                                              
## NewsDesk.nb.fctrmyMultimedia                                          
## `NewsDesk.nb.fctrmyMisc::`                                            
## NewsDesk.nb.fctrTravel                                                
## NewsDesk.nb.fctrmyEducation                                        ***
## NewsDesk.nb.fctrMetro                                                 
## `NewsDesk.nb.fctrDaily Clip Report::`                                 
## `NewsDesk.nb.fctrReaders Respond::`                                   
## NewsDesk.nb.fctrNational                                              
## NewsDesk.nb.fctrSports                                                
## `NewsDesk.nb.fctrmyEducation::`                                       
## `NewsDesk.nb.fctrmyPolitics::`                                        
## `NewsDesk.nb.fctrVerbatim::`                                          
## `NewsDesk.nb.fctrFirst Draft::`                                       
## `NewsDesk.nb.fctrmyMultimedia::`                                      
## `NewsDesk.nb.fctrToday in Politics::`                                 
## `NewsDesk.nb.fctrReporter's Notebook::`                               
## `NewsDesk.nb.fctrThe Daily Gift::`                                    
## H.num.chars.log                                                    ***
## H.num.words.log                                                    ***
## H.num.words.unq.log                                                ***
## `SubsectionName.nb.fctrCulture::Arts`                                 
## SubsectionName.nb.fctrDealbook                                     ***
## `SubsectionName.nb.fctrScience::Health`                               
## `SubsectionName.nb.fctrOpEd::Opinion`                              ***
## `SubsectionName.nb.fctrRoom For Debate`                            ***
## `SubsectionName.nb.fctrForeign::World`                             ***
## `SubsectionName.nb.fctrFashion & Style`                               
## `SubsectionName.nb.fctrTStyle::TStyle`                                
## `SubsectionName.nb.fctrAsia Pacific`                                  
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
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`            
## SubsectionName.nb.fctrPolitics                                        
## `SubsectionName.nb.fctrSports::Sports`                                
## `SubsectionName.nb.fctrmyEducation::myEducation::`                    
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                      
## `SubsectionName.nb.fctrNational::National`                            
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                          
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                    
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                  
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`        
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`    
## `SubsectionName.nb.fctrCulture::Culture`                              
## `SubsectionName.nb.fctrTStyle::Technology`                            
## `SubsectionName.nb.fctrmyMisc::U.S.`                                  
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`              
## `SubsectionName.nb.fctrmyMisc::Travel`                                
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                   
## `SubsectionName.nb.fctrmyEducation::U.S.`                             
## `SubsectionName.nb.fctrmyEducation::Travel`                           
## A.num.chars.log                                                    ***
## S.num.chars.log                                                    ***
## A.num.words.log                                                    ***
## S.num.words.log                                                    ***
## A.num.words.unq.log                                                ***
## S.num.words.unq.log                                                ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance:  4042.7  on 4474  degrees of freedom
## Residual deviance: 30204.6  on 4350  degrees of freedom
## AIC: 30455
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

![](NYTBlogs_zoo_files/figure-html/fit.models_1-4.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.7108351
## 3        0.2 0.7108351
## 4        0.3 0.7108351
## 5        0.4 0.7108351
## 6        0.5 0.7108351
## 7        0.6 0.7108351
## 8        0.7 0.7108351
## 9        0.8 0.7108351
## 10       0.9 0.7108351
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3541
## 2            Y                                      234
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      185
## 2                                      515
##          Prediction
## Reference    N    Y
##         N 3541  185
##         Y  234  515
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.063687e-01   6.550520e-01   8.974545e-01   9.147484e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   6.900823e-46   1.902946e-02 
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

![](NYTBlogs_zoo_files/figure-html/fit.models_1-6.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.7107195
## 3        0.2 0.7107195
## 4        0.3 0.7107195
## 5        0.4 0.7107195
## 6        0.5 0.7107195
## 7        0.6 0.7107195
## 8        0.7 0.7107195
## 9        0.8 0.7107195
## 10       0.9 0.7107195
## 11       1.0 0.0000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-8.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1618
## 2            Y                                      102
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       95
## 2                                      242
##          Prediction
## Reference    N    Y
##         N 1618   95
##         Y  102  242
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.042295e-01   6.533426e-01   8.906870e-01   9.166067e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   9.648468e-21   6.690281e-01 
##            model_id model_method
## 1 Conditional.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     31.038                 9.254
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8189662                    0.9       0.7108351        0.8891662
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8974545             0.9147484     0.5731653   0.8240151
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.7107195        0.9042295
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1              0.890687             0.9166067     0.6533426    30454.58
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01760239      0.08337694
## [1] "fitting model: Conditional.X.no.rnorm.rpart"
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
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

![](NYTBlogs_zoo_files/figure-html/fit.models_1-9.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_1-10.png) 

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
##       WordCount.log                       < 6.528688 to the left,  improve=110.27730, (0 missing)
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

![](NYTBlogs_zoo_files/figure-html/fit.models_1-11.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_1-12.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_1-13.png) 

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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     11.762                  2.14
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
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-14.png) 

```
## + : mtry=  2 
## - : mtry=  2 
## + : mtry=101 
## - : mtry=101 
## + : mtry=201 
## - : mtry=201 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 101 on full training set
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-15.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_1-16.png) 

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
## importance       201   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           201   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.7959617
## 3        0.2 0.9195826
## 4        0.3 0.9752604
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 0.9973226
## 8        0.7 0.9207493
## 9        0.8 0.8073248
## 10       0.9 0.6142461
## 11       1.0 0.0000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-18.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_1-19.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.5872576
## 3        0.2 0.6749156
## 4        0.3 0.7256177
## 5        0.4 0.7186147
## 6        0.5 0.6835443
## 7        0.6 0.6666667
## 8        0.7 0.6162362
## 9        0.8 0.4937238
## 10       0.9 0.3043478
## 11       1.0 0.0000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-20.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1567
## 2            Y                                               65
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                              146
## 2                                              279
##          Prediction
## Reference    N    Y
##         N 1567  146
##         Y   65  279
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.974234e-01   6.633970e-01   8.834974e-01   9.102047e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   4.637200e-17   3.641161e-08 
##                    model_id model_method
## 1 Conditional.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                    351.532                86.047
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.9086034
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.999176                     1     0.6514493   0.9284694
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.7256177        0.8974234
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8834974             0.9102047      0.663397
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        WordCount.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        WordCount.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        WordCount.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        WordCount.log
## 7                                                                                                                                                                                                                 WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:SectionName.nb.fctr, WordCount.log:H.num.chars.log, WordCount.log:NewsDesk.nb.fctr, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
## 8                                                                                                                                                                                                                                                                                                           WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, S.can, H.has.ebola, S.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, SectionName.nb.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
## 9  WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 10         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 11         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##    max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1                0                      0.630                 0.003
## 2                0                      0.351                 0.001
## 3                0                      0.750                 0.072
## 4                0                      0.628                 0.061
## 5                3                      1.866                 0.068
## 6                1                      1.233                 0.076
## 7                1                      7.167                 2.122
## 8                1                      9.685                 2.821
## 9                1                     31.038                 9.254
## 10               3                     11.762                 2.140
## 11               3                    351.532                86.047
##    max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.5000000                    0.5       0.0000000        0.8326257
## 2    0.4975446                    0.1       0.2867534        0.1673743
## 3    0.5000000                    0.5       0.0000000        0.8326257
## 4    0.7075116                    0.2       0.4564165        0.7993296
## 5    0.5000000                    0.5       0.0000000        0.8149732
## 6    0.7311226                    0.2       0.4240535        0.8308383
## 7    0.6927673                    0.9       0.5098935        0.8659114
## 8    0.9296428                    0.3       0.6849657        0.8876000
## 9    0.8189662                    0.9       0.7108351        0.8891662
## 10   0.6662843                    0.7       0.4808549        0.8744112
## 11   1.0000000                    0.5       1.0000000        0.9086034
##    max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.8213602             0.8434553    0.00000000   0.5000000
## 2              0.1565447             0.1786398    0.00000000   0.4821958
## 3              0.8213602             0.8434553    0.00000000   0.5000000
## 4              0.7872899             0.8109773    0.33467781   0.6459615
## 5              0.8213602             0.8434553    0.08110908   0.5000000
## 6              0.6975347             0.7243123    0.01384725   0.7322841
## 7              0.8454615             0.8662502    0.47908197   0.6748598
## 8              0.8775150             0.8962781    0.56449974   0.9130877
## 9              0.8974545             0.9147484    0.57316530   0.8240151
## 10             0.8595050             0.8794511    0.44056055   0.6862731
## 11             0.9991760             1.0000000    0.65144929   0.9284694
##    opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                     0.5       0.0000000        0.8327662
## 2                     0.1       0.2865473        0.1672338
## 3                     0.5       0.0000000        0.8327662
## 4                     0.2       0.3785901        0.7685950
## 5                     0.5       0.0000000        0.8327662
## 6                     0.2       0.3961905        0.6917842
## 7                     0.9       0.4752475        0.8454059
## 8                     0.4       0.6800574        0.8915897
## 9                     0.9       0.7107195        0.9042295
## 10                    0.7       0.5176909        0.8740885
## 11                    0.3       0.7256177        0.8974234
##    max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.8159247             0.8486533     0.0000000
## 2              0.1513467             0.1840753     0.0000000
## 3              0.8159247             0.8486533     0.0000000
## 4              0.7497515             0.7866700     0.2382228
## 5              0.8159247             0.8486533     0.0000000
## 6              0.6713234             0.7116969     0.2210018
## 7              0.8290495             0.8607712     0.3865379
## 8              0.8773491             0.9047029     0.6148086
## 9              0.8906870             0.9166067     0.6533426
## 10             0.8589743             0.8881272     0.4517912
## 11             0.8834974             0.9102047     0.6633970
##    max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## 1                  NA              NA          NA
## 2                  NA              NA          NA
## 3                  NA              NA          NA
## 4                  NA              NA          NA
## 5         0.004766392     0.009878063          NA
## 6         0.001635096     0.008694535    3671.246
## 7         0.040233795     0.167035965   46534.226
## 8         0.008887506     0.032541531    2446.224
## 9         0.017602388     0.083376941   30454.582
## 10        0.014254671     0.073840685          NA
## 11                 NA              NA          NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          6          1 230.353 645.448 415.096
## 11 fit.models          6          2 645.449      NA      NA
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        WordCount.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        WordCount.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        WordCount.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        WordCount.log
## 7                                                                                                                                                                                                                 WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:SectionName.nb.fctr, WordCount.log:H.num.chars.log, WordCount.log:NewsDesk.nb.fctr, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
## 8                                                                                                                                                                                                                                                                                                           WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, S.can, H.has.ebola, S.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, SectionName.nb.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
## 9  WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, .rnorm, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 10         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 11         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, SubsectionName.nb.fctr, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##    max.nTuningRuns max.auc.fit opt.prob.threshold.fit max.f.score.fit
## 1                0   0.5000000                    0.5       0.0000000
## 2                0   0.4975446                    0.1       0.2867534
## 3                0   0.5000000                    0.5       0.0000000
## 4                0   0.7075116                    0.2       0.4564165
## 5                3   0.5000000                    0.5       0.0000000
## 6                1   0.7311226                    0.2       0.4240535
## 7                1   0.6927673                    0.9       0.5098935
## 8                1   0.9296428                    0.3       0.6849657
## 9                1   0.8189662                    0.9       0.7108351
## 10               3   0.6662843                    0.7       0.4808549
## 11               3   1.0000000                    0.5       1.0000000
##    max.Accuracy.fit max.Kappa.fit max.auc.OOB opt.prob.threshold.OOB
## 1         0.8326257    0.00000000   0.5000000                    0.5
## 2         0.1673743    0.00000000   0.4821958                    0.1
## 3         0.8326257    0.00000000   0.5000000                    0.5
## 4         0.7993296    0.33467781   0.6459615                    0.2
## 5         0.8149732    0.08110908   0.5000000                    0.5
## 6         0.8308383    0.01384725   0.7322841                    0.2
## 7         0.8659114    0.47908197   0.6748598                    0.9
## 8         0.8876000    0.56449974   0.9130877                    0.4
## 9         0.8891662    0.57316530   0.8240151                    0.9
## 10        0.8744112    0.44056055   0.6862731                    0.7
## 11        0.9086034    0.65144929   0.9284694                    0.3
##    max.f.score.OOB max.Accuracy.OOB max.Kappa.OOB
## 1        0.0000000        0.8327662     0.0000000
## 2        0.2865473        0.1672338     0.0000000
## 3        0.0000000        0.8327662     0.0000000
## 4        0.3785901        0.7685950     0.2382228
## 5        0.0000000        0.8327662     0.0000000
## 6        0.3961905        0.6917842     0.2210018
## 7        0.4752475        0.8454059     0.3865379
## 8        0.6800574        0.8915897     0.6148086
## 9        0.7107195        0.9042295     0.6533426
## 10       0.5176909        0.8740885     0.4517912
## 11       0.7256177        0.8974234     0.6633970
##    inv.elapsedtime.everything inv.elapsedtime.final  inv.aic.fit
## 1                 1.587301587          3.333333e+02           NA
## 2                 2.849002849          1.000000e+03           NA
## 3                 1.333333333          1.388889e+01           NA
## 4                 1.592356688          1.639344e+01           NA
## 5                 0.535905681          1.470588e+01           NA
## 6                 0.811030008          1.315789e+01 2.723871e-04
## 7                 0.139528394          4.712535e-01 2.148956e-05
## 8                 0.103252452          3.544842e-01 4.087933e-04
## 9                 0.032218571          1.080614e-01 3.283578e-05
## 10                0.085019554          4.672897e-01           NA
## 11                0.002844691          1.162156e-02           NA
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

![](NYTBlogs_zoo_files/figure-html/fit.models_2-1.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_2-2.png) 

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
## 9             Conditional.X.glm        0.9042295   0.8240151     0.6533426
## 11    Conditional.X.no.rnorm.rf        0.8974234   0.9284694     0.6633970
## 8                 Low.cor.X.glm        0.8915897   0.9130877     0.6148086
## 10 Conditional.X.no.rnorm.rpart        0.8740885   0.6862731     0.4517912
## 7       Interact.High.cor.Y.glm        0.8454059   0.6748598     0.3865379
## 1             MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3          Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 5               Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 4     Max.cor.Y.cv.0.cp.0.rpart        0.7685950   0.6459615     0.2382228
## 6                 Max.cor.Y.glm        0.6917842   0.7322841     0.2210018
## 2       Random.myrandom_classfr        0.1672338   0.4821958     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 9    30454.582                    0.9
## 11          NA                    0.3
## 8     2446.224                    0.4
## 10          NA                    0.7
## 7    46534.226                    0.9
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 4           NA                    0.2
## 6     3671.246                    0.2
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

![](NYTBlogs_zoo_files/figure-html/fit.models_2-3.png) 

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
## [1] "Best model id: Conditional.X.glm"
```

```r
if (is.null(glb_sel_mdl_id)) 
    { glb_sel_mdl_id <- dsp_models_df[1, "model_id"] } else 
        print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

```
## Warning: not plotting observations with leverage one:
##   1693, 3596, 3794, 4229, 4338, 4394
```

![](NYTBlogs_zoo_files/figure-html/fit.models_2-4.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_2-5.png) 

```
## Warning: not plotting observations with leverage one:
##   1693, 3596, 3794, 4229, 4338, 4394
```

![](NYTBlogs_zoo_files/figure-html/fit.models_2-6.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](NYTBlogs_zoo_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -8.49    0.00    0.00    0.00    8.49  
## 
## Coefficients: (78 not defined because of singularities)
##                                                                      Estimate
## (Intercept)                                                         1.114e+14
## WordCount.log                                                       3.354e+14
## `PubDate.hour.fctr(7.67,15.3]`                                      2.625e+13
## `PubDate.hour.fctr(15.3,23]`                                       -2.864e+13
## H.is.question                                                       9.650e+13
## PubDate.wkend                                                      -1.058e+13
## A.can                                                               7.066e+15
## S.can                                                              -7.485e+15
## H.has.ebola                                                        -2.713e+14
## S.make                                                              2.047e+13
## A.make                                                                     NA
## S.one                                                              -2.194e+15
## S.state                                                             7.238e+13
## A.state                                                                    NA
## A.one                                                               2.245e+15
## S.said                                                              3.240e+14
## A.said                                                                     NA
## .rnorm                                                              1.287e+12
## `PubDate.date.fctr(7,13]`                                           2.527e+13
## `PubDate.date.fctr(13,19]`                                          1.969e+13
## `PubDate.date.fctr(19,25]`                                          4.435e+12
## `PubDate.date.fctr(25,31]`                                          5.884e+13
## `PubDate.second.fctr(14.8,29.5]`                                   -1.141e+14
## `PubDate.second.fctr(29.5,44.2]`                                   -8.437e+13
## `PubDate.second.fctr(44.2,59.1]`                                   -7.397e+13
## S.presid                                                           -4.214e+13
## A.presid                                                                   NA
## S.take                                                             -2.138e+15
## A.take                                                              2.261e+15
## `PubDate.minute.fctr(14.8,29.5]`                                   -2.361e+13
## `PubDate.minute.fctr(29.5,44.2]`                                   -7.693e+13
## `PubDate.minute.fctr(44.2,59.1]`                                    2.236e+13
## S.new                                                               1.037e+15
## A.new                                                              -1.073e+15
## PubDate.wkday.fctr1                                                 8.902e+12
## PubDate.wkday.fctr2                                                -9.685e+13
## PubDate.wkday.fctr3                                                -8.142e+13
## PubDate.wkday.fctr4                                                -8.666e+13
## PubDate.wkday.fctr5                                                -9.711e+13
## PubDate.wkday.fctr6                                                -1.381e+14
## S.day                                                              -1.093e+15
## A.day                                                               1.174e+15
## H.X2014                                                            -2.429e+14
## S.show                                                             -1.055e+14
## A.show                                                                     NA
## S.report                                                            1.418e+14
## A.report                                                                   NA
## S.share                                                            -3.262e+14
## A.share                                                                    NA
## S.year                                                             -1.727e+14
## A.year                                                                     NA
## S.compani                                                           1.596e+14
## A.compani                                                                  NA
## H.new                                                              -1.549e+14
## S.first                                                            -1.015e+14
## A.first                                                                    NA
## S.time                                                              5.302e+15
## A.time                                                             -5.289e+15
## H.newyork                                                           5.379e+13
## S.articl                                                           -4.203e+14
## A.articl                                                                   NA
## S.will                                                             -5.193e+13
## A.will                                                             -1.496e+14
## H.day                                                              -1.229e+14
## S.newyork                                                          -2.172e+13
## A.newyork                                                                  NA
## H.today                                                            -3.220e+15
## H.report                                                           -2.823e+14
## S.intern                                                            1.298e+14
## A.intern                                                                   NA
## H.week                                                             -6.385e+13
## H.fashion                                                          -6.428e+13
## S.week                                                             -1.562e+14
## A.week                                                                     NA
## S.fashion                                                           5.450e+14
## A.fashion                                                                  NA
## `Headline.pfx.fctrmyMisc::`                                        -1.521e+14
## `Headline.pfx.fctr19[0-9][0-9]::`                                   1.252e+14
## `Headline.pfx.fctrDaily Report::`                                  -4.742e+14
## `Headline.pfx.fctr.*Fashion Week::`                                 1.859e+15
## `Headline.pfx.fctrWhat We're::`                                    -5.013e+15
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                  -7.506e+14
## `Headline.pfx.fctrToday in Small Business::`                        3.135e+15
## `Headline.pfx.fctrDaily Clip Report::`                             -5.547e+15
## `Headline.pfx.fctrMorning Agenda::`                                -1.499e+15
## `Headline.pfx.fctrNew York Today::`                                 2.614e+15
## `Headline.pfx.fctr6 Q's About the News::`                           2.765e+15
## `Headline.pfx.fctrTest Yourself::`                                  3.806e+15
## `Headline.pfx.fctrWord of the Day::`                                1.049e+15
## `Headline.pfx.fctrmyMultimedia::`                                  -8.396e+14
## `Headline.pfx.fctrmyTech::`                                         3.577e+14
## `Headline.pfx.fctrmyPolitics::`                                    -3.135e+13
## `Headline.pfx.fctrmyFood::`                                        -9.362e+14
## `Headline.pfx.fctrYour Turn::`                                     -4.220e+14
## `Headline.pfx.fctrReaders Respond::`                               -2.556e+14
## `Headline.pfx.fctrAsk Well::`                                      -7.060e+14
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           4.725e+15
## `Headline.pfx.fctrOn This Day::`                                    4.007e+15
## `Headline.pfx.fctrVerbatim::`                                      -3.499e+15
## `Headline.pfx.fctrFirst Draft::`                                   -5.732e+15
## `Headline.pfx.fctrToday in Politics::`                             -7.407e+14
## `Headline.pfx.fctrReporter's Notebook::`                           -1.557e+15
## `Headline.pfx.fctrThe Daily Gift::`                                -4.210e+14
## SectionName.nb.fctrArts                                            -2.565e+15
## `SectionName.nb.fctrBusiness Day`                                  -4.122e+15
## SectionName.nb.fctrHealth                                          -5.326e+14
## SectionName.nb.fctrOpinion                                         -5.235e+14
## SectionName.nb.fctrWorld                                           -3.531e+15
## SectionName.nb.fctrStyles                                          -9.600e+15
## SectionName.nb.fctrTStyle                                          -1.026e+16
## SectionName.nb.fctrTechnology                                      -3.194e+15
## SectionName.nb.fctrMagazine                                        -3.796e+15
## SectionName.nb.fctrMultimedia                                      -3.547e+15
## `SectionName.nb.fctrmyMisc::`                                      -2.663e+15
## SectionName.nb.fctrTravel                                          -3.120e+15
## SectionName.nb.fctrU.S.                                            -3.614e+15
## `SectionName.nb.fctrN.Y. / Region`                                 -2.667e+15
## `SectionName.nb.fctrDaily Clip Report::`                                   NA
## SectionName.nb.fctrOpen                                            -4.027e+15
## `SectionName.nb.fctrReaders Respond::`                              2.174e+14
## SectionName.nb.fctrSports                                          -1.547e+15
## `SectionName.nb.fctrmyEducation::`                                 -1.699e+15
## `SectionName.nb.fctrmyPolitics::`                                  -2.861e+15
## SectionName.nb.fctrNational                                        -3.510e+15
## `SectionName.nb.fctrVerbatim::`                                            NA
## `SectionName.nb.fctrFirst Draft::`                                         NA
## `SectionName.nb.fctrmyMultimedia::`                                -1.773e+15
## `SectionName.nb.fctrToday in Politics::`                                   NA
## `SectionName.nb.fctrReporter's Notebook::`                                 NA
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                                      NA
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrScience                                                    NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrStyles                                              3.362e+15
## NewsDesk.nb.fctrTStyle                                              5.103e+15
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrTravel                                                     NA
## NewsDesk.nb.fctrmyEducation                                        -2.814e+15
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## H.num.chars.log                                                    -3.555e+13
## H.num.words.log                                                     9.193e+14
## H.num.words.unq.log                                                -7.739e+14
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      1.421e+15
## `SubsectionName.nb.fctrScience::Health`                                    NA
## `SubsectionName.nb.fctrOpEd::Opinion`                              -4.664e+12
## `SubsectionName.nb.fctrRoom For Debate`                            -4.471e+15
## `SubsectionName.nb.fctrForeign::World`                             -2.585e+15
## `SubsectionName.nb.fctrFashion & Style`                                    NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrAsia Pacific`                                       NA
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
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                             NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                                 NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## A.num.chars.log                                                    -5.957e+15
## S.num.chars.log                                                     5.906e+15
## A.num.words.log                                                    -3.865e+16
## S.num.words.log                                                     3.859e+16
## A.num.words.unq.log                                                 3.854e+16
## S.num.words.unq.log                                                -3.852e+16
##                                                                    Std. Error
## (Intercept)                                                         2.822e+07
## WordCount.log                                                       1.268e+06
## `PubDate.hour.fctr(7.67,15.3]`                                      3.649e+06
## `PubDate.hour.fctr(15.3,23]`                                        3.943e+06
## H.is.question                                                       4.716e+06
## PubDate.wkend                                                       7.528e+06
## A.can                                                               7.819e+07
## S.can                                                               7.876e+07
## H.has.ebola                                                         9.135e+06
## S.make                                                              5.285e+06
## A.make                                                                     NA
## S.one                                                               1.076e+08
## S.state                                                             5.586e+06
## A.state                                                                    NA
## A.one                                                               1.077e+08
## S.said                                                              5.267e+06
## A.said                                                                     NA
## .rnorm                                                              1.010e+06
## `PubDate.date.fctr(7,13]`                                           3.162e+06
## `PubDate.date.fctr(13,19]`                                          3.122e+06
## `PubDate.date.fctr(19,25]`                                          3.039e+06
## `PubDate.date.fctr(25,31]`                                          3.384e+06
## `PubDate.second.fctr(14.8,29.5]`                                    2.872e+06
## `PubDate.second.fctr(29.5,44.2]`                                    2.860e+06
## `PubDate.second.fctr(44.2,59.1]`                                    2.873e+06
## S.presid                                                            5.760e+06
## A.presid                                                                   NA
## S.take                                                              9.748e+07
## A.take                                                              9.734e+07
## `PubDate.minute.fctr(14.8,29.5]`                                    2.956e+06
## `PubDate.minute.fctr(29.5,44.2]`                                    2.777e+06
## `PubDate.minute.fctr(44.2,59.1]`                                    3.007e+06
## S.new                                                               5.280e+07
## A.new                                                               5.268e+07
## PubDate.wkday.fctr1                                                 8.681e+06
## PubDate.wkday.fctr2                                                 9.186e+06
## PubDate.wkday.fctr3                                                 9.211e+06
## PubDate.wkday.fctr4                                                 9.067e+06
## PubDate.wkday.fctr5                                                 9.213e+06
## PubDate.wkday.fctr6                                                 7.903e+06
## S.day                                                               7.472e+07
## A.day                                                               7.449e+07
## H.X2014                                                             1.002e+07
## S.show                                                              5.418e+06
## A.show                                                                     NA
## S.report                                                            6.020e+06
## A.report                                                                   NA
## S.share                                                             6.372e+06
## A.share                                                                    NA
## S.year                                                              4.739e+06
## A.year                                                                     NA
## S.compani                                                           4.488e+06
## A.compani                                                                  NA
## H.new                                                               5.590e+06
## S.first                                                             5.700e+06
## A.first                                                                    NA
## S.time                                                              7.022e+07
## A.time                                                              7.006e+07
## H.newyork                                                           7.695e+06
## S.articl                                                            1.008e+07
## A.articl                                                                   NA
## S.will                                                              7.130e+07
## A.will                                                              7.128e+07
## H.day                                                               9.132e+06
## S.newyork                                                           5.027e+06
## A.newyork                                                                  NA
## H.today                                                             2.767e+07
## H.report                                                            1.027e+07
## S.intern                                                            9.825e+06
## A.intern                                                                   NA
## H.week                                                              1.171e+07
## H.fashion                                                           1.432e+07
## S.week                                                              5.020e+06
## A.week                                                                     NA
## S.fashion                                                           7.053e+06
## A.fashion                                                                  NA
## `Headline.pfx.fctrmyMisc::`                                         1.310e+07
## `Headline.pfx.fctr19[0-9][0-9]::`                                   2.205e+07
## `Headline.pfx.fctrDaily Report::`                                   2.070e+07
## `Headline.pfx.fctr.*Fashion Week::`                                 2.362e+07
## `Headline.pfx.fctrWhat We're::`                                     1.979e+07
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                   2.223e+07
## `Headline.pfx.fctrToday in Small Business::`                        3.376e+07
## `Headline.pfx.fctrDaily Clip Report::`                              2.430e+07
## `Headline.pfx.fctrMorning Agenda::`                                 1.746e+07
## `Headline.pfx.fctrNew York Today::`                                 3.381e+07
## `Headline.pfx.fctr6 Q's About the News::`                           1.817e+07
## `Headline.pfx.fctrTest Yourself::`                                  1.925e+07
## `Headline.pfx.fctrWord of the Day::`                                2.292e+07
## `Headline.pfx.fctrmyMultimedia::`                                   1.877e+07
## `Headline.pfx.fctrmyTech::`                                         1.572e+07
## `Headline.pfx.fctrmyPolitics::`                                     1.906e+07
## `Headline.pfx.fctrmyFood::`                                         1.568e+07
## `Headline.pfx.fctrYour Turn::`                                      2.797e+07
## `Headline.pfx.fctrReaders Respond::`                                3.105e+07
## `Headline.pfx.fctrAsk Well::`                                       3.173e+07
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           2.940e+07
## `Headline.pfx.fctrOn This Day::`                                    2.835e+07
## `Headline.pfx.fctrVerbatim::`                                       2.134e+07
## `Headline.pfx.fctrFirst Draft::`                                    1.890e+07
## `Headline.pfx.fctrToday in Politics::`                              3.406e+07
## `Headline.pfx.fctrReporter's Notebook::`                            3.312e+07
## `Headline.pfx.fctrThe Daily Gift::`                                 2.240e+07
## SectionName.nb.fctrArts                                             8.887e+06
## `SectionName.nb.fctrBusiness Day`                                   1.254e+07
## SectionName.nb.fctrHealth                                           1.011e+07
## SectionName.nb.fctrOpinion                                          2.004e+07
## SectionName.nb.fctrWorld                                            1.067e+07
## SectionName.nb.fctrStyles                                           5.057e+07
## SectionName.nb.fctrTStyle                                           6.915e+07
## SectionName.nb.fctrTechnology                                       1.023e+07
## SectionName.nb.fctrMagazine                                         1.720e+07
## SectionName.nb.fctrMultimedia                                       1.205e+07
## `SectionName.nb.fctrmyMisc::`                                       8.672e+06
## SectionName.nb.fctrTravel                                           1.091e+07
## SectionName.nb.fctrU.S.                                             4.915e+07
## `SectionName.nb.fctrN.Y. / Region`                                  1.075e+07
## `SectionName.nb.fctrDaily Clip Report::`                                   NA
## SectionName.nb.fctrOpen                                             4.855e+07
## `SectionName.nb.fctrReaders Respond::`                              3.613e+07
## SectionName.nb.fctrSports                                           6.980e+07
## `SectionName.nb.fctrmyEducation::`                                  5.018e+07
## `SectionName.nb.fctrmyPolitics::`                                   1.814e+07
## SectionName.nb.fctrNational                                         4.850e+07
## `SectionName.nb.fctrVerbatim::`                                            NA
## `SectionName.nb.fctrFirst Draft::`                                         NA
## `SectionName.nb.fctrmyMultimedia::`                                 5.089e+07
## `SectionName.nb.fctrToday in Politics::`                                   NA
## `SectionName.nb.fctrReporter's Notebook::`                                 NA
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                                      NA
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrScience                                                    NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrStyles                                              4.917e+07
## NewsDesk.nb.fctrTStyle                                              6.849e+07
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrTravel                                                     NA
## NewsDesk.nb.fctrmyEducation                                         4.956e+07
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## H.num.chars.log                                                     6.540e+06
## H.num.words.log                                                     3.801e+07
## H.num.words.unq.log                                                 3.752e+07
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      9.845e+06
## `SubsectionName.nb.fctrScience::Health`                                    NA
## `SubsectionName.nb.fctrOpEd::Opinion`                               1.862e+07
## `SubsectionName.nb.fctrRoom For Debate`                             2.109e+07
## `SubsectionName.nb.fctrForeign::World`                              1.904e+07
## `SubsectionName.nb.fctrFashion & Style`                                    NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrAsia Pacific`                                       NA
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
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                             NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                                 NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## A.num.chars.log                                                     1.136e+08
## S.num.chars.log                                                     1.136e+08
## A.num.words.log                                                     5.198e+08
## S.num.words.log                                                     5.198e+08
## A.num.words.unq.log                                                 5.234e+08
## S.num.words.unq.log                                                 5.229e+08
##                                                                       z value
## (Intercept)                                                           3947743
## WordCount.log                                                       264519219
## `PubDate.hour.fctr(7.67,15.3]`                                        7194709
## `PubDate.hour.fctr(15.3,23]`                                         -7264584
## H.is.question                                                        20461200
## PubDate.wkend                                                        -1405835
## A.can                                                                90373556
## S.can                                                               -95036155
## H.has.ebola                                                         -29697089
## S.make                                                                3872169
## A.make                                                                     NA
## S.one                                                               -20393498
## S.state                                                              12958006
## A.state                                                                    NA
## A.one                                                                20842414
## S.said                                                               61512829
## A.said                                                                     NA
## .rnorm                                                                1274900
## `PubDate.date.fctr(7,13]`                                             7991643
## `PubDate.date.fctr(13,19]`                                            6308195
## `PubDate.date.fctr(19,25]`                                            1459470
## `PubDate.date.fctr(25,31]`                                           17390416
## `PubDate.second.fctr(14.8,29.5]`                                    -39737143
## `PubDate.second.fctr(29.5,44.2]`                                    -29503498
## `PubDate.second.fctr(44.2,59.1]`                                    -25744820
## S.presid                                                             -7316072
## A.presid                                                                   NA
## S.take                                                              -21931291
## A.take                                                               23224939
## `PubDate.minute.fctr(14.8,29.5]`                                     -7987132
## `PubDate.minute.fctr(29.5,44.2]`                                    -27702850
## `PubDate.minute.fctr(44.2,59.1]`                                      7436728
## S.new                                                                19644762
## A.new                                                               -20367378
## PubDate.wkday.fctr1                                                   1025467
## PubDate.wkday.fctr2                                                 -10543099
## PubDate.wkday.fctr3                                                  -8839123
## PubDate.wkday.fctr4                                                  -9557947
## PubDate.wkday.fctr5                                                 -10540249
## PubDate.wkday.fctr6                                                 -17477493
## S.day                                                               -14622186
## A.day                                                                15762204
## H.X2014                                                             -24243199
## S.show                                                              -19469395
## A.show                                                                     NA
## S.report                                                             23550578
## A.report                                                                   NA
## S.share                                                             -51185412
## A.share                                                                    NA
## S.year                                                              -36444995
## A.year                                                                     NA
## S.compani                                                            35552880
## A.compani                                                                  NA
## H.new                                                               -27719196
## S.first                                                             -17800703
## A.first                                                                    NA
## S.time                                                               75509509
## A.time                                                              -75491049
## H.newyork                                                             6990821
## S.articl                                                            -41675737
## A.articl                                                                   NA
## S.will                                                                -728279
## A.will                                                               -2098333
## H.day                                                               -13453189
## S.newyork                                                            -4320777
## A.newyork                                                                  NA
## H.today                                                            -116347727
## H.report                                                            -27480234
## S.intern                                                             13209819
## A.intern                                                                   NA
## H.week                                                               -5450904
## H.fashion                                                            -4489520
## S.week                                                              -31121279
## A.week                                                                     NA
## S.fashion                                                            77272818
## A.fashion                                                                  NA
## `Headline.pfx.fctrmyMisc::`                                         -11611520
## `Headline.pfx.fctr19[0-9][0-9]::`                                     5678931
## `Headline.pfx.fctrDaily Report::`                                   -22904049
## `Headline.pfx.fctr.*Fashion Week::`                                  78700838
## `Headline.pfx.fctrWhat We're::`                                    -253350626
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                   -33758318
## `Headline.pfx.fctrToday in Small Business::`                         92861408
## `Headline.pfx.fctrDaily Clip Report::`                             -228250127
## `Headline.pfx.fctrMorning Agenda::`                                 -85827635
## `Headline.pfx.fctrNew York Today::`                                  77309219
## `Headline.pfx.fctr6 Q's About the News::`                           152191715
## `Headline.pfx.fctrTest Yourself::`                                  197659970
## `Headline.pfx.fctrWord of the Day::`                                 45775553
## `Headline.pfx.fctrmyMultimedia::`                                   -44743323
## `Headline.pfx.fctrmyTech::`                                          22745538
## `Headline.pfx.fctrmyPolitics::`                                      -1644898
## `Headline.pfx.fctrmyFood::`                                         -59711688
## `Headline.pfx.fctrYour Turn::`                                      -15083471
## `Headline.pfx.fctrReaders Respond::`                                 -8231907
## `Headline.pfx.fctrAsk Well::`                                       -22250076
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           160733293
## `Headline.pfx.fctrOn This Day::`                                    141339385
## `Headline.pfx.fctrVerbatim::`                                      -163971426
## `Headline.pfx.fctrFirst Draft::`                                   -303347836
## `Headline.pfx.fctrToday in Politics::`                              -21749615
## `Headline.pfx.fctrReporter's Notebook::`                            -47004678
## `Headline.pfx.fctrThe Daily Gift::`                                 -18794758
## SectionName.nb.fctrArts                                            -288575264
## `SectionName.nb.fctrBusiness Day`                                  -328788372
## SectionName.nb.fctrHealth                                           -52673299
## SectionName.nb.fctrOpinion                                          -26127954
## SectionName.nb.fctrWorld                                           -330782272
## SectionName.nb.fctrStyles                                          -189850329
## SectionName.nb.fctrTStyle                                          -148434187
## SectionName.nb.fctrTechnology                                      -312161658
## SectionName.nb.fctrMagazine                                        -220687515
## SectionName.nb.fctrMultimedia                                      -294364209
## `SectionName.nb.fctrmyMisc::`                                      -307138314
## SectionName.nb.fctrTravel                                          -285940989
## SectionName.nb.fctrU.S.                                             -73528838
## `SectionName.nb.fctrN.Y. / Region`                                 -247995938
## `SectionName.nb.fctrDaily Clip Report::`                                   NA
## SectionName.nb.fctrOpen                                             -82933498
## `SectionName.nb.fctrReaders Respond::`                                6017347
## SectionName.nb.fctrSports                                           -22164225
## `SectionName.nb.fctrmyEducation::`                                  -33858273
## `SectionName.nb.fctrmyPolitics::`                                  -157697125
## SectionName.nb.fctrNational                                         -72369852
## `SectionName.nb.fctrVerbatim::`                                            NA
## `SectionName.nb.fctrFirst Draft::`                                         NA
## `SectionName.nb.fctrmyMultimedia::`                                 -34842788
## `SectionName.nb.fctrToday in Politics::`                                   NA
## `SectionName.nb.fctrReporter's Notebook::`                                 NA
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                                      NA
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrScience                                                    NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrStyles                                               68359483
## NewsDesk.nb.fctrTStyle                                               74507190
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrTravel                                                     NA
## NewsDesk.nb.fctrmyEducation                                         -56780192
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## H.num.chars.log                                                      -5435805
## H.num.words.log                                                      24185098
## H.num.words.unq.log                                                 -20627570
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      144300917
## `SubsectionName.nb.fctrScience::Health`                                    NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                 -250546
## `SubsectionName.nb.fctrRoom For Debate`                            -211948331
## `SubsectionName.nb.fctrForeign::World`                             -135744688
## `SubsectionName.nb.fctrFashion & Style`                                    NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrAsia Pacific`                                       NA
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
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                             NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                                 NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## A.num.chars.log                                                     -52418342
## S.num.chars.log                                                      51975168
## A.num.words.log                                                     -74350920
## S.num.words.log                                                      74236876
## A.num.words.unq.log                                                  73631557
## S.num.words.unq.log                                                 -73666001
##                                                                    Pr(>|z|)
## (Intercept)                                                          <2e-16
## WordCount.log                                                        <2e-16
## `PubDate.hour.fctr(7.67,15.3]`                                       <2e-16
## `PubDate.hour.fctr(15.3,23]`                                         <2e-16
## H.is.question                                                        <2e-16
## PubDate.wkend                                                        <2e-16
## A.can                                                                <2e-16
## S.can                                                                <2e-16
## H.has.ebola                                                          <2e-16
## S.make                                                               <2e-16
## A.make                                                                   NA
## S.one                                                                <2e-16
## S.state                                                              <2e-16
## A.state                                                                  NA
## A.one                                                                <2e-16
## S.said                                                               <2e-16
## A.said                                                                   NA
## .rnorm                                                               <2e-16
## `PubDate.date.fctr(7,13]`                                            <2e-16
## `PubDate.date.fctr(13,19]`                                           <2e-16
## `PubDate.date.fctr(19,25]`                                           <2e-16
## `PubDate.date.fctr(25,31]`                                           <2e-16
## `PubDate.second.fctr(14.8,29.5]`                                     <2e-16
## `PubDate.second.fctr(29.5,44.2]`                                     <2e-16
## `PubDate.second.fctr(44.2,59.1]`                                     <2e-16
## S.presid                                                             <2e-16
## A.presid                                                                 NA
## S.take                                                               <2e-16
## A.take                                                               <2e-16
## `PubDate.minute.fctr(14.8,29.5]`                                     <2e-16
## `PubDate.minute.fctr(29.5,44.2]`                                     <2e-16
## `PubDate.minute.fctr(44.2,59.1]`                                     <2e-16
## S.new                                                                <2e-16
## A.new                                                                <2e-16
## PubDate.wkday.fctr1                                                  <2e-16
## PubDate.wkday.fctr2                                                  <2e-16
## PubDate.wkday.fctr3                                                  <2e-16
## PubDate.wkday.fctr4                                                  <2e-16
## PubDate.wkday.fctr5                                                  <2e-16
## PubDate.wkday.fctr6                                                  <2e-16
## S.day                                                                <2e-16
## A.day                                                                <2e-16
## H.X2014                                                              <2e-16
## S.show                                                               <2e-16
## A.show                                                                   NA
## S.report                                                             <2e-16
## A.report                                                                 NA
## S.share                                                              <2e-16
## A.share                                                                  NA
## S.year                                                               <2e-16
## A.year                                                                   NA
## S.compani                                                            <2e-16
## A.compani                                                                NA
## H.new                                                                <2e-16
## S.first                                                              <2e-16
## A.first                                                                  NA
## S.time                                                               <2e-16
## A.time                                                               <2e-16
## H.newyork                                                            <2e-16
## S.articl                                                             <2e-16
## A.articl                                                                 NA
## S.will                                                               <2e-16
## A.will                                                               <2e-16
## H.day                                                                <2e-16
## S.newyork                                                            <2e-16
## A.newyork                                                                NA
## H.today                                                              <2e-16
## H.report                                                             <2e-16
## S.intern                                                             <2e-16
## A.intern                                                                 NA
## H.week                                                               <2e-16
## H.fashion                                                            <2e-16
## S.week                                                               <2e-16
## A.week                                                                   NA
## S.fashion                                                            <2e-16
## A.fashion                                                                NA
## `Headline.pfx.fctrmyMisc::`                                          <2e-16
## `Headline.pfx.fctr19[0-9][0-9]::`                                    <2e-16
## `Headline.pfx.fctrDaily Report::`                                    <2e-16
## `Headline.pfx.fctr.*Fashion Week::`                                  <2e-16
## `Headline.pfx.fctrWhat We're::`                                      <2e-16
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                    <2e-16
## `Headline.pfx.fctrToday in Small Business::`                         <2e-16
## `Headline.pfx.fctrDaily Clip Report::`                               <2e-16
## `Headline.pfx.fctrMorning Agenda::`                                  <2e-16
## `Headline.pfx.fctrNew York Today::`                                  <2e-16
## `Headline.pfx.fctr6 Q's About the News::`                            <2e-16
## `Headline.pfx.fctrTest Yourself::`                                   <2e-16
## `Headline.pfx.fctrWord of the Day::`                                 <2e-16
## `Headline.pfx.fctrmyMultimedia::`                                    <2e-16
## `Headline.pfx.fctrmyTech::`                                          <2e-16
## `Headline.pfx.fctrmyPolitics::`                                      <2e-16
## `Headline.pfx.fctrmyFood::`                                          <2e-16
## `Headline.pfx.fctrYour Turn::`                                       <2e-16
## `Headline.pfx.fctrReaders Respond::`                                 <2e-16
## `Headline.pfx.fctrAsk Well::`                                        <2e-16
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                            <2e-16
## `Headline.pfx.fctrOn This Day::`                                     <2e-16
## `Headline.pfx.fctrVerbatim::`                                        <2e-16
## `Headline.pfx.fctrFirst Draft::`                                     <2e-16
## `Headline.pfx.fctrToday in Politics::`                               <2e-16
## `Headline.pfx.fctrReporter's Notebook::`                             <2e-16
## `Headline.pfx.fctrThe Daily Gift::`                                  <2e-16
## SectionName.nb.fctrArts                                              <2e-16
## `SectionName.nb.fctrBusiness Day`                                    <2e-16
## SectionName.nb.fctrHealth                                            <2e-16
## SectionName.nb.fctrOpinion                                           <2e-16
## SectionName.nb.fctrWorld                                             <2e-16
## SectionName.nb.fctrStyles                                            <2e-16
## SectionName.nb.fctrTStyle                                            <2e-16
## SectionName.nb.fctrTechnology                                        <2e-16
## SectionName.nb.fctrMagazine                                          <2e-16
## SectionName.nb.fctrMultimedia                                        <2e-16
## `SectionName.nb.fctrmyMisc::`                                        <2e-16
## SectionName.nb.fctrTravel                                            <2e-16
## SectionName.nb.fctrU.S.                                              <2e-16
## `SectionName.nb.fctrN.Y. / Region`                                   <2e-16
## `SectionName.nb.fctrDaily Clip Report::`                                 NA
## SectionName.nb.fctrOpen                                              <2e-16
## `SectionName.nb.fctrReaders Respond::`                               <2e-16
## SectionName.nb.fctrSports                                            <2e-16
## `SectionName.nb.fctrmyEducation::`                                   <2e-16
## `SectionName.nb.fctrmyPolitics::`                                    <2e-16
## SectionName.nb.fctrNational                                          <2e-16
## `SectionName.nb.fctrVerbatim::`                                          NA
## `SectionName.nb.fctrFirst Draft::`                                       NA
## `SectionName.nb.fctrmyMultimedia::`                                  <2e-16
## `SectionName.nb.fctrToday in Politics::`                                 NA
## `SectionName.nb.fctrReporter's Notebook::`                               NA
## SectionName.nb.fctrCulture                                               NA
## `SectionName.nb.fctrThe Daily Gift::`                                    NA
## NewsDesk.nb.fctrCulture                                                  NA
## NewsDesk.nb.fctrScience                                                  NA
## NewsDesk.nb.fctrOpEd                                                     NA
## NewsDesk.nb.fctrForeign                                                  NA
## NewsDesk.nb.fctrStyles                                               <2e-16
## NewsDesk.nb.fctrTStyle                                               <2e-16
## NewsDesk.nb.fctrMagazine                                                 NA
## NewsDesk.nb.fctrmyMultimedia                                             NA
## `NewsDesk.nb.fctrmyMisc::`                                               NA
## NewsDesk.nb.fctrTravel                                                   NA
## NewsDesk.nb.fctrmyEducation                                          <2e-16
## NewsDesk.nb.fctrMetro                                                    NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                    NA
## `NewsDesk.nb.fctrReaders Respond::`                                      NA
## NewsDesk.nb.fctrNational                                                 NA
## NewsDesk.nb.fctrSports                                                   NA
## `NewsDesk.nb.fctrmyEducation::`                                          NA
## `NewsDesk.nb.fctrmyPolitics::`                                           NA
## `NewsDesk.nb.fctrVerbatim::`                                             NA
## `NewsDesk.nb.fctrFirst Draft::`                                          NA
## `NewsDesk.nb.fctrmyMultimedia::`                                         NA
## `NewsDesk.nb.fctrToday in Politics::`                                    NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                  NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                       NA
## H.num.chars.log                                                      <2e-16
## H.num.words.log                                                      <2e-16
## H.num.words.unq.log                                                  <2e-16
## `SubsectionName.nb.fctrCulture::Arts`                                    NA
## SubsectionName.nb.fctrDealbook                                       <2e-16
## `SubsectionName.nb.fctrScience::Health`                                  NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                <2e-16
## `SubsectionName.nb.fctrRoom For Debate`                              <2e-16
## `SubsectionName.nb.fctrForeign::World`                               <2e-16
## `SubsectionName.nb.fctrFashion & Style`                                  NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                   NA
## `SubsectionName.nb.fctrAsia Pacific`                                     NA
## `SubsectionName.nb.fctrBusiness::Technology`                             NA
## `SubsectionName.nb.fctrMagazine::Magazine`                               NA
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                         NA
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                 NA
## `SubsectionName.nb.fctrTravel::Travel`                                   NA
## SubsectionName.nb.fctrEducation                                          NA
## `SubsectionName.nb.fctrThe Public Editor`                                NA
## `SubsectionName.nb.fctrSmall Business`                                   NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                             NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`           NA
## `SubsectionName.nb.fctrmyMisc::Open`                                     NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`               NA
## SubsectionName.nb.fctrPolitics                                           NA
## `SubsectionName.nb.fctrSports::Sports`                                   NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                       NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                         NA
## `SubsectionName.nb.fctrNational::National`                               NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                             NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                       NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                     NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`           NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`       NA
## `SubsectionName.nb.fctrCulture::Culture`                                 NA
## `SubsectionName.nb.fctrTStyle::Technology`                               NA
## `SubsectionName.nb.fctrmyMisc::U.S.`                                     NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                 NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                   NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                      NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                NA
## `SubsectionName.nb.fctrmyEducation::Travel`                              NA
## A.num.chars.log                                                      <2e-16
## S.num.chars.log                                                      <2e-16
## A.num.words.log                                                      <2e-16
## S.num.words.log                                                      <2e-16
## A.num.words.unq.log                                                  <2e-16
## S.num.words.unq.log                                                  <2e-16
##                                                                       
## (Intercept)                                                        ***
## WordCount.log                                                      ***
## `PubDate.hour.fctr(7.67,15.3]`                                     ***
## `PubDate.hour.fctr(15.3,23]`                                       ***
## H.is.question                                                      ***
## PubDate.wkend                                                      ***
## A.can                                                              ***
## S.can                                                              ***
## H.has.ebola                                                        ***
## S.make                                                             ***
## A.make                                                                
## S.one                                                              ***
## S.state                                                            ***
## A.state                                                               
## A.one                                                              ***
## S.said                                                             ***
## A.said                                                                
## .rnorm                                                             ***
## `PubDate.date.fctr(7,13]`                                          ***
## `PubDate.date.fctr(13,19]`                                         ***
## `PubDate.date.fctr(19,25]`                                         ***
## `PubDate.date.fctr(25,31]`                                         ***
## `PubDate.second.fctr(14.8,29.5]`                                   ***
## `PubDate.second.fctr(29.5,44.2]`                                   ***
## `PubDate.second.fctr(44.2,59.1]`                                   ***
## S.presid                                                           ***
## A.presid                                                              
## S.take                                                             ***
## A.take                                                             ***
## `PubDate.minute.fctr(14.8,29.5]`                                   ***
## `PubDate.minute.fctr(29.5,44.2]`                                   ***
## `PubDate.minute.fctr(44.2,59.1]`                                   ***
## S.new                                                              ***
## A.new                                                              ***
## PubDate.wkday.fctr1                                                ***
## PubDate.wkday.fctr2                                                ***
## PubDate.wkday.fctr3                                                ***
## PubDate.wkday.fctr4                                                ***
## PubDate.wkday.fctr5                                                ***
## PubDate.wkday.fctr6                                                ***
## S.day                                                              ***
## A.day                                                              ***
## H.X2014                                                            ***
## S.show                                                             ***
## A.show                                                                
## S.report                                                           ***
## A.report                                                              
## S.share                                                            ***
## A.share                                                               
## S.year                                                             ***
## A.year                                                                
## S.compani                                                          ***
## A.compani                                                             
## H.new                                                              ***
## S.first                                                            ***
## A.first                                                               
## S.time                                                             ***
## A.time                                                             ***
## H.newyork                                                          ***
## S.articl                                                           ***
## A.articl                                                              
## S.will                                                             ***
## A.will                                                             ***
## H.day                                                              ***
## S.newyork                                                          ***
## A.newyork                                                             
## H.today                                                            ***
## H.report                                                           ***
## S.intern                                                           ***
## A.intern                                                              
## H.week                                                             ***
## H.fashion                                                          ***
## S.week                                                             ***
## A.week                                                                
## S.fashion                                                          ***
## A.fashion                                                             
## `Headline.pfx.fctrmyMisc::`                                        ***
## `Headline.pfx.fctr19[0-9][0-9]::`                                  ***
## `Headline.pfx.fctrDaily Report::`                                  ***
## `Headline.pfx.fctr.*Fashion Week::`                                ***
## `Headline.pfx.fctrWhat We're::`                                    ***
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                  ***
## `Headline.pfx.fctrToday in Small Business::`                       ***
## `Headline.pfx.fctrDaily Clip Report::`                             ***
## `Headline.pfx.fctrMorning Agenda::`                                ***
## `Headline.pfx.fctrNew York Today::`                                ***
## `Headline.pfx.fctr6 Q's About the News::`                          ***
## `Headline.pfx.fctrTest Yourself::`                                 ***
## `Headline.pfx.fctrWord of the Day::`                               ***
## `Headline.pfx.fctrmyMultimedia::`                                  ***
## `Headline.pfx.fctrmyTech::`                                        ***
## `Headline.pfx.fctrmyPolitics::`                                    ***
## `Headline.pfx.fctrmyFood::`                                        ***
## `Headline.pfx.fctrYour Turn::`                                     ***
## `Headline.pfx.fctrReaders Respond::`                               ***
## `Headline.pfx.fctrAsk Well::`                                      ***
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                          ***
## `Headline.pfx.fctrOn This Day::`                                   ***
## `Headline.pfx.fctrVerbatim::`                                      ***
## `Headline.pfx.fctrFirst Draft::`                                   ***
## `Headline.pfx.fctrToday in Politics::`                             ***
## `Headline.pfx.fctrReporter's Notebook::`                           ***
## `Headline.pfx.fctrThe Daily Gift::`                                ***
## SectionName.nb.fctrArts                                            ***
## `SectionName.nb.fctrBusiness Day`                                  ***
## SectionName.nb.fctrHealth                                          ***
## SectionName.nb.fctrOpinion                                         ***
## SectionName.nb.fctrWorld                                           ***
## SectionName.nb.fctrStyles                                          ***
## SectionName.nb.fctrTStyle                                          ***
## SectionName.nb.fctrTechnology                                      ***
## SectionName.nb.fctrMagazine                                        ***
## SectionName.nb.fctrMultimedia                                      ***
## `SectionName.nb.fctrmyMisc::`                                      ***
## SectionName.nb.fctrTravel                                          ***
## SectionName.nb.fctrU.S.                                            ***
## `SectionName.nb.fctrN.Y. / Region`                                 ***
## `SectionName.nb.fctrDaily Clip Report::`                              
## SectionName.nb.fctrOpen                                            ***
## `SectionName.nb.fctrReaders Respond::`                             ***
## SectionName.nb.fctrSports                                          ***
## `SectionName.nb.fctrmyEducation::`                                 ***
## `SectionName.nb.fctrmyPolitics::`                                  ***
## SectionName.nb.fctrNational                                        ***
## `SectionName.nb.fctrVerbatim::`                                       
## `SectionName.nb.fctrFirst Draft::`                                    
## `SectionName.nb.fctrmyMultimedia::`                                ***
## `SectionName.nb.fctrToday in Politics::`                              
## `SectionName.nb.fctrReporter's Notebook::`                            
## SectionName.nb.fctrCulture                                            
## `SectionName.nb.fctrThe Daily Gift::`                                 
## NewsDesk.nb.fctrCulture                                               
## NewsDesk.nb.fctrScience                                               
## NewsDesk.nb.fctrOpEd                                                  
## NewsDesk.nb.fctrForeign                                               
## NewsDesk.nb.fctrStyles                                             ***
## NewsDesk.nb.fctrTStyle                                             ***
## NewsDesk.nb.fctrMagazine                                              
## NewsDesk.nb.fctrmyMultimedia                                          
## `NewsDesk.nb.fctrmyMisc::`                                            
## NewsDesk.nb.fctrTravel                                                
## NewsDesk.nb.fctrmyEducation                                        ***
## NewsDesk.nb.fctrMetro                                                 
## `NewsDesk.nb.fctrDaily Clip Report::`                                 
## `NewsDesk.nb.fctrReaders Respond::`                                   
## NewsDesk.nb.fctrNational                                              
## NewsDesk.nb.fctrSports                                                
## `NewsDesk.nb.fctrmyEducation::`                                       
## `NewsDesk.nb.fctrmyPolitics::`                                        
## `NewsDesk.nb.fctrVerbatim::`                                          
## `NewsDesk.nb.fctrFirst Draft::`                                       
## `NewsDesk.nb.fctrmyMultimedia::`                                      
## `NewsDesk.nb.fctrToday in Politics::`                                 
## `NewsDesk.nb.fctrReporter's Notebook::`                               
## `NewsDesk.nb.fctrThe Daily Gift::`                                    
## H.num.chars.log                                                    ***
## H.num.words.log                                                    ***
## H.num.words.unq.log                                                ***
## `SubsectionName.nb.fctrCulture::Arts`                                 
## SubsectionName.nb.fctrDealbook                                     ***
## `SubsectionName.nb.fctrScience::Health`                               
## `SubsectionName.nb.fctrOpEd::Opinion`                              ***
## `SubsectionName.nb.fctrRoom For Debate`                            ***
## `SubsectionName.nb.fctrForeign::World`                             ***
## `SubsectionName.nb.fctrFashion & Style`                               
## `SubsectionName.nb.fctrTStyle::TStyle`                                
## `SubsectionName.nb.fctrAsia Pacific`                                  
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
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`            
## SubsectionName.nb.fctrPolitics                                        
## `SubsectionName.nb.fctrSports::Sports`                                
## `SubsectionName.nb.fctrmyEducation::myEducation::`                    
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                      
## `SubsectionName.nb.fctrNational::National`                            
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                          
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                    
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                  
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`        
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`    
## `SubsectionName.nb.fctrCulture::Culture`                              
## `SubsectionName.nb.fctrTStyle::Technology`                            
## `SubsectionName.nb.fctrmyMisc::U.S.`                                  
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`              
## `SubsectionName.nb.fctrmyMisc::Travel`                                
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                   
## `SubsectionName.nb.fctrmyEducation::U.S.`                             
## `SubsectionName.nb.fctrmyEducation::Travel`                           
## A.num.chars.log                                                    ***
## S.num.chars.log                                                    ***
## A.num.words.log                                                    ***
## S.num.words.log                                                    ***
## A.num.words.unq.log                                                ***
## S.num.words.unq.log                                                ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance:  4042.7  on 4474  degrees of freedom
## Residual deviance: 30204.6  on 4350  degrees of freedom
## AIC: 30455
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
##                                            id        cor.y exclude.as.feat
## SectionName.nb.fctr       SectionName.nb.fctr -0.146001417           FALSE
## Headline.pfx.fctr           Headline.pfx.fctr -0.088287365           FALSE
## WordCount.log                   WordCount.log  0.265800360           FALSE
## SubsectionName.nb.fctr SubsectionName.nb.fctr -0.206432730           FALSE
## H.today                               H.today -0.063723058           FALSE
## S.can                                   S.can  0.029999780           FALSE
## A.can                                   A.can  0.031498867           FALSE
## S.fashion                           S.fashion -0.086446251           FALSE
## S.time                                 S.time -0.057595102           FALSE
## A.time                                 A.time -0.057790617           FALSE
## NewsDesk.nb.fctr             NewsDesk.nb.fctr -0.167013566           FALSE
## A.num.words.log               A.num.words.log -0.245073324           FALSE
## S.num.words.log               S.num.words.log -0.245354135           FALSE
## S.num.words.unq.log       S.num.words.unq.log -0.250796919           FALSE
## A.num.words.unq.log       A.num.words.unq.log -0.250601203           FALSE
## S.said                                 S.said  0.001363226           FALSE
## A.num.chars.log               A.num.chars.log -0.224548821           FALSE
## S.num.chars.log               S.num.chars.log -0.224692967           FALSE
## S.share                               S.share -0.050329686           FALSE
## S.articl                             S.articl -0.059520554           FALSE
## PubDate.second.fctr       PubDate.second.fctr -0.011879458           FALSE
## S.year                                 S.year -0.051146178           FALSE
## S.compani                           S.compani -0.053012962           FALSE
## S.week                                 S.week -0.084814939           FALSE
## H.has.ebola                       H.has.ebola  0.025881397           FALSE
## H.new                                   H.new -0.053121542           FALSE
## PubDate.minute.fctr       PubDate.minute.fctr -0.034073846           FALSE
## H.report                             H.report -0.064948102           FALSE
## H.X2014                               H.X2014 -0.046206380           FALSE
## H.num.words.log               H.num.words.log -0.200686356           FALSE
## S.report                             S.report -0.050211524           FALSE
## A.take                                 A.take -0.026086108           FALSE
## S.take                                 S.take -0.025762398           FALSE
## A.one                                   A.one  0.005696039           FALSE
## H.num.words.unq.log       H.num.words.unq.log -0.204496360           FALSE
## H.is.question                   H.is.question  0.129154799           FALSE
## S.one                                   S.one  0.006342094           FALSE
## A.new                                   A.new -0.035359447           FALSE
## S.new                                   S.new -0.034948520           FALSE
## S.show                                 S.show -0.048801740           FALSE
## S.first                               S.first -0.053388178           FALSE
## PubDate.wkday.fctr         PubDate.wkday.fctr -0.039801288           FALSE
## PubDate.date.fctr           PubDate.date.fctr -0.011647558           FALSE
## A.day                                   A.day -0.045909684           FALSE
## S.day                                   S.day -0.045649185           FALSE
## H.day                                   H.day -0.061669687           FALSE
## S.intern                             S.intern -0.068485701           FALSE
## S.state                               S.state  0.006069626           FALSE
## S.presid                             S.presid -0.019828826           FALSE
## PubDate.hour.fctr           PubDate.hour.fctr  0.135436805           FALSE
## H.newyork                           H.newyork -0.057970095           FALSE
## H.week                                 H.week -0.075105216           FALSE
## H.num.chars.log               H.num.chars.log -0.171062360           FALSE
## H.fashion                           H.fashion -0.081708612           FALSE
## S.newyork                           S.newyork -0.062117105           FALSE
## S.make                                 S.make  0.023138853           FALSE
## A.will                                 A.will -0.061025004           FALSE
## PubDate.wkend                   PubDate.wkend  0.106728760           FALSE
## .rnorm                                 .rnorm -0.008703337           FALSE
## S.will                                 S.will -0.060575493           FALSE
## A.articl                             A.articl -0.059520554           FALSE
## A.compani                           A.compani -0.053099633           FALSE
## A.fashion                           A.fashion -0.086446251           FALSE
## A.first                               A.first -0.053388178           FALSE
## A.has.http                         A.has.http -0.013592603           FALSE
## A.intern                             A.intern -0.068485701           FALSE
## A.make                                 A.make  0.023138853           FALSE
## A.newyork                           A.newyork -0.062117105           FALSE
## A.num.chars                       A.num.chars -0.177037425            TRUE
## A.num.words                       A.num.words -0.204211072            TRUE
## A.num.words.unq               A.num.words.unq -0.210242145            TRUE
## A.presid                             A.presid -0.019828826           FALSE
## A.report                             A.report -0.050211524           FALSE
## A.said                                 A.said  0.001363226           FALSE
## A.share                               A.share -0.050329686           FALSE
## A.show                                 A.show -0.048801740           FALSE
## A.state                               A.state  0.005702163           FALSE
## A.week                                 A.week -0.084814939           FALSE
## A.year                                 A.year -0.051146178           FALSE
## H.daili                               H.daili -0.069192975           FALSE
## H.has.http                         H.has.http           NA           FALSE
## H.num.chars                       H.num.chars -0.147211183            TRUE
## H.num.words                       H.num.words -0.186036895            TRUE
## H.num.words.unq               H.num.words.unq -0.189702157            TRUE
## H.X2015                               H.X2015 -0.066584892           FALSE
## Popular                               Popular  1.000000000            TRUE
## Popular.fctr                     Popular.fctr           NA            TRUE
## PubDate.month.fctr         PubDate.month.fctr  0.019148739            TRUE
## PubDate.year.fctr           PubDate.year.fctr           NA           FALSE
## S.has.http                         S.has.http           NA           FALSE
## S.num.chars                       S.num.chars -0.179331806            TRUE
## S.num.words                       S.num.words -0.206385049            TRUE
## S.num.words.unq               S.num.words.unq -0.212102717            TRUE
## UniqueID                             UniqueID  0.011824920            TRUE
## WordCount                           WordCount  0.257526549            TRUE
##                          cor.y.abs          cor.high.X is.ConditionalX.y
## SectionName.nb.fctr    0.146001417                <NA>              TRUE
## Headline.pfx.fctr      0.088287365                <NA>              TRUE
## WordCount.log          0.265800360                <NA>              TRUE
## SubsectionName.nb.fctr 0.206432730    NewsDesk.nb.fctr              TRUE
## H.today                0.063723058                <NA>              TRUE
## S.can                  0.029999780                <NA>              TRUE
## A.can                  0.031498867               S.can              TRUE
## S.fashion              0.086446251                <NA>              TRUE
## S.time                 0.057595102                <NA>              TRUE
## A.time                 0.057790617              S.time              TRUE
## NewsDesk.nb.fctr       0.167013566 SectionName.nb.fctr              TRUE
## A.num.words.log        0.245073324                <NA>              TRUE
## S.num.words.log        0.245354135     A.num.words.log              TRUE
## S.num.words.unq.log    0.250796919     S.num.chars.log              TRUE
## A.num.words.unq.log    0.250601203                <NA>              TRUE
## S.said                 0.001363226                <NA>              TRUE
## A.num.chars.log        0.224548821                <NA>              TRUE
## S.num.chars.log        0.224692967     A.num.chars.log              TRUE
## S.share                0.050329686                <NA>              TRUE
## S.articl               0.059520554                <NA>              TRUE
## PubDate.second.fctr    0.011879458                <NA>              TRUE
## S.year                 0.051146178                <NA>              TRUE
## S.compani              0.053012962                <NA>              TRUE
## S.week                 0.084814939                <NA>              TRUE
## H.has.ebola            0.025881397                <NA>              TRUE
## H.new                  0.053121542                <NA>              TRUE
## PubDate.minute.fctr    0.034073846                <NA>              TRUE
## H.report               0.064948102                <NA>              TRUE
## H.X2014                0.046206380                <NA>              TRUE
## H.num.words.log        0.200686356                <NA>              TRUE
## S.report               0.050211524                <NA>              TRUE
## A.take                 0.026086108              S.take              TRUE
## S.take                 0.025762398                <NA>              TRUE
## A.one                  0.005696039                <NA>              TRUE
## H.num.words.unq.log    0.204496360     H.num.chars.log              TRUE
## H.is.question          0.129154799                <NA>              TRUE
## S.one                  0.006342094                <NA>              TRUE
## A.new                  0.035359447               S.new              TRUE
## S.new                  0.034948520                <NA>              TRUE
## S.show                 0.048801740                <NA>              TRUE
## S.first                0.053388178                <NA>              TRUE
## PubDate.wkday.fctr     0.039801288                <NA>              TRUE
## PubDate.date.fctr      0.011647558                <NA>              TRUE
## A.day                  0.045909684               S.day              TRUE
## S.day                  0.045649185                <NA>              TRUE
## H.day                  0.061669687                <NA>              TRUE
## S.intern               0.068485701                <NA>              TRUE
## S.state                0.006069626                <NA>              TRUE
## S.presid               0.019828826                <NA>              TRUE
## PubDate.hour.fctr      0.135436805                <NA>              TRUE
## H.newyork              0.057970095                <NA>              TRUE
## H.week                 0.075105216                <NA>              TRUE
## H.num.chars.log        0.171062360                <NA>              TRUE
## H.fashion              0.081708612              H.week              TRUE
## S.newyork              0.062117105                <NA>              TRUE
## S.make                 0.023138853                <NA>              TRUE
## A.will                 0.061025004              S.will              TRUE
## PubDate.wkend          0.106728760                <NA>              TRUE
## .rnorm                 0.008703337                <NA>              TRUE
## S.will                 0.060575493                <NA>              TRUE
## A.articl               0.059520554            S.articl              TRUE
## A.compani              0.053099633           S.compani              TRUE
## A.fashion              0.086446251           S.fashion              TRUE
## A.first                0.053388178             S.first              TRUE
## A.has.http             0.013592603                <NA>             FALSE
## A.intern               0.068485701            S.intern              TRUE
## A.make                 0.023138853              S.make              TRUE
## A.newyork              0.062117105           S.newyork              TRUE
## A.num.chars            0.177037425                <NA>                NA
## A.num.words            0.204211072                <NA>                NA
## A.num.words.unq        0.210242145                <NA>                NA
## A.presid               0.019828826            S.presid              TRUE
## A.report               0.050211524            S.report              TRUE
## A.said                 0.001363226                <NA>              TRUE
## A.share                0.050329686             S.share              TRUE
## A.show                 0.048801740              S.show              TRUE
## A.state                0.005702163                <NA>              TRUE
## A.week                 0.084814939              S.week              TRUE
## A.year                 0.051146178              S.year              TRUE
## H.daili                0.069192975                <NA>             FALSE
## H.has.http                      NA                <NA>             FALSE
## H.num.chars            0.147211183                <NA>                NA
## H.num.words            0.186036895                <NA>                NA
## H.num.words.unq        0.189702157                <NA>                NA
## H.X2015                0.066584892                <NA>             FALSE
## Popular                1.000000000                <NA>                NA
## Popular.fctr                    NA                <NA>                NA
## PubDate.month.fctr     0.019148739                <NA>                NA
## PubDate.year.fctr               NA                <NA>             FALSE
## S.has.http                      NA                <NA>             FALSE
## S.num.chars            0.179331806                <NA>                NA
## S.num.words            0.206385049                <NA>                NA
## S.num.words.unq        0.212102717                <NA>                NA
## UniqueID               0.011824920                <NA>                NA
## WordCount              0.257526549                <NA>                NA
##                        is.cor.y.abs.low rsp_var_raw id_var rsp_var
## SectionName.nb.fctr               FALSE       FALSE     NA      NA
## Headline.pfx.fctr                 FALSE       FALSE     NA      NA
## WordCount.log                     FALSE       FALSE     NA      NA
## SubsectionName.nb.fctr            FALSE       FALSE     NA      NA
## H.today                           FALSE       FALSE     NA      NA
## S.can                             FALSE       FALSE     NA      NA
## A.can                             FALSE       FALSE     NA      NA
## S.fashion                         FALSE       FALSE     NA      NA
## S.time                            FALSE       FALSE     NA      NA
## A.time                            FALSE       FALSE     NA      NA
## NewsDesk.nb.fctr                  FALSE       FALSE     NA      NA
## A.num.words.log                   FALSE       FALSE     NA      NA
## S.num.words.log                   FALSE       FALSE     NA      NA
## S.num.words.unq.log               FALSE       FALSE     NA      NA
## A.num.words.unq.log               FALSE       FALSE     NA      NA
## S.said                             TRUE       FALSE     NA      NA
## A.num.chars.log                   FALSE       FALSE     NA      NA
## S.num.chars.log                   FALSE       FALSE     NA      NA
## S.share                           FALSE       FALSE     NA      NA
## S.articl                          FALSE       FALSE     NA      NA
## PubDate.second.fctr               FALSE       FALSE     NA      NA
## S.year                            FALSE       FALSE     NA      NA
## S.compani                         FALSE       FALSE     NA      NA
## S.week                            FALSE       FALSE     NA      NA
## H.has.ebola                       FALSE       FALSE     NA      NA
## H.new                             FALSE       FALSE     NA      NA
## PubDate.minute.fctr               FALSE       FALSE     NA      NA
## H.report                          FALSE       FALSE     NA      NA
## H.X2014                           FALSE       FALSE     NA      NA
## H.num.words.log                   FALSE       FALSE     NA      NA
## S.report                          FALSE       FALSE     NA      NA
## A.take                            FALSE       FALSE     NA      NA
## S.take                            FALSE       FALSE     NA      NA
## A.one                              TRUE       FALSE     NA      NA
## H.num.words.unq.log               FALSE       FALSE     NA      NA
## H.is.question                     FALSE       FALSE     NA      NA
## S.one                              TRUE       FALSE     NA      NA
## A.new                             FALSE       FALSE     NA      NA
## S.new                             FALSE       FALSE     NA      NA
## S.show                            FALSE       FALSE     NA      NA
## S.first                           FALSE       FALSE     NA      NA
## PubDate.wkday.fctr                FALSE       FALSE     NA      NA
## PubDate.date.fctr                 FALSE       FALSE     NA      NA
## A.day                             FALSE       FALSE     NA      NA
## S.day                             FALSE       FALSE     NA      NA
## H.day                             FALSE       FALSE     NA      NA
## S.intern                          FALSE       FALSE     NA      NA
## S.state                            TRUE       FALSE     NA      NA
## S.presid                          FALSE       FALSE     NA      NA
## PubDate.hour.fctr                 FALSE       FALSE     NA      NA
## H.newyork                         FALSE       FALSE     NA      NA
## H.week                            FALSE       FALSE     NA      NA
## H.num.chars.log                   FALSE       FALSE     NA      NA
## H.fashion                         FALSE       FALSE     NA      NA
## S.newyork                         FALSE       FALSE     NA      NA
## S.make                            FALSE       FALSE     NA      NA
## A.will                            FALSE       FALSE     NA      NA
## PubDate.wkend                     FALSE       FALSE     NA      NA
## .rnorm                            FALSE       FALSE     NA      NA
## S.will                            FALSE       FALSE     NA      NA
## A.articl                          FALSE       FALSE     NA      NA
## A.compani                         FALSE       FALSE     NA      NA
## A.fashion                         FALSE       FALSE     NA      NA
## A.first                           FALSE       FALSE     NA      NA
## A.has.http                        FALSE       FALSE     NA      NA
## A.intern                          FALSE       FALSE     NA      NA
## A.make                            FALSE       FALSE     NA      NA
## A.newyork                         FALSE       FALSE     NA      NA
## A.num.chars                       FALSE       FALSE     NA      NA
## A.num.words                       FALSE       FALSE     NA      NA
## A.num.words.unq                   FALSE       FALSE     NA      NA
## A.presid                          FALSE       FALSE     NA      NA
## A.report                          FALSE       FALSE     NA      NA
## A.said                             TRUE       FALSE     NA      NA
## A.share                           FALSE       FALSE     NA      NA
## A.show                            FALSE       FALSE     NA      NA
## A.state                            TRUE       FALSE     NA      NA
## A.week                            FALSE       FALSE     NA      NA
## A.year                            FALSE       FALSE     NA      NA
## H.daili                           FALSE       FALSE     NA      NA
## H.has.http                           NA       FALSE     NA      NA
## H.num.chars                       FALSE       FALSE     NA      NA
## H.num.words                       FALSE       FALSE     NA      NA
## H.num.words.unq                   FALSE       FALSE     NA      NA
## H.X2015                           FALSE       FALSE     NA      NA
## Popular                           FALSE        TRUE     NA      NA
## Popular.fctr                         NA          NA     NA    TRUE
## PubDate.month.fctr                FALSE       FALSE     NA      NA
## PubDate.year.fctr                    NA       FALSE     NA      NA
## S.has.http                           NA       FALSE     NA      NA
## S.num.chars                       FALSE       FALSE     NA      NA
## S.num.words                       FALSE       FALSE     NA      NA
## S.num.words.unq                   FALSE       FALSE     NA      NA
## UniqueID                          FALSE       FALSE   TRUE      NA
## WordCount                         FALSE       FALSE     NA      NA
##                         importance Conditional.X.glm.importance
## SectionName.nb.fctr    100.0000000                  100.0000000
## Headline.pfx.fctr       91.6999082                   91.6999082
## WordCount.log           79.9525891                   79.9525891
## SubsectionName.nb.fctr  64.0476445                   64.0476445
## H.today                 35.1243683                   35.1243683
## S.can                   28.6767051                   28.6767051
## A.can                   27.2660693                   27.2660693
## S.fashion               23.3025352                   23.3025352
## S.time                  22.7690587                   22.7690587
## A.time                  22.7634736                   22.7634736
## NewsDesk.nb.fctr        22.4658143                   22.4658143
## A.num.words.log         22.4185359                   22.4185359
## S.num.words.log         22.3840328                   22.3840328
## S.num.words.unq.log     22.2113186                   22.2113186
## A.num.words.unq.log     22.2008978                   22.2008978
## S.said                  18.5344638                   18.5344638
## A.num.chars.log         15.7829921                   15.7829921
## S.num.chars.log         15.6489129                   15.6489129
## S.share                 15.4099777                   15.4099777
## S.articl                12.5328941                   12.5328941
## PubDate.second.fctr     11.9463864                   11.9463864
## S.year                  10.9503706                   10.9503706
## S.compani               10.6804675                   10.6804675
## S.week                   9.3397184                    9.3397184
## H.has.ebola              8.9088400                    8.9088400
## H.new                    8.3104425                    8.3104425
## PubDate.minute.fctr      8.3054974                    8.3054974
## H.report                 8.2381465                    8.2381465
## H.X2014                  7.2588049                    7.2588049
## H.num.words.log          7.2412268                    7.2412268
## S.report                 7.0492572                    7.0492572
## A.take                   6.9507375                    6.9507375
## S.take                   6.5593535                    6.5593535
## A.one                    6.2299217                    6.2299217
## H.num.words.unq.log      6.1649222                    6.1649222
## H.is.question            6.1145880                    6.1145880
## S.one                    6.0941052                    6.0941052
## A.new                    6.0862030                    6.0862030
## S.new                    5.8675807                    5.8675807
## S.show                   5.8145248                    5.8145248
## S.first                  5.3096739                    5.3096739
## PubDate.wkday.fctr       5.2118890                    5.2118890
## PubDate.date.fctr        5.1855444                    5.1855444
## A.day                    4.6929407                    4.6929407
## S.day                    4.3480365                    4.3480365
## H.day                    3.9943648                    3.9943648
## S.intern                 3.9207350                    3.9207350
## S.state                  3.8445505                    3.8445505
## S.presid                 2.1376240                    2.1376240
## PubDate.hour.fctr        2.1220468                    2.1220468
## H.newyork                2.0392217                    2.0392217
## H.week                   1.5733310                    1.5733310
## H.num.chars.log          1.5687626                    1.5687626
## H.fashion                1.2824711                    1.2824711
## S.newyork                1.2314191                    1.2314191
## S.make                   1.0956959                    1.0956959
## A.will                   0.5590347                    0.5590347
## PubDate.wkend            0.3495243                    0.3495243
## .rnorm                   0.3099109                    0.3099109
## S.will                   0.1445346                    0.1445346
## A.articl                        NA                           NA
## A.compani                       NA                           NA
## A.fashion                       NA                           NA
## A.first                         NA                           NA
## A.has.http                      NA                           NA
## A.intern                        NA                           NA
## A.make                          NA                           NA
## A.newyork                       NA                           NA
## A.num.chars                     NA                           NA
## A.num.words                     NA                           NA
## A.num.words.unq                 NA                           NA
## A.presid                        NA                           NA
## A.report                        NA                           NA
## A.said                          NA                           NA
## A.share                         NA                           NA
## A.show                          NA                           NA
## A.state                         NA                           NA
## A.week                          NA                           NA
## A.year                          NA                           NA
## H.daili                         NA                           NA
## H.has.http                      NA                           NA
## H.num.chars                     NA                           NA
## H.num.words                     NA                           NA
## H.num.words.unq                 NA                           NA
## H.X2015                         NA                           NA
## Popular                         NA                           NA
## Popular.fctr                    NA                           NA
## PubDate.month.fctr              NA                           NA
## PubDate.year.fctr               NA                           NA
## S.has.http                      NA                           NA
## S.num.chars                     NA                           NA
## S.num.words                     NA                           NA
## S.num.words.unq                 NA                           NA
## UniqueID                        NA                           NA
## WordCount                       NA                           NA
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 60
```

![](NYTBlogs_zoo_files/figure-html/fit.models_2-8.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_2-9.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_2-10.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_2-11.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_2-12.png) 

```
## [1] "Min/Max Boundaries: "
## [1] UniqueID                                       
## [2] Popular.fctr                                   
## [3] Popular.fctr.predict.Conditional.X.glm.prob    
## [4] Popular.fctr.predict.Conditional.X.glm         
## [5] Popular.fctr.predict.Conditional.X.glm.accurate
## [6] Popular.fctr.predict.Conditional.X.glm.error   
## [7] .label                                         
## <0 rows> (or 0-length row.names)
## [1] "Inaccurate: "
##     UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 4          4            Y                                2.220446e-16
## 92        92            Y                                2.220446e-16
## 116      116            Y                                2.220446e-16
## 130      130            Y                                2.220446e-16
## 446      446            Y                                2.220446e-16
## 450      450            Y                                2.220446e-16
##     Popular.fctr.predict.Conditional.X.glm
## 4                                        N
## 92                                       N
## 116                                      N
## 130                                      N
## 446                                      N
## 450                                      N
##     Popular.fctr.predict.Conditional.X.glm.accurate
## 4                                             FALSE
## 92                                            FALSE
## 116                                           FALSE
## 130                                           FALSE
## 446                                           FALSE
## 450                                           FALSE
##     Popular.fctr.predict.Conditional.X.glm.error
## 4                                           -0.9
## 92                                          -0.9
## 116                                         -0.9
## 130                                         -0.9
## 446                                         -0.9
## 450                                         -0.9
##      UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 1156     1156            Y                                2.220446e-16
## 3765     3765            Y                                2.220446e-16
## 4012     4012            Y                                2.220446e-16
## 17         17            N                                1.000000e+00
## 3312     3312            N                                1.000000e+00
## 5317     5317            N                                1.000000e+00
##      Popular.fctr.predict.Conditional.X.glm
## 1156                                      N
## 3765                                      N
## 4012                                      N
## 17                                        Y
## 3312                                      Y
## 5317                                      Y
##      Popular.fctr.predict.Conditional.X.glm.accurate
## 1156                                           FALSE
## 3765                                           FALSE
## 4012                                           FALSE
## 17                                             FALSE
## 3312                                           FALSE
## 5317                                           FALSE
##      Popular.fctr.predict.Conditional.X.glm.error
## 1156                                         -0.9
## 3765                                         -0.9
## 4012                                         -0.9
## 17                                            0.1
## 3312                                          0.1
## 5317                                          0.1
##      UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 6351     6351            N                                           1
## 6400     6400            N                                           1
## 6409     6409            N                                           1
## 6412     6412            N                                           1
## 6435     6435            N                                           1
## 6479     6479            N                                           1
##      Popular.fctr.predict.Conditional.X.glm
## 6351                                      Y
## 6400                                      Y
## 6409                                      Y
## 6412                                      Y
## 6435                                      Y
## 6479                                      Y
##      Popular.fctr.predict.Conditional.X.glm.accurate
## 6351                                           FALSE
## 6400                                           FALSE
## 6409                                           FALSE
## 6412                                           FALSE
## 6435                                           FALSE
## 6479                                           FALSE
##      Popular.fctr.predict.Conditional.X.glm.error
## 6351                                          0.1
## 6400                                          0.1
## 6409                                          0.1
## 6412                                          0.1
## 6435                                          0.1
## 6479                                          0.1
```

![](NYTBlogs_zoo_files/figure-html/fit.models_2-13.png) 

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
##      Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 92              Y                                2.220446e-16
## 693             Y                                2.220446e-16
## 4020            Y                                2.220446e-16
## 4721            Y                                2.220446e-16
##      Popular.fctr.predict.Conditional.X.glm
## 92                                        N
## 693                                       N
## 4020                                      N
## 4721                                      N
##      Popular.fctr.predict.Conditional.X.glm.accurate
## 92                                             FALSE
## 693                                            FALSE
## 4020                                           FALSE
## 4721                                           FALSE
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
##      SectionName.nb.fctr Headline.pfx.fctr WordCount.log
## 92          Business Day          myMisc::      5.723585
## 693         Business Day          myMisc::      5.641907
## 4020       N.Y. / Region    myMultimedia::      3.610918
## 4721               World          myMisc::      6.030685
##      SubsectionName.nb.fctr H.today
## 92                 Dealbook       0
## 693          Small Business       0
## 4020   Metro::N.Y. / Region       0
## 4721           Asia Pacific       0
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
## 11 fit.models          6          2 645.449 663.781  18.332
## 12 fit.models          6          3 663.782      NA      NA
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
## [1] "Popular.fctr.predict.Conditional.X.glm.prob"    
## [2] "Popular.fctr.predict.Conditional.X.glm"         
## [3] "Popular.fctr.predict.Conditional.X.glm.accurate"
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

![](NYTBlogs_zoo_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 12        fit.models          6          3 663.782 743.161  79.379
## 13 fit.data.training          7          0 743.161      NA      NA
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
##                                            id  importance
## SectionName.nb.fctr       SectionName.nb.fctr 100.0000000
## Headline.pfx.fctr           Headline.pfx.fctr  91.6999082
## WordCount.log                   WordCount.log  79.9525891
## SubsectionName.nb.fctr SubsectionName.nb.fctr  64.0476445
## H.today                               H.today  35.1243683
## S.can                                   S.can  28.6767051
## A.can                                   A.can  27.2660693
## S.fashion                           S.fashion  23.3025352
## S.time                                 S.time  22.7690587
## A.time                                 A.time  22.7634736
## NewsDesk.nb.fctr             NewsDesk.nb.fctr  22.4658143
## A.num.words.log               A.num.words.log  22.4185359
## S.num.words.log               S.num.words.log  22.3840328
## S.num.words.unq.log       S.num.words.unq.log  22.2113186
## A.num.words.unq.log       A.num.words.unq.log  22.2008978
## S.said                                 S.said  18.5344638
## A.num.chars.log               A.num.chars.log  15.7829921
## S.num.chars.log               S.num.chars.log  15.6489129
## S.share                               S.share  15.4099777
## S.articl                             S.articl  12.5328941
## PubDate.second.fctr       PubDate.second.fctr  11.9463864
## S.year                                 S.year  10.9503706
## S.compani                           S.compani  10.6804675
## S.week                                 S.week   9.3397184
## H.has.ebola                       H.has.ebola   8.9088400
## H.new                                   H.new   8.3104425
## PubDate.minute.fctr       PubDate.minute.fctr   8.3054974
## H.report                             H.report   8.2381465
## H.X2014                               H.X2014   7.2588049
## H.num.words.log               H.num.words.log   7.2412268
## S.report                             S.report   7.0492572
## A.take                                 A.take   6.9507375
## S.take                                 S.take   6.5593535
## A.one                                   A.one   6.2299217
## H.num.words.unq.log       H.num.words.unq.log   6.1649222
## H.is.question                   H.is.question   6.1145880
## S.one                                   S.one   6.0941052
## A.new                                   A.new   6.0862030
## S.new                                   S.new   5.8675807
## S.show                                 S.show   5.8145248
## S.first                               S.first   5.3096739
## PubDate.wkday.fctr         PubDate.wkday.fctr   5.2118890
## PubDate.date.fctr           PubDate.date.fctr   5.1855444
## A.day                                   A.day   4.6929407
## S.day                                   S.day   4.3480365
## H.day                                   H.day   3.9943648
## S.intern                             S.intern   3.9207350
## S.state                               S.state   3.8445505
## S.presid                             S.presid   2.1376240
## PubDate.hour.fctr           PubDate.hour.fctr   2.1220468
## H.newyork                           H.newyork   2.0392217
## H.week                                 H.week   1.5733310
## H.num.chars.log               H.num.chars.log   1.5687626
## H.fashion                           H.fashion   1.2824711
## S.newyork                           S.newyork   1.2314191
## S.make                                 S.make   1.0956959
## A.will                                 A.will   0.5590347
## PubDate.wkend                   PubDate.wkend   0.3495243
## .rnorm                                 .rnorm   0.3099109
## S.will                                 S.will   0.1445346
## [1] "fitting model: Final.glm"
## [1] "    indep_vars: SectionName.nb.fctr, Headline.pfx.fctr, WordCount.log, SubsectionName.nb.fctr, H.today, S.can, A.can, S.fashion, S.time, A.time, NewsDesk.nb.fctr, A.num.words.log, S.num.words.log, S.num.words.unq.log, A.num.words.unq.log, S.said, A.num.chars.log, S.num.chars.log, S.share, S.articl, PubDate.second.fctr, S.year, S.compani, S.week, H.has.ebola, H.new, PubDate.minute.fctr, H.report, H.X2014, H.num.words.log, S.report, A.take, S.take, A.one, H.num.words.unq.log, H.is.question, S.one, A.new, S.new, S.show, S.first, PubDate.wkday.fctr, PubDate.date.fctr, A.day, S.day, H.day, S.intern, S.state, S.presid, PubDate.hour.fctr, H.newyork, H.week, H.num.chars.log, H.fashion, S.newyork, S.make, A.will, PubDate.wkend, .rnorm, S.will"
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
##   2475, 4746, 4995, 5262, 5304, 5546, 6185, 6207, 6244, 6329, 6404
```

![](NYTBlogs_zoo_files/figure-html/fit.data.training_0-1.png) ![](NYTBlogs_zoo_files/figure-html/fit.data.training_0-2.png) 

```
## Warning: not plotting observations with leverage one:
##   2475, 4746, 4995, 5262, 5304, 5546, 6185, 6207, 6244, 6329, 6404
```

![](NYTBlogs_zoo_files/figure-html/fit.data.training_0-3.png) 

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
## -3.1719  -0.3328  -0.1408   0.0000   3.4183  
## 
## Coefficients: (56 not defined because of singularities)
##                                                                      Estimate
## (Intercept)                                                        -1.821e+00
## SectionName.nb.fctrArts                                            -3.216e+00
## `SectionName.nb.fctrBusiness Day`                                  -4.458e+00
## SectionName.nb.fctrHealth                                          -6.691e-02
## SectionName.nb.fctrOpinion                                         -7.341e-02
## SectionName.nb.fctrWorld                                           -5.161e+00
## SectionName.nb.fctrStyles                                           3.893e+13
## SectionName.nb.fctrTStyle                                          -4.680e+00
## SectionName.nb.fctrTechnology                                       3.893e+13
## SectionName.nb.fctrMagazine                                        -2.749e+01
## SectionName.nb.fctrMultimedia                                      -4.974e+00
## `SectionName.nb.fctrmyMisc::`                                      -2.816e+00
## SectionName.nb.fctrTravel                                          -3.021e+13
## SectionName.nb.fctrU.S.                                             3.893e+13
## `SectionName.nb.fctrN.Y. / Region`                                 -1.969e+00
## `SectionName.nb.fctrDaily Clip Report::`                           -2.601e+01
## SectionName.nb.fctrOpen                                            -2.723e+01
## `SectionName.nb.fctrReaders Respond::`                              4.585e-01
## SectionName.nb.fctrSports                                          -2.665e+01
## `SectionName.nb.fctrmyEducation::`                                 -5.580e-01
## `SectionName.nb.fctrmyPolitics::`                                  -4.563e+00
## SectionName.nb.fctrNational                                        -2.634e+01
## `SectionName.nb.fctrVerbatim::`                                    -2.556e+01
## `SectionName.nb.fctrFirst Draft::`                                 -2.667e+01
## `SectionName.nb.fctrmyMultimedia::`                                -2.655e+01
## `SectionName.nb.fctrToday in Politics::`                           -2.836e+01
## `SectionName.nb.fctrReporter's Notebook::`                         -1.738e+00
## SectionName.nb.fctrCulture                                         -2.653e+01
## `SectionName.nb.fctrThe Daily Gift::`                              -4.538e+00
## `Headline.pfx.fctrmyMisc::`                                        -6.872e-01
## `Headline.pfx.fctr19[0-9][0-9]::`                                  -4.504e+15
## `Headline.pfx.fctrDaily Report::`                                  -2.403e+01
## `Headline.pfx.fctr.*Fashion Week::`                                -1.865e+01
## `Headline.pfx.fctrWhat We're::`                                    -2.815e+01
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                  -1.774e+01
## `Headline.pfx.fctrToday in Small Business::`                       -2.361e+01
## `Headline.pfx.fctrDaily Clip Report::`                                     NA
## `Headline.pfx.fctrMorning Agenda::`                                -2.361e+01
## `Headline.pfx.fctrNew York Today::`                                -2.323e+00
## `Headline.pfx.fctr6 Q's About the News::`                           5.145e-01
## `Headline.pfx.fctrTest Yourself::`                                  4.312e+00
## `Headline.pfx.fctrWord of the Day::`                                1.439e+00
## `Headline.pfx.fctrmyMultimedia::`                                   9.789e-01
## `Headline.pfx.fctrmyTech::`                                        -2.404e-01
## `Headline.pfx.fctrmyPolitics::`                                     5.529e-01
## `Headline.pfx.fctrmyFood::`                                        -1.446e+00
## `Headline.pfx.fctrYour Turn::`                                      3.682e+00
## `Headline.pfx.fctrReaders Respond::`                               -1.709e+00
## `Headline.pfx.fctrAsk Well::`                                      -1.395e+00
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           2.792e+01
## `Headline.pfx.fctrOn This Day::`                                    2.352e+00
## `Headline.pfx.fctrVerbatim::`                                              NA
## `Headline.pfx.fctrFirst Draft::`                                           NA
## `Headline.pfx.fctrToday in Politics::`                                     NA
## `Headline.pfx.fctrReporter's Notebook::`                                   NA
## `Headline.pfx.fctrThe Daily Gift::`                                -2.056e+01
## WordCount.log                                                       1.233e+00
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      1.610e+00
## `SubsectionName.nb.fctrScience::Health`                                    NA
## `SubsectionName.nb.fctrOpEd::Opinion`                               5.576e-01
## `SubsectionName.nb.fctrRoom For Debate`                            -8.131e+00
## `SubsectionName.nb.fctrForeign::World`                             -2.180e+01
## `SubsectionName.nb.fctrFashion & Style`                            -3.893e+13
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrAsia Pacific`                                       NA
## `SubsectionName.nb.fctrBusiness::Technology`                       -3.893e+13
## `SubsectionName.nb.fctrMagazine::Magazine`                                 NA
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                           NA
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                   NA
## `SubsectionName.nb.fctrTravel::Travel`                              3.021e+13
## SubsectionName.nb.fctrEducation                                    -3.893e+13
## `SubsectionName.nb.fctrThe Public Editor`                                  NA
## `SubsectionName.nb.fctrSmall Business`                                     NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                               NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`             NA
## `SubsectionName.nb.fctrmyMisc::Open`                                       NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                     -3.893e+13
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                         -3.893e+13
## `SubsectionName.nb.fctrmyMisc::U.S.`                               -3.893e+13
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                              3.021e+13
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## H.today                                                            -1.118e+00
## S.can                                                              -3.676e+01
## A.can                                                               3.632e+01
## S.fashion                                                          -7.797e-01
## S.time                                                              2.549e+01
## A.time                                                             -2.570e+01
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrScience                                                    NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrStyles                                                     NA
## NewsDesk.nb.fctrTStyle                                                     NA
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrTravel                                                     NA
## NewsDesk.nb.fctrmyEducation                                                NA
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## A.num.words.log                                                    -1.181e+02
## S.num.words.log                                                     1.171e+02
## S.num.words.unq.log                                                -1.185e+02
## A.num.words.unq.log                                                 1.181e+02
## S.said                                                              6.931e-01
## A.num.chars.log                                                    -1.075e+01
## S.num.chars.log                                                     1.113e+01
## S.share                                                            -1.071e+00
## S.articl                                                            1.661e-01
## `PubDate.second.fctr(14.8,29.5]`                                   -1.041e-01
## `PubDate.second.fctr(29.5,44.2]`                                    6.003e-03
## `PubDate.second.fctr(44.2,59.1]`                                   -1.749e-01
## S.year                                                             -4.185e-01
## S.compani                                                          -3.038e-01
## S.week                                                             -3.534e-01
## H.has.ebola                                                        -4.698e-01
## H.new                                                              -5.203e-01
## `PubDate.minute.fctr(14.8,29.5]`                                   -1.566e-02
## `PubDate.minute.fctr(29.5,44.2]`                                   -2.969e-01
## `PubDate.minute.fctr(44.2,59.1]`                                   -1.982e-02
## H.report                                                           -6.322e-01
## H.X2014                                                            -9.451e-01
## H.num.words.log                                                     1.674e+00
## S.report                                                           -2.419e-01
## A.take                                                              2.644e+01
## S.take                                                             -2.643e+01
## A.one                                                              -8.763e+00
## H.num.words.unq.log                                                -2.054e+00
## H.is.question                                                       1.306e+00
## S.one                                                               8.792e+00
## A.new                                                              -1.771e+01
## S.new                                                               1.766e+01
## S.show                                                             -2.156e-01
## S.first                                                            -1.930e-01
## PubDate.wkday.fctr1                                                -8.399e-01
## PubDate.wkday.fctr2                                                -1.189e+00
## PubDate.wkday.fctr3                                                -1.020e+00
## PubDate.wkday.fctr4                                                -1.089e+00
## PubDate.wkday.fctr5                                                -1.242e+00
## PubDate.wkday.fctr6                                                -7.662e-01
## `PubDate.date.fctr(7,13]`                                           1.198e-01
## `PubDate.date.fctr(13,19]`                                         -4.797e-02
## `PubDate.date.fctr(19,25]`                                          2.080e-02
## `PubDate.date.fctr(25,31]`                                          8.927e-02
## A.day                                                               1.459e+00
## S.day                                                              -1.271e+00
## H.day                                                              -2.051e-01
## S.intern                                                            1.811e-01
## S.state                                                             5.085e-01
## S.presid                                                           -5.715e-02
## `PubDate.hour.fctr(7.67,15.3]`                                     -1.969e-01
## `PubDate.hour.fctr(15.3,23]`                                       -1.785e-02
## H.newyork                                                          -1.118e-01
## H.week                                                             -3.155e-01
## H.num.chars.log                                                    -2.404e-01
## H.fashion                                                           9.093e-01
## S.newyork                                                           3.422e-01
## S.make                                                             -1.231e-01
## A.will                                                             -1.020e+00
## PubDate.wkend                                                      -2.347e-01
## .rnorm                                                             -2.889e-02
## S.will                                                              8.004e-01
##                                                                    Std. Error
## (Intercept)                                                         1.276e+00
## SectionName.nb.fctrArts                                             3.655e-01
## `SectionName.nb.fctrBusiness Day`                                   5.884e-01
## SectionName.nb.fctrHealth                                           3.652e-01
## SectionName.nb.fctrOpinion                                          7.061e-01
## SectionName.nb.fctrWorld                                            6.883e-01
## SectionName.nb.fctrStyles                                           4.216e+13
## SectionName.nb.fctrTStyle                                           4.908e-01
## SectionName.nb.fctrTechnology                                       4.216e+13
## SectionName.nb.fctrMagazine                                         4.844e+04
## SectionName.nb.fctrMultimedia                                       7.945e-01
## `SectionName.nb.fctrmyMisc::`                                       3.489e-01
## SectionName.nb.fctrTravel                                           1.141e+15
## SectionName.nb.fctrU.S.                                             4.216e+13
## `SectionName.nb.fctrN.Y. / Region`                                  4.490e-01
## `SectionName.nb.fctrDaily Clip Report::`                            4.004e+04
## SectionName.nb.fctrOpen                                             1.477e+05
## `SectionName.nb.fctrReaders Respond::`                              1.200e+00
## SectionName.nb.fctrSports                                           2.191e+05
## `SectionName.nb.fctrmyEducation::`                                  1.354e+00
## `SectionName.nb.fctrmyPolitics::`                                   8.295e-01
## SectionName.nb.fctrNational                                         2.156e+05
## `SectionName.nb.fctrVerbatim::`                                     5.004e+04
## `SectionName.nb.fctrFirst Draft::`                                  3.516e+04
## `SectionName.nb.fctrmyMultimedia::`                                 1.378e+05
## `SectionName.nb.fctrToday in Politics::`                            4.687e+04
## `SectionName.nb.fctrReporter's Notebook::`                          1.188e+00
## SectionName.nb.fctrCulture                                          3.145e+05
## `SectionName.nb.fctrThe Daily Gift::`                               3.257e+05
## `Headline.pfx.fctrmyMisc::`                                         5.201e-01
## `Headline.pfx.fctr19[0-9][0-9]::`                                   5.488e+06
## `Headline.pfx.fctrDaily Report::`                                   6.998e+04
## `Headline.pfx.fctr.*Fashion Week::`                                 5.333e+03
## `Headline.pfx.fctrWhat We're::`                                     4.944e+04
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                   3.416e+04
## `Headline.pfx.fctrToday in Small Business::`                        5.696e+04
## `Headline.pfx.fctrDaily Clip Report::`                                     NA
## `Headline.pfx.fctrMorning Agenda::`                                 2.287e+04
## `Headline.pfx.fctrNew York Today::`                                 1.729e+00
## `Headline.pfx.fctr6 Q's About the News::`                           7.829e+04
## `Headline.pfx.fctrTest Yourself::`                                  3.563e+04
## `Headline.pfx.fctrWord of the Day::`                                5.148e+04
## `Headline.pfx.fctrmyMultimedia::`                                   9.599e-01
## `Headline.pfx.fctrmyTech::`                                         5.992e-01
## `Headline.pfx.fctrmyPolitics::`                                     7.822e-01
## `Headline.pfx.fctrmyFood::`                                         6.695e-01
## `Headline.pfx.fctrYour Turn::`                                      1.125e+00
## `Headline.pfx.fctrReaders Respond::`                                1.120e+00
## `Headline.pfx.fctrAsk Well::`                                       8.502e-01
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           2.310e+04
## `Headline.pfx.fctrOn This Day::`                                    5.997e+04
## `Headline.pfx.fctrVerbatim::`                                              NA
## `Headline.pfx.fctrFirst Draft::`                                           NA
## `Headline.pfx.fctrToday in Politics::`                                     NA
## `Headline.pfx.fctrReporter's Notebook::`                                   NA
## `Headline.pfx.fctrThe Daily Gift::`                                 7.020e+04
## WordCount.log                                                       7.608e-02
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      5.124e-01
## `SubsectionName.nb.fctrScience::Health`                                    NA
## `SubsectionName.nb.fctrOpEd::Opinion`                               6.408e-01
## `SubsectionName.nb.fctrRoom For Debate`                             1.228e+00
## `SubsectionName.nb.fctrForeign::World`                              5.744e+04
## `SubsectionName.nb.fctrFashion & Style`                             4.216e+13
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrAsia Pacific`                                       NA
## `SubsectionName.nb.fctrBusiness::Technology`                        4.216e+13
## `SubsectionName.nb.fctrMagazine::Magazine`                                 NA
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                           NA
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                   NA
## `SubsectionName.nb.fctrTravel::Travel`                              1.141e+15
## SubsectionName.nb.fctrEducation                                     4.216e+13
## `SubsectionName.nb.fctrThe Public Editor`                                  NA
## `SubsectionName.nb.fctrSmall Business`                                     NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                               NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`             NA
## `SubsectionName.nb.fctrmyMisc::Open`                                       NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                      4.216e+13
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                          4.216e+13
## `SubsectionName.nb.fctrmyMisc::U.S.`                                4.216e+13
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                              1.141e+15
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## H.today                                                             1.432e+00
## S.can                                                               4.149e+05
## A.can                                                               4.149e+05
## S.fashion                                                           1.091e+00
## S.time                                                              3.013e+05
## A.time                                                              3.013e+05
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrScience                                                    NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrStyles                                                     NA
## NewsDesk.nb.fctrTStyle                                                     NA
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrTravel                                                     NA
## NewsDesk.nb.fctrmyEducation                                                NA
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## A.num.words.log                                                     7.858e+01
## S.num.words.log                                                     7.856e+01
## S.num.words.unq.log                                                 7.926e+01
## A.num.words.unq.log                                                 7.929e+01
## S.said                                                              2.177e-01
## A.num.chars.log                                                     1.951e+01
## S.num.chars.log                                                     1.951e+01
## S.share                                                             4.160e-01
## S.articl                                                            6.089e-01
## `PubDate.second.fctr(14.8,29.5]`                                    1.425e-01
## `PubDate.second.fctr(29.5,44.2]`                                    1.409e-01
## `PubDate.second.fctr(44.2,59.1]`                                    1.433e-01
## S.year                                                              2.509e-01
## S.compani                                                           2.216e-01
## S.week                                                              3.152e-01
## H.has.ebola                                                         4.048e-01
## H.new                                                               3.380e-01
## `PubDate.minute.fctr(14.8,29.5]`                                    1.439e-01
## `PubDate.minute.fctr(29.5,44.2]`                                    1.423e-01
## `PubDate.minute.fctr(44.2,59.1]`                                    1.488e-01
## H.report                                                            5.585e-01
## H.X2014                                                             7.915e-01
## H.num.words.log                                                     1.681e+00
## S.report                                                            3.075e-01
## A.take                                                              6.595e+05
## S.take                                                              6.595e+05
## A.one                                                               4.948e+05
## H.num.words.unq.log                                                 1.652e+00
## H.is.question                                                       1.826e-01
## S.one                                                               4.948e+05
## A.new                                                               2.089e+05
## S.new                                                               2.089e+05
## S.show                                                              3.265e-01
## S.first                                                             3.808e-01
## PubDate.wkday.fctr1                                                 3.795e-01
## PubDate.wkday.fctr2                                                 4.094e-01
## PubDate.wkday.fctr3                                                 4.106e-01
## PubDate.wkday.fctr4                                                 4.043e-01
## PubDate.wkday.fctr5                                                 4.120e-01
## PubDate.wkday.fctr6                                                 3.731e-01
## `PubDate.date.fctr(7,13]`                                           1.555e-01
## `PubDate.date.fctr(13,19]`                                          1.558e-01
## `PubDate.date.fctr(19,25]`                                          1.528e-01
## `PubDate.date.fctr(25,31]`                                          1.646e-01
## A.day                                                               3.002e+05
## S.day                                                               3.002e+05
## H.day                                                               5.051e-01
## S.intern                                                            5.097e-01
## S.state                                                             2.606e-01
## S.presid                                                            3.328e-01
## `PubDate.hour.fctr(7.67,15.3]`                                      1.717e-01
## `PubDate.hour.fctr(15.3,23]`                                        1.798e-01
## H.newyork                                                           4.910e-01
## H.week                                                              5.594e-01
## H.num.chars.log                                                     2.982e-01
## H.fashion                                                           1.233e+00
## S.newyork                                                           3.046e-01
## S.make                                                              2.307e-01
## A.will                                                              3.392e+05
## PubDate.wkend                                                       3.338e-01
## .rnorm                                                              5.068e-02
## S.will                                                              3.392e+05
##                                                                       z value
## (Intercept)                                                        -1.427e+00
## SectionName.nb.fctrArts                                            -8.800e+00
## `SectionName.nb.fctrBusiness Day`                                  -7.576e+00
## SectionName.nb.fctrHealth                                          -1.830e-01
## SectionName.nb.fctrOpinion                                         -1.040e-01
## SectionName.nb.fctrWorld                                           -7.498e+00
## SectionName.nb.fctrStyles                                           9.230e-01
## SectionName.nb.fctrTStyle                                          -9.536e+00
## SectionName.nb.fctrTechnology                                       9.230e-01
## SectionName.nb.fctrMagazine                                        -1.000e-03
## SectionName.nb.fctrMultimedia                                      -6.260e+00
## `SectionName.nb.fctrmyMisc::`                                      -8.071e+00
## SectionName.nb.fctrTravel                                          -2.600e-02
## SectionName.nb.fctrU.S.                                             9.230e-01
## `SectionName.nb.fctrN.Y. / Region`                                 -4.385e+00
## `SectionName.nb.fctrDaily Clip Report::`                           -1.000e-03
## SectionName.nb.fctrOpen                                             0.000e+00
## `SectionName.nb.fctrReaders Respond::`                              3.820e-01
## SectionName.nb.fctrSports                                           0.000e+00
## `SectionName.nb.fctrmyEducation::`                                 -4.120e-01
## `SectionName.nb.fctrmyPolitics::`                                  -5.501e+00
## SectionName.nb.fctrNational                                         0.000e+00
## `SectionName.nb.fctrVerbatim::`                                    -1.000e-03
## `SectionName.nb.fctrFirst Draft::`                                 -1.000e-03
## `SectionName.nb.fctrmyMultimedia::`                                 0.000e+00
## `SectionName.nb.fctrToday in Politics::`                           -1.000e-03
## `SectionName.nb.fctrReporter's Notebook::`                         -1.463e+00
## SectionName.nb.fctrCulture                                          0.000e+00
## `SectionName.nb.fctrThe Daily Gift::`                               0.000e+00
## `Headline.pfx.fctrmyMisc::`                                        -1.321e+00
## `Headline.pfx.fctr19[0-9][0-9]::`                                  -8.207e+08
## `Headline.pfx.fctrDaily Report::`                                   0.000e+00
## `Headline.pfx.fctr.*Fashion Week::`                                -3.000e-03
## `Headline.pfx.fctrWhat We're::`                                    -1.000e-03
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                  -1.000e-03
## `Headline.pfx.fctrToday in Small Business::`                        0.000e+00
## `Headline.pfx.fctrDaily Clip Report::`                                     NA
## `Headline.pfx.fctrMorning Agenda::`                                -1.000e-03
## `Headline.pfx.fctrNew York Today::`                                -1.344e+00
## `Headline.pfx.fctr6 Q's About the News::`                           0.000e+00
## `Headline.pfx.fctrTest Yourself::`                                  0.000e+00
## `Headline.pfx.fctrWord of the Day::`                                0.000e+00
## `Headline.pfx.fctrmyMultimedia::`                                   1.020e+00
## `Headline.pfx.fctrmyTech::`                                        -4.010e-01
## `Headline.pfx.fctrmyPolitics::`                                     7.070e-01
## `Headline.pfx.fctrmyFood::`                                        -2.159e+00
## `Headline.pfx.fctrYour Turn::`                                      3.272e+00
## `Headline.pfx.fctrReaders Respond::`                               -1.526e+00
## `Headline.pfx.fctrAsk Well::`                                      -1.641e+00
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           1.000e-03
## `Headline.pfx.fctrOn This Day::`                                    0.000e+00
## `Headline.pfx.fctrVerbatim::`                                              NA
## `Headline.pfx.fctrFirst Draft::`                                           NA
## `Headline.pfx.fctrToday in Politics::`                                     NA
## `Headline.pfx.fctrReporter's Notebook::`                                   NA
## `Headline.pfx.fctrThe Daily Gift::`                                 0.000e+00
## WordCount.log                                                       1.620e+01
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      3.143e+00
## `SubsectionName.nb.fctrScience::Health`                                    NA
## `SubsectionName.nb.fctrOpEd::Opinion`                               8.700e-01
## `SubsectionName.nb.fctrRoom For Debate`                            -6.622e+00
## `SubsectionName.nb.fctrForeign::World`                              0.000e+00
## `SubsectionName.nb.fctrFashion & Style`                            -9.230e-01
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrAsia Pacific`                                       NA
## `SubsectionName.nb.fctrBusiness::Technology`                       -9.230e-01
## `SubsectionName.nb.fctrMagazine::Magazine`                                 NA
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                           NA
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                   NA
## `SubsectionName.nb.fctrTravel::Travel`                              2.600e-02
## SubsectionName.nb.fctrEducation                                    -9.230e-01
## `SubsectionName.nb.fctrThe Public Editor`                                  NA
## `SubsectionName.nb.fctrSmall Business`                                     NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                               NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`             NA
## `SubsectionName.nb.fctrmyMisc::Open`                                       NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                     -9.230e-01
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                         -9.230e-01
## `SubsectionName.nb.fctrmyMisc::U.S.`                               -9.230e-01
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                              2.600e-02
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## H.today                                                            -7.810e-01
## S.can                                                               0.000e+00
## A.can                                                               0.000e+00
## S.fashion                                                          -7.150e-01
## S.time                                                              0.000e+00
## A.time                                                              0.000e+00
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrScience                                                    NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrStyles                                                     NA
## NewsDesk.nb.fctrTStyle                                                     NA
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrTravel                                                     NA
## NewsDesk.nb.fctrmyEducation                                                NA
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## A.num.words.log                                                    -1.502e+00
## S.num.words.log                                                     1.491e+00
## S.num.words.unq.log                                                -1.496e+00
## A.num.words.unq.log                                                 1.490e+00
## S.said                                                              3.183e+00
## A.num.chars.log                                                    -5.510e-01
## S.num.chars.log                                                     5.700e-01
## S.share                                                            -2.573e+00
## S.articl                                                            2.730e-01
## `PubDate.second.fctr(14.8,29.5]`                                   -7.300e-01
## `PubDate.second.fctr(29.5,44.2]`                                    4.300e-02
## `PubDate.second.fctr(44.2,59.1]`                                   -1.221e+00
## S.year                                                             -1.668e+00
## S.compani                                                          -1.371e+00
## S.week                                                             -1.121e+00
## H.has.ebola                                                        -1.161e+00
## H.new                                                              -1.539e+00
## `PubDate.minute.fctr(14.8,29.5]`                                   -1.090e-01
## `PubDate.minute.fctr(29.5,44.2]`                                   -2.087e+00
## `PubDate.minute.fctr(44.2,59.1]`                                   -1.330e-01
## H.report                                                           -1.132e+00
## H.X2014                                                            -1.194e+00
## H.num.words.log                                                     9.960e-01
## S.report                                                           -7.870e-01
## A.take                                                              0.000e+00
## S.take                                                              0.000e+00
## A.one                                                               0.000e+00
## H.num.words.unq.log                                                -1.243e+00
## H.is.question                                                       7.154e+00
## S.one                                                               0.000e+00
## A.new                                                               0.000e+00
## S.new                                                               0.000e+00
## S.show                                                             -6.600e-01
## S.first                                                            -5.070e-01
## PubDate.wkday.fctr1                                                -2.213e+00
## PubDate.wkday.fctr2                                                -2.904e+00
## PubDate.wkday.fctr3                                                -2.484e+00
## PubDate.wkday.fctr4                                                -2.692e+00
## PubDate.wkday.fctr5                                                -3.014e+00
## PubDate.wkday.fctr6                                                -2.054e+00
## `PubDate.date.fctr(7,13]`                                           7.700e-01
## `PubDate.date.fctr(13,19]`                                         -3.080e-01
## `PubDate.date.fctr(19,25]`                                          1.360e-01
## `PubDate.date.fctr(25,31]`                                          5.420e-01
## A.day                                                               0.000e+00
## S.day                                                               0.000e+00
## H.day                                                              -4.060e-01
## S.intern                                                            3.550e-01
## S.state                                                             1.952e+00
## S.presid                                                           -1.720e-01
## `PubDate.hour.fctr(7.67,15.3]`                                     -1.146e+00
## `PubDate.hour.fctr(15.3,23]`                                       -9.900e-02
## H.newyork                                                          -2.280e-01
## H.week                                                             -5.640e-01
## H.num.chars.log                                                    -8.060e-01
## H.fashion                                                           7.380e-01
## S.newyork                                                           1.123e+00
## S.make                                                             -5.340e-01
## A.will                                                              0.000e+00
## PubDate.wkend                                                      -7.030e-01
## .rnorm                                                             -5.700e-01
## S.will                                                              0.000e+00
##                                                                    Pr(>|z|)
## (Intercept)                                                         0.15359
## SectionName.nb.fctrArts                                             < 2e-16
## `SectionName.nb.fctrBusiness Day`                                  3.56e-14
## SectionName.nb.fctrHealth                                           0.85463
## SectionName.nb.fctrOpinion                                          0.91719
## SectionName.nb.fctrWorld                                           6.47e-14
## SectionName.nb.fctrStyles                                           0.35579
## SectionName.nb.fctrTStyle                                           < 2e-16
## SectionName.nb.fctrTechnology                                       0.35579
## SectionName.nb.fctrMagazine                                         0.99955
## SectionName.nb.fctrMultimedia                                      3.84e-10
## `SectionName.nb.fctrmyMisc::`                                      6.99e-16
## SectionName.nb.fctrTravel                                           0.97888
## SectionName.nb.fctrU.S.                                             0.35579
## `SectionName.nb.fctrN.Y. / Region`                                 1.16e-05
## `SectionName.nb.fctrDaily Clip Report::`                            0.99948
## SectionName.nb.fctrOpen                                             0.99985
## `SectionName.nb.fctrReaders Respond::`                              0.70251
## SectionName.nb.fctrSports                                           0.99990
## `SectionName.nb.fctrmyEducation::`                                  0.68019
## `SectionName.nb.fctrmyPolitics::`                                  3.78e-08
## SectionName.nb.fctrNational                                         0.99990
## `SectionName.nb.fctrVerbatim::`                                     0.99959
## `SectionName.nb.fctrFirst Draft::`                                  0.99939
## `SectionName.nb.fctrmyMultimedia::`                                 0.99985
## `SectionName.nb.fctrToday in Politics::`                            0.99952
## `SectionName.nb.fctrReporter's Notebook::`                          0.14346
## SectionName.nb.fctrCulture                                          0.99993
## `SectionName.nb.fctrThe Daily Gift::`                               0.99999
## `Headline.pfx.fctrmyMisc::`                                         0.18638
## `Headline.pfx.fctr19[0-9][0-9]::`                                   < 2e-16
## `Headline.pfx.fctrDaily Report::`                                   0.99973
## `Headline.pfx.fctr.*Fashion Week::`                                 0.99721
## `Headline.pfx.fctrWhat We're::`                                     0.99955
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                   0.99959
## `Headline.pfx.fctrToday in Small Business::`                        0.99967
## `Headline.pfx.fctrDaily Clip Report::`                                   NA
## `Headline.pfx.fctrMorning Agenda::`                                 0.99918
## `Headline.pfx.fctrNew York Today::`                                 0.17905
## `Headline.pfx.fctr6 Q's About the News::`                           0.99999
## `Headline.pfx.fctrTest Yourself::`                                  0.99990
## `Headline.pfx.fctrWord of the Day::`                                0.99998
## `Headline.pfx.fctrmyMultimedia::`                                   0.30781
## `Headline.pfx.fctrmyTech::`                                         0.68833
## `Headline.pfx.fctrmyPolitics::`                                     0.47963
## `Headline.pfx.fctrmyFood::`                                         0.03081
## `Headline.pfx.fctrYour Turn::`                                      0.00107
## `Headline.pfx.fctrReaders Respond::`                                0.12702
## `Headline.pfx.fctrAsk Well::`                                       0.10072
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           0.99904
## `Headline.pfx.fctrOn This Day::`                                    0.99997
## `Headline.pfx.fctrVerbatim::`                                            NA
## `Headline.pfx.fctrFirst Draft::`                                         NA
## `Headline.pfx.fctrToday in Politics::`                                   NA
## `Headline.pfx.fctrReporter's Notebook::`                                 NA
## `Headline.pfx.fctrThe Daily Gift::`                                 0.99977
## WordCount.log                                                       < 2e-16
## `SubsectionName.nb.fctrCulture::Arts`                                    NA
## SubsectionName.nb.fctrDealbook                                      0.00167
## `SubsectionName.nb.fctrScience::Health`                                  NA
## `SubsectionName.nb.fctrOpEd::Opinion`                               0.38422
## `SubsectionName.nb.fctrRoom For Debate`                            3.54e-11
## `SubsectionName.nb.fctrForeign::World`                              0.99970
## `SubsectionName.nb.fctrFashion & Style`                             0.35579
## `SubsectionName.nb.fctrTStyle::TStyle`                                   NA
## `SubsectionName.nb.fctrAsia Pacific`                                     NA
## `SubsectionName.nb.fctrBusiness::Technology`                        0.35579
## `SubsectionName.nb.fctrMagazine::Magazine`                               NA
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                         NA
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                 NA
## `SubsectionName.nb.fctrTravel::Travel`                              0.97888
## SubsectionName.nb.fctrEducation                                     0.35579
## `SubsectionName.nb.fctrThe Public Editor`                                NA
## `SubsectionName.nb.fctrSmall Business`                                   NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                             NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`           NA
## `SubsectionName.nb.fctrmyMisc::Open`                                     NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`               NA
## SubsectionName.nb.fctrPolitics                                      0.35579
## `SubsectionName.nb.fctrSports::Sports`                                   NA
## `SubsectionName.nb.fctrmyEducation::myEducation::`                       NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                         NA
## `SubsectionName.nb.fctrNational::National`                               NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                             NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                       NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                     NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`           NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`       NA
## `SubsectionName.nb.fctrCulture::Culture`                                 NA
## `SubsectionName.nb.fctrTStyle::Technology`                          0.35579
## `SubsectionName.nb.fctrmyMisc::U.S.`                                0.35579
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                 NA
## `SubsectionName.nb.fctrmyMisc::Travel`                              0.97888
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                      NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                NA
## `SubsectionName.nb.fctrmyEducation::Travel`                              NA
## H.today                                                             0.43498
## S.can                                                               0.99993
## A.can                                                               0.99993
## S.fashion                                                           0.47473
## S.time                                                              0.99993
## A.time                                                              0.99993
## NewsDesk.nb.fctrCulture                                                  NA
## NewsDesk.nb.fctrScience                                                  NA
## NewsDesk.nb.fctrOpEd                                                     NA
## NewsDesk.nb.fctrForeign                                                  NA
## NewsDesk.nb.fctrStyles                                                   NA
## NewsDesk.nb.fctrTStyle                                                   NA
## NewsDesk.nb.fctrMagazine                                                 NA
## NewsDesk.nb.fctrmyMultimedia                                             NA
## `NewsDesk.nb.fctrmyMisc::`                                               NA
## NewsDesk.nb.fctrTravel                                                   NA
## NewsDesk.nb.fctrmyEducation                                              NA
## NewsDesk.nb.fctrMetro                                                    NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                    NA
## `NewsDesk.nb.fctrReaders Respond::`                                      NA
## NewsDesk.nb.fctrNational                                                 NA
## NewsDesk.nb.fctrSports                                                   NA
## `NewsDesk.nb.fctrmyEducation::`                                          NA
## `NewsDesk.nb.fctrmyPolitics::`                                           NA
## `NewsDesk.nb.fctrVerbatim::`                                             NA
## `NewsDesk.nb.fctrFirst Draft::`                                          NA
## `NewsDesk.nb.fctrmyMultimedia::`                                         NA
## `NewsDesk.nb.fctrToday in Politics::`                                    NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                  NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                       NA
## A.num.words.log                                                     0.13299
## S.num.words.log                                                     0.13596
## S.num.words.unq.log                                                 0.13476
## A.num.words.unq.log                                                 0.13629
## S.said                                                              0.00146
## A.num.chars.log                                                     0.58168
## S.num.chars.log                                                     0.56844
## S.share                                                             0.01007
## S.articl                                                            0.78500
## `PubDate.second.fctr(14.8,29.5]`                                    0.46526
## `PubDate.second.fctr(29.5,44.2]`                                    0.96602
## `PubDate.second.fctr(44.2,59.1]`                                    0.22227
## S.year                                                              0.09537
## S.compani                                                           0.17051
## S.week                                                              0.26216
## H.has.ebola                                                         0.24578
## H.new                                                               0.12377
## `PubDate.minute.fctr(14.8,29.5]`                                    0.91336
## `PubDate.minute.fctr(29.5,44.2]`                                    0.03691
## `PubDate.minute.fctr(44.2,59.1]`                                    0.89400
## H.report                                                            0.25764
## H.X2014                                                             0.23247
## H.num.words.log                                                     0.31933
## S.report                                                            0.43138
## A.take                                                              0.99997
## S.take                                                              0.99997
## A.one                                                               0.99999
## H.num.words.unq.log                                                 0.21395
## H.is.question                                                      8.41e-13
## S.one                                                               0.99999
## A.new                                                               0.99993
## S.new                                                               0.99993
## S.show                                                              0.50905
## S.first                                                             0.61235
## PubDate.wkday.fctr1                                                 0.02689
## PubDate.wkday.fctr2                                                 0.00368
## PubDate.wkday.fctr3                                                 0.01298
## PubDate.wkday.fctr4                                                 0.00710
## PubDate.wkday.fctr5                                                 0.00258
## PubDate.wkday.fctr6                                                 0.04001
## `PubDate.date.fctr(7,13]`                                           0.44118
## `PubDate.date.fctr(13,19]`                                          0.75814
## `PubDate.date.fctr(19,25]`                                          0.89170
## `PubDate.date.fctr(25,31]`                                          0.58756
## A.day                                                               1.00000
## S.day                                                               1.00000
## H.day                                                               0.68468
## S.intern                                                            0.72237
## S.state                                                             0.05099
## S.presid                                                            0.86365
## `PubDate.hour.fctr(7.67,15.3]`                                      0.25163
## `PubDate.hour.fctr(15.3,23]`                                        0.92095
## H.newyork                                                           0.81989
## H.week                                                              0.57272
## H.num.chars.log                                                     0.42006
## H.fashion                                                           0.46077
## S.newyork                                                           0.26124
## S.make                                                              0.59350
## A.will                                                              1.00000
## PubDate.wkend                                                       0.48187
## .rnorm                                                              0.56861
## S.will                                                              1.00000
##                                                                       
## (Intercept)                                                           
## SectionName.nb.fctrArts                                            ***
## `SectionName.nb.fctrBusiness Day`                                  ***
## SectionName.nb.fctrHealth                                             
## SectionName.nb.fctrOpinion                                            
## SectionName.nb.fctrWorld                                           ***
## SectionName.nb.fctrStyles                                             
## SectionName.nb.fctrTStyle                                          ***
## SectionName.nb.fctrTechnology                                         
## SectionName.nb.fctrMagazine                                           
## SectionName.nb.fctrMultimedia                                      ***
## `SectionName.nb.fctrmyMisc::`                                      ***
## SectionName.nb.fctrTravel                                             
## SectionName.nb.fctrU.S.                                               
## `SectionName.nb.fctrN.Y. / Region`                                 ***
## `SectionName.nb.fctrDaily Clip Report::`                              
## SectionName.nb.fctrOpen                                               
## `SectionName.nb.fctrReaders Respond::`                                
## SectionName.nb.fctrSports                                             
## `SectionName.nb.fctrmyEducation::`                                    
## `SectionName.nb.fctrmyPolitics::`                                  ***
## SectionName.nb.fctrNational                                           
## `SectionName.nb.fctrVerbatim::`                                       
## `SectionName.nb.fctrFirst Draft::`                                    
## `SectionName.nb.fctrmyMultimedia::`                                   
## `SectionName.nb.fctrToday in Politics::`                              
## `SectionName.nb.fctrReporter's Notebook::`                            
## SectionName.nb.fctrCulture                                            
## `SectionName.nb.fctrThe Daily Gift::`                                 
## `Headline.pfx.fctrmyMisc::`                                           
## `Headline.pfx.fctr19[0-9][0-9]::`                                  ***
## `Headline.pfx.fctrDaily Report::`                                     
## `Headline.pfx.fctr.*Fashion Week::`                                   
## `Headline.pfx.fctrWhat We're::`                                       
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                     
## `Headline.pfx.fctrToday in Small Business::`                          
## `Headline.pfx.fctrDaily Clip Report::`                                
## `Headline.pfx.fctrMorning Agenda::`                                   
## `Headline.pfx.fctrNew York Today::`                                   
## `Headline.pfx.fctr6 Q's About the News::`                             
## `Headline.pfx.fctrTest Yourself::`                                    
## `Headline.pfx.fctrWord of the Day::`                                  
## `Headline.pfx.fctrmyMultimedia::`                                     
## `Headline.pfx.fctrmyTech::`                                           
## `Headline.pfx.fctrmyPolitics::`                                       
## `Headline.pfx.fctrmyFood::`                                        *  
## `Headline.pfx.fctrYour Turn::`                                     ** 
## `Headline.pfx.fctrReaders Respond::`                                  
## `Headline.pfx.fctrAsk Well::`                                         
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                             
## `Headline.pfx.fctrOn This Day::`                                      
## `Headline.pfx.fctrVerbatim::`                                         
## `Headline.pfx.fctrFirst Draft::`                                      
## `Headline.pfx.fctrToday in Politics::`                                
## `Headline.pfx.fctrReporter's Notebook::`                              
## `Headline.pfx.fctrThe Daily Gift::`                                   
## WordCount.log                                                      ***
## `SubsectionName.nb.fctrCulture::Arts`                                 
## SubsectionName.nb.fctrDealbook                                     ** 
## `SubsectionName.nb.fctrScience::Health`                               
## `SubsectionName.nb.fctrOpEd::Opinion`                                 
## `SubsectionName.nb.fctrRoom For Debate`                            ***
## `SubsectionName.nb.fctrForeign::World`                                
## `SubsectionName.nb.fctrFashion & Style`                               
## `SubsectionName.nb.fctrTStyle::TStyle`                                
## `SubsectionName.nb.fctrAsia Pacific`                                  
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
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`            
## SubsectionName.nb.fctrPolitics                                        
## `SubsectionName.nb.fctrSports::Sports`                                
## `SubsectionName.nb.fctrmyEducation::myEducation::`                    
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                      
## `SubsectionName.nb.fctrNational::National`                            
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                          
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                    
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                  
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`        
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`    
## `SubsectionName.nb.fctrCulture::Culture`                              
## `SubsectionName.nb.fctrTStyle::Technology`                            
## `SubsectionName.nb.fctrmyMisc::U.S.`                                  
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`              
## `SubsectionName.nb.fctrmyMisc::Travel`                                
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                   
## `SubsectionName.nb.fctrmyEducation::U.S.`                             
## `SubsectionName.nb.fctrmyEducation::Travel`                           
## H.today                                                               
## S.can                                                                 
## A.can                                                                 
## S.fashion                                                             
## S.time                                                                
## A.time                                                                
## NewsDesk.nb.fctrCulture                                               
## NewsDesk.nb.fctrScience                                               
## NewsDesk.nb.fctrOpEd                                                  
## NewsDesk.nb.fctrForeign                                               
## NewsDesk.nb.fctrStyles                                                
## NewsDesk.nb.fctrTStyle                                                
## NewsDesk.nb.fctrMagazine                                              
## NewsDesk.nb.fctrmyMultimedia                                          
## `NewsDesk.nb.fctrmyMisc::`                                            
## NewsDesk.nb.fctrTravel                                                
## NewsDesk.nb.fctrmyEducation                                           
## NewsDesk.nb.fctrMetro                                                 
## `NewsDesk.nb.fctrDaily Clip Report::`                                 
## `NewsDesk.nb.fctrReaders Respond::`                                   
## NewsDesk.nb.fctrNational                                              
## NewsDesk.nb.fctrSports                                                
## `NewsDesk.nb.fctrmyEducation::`                                       
## `NewsDesk.nb.fctrmyPolitics::`                                        
## `NewsDesk.nb.fctrVerbatim::`                                          
## `NewsDesk.nb.fctrFirst Draft::`                                       
## `NewsDesk.nb.fctrmyMultimedia::`                                      
## `NewsDesk.nb.fctrToday in Politics::`                                 
## `NewsDesk.nb.fctrReporter's Notebook::`                               
## `NewsDesk.nb.fctrThe Daily Gift::`                                    
## A.num.words.log                                                       
## S.num.words.log                                                       
## S.num.words.unq.log                                                   
## A.num.words.unq.log                                                   
## S.said                                                             ** 
## A.num.chars.log                                                       
## S.num.chars.log                                                       
## S.share                                                            *  
## S.articl                                                              
## `PubDate.second.fctr(14.8,29.5]`                                      
## `PubDate.second.fctr(29.5,44.2]`                                      
## `PubDate.second.fctr(44.2,59.1]`                                      
## S.year                                                             .  
## S.compani                                                             
## S.week                                                                
## H.has.ebola                                                           
## H.new                                                                 
## `PubDate.minute.fctr(14.8,29.5]`                                      
## `PubDate.minute.fctr(29.5,44.2]`                                   *  
## `PubDate.minute.fctr(44.2,59.1]`                                      
## H.report                                                              
## H.X2014                                                               
## H.num.words.log                                                       
## S.report                                                              
## A.take                                                                
## S.take                                                                
## A.one                                                                 
## H.num.words.unq.log                                                   
## H.is.question                                                      ***
## S.one                                                                 
## A.new                                                                 
## S.new                                                                 
## S.show                                                                
## S.first                                                               
## PubDate.wkday.fctr1                                                *  
## PubDate.wkday.fctr2                                                ** 
## PubDate.wkday.fctr3                                                *  
## PubDate.wkday.fctr4                                                ** 
## PubDate.wkday.fctr5                                                ** 
## PubDate.wkday.fctr6                                                *  
## `PubDate.date.fctr(7,13]`                                             
## `PubDate.date.fctr(13,19]`                                            
## `PubDate.date.fctr(19,25]`                                            
## `PubDate.date.fctr(25,31]`                                            
## A.day                                                                 
## S.day                                                                 
## H.day                                                                 
## S.intern                                                              
## S.state                                                            .  
## S.presid                                                              
## `PubDate.hour.fctr(7.67,15.3]`                                        
## `PubDate.hour.fctr(15.3,23]`                                          
## H.newyork                                                             
## H.week                                                                
## H.num.chars.log                                                       
## H.fashion                                                             
## S.newyork                                                             
## S.make                                                                
## A.will                                                                
## PubDate.wkend                                                         
## .rnorm                                                                
## S.will                                                                
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5900.1  on 6531  degrees of freedom
## Residual deviance: 2729.0  on 6400  degrees of freedom
## AIC: 2993
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

![](NYTBlogs_zoo_files/figure-html/fit.data.training_0-4.png) ![](NYTBlogs_zoo_files/figure-html/fit.data.training_0-5.png) 

```
##    threshold   f.score
## 1        0.0 0.2866885
## 2        0.1 0.6555698
## 3        0.2 0.7328125
## 4        0.3 0.7395833
## 5        0.4 0.7346748
## 6        0.5 0.7267356
## 7        0.6 0.7002096
## 8        0.7 0.6584408
## 9        0.8 0.5608994
## 10       0.9 0.3766234
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Final.glm.N
## 1            N                             5080
## 2            Y                              241
##   Popular.fctr.predict.Final.glm.Y
## 1                              359
## 2                              852
##          Prediction
## Reference    N    Y
##         N 5080  359
##         Y  241  852
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.081445e-01   6.839987e-01   9.008804e-01   9.150419e-01   8.326699e-01 
## AccuracyPValue  McnemarPValue 
##   1.936274e-69   1.783681e-06
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](NYTBlogs_zoo_files/figure-html/fit.data.training_0-6.png) 

```
##    model_id model_method
## 1 Final.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     feats
## 1 SectionName.nb.fctr, Headline.pfx.fctr, WordCount.log, SubsectionName.nb.fctr, H.today, S.can, A.can, S.fashion, S.time, A.time, NewsDesk.nb.fctr, A.num.words.log, S.num.words.log, S.num.words.unq.log, A.num.words.unq.log, S.said, A.num.chars.log, S.num.chars.log, S.share, S.articl, PubDate.second.fctr, S.year, S.compani, S.week, H.has.ebola, H.new, PubDate.minute.fctr, H.report, H.X2014, H.num.words.log, S.report, A.take, S.take, A.one, H.num.words.unq.log, H.is.question, S.one, A.new, S.new, S.show, S.first, PubDate.wkday.fctr, PubDate.date.fctr, A.day, S.day, H.day, S.intern, S.state, S.presid, PubDate.hour.fctr, H.newyork, H.week, H.num.chars.log, H.fashion, S.newyork, S.make, A.will, PubDate.wkend, .rnorm, S.will
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     38.329                12.436
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9491285                    0.3       0.7395833         0.902326
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit min.aic.fit
## 1             0.9008804             0.9150419     0.6313083    2993.049
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01049069      0.02440289
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13 fit.data.training          7          0 743.161 788.669  45.508
## 14 fit.data.training          7          1 788.669      NA      NA
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

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```r
glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
                                               entity_df=glb_trnent_df)
glb_feats_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_feats_df$importance
print(glb_feats_df)
```

```
##                                            id  importance        cor.y
## SectionName.nb.fctr       SectionName.nb.fctr 100.0000000 -0.146001417
## Headline.pfx.fctr           Headline.pfx.fctr  91.6999082 -0.088287365
## WordCount.log                   WordCount.log  79.9525891  0.265800360
## SubsectionName.nb.fctr SubsectionName.nb.fctr  64.0476445 -0.206432730
## H.today                               H.today  35.1243683 -0.063723058
## S.can                                   S.can  28.6767051  0.029999780
## A.can                                   A.can  27.2660693  0.031498867
## S.fashion                           S.fashion  23.3025352 -0.086446251
## S.time                                 S.time  22.7690587 -0.057595102
## A.time                                 A.time  22.7634736 -0.057790617
## NewsDesk.nb.fctr             NewsDesk.nb.fctr  22.4658143 -0.167013566
## A.num.words.log               A.num.words.log  22.4185359 -0.245073324
## S.num.words.log               S.num.words.log  22.3840328 -0.245354135
## S.num.words.unq.log       S.num.words.unq.log  22.2113186 -0.250796919
## A.num.words.unq.log       A.num.words.unq.log  22.2008978 -0.250601203
## S.said                                 S.said  18.5344638  0.001363226
## A.num.chars.log               A.num.chars.log  15.7829921 -0.224548821
## S.num.chars.log               S.num.chars.log  15.6489129 -0.224692967
## S.share                               S.share  15.4099777 -0.050329686
## S.articl                             S.articl  12.5328941 -0.059520554
## PubDate.second.fctr       PubDate.second.fctr  11.9463864 -0.011879458
## S.year                                 S.year  10.9503706 -0.051146178
## S.compani                           S.compani  10.6804675 -0.053012962
## S.week                                 S.week   9.3397184 -0.084814939
## H.has.ebola                       H.has.ebola   8.9088400  0.025881397
## H.new                                   H.new   8.3104425 -0.053121542
## PubDate.minute.fctr       PubDate.minute.fctr   8.3054974 -0.034073846
## H.report                             H.report   8.2381465 -0.064948102
## H.X2014                               H.X2014   7.2588049 -0.046206380
## H.num.words.log               H.num.words.log   7.2412268 -0.200686356
## S.report                             S.report   7.0492572 -0.050211524
## A.take                                 A.take   6.9507375 -0.026086108
## S.take                                 S.take   6.5593535 -0.025762398
## A.one                                   A.one   6.2299217  0.005696039
## H.num.words.unq.log       H.num.words.unq.log   6.1649222 -0.204496360
## H.is.question                   H.is.question   6.1145880  0.129154799
## S.one                                   S.one   6.0941052  0.006342094
## A.new                                   A.new   6.0862030 -0.035359447
## S.new                                   S.new   5.8675807 -0.034948520
## S.show                                 S.show   5.8145248 -0.048801740
## S.first                               S.first   5.3096739 -0.053388178
## PubDate.wkday.fctr         PubDate.wkday.fctr   5.2118890 -0.039801288
## PubDate.date.fctr           PubDate.date.fctr   5.1855444 -0.011647558
## A.day                                   A.day   4.6929407 -0.045909684
## S.day                                   S.day   4.3480365 -0.045649185
## H.day                                   H.day   3.9943648 -0.061669687
## S.intern                             S.intern   3.9207350 -0.068485701
## S.state                               S.state   3.8445505  0.006069626
## S.presid                             S.presid   2.1376240 -0.019828826
## PubDate.hour.fctr           PubDate.hour.fctr   2.1220468  0.135436805
## H.newyork                           H.newyork   2.0392217 -0.057970095
## H.week                                 H.week   1.5733310 -0.075105216
## H.num.chars.log               H.num.chars.log   1.5687626 -0.171062360
## H.fashion                           H.fashion   1.2824711 -0.081708612
## S.newyork                           S.newyork   1.2314191 -0.062117105
## S.make                                 S.make   1.0956959  0.023138853
## A.will                                 A.will   0.5590347 -0.061025004
## PubDate.wkend                   PubDate.wkend   0.3495243  0.106728760
## .rnorm                                 .rnorm   0.3099109 -0.008703337
## S.will                                 S.will   0.1445346 -0.060575493
## A.articl                             A.articl          NA -0.059520554
## A.compani                           A.compani          NA -0.053099633
## A.fashion                           A.fashion          NA -0.086446251
## A.first                               A.first          NA -0.053388178
## A.has.http                         A.has.http          NA -0.013592603
## A.intern                             A.intern          NA -0.068485701
## A.make                                 A.make          NA  0.023138853
## A.newyork                           A.newyork          NA -0.062117105
## A.num.chars                       A.num.chars          NA -0.177037425
## A.num.words                       A.num.words          NA -0.204211072
## A.num.words.unq               A.num.words.unq          NA -0.210242145
## A.presid                             A.presid          NA -0.019828826
## A.report                             A.report          NA -0.050211524
## A.said                                 A.said          NA  0.001363226
## A.share                               A.share          NA -0.050329686
## A.show                                 A.show          NA -0.048801740
## A.state                               A.state          NA  0.005702163
## A.week                                 A.week          NA -0.084814939
## A.year                                 A.year          NA -0.051146178
## H.daili                               H.daili          NA -0.069192975
## H.has.http                         H.has.http          NA           NA
## H.num.chars                       H.num.chars          NA -0.147211183
## H.num.words                       H.num.words          NA -0.186036895
## H.num.words.unq               H.num.words.unq          NA -0.189702157
## H.X2015                               H.X2015          NA -0.066584892
## Popular                               Popular          NA  1.000000000
## Popular.fctr                     Popular.fctr          NA           NA
## PubDate.month.fctr         PubDate.month.fctr          NA  0.019148739
## PubDate.year.fctr           PubDate.year.fctr          NA           NA
## S.has.http                         S.has.http          NA           NA
## S.num.chars                       S.num.chars          NA -0.179331806
## S.num.words                       S.num.words          NA -0.206385049
## S.num.words.unq               S.num.words.unq          NA -0.212102717
## UniqueID                             UniqueID          NA  0.011824920
## WordCount                           WordCount          NA  0.257526549
##                        exclude.as.feat   cor.y.abs          cor.high.X
## SectionName.nb.fctr              FALSE 0.146001417                <NA>
## Headline.pfx.fctr                FALSE 0.088287365                <NA>
## WordCount.log                    FALSE 0.265800360                <NA>
## SubsectionName.nb.fctr           FALSE 0.206432730    NewsDesk.nb.fctr
## H.today                          FALSE 0.063723058                <NA>
## S.can                            FALSE 0.029999780                <NA>
## A.can                            FALSE 0.031498867               S.can
## S.fashion                        FALSE 0.086446251                <NA>
## S.time                           FALSE 0.057595102                <NA>
## A.time                           FALSE 0.057790617              S.time
## NewsDesk.nb.fctr                 FALSE 0.167013566 SectionName.nb.fctr
## A.num.words.log                  FALSE 0.245073324                <NA>
## S.num.words.log                  FALSE 0.245354135     A.num.words.log
## S.num.words.unq.log              FALSE 0.250796919     S.num.chars.log
## A.num.words.unq.log              FALSE 0.250601203                <NA>
## S.said                           FALSE 0.001363226                <NA>
## A.num.chars.log                  FALSE 0.224548821                <NA>
## S.num.chars.log                  FALSE 0.224692967     A.num.chars.log
## S.share                          FALSE 0.050329686                <NA>
## S.articl                         FALSE 0.059520554                <NA>
## PubDate.second.fctr              FALSE 0.011879458                <NA>
## S.year                           FALSE 0.051146178                <NA>
## S.compani                        FALSE 0.053012962                <NA>
## S.week                           FALSE 0.084814939                <NA>
## H.has.ebola                      FALSE 0.025881397                <NA>
## H.new                            FALSE 0.053121542                <NA>
## PubDate.minute.fctr              FALSE 0.034073846                <NA>
## H.report                         FALSE 0.064948102                <NA>
## H.X2014                          FALSE 0.046206380                <NA>
## H.num.words.log                  FALSE 0.200686356                <NA>
## S.report                         FALSE 0.050211524                <NA>
## A.take                           FALSE 0.026086108              S.take
## S.take                           FALSE 0.025762398                <NA>
## A.one                            FALSE 0.005696039                <NA>
## H.num.words.unq.log              FALSE 0.204496360     H.num.chars.log
## H.is.question                    FALSE 0.129154799                <NA>
## S.one                            FALSE 0.006342094                <NA>
## A.new                            FALSE 0.035359447               S.new
## S.new                            FALSE 0.034948520                <NA>
## S.show                           FALSE 0.048801740                <NA>
## S.first                          FALSE 0.053388178                <NA>
## PubDate.wkday.fctr               FALSE 0.039801288                <NA>
## PubDate.date.fctr                FALSE 0.011647558                <NA>
## A.day                            FALSE 0.045909684               S.day
## S.day                            FALSE 0.045649185                <NA>
## H.day                            FALSE 0.061669687                <NA>
## S.intern                         FALSE 0.068485701                <NA>
## S.state                          FALSE 0.006069626                <NA>
## S.presid                         FALSE 0.019828826                <NA>
## PubDate.hour.fctr                FALSE 0.135436805                <NA>
## H.newyork                        FALSE 0.057970095                <NA>
## H.week                           FALSE 0.075105216                <NA>
## H.num.chars.log                  FALSE 0.171062360                <NA>
## H.fashion                        FALSE 0.081708612              H.week
## S.newyork                        FALSE 0.062117105                <NA>
## S.make                           FALSE 0.023138853                <NA>
## A.will                           FALSE 0.061025004              S.will
## PubDate.wkend                    FALSE 0.106728760                <NA>
## .rnorm                           FALSE 0.008703337                <NA>
## S.will                           FALSE 0.060575493                <NA>
## A.articl                         FALSE 0.059520554            S.articl
## A.compani                        FALSE 0.053099633           S.compani
## A.fashion                        FALSE 0.086446251           S.fashion
## A.first                          FALSE 0.053388178             S.first
## A.has.http                       FALSE 0.013592603                <NA>
## A.intern                         FALSE 0.068485701            S.intern
## A.make                           FALSE 0.023138853              S.make
## A.newyork                        FALSE 0.062117105           S.newyork
## A.num.chars                       TRUE 0.177037425                <NA>
## A.num.words                       TRUE 0.204211072                <NA>
## A.num.words.unq                   TRUE 0.210242145                <NA>
## A.presid                         FALSE 0.019828826            S.presid
## A.report                         FALSE 0.050211524            S.report
## A.said                           FALSE 0.001363226                <NA>
## A.share                          FALSE 0.050329686             S.share
## A.show                           FALSE 0.048801740              S.show
## A.state                          FALSE 0.005702163                <NA>
## A.week                           FALSE 0.084814939              S.week
## A.year                           FALSE 0.051146178              S.year
## H.daili                          FALSE 0.069192975                <NA>
## H.has.http                       FALSE          NA                <NA>
## H.num.chars                       TRUE 0.147211183                <NA>
## H.num.words                       TRUE 0.186036895                <NA>
## H.num.words.unq                   TRUE 0.189702157                <NA>
## H.X2015                          FALSE 0.066584892                <NA>
## Popular                           TRUE 1.000000000                <NA>
## Popular.fctr                      TRUE          NA                <NA>
## PubDate.month.fctr                TRUE 0.019148739                <NA>
## PubDate.year.fctr                FALSE          NA                <NA>
## S.has.http                       FALSE          NA                <NA>
## S.num.chars                       TRUE 0.179331806                <NA>
## S.num.words                       TRUE 0.206385049                <NA>
## S.num.words.unq                   TRUE 0.212102717                <NA>
## UniqueID                          TRUE 0.011824920                <NA>
## WordCount                         TRUE 0.257526549                <NA>
##                        is.ConditionalX.y is.cor.y.abs.low rsp_var_raw
## SectionName.nb.fctr                 TRUE            FALSE       FALSE
## Headline.pfx.fctr                   TRUE            FALSE       FALSE
## WordCount.log                       TRUE            FALSE       FALSE
## SubsectionName.nb.fctr              TRUE            FALSE       FALSE
## H.today                             TRUE            FALSE       FALSE
## S.can                               TRUE            FALSE       FALSE
## A.can                               TRUE            FALSE       FALSE
## S.fashion                           TRUE            FALSE       FALSE
## S.time                              TRUE            FALSE       FALSE
## A.time                              TRUE            FALSE       FALSE
## NewsDesk.nb.fctr                    TRUE            FALSE       FALSE
## A.num.words.log                     TRUE            FALSE       FALSE
## S.num.words.log                     TRUE            FALSE       FALSE
## S.num.words.unq.log                 TRUE            FALSE       FALSE
## A.num.words.unq.log                 TRUE            FALSE       FALSE
## S.said                              TRUE             TRUE       FALSE
## A.num.chars.log                     TRUE            FALSE       FALSE
## S.num.chars.log                     TRUE            FALSE       FALSE
## S.share                             TRUE            FALSE       FALSE
## S.articl                            TRUE            FALSE       FALSE
## PubDate.second.fctr                 TRUE            FALSE       FALSE
## S.year                              TRUE            FALSE       FALSE
## S.compani                           TRUE            FALSE       FALSE
## S.week                              TRUE            FALSE       FALSE
## H.has.ebola                         TRUE            FALSE       FALSE
## H.new                               TRUE            FALSE       FALSE
## PubDate.minute.fctr                 TRUE            FALSE       FALSE
## H.report                            TRUE            FALSE       FALSE
## H.X2014                             TRUE            FALSE       FALSE
## H.num.words.log                     TRUE            FALSE       FALSE
## S.report                            TRUE            FALSE       FALSE
## A.take                              TRUE            FALSE       FALSE
## S.take                              TRUE            FALSE       FALSE
## A.one                               TRUE             TRUE       FALSE
## H.num.words.unq.log                 TRUE            FALSE       FALSE
## H.is.question                       TRUE            FALSE       FALSE
## S.one                               TRUE             TRUE       FALSE
## A.new                               TRUE            FALSE       FALSE
## S.new                               TRUE            FALSE       FALSE
## S.show                              TRUE            FALSE       FALSE
## S.first                             TRUE            FALSE       FALSE
## PubDate.wkday.fctr                  TRUE            FALSE       FALSE
## PubDate.date.fctr                   TRUE            FALSE       FALSE
## A.day                               TRUE            FALSE       FALSE
## S.day                               TRUE            FALSE       FALSE
## H.day                               TRUE            FALSE       FALSE
## S.intern                            TRUE            FALSE       FALSE
## S.state                             TRUE             TRUE       FALSE
## S.presid                            TRUE            FALSE       FALSE
## PubDate.hour.fctr                   TRUE            FALSE       FALSE
## H.newyork                           TRUE            FALSE       FALSE
## H.week                              TRUE            FALSE       FALSE
## H.num.chars.log                     TRUE            FALSE       FALSE
## H.fashion                           TRUE            FALSE       FALSE
## S.newyork                           TRUE            FALSE       FALSE
## S.make                              TRUE            FALSE       FALSE
## A.will                              TRUE            FALSE       FALSE
## PubDate.wkend                       TRUE            FALSE       FALSE
## .rnorm                              TRUE            FALSE       FALSE
## S.will                              TRUE            FALSE       FALSE
## A.articl                            TRUE            FALSE       FALSE
## A.compani                           TRUE            FALSE       FALSE
## A.fashion                           TRUE            FALSE       FALSE
## A.first                             TRUE            FALSE       FALSE
## A.has.http                         FALSE            FALSE       FALSE
## A.intern                            TRUE            FALSE       FALSE
## A.make                              TRUE            FALSE       FALSE
## A.newyork                           TRUE            FALSE       FALSE
## A.num.chars                           NA            FALSE       FALSE
## A.num.words                           NA            FALSE       FALSE
## A.num.words.unq                       NA            FALSE       FALSE
## A.presid                            TRUE            FALSE       FALSE
## A.report                            TRUE            FALSE       FALSE
## A.said                              TRUE             TRUE       FALSE
## A.share                             TRUE            FALSE       FALSE
## A.show                              TRUE            FALSE       FALSE
## A.state                             TRUE             TRUE       FALSE
## A.week                              TRUE            FALSE       FALSE
## A.year                              TRUE            FALSE       FALSE
## H.daili                            FALSE            FALSE       FALSE
## H.has.http                         FALSE               NA       FALSE
## H.num.chars                           NA            FALSE       FALSE
## H.num.words                           NA            FALSE       FALSE
## H.num.words.unq                       NA            FALSE       FALSE
## H.X2015                            FALSE            FALSE       FALSE
## Popular                               NA            FALSE        TRUE
## Popular.fctr                          NA               NA          NA
## PubDate.month.fctr                    NA            FALSE       FALSE
## PubDate.year.fctr                  FALSE               NA       FALSE
## S.has.http                         FALSE               NA       FALSE
## S.num.chars                           NA            FALSE       FALSE
## S.num.words                           NA            FALSE       FALSE
## S.num.words.unq                       NA            FALSE       FALSE
## UniqueID                              NA            FALSE       FALSE
## WordCount                             NA            FALSE       FALSE
##                        id_var rsp_var Conditional.X.glm.importance
## SectionName.nb.fctr        NA      NA                  100.0000000
## Headline.pfx.fctr          NA      NA                   91.6999082
## WordCount.log              NA      NA                   79.9525891
## SubsectionName.nb.fctr     NA      NA                   64.0476445
## H.today                    NA      NA                   35.1243683
## S.can                      NA      NA                   28.6767051
## A.can                      NA      NA                   27.2660693
## S.fashion                  NA      NA                   23.3025352
## S.time                     NA      NA                   22.7690587
## A.time                     NA      NA                   22.7634736
## NewsDesk.nb.fctr           NA      NA                   22.4658143
## A.num.words.log            NA      NA                   22.4185359
## S.num.words.log            NA      NA                   22.3840328
## S.num.words.unq.log        NA      NA                   22.2113186
## A.num.words.unq.log        NA      NA                   22.2008978
## S.said                     NA      NA                   18.5344638
## A.num.chars.log            NA      NA                   15.7829921
## S.num.chars.log            NA      NA                   15.6489129
## S.share                    NA      NA                   15.4099777
## S.articl                   NA      NA                   12.5328941
## PubDate.second.fctr        NA      NA                   11.9463864
## S.year                     NA      NA                   10.9503706
## S.compani                  NA      NA                   10.6804675
## S.week                     NA      NA                    9.3397184
## H.has.ebola                NA      NA                    8.9088400
## H.new                      NA      NA                    8.3104425
## PubDate.minute.fctr        NA      NA                    8.3054974
## H.report                   NA      NA                    8.2381465
## H.X2014                    NA      NA                    7.2588049
## H.num.words.log            NA      NA                    7.2412268
## S.report                   NA      NA                    7.0492572
## A.take                     NA      NA                    6.9507375
## S.take                     NA      NA                    6.5593535
## A.one                      NA      NA                    6.2299217
## H.num.words.unq.log        NA      NA                    6.1649222
## H.is.question              NA      NA                    6.1145880
## S.one                      NA      NA                    6.0941052
## A.new                      NA      NA                    6.0862030
## S.new                      NA      NA                    5.8675807
## S.show                     NA      NA                    5.8145248
## S.first                    NA      NA                    5.3096739
## PubDate.wkday.fctr         NA      NA                    5.2118890
## PubDate.date.fctr          NA      NA                    5.1855444
## A.day                      NA      NA                    4.6929407
## S.day                      NA      NA                    4.3480365
## H.day                      NA      NA                    3.9943648
## S.intern                   NA      NA                    3.9207350
## S.state                    NA      NA                    3.8445505
## S.presid                   NA      NA                    2.1376240
## PubDate.hour.fctr          NA      NA                    2.1220468
## H.newyork                  NA      NA                    2.0392217
## H.week                     NA      NA                    1.5733310
## H.num.chars.log            NA      NA                    1.5687626
## H.fashion                  NA      NA                    1.2824711
## S.newyork                  NA      NA                    1.2314191
## S.make                     NA      NA                    1.0956959
## A.will                     NA      NA                    0.5590347
## PubDate.wkend              NA      NA                    0.3495243
## .rnorm                     NA      NA                    0.3099109
## S.will                     NA      NA                    0.1445346
## A.articl                   NA      NA                           NA
## A.compani                  NA      NA                           NA
## A.fashion                  NA      NA                           NA
## A.first                    NA      NA                           NA
## A.has.http                 NA      NA                           NA
## A.intern                   NA      NA                           NA
## A.make                     NA      NA                           NA
## A.newyork                  NA      NA                           NA
## A.num.chars                NA      NA                           NA
## A.num.words                NA      NA                           NA
## A.num.words.unq            NA      NA                           NA
## A.presid                   NA      NA                           NA
## A.report                   NA      NA                           NA
## A.said                     NA      NA                           NA
## A.share                    NA      NA                           NA
## A.show                     NA      NA                           NA
## A.state                    NA      NA                           NA
## A.week                     NA      NA                           NA
## A.year                     NA      NA                           NA
## H.daili                    NA      NA                           NA
## H.has.http                 NA      NA                           NA
## H.num.chars                NA      NA                           NA
## H.num.words                NA      NA                           NA
## H.num.words.unq            NA      NA                           NA
## H.X2015                    NA      NA                           NA
## Popular                    NA      NA                           NA
## Popular.fctr               NA    TRUE                           NA
## PubDate.month.fctr         NA      NA                           NA
## PubDate.year.fctr          NA      NA                           NA
## S.has.http                 NA      NA                           NA
## S.num.chars                NA      NA                           NA
## S.num.words                NA      NA                           NA
## S.num.words.unq            NA      NA                           NA
## UniqueID                 TRUE      NA                           NA
## WordCount                  NA      NA                           NA
##                        Final.glm.importance
## SectionName.nb.fctr             100.0000000
## Headline.pfx.fctr                91.6999082
## WordCount.log                    79.9525891
## SubsectionName.nb.fctr           64.0476445
## H.today                          35.1243683
## S.can                            28.6767051
## A.can                            27.2660693
## S.fashion                        23.3025352
## S.time                           22.7690587
## A.time                           22.7634736
## NewsDesk.nb.fctr                 22.4658143
## A.num.words.log                  22.4185359
## S.num.words.log                  22.3840328
## S.num.words.unq.log              22.2113186
## A.num.words.unq.log              22.2008978
## S.said                           18.5344638
## A.num.chars.log                  15.7829921
## S.num.chars.log                  15.6489129
## S.share                          15.4099777
## S.articl                         12.5328941
## PubDate.second.fctr              11.9463864
## S.year                           10.9503706
## S.compani                        10.6804675
## S.week                            9.3397184
## H.has.ebola                       8.9088400
## H.new                             8.3104425
## PubDate.minute.fctr               8.3054974
## H.report                          8.2381465
## H.X2014                           7.2588049
## H.num.words.log                   7.2412268
## S.report                          7.0492572
## A.take                            6.9507375
## S.take                            6.5593535
## A.one                             6.2299217
## H.num.words.unq.log               6.1649222
## H.is.question                     6.1145880
## S.one                             6.0941052
## A.new                             6.0862030
## S.new                             5.8675807
## S.show                            5.8145248
## S.first                           5.3096739
## PubDate.wkday.fctr                5.2118890
## PubDate.date.fctr                 5.1855444
## A.day                             4.6929407
## S.day                             4.3480365
## H.day                             3.9943648
## S.intern                          3.9207350
## S.state                           3.8445505
## S.presid                          2.1376240
## PubDate.hour.fctr                 2.1220468
## H.newyork                         2.0392217
## H.week                            1.5733310
## H.num.chars.log                   1.5687626
## H.fashion                         1.2824711
## S.newyork                         1.2314191
## S.make                            1.0956959
## A.will                            0.5590347
## PubDate.wkend                     0.3495243
## .rnorm                            0.3099109
## S.will                            0.1445346
## A.articl                                 NA
## A.compani                                NA
## A.fashion                                NA
## A.first                                  NA
## A.has.http                               NA
## A.intern                                 NA
## A.make                                   NA
## A.newyork                                NA
## A.num.chars                              NA
## A.num.words                              NA
## A.num.words.unq                          NA
## A.presid                                 NA
## A.report                                 NA
## A.said                                   NA
## A.share                                  NA
## A.show                                   NA
## A.state                                  NA
## A.week                                   NA
## A.year                                   NA
## H.daili                                  NA
## H.has.http                               NA
## H.num.chars                              NA
## H.num.words                              NA
## H.num.words.unq                          NA
## H.X2015                                  NA
## Popular                                  NA
## Popular.fctr                             NA
## PubDate.month.fctr                       NA
## PubDate.year.fctr                        NA
## S.has.http                               NA
## S.num.chars                              NA
## S.num.words                              NA
## S.num.words.unq                          NA
## UniqueID                                 NA
## WordCount                                NA
```

```r
glb_analytics_diag_plots(obs_df=glb_trnent_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnent_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 60
```

![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-1.png) ![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-2.png) ![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-3.png) ![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-4.png) ![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-5.png) 

```
## [1] "Min/Max Boundaries: "
## [1] UniqueID                               
## [2] Popular.fctr                           
## [3] Popular.fctr.predict.Final.glm.prob    
## [4] Popular.fctr.predict.Final.glm         
## [5] Popular.fctr.predict.Final.glm.accurate
## [6] Popular.fctr.predict.Final.glm.error   
## [7] .label                                 
## <0 rows> (or 0-length row.names)
## [1] "Inaccurate: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 4721     4721            Y                         0.002902033
## 4352     4352            Y                         0.003222611
## 2182     2182            Y                         0.005548778
## 155       155            Y                         0.007638517
## 92         92            Y                         0.011002035
## 3554     3554            Y                         0.011432079
##      Popular.fctr.predict.Final.glm
## 4721                              N
## 4352                              N
## 2182                              N
## 155                               N
## 92                                N
## 3554                              N
##      Popular.fctr.predict.Final.glm.accurate
## 4721                                   FALSE
## 4352                                   FALSE
## 2182                                   FALSE
## 155                                    FALSE
## 92                                     FALSE
## 3554                                   FALSE
##      Popular.fctr.predict.Final.glm.error
## 4721                           -0.8970980
## 4352                           -0.8967774
## 2182                           -0.8944512
## 155                            -0.8923615
## 92                             -0.8889980
## 3554                           -0.8885679
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 92         92            Y                          0.01100203
## 2913     2913            Y                          0.18315846
## 2318     2318            Y                          0.22602471
## 6450     6450            Y                          0.28231981
## 3306     3306            Y                          0.61506999
## 3213     3213            Y                          0.84982247
##      Popular.fctr.predict.Final.glm
## 92                                N
## 2913                              N
## 2318                              N
## 6450                              N
## 3306                              N
## 3213                              N
##      Popular.fctr.predict.Final.glm.accurate
## 92                                     FALSE
## 2913                                   FALSE
## 2318                                   FALSE
## 6450                                   FALSE
## 3306                                   FALSE
## 3213                                   FALSE
##      Popular.fctr.predict.Final.glm.error
## 92                            -0.88899797
## 2913                          -0.71684154
## 2318                          -0.67397529
## 6450                          -0.61768019
## 3306                          -0.28493001
## 3213                          -0.05017753
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 770       770            N                           0.9740296
## 59         59            N                           0.9761618
## 1448     1448            N                           0.9844570
## 4882     4882            N                           0.9850965
## 4786     4786            N                           0.9861986
## 4975     4975            N                           0.9934650
##      Popular.fctr.predict.Final.glm
## 770                               Y
## 59                                Y
## 1448                              Y
## 4882                              Y
## 4786                              Y
## 4975                              Y
##      Popular.fctr.predict.Final.glm.accurate
## 770                                    FALSE
## 59                                     FALSE
## 1448                                   FALSE
## 4882                                   FALSE
## 4786                                   FALSE
## 4975                                   FALSE
##      Popular.fctr.predict.Final.glm.error
## 770                            0.07402960
## 59                             0.07616180
## 1448                           0.08445697
## 4882                           0.08509647
## 4786                           0.08619855
## 4975                           0.09346502
```

![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-6.png) 

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
## 92              Y                         0.011002035
## 693             Y                         0.046559886
## 4020            Y                         0.039266006
## 4721            Y                         0.002902033
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

![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-7.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          7          1 788.669 884.654  95.985
## 15  predict.data.new          8          0 884.655      NA      NA
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

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```r
glb_analytics_diag_plots(obs_df=glb_newent_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_newent_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 60
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

![](NYTBlogs_zoo_files/figure-html/predict.data.new-1.png) 

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

![](NYTBlogs_zoo_files/figure-html/predict.data.new-2.png) 

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

![](NYTBlogs_zoo_files/figure-html/predict.data.new-3.png) 

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

![](NYTBlogs_zoo_files/figure-html/predict.data.new-4.png) 

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

![](NYTBlogs_zoo_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
## [1] UniqueID                               
## [2] Popular.fctr                           
## [3] Popular.fctr.predict.Final.glm.prob    
## [4] Popular.fctr.predict.Final.glm         
## [5] Popular.fctr.predict.Final.glm.accurate
## [6] Popular.fctr.predict.Final.glm.error   
## [7] .label                                 
## <0 rows> (or 0-length row.names)
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

![](NYTBlogs_zoo_files/figure-html/predict.data.new-6.png) 

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
## [1] "glb_sel_mdl_id: Conditional.X.glm"
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
## [1] 4475  107
```

```r
print(dsp_models_df)
```

```
##                        model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 9             Conditional.X.glm        0.9042295   0.8240151     0.6533426
## 11    Conditional.X.no.rnorm.rf        0.8974234   0.9284694     0.6633970
## 8                 Low.cor.X.glm        0.8915897   0.9130877     0.6148086
## 10 Conditional.X.no.rnorm.rpart        0.8740885   0.6862731     0.4517912
## 7       Interact.High.cor.Y.glm        0.8454059   0.6748598     0.3865379
## 1             MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3          Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 5               Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 4     Max.cor.Y.cv.0.cp.0.rpart        0.7685950   0.6459615     0.2382228
## 6                 Max.cor.Y.glm        0.6917842   0.7322841     0.2210018
## 2       Random.myrandom_classfr        0.1672338   0.4821958     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 9    30454.582                    0.9
## 11          NA                    0.3
## 8     2446.224                    0.4
## 10          NA                    0.7
## 7    46534.226                    0.9
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 4           NA                    0.2
## 6     3671.246                    0.2
## 2           NA                    0.1
```

```r
print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
```

```
## [1] "Conditional.X.glm OOB confusion matrix & accuracy: "
```

```r
print(t(confusionMatrix(glb_OOBent_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                        glb_OOBent_df[, glb_rsp_var])$table))
```

```
##          Prediction
## Reference    N    Y
##         N 1618   95
##         Y  102  242
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
##              NewsDesk.nb .n.OOB .n.Tst .freqRatio.Tst .freqRatio.OOB
## 1               Business    516    500    0.267379679   0.2508507535
## 14                  OpEd    217    205    0.109625668   0.1054934370
## 10              myMisc::    265    199    0.106417112   0.1288283909
## 19                Styles     75     77    0.041176471   0.0364608653
## 17               Science     66     57    0.030481283   0.0320855615
## 2                Culture    203    243    0.129946524   0.0986874088
## 7                  Metro     57     66    0.035294118   0.0277102577
## 15     Readers Respond::      2      4    0.002139037   0.0009722897
## 16 Reporter's Notebook::      2      7    0.003743316   0.0009722897
## 5                Foreign    121    107    0.057219251   0.0588235294
## 11          myMultimedia     38     53    0.028342246   0.0184735051
## 18                Sports      1     NA             NA   0.0004861449
## 20      The Daily Gift::      1      2    0.001069519   0.0004861449
## 23                TStyle    239    107    0.057219251   0.1161886242
## 3    Daily Clip Report::     18     22    0.011764706   0.0087506077
## 4          First Draft::     18     14    0.007486631   0.0087506077
## 6               Magazine     10      3    0.001604278   0.0048614487
## 8            myEducation    118     93    0.049732620   0.0573650948
## 9          myEducation::      4      3    0.001604278   0.0019445795
## 12        myMultimedia::      2      3    0.001604278   0.0009722897
## 13          myPolitics::     25     41    0.021925134   0.0121536218
## 21   Today in Politics::     14     21    0.011229947   0.0068060282
## 22                Travel     34     31    0.016577540   0.0165289256
## 24            Verbatim::     11     12    0.006417112   0.0053475936
##    accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 1                  59               457        0.8856589
## 14                 38               179        0.8248848
## 10                 28               237        0.8943396
## 19                 25                50        0.6666667
## 17                 24                42        0.6363636
## 2                   9               194        0.9556650
## 7                   5                52        0.9122807
## 15                  2                 0        0.0000000
## 16                  2                 0        0.0000000
## 5                   1               120        0.9917355
## 11                  1                37        0.9736842
## 18                  1                 0        0.0000000
## 20                  1                 0        0.0000000
## 23                  1               238        0.9958159
## 3                   0                18        1.0000000
## 4                   0                18        1.0000000
## 6                   0                10        1.0000000
## 8                   0               118        1.0000000
## 9                   0                 4        1.0000000
## 12                  0                 2        1.0000000
## 13                  0                25        1.0000000
## 21                  0                14        1.0000000
## 22                  0                34        1.0000000
## 24                  0                11        1.0000000
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
#dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="Foreign")
#dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="Metro")
#dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="Science")
#dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="Styles")
#dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="TStyle")
#dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="myEducation")
dsp_NewsDesk.nb_conf_mtrx(NewsDesk.nb="myMultimedia")
```

```
## [1] "Conditional.X.glm OOB::NewsDesk.nb=myMultimedia confusion matrix & accuracy: "
##          Prediction
## Reference  N  Y
##         N 37  0
##         Y  1  0
## [1] 0.9736842
## [1] "Conditional.X.glm OOB::NewsDesk.nb=myMultimedia errors: "
##      Headline.pfx                              Headline Popular
## 4012     myMisc:: A Limited View of Boys From the Bronx       1
```

```r
print(nrow(glb_entity_df[sel_obs(Popular=1, NewsDesk.nb="myMisc::"),]))
```

```
## [1] 97
```

```r
dsp_obs(Popular=1, NewsDesk.nb="myMisc::", all=TRUE)
```

```
##      UniqueID Popular
## 56         56       1
## 95         95       1
## 130       130       1
## 163       163       1
## 291       291       1
## 317       317       1
## 364       364       1
## 436       436       1
## 493       493       1
## 629       629       1
## 685       685       1
## 779       779       1
## 809       809       1
## 882       882       1
## 983       983       1
## 1092     1092       1
## 1132     1132       1
## 1156     1156       1
## 1193     1193       1
## 1312     1312       1
## 1404     1404       1
## 1489     1489       1
## 1526     1526       1
## 1560     1560       1
## 1596     1596       1
## 1702     1702       1
## 1767     1767       1
## 1807     1807       1
## 1871     1871       1
## 2007     2007       1
## 2088     2088       1
## 2123     2123       1
## 2165     2165       1
## 2167     2167       1
## 2196     2196       1
## 2228     2228       1
## 2267     2267       1
## 2319     2319       1
## 2330     2330       1
## 2438     2438       1
## 2512     2512       1
## 2542     2542       1
## 2603     2603       1
## 2725     2725       1
## 2756     2756       1
## 2852     2852       1
## 2873     2873       1
## 2883     2883       1
## 3044     3044       1
## 3074     3074       1
## 3235     3235       1
## 3263     3263       1
## 3287     3287       1
## 3368     3368       1
## 3414     3414       1
## 3472     3472       1
## 3492     3492       1
## 3521     3521       1
## 3574     3574       1
## 3635     3635       1
## 3809     3809       1
## 3851     3851       1
## 3901     3901       1
## 3908     3908       1
## 4173     4173       1
## 4206     4206       1
## 4265     4265       1
## 4281     4281       1
## 4359     4359       1
## 4398     4398       1
## 4415     4415       1
## 4465     4465       1
## 4540     4540       1
## 4712     4712       1
## 4833     4833       1
## 5038     5038       1
## 5058     5058       1
## 5276     5276       1
## 5285     5285       1
## 5345     5345       1
## 5387     5387       1
## 5649     5649       1
## 5881     5881       1
## 5884     5884       1
## 5923     5923       1
## 6057     6057       1
## 6159     6159       1
## 6221     6221       1
## 6251     6251       1
## 6252     6252       1
## 6283     6283       1
## 6286     6286       1
## 6327     6327       1
## 6357     6357       1
## 6370     6370       1
## 6395     6395       1
## 6484     6484       1
##                                                                                                Headline
## 56                                                             How Can Men Help Prevent Sexual Assault?
## 95                                                                             Phrases We Love Too Much
## 130                                    How a Revelation About Hello Kittys Identity Blew Everyones Mind
## 163                                           Why Leaked Nude Photos Are Another Frontier for Feminists
## 291                                                      A Debate Fueled by Carbs (or the Lack Thereof)
## 317                                                                    What Does the Middle Class Need?
## 364                                                                      Joan Rivers and the Front Page
## 436                                                                 Should an American Man Wear Shorts?
## 493                                                                   When Family Dinner Doesnt Satisfy
## 629                                                                   When Spell-Check Can&rsquo;t Help
## 685                                                                               What Janay Rice Wants
## 779                                                                 Are Biblical Epics Epically Racist?
## 809                                                                   What Does It Mean to Be Scottish?
## 882                                                                        How Women Talk About Clothes
## 983                                                                How Keeping a Diary Can Surprise You
## 1092                                                                     How to Fake Your Next Vacation
## 1132                                                                                       Wasted Words
## 1156                                                       Tracking a Microtrend Among Affluent Parents
## 1193                                                                        What Blade Runner Got Wrong
## 1312                                                                                Boycott the N.F.L.?
## 1404                                                                           When Tips Are Not Enough
## 1489                                            What to Expect From Narendra Modi at the United Nations
## 1526                                                        Why Are We So Obsessed With Gilmore Girls? 
## 1560                                                                   What Is the Right Way to Travel?
## 1596                                               Should We Continue to Prosecute Nazi War Criminals? 
## 1702                                                                                        Do the Math
## 1767                                                     Why Asexuals Dont Want to Be Invisible Anymore
## 1807                                                           How Fear of Death Could Make You Splurge
## 1871                                    A Foreign Policy Turning Point or a Moral-Equivalence Blunder? 
## 2007                                                                 Can a Social Network Stay Ad-Free?
## 2088                                  Narendra Modi, in U.N. Speech, Inserts India Into Terrorism Fight
## 2123                                                                        Dont Do the Things You Love
## 2165                                                                              California Is Burning
## 2167                                        Netanyahu Links Hamas With ISIS, and Equates ISIS With Iran
## 2196                                                                                      Beware of Joy
## 2228                                    Why Are Republicans in Favor of Over-the-Counter Birth Control?
## 2267                                                  If You Have Unlimited Vacation, Will You Take It?
## 2319                                                                                Close but Not Quite
## 2330                                                                              Fighting Human Nature
## 2438                                                                             The Cost of Being Cool
## 2512                                                Steven Salaita and the Quagmire of Academic Freedom
## 2542                                                               Do You Have Time to Read This Story?
## 2603                                    Shamed, Flamed, Harassed: What Its Like To Be Called Fat Online
## 2725                                                       Do We Get Less Narcissistic as We Get Older?
## 2756                                                                      Is Catalonia Spains Scotland?
## 2852                                                                   When Education Brings Depression
## 2873                                                                                 Ugly Disagreements
## 2883                                                                    In the Face of Ebola, Stay Calm
## 3044                                  Joe Bidens Latest Gaffe: the Truth or an Ultimate Embarrassment? 
## 3074                                                        Inside the Bounds of a Hasidic Neighborhood
## 3235                                                           Columbus Day, or Indigenous Peoples Day?
## 3263                                                              Discussion of Pedophilia Turns Heated
## 3287                                                     Women Fight ISIS and Sexism in Kurdish Regions
## 3368                                                                                   The Slang Patrol
## 3414                              Me and David Greenglass, a Man Whose Name Is Synonymous With Betrayal
## 3472                                                              Yes Means Yes: The Big Consent Debate
## 3492                                                  Life and Death Through the Eyes of an Ebola Nurse
## 3521                                                              Why Indias Muslims Havent Radicalized
## 3574                                                                What Is a Nobel Prize Really Worth?
## 3635                                                               Who Was Right About W.M.D.s in Iraq?
## 3809                                                                    How Brain Myths Could Hurt Kids
## 3851                                               On Its 20th Anniversary, Does Pulp Fiction Hold Up? 
## 3901                                                                                   Tangled Passages
## 3908                                                             Has Ebola Exposed a Strain of Racism? 
## 4173                                                    Should a Child Offender Be Treated as an Adult?
## 4206                                       Is a Sculpture That Resembles a Sex Toy a National Scandal? 
## 4265                                                               Steering the Climate Change Coverage
## 4281                                                        What Jian Ghomeshis Accusers Were Afraid Of
## 4359                                                              Taylor Swifts Unwelcome P.R. Campaign
## 4398                              Can You Expect Comity or Conflict in a Republican-Controlled Senate? 
## 4415                                                                           Phrases We Love Too Much
## 4465                                  If David Byrne Does Not Care About Contemporary Art, Should We?  
## 4540                            1983 | Having Claimed 558 Lives, AIDS Finally Made It to the Front Page
## 4712                                                    Is It Voter Fraud or Voter Suppression in 2014?
## 4833                                                   Why Do We Still Care About the Confederate Flag?
## 5038                                                               Why Migraines Deserve More Attention
## 5058                             Sexual Harassment at Yale: Delicate Subject, High-Impact Investigation
## 5276                                                          The Benefits of Being Politically Correct
## 5285 Please. Don't 'Decry' the 'Divorc&eacute;e.' Or Give Us Your 'CV.' The Times Guide to Modern Usage
## 5345                                                                   Should Your Child Play Football?
## 5387                                                                                    Bright Passages
## 5649                                                    How to Be French (When Asking for a Cigarette) 
## 5881                                                                 Should You Pack Your Childs Lunch?
## 5884                                                                             Brooklyn, Planet Earth
## 5923                                     How OkCupid Has Become More Inclusive on Gender and Sexuality 
## 6057                                                                          When Are You Not Working?
## 6159                   Does the University of Virginia Have a Culture of Silence Around Sexual Assault?
## 6221                                                           Brown Family's Lawyer Criticizes Process
## 6251                 Ferguson and Other Cities React to Grand Jury Decision Not to Indict Darren Wilson
## 6252                                                         Was Chuck Hagel a Failure or a Scapegoat? 
## 6283                                                                    Can Brain Science Be Dangerous?
## 6286                                                                  A Quiet Wedding for Darren Wilson
## 6327                                                  What Ferguson Says About the Fear of Social Media
## 6357                                           What Big Thing Would Reinvigorate the Democratic Party? 
## 6370                          Latest Updates: Protests Nationwide as More Troops Are Called to Ferguson
## 6395                                                               What If Were Wrong About Depression?
## 6484                                                                      Does Your Job Make You Happy?
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
##      Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 92              Y                                2.220446e-16
## 693             Y                                2.220446e-16
## 4020            Y                                2.220446e-16
## 4721            Y                                2.220446e-16
##      Popular.fctr.predict.Conditional.X.glm
## 92                                        N
## 693                                       N
## 4020                                      N
## 4721                                      N
##      Popular.fctr.predict.Conditional.X.glm.accurate
## 92                                             FALSE
## 693                                            FALSE
## 4020                                           FALSE
## 4721                                           FALSE
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
##      Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 1053            Y                                1.000000e+00
## 3228            Y                                1.000000e+00
## 3437            Y                                1.000000e+00
## 3602            N                                2.220446e-16
## 4134            N                                1.000000e+00
## 4217            N                                1.000000e+00
## 4387            Y                                1.000000e+00
## 5244            N                                1.000000e+00
## 5658            Y                                1.000000e+00
##      Popular.fctr.predict.Conditional.X.glm
## 1053                                      Y
## 3228                                      Y
## 3437                                      Y
## 3602                                      N
## 4134                                      Y
## 4217                                      Y
## 4387                                      Y
## 5244                                      Y
## 5658                                      Y
##      Popular.fctr.predict.Conditional.X.glm.accurate
## 1053                                            TRUE
## 3228                                            TRUE
## 3437                                            TRUE
## 3602                                            TRUE
## 4134                                           FALSE
## 4217                                           FALSE
## 4387                                            TRUE
## 5244                                           FALSE
## 5658                                            TRUE
##      Popular.fctr Popular.fctr.predict.Final.glm.prob
## 6558         <NA>                           0.5387935
## 7535         <NA>                           0.6077794
## 7864         <NA>                           0.4438296
##      Popular.fctr.predict.Final.glm
## 6558                              N
## 7535                              N
## 7864                              N
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
##      Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 6446            Y                                           1
##      Popular.fctr.predict.Conditional.X.glm
## 6446                                      Y
##      Popular.fctr.predict.Conditional.X.glm.accurate
## 6446                                            TRUE
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
##                        is.ConditionalX.y  importance
## SectionName.nb.fctr                 TRUE 100.0000000
## Headline.pfx.fctr                   TRUE  91.6999082
## WordCount.log                       TRUE  79.9525891
## SubsectionName.nb.fctr              TRUE  64.0476445
## H.today                             TRUE  35.1243683
## S.can                               TRUE  28.6767051
## A.can                               TRUE  27.2660693
## S.fashion                           TRUE  23.3025352
## S.time                              TRUE  22.7690587
## A.time                              TRUE  22.7634736
## NewsDesk.nb.fctr                    TRUE  22.4658143
## A.num.words.log                     TRUE  22.4185359
## S.num.words.log                     TRUE  22.3840328
## S.num.words.unq.log                 TRUE  22.2113186
## A.num.words.unq.log                 TRUE  22.2008978
## S.said                              TRUE  18.5344638
## A.num.chars.log                     TRUE  15.7829921
## S.num.chars.log                     TRUE  15.6489129
## S.share                             TRUE  15.4099777
## S.articl                            TRUE  12.5328941
## PubDate.second.fctr                 TRUE  11.9463864
## S.year                              TRUE  10.9503706
## S.compani                           TRUE  10.6804675
## S.week                              TRUE   9.3397184
## H.has.ebola                         TRUE   8.9088400
## H.new                               TRUE   8.3104425
## PubDate.minute.fctr                 TRUE   8.3054974
## H.report                            TRUE   8.2381465
## H.X2014                             TRUE   7.2588049
## H.num.words.log                     TRUE   7.2412268
## S.report                            TRUE   7.0492572
## A.take                              TRUE   6.9507375
## S.take                              TRUE   6.5593535
## A.one                               TRUE   6.2299217
## H.num.words.unq.log                 TRUE   6.1649222
## H.is.question                       TRUE   6.1145880
## S.one                               TRUE   6.0941052
## A.new                               TRUE   6.0862030
## S.new                               TRUE   5.8675807
## S.show                              TRUE   5.8145248
## S.first                             TRUE   5.3096739
## PubDate.wkday.fctr                  TRUE   5.2118890
## PubDate.date.fctr                   TRUE   5.1855444
## A.day                               TRUE   4.6929407
## S.day                               TRUE   4.3480365
## H.day                               TRUE   3.9943648
## S.intern                            TRUE   3.9207350
## S.state                             TRUE   3.8445505
## S.presid                            TRUE   2.1376240
## PubDate.hour.fctr                   TRUE   2.1220468
## H.newyork                           TRUE   2.0392217
## H.week                              TRUE   1.5733310
## H.num.chars.log                     TRUE   1.5687626
## H.fashion                           TRUE   1.2824711
## S.newyork                           TRUE   1.2314191
## S.make                              TRUE   1.0956959
## A.will                              TRUE   0.5590347
## PubDate.wkend                       TRUE   0.3495243
## .rnorm                              TRUE   0.3099109
## S.will                              TRUE   0.1445346
##                        Conditional.X.glm.importance Final.glm.importance
## SectionName.nb.fctr                     100.0000000          100.0000000
## Headline.pfx.fctr                        91.6999082           91.6999082
## WordCount.log                            79.9525891           79.9525891
## SubsectionName.nb.fctr                   64.0476445           64.0476445
## H.today                                  35.1243683           35.1243683
## S.can                                    28.6767051           28.6767051
## A.can                                    27.2660693           27.2660693
## S.fashion                                23.3025352           23.3025352
## S.time                                   22.7690587           22.7690587
## A.time                                   22.7634736           22.7634736
## NewsDesk.nb.fctr                         22.4658143           22.4658143
## A.num.words.log                          22.4185359           22.4185359
## S.num.words.log                          22.3840328           22.3840328
## S.num.words.unq.log                      22.2113186           22.2113186
## A.num.words.unq.log                      22.2008978           22.2008978
## S.said                                   18.5344638           18.5344638
## A.num.chars.log                          15.7829921           15.7829921
## S.num.chars.log                          15.6489129           15.6489129
## S.share                                  15.4099777           15.4099777
## S.articl                                 12.5328941           12.5328941
## PubDate.second.fctr                      11.9463864           11.9463864
## S.year                                   10.9503706           10.9503706
## S.compani                                10.6804675           10.6804675
## S.week                                    9.3397184            9.3397184
## H.has.ebola                               8.9088400            8.9088400
## H.new                                     8.3104425            8.3104425
## PubDate.minute.fctr                       8.3054974            8.3054974
## H.report                                  8.2381465            8.2381465
## H.X2014                                   7.2588049            7.2588049
## H.num.words.log                           7.2412268            7.2412268
## S.report                                  7.0492572            7.0492572
## A.take                                    6.9507375            6.9507375
## S.take                                    6.5593535            6.5593535
## A.one                                     6.2299217            6.2299217
## H.num.words.unq.log                       6.1649222            6.1649222
## H.is.question                             6.1145880            6.1145880
## S.one                                     6.0941052            6.0941052
## A.new                                     6.0862030            6.0862030
## S.new                                     5.8675807            5.8675807
## S.show                                    5.8145248            5.8145248
## S.first                                   5.3096739            5.3096739
## PubDate.wkday.fctr                        5.2118890            5.2118890
## PubDate.date.fctr                         5.1855444            5.1855444
## A.day                                     4.6929407            4.6929407
## S.day                                     4.3480365            4.3480365
## H.day                                     3.9943648            3.9943648
## S.intern                                  3.9207350            3.9207350
## S.state                                   3.8445505            3.8445505
## S.presid                                  2.1376240            2.1376240
## PubDate.hour.fctr                         2.1220468            2.1220468
## H.newyork                                 2.0392217            2.0392217
## H.week                                    1.5733310            1.5733310
## H.num.chars.log                           1.5687626            1.5687626
## H.fashion                                 1.2824711            1.2824711
## S.newyork                                 1.2314191            1.2314191
## S.make                                    1.0956959            1.0956959
## A.will                                    0.5590347            0.5590347
## PubDate.wkend                             0.3495243            0.3495243
## .rnorm                                    0.3099109            0.3099109
## S.will                                    0.1445346            0.1445346
```

```r
print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
    c("is.ConditionalX.y", 
      grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
```

```
##           is.ConditionalX.y importance Conditional.X.glm.importance
## A.articl               TRUE         NA                           NA
## A.compani              TRUE         NA                           NA
## A.fashion              TRUE         NA                           NA
## A.first                TRUE         NA                           NA
## A.intern               TRUE         NA                           NA
## A.make                 TRUE         NA                           NA
## A.newyork              TRUE         NA                           NA
## A.presid               TRUE         NA                           NA
## A.report               TRUE         NA                           NA
## A.said                 TRUE         NA                           NA
## A.share                TRUE         NA                           NA
## A.show                 TRUE         NA                           NA
## A.state                TRUE         NA                           NA
## A.week                 TRUE         NA                           NA
## A.year                 TRUE         NA                           NA
##           Final.glm.importance
## A.articl                    NA
## A.compani                   NA
## A.fashion                   NA
## A.first                     NA
## A.intern                    NA
## A.make                      NA
## A.newyork                   NA
## A.presid                    NA
## A.report                    NA
## A.said                      NA
## A.share                     NA
## A.show                      NA
## A.state                     NA
## A.week                      NA
## A.year                      NA
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
##                   label step_major step_minor     bgn     end elapsed
## 15     predict.data.new          8          0 884.655 981.726  97.071
## 16 display.session.info          9          0 981.727      NA      NA
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
## 10              fit.models          6          1 230.353 645.448 415.096
## 15        predict.data.new          8          0 884.655 981.726  97.071
## 14       fit.data.training          7          1 788.669 884.654  95.985
## 6         extract.features          3          0  74.629 163.470  88.841
## 12              fit.models          6          3 663.782 743.161  79.379
## 9               fit.models          6          0 177.935 230.352  52.417
## 13       fit.data.training          7          0 743.161 788.669  45.508
## 3             cleanse.data          2          1  36.823  68.334  31.511
## 2             inspect.data          2          0  13.071  36.823  23.752
## 11              fit.models          6          2 645.449 663.781  18.332
## 7          select.features          4          0 163.470 174.689  11.220
## 4      manage.missing.data          2          2  68.334  74.572   6.238
## 8  partition.data.training          5          0 174.690 177.934   3.244
## 1              import.data          1          0  11.965  13.070   1.105
## 5              encode.data          2          3  74.573  74.628   0.055
##    duration
## 10  415.095
## 15   97.071
## 14   95.985
## 6    88.841
## 12   79.379
## 9    52.417
## 13   45.508
## 3    31.511
## 2    23.752
## 11   18.332
## 7    11.219
## 4     6.238
## 8     3.244
## 1     1.105
## 5     0.055
```

```
## [1] "Total Elapsed Time: 981.726 secs"
```

![](NYTBlogs_zoo_files/figure-html/display.session.info-1.png) 

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
##  [7] caret_6.0-41        mice_2.22           lattice_0.20-31    
## [10] Rcpp_0.11.5         tm_0.6              NLP_0.1-6          
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
