# NYTimes:Blogs:: Popular classification:: category
bdanalytics  

**  **    
**Date: (Thu) May 14, 2015**    

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
glb_out_pfx <- "NYTBlogs_category_"

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

![](NYTBlogs_category_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor    bgn end elapsed
## 1 import.data          1          0 11.062  NA      NA
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
## 1  import.data          1          0 11.062 12.053   0.991
## 2 inspect.data          2          0 12.053     NA      NA
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

![](NYTBlogs_category_files/figure-html/inspect.data-1.png) 

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

![](NYTBlogs_category_files/figure-html/inspect.data-2.png) 

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

![](NYTBlogs_category_files/figure-html/inspect.data-3.png) 

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

![](NYTBlogs_category_files/figure-html/inspect.data-4.png) 

```r
srt_entity_df <- orderBy(~PubDate.POSIX, glb_entity_df)
print(myplot_scatter(subset(srt_entity_df, 
                            PubDate.POSIX < strptime("2014-09-02", "%Y-%m-%d")), 
                            xcol_name="PubDate.POSIX", ycol_name=glb_rsp_var,
                           colorcol_name=glb_rsp_var
                     ))
```

![](NYTBlogs_category_files/figure-html/inspect.data-5.png) 

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

![](NYTBlogs_category_files/figure-html/inspect.data-6.png) 

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

![](NYTBlogs_category_files/figure-html/inspect.data-7.png) 

```r
last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
srt_entity_df[, "PubDate.last10"] <- last10
srt_entity_df[is.na(srt_entity_df$PubDate.last10), "PubDate.last10"] <- 0
srt_entity_df[, "PubDate.last10.log"] <- log(1 + srt_entity_df[, "PubDate.last10"])
print(gp <- myplot_box(df=subset(srt_entity_df, PubDate.last10.log > 0), 
                       ycol_names="PubDate.last10.log", 
                       xcol_name=glb_rsp_var))
```

![](NYTBlogs_category_files/figure-html/inspect.data-8.png) 

```r
last100 = as.numeric(merge(z-lag(z, -100), b, all = TRUE))
srt_entity_df[, "PubDate.last100"] <- last100
srt_entity_df[is.na(srt_entity_df$PubDate.last100), "PubDate.last100"] <- 0
srt_entity_df[, "PubDate.last100.log"] <- log(1 + srt_entity_df[, "PubDate.last100"])
print(gp <- myplot_box(df=subset(srt_entity_df, PubDate.last100.log > 0), 
                       ycol_names="PubDate.last100.log", 
                       xcol_name=glb_rsp_var))
```

![](NYTBlogs_category_files/figure-html/inspect.data-9.png) 

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

![](NYTBlogs_category_files/figure-html/inspect.data-10.png) 

```
## [1] "var: PubDate.date.fctr"
```

![](NYTBlogs_category_files/figure-html/inspect.data-11.png) 

```
## [1] "var: PubDate.wkday.fctr"
```

![](NYTBlogs_category_files/figure-html/inspect.data-12.png) 

```
## [1] "var: PubDate.wkend"
```

![](NYTBlogs_category_files/figure-html/inspect.data-13.png) 

```
## [1] "var: PubDate.hour.fctr"
```

![](NYTBlogs_category_files/figure-html/inspect.data-14.png) 

```
## [1] "var: PubDate.minute.fctr"
```

![](NYTBlogs_category_files/figure-html/inspect.data-15.png) 

```
## [1] "var: PubDate.second.fctr"
```

![](NYTBlogs_category_files/figure-html/inspect.data-16.png) 

```
## [1] "var: PubDate.last1.log"
```

![](NYTBlogs_category_files/figure-html/inspect.data-17.png) 

```
## [1] "var: PubDate.last10.log"
```

![](NYTBlogs_category_files/figure-html/inspect.data-18.png) 

```
## [1] "var: PubDate.last100.log"
```

![](NYTBlogs_category_files/figure-html/inspect.data-19.png) 

```
## [1] "var: WordCount.log"
```

![](NYTBlogs_category_files/figure-html/inspect.data-20.png) 

```
## [1] "var: .rnorm"
```

![](NYTBlogs_category_files/figure-html/inspect.data-21.png) 

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
##          label step_major step_minor    bgn   end elapsed
## 2 inspect.data          2          0 12.053 30.63  18.577
## 3 cleanse.data          2          1 30.630    NA      NA
```

### Step `2.1: cleanse data`

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

#stop("here")
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
##              myCategory NewsDesk SectionName  SubsectionName   N Y NA
## 35      Science#Health#  Science                               0 2  2
## 5       #U.S.#Education                 U.S.       Education 325 0 90
## 16        Culture#Arts#                 Arts                   0 0 11
## 13 Business#Technology#           Technology                   0 0  1
## 27              myOther National                               2 0  0
## 39      Styles##Fashion   Styles       Style Fashion & Style   2 0  0
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
## 3        cleanse.data          2          1 30.630 35.014   4.384
## 4 manage.missing.data          2          2 35.014     NA      NA
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
##  Min.   :-3.881663   Length:8402       
##  1st Qu.:-0.665043   Class :character  
##  Median :-0.004510   Mode  :character  
##  Mean   :-0.006807                     
##  3rd Qu.: 0.664125                     
##  Max.   : 3.356092                     
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
##  1st Qu.: 5.263    1st Qu.: 8.516     1st Qu.:11.37       1st Qu.:5.2679  
##  Median : 6.292    Median : 8.868     Median :11.43       Median :5.9506  
##  Mean   : 6.094    Mean   : 9.048     Mean   :11.49       Mean   :5.8273  
##  3rd Qu.: 7.126    3rd Qu.: 9.424     3rd Qu.:11.78       3rd Qu.:6.6067  
##  Max.   :10.875    Max.   :11.744     Max.   :12.95       Max.   :9.2977  
##                                                                           
##      .rnorm           myCategory       
##  Min.   :-3.881663   Length:8402       
##  1st Qu.:-0.665043   Class :character  
##  Median :-0.004510   Mode  :character  
##  Mean   :-0.006807                     
##  3rd Qu.: 0.664125                     
##  Max.   : 3.356092                     
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
## 4 manage.missing.data          2          2 35.014 39.793   4.779
## 5         encode.data          2          3 39.794     NA      NA
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
## 5      encode.data          2          3 39.794 39.853    0.06
## 6 extract.features          3          0 39.854     NA      NA
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

![](NYTBlogs_category_files/figure-html/extract.features-1.png) ![](NYTBlogs_category_files/figure-html/extract.features-2.png) ![](NYTBlogs_category_files/figure-html/extract.features-3.png) 

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

![](NYTBlogs_category_files/figure-html/extract.features-4.png) ![](NYTBlogs_category_files/figure-html/extract.features-5.png) ![](NYTBlogs_category_files/figure-html/extract.features-6.png) 

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

![](NYTBlogs_category_files/figure-html/extract.features-7.png) ![](NYTBlogs_category_files/figure-html/extract.features-8.png) ![](NYTBlogs_category_files/figure-html/extract.features-9.png) 

```
## [1] "Binding DTM for Headline..."
```

![](NYTBlogs_category_files/figure-html/extract.features-10.png) ![](NYTBlogs_category_files/figure-html/extract.features-11.png) ![](NYTBlogs_category_files/figure-html/extract.features-12.png) 

```
## [1] "Binding DTM for Snippet..."
```

![](NYTBlogs_category_files/figure-html/extract.features-13.png) ![](NYTBlogs_category_files/figure-html/extract.features-14.png) ![](NYTBlogs_category_files/figure-html/extract.features-15.png) 

```
## [1] "Binding DTM for Abstract..."
```

![](NYTBlogs_category_files/figure-html/extract.features-16.png) ![](NYTBlogs_category_files/figure-html/extract.features-17.png) ![](NYTBlogs_category_files/figure-html/extract.features-18.png) 

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

![](NYTBlogs_category_files/figure-html/extract.features-19.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 6 extract.features          3          0  39.854 131.319  91.465
## 7  select.features          4          0 131.320      NA      NA
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
##                                      id        cor.y exclude.as.feat
## Popular                         Popular  1.000000000               1
## WordCount.log             WordCount.log  0.264960434               0
## WordCount                     WordCount  0.257526549               1
## S.num.words.unq.log S.num.words.unq.log -0.250796919               0
## A.num.words.unq.log A.num.words.unq.log -0.250601203               0
## S.num.words.log         S.num.words.log -0.245354135               0
## A.num.words.log         A.num.words.log -0.245073324               0
## S.num.chars.log         S.num.chars.log -0.224692967               0
## A.num.chars.log         A.num.chars.log -0.224548821               0
## S.num.words.unq         S.num.words.unq -0.212102717               1
## A.num.words.unq         A.num.words.unq -0.210242145               1
## S.num.words                 S.num.words -0.206385049               1
## H.num.words.unq.log H.num.words.unq.log -0.204496360               0
## A.num.words                 A.num.words -0.204211072               1
## H.num.words.log         H.num.words.log -0.200686356               0
## H.num.words.unq         H.num.words.unq -0.189702157               1
## H.num.words                 H.num.words -0.186036895               1
## S.num.chars                 S.num.chars -0.179331806               1
## A.num.chars                 A.num.chars -0.177037425               1
## H.num.chars.log         H.num.chars.log -0.171062360               0
## H.num.chars                 H.num.chars -0.147211183               1
## PubDate.hour.fctr     PubDate.hour.fctr  0.135436805               0
## H.is.question             H.is.question  0.129154799               0
## PubDate.wkend             PubDate.wkend  0.106728760               0
## S.fashion                     S.fashion -0.086446251               0
## A.fashion                     A.fashion -0.086446251               0
## S.week                           S.week -0.084814939               0
## A.week                           A.week -0.084814939               0
## H.fashion                     H.fashion -0.081708612               0
## H.week                           H.week -0.075105216               0
## H.daili                         H.daili -0.069192975               0
## S.intern                       S.intern -0.068485701               0
## A.intern                       A.intern -0.068485701               0
## H.X2015                         H.X2015 -0.066584892               0
## H.report                       H.report -0.064948102               0
## H.today                         H.today -0.063723058               0
## S.newyork                     S.newyork -0.062117105               0
## A.newyork                     A.newyork -0.062117105               0
## H.day                             H.day -0.061669687               0
## A.will                           A.will -0.061025004               0
## S.will                           S.will -0.060575493               0
## S.articl                       S.articl -0.059520554               0
## A.articl                       A.articl -0.059520554               0
## H.newyork                     H.newyork -0.057970095               0
## A.time                           A.time -0.057790617               0
## S.time                           S.time -0.057595102               0
## PubDate.last10           PubDate.last10  0.053980930               1
## S.first                         S.first -0.053388178               0
## A.first                         A.first -0.053388178               0
## H.new                             H.new -0.053121542               0
## A.compani                     A.compani -0.053099633               0
## S.compani                     S.compani -0.053012962               0
## S.year                           S.year -0.051146178               0
## A.year                           A.year -0.051146178               0
## S.share                         S.share -0.050329686               0
## A.share                         A.share -0.050329686               0
## S.report                       S.report -0.050211524               0
## A.report                       A.report -0.050211524               0
## PubDate.last10.log   PubDate.last10.log  0.049317022               0
## S.show                           S.show -0.048801740               0
## A.show                           A.show -0.048801740               0
## PubDate.last1.log     PubDate.last1.log  0.046357515               0
## H.X2014                         H.X2014 -0.046206380               0
## A.day                             A.day -0.045909684               0
## S.day                             S.day -0.045649185               0
## PubDate.last100         PubDate.last100  0.039892288               1
## PubDate.wkday.fctr   PubDate.wkday.fctr -0.039801288               0
## PubDate.last1             PubDate.last1  0.035922671               1
## A.new                             A.new -0.035359447               0
## S.new                             S.new -0.034948520               0
## PubDate.minute.fctr PubDate.minute.fctr -0.034073846               0
## A.can                             A.can  0.031498867               0
## S.can                             S.can  0.029999780               0
## A.take                           A.take -0.026086108               0
## H.has.ebola                 H.has.ebola  0.025881397               0
## S.take                           S.take -0.025762398               0
## S.make                           S.make  0.023138853               0
## A.make                           A.make  0.023138853               0
## S.presid                       S.presid -0.019828826               0
## A.presid                       A.presid -0.019828826               0
## PubDate.month.fctr   PubDate.month.fctr  0.019148739               1
## .rnorm                           .rnorm  0.017561723               0
## PubDate.POSIX             PubDate.POSIX  0.015683258               1
## PubDate.zoo                 PubDate.zoo  0.015683258               1
## A.has.http                   A.has.http -0.013592603               0
## myCategory.fctr         myCategory.fctr  0.012345410               0
## PubDate.second.fctr PubDate.second.fctr -0.011879458               0
## UniqueID                       UniqueID  0.011824920               1
## PubDate.date.fctr     PubDate.date.fctr -0.011647558               0
## PubDate.last100.log PubDate.last100.log -0.007663322               0
## S.one                             S.one  0.006342094               0
## S.state                         S.state  0.006069626               0
## A.state                         A.state  0.005702163               0
## A.one                             A.one  0.005696039               0
## S.said                           S.said  0.001363226               0
## A.said                           A.said  0.001363226               0
## H.has.http                   H.has.http           NA               0
## S.has.http                   S.has.http           NA               0
## PubDate.year.fctr     PubDate.year.fctr           NA               0
##                       cor.y.abs
## Popular             1.000000000
## WordCount.log       0.264960434
## WordCount           0.257526549
## S.num.words.unq.log 0.250796919
## A.num.words.unq.log 0.250601203
## S.num.words.log     0.245354135
## A.num.words.log     0.245073324
## S.num.chars.log     0.224692967
## A.num.chars.log     0.224548821
## S.num.words.unq     0.212102717
## A.num.words.unq     0.210242145
## S.num.words         0.206385049
## H.num.words.unq.log 0.204496360
## A.num.words         0.204211072
## H.num.words.log     0.200686356
## H.num.words.unq     0.189702157
## H.num.words         0.186036895
## S.num.chars         0.179331806
## A.num.chars         0.177037425
## H.num.chars.log     0.171062360
## H.num.chars         0.147211183
## PubDate.hour.fctr   0.135436805
## H.is.question       0.129154799
## PubDate.wkend       0.106728760
## S.fashion           0.086446251
## A.fashion           0.086446251
## S.week              0.084814939
## A.week              0.084814939
## H.fashion           0.081708612
## H.week              0.075105216
## H.daili             0.069192975
## S.intern            0.068485701
## A.intern            0.068485701
## H.X2015             0.066584892
## H.report            0.064948102
## H.today             0.063723058
## S.newyork           0.062117105
## A.newyork           0.062117105
## H.day               0.061669687
## A.will              0.061025004
## S.will              0.060575493
## S.articl            0.059520554
## A.articl            0.059520554
## H.newyork           0.057970095
## A.time              0.057790617
## S.time              0.057595102
## PubDate.last10      0.053980930
## S.first             0.053388178
## A.first             0.053388178
## H.new               0.053121542
## A.compani           0.053099633
## S.compani           0.053012962
## S.year              0.051146178
## A.year              0.051146178
## S.share             0.050329686
## A.share             0.050329686
## S.report            0.050211524
## A.report            0.050211524
## PubDate.last10.log  0.049317022
## S.show              0.048801740
## A.show              0.048801740
## PubDate.last1.log   0.046357515
## H.X2014             0.046206380
## A.day               0.045909684
## S.day               0.045649185
## PubDate.last100     0.039892288
## PubDate.wkday.fctr  0.039801288
## PubDate.last1       0.035922671
## A.new               0.035359447
## S.new               0.034948520
## PubDate.minute.fctr 0.034073846
## A.can               0.031498867
## S.can               0.029999780
## A.take              0.026086108
## H.has.ebola         0.025881397
## S.take              0.025762398
## S.make              0.023138853
## A.make              0.023138853
## S.presid            0.019828826
## A.presid            0.019828826
## PubDate.month.fctr  0.019148739
## .rnorm              0.017561723
## PubDate.POSIX       0.015683258
## PubDate.zoo         0.015683258
## A.has.http          0.013592603
## myCategory.fctr     0.012345410
## PubDate.second.fctr 0.011879458
## UniqueID            0.011824920
## PubDate.date.fctr   0.011647558
## PubDate.last100.log 0.007663322
## S.one               0.006342094
## S.state             0.006069626
## A.state             0.005702163
## A.one               0.005696039
## S.said              0.001363226
## A.said              0.001363226
## H.has.http                   NA
## S.has.http                   NA
## PubDate.year.fctr            NA
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
## [1] "cor(H.fashion, H.week)=0.7616"
## [1] "cor(Popular.fctr, H.fashion)=-0.0817"
## [1] "cor(Popular.fctr, H.week)=-0.0751"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.week as highly correlated with H.fashion
```

```
##                                      id        cor.y exclude.as.feat
## Popular                         Popular  1.000000000               1
## WordCount.log             WordCount.log  0.264960434               0
## WordCount                     WordCount  0.257526549               1
## PubDate.hour.fctr     PubDate.hour.fctr  0.135436805               0
## H.is.question             H.is.question  0.129154799               0
## PubDate.wkend             PubDate.wkend  0.106728760               0
## PubDate.last10           PubDate.last10  0.053980930               1
## PubDate.last10.log   PubDate.last10.log  0.049317022               0
## PubDate.last1.log     PubDate.last1.log  0.046357515               0
## PubDate.last100         PubDate.last100  0.039892288               1
## PubDate.last1             PubDate.last1  0.035922671               1
## A.can                             A.can  0.031498867               0
## S.can                             S.can  0.029999780               0
## H.has.ebola                 H.has.ebola  0.025881397               0
## S.make                           S.make  0.023138853               0
## A.make                           A.make  0.023138853               0
## PubDate.month.fctr   PubDate.month.fctr  0.019148739               1
## .rnorm                           .rnorm  0.017561723               0
## PubDate.POSIX             PubDate.POSIX  0.015683258               1
## PubDate.zoo                 PubDate.zoo  0.015683258               1
## myCategory.fctr         myCategory.fctr  0.012345410               0
## UniqueID                       UniqueID  0.011824920               1
## S.one                             S.one  0.006342094               0
## S.state                         S.state  0.006069626               0
## A.state                         A.state  0.005702163               0
## A.one                             A.one  0.005696039               0
## S.said                           S.said  0.001363226               0
## A.said                           A.said  0.001363226               0
## PubDate.last100.log PubDate.last100.log -0.007663322               0
## PubDate.date.fctr     PubDate.date.fctr -0.011647558               0
## PubDate.second.fctr PubDate.second.fctr -0.011879458               0
## A.has.http                   A.has.http -0.013592603               0
## S.presid                       S.presid -0.019828826               0
## A.presid                       A.presid -0.019828826               0
## S.take                           S.take -0.025762398               0
## A.take                           A.take -0.026086108               0
## PubDate.minute.fctr PubDate.minute.fctr -0.034073846               0
## S.new                             S.new -0.034948520               0
## A.new                             A.new -0.035359447               0
## PubDate.wkday.fctr   PubDate.wkday.fctr -0.039801288               0
## S.day                             S.day -0.045649185               0
## A.day                             A.day -0.045909684               0
## H.X2014                         H.X2014 -0.046206380               0
## S.show                           S.show -0.048801740               0
## A.show                           A.show -0.048801740               0
## S.report                       S.report -0.050211524               0
## A.report                       A.report -0.050211524               0
## S.share                         S.share -0.050329686               0
## A.share                         A.share -0.050329686               0
## S.year                           S.year -0.051146178               0
## A.year                           A.year -0.051146178               0
## S.compani                     S.compani -0.053012962               0
## A.compani                     A.compani -0.053099633               0
## H.new                             H.new -0.053121542               0
## S.first                         S.first -0.053388178               0
## A.first                         A.first -0.053388178               0
## S.time                           S.time -0.057595102               0
## A.time                           A.time -0.057790617               0
## H.newyork                     H.newyork -0.057970095               0
## S.articl                       S.articl -0.059520554               0
## A.articl                       A.articl -0.059520554               0
## S.will                           S.will -0.060575493               0
## A.will                           A.will -0.061025004               0
## H.day                             H.day -0.061669687               0
## S.newyork                     S.newyork -0.062117105               0
## A.newyork                     A.newyork -0.062117105               0
## H.today                         H.today -0.063723058               0
## H.report                       H.report -0.064948102               0
## H.X2015                         H.X2015 -0.066584892               0
## S.intern                       S.intern -0.068485701               0
## A.intern                       A.intern -0.068485701               0
## H.daili                         H.daili -0.069192975               0
## H.week                           H.week -0.075105216               0
## H.fashion                     H.fashion -0.081708612               0
## S.week                           S.week -0.084814939               0
## A.week                           A.week -0.084814939               0
## S.fashion                     S.fashion -0.086446251               0
## A.fashion                     A.fashion -0.086446251               0
## H.num.chars                 H.num.chars -0.147211183               1
## H.num.chars.log         H.num.chars.log -0.171062360               0
## A.num.chars                 A.num.chars -0.177037425               1
## S.num.chars                 S.num.chars -0.179331806               1
## H.num.words                 H.num.words -0.186036895               1
## H.num.words.unq         H.num.words.unq -0.189702157               1
## H.num.words.log         H.num.words.log -0.200686356               0
## A.num.words                 A.num.words -0.204211072               1
## H.num.words.unq.log H.num.words.unq.log -0.204496360               0
## S.num.words                 S.num.words -0.206385049               1
## A.num.words.unq         A.num.words.unq -0.210242145               1
## S.num.words.unq         S.num.words.unq -0.212102717               1
## A.num.chars.log         A.num.chars.log -0.224548821               0
## S.num.chars.log         S.num.chars.log -0.224692967               0
## A.num.words.log         A.num.words.log -0.245073324               0
## S.num.words.log         S.num.words.log -0.245354135               0
## A.num.words.unq.log A.num.words.unq.log -0.250601203               0
## S.num.words.unq.log S.num.words.unq.log -0.250796919               0
## H.has.http                   H.has.http           NA               0
## S.has.http                   S.has.http           NA               0
## PubDate.year.fctr     PubDate.year.fctr           NA               0
##                       cor.y.abs      cor.high.X is.ConditionalX.y
## Popular             1.000000000            <NA>                NA
## WordCount.log       0.264960434            <NA>              TRUE
## WordCount           0.257526549            <NA>                NA
## PubDate.hour.fctr   0.135436805            <NA>              TRUE
## H.is.question       0.129154799            <NA>              TRUE
## PubDate.wkend       0.106728760            <NA>              TRUE
## PubDate.last10      0.053980930            <NA>                NA
## PubDate.last10.log  0.049317022            <NA>              TRUE
## PubDate.last1.log   0.046357515            <NA>              TRUE
## PubDate.last100     0.039892288            <NA>                NA
## PubDate.last1       0.035922671            <NA>                NA
## A.can               0.031498867           S.can              TRUE
## S.can               0.029999780            <NA>              TRUE
## H.has.ebola         0.025881397            <NA>              TRUE
## S.make              0.023138853            <NA>              TRUE
## A.make              0.023138853          S.make              TRUE
## PubDate.month.fctr  0.019148739            <NA>                NA
## .rnorm              0.017561723            <NA>              TRUE
## PubDate.POSIX       0.015683258            <NA>                NA
## PubDate.zoo         0.015683258            <NA>                NA
## myCategory.fctr     0.012345410            <NA>              TRUE
## UniqueID            0.011824920            <NA>                NA
## S.one               0.006342094            <NA>              TRUE
## S.state             0.006069626            <NA>              TRUE
## A.state             0.005702163            <NA>              TRUE
## A.one               0.005696039            <NA>              TRUE
## S.said              0.001363226            <NA>              TRUE
## A.said              0.001363226            <NA>              TRUE
## PubDate.last100.log 0.007663322            <NA>              TRUE
## PubDate.date.fctr   0.011647558            <NA>              TRUE
## PubDate.second.fctr 0.011879458            <NA>              TRUE
## A.has.http          0.013592603            <NA>             FALSE
## S.presid            0.019828826            <NA>              TRUE
## A.presid            0.019828826        S.presid              TRUE
## S.take              0.025762398            <NA>              TRUE
## A.take              0.026086108          S.take              TRUE
## PubDate.minute.fctr 0.034073846            <NA>              TRUE
## S.new               0.034948520            <NA>              TRUE
## A.new               0.035359447           S.new              TRUE
## PubDate.wkday.fctr  0.039801288            <NA>              TRUE
## S.day               0.045649185            <NA>              TRUE
## A.day               0.045909684           S.day              TRUE
## H.X2014             0.046206380            <NA>              TRUE
## S.show              0.048801740            <NA>              TRUE
## A.show              0.048801740          S.show              TRUE
## S.report            0.050211524            <NA>              TRUE
## A.report            0.050211524        S.report              TRUE
## S.share             0.050329686            <NA>              TRUE
## A.share             0.050329686         S.share              TRUE
## S.year              0.051146178            <NA>              TRUE
## A.year              0.051146178          S.year              TRUE
## S.compani           0.053012962            <NA>              TRUE
## A.compani           0.053099633       S.compani              TRUE
## H.new               0.053121542            <NA>              TRUE
## S.first             0.053388178            <NA>              TRUE
## A.first             0.053388178         S.first              TRUE
## S.time              0.057595102            <NA>              TRUE
## A.time              0.057790617          S.time              TRUE
## H.newyork           0.057970095            <NA>              TRUE
## S.articl            0.059520554            <NA>              TRUE
## A.articl            0.059520554        S.articl              TRUE
## S.will              0.060575493            <NA>              TRUE
## A.will              0.061025004          S.will              TRUE
## H.day               0.061669687            <NA>              TRUE
## S.newyork           0.062117105            <NA>              TRUE
## A.newyork           0.062117105       S.newyork              TRUE
## H.today             0.063723058            <NA>              TRUE
## H.report            0.064948102            <NA>              TRUE
## H.X2015             0.066584892            <NA>             FALSE
## S.intern            0.068485701            <NA>              TRUE
## A.intern            0.068485701        S.intern              TRUE
## H.daili             0.069192975            <NA>             FALSE
## H.week              0.075105216            <NA>              TRUE
## H.fashion           0.081708612          H.week              TRUE
## S.week              0.084814939            <NA>              TRUE
## A.week              0.084814939          S.week              TRUE
## S.fashion           0.086446251            <NA>              TRUE
## A.fashion           0.086446251       S.fashion              TRUE
## H.num.chars         0.147211183            <NA>                NA
## H.num.chars.log     0.171062360            <NA>              TRUE
## A.num.chars         0.177037425            <NA>                NA
## S.num.chars         0.179331806            <NA>                NA
## H.num.words         0.186036895            <NA>                NA
## H.num.words.unq     0.189702157            <NA>                NA
## H.num.words.log     0.200686356            <NA>              TRUE
## A.num.words         0.204211072            <NA>                NA
## H.num.words.unq.log 0.204496360 H.num.chars.log              TRUE
## S.num.words         0.206385049            <NA>                NA
## A.num.words.unq     0.210242145            <NA>                NA
## S.num.words.unq     0.212102717            <NA>                NA
## A.num.chars.log     0.224548821            <NA>              TRUE
## S.num.chars.log     0.224692967 A.num.chars.log              TRUE
## A.num.words.log     0.245073324            <NA>              TRUE
## S.num.words.log     0.245354135 A.num.words.log              TRUE
## A.num.words.unq.log 0.250601203            <NA>              TRUE
## S.num.words.unq.log 0.250796919 S.num.chars.log              TRUE
## H.has.http                   NA            <NA>             FALSE
## S.has.http                   NA            <NA>             FALSE
## PubDate.year.fctr            NA            <NA>             FALSE
##                     is.cor.y.abs.low
## Popular                        FALSE
## WordCount.log                  FALSE
## WordCount                      FALSE
## PubDate.hour.fctr              FALSE
## H.is.question                  FALSE
## PubDate.wkend                  FALSE
## PubDate.last10                 FALSE
## PubDate.last10.log             FALSE
## PubDate.last1.log              FALSE
## PubDate.last100                FALSE
## PubDate.last1                  FALSE
## A.can                          FALSE
## S.can                          FALSE
## H.has.ebola                    FALSE
## S.make                         FALSE
## A.make                         FALSE
## PubDate.month.fctr             FALSE
## .rnorm                         FALSE
## PubDate.POSIX                   TRUE
## PubDate.zoo                     TRUE
## myCategory.fctr                 TRUE
## UniqueID                        TRUE
## S.one                           TRUE
## S.state                         TRUE
## A.state                         TRUE
## A.one                           TRUE
## S.said                          TRUE
## A.said                          TRUE
## PubDate.last100.log             TRUE
## PubDate.date.fctr               TRUE
## PubDate.second.fctr             TRUE
## A.has.http                      TRUE
## S.presid                       FALSE
## A.presid                       FALSE
## S.take                         FALSE
## A.take                         FALSE
## PubDate.minute.fctr            FALSE
## S.new                          FALSE
## A.new                          FALSE
## PubDate.wkday.fctr             FALSE
## S.day                          FALSE
## A.day                          FALSE
## H.X2014                        FALSE
## S.show                         FALSE
## A.show                         FALSE
## S.report                       FALSE
## A.report                       FALSE
## S.share                        FALSE
## A.share                        FALSE
## S.year                         FALSE
## A.year                         FALSE
## S.compani                      FALSE
## A.compani                      FALSE
## H.new                          FALSE
## S.first                        FALSE
## A.first                        FALSE
## S.time                         FALSE
## A.time                         FALSE
## H.newyork                      FALSE
## S.articl                       FALSE
## A.articl                       FALSE
## S.will                         FALSE
## A.will                         FALSE
## H.day                          FALSE
## S.newyork                      FALSE
## A.newyork                      FALSE
## H.today                        FALSE
## H.report                       FALSE
## H.X2015                        FALSE
## S.intern                       FALSE
## A.intern                       FALSE
## H.daili                        FALSE
## H.week                         FALSE
## H.fashion                      FALSE
## S.week                         FALSE
## A.week                         FALSE
## S.fashion                      FALSE
## A.fashion                      FALSE
## H.num.chars                    FALSE
## H.num.chars.log                FALSE
## A.num.chars                    FALSE
## S.num.chars                    FALSE
## H.num.words                    FALSE
## H.num.words.unq                FALSE
## H.num.words.log                FALSE
## A.num.words                    FALSE
## H.num.words.unq.log            FALSE
## S.num.words                    FALSE
## A.num.words.unq                FALSE
## S.num.words.unq                FALSE
## A.num.chars.log                FALSE
## S.num.chars.log                FALSE
## A.num.words.log                FALSE
## S.num.words.log                FALSE
## A.num.words.unq.log            FALSE
## S.num.words.unq.log            FALSE
## H.has.http                        NA
## S.has.http                        NA
## PubDate.year.fctr                 NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor     bgn     end elapsed
## 7         select.features          4          0 131.320 140.925   9.605
## 8 partition.data.training          5          0 140.925      NA      NA
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
## [1] 99  7
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
## UniqueID                    NA             TRUE       FALSE   TRUE      NA
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
## [1] 8402  110
```

```r
print("glb_trnent_df: "); print(dim(glb_trnent_df))
```

```
## [1] "glb_trnent_df: "
```

```
## [1] 6532  109
```

```r
print("glb_fitent_df: "); print(dim(glb_fitent_df))
```

```
## [1] "glb_fitent_df: "
```

```
## [1] 4475  109
```

```r
print("glb_OOBent_df: "); print(dim(glb_OOBent_df))
```

```
## [1] "glb_OOBent_df: "
```

```
## [1] 2057  109
```

```r
print("glb_newent_df: "); print(dim(glb_newent_df))
```

```
## [1] "glb_newent_df: "
```

```
## [1] 1870  109
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
## 8 partition.data.training          5          0 140.925 142.398   1.473
## 9              fit.models          6          0 142.398      NA      NA
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
## 1                      0.661                 0.002         0.5
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

![](NYTBlogs_category_files/figure-html/fit.models_0-1.png) 

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

![](NYTBlogs_category_files/figure-html/fit.models_0-2.png) 

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

![](NYTBlogs_category_files/figure-html/fit.models_0-3.png) 

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

![](NYTBlogs_category_files/figure-html/fit.models_0-4.png) 

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
## 1                      0.339                 0.002   0.5007516
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
## [1] "    indep_vars: WordCount.log"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.00234 on full training set
```

```
## Loading required package: rpart.plot
```

![](NYTBlogs_category_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##            CP nsplit rel error
## 1 0.002336449      0         1
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
## 1                      0.715                 0.074         0.5
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

![](NYTBlogs_category_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##              CP nsplit rel error
## 1  0.0023364486      0 1.0000000
## 2  0.0017801513      7 0.9826435
## 3  0.0013351135     18 0.9519359
## 4  0.0010013351     24 0.9439252
## 5  0.0008900757     52 0.9065421
## 6  0.0006675567     58 0.9012016
## 7  0.0004450378     62 0.8985314
## 8  0.0003337784     65 0.8971963
## 9  0.0002670227     69 0.8958611
## 10 0.0000000000     74 0.8945260
## 
## Variable importance
## WordCount.log 
##           100 
## 
## Node number 1: 4475 observations,    complexity param=0.002336449
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (3259 obs) right son=3 (1216 obs)
##   Primary splits:
##       WordCount.log < 6.524296 to the left,  improve=104.8553, (0 missing)
## 
## Node number 2: 3259 observations,    complexity param=0.0003337784
##   predicted class=N  expected loss=0.1012581  P(node) =0.7282682
##     class counts:  2929   330
##    probabilities: 0.899 0.101 
##   left son=4 (1966 obs) right son=5 (1293 obs)
##   Primary splits:
##       WordCount.log < 5.791487 to the left,  improve=17.69521, (0 missing)
## 
## Node number 3: 1216 observations,    complexity param=0.002336449
##   predicted class=N  expected loss=0.3445724  P(node) =0.2717318
##     class counts:   797   419
##    probabilities: 0.655 0.345 
##   left son=6 (89 obs) right son=7 (1127 obs)
##   Primary splits:
##       WordCount.log < 7.602894 to the right, improve=3.300378, (0 missing)
## 
## Node number 4: 1966 observations
##   predicted class=N  expected loss=0.05900305  P(node) =0.4393296
##     class counts:  1850   116
##    probabilities: 0.941 0.059 
## 
## Node number 5: 1293 observations,    complexity param=0.0003337784
##   predicted class=N  expected loss=0.1655066  P(node) =0.2889385
##     class counts:  1079   214
##    probabilities: 0.834 0.166 
##   left son=10 (488 obs) right son=11 (805 obs)
##   Primary splits:
##       WordCount.log < 6.057954 to the left,  improve=1.435521, (0 missing)
## 
## Node number 6: 89 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.2134831  P(node) =0.01988827
##     class counts:    70    19
##    probabilities: 0.787 0.213 
##   left son=12 (82 obs) right son=13 (7 obs)
##   Primary splits:
##       WordCount.log < 8.229096 to the left,  improve=1.946874, (0 missing)
## 
## Node number 7: 1127 observations,    complexity param=0.002336449
##   predicted class=N  expected loss=0.3549246  P(node) =0.2518436
##     class counts:   727   400
##    probabilities: 0.645 0.355 
##   left son=14 (1043 obs) right son=15 (84 obs)
##   Primary splits:
##       WordCount.log < 7.351479 to the left,  improve=3.219334, (0 missing)
## 
## Node number 10: 488 observations
##   predicted class=N  expected loss=0.1352459  P(node) =0.1090503
##     class counts:   422    66
##    probabilities: 0.865 0.135 
## 
## Node number 11: 805 observations,    complexity param=0.0003337784
##   predicted class=N  expected loss=0.1838509  P(node) =0.1798883
##     class counts:   657   148
##    probabilities: 0.816 0.184 
##   left son=22 (25 obs) right son=23 (780 obs)
##   Primary splits:
##       WordCount.log < 6.508023 to the right, improve=1.067817, (0 missing)
## 
## Node number 12: 82 observations
##   predicted class=N  expected loss=0.1829268  P(node) =0.01832402
##     class counts:    67    15
##    probabilities: 0.817 0.183 
## 
## Node number 13: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 14: 1043 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3441994  P(node) =0.2330726
##     class counts:   684   359
##    probabilities: 0.656 0.344 
##   left son=28 (9 obs) right son=29 (1034 obs)
##   Primary splits:
##       WordCount.log < 7.330075 to the right, improve=2.15108, (0 missing)
## 
## Node number 15: 84 observations,    complexity param=0.002336449
##   predicted class=N  expected loss=0.4880952  P(node) =0.01877095
##     class counts:    43    41
##    probabilities: 0.512 0.488 
##   left son=30 (73 obs) right son=31 (11 obs)
##   Primary splits:
##       WordCount.log < 7.375882 to the right, improve=2.758258, (0 missing)
## 
## Node number 22: 25 observations
##   predicted class=N  expected loss=0.04  P(node) =0.005586592
##     class counts:    24     1
##    probabilities: 0.960 0.040 
## 
## Node number 23: 780 observations,    complexity param=0.0003337784
##   predicted class=N  expected loss=0.1884615  P(node) =0.1743017
##     class counts:   633   147
##    probabilities: 0.812 0.188 
##   left son=46 (769 obs) right son=47 (11 obs)
##   Primary splits:
##       WordCount.log < 6.498281 to the left,  improve=2.843874, (0 missing)
## 
## Node number 28: 9 observations
##   predicted class=N  expected loss=0  P(node) =0.002011173
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 29: 1034 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3471954  P(node) =0.2310615
##     class counts:   675   359
##    probabilities: 0.653 0.347 
##   left son=58 (200 obs) right son=59 (834 obs)
##   Primary splits:
##       WordCount.log < 6.656083 to the left,  improve=2.239201, (0 missing)
## 
## Node number 30: 73 observations,    complexity param=0.002336449
##   predicted class=N  expected loss=0.4383562  P(node) =0.01631285
##     class counts:    41    32
##    probabilities: 0.562 0.438 
##   left son=60 (8 obs) right son=61 (65 obs)
##   Primary splits:
##       WordCount.log < 7.394185 to the left,  improve=3.452898, (0 missing)
## 
## Node number 31: 11 observations
##   predicted class=Y  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     2     9
##    probabilities: 0.182 0.818 
## 
## Node number 46: 769 observations
##   predicted class=N  expected loss=0.183355  P(node) =0.1718436
##     class counts:   628   141
##    probabilities: 0.817 0.183 
## 
## Node number 47: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.002458101
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 58: 200 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.28  P(node) =0.04469274
##     class counts:   144    56
##    probabilities: 0.720 0.280 
##   left son=116 (113 obs) right son=117 (87 obs)
##   Primary splits:
##       WordCount.log < 6.586171 to the right, improve=1.793901, (0 missing)
## 
## Node number 59: 834 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3633094  P(node) =0.1863687
##     class counts:   531   303
##    probabilities: 0.637 0.363 
##   left son=118 (826 obs) right son=119 (8 obs)
##   Primary splits:
##       WordCount.log < 6.660574 to the right, improve=2.415646, (0 missing)
## 
## Node number 60: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.001787709
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 61: 65 observations,    complexity param=0.002336449
##   predicted class=N  expected loss=0.4923077  P(node) =0.01452514
##     class counts:    33    32
##    probabilities: 0.508 0.492 
##   left son=122 (41 obs) right son=123 (24 obs)
##   Primary splits:
##       WordCount.log < 7.450369 to the right, improve=0.6305191, (0 missing)
## 
## Node number 116: 113 observations
##   predicted class=N  expected loss=0.2212389  P(node) =0.0252514
##     class counts:    88    25
##    probabilities: 0.779 0.221 
## 
## Node number 117: 87 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3563218  P(node) =0.01944134
##     class counts:    56    31
##    probabilities: 0.644 0.356 
##   left son=234 (74 obs) right son=235 (13 obs)
##   Primary splits:
##       WordCount.log < 6.575773 to the left,  improve=2.051497, (0 missing)
## 
## Node number 118: 826 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3595642  P(node) =0.184581
##     class counts:   529   297
##    probabilities: 0.640 0.360 
##   left son=236 (7 obs) right son=237 (819 obs)
##   Primary splits:
##       WordCount.log < 6.663771 to the left,  improve=1.82548, (0 missing)
## 
## Node number 119: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001787709
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## Node number 122: 41 observations,    complexity param=0.002336449
##   predicted class=N  expected loss=0.4390244  P(node) =0.009162011
##     class counts:    23    18
##    probabilities: 0.561 0.439 
##   left son=244 (23 obs) right son=245 (18 obs)
##   Primary splits:
##       WordCount.log < 7.544861 to the left,  improve=0.8714505, (0 missing)
## 
## Node number 123: 24 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.4166667  P(node) =0.005363128
##     class counts:    10    14
##    probabilities: 0.417 0.583 
##   left son=246 (17 obs) right son=247 (7 obs)
##   Primary splits:
##       WordCount.log < 7.425358 to the left,  improve=1.481793, (0 missing)
## 
## Node number 234: 74 observations,    complexity param=0.0006675567
##   predicted class=N  expected loss=0.3108108  P(node) =0.01653631
##     class counts:    51    23
##    probabilities: 0.689 0.311 
##   left son=468 (13 obs) right son=469 (61 obs)
##   Primary splits:
##       WordCount.log < 6.567375 to the right, improve=0.7771037, (0 missing)
## 
## Node number 235: 13 observations
##   predicted class=Y  expected loss=0.3846154  P(node) =0.002905028
##     class counts:     5     8
##    probabilities: 0.385 0.615 
## 
## Node number 236: 7 observations
##   predicted class=N  expected loss=0  P(node) =0.001564246
##     class counts:     7     0
##    probabilities: 1.000 0.000 
## 
## Node number 237: 819 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3626374  P(node) =0.1830168
##     class counts:   522   297
##    probabilities: 0.637 0.363 
##   left son=474 (805 obs) right son=475 (14 obs)
##   Primary splits:
##       WordCount.log < 7.296074 to the left,  improve=1.241854, (0 missing)
## 
## Node number 244: 23 observations
##   predicted class=N  expected loss=0.3478261  P(node) =0.005139665
##     class counts:    15     8
##    probabilities: 0.652 0.348 
## 
## Node number 245: 18 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.004022346
##     class counts:     8    10
##    probabilities: 0.444 0.556 
## 
## Node number 246: 17 observations
##   predicted class=N  expected loss=0.4705882  P(node) =0.003798883
##     class counts:     9     8
##    probabilities: 0.529 0.471 
## 
## Node number 247: 7 observations
##   predicted class=Y  expected loss=0.1428571  P(node) =0.001564246
##     class counts:     1     6
##    probabilities: 0.143 0.857 
## 
## Node number 468: 13 observations
##   predicted class=N  expected loss=0.1538462  P(node) =0.002905028
##     class counts:    11     2
##    probabilities: 0.846 0.154 
## 
## Node number 469: 61 observations,    complexity param=0.0006675567
##   predicted class=N  expected loss=0.3442623  P(node) =0.01363128
##     class counts:    40    21
##    probabilities: 0.656 0.344 
##   left son=938 (46 obs) right son=939 (15 obs)
##   Primary splits:
##       WordCount.log < 6.554645 to the left,  improve=1.422143, (0 missing)
## 
## Node number 474: 805 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3590062  P(node) =0.1798883
##     class counts:   516   289
##    probabilities: 0.641 0.359 
##   left son=948 (11 obs) right son=949 (794 obs)
##   Primary splits:
##       WordCount.log < 7.281041 to the right, improve=1.60318, (0 missing)
## 
## Node number 475: 14 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.003128492
##     class counts:     6     8
##    probabilities: 0.429 0.571 
## 
## Node number 938: 46 observations
##   predicted class=N  expected loss=0.2826087  P(node) =0.01027933
##     class counts:    33    13
##    probabilities: 0.717 0.283 
## 
## Node number 939: 15 observations
##   predicted class=Y  expected loss=0.4666667  P(node) =0.003351955
##     class counts:     7     8
##    probabilities: 0.467 0.533 
## 
## Node number 948: 11 observations
##   predicted class=N  expected loss=0.09090909  P(node) =0.002458101
##     class counts:    10     1
##    probabilities: 0.909 0.091 
## 
## Node number 949: 794 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3627204  P(node) =0.1774302
##     class counts:   506   288
##    probabilities: 0.637 0.363 
##   left son=1898 (782 obs) right son=1899 (12 obs)
##   Primary splits:
##       WordCount.log < 7.269616 to the left,  improve=2.251223, (0 missing)
## 
## Node number 1898: 782 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3580563  P(node) =0.1747486
##     class counts:   502   280
##    probabilities: 0.642 0.358 
##   left son=3796 (597 obs) right son=3797 (185 obs)
##   Primary splits:
##       WordCount.log < 6.775937 to the right, improve=1.086572, (0 missing)
## 
## Node number 1899: 12 observations
##   predicted class=Y  expected loss=0.3333333  P(node) =0.002681564
##     class counts:     4     8
##    probabilities: 0.333 0.667 
## 
## Node number 3796: 597 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3433836  P(node) =0.1334078
##     class counts:   392   205
##    probabilities: 0.657 0.343 
##   left son=7592 (16 obs) right son=7593 (581 obs)
##   Primary splits:
##       WordCount.log < 6.785022 to the left,  improve=3.877102, (0 missing)
## 
## Node number 3797: 185 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.4054054  P(node) =0.04134078
##     class counts:   110    75
##    probabilities: 0.595 0.405 
##   left son=7594 (135 obs) right son=7595 (50 obs)
##   Primary splits:
##       WordCount.log < 6.742291 to the left,  improve=5.189189, (0 missing)
## 
## Node number 7592: 16 observations
##   predicted class=N  expected loss=0  P(node) =0.003575419
##     class counts:    16     0
##    probabilities: 1.000 0.000 
## 
## Node number 7593: 581 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3528399  P(node) =0.1298324
##     class counts:   376   205
##    probabilities: 0.647 0.353 
##   left son=15186 (570 obs) right son=15187 (11 obs)
##   Primary splits:
##       WordCount.log < 6.790659 to the right, improve=0.83196, (0 missing)
## 
## Node number 7594: 135 observations,    complexity param=0.0002670227
##   predicted class=N  expected loss=0.3333333  P(node) =0.0301676
##     class counts:    90    45
##    probabilities: 0.667 0.333 
##   left son=15188 (34 obs) right son=15189 (101 obs)
##   Primary splits:
##       WordCount.log < 6.722028 to the right, improve=1.476412, (0 missing)
## 
## Node number 7595: 50 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.4  P(node) =0.01117318
##     class counts:    20    30
##    probabilities: 0.400 0.600 
##   left son=15190 (41 obs) right son=15191 (9 obs)
##   Primary splits:
##       WordCount.log < 6.773652 to the left,  improve=1.831978, (0 missing)
## 
## Node number 15186: 570 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3491228  P(node) =0.1273743
##     class counts:   371   199
##    probabilities: 0.651 0.349 
##   left son=30372 (429 obs) right son=30373 (141 obs)
##   Primary splits:
##       WordCount.log < 7.107425 to the left,  improve=0.429473, (0 missing)
## 
## Node number 15187: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.002458101
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 15188: 34 observations
##   predicted class=N  expected loss=0.2058824  P(node) =0.007597765
##     class counts:    27     7
##    probabilities: 0.794 0.206 
## 
## Node number 15189: 101 observations,    complexity param=0.0002670227
##   predicted class=N  expected loss=0.3762376  P(node) =0.02256983
##     class counts:    63    38
##    probabilities: 0.624 0.376 
##   left son=30378 (26 obs) right son=30379 (75 obs)
##   Primary splits:
##       WordCount.log < 6.678971 to the left,  improve=0.3290175, (0 missing)
## 
## Node number 15190: 41 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.4634146  P(node) =0.009162011
##     class counts:    19    22
##    probabilities: 0.463 0.537 
##   left son=30380 (32 obs) right son=30381 (9 obs)
##   Primary splits:
##       WordCount.log < 6.748173 to the right, improve=0.3902439, (0 missing)
## 
## Node number 15191: 9 observations
##   predicted class=Y  expected loss=0.1111111  P(node) =0.002011173
##     class counts:     1     8
##    probabilities: 0.111 0.889 
## 
## Node number 30372: 429 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3379953  P(node) =0.09586592
##     class counts:   284   145
##    probabilities: 0.662 0.338 
##   left son=60744 (10 obs) right son=60745 (419 obs)
##   Primary splits:
##       WordCount.log < 7.099614 to the right, improve=2.339347, (0 missing)
## 
## Node number 30373: 141 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3829787  P(node) =0.03150838
##     class counts:    87    54
##    probabilities: 0.617 0.383 
##   left son=60746 (133 obs) right son=60747 (8 obs)
##   Primary splits:
##       WordCount.log < 7.115987 to the right, improve=0.993561, (0 missing)
## 
## Node number 30378: 26 observations
##   predicted class=N  expected loss=0.3076923  P(node) =0.005810056
##     class counts:    18     8
##    probabilities: 0.692 0.308 
## 
## Node number 30379: 75 observations,    complexity param=0.0002670227
##   predicted class=N  expected loss=0.4  P(node) =0.01675978
##     class counts:    45    30
##    probabilities: 0.600 0.400 
##   left son=60758 (59 obs) right son=60759 (16 obs)
##   Primary splits:
##       WordCount.log < 6.691463 to the right, improve=0.4067797, (0 missing)
## 
## Node number 30380: 32 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.5  P(node) =0.007150838
##     class counts:    16    16
##    probabilities: 0.500 0.500 
##   left son=60760 (9 obs) right son=60761 (23 obs)
##   Primary splits:
##       WordCount.log < 6.756351 to the left,  improve=0.6956522, (0 missing)
## 
## Node number 30381: 9 observations
##   predicted class=Y  expected loss=0.3333333  P(node) =0.002011173
##     class counts:     3     6
##    probabilities: 0.333 0.667 
## 
## Node number 60744: 10 observations
##   predicted class=N  expected loss=0  P(node) =0.002234637
##     class counts:    10     0
##    probabilities: 1.000 0.000 
## 
## Node number 60745: 419 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3460621  P(node) =0.09363128
##     class counts:   274   145
##    probabilities: 0.654 0.346 
##   left son=121490 (412 obs) right son=121491 (7 obs)
##   Primary splits:
##       WordCount.log < 7.091742 to the left,  improve=0.7231421, (0 missing)
## 
## Node number 60746: 133 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3684211  P(node) =0.02972067
##     class counts:    84    49
##    probabilities: 0.632 0.368 
##   left son=121492 (24 obs) right son=121493 (109 obs)
##   Primary splits:
##       WordCount.log < 7.151485 to the left,  improve=0.3450427, (0 missing)
## 
## Node number 60747: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001787709
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 60758: 59 observations,    complexity param=0.0002670227
##   predicted class=N  expected loss=0.3728814  P(node) =0.01318436
##     class counts:    37    22
##    probabilities: 0.627 0.373 
##   left son=121516 (10 obs) right son=121517 (49 obs)
##   Primary splits:
##       WordCount.log < 6.697651 to the left,  improve=0.719751, (0 missing)
## 
## Node number 60759: 16 observations
##   predicted class=N  expected loss=0.5  P(node) =0.003575419
##     class counts:     8     8
##    probabilities: 0.500 0.500 
## 
## Node number 60760: 9 observations
##   predicted class=N  expected loss=0.3333333  P(node) =0.002011173
##     class counts:     6     3
##    probabilities: 0.667 0.333 
## 
## Node number 60761: 23 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.4347826  P(node) =0.005139665
##     class counts:    10    13
##    probabilities: 0.435 0.565 
##   left son=121522 (11 obs) right son=121523 (12 obs)
##   Primary splits:
##       WordCount.log < 6.764462 to the right, improve=0.516469, (0 missing)
## 
## Node number 121490: 412 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.342233  P(node) =0.09206704
##     class counts:   271   141
##    probabilities: 0.658 0.342 
##   left son=242980 (184 obs) right son=242981 (228 obs)
##   Primary splits:
##       WordCount.log < 6.935857 to the right, improve=0.9544408, (0 missing)
## 
## Node number 121491: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 121492: 24 observations
##   predicted class=N  expected loss=0.2916667  P(node) =0.005363128
##     class counts:    17     7
##    probabilities: 0.708 0.292 
## 
## Node number 121493: 109 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3853211  P(node) =0.02435754
##     class counts:    67    42
##    probabilities: 0.615 0.385 
##   left son=242986 (77 obs) right son=242987 (32 obs)
##   Primary splits:
##       WordCount.log < 7.180451 to the right, improve=0.6305925, (0 missing)
## 
## Node number 121516: 10 observations
##   predicted class=N  expected loss=0.2  P(node) =0.002234637
##     class counts:     8     2
##    probabilities: 0.800 0.200 
## 
## Node number 121517: 49 observations,    complexity param=0.0002670227
##   predicted class=N  expected loss=0.4081633  P(node) =0.01094972
##     class counts:    29    20
##    probabilities: 0.592 0.408 
##   left son=243034 (40 obs) right son=243035 (9 obs)
##   Primary splits:
##       WordCount.log < 6.702574 to the right, improve=0.4790249, (0 missing)
## 
## Node number 121522: 11 observations
##   predicted class=N  expected loss=0.4545455  P(node) =0.002458101
##     class counts:     6     5
##    probabilities: 0.545 0.455 
## 
## Node number 121523: 12 observations
##   predicted class=Y  expected loss=0.3333333  P(node) =0.002681564
##     class counts:     4     8
##    probabilities: 0.333 0.667 
## 
## Node number 242980: 184 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3043478  P(node) =0.04111732
##     class counts:   128    56
##    probabilities: 0.696 0.304 
##   left son=485960 (11 obs) right son=485961 (173 obs)
##   Primary splits:
##       WordCount.log < 6.942156 to the left,  improve=2.167379, (0 missing)
## 
## Node number 242981: 228 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.372807  P(node) =0.05094972
##     class counts:   143    85
##    probabilities: 0.627 0.373 
##   left son=485962 (175 obs) right son=485963 (53 obs)
##   Primary splits:
##       WordCount.log < 6.89821  to the left,  improve=1.915098, (0 missing)
## 
## Node number 242986: 77 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3506494  P(node) =0.0172067
##     class counts:    50    27
##    probabilities: 0.649 0.351 
##   left son=485972 (33 obs) right son=485973 (44 obs)
##   Primary splits:
##       WordCount.log < 7.219642 to the left,  improve=0.7012987, (0 missing)
## 
## Node number 242987: 32 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.46875  P(node) =0.007150838
##     class counts:    17    15
##    probabilities: 0.531 0.469 
##   left son=485974 (21 obs) right son=485975 (11 obs)
##   Primary splits:
##       WordCount.log < 7.168965 to the left,  improve=0.941829, (0 missing)
## 
## Node number 243034: 40 observations
##   predicted class=N  expected loss=0.375  P(node) =0.008938547
##     class counts:    25    15
##    probabilities: 0.625 0.375 
## 
## Node number 243035: 9 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.002011173
##     class counts:     4     5
##    probabilities: 0.444 0.556 
## 
## Node number 485960: 11 observations
##   predicted class=N  expected loss=0  P(node) =0.002458101
##     class counts:    11     0
##    probabilities: 1.000 0.000 
## 
## Node number 485961: 173 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3236994  P(node) =0.03865922
##     class counts:   117    56
##    probabilities: 0.676 0.324 
##   left son=971922 (19 obs) right son=971923 (154 obs)
##   Primary splits:
##       WordCount.log < 7.077076 to the right, improve=0.5467584, (0 missing)
## 
## Node number 485962: 175 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3371429  P(node) =0.03910615
##     class counts:   116    59
##    probabilities: 0.663 0.337 
##   left son=971924 (14 obs) right son=971925 (161 obs)
##   Primary splits:
##       WordCount.log < 6.892134 to the right, improve=2.14882, (0 missing)
## 
## Node number 485963: 53 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.490566  P(node) =0.01184358
##     class counts:    27    26
##    probabilities: 0.509 0.491 
##   left son=971926 (46 obs) right son=971927 (7 obs)
##   Primary splits:
##       WordCount.log < 6.932935 to the left,  improve=0.8073362, (0 missing)
## 
## Node number 485972: 33 observations
##   predicted class=N  expected loss=0.2727273  P(node) =0.007374302
##     class counts:    24     9
##    probabilities: 0.727 0.273 
## 
## Node number 485973: 44 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.4090909  P(node) =0.009832402
##     class counts:    26    18
##    probabilities: 0.591 0.409 
##   left son=971946 (31 obs) right son=971947 (13 obs)
##   Primary splits:
##       WordCount.log < 7.238138 to the right, improve=1.570494, (0 missing)
## 
## Node number 485974: 21 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3809524  P(node) =0.004692737
##     class counts:    13     8
##    probabilities: 0.619 0.381 
##   left son=971948 (14 obs) right son=971949 (7 obs)
##   Primary splits:
##       WordCount.log < 7.158903 to the right, improve=0.7619048, (0 missing)
## 
## Node number 485975: 11 observations
##   predicted class=Y  expected loss=0.3636364  P(node) =0.002458101
##     class counts:     4     7
##    probabilities: 0.364 0.636 
## 
## Node number 971922: 19 observations
##   predicted class=N  expected loss=0.2105263  P(node) =0.00424581
##     class counts:    15     4
##    probabilities: 0.789 0.211 
## 
## Node number 971923: 154 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3376623  P(node) =0.03441341
##     class counts:   102    52
##    probabilities: 0.662 0.338 
##   left son=1943846 (145 obs) right son=1943847 (9 obs)
##   Primary splits:
##       WordCount.log < 7.070299 to the left,  improve=0.907638, (0 missing)
## 
## Node number 971924: 14 observations
##   predicted class=N  expected loss=0.07142857  P(node) =0.003128492
##     class counts:    13     1
##    probabilities: 0.929 0.071 
## 
## Node number 971925: 161 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3602484  P(node) =0.03597765
##     class counts:   103    58
##    probabilities: 0.640 0.360 
##   left son=1943850 (120 obs) right son=1943851 (41 obs)
##   Primary splits:
##       WordCount.log < 6.862235 to the left,  improve=1.170936, (0 missing)
## 
## Node number 971926: 46 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.4565217  P(node) =0.01027933
##     class counts:    25    21
##    probabilities: 0.543 0.457 
##   left son=1943852 (16 obs) right son=1943853 (30 obs)
##   Primary splits:
##       WordCount.log < 6.920178 to the right, improve=1.017754, (0 missing)
## 
## Node number 971927: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.001564246
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 971946: 31 observations
##   predicted class=N  expected loss=0.3225806  P(node) =0.006927374
##     class counts:    21    10
##    probabilities: 0.677 0.323 
## 
## Node number 971947: 13 observations
##   predicted class=Y  expected loss=0.3846154  P(node) =0.002905028
##     class counts:     5     8
##    probabilities: 0.385 0.615 
## 
## Node number 971948: 14 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.003128492
##     class counts:    10     4
##    probabilities: 0.714 0.286 
## 
## Node number 971949: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 1943846: 145 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3241379  P(node) =0.03240223
##     class counts:    98    47
##    probabilities: 0.676 0.324 
##   left son=3887692 (14 obs) right son=3887693 (131 obs)
##   Primary splits:
##       WordCount.log < 7.054881 to the right, improve=1.018494, (0 missing)
## 
## Node number 1943847: 9 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.002011173
##     class counts:     4     5
##    probabilities: 0.444 0.556 
## 
## Node number 1943850: 120 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.325  P(node) =0.02681564
##     class counts:    81    39
##    probabilities: 0.675 0.325 
##   left son=3887700 (12 obs) right son=3887701 (108 obs)
##   Primary splits:
##       WordCount.log < 6.855935 to the right, improve=1.557407, (0 missing)
## 
## Node number 1943851: 41 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.4634146  P(node) =0.009162011
##     class counts:    22    19
##    probabilities: 0.537 0.463 
##   left son=3887702 (27 obs) right son=3887703 (14 obs)
##   Primary splits:
##       WordCount.log < 6.873163 to the right, improve=1.36908, (0 missing)
## 
## Node number 1943852: 16 observations
##   predicted class=N  expected loss=0.3125  P(node) =0.003575419
##     class counts:    11     5
##    probabilities: 0.688 0.313 
## 
## Node number 1943853: 30 observations,    complexity param=0.0006675567
##   predicted class=Y  expected loss=0.4666667  P(node) =0.006703911
##     class counts:    14    16
##    probabilities: 0.467 0.533 
##   left son=3887706 (20 obs) right son=3887707 (10 obs)
##   Primary splits:
##       WordCount.log < 6.907755 to the right, improve=0.1333333, (0 missing)
## 
## Node number 3887692: 14 observations
##   predicted class=N  expected loss=0.1428571  P(node) =0.003128492
##     class counts:    12     2
##    probabilities: 0.857 0.143 
## 
## Node number 3887693: 131 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3435115  P(node) =0.02927374
##     class counts:    86    45
##    probabilities: 0.656 0.344 
##   left son=7775386 (74 obs) right son=7775387 (57 obs)
##   Primary splits:
##       WordCount.log < 7.000789 to the left,  improve=1.213415, (0 missing)
## 
## Node number 3887700: 12 observations
##   predicted class=N  expected loss=0.08333333  P(node) =0.002681564
##     class counts:    11     1
##    probabilities: 0.917 0.083 
## 
## Node number 3887701: 108 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.3518519  P(node) =0.02413408
##     class counts:    70    38
##    probabilities: 0.648 0.352 
##   left son=7775402 (69 obs) right son=7775403 (39 obs)
##   Primary splits:
##       WordCount.log < 6.810693 to the right, improve=0.4164499, (0 missing)
## 
## Node number 3887702: 27 observations
##   predicted class=N  expected loss=0.3703704  P(node) =0.00603352
##     class counts:    17    10
##    probabilities: 0.630 0.370 
## 
## Node number 3887703: 14 observations
##   predicted class=Y  expected loss=0.3571429  P(node) =0.003128492
##     class counts:     5     9
##    probabilities: 0.357 0.643 
## 
## Node number 3887706: 20 observations,    complexity param=0.0006675567
##   predicted class=N  expected loss=0.5  P(node) =0.004469274
##     class counts:    10    10
##    probabilities: 0.500 0.500 
##   left son=7775412 (13 obs) right son=7775413 (7 obs)
##   Primary splits:
##       WordCount.log < 6.916219 to the left,  improve=0.1098901, (0 missing)
## 
## Node number 3887707: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.002234637
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 7775386: 74 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2837838  P(node) =0.01653631
##     class counts:    53    21
##    probabilities: 0.716 0.284 
##   left son=15550772 (19 obs) right son=15550773 (55 obs)
##   Primary splits:
##       WordCount.log < 6.98749  to the right, improve=1.629406, (0 missing)
## 
## Node number 7775387: 57 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.4210526  P(node) =0.01273743
##     class counts:    33    24
##    probabilities: 0.579 0.421 
##   left son=15550774 (43 obs) right son=15550775 (14 obs)
##   Primary splits:
##       WordCount.log < 7.013015 to the right, improve=1.826019, (0 missing)
## 
## Node number 7775402: 69 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.3188406  P(node) =0.01541899
##     class counts:    47    22
##    probabilities: 0.681 0.319 
##   left son=15550804 (18 obs) right son=15550805 (51 obs)
##   Primary splits:
##       WordCount.log < 6.821652 to the left,  improve=1.127877, (0 missing)
## 
## Node number 7775403: 39 observations,    complexity param=0.001001335
##   predicted class=N  expected loss=0.4102564  P(node) =0.008715084
##     class counts:    23    16
##    probabilities: 0.590 0.410 
##   left son=15550806 (28 obs) right son=15550807 (11 obs)
##   Primary splits:
##       WordCount.log < 6.806276 to the left,  improve=1.5666, (0 missing)
## 
## Node number 7775412: 13 observations
##   predicted class=N  expected loss=0.4615385  P(node) =0.002905028
##     class counts:     7     6
##    probabilities: 0.538 0.462 
## 
## Node number 7775413: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 15550772: 19 observations
##   predicted class=N  expected loss=0.1052632  P(node) =0.00424581
##     class counts:    17     2
##    probabilities: 0.895 0.105 
## 
## Node number 15550773: 55 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3454545  P(node) =0.0122905
##     class counts:    36    19
##    probabilities: 0.655 0.345 
##   left son=31101546 (34 obs) right son=31101547 (21 obs)
##   Primary splits:
##       WordCount.log < 6.959873 to the right, improve=0.4693659, (0 missing)
## 
## Node number 15550774: 43 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3488372  P(node) =0.009608939
##     class counts:    28    15
##    probabilities: 0.651 0.349 
##   left son=31101548 (7 obs) right son=31101549 (36 obs)
##   Primary splits:
##       WordCount.log < 7.025094 to the left,  improve=2.034884, (0 missing)
## 
## Node number 15550775: 14 observations
##   predicted class=Y  expected loss=0.3571429  P(node) =0.003128492
##     class counts:     5     9
##    probabilities: 0.357 0.643 
## 
## Node number 15550804: 18 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.004022346
##     class counts:    15     3
##    probabilities: 0.833 0.167 
## 
## Node number 15550805: 51 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.372549  P(node) =0.01139665
##     class counts:    32    19
##    probabilities: 0.627 0.373 
##   left son=31101610 (20 obs) right son=31101611 (31 obs)
##   Primary splits:
##       WordCount.log < 6.843217 to the right, improve=0.3463631, (0 missing)
## 
## Node number 15550806: 28 observations
##   predicted class=N  expected loss=0.3214286  P(node) =0.006256983
##     class counts:    19     9
##    probabilities: 0.679 0.321 
## 
## Node number 15550807: 11 observations
##   predicted class=Y  expected loss=0.3636364  P(node) =0.002458101
##     class counts:     4     7
##    probabilities: 0.364 0.636 
## 
## Node number 31101546: 34 observations
##   predicted class=N  expected loss=0.2941176  P(node) =0.007597765
##     class counts:    24    10
##    probabilities: 0.706 0.294 
## 
## Node number 31101547: 21 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.4285714  P(node) =0.004692737
##     class counts:    12     9
##    probabilities: 0.571 0.429 
##   left son=62203094 (11 obs) right son=62203095 (10 obs)
##   Primary splits:
##       WordCount.log < 6.949377 to the left,  improve=1.122078, (0 missing)
## 
## Node number 31101548: 7 observations
##   predicted class=N  expected loss=0  P(node) =0.001564246
##     class counts:     7     0
##    probabilities: 1.000 0.000 
## 
## Node number 31101549: 36 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.4166667  P(node) =0.008044693
##     class counts:    21    15
##    probabilities: 0.583 0.417 
##   left son=62203098 (14 obs) right son=62203099 (22 obs)
##   Primary splits:
##       WordCount.log < 7.046212 to the right, improve=0.7857143, (0 missing)
## 
## Node number 31101610: 20 observations
##   predicted class=N  expected loss=0.3  P(node) =0.004469274
##     class counts:    14     6
##    probabilities: 0.700 0.300 
## 
## Node number 31101611: 31 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.4193548  P(node) =0.006927374
##     class counts:    18    13
##    probabilities: 0.581 0.419 
##   left son=62203222 (24 obs) right son=62203223 (7 obs)
##   Primary splits:
##       WordCount.log < 6.838941 to the left,  improve=0.4182028, (0 missing)
## 
## Node number 62203094: 11 observations
##   predicted class=N  expected loss=0.2727273  P(node) =0.002458101
##     class counts:     8     3
##    probabilities: 0.727 0.273 
## 
## Node number 62203095: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.002234637
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 62203098: 14 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.003128492
##     class counts:    10     4
##    probabilities: 0.714 0.286 
## 
## Node number 62203099: 22 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.5  P(node) =0.004916201
##     class counts:    11    11
##    probabilities: 0.500 0.500 
##   left son=124406198 (14 obs) right son=124406199 (8 obs)
##   Primary splits:
##       WordCount.log < 7.037904 to the left,  improve=0.3928571, (0 missing)
## 
## Node number 62203222: 24 observations
##   predicted class=N  expected loss=0.375  P(node) =0.005363128
##     class counts:    15     9
##    probabilities: 0.625 0.375 
## 
## Node number 62203223: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 124406198: 14 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.003128492
##     class counts:     8     6
##    probabilities: 0.571 0.429 
## 
## Node number 124406199: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001787709
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##         1) root 4475 749 N (0.83262570 0.16737430)  
##           2) WordCount.log< 6.524296 3259 330 N (0.89874195 0.10125805)  
##             4) WordCount.log< 5.791487 1966 116 N (0.94099695 0.05900305) *
##             5) WordCount.log>=5.791487 1293 214 N (0.83449343 0.16550657)  
##              10) WordCount.log< 6.057954 488  66 N (0.86475410 0.13524590) *
##              11) WordCount.log>=6.057954 805 148 N (0.81614907 0.18385093)  
##                22) WordCount.log>=6.508023 25   1 N (0.96000000 0.04000000) *
##                23) WordCount.log< 6.508023 780 147 N (0.81153846 0.18846154)  
##                  46) WordCount.log< 6.498281 769 141 N (0.81664499 0.18335501) *
##                  47) WordCount.log>=6.498281 11   5 Y (0.45454545 0.54545455) *
##           3) WordCount.log>=6.524296 1216 419 N (0.65542763 0.34457237)  
##             6) WordCount.log>=7.602894 89  19 N (0.78651685 0.21348315)  
##              12) WordCount.log< 8.229096 82  15 N (0.81707317 0.18292683) *
##              13) WordCount.log>=8.229096 7   3 Y (0.42857143 0.57142857) *
##             7) WordCount.log< 7.602894 1127 400 N (0.64507542 0.35492458)  
##              14) WordCount.log< 7.351479 1043 359 N (0.65580058 0.34419942)  
##                28) WordCount.log>=7.330075 9   0 N (1.00000000 0.00000000) *
##                29) WordCount.log< 7.330075 1034 359 N (0.65280464 0.34719536)  
##                  58) WordCount.log< 6.656083 200  56 N (0.72000000 0.28000000)  
##                   116) WordCount.log>=6.586171 113  25 N (0.77876106 0.22123894) *
##                   117) WordCount.log< 6.586171 87  31 N (0.64367816 0.35632184)  
##                     234) WordCount.log< 6.575773 74  23 N (0.68918919 0.31081081)  
##                       468) WordCount.log>=6.567375 13   2 N (0.84615385 0.15384615) *
##                       469) WordCount.log< 6.567375 61  21 N (0.65573770 0.34426230)  
##                         938) WordCount.log< 6.554645 46  13 N (0.71739130 0.28260870) *
##                         939) WordCount.log>=6.554645 15   7 Y (0.46666667 0.53333333) *
##                     235) WordCount.log>=6.575773 13   5 Y (0.38461538 0.61538462) *
##                  59) WordCount.log>=6.656083 834 303 N (0.63669065 0.36330935)  
##                   118) WordCount.log>=6.660574 826 297 N (0.64043584 0.35956416)  
##                     236) WordCount.log< 6.663771 7   0 N (1.00000000 0.00000000) *
##                     237) WordCount.log>=6.663771 819 297 N (0.63736264 0.36263736)  
##                       474) WordCount.log< 7.296074 805 289 N (0.64099379 0.35900621)  
##                         948) WordCount.log>=7.281041 11   1 N (0.90909091 0.09090909) *
##                         949) WordCount.log< 7.281041 794 288 N (0.63727960 0.36272040)  
##                          1898) WordCount.log< 7.269616 782 280 N (0.64194373 0.35805627)  
##                            3796) WordCount.log>=6.775937 597 205 N (0.65661642 0.34338358)  
##                              7592) WordCount.log< 6.785022 16   0 N (1.00000000 0.00000000) *
##                              7593) WordCount.log>=6.785022 581 205 N (0.64716007 0.35283993)  
##                               15186) WordCount.log>=6.790659 570 199 N (0.65087719 0.34912281)  
##                                 30372) WordCount.log< 7.107425 429 145 N (0.66200466 0.33799534)  
##                                   60744) WordCount.log>=7.099614 10   0 N (1.00000000 0.00000000) *
##                                   60745) WordCount.log< 7.099614 419 145 N (0.65393795 0.34606205)  
##                                    121490) WordCount.log< 7.091742 412 141 N (0.65776699 0.34223301)  
##                                      242980) WordCount.log>=6.935857 184  56 N (0.69565217 0.30434783)  
##                                        485960) WordCount.log< 6.942156 11   0 N (1.00000000 0.00000000) *
##                                        485961) WordCount.log>=6.942156 173  56 N (0.67630058 0.32369942)  
##                                          971922) WordCount.log>=7.077076 19   4 N (0.78947368 0.21052632) *
##                                          971923) WordCount.log< 7.077076 154  52 N (0.66233766 0.33766234)  
##                                           1943846) WordCount.log< 7.070299 145  47 N (0.67586207 0.32413793)  
##                                             3887692) WordCount.log>=7.054881 14   2 N (0.85714286 0.14285714) *
##                                             3887693) WordCount.log< 7.054881 131  45 N (0.65648855 0.34351145)  
##                                               7775386) WordCount.log< 7.000789 74  21 N (0.71621622 0.28378378)  
##                                                15550772) WordCount.log>=6.98749 19   2 N (0.89473684 0.10526316) *
##                                                15550773) WordCount.log< 6.98749 55  19 N (0.65454545 0.34545455)  
##                                                  31101546) WordCount.log>=6.959873 34  10 N (0.70588235 0.29411765) *
##                                                  31101547) WordCount.log< 6.959873 21   9 N (0.57142857 0.42857143)  
##                                                    62203094) WordCount.log< 6.949377 11   3 N (0.72727273 0.27272727) *
##                                                    62203095) WordCount.log>=6.949377 10   4 Y (0.40000000 0.60000000) *
##                                               7775387) WordCount.log>=7.000789 57  24 N (0.57894737 0.42105263)  
##                                                15550774) WordCount.log>=7.013015 43  15 N (0.65116279 0.34883721)  
##                                                  31101548) WordCount.log< 7.025094 7   0 N (1.00000000 0.00000000) *
##                                                  31101549) WordCount.log>=7.025094 36  15 N (0.58333333 0.41666667)  
##                                                    62203098) WordCount.log>=7.046212 14   4 N (0.71428571 0.28571429) *
##                                                    62203099) WordCount.log< 7.046212 22  11 N (0.50000000 0.50000000)  
##                                                     124406198) WordCount.log< 7.037904 14   6 N (0.57142857 0.42857143) *
##                                                     124406199) WordCount.log>=7.037904 8   3 Y (0.37500000 0.62500000) *
##                                                15550775) WordCount.log< 7.013015 14   5 Y (0.35714286 0.64285714) *
##                                           1943847) WordCount.log>=7.070299 9   4 Y (0.44444444 0.55555556) *
##                                      242981) WordCount.log< 6.935857 228  85 N (0.62719298 0.37280702)  
##                                        485962) WordCount.log< 6.89821 175  59 N (0.66285714 0.33714286)  
##                                          971924) WordCount.log>=6.892134 14   1 N (0.92857143 0.07142857) *
##                                          971925) WordCount.log< 6.892134 161  58 N (0.63975155 0.36024845)  
##                                           1943850) WordCount.log< 6.862235 120  39 N (0.67500000 0.32500000)  
##                                             3887700) WordCount.log>=6.855935 12   1 N (0.91666667 0.08333333) *
##                                             3887701) WordCount.log< 6.855935 108  38 N (0.64814815 0.35185185)  
##                                               7775402) WordCount.log>=6.810693 69  22 N (0.68115942 0.31884058)  
##                                                15550804) WordCount.log< 6.821652 18   3 N (0.83333333 0.16666667) *
##                                                15550805) WordCount.log>=6.821652 51  19 N (0.62745098 0.37254902)  
##                                                  31101610) WordCount.log>=6.843217 20   6 N (0.70000000 0.30000000) *
##                                                  31101611) WordCount.log< 6.843217 31  13 N (0.58064516 0.41935484)  
##                                                    62203222) WordCount.log< 6.838941 24   9 N (0.62500000 0.37500000) *
##                                                    62203223) WordCount.log>=6.838941 7   3 Y (0.42857143 0.57142857) *
##                                               7775403) WordCount.log< 6.810693 39  16 N (0.58974359 0.41025641)  
##                                                15550806) WordCount.log< 6.806276 28   9 N (0.67857143 0.32142857) *
##                                                15550807) WordCount.log>=6.806276 11   4 Y (0.36363636 0.63636364) *
##                                           1943851) WordCount.log>=6.862235 41  19 N (0.53658537 0.46341463)  
##                                             3887702) WordCount.log>=6.873163 27  10 N (0.62962963 0.37037037) *
##                                             3887703) WordCount.log< 6.873163 14   5 Y (0.35714286 0.64285714) *
##                                        485963) WordCount.log>=6.89821 53  26 N (0.50943396 0.49056604)  
##                                          971926) WordCount.log< 6.932935 46  21 N (0.54347826 0.45652174)  
##                                           1943852) WordCount.log>=6.920178 16   5 N (0.68750000 0.31250000) *
##                                           1943853) WordCount.log< 6.920178 30  14 Y (0.46666667 0.53333333)  
##                                             3887706) WordCount.log>=6.907755 20  10 N (0.50000000 0.50000000)  
##                                               7775412) WordCount.log< 6.916219 13   6 N (0.53846154 0.46153846) *
##                                               7775413) WordCount.log>=6.916219 7   3 Y (0.42857143 0.57142857) *
##                                             3887707) WordCount.log< 6.907755 10   4 Y (0.40000000 0.60000000) *
##                                          971927) WordCount.log>=6.932935 7   2 Y (0.28571429 0.71428571) *
##                                    121491) WordCount.log>=7.091742 7   3 Y (0.42857143 0.57142857) *
##                                 30373) WordCount.log>=7.107425 141  54 N (0.61702128 0.38297872)  
##                                   60746) WordCount.log>=7.115987 133  49 N (0.63157895 0.36842105)  
##                                    121492) WordCount.log< 7.151485 24   7 N (0.70833333 0.29166667) *
##                                    121493) WordCount.log>=7.151485 109  42 N (0.61467890 0.38532110)  
##                                      242986) WordCount.log>=7.180451 77  27 N (0.64935065 0.35064935)  
##                                        485972) WordCount.log< 7.219642 33   9 N (0.72727273 0.27272727) *
##                                        485973) WordCount.log>=7.219642 44  18 N (0.59090909 0.40909091)  
##                                          971946) WordCount.log>=7.238138 31  10 N (0.67741935 0.32258065) *
##                                          971947) WordCount.log< 7.238138 13   5 Y (0.38461538 0.61538462) *
##                                      242987) WordCount.log< 7.180451 32  15 N (0.53125000 0.46875000)  
##                                        485974) WordCount.log< 7.168965 21   8 N (0.61904762 0.38095238)  
##                                          971948) WordCount.log>=7.158903 14   4 N (0.71428571 0.28571429) *
##                                          971949) WordCount.log< 7.158903 7   3 Y (0.42857143 0.57142857) *
##                                        485975) WordCount.log>=7.168965 11   4 Y (0.36363636 0.63636364) *
##                                   60747) WordCount.log< 7.115987 8   3 Y (0.37500000 0.62500000) *
##                               15187) WordCount.log< 6.790659 11   5 Y (0.45454545 0.54545455) *
##                            3797) WordCount.log< 6.775937 185  75 N (0.59459459 0.40540541)  
##                              7594) WordCount.log< 6.742291 135  45 N (0.66666667 0.33333333)  
##                               15188) WordCount.log>=6.722028 34   7 N (0.79411765 0.20588235) *
##                               15189) WordCount.log< 6.722028 101  38 N (0.62376238 0.37623762)  
##                                 30378) WordCount.log< 6.678971 26   8 N (0.69230769 0.30769231) *
##                                 30379) WordCount.log>=6.678971 75  30 N (0.60000000 0.40000000)  
##                                   60758) WordCount.log>=6.691463 59  22 N (0.62711864 0.37288136)  
##                                    121516) WordCount.log< 6.697651 10   2 N (0.80000000 0.20000000) *
##                                    121517) WordCount.log>=6.697651 49  20 N (0.59183673 0.40816327)  
##                                      243034) WordCount.log>=6.702574 40  15 N (0.62500000 0.37500000) *
##                                      243035) WordCount.log< 6.702574 9   4 Y (0.44444444 0.55555556) *
##                                   60759) WordCount.log< 6.691463 16   8 N (0.50000000 0.50000000) *
##                              7595) WordCount.log>=6.742291 50  20 Y (0.40000000 0.60000000)  
##                               15190) WordCount.log< 6.773652 41  19 Y (0.46341463 0.53658537)  
##                                 30380) WordCount.log>=6.748173 32  16 N (0.50000000 0.50000000)  
##                                   60760) WordCount.log< 6.756351 9   3 N (0.66666667 0.33333333) *
##                                   60761) WordCount.log>=6.756351 23  10 Y (0.43478261 0.56521739)  
##                                    121522) WordCount.log>=6.764462 11   5 N (0.54545455 0.45454545) *
##                                    121523) WordCount.log< 6.764462 12   4 Y (0.33333333 0.66666667) *
##                                 30381) WordCount.log< 6.748173 9   3 Y (0.33333333 0.66666667) *
##                               15191) WordCount.log>=6.773652 9   1 Y (0.11111111 0.88888889) *
##                          1899) WordCount.log>=7.269616 12   4 Y (0.33333333 0.66666667) *
##                       475) WordCount.log>=7.296074 14   6 Y (0.42857143 0.57142857) *
##                   119) WordCount.log< 6.660574 8   2 Y (0.25000000 0.75000000) *
##              15) WordCount.log>=7.351479 84  41 N (0.51190476 0.48809524)  
##                30) WordCount.log>=7.375882 73  32 N (0.56164384 0.43835616)  
##                  60) WordCount.log< 7.394185 8   0 N (1.00000000 0.00000000) *
##                  61) WordCount.log>=7.394185 65  32 N (0.50769231 0.49230769)  
##                   122) WordCount.log>=7.450369 41  18 N (0.56097561 0.43902439)  
##                     244) WordCount.log< 7.544861 23   8 N (0.65217391 0.34782609) *
##                     245) WordCount.log>=7.544861 18   8 Y (0.44444444 0.55555556) *
##                   123) WordCount.log< 7.450369 24  10 Y (0.41666667 0.58333333)  
##                     246) WordCount.log< 7.425358 17   8 N (0.52941176 0.47058824) *
##                     247) WordCount.log>=7.425358 7   1 Y (0.14285714 0.85714286) *
##                31) WordCount.log< 7.375882 11   2 Y (0.18181818 0.81818182) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_category_files/figure-html/fit.models_0-7.png) 

```
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.40217391
## 3        0.2 0.46180758
## 4        0.3 0.45156591
## 5        0.4 0.40212578
## 6        0.5 0.37616387
## 7        0.6 0.24890830
## 8        0.7 0.08596713
## 9        0.8 0.05927835
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](NYTBlogs_category_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3156
## 2            Y                                              353
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              570
## 2                                              396
##          Prediction
## Reference    N    Y
##         N 3156  570
##         Y  353  396
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.937430e-01   3.367504e-01   7.815865e-01   8.055149e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.162731e-12 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_category_files/figure-html/fit.models_0-9.png) 

```
##    threshold    f.score
## 1        0.0 0.28654727
## 2        0.1 0.36516854
## 3        0.2 0.37638376
## 4        0.3 0.32659409
## 5        0.4 0.24768946
## 6        0.5 0.21526419
## 7        0.6 0.11004785
## 8        0.7 0.02209945
## 9        0.8 0.01671309
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](NYTBlogs_category_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1397
## 2            Y                                              191
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              316
## 2                                              153
##          Prediction
## Reference    N    Y
##         N 1397  316
##         Y  191  153
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.535246e-01   2.272928e-01   7.343044e-01   7.720195e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   3.649312e-08 
##                    model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart WordCount.log               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.598                 0.061   0.7667409
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4618076         0.793743
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7815865             0.8055149     0.3367504   0.6722244
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3763838        0.7535246
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7343044             0.7720195     0.2272928
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
## Fitting cp = 0.00234 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](NYTBlogs_category_files/figure-html/fit.models_0-11.png) ![](NYTBlogs_category_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##            CP nsplit rel error
## 1 0.002336449      0         1
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
## 1                      1.302                 0.073         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8214525
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553    0.07605123         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.007708779     0.001192306
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

![](NYTBlogs_category_files/figure-html/fit.models_0-13.png) ![](NYTBlogs_category_files/figure-html/fit.models_0-14.png) ![](NYTBlogs_category_files/figure-html/fit.models_0-15.png) ![](NYTBlogs_category_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4491  -0.6474  -0.4783  -0.2720   3.2121  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -7.14753    0.33665  -21.23   <2e-16 ***
## WordCount.log  0.90771    0.05274   17.21   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 3660.3  on 4473  degrees of freedom
## AIC: 3664.3
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_category_files/figure-html/fit.models_0-17.png) 

```
##    threshold     f.score
## 1        0.0 0.286753446
## 2        0.1 0.355660625
## 3        0.2 0.415026834
## 4        0.3 0.293620292
## 5        0.4 0.092760181
## 6        0.5 0.020698577
## 7        0.6 0.007957560
## 8        0.7 0.002666667
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](NYTBlogs_category_files/figure-html/fit.models_0-18.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 2703
## 2            Y                                  285
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                 1023
## 2                                  464
##          Prediction
## Reference    N    Y
##         N 2703 1023
##         Y  285  464
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.077095e-01   2.475099e-01   6.941397e-01   7.210078e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.616331e-92 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_category_files/figure-html/fit.models_0-19.png) 

```
##    threshold    f.score
## 1        0.0 0.28654727
## 2        0.1 0.35507657
## 3        0.2 0.40618956
## 4        0.3 0.31869919
## 5        0.4 0.09638554
## 6        0.5 0.02747253
## 7        0.6 0.02285714
## 8        0.7 0.01152738
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](NYTBlogs_category_files/figure-html/fit.models_0-20.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1233
## 2            Y                                  134
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  480
## 2                                  210
##          Prediction
## Reference    N    Y
##         N 1233  480
##         Y  134  210
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.015070e-01   2.355742e-01   6.812119e-01   7.212263e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   4.587872e-44 
##        model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm WordCount.log               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.172                 0.077   0.7314944
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4150268        0.8308379
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.6941397             0.7210078    0.01207987   0.7289876
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.4061896         0.701507
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.6812119             0.7212263     0.2355742    3664.292
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0008087605     0.009862351
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
## [1] "    indep_vars: WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log"
## + Fold1: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## - Fold1: parameter=none 
## + Fold2: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## - Fold2: parameter=none 
## + Fold3: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](NYTBlogs_category_files/figure-html/fit.models_0-21.png) ![](NYTBlogs_category_files/figure-html/fit.models_0-22.png) ![](NYTBlogs_category_files/figure-html/fit.models_0-23.png) ![](NYTBlogs_category_files/figure-html/fit.models_0-24.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.2907  -0.5575  -0.3786  -0.1673   3.2101  
## 
## Coefficients:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                      -7.135739   0.370933 -19.237  < 2e-16 ***
## WordCount.log                     2.279009   0.151779  15.015  < 2e-16 ***
## `WordCount.log:S.can`             0.045823   0.030894   1.483  0.13801    
## `WordCount.log:S.make`            0.030495   0.032983   0.925  0.35519    
## `WordCount.log:S.presid`         -0.003700   0.036607  -0.101  0.91949    
## `WordCount.log:S.take`           -0.074626   0.044789  -1.666  0.09568 .  
## `WordCount.log:S.new`             0.017077   0.025091   0.681  0.49614    
## `WordCount.log:S.day`            -0.093463   0.052649  -1.775  0.07586 .  
## `WordCount.log:S.show`           -0.111504   0.053795  -2.073  0.03819 *  
## `WordCount.log:S.report`         -0.096920   0.051574  -1.879  0.06021 .  
## `WordCount.log:S.share`          -0.138405   0.061266  -2.259  0.02388 *  
## `WordCount.log:S.year`           -0.011964   0.036746  -0.326  0.74475    
## `WordCount.log:S.compani`        -0.074288   0.035520  -2.091  0.03649 *  
## `WordCount.log:S.first`          -0.085879   0.053732  -1.598  0.10998    
## `WordCount.log:S.time`           -0.065974   0.035606  -1.853  0.06390 .  
## `WordCount.log:S.articl`         -0.178283   0.080474  -2.215  0.02673 *  
## `WordCount.log:S.will`           -0.065475   0.030671  -2.135  0.03278 *  
## `WordCount.log:S.newyork`        -0.008755   0.039044  -0.224  0.82258    
## `WordCount.log:S.intern`         -0.366196   0.122813  -2.982  0.00287 ** 
## `WordCount.log:H.week`           -0.134209   0.082085  -1.635  0.10205    
## `WordCount.log:S.week`           -0.072670   0.044646  -1.628  0.10359    
## `WordCount.log:S.fashion`        -3.803003 103.589146  -0.037  0.97071    
## `WordCount.log:H.num.chars.log`  -0.109894   0.018136  -6.060 1.37e-09 ***
## `WordCount.log:A.num.chars.log`  -0.802714   0.898230  -0.894  0.37150    
## `WordCount.log:A.num.words.log`  -0.132725   0.060169  -2.206  0.02739 *  
## `WordCount.log:S.num.chars.log`   0.679317   0.898989   0.756  0.44986    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 3126.5  on 4449  degrees of freedom
## AIC: 3178.5
## 
## Number of Fisher Scoring iterations: 17
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_category_files/figure-html/fit.models_0-25.png) 

```
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.43230870
## 3        0.2 0.52093973
## 4        0.3 0.51752022
## 5        0.4 0.46390916
## 6        0.5 0.37873134
## 7        0.6 0.29760666
## 8        0.7 0.18369690
## 9        0.8 0.06289308
## 10       0.9 0.01321004
## 11       1.0 0.00000000
```

![](NYTBlogs_category_files/figure-html/fit.models_0-26.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3027
## 2            Y                                            239
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            699
## 2                                            510
##          Prediction
## Reference    N    Y
##         N 3027  699
##         Y  239  510
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.903911e-01   3.961201e-01   7.781661e-01   8.022358e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   8.945624e-51 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_category_files/figure-html/fit.models_0-27.png) 

```
##    threshold     f.score
## 1        0.0 0.286547272
## 2        0.1 0.424753868
## 3        0.2 0.506131550
## 4        0.3 0.466165414
## 5        0.4 0.407142857
## 6        0.5 0.359183673
## 7        0.6 0.271889401
## 8        0.7 0.185000000
## 9        0.8 0.102981030
## 10       0.9 0.005747126
## 11       1.0 0.000000000
```

![](NYTBlogs_category_files/figure-html/fit.models_0-28.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1387
## 2            Y                                            117
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            326
## 2                                            227
##          Prediction
## Reference    N    Y
##         N 1387  326
##         Y  117  227
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.846378e-01   3.778434e-01   7.662310e-01   8.022297e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   4.964056e-23 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## 1 WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.572                 0.502
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8262824                    0.2       0.5209397        0.8505038
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7781661             0.8022358     0.3059876   0.8094734
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.5061315        0.7846378
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1              0.766231             0.8022297     0.3778434    3178.524
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.005015309      0.03541348
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
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, S.can, H.has.ebola, S.make, .rnorm, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log"
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

```
## Warning: not plotting observations with leverage one:
##   1143, 4408
```

![](NYTBlogs_category_files/figure-html/fit.models_0-29.png) ![](NYTBlogs_category_files/figure-html/fit.models_0-30.png) 

```
## Warning: not plotting observations with leverage one:
##   1143, 4408
```

![](NYTBlogs_category_files/figure-html/fit.models_0-31.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.7691  -0.3430  -0.1723  -0.0001   3.3603  
## 
## Coefficients: (1 not defined because of singularities)
##                                                         Estimate
## (Intercept)                                           -5.411e+00
## WordCount.log                                          1.113e+00
## `PubDate.hour.fctr(7.67,15.3]`                         1.878e-01
## `PubDate.hour.fctr(15.3,23]`                           3.753e-01
## H.is.question                                          1.266e+00
## PubDate.wkend                                         -4.060e-01
## PubDate.last10.log                                     2.535e-01
## PubDate.last1.log                                     -2.531e-02
## S.can                                                 -2.296e-01
## H.has.ebola                                           -4.900e-01
## S.make                                                -1.740e-01
## .rnorm                                                -3.413e-03
## `myCategory.fctrForeign#World#Asia Pacific`           -4.522e+00
## `myCategory.fctr#Multimedia#`                         -4.517e+00
## `myCategory.fctrCulture#Arts#`                        -3.167e+00
## `myCategory.fctrBusiness#Business Day#Dealbook`       -2.796e+00
## myCategory.fctrmyOther                                -2.014e+01
## `myCategory.fctrBusiness#Technology#`                 -1.930e+00
## `myCategory.fctrBusiness#Crosswords/Games#`            4.700e-01
## `myCategory.fctrTStyle##`                             -4.271e+00
## `myCategory.fctrForeign#World#`                       -1.855e+01
## `myCategory.fctrOpEd#Opinion#`                         5.153e-01
## `myCategory.fctrStyles##Fashion`                      -1.942e+01
## `myCategory.fctr#Opinion#Room For Debate`             -5.562e+00
## `myCategory.fctr#U.S.#Education`                      -2.054e+01
## `myCategory.fctr##`                                   -2.731e+00
## `myCategory.fctrMetro#N.Y. / Region#`                 -1.873e+00
## `myCategory.fctrBusiness#Business Day#Small Business` -4.330e+00
## `myCategory.fctrStyles#U.S.#`                         -1.210e-01
## `myCategory.fctrTravel#Travel#`                       -4.326e+00
## `myCategory.fctr#Opinion#The Public Editor`            1.090e+00
## S.one                                                  1.559e+01
## S.state                                                1.675e+01
## A.state                                               -1.610e+01
## A.one                                                 -1.571e+01
## S.said                                                 5.697e-01
## A.said                                                        NA
## PubDate.last100.log                                    6.590e-03
## `PubDate.date.fctr(7,13]`                             -2.812e-04
## `PubDate.date.fctr(13,19]`                            -1.312e-01
## `PubDate.date.fctr(19,25]`                            -6.330e-02
## `PubDate.date.fctr(25,31]`                             1.535e-01
## `PubDate.second.fctr(14.8,29.5]`                       2.741e-02
## `PubDate.second.fctr(29.5,44.2]`                       1.010e-02
## `PubDate.second.fctr(44.2,59.1]`                      -2.761e-01
## S.presid                                               6.612e-02
## S.take                                                -2.418e-01
## `PubDate.minute.fctr(14.8,29.5]`                      -1.560e-01
## `PubDate.minute.fctr(29.5,44.2]`                      -3.409e-01
## `PubDate.minute.fctr(44.2,59.1]`                      -2.061e-02
## S.new                                                  7.136e-02
## PubDate.wkday.fctr1                                   -5.991e-01
## PubDate.wkday.fctr2                                   -1.138e+00
## PubDate.wkday.fctr3                                   -7.769e-01
## PubDate.wkday.fctr4                                   -9.695e-01
## PubDate.wkday.fctr5                                   -9.173e-01
## PubDate.wkday.fctr6                                   -1.196e+00
## S.day                                                 -1.334e-01
## H.X2014                                               -1.950e-01
## S.show                                                -2.570e-01
## S.report                                              -7.548e-01
## S.share                                               -5.297e-01
## S.year                                                -3.108e-01
## S.compani                                             -2.552e-01
## H.new                                                 -5.444e-01
## S.first                                               -1.250e-01
## S.time                                                -2.832e-01
## H.newyork                                              1.518e-01
## S.articl                                              -4.118e-01
## S.will                                                -3.323e-01
## H.day                                                 -8.258e-01
## S.newyork                                              3.570e-01
## H.today                                               -2.449e+00
## H.report                                              -8.046e-01
## S.intern                                              -8.569e-01
## H.week                                                -7.086e-01
## S.week                                                -1.661e-02
## S.fashion                                             -1.427e+01
## H.num.chars.log                                        2.080e-01
## H.num.words.log                                       -7.497e-01
## A.num.chars.log                                        3.516e-01
## A.num.words.log                                        9.265e-01
## A.num.words.unq.log                                   -2.291e+00
##                                                       Std. Error z value
## (Intercept)                                            2.086e+00  -2.594
## WordCount.log                                          8.554e-02  13.014
## `PubDate.hour.fctr(7.67,15.3]`                         2.328e-01   0.807
## `PubDate.hour.fctr(15.3,23]`                           2.366e-01   1.586
## H.is.question                                          2.108e-01   6.006
## PubDate.wkend                                          4.328e-01  -0.938
## PubDate.last10.log                                     1.308e-01   1.938
## PubDate.last1.log                                      4.278e-02  -0.592
## S.can                                                  2.953e-01  -0.778
## H.has.ebola                                            4.359e-01  -1.124
## S.make                                                 2.741e-01  -0.635
## .rnorm                                                 6.118e-02  -0.056
## `myCategory.fctrForeign#World#Asia Pacific`            6.547e-01  -6.907
## `myCategory.fctr#Multimedia#`                          7.839e-01  -5.762
## `myCategory.fctrCulture#Arts#`                         3.269e-01  -9.688
## `myCategory.fctrBusiness#Business Day#Dealbook`        2.822e-01  -9.906
## myCategory.fctrmyOther                                 1.843e+03  -0.011
## `myCategory.fctrBusiness#Technology#`                  3.088e-01  -6.249
## `myCategory.fctrBusiness#Crosswords/Games#`            4.690e-01   1.002
## `myCategory.fctrTStyle##`                              4.784e-01  -8.928
## `myCategory.fctrForeign#World#`                        8.891e+02  -0.021
## `myCategory.fctrOpEd#Opinion#`                         2.793e-01   1.845
## `myCategory.fctrStyles##Fashion`                       1.036e+03  -0.019
## `myCategory.fctr#Opinion#Room For Debate`              5.759e-01  -9.659
## `myCategory.fctr#U.S.#Education`                       6.050e+02  -0.034
## `myCategory.fctr##`                                    2.657e-01 -10.277
## `myCategory.fctrMetro#N.Y. / Region#`                  4.248e-01  -4.411
## `myCategory.fctrBusiness#Business Day#Small Business`  6.674e-01  -6.487
## `myCategory.fctrStyles#U.S.#`                          3.178e-01  -0.381
## `myCategory.fctrTravel#Travel#`                        1.039e+00  -4.164
## `myCategory.fctr#Opinion#The Public Editor`            1.168e+00   0.933
## S.one                                                  1.075e+04   0.001
## S.state                                                1.075e+04   0.002
## A.state                                                1.075e+04  -0.001
## A.one                                                  1.075e+04  -0.001
## S.said                                                 2.640e-01   2.158
## A.said                                                        NA      NA
## PubDate.last100.log                                    4.455e-02   0.148
## `PubDate.date.fctr(7,13]`                              1.903e-01  -0.001
## `PubDate.date.fctr(13,19]`                             1.887e-01  -0.695
## `PubDate.date.fctr(19,25]`                             1.849e-01  -0.342
## `PubDate.date.fctr(25,31]`                             1.967e-01   0.780
## `PubDate.second.fctr(14.8,29.5]`                       1.680e-01   0.163
## `PubDate.second.fctr(29.5,44.2]`                       1.651e-01   0.061
## `PubDate.second.fctr(44.2,59.1]`                       1.725e-01  -1.601
## S.presid                                               2.960e-01   0.223
## S.take                                                 3.807e-01  -0.635
## `PubDate.minute.fctr(14.8,29.5]`                       1.730e-01  -0.902
## `PubDate.minute.fctr(29.5,44.2]`                       1.697e-01  -2.009
## `PubDate.minute.fctr(44.2,59.1]`                       1.763e-01  -0.117
## S.new                                                  2.029e-01   0.352
## PubDate.wkday.fctr1                                    5.018e-01  -1.194
## PubDate.wkday.fctr2                                    5.477e-01  -2.078
## PubDate.wkday.fctr3                                    5.381e-01  -1.444
## PubDate.wkday.fctr4                                    5.327e-01  -1.820
## PubDate.wkday.fctr5                                    5.391e-01  -1.701
## PubDate.wkday.fctr6                                    4.473e-01  -2.673
## S.day                                                  4.177e-01  -0.319
## H.X2014                                                8.424e-01  -0.232
## S.show                                                 4.014e-01  -0.640
## S.report                                               3.990e-01  -1.892
## S.share                                                4.215e-01  -1.257
## S.year                                                 2.954e-01  -1.052
## S.compani                                              2.663e-01  -0.958
## H.new                                                  4.233e-01  -1.286
## S.first                                                4.197e-01  -0.298
## S.time                                                 2.935e-01  -0.965
## H.newyork                                              4.706e-01   0.323
## S.articl                                               7.638e-01  -0.539
## S.will                                                 2.369e-01  -1.403
## H.day                                                  7.069e-01  -1.168
## S.newyork                                              3.321e-01   1.075
## H.today                                                6.513e-01  -3.761
## H.report                                               6.039e-01  -1.332
## S.intern                                               8.334e-01  -1.028
## H.week                                                 6.414e-01  -1.105
## S.week                                                 3.189e-01  -0.052
## S.fashion                                              6.812e+02  -0.021
## H.num.chars.log                                        3.521e-01   0.591
## H.num.words.log                                        4.086e-01  -1.835
## A.num.chars.log                                        4.841e-01   0.726
## A.num.words.log                                        1.583e+00   0.585
## A.num.words.unq.log                                    1.527e+00  -1.500
##                                                       Pr(>|z|)    
## (Intercept)                                            0.00949 ** 
## WordCount.log                                          < 2e-16 ***
## `PubDate.hour.fctr(7.67,15.3]`                         0.41986    
## `PubDate.hour.fctr(15.3,23]`                           0.11269    
## H.is.question                                         1.91e-09 ***
## PubDate.wkend                                          0.34815    
## PubDate.last10.log                                     0.05263 .  
## PubDate.last1.log                                      0.55414    
## S.can                                                  0.43685    
## H.has.ebola                                            0.26093    
## S.make                                                 0.52548    
## .rnorm                                                 0.95551    
## `myCategory.fctrForeign#World#Asia Pacific`           4.93e-12 ***
## `myCategory.fctr#Multimedia#`                         8.30e-09 ***
## `myCategory.fctrCulture#Arts#`                         < 2e-16 ***
## `myCategory.fctrBusiness#Business Day#Dealbook`        < 2e-16 ***
## myCategory.fctrmyOther                                 0.99128    
## `myCategory.fctrBusiness#Technology#`                 4.12e-10 ***
## `myCategory.fctrBusiness#Crosswords/Games#`            0.31635    
## `myCategory.fctrTStyle##`                              < 2e-16 ***
## `myCategory.fctrForeign#World#`                        0.98335    
## `myCategory.fctrOpEd#Opinion#`                         0.06501 .  
## `myCategory.fctrStyles##Fashion`                       0.98504    
## `myCategory.fctr#Opinion#Room For Debate`              < 2e-16 ***
## `myCategory.fctr#U.S.#Education`                       0.97292    
## `myCategory.fctr##`                                    < 2e-16 ***
## `myCategory.fctrMetro#N.Y. / Region#`                 1.03e-05 ***
## `myCategory.fctrBusiness#Business Day#Small Business` 8.74e-11 ***
## `myCategory.fctrStyles#U.S.#`                          0.70347    
## `myCategory.fctrTravel#Travel#`                       3.12e-05 ***
## `myCategory.fctr#Opinion#The Public Editor`            0.35086    
## S.one                                                  0.99884    
## S.state                                                0.99876    
## A.state                                                0.99881    
## A.one                                                  0.99883    
## S.said                                                 0.03094 *  
## A.said                                                      NA    
## PubDate.last100.log                                    0.88239    
## `PubDate.date.fctr(7,13]`                              0.99882    
## `PubDate.date.fctr(13,19]`                             0.48699    
## `PubDate.date.fctr(19,25]`                             0.73213    
## `PubDate.date.fctr(25,31]`                             0.43512    
## `PubDate.second.fctr(14.8,29.5]`                       0.87038    
## `PubDate.second.fctr(29.5,44.2]`                       0.95119    
## `PubDate.second.fctr(44.2,59.1]`                       0.10947    
## S.presid                                               0.82326    
## S.take                                                 0.52531    
## `PubDate.minute.fctr(14.8,29.5]`                       0.36716    
## `PubDate.minute.fctr(29.5,44.2]`                       0.04452 *  
## `PubDate.minute.fctr(44.2,59.1]`                       0.90694    
## S.new                                                  0.72509    
## PubDate.wkday.fctr1                                    0.23256    
## PubDate.wkday.fctr2                                    0.03772 *  
## PubDate.wkday.fctr3                                    0.14876    
## PubDate.wkday.fctr4                                    0.06874 .  
## PubDate.wkday.fctr5                                    0.08888 .  
## PubDate.wkday.fctr6                                    0.00752 ** 
## S.day                                                  0.74950    
## H.X2014                                                0.81691    
## S.show                                                 0.52206    
## S.report                                               0.05851 .  
## S.share                                                0.20890    
## S.year                                                 0.29265    
## S.compani                                              0.33790    
## H.new                                                  0.19841    
## S.first                                                0.76580    
## S.time                                                 0.33460    
## H.newyork                                              0.74697    
## S.articl                                               0.58978    
## S.will                                                 0.16074    
## H.day                                                  0.24274    
## S.newyork                                              0.28238    
## H.today                                                0.00017 ***
## H.report                                               0.18277    
## S.intern                                               0.30383    
## H.week                                                 0.26928    
## S.week                                                 0.95846    
## S.fashion                                              0.98329    
## H.num.chars.log                                        0.55473    
## H.num.words.log                                        0.06652 .  
## A.num.chars.log                                        0.46762    
## A.num.words.log                                        0.55831    
## A.num.words.unq.log                                    0.13353    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 1947.5  on 4393  degrees of freedom
## AIC: 2111.5
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

![](NYTBlogs_category_files/figure-html/fit.models_0-32.png) ![](NYTBlogs_category_files/figure-html/fit.models_0-33.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6511628
## 3        0.2 0.7219344
## 4        0.3 0.7342747
## 5        0.4 0.7278958
## 6        0.5 0.7175793
## 7        0.6 0.6922487
## 8        0.7 0.6456954
## 9        0.8 0.5486239
## 10       0.9 0.3797468
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3489
## 2            Y                                  177
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  237
## 2                                  572
##          Prediction
## Reference    N    Y
##         N 3489  237
##         Y  177  572
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.074860e-01   6.783688e-01   8.986168e-01   9.158192e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   2.373225e-47   3.735297e-03 
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

![](NYTBlogs_category_files/figure-html/fit.models_0-34.png) ![](NYTBlogs_category_files/figure-html/fit.models_0-35.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6266531
## 3        0.2 0.6982097
## 4        0.3 0.7128713
## 5        0.4 0.7106447
## 6        0.5 0.7093750
## 7        0.6 0.6689076
## 8        0.7 0.6252285
## 9        0.8 0.5050916
## 10       0.9 0.3182898
## 11       1.0 0.0000000
```

![](NYTBlogs_category_files/figure-html/fit.models_0-36.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1602
## 2            Y                                   92
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  111
## 2                                  252
##          Prediction
## Reference    N    Y
##         N 1602  111
##         Y   92  252
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.013126e-01   6.533400e-01   8.876034e-01   9.138653e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   4.154945e-19   2.064626e-01 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, S.can, H.has.ebola, S.make, .rnorm, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      6.661                 1.855
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9427059                    0.3       0.7342747        0.9074844
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8986168             0.9158192     0.6475191   0.9245425
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.7128713        0.9013126
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8876034             0.9138653       0.65334    2111.473
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.006501775       0.0209269
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 9  fit.models          6          0 142.398 185.619  43.221
## 10 fit.models          6          1 185.620      NA      NA
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
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, .rnorm, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
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

```
## Warning: not plotting observations with leverage one:
##   1143, 1930, 3625, 3799, 4408
```

![](NYTBlogs_category_files/figure-html/fit.models_1-1.png) ![](NYTBlogs_category_files/figure-html/fit.models_1-2.png) 

```
## Warning: not plotting observations with leverage one:
##   1143, 1930, 3625, 3799, 4408
```

![](NYTBlogs_category_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.7676  -0.3432  -0.1717  -0.0001   3.3071  
## 
## Coefficients: (17 not defined because of singularities)
##                                                         Estimate
## (Intercept)                                           -5.705e+00
## WordCount.log                                          1.117e+00
## `PubDate.hour.fctr(7.67,15.3]`                         2.002e-01
## `PubDate.hour.fctr(15.3,23]`                           3.951e-01
## H.is.question                                          1.273e+00
## PubDate.wkend                                         -4.145e-01
## PubDate.last10.log                                     2.700e-01
## PubDate.last1.log                                     -3.307e-02
## A.can                                                  2.798e+01
## S.can                                                 -2.825e+01
## H.has.ebola                                           -5.142e-01
## S.make                                                -1.781e-01
## A.make                                                        NA
## .rnorm                                                -3.031e-03
## `myCategory.fctrForeign#World#Asia Pacific`           -4.491e+00
## `myCategory.fctr#Multimedia#`                         -4.534e+00
## `myCategory.fctrCulture#Arts#`                        -3.156e+00
## `myCategory.fctrBusiness#Business Day#Dealbook`       -2.803e+00
## myCategory.fctrmyOther                                -2.015e+01
## `myCategory.fctrBusiness#Technology#`                 -1.937e+00
## `myCategory.fctrBusiness#Crosswords/Games#`            4.903e-01
## `myCategory.fctrTStyle##`                             -4.315e+00
## `myCategory.fctrForeign#World#`                       -1.850e+01
## `myCategory.fctrOpEd#Opinion#`                         5.636e-01
## `myCategory.fctrStyles##Fashion`                      -1.984e+01
## `myCategory.fctr#Opinion#Room For Debate`             -5.611e+00
## `myCategory.fctr#U.S.#Education`                      -2.042e+01
## `myCategory.fctr##`                                   -2.736e+00
## `myCategory.fctrMetro#N.Y. / Region#`                 -1.886e+00
## `myCategory.fctrBusiness#Business Day#Small Business` -4.311e+00
## `myCategory.fctrStyles#U.S.#`                         -1.470e-01
## `myCategory.fctrTravel#Travel#`                       -4.326e+00
## `myCategory.fctr#Opinion#The Public Editor`            1.071e+00
## S.one                                                  2.256e+00
## S.state                                                5.874e+00
## A.state                                               -5.235e+00
## A.one                                                 -2.368e+00
## S.said                                                 5.767e-01
## A.said                                                        NA
## PubDate.last100.log                                    7.809e-03
## `PubDate.date.fctr(7,13]`                              7.792e-03
## `PubDate.date.fctr(13,19]`                            -1.212e-01
## `PubDate.date.fctr(19,25]`                            -6.261e-02
## `PubDate.date.fctr(25,31]`                             1.529e-01
## `PubDate.second.fctr(14.8,29.5]`                       2.721e-02
## `PubDate.second.fctr(29.5,44.2]`                       1.665e-02
## `PubDate.second.fctr(44.2,59.1]`                      -2.527e-01
## S.presid                                               8.691e-02
## A.presid                                                      NA
## S.take                                                -2.048e-01
## A.take                                                        NA
## `PubDate.minute.fctr(14.8,29.5]`                      -1.478e-01
## `PubDate.minute.fctr(29.5,44.2]`                      -3.364e-01
## `PubDate.minute.fctr(44.2,59.1]`                       2.001e-03
## S.new                                                  9.740e+00
## A.new                                                 -9.663e+00
## PubDate.wkday.fctr1                                   -5.858e-01
## PubDate.wkday.fctr2                                   -1.123e+00
## PubDate.wkday.fctr3                                   -7.650e-01
## PubDate.wkday.fctr4                                   -9.723e-01
## PubDate.wkday.fctr5                                   -9.121e-01
## PubDate.wkday.fctr6                                   -1.191e+00
## S.day                                                 -5.034e-03
## A.day                                                         NA
## H.X2014                                               -2.651e-01
## S.show                                                -2.537e-01
## A.show                                                        NA
## S.report                                              -7.517e-01
## A.report                                                      NA
## S.share                                               -5.393e-01
## A.share                                                       NA
## S.year                                                -3.445e-01
## A.year                                                        NA
## S.compani                                             -2.773e-01
## A.compani                                                     NA
## H.new                                                 -5.485e-01
## S.first                                               -1.499e-01
## A.first                                                       NA
## S.time                                                -2.567e-01
## A.time                                                        NA
## H.newyork                                              1.464e-01
## S.articl                                              -4.168e-01
## A.articl                                                      NA
## S.will                                                 3.644e-01
## A.will                                                -6.930e-01
## H.day                                                 -8.718e-01
## S.newyork                                              3.642e-01
## A.newyork                                                     NA
## H.today                                               -2.456e+00
## H.report                                              -8.198e-01
## S.intern                                              -8.781e-01
## A.intern                                                      NA
## H.week                                                -7.435e-01
## H.fashion                                              6.597e-01
## S.week                                                 6.546e-05
## A.week                                                        NA
## S.fashion                                             -1.438e+01
## A.fashion                                                     NA
## H.num.chars.log                                        1.408e-01
## H.num.words.log                                        1.087e+00
## H.num.words.unq.log                                   -1.763e+00
## A.num.chars.log                                        8.026e-01
## S.num.chars.log                                       -4.142e-01
## A.num.words.log                                       -1.025e+02
## S.num.words.log                                        1.034e+02
## A.num.words.unq.log                                    8.437e+01
## S.num.words.unq.log                                   -8.656e+01
##                                                       Std. Error z value
## (Intercept)                                            2.101e+00  -2.715
## WordCount.log                                          8.622e-02  12.951
## `PubDate.hour.fctr(7.67,15.3]`                         2.339e-01   0.856
## `PubDate.hour.fctr(15.3,23]`                           2.374e-01   1.664
## H.is.question                                          2.123e-01   5.993
## PubDate.wkend                                          4.360e-01  -0.951
## PubDate.last10.log                                     1.322e-01   2.042
## PubDate.last1.log                                      4.294e-02  -0.770
## A.can                                                  1.075e+04   0.003
## S.can                                                  1.075e+04  -0.003
## H.has.ebola                                            4.393e-01  -1.170
## S.make                                                 2.749e-01  -0.648
## A.make                                                        NA      NA
## .rnorm                                                 6.145e-02  -0.049
## `myCategory.fctrForeign#World#Asia Pacific`            6.543e-01  -6.864
## `myCategory.fctr#Multimedia#`                          7.841e-01  -5.783
## `myCategory.fctrCulture#Arts#`                         3.270e-01  -9.653
## `myCategory.fctrBusiness#Business Day#Dealbook`        2.829e-01  -9.909
## myCategory.fctrmyOther                                 1.848e+03  -0.011
## `myCategory.fctrBusiness#Technology#`                  3.094e-01  -6.262
## `myCategory.fctrBusiness#Crosswords/Games#`            4.695e-01   1.044
## `myCategory.fctrTStyle##`                              4.904e-01  -8.797
## `myCategory.fctrForeign#World#`                        8.934e+02  -0.021
## `myCategory.fctrOpEd#Opinion#`                         2.807e-01   2.008
## `myCategory.fctrStyles##Fashion`                       1.047e+03  -0.019
## `myCategory.fctr#Opinion#Room For Debate`              5.870e-01  -9.558
## `myCategory.fctr#U.S.#Education`                       6.033e+02  -0.034
## `myCategory.fctr##`                                    2.663e-01 -10.272
## `myCategory.fctrMetro#N.Y. / Region#`                  4.249e-01  -4.437
## `myCategory.fctrBusiness#Business Day#Small Business`  6.671e-01  -6.463
## `myCategory.fctrStyles#U.S.#`                          3.203e-01  -0.459
## `myCategory.fctrTravel#Travel#`                        1.039e+00  -4.164
## `myCategory.fctr#Opinion#The Public Editor`            1.170e+00   0.915
## S.one                                                  1.523e+04   0.000
## S.state                                                1.075e+04   0.001
## A.state                                                1.075e+04   0.000
## A.one                                                  1.523e+04   0.000
## S.said                                                 2.653e-01   2.174
## A.said                                                        NA      NA
## PubDate.last100.log                                    4.451e-02   0.175
## `PubDate.date.fctr(7,13]`                              1.911e-01   0.041
## `PubDate.date.fctr(13,19]`                             1.896e-01  -0.639
## `PubDate.date.fctr(19,25]`                             1.856e-01  -0.337
## `PubDate.date.fctr(25,31]`                             1.975e-01   0.774
## `PubDate.second.fctr(14.8,29.5]`                       1.688e-01   0.161
## `PubDate.second.fctr(29.5,44.2]`                       1.657e-01   0.100
## `PubDate.second.fctr(44.2,59.1]`                       1.728e-01  -1.462
## S.presid                                               3.001e-01   0.290
## A.presid                                                      NA      NA
## S.take                                                 3.799e-01  -0.539
## A.take                                                        NA      NA
## `PubDate.minute.fctr(14.8,29.5]`                       1.735e-01  -0.852
## `PubDate.minute.fctr(29.5,44.2]`                       1.702e-01  -1.976
## `PubDate.minute.fctr(44.2,59.1]`                       1.769e-01   0.011
## S.new                                                  1.075e+04   0.001
## A.new                                                  1.075e+04  -0.001
## PubDate.wkday.fctr1                                    5.046e-01  -1.161
## PubDate.wkday.fctr2                                    5.510e-01  -2.038
## PubDate.wkday.fctr3                                    5.412e-01  -1.414
## PubDate.wkday.fctr4                                    5.354e-01  -1.816
## PubDate.wkday.fctr5                                    5.421e-01  -1.682
## PubDate.wkday.fctr6                                    4.471e-01  -2.663
## S.day                                                  4.071e-01  -0.012
## A.day                                                         NA      NA
## H.X2014                                                8.563e-01  -0.310
## S.show                                                 4.006e-01  -0.633
## A.show                                                        NA      NA
## S.report                                               4.033e-01  -1.864
## A.report                                                      NA      NA
## S.share                                                4.204e-01  -1.283
## A.share                                                       NA      NA
## S.year                                                 2.958e-01  -1.165
## A.year                                                        NA      NA
## S.compani                                              2.673e-01  -1.037
## A.compani                                                     NA      NA
## H.new                                                  4.227e-01  -1.298
## S.first                                                4.203e-01  -0.357
## A.first                                                       NA      NA
## S.time                                                 2.976e-01  -0.862
## A.time                                                        NA      NA
## H.newyork                                              4.704e-01   0.311
## S.articl                                               7.601e-01  -0.548
## A.articl                                                      NA      NA
## S.will                                                 1.079e+04   0.000
## A.will                                                 1.079e+04   0.000
## H.day                                                  7.091e-01  -1.230
## S.newyork                                              3.313e-01   1.099
## A.newyork                                                     NA      NA
## H.today                                                6.520e-01  -3.766
## H.report                                               6.043e-01  -1.357
## S.intern                                               8.408e-01  -1.044
## A.intern                                                      NA      NA
## H.week                                                 6.388e-01  -1.164
## H.fashion                                              1.230e+00   0.536
## S.week                                                 3.193e-01   0.000
## A.week                                                        NA      NA
## S.fashion                                              6.830e+02  -0.021
## A.fashion                                                     NA      NA
## H.num.chars.log                                        3.574e-01   0.394
## H.num.words.log                                        2.005e+00   0.542
## H.num.words.unq.log                                    1.971e+00  -0.894
## A.num.chars.log                                        1.696e+01   0.047
## S.num.chars.log                                        1.697e+01  -0.024
## A.num.words.log                                        1.454e+02  -0.705
## S.num.words.log                                        1.453e+02   0.711
## A.num.words.unq.log                                    1.479e+02   0.570
## S.num.words.unq.log                                    1.479e+02  -0.585
##                                                       Pr(>|z|)    
## (Intercept)                                           0.006621 ** 
## WordCount.log                                          < 2e-16 ***
## `PubDate.hour.fctr(7.67,15.3]`                        0.391877    
## `PubDate.hour.fctr(15.3,23]`                          0.096093 .  
## H.is.question                                         2.06e-09 ***
## PubDate.wkend                                         0.341749    
## PubDate.last10.log                                    0.041120 *  
## PubDate.last1.log                                     0.441313    
## A.can                                                 0.997924    
## S.can                                                 0.997904    
## H.has.ebola                                           0.241819    
## S.make                                                0.516971    
## A.make                                                      NA    
## .rnorm                                                0.960653    
## `myCategory.fctrForeign#World#Asia Pacific`           6.72e-12 ***
## `myCategory.fctr#Multimedia#`                         7.35e-09 ***
## `myCategory.fctrCulture#Arts#`                         < 2e-16 ***
## `myCategory.fctrBusiness#Business Day#Dealbook`        < 2e-16 ***
## myCategory.fctrmyOther                                0.991298    
## `myCategory.fctrBusiness#Technology#`                 3.80e-10 ***
## `myCategory.fctrBusiness#Crosswords/Games#`           0.296311    
## `myCategory.fctrTStyle##`                              < 2e-16 ***
## `myCategory.fctrForeign#World#`                       0.983481    
## `myCategory.fctrOpEd#Opinion#`                        0.044673 *  
## `myCategory.fctrStyles##Fashion`                      0.984886    
## `myCategory.fctr#Opinion#Room For Debate`              < 2e-16 ***
## `myCategory.fctr#U.S.#Education`                      0.972997    
## `myCategory.fctr##`                                    < 2e-16 ***
## `myCategory.fctrMetro#N.Y. / Region#`                 9.11e-06 ***
## `myCategory.fctrBusiness#Business Day#Small Business` 1.03e-10 ***
## `myCategory.fctrStyles#U.S.#`                         0.646333    
## `myCategory.fctrTravel#Travel#`                       3.13e-05 ***
## `myCategory.fctr#Opinion#The Public Editor`           0.360093    
## S.one                                                 0.999882    
## S.state                                               0.999564    
## A.state                                               0.999612    
## A.one                                                 0.999876    
## S.said                                                0.029718 *  
## A.said                                                      NA    
## PubDate.last100.log                                   0.860739    
## `PubDate.date.fctr(7,13]`                             0.967476    
## `PubDate.date.fctr(13,19]`                            0.522577    
## `PubDate.date.fctr(19,25]`                            0.735827    
## `PubDate.date.fctr(25,31]`                            0.438857    
## `PubDate.second.fctr(14.8,29.5]`                      0.871934    
## `PubDate.second.fctr(29.5,44.2]`                      0.919977    
## `PubDate.second.fctr(44.2,59.1]`                      0.143626    
## S.presid                                              0.772121    
## A.presid                                                    NA    
## S.take                                                0.589795    
## A.take                                                      NA    
## `PubDate.minute.fctr(14.8,29.5]`                      0.394233    
## `PubDate.minute.fctr(29.5,44.2]`                      0.048179 *  
## `PubDate.minute.fctr(44.2,59.1]`                      0.990977    
## S.new                                                 0.999277    
## A.new                                                 0.999283    
## PubDate.wkday.fctr1                                   0.245646    
## PubDate.wkday.fctr2                                   0.041540 *  
## PubDate.wkday.fctr3                                   0.157498    
## PubDate.wkday.fctr4                                   0.069356 .  
## PubDate.wkday.fctr5                                   0.092488 .  
## PubDate.wkday.fctr6                                   0.007745 ** 
## S.day                                                 0.990135    
## A.day                                                       NA    
## H.X2014                                               0.756908    
## S.show                                                0.526616    
## A.show                                                      NA    
## S.report                                              0.062365 .  
## A.report                                                    NA    
## S.share                                               0.199503    
## A.share                                                     NA    
## S.year                                                0.244195    
## A.year                                                      NA    
## S.compani                                             0.299657    
## A.compani                                                   NA    
## H.new                                                 0.194353    
## S.first                                               0.721245    
## A.first                                                     NA    
## S.time                                                0.388427    
## A.time                                                      NA    
## H.newyork                                             0.755691    
## S.articl                                              0.583433    
## A.articl                                                    NA    
## S.will                                                0.999973    
## A.will                                                0.999949    
## H.day                                                 0.218867    
## S.newyork                                             0.271643    
## A.newyork                                                   NA    
## H.today                                               0.000166 ***
## H.report                                              0.174899    
## S.intern                                              0.296329    
## A.intern                                                    NA    
## H.week                                                0.244493    
## H.fashion                                             0.591815    
## S.week                                                0.999836    
## A.week                                                      NA    
## S.fashion                                             0.983199    
## A.fashion                                                   NA    
## H.num.chars.log                                       0.693535    
## H.num.words.log                                       0.587752    
## H.num.words.unq.log                                   0.371142    
## A.num.chars.log                                       0.962257    
## S.num.chars.log                                       0.980529    
## A.num.words.log                                       0.480679    
## S.num.words.log                                       0.476964    
## A.num.words.unq.log                                   0.568448    
## S.num.words.unq.log                                   0.558360    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 1939.9  on 4385  degrees of freedom
## AIC: 2119.9
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

![](NYTBlogs_category_files/figure-html/fit.models_1-4.png) ![](NYTBlogs_category_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6529851
## 3        0.2 0.7251732
## 4        0.3 0.7339332
## 5        0.4 0.7283951
## 6        0.5 0.7203451
## 7        0.6 0.6933128
## 8        0.7 0.6468647
## 9        0.8 0.5541284
## 10       0.9 0.3797468
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3490
## 2            Y                                      178
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      236
## 2                                      571
##          Prediction
## Reference    N    Y
##         N 3490  236
##         Y  178  571
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.074860e-01   6.780360e-01   8.986168e-01   9.158192e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   2.373225e-47   5.088185e-03 
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

![](NYTBlogs_category_files/figure-html/fit.models_1-6.png) ![](NYTBlogs_category_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6272912
## 3        0.2 0.7025641
## 4        0.3 0.7138810
## 5        0.4 0.7187970
## 6        0.5 0.7115987
## 7        0.6 0.6711636
## 8        0.7 0.6363636
## 9        0.8 0.5171717
## 10       0.9 0.3411215
## 11       1.0 0.0000000
```

![](NYTBlogs_category_files/figure-html/fit.models_1-8.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1631
## 2            Y                                      105
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       82
## 2                                      239
##          Prediction
## Reference    N    Y
##         N 1631   82
##         Y  105  239
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.090909e-01   6.646557e-01   8.958343e-01   9.211676e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.165728e-23   1.076602e-01 
##            model_id model_method
## 1 Conditional.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, .rnorm, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      9.244                 2.655
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9432269                    0.3       0.7339332        0.9056969
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8986168             0.9158192     0.6418929   0.9250838
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4        0.718797        0.9090909
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8958343             0.9211676     0.6646557     2119.87
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.005815118      0.01887435
## [1] "fitting model: Conditional.X.no.rnorm.rpart"
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
## + Fold1: cp=0.01135 
## - Fold1: cp=0.01135 
## + Fold2: cp=0.01135 
## - Fold2: cp=0.01135 
## + Fold3: cp=0.01135 
## - Fold3: cp=0.01135 
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0113 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](NYTBlogs_category_files/figure-html/fit.models_1-9.png) ![](NYTBlogs_category_files/figure-html/fit.models_1-10.png) 

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
##                       A.num.words.unq.log 
##                                         6 
##                           A.num.words.log 
##                                         6 
##                       S.num.words.unq.log 
##                                         6 
##                           S.num.words.log 
##                                         6 
##                           A.num.chars.log 
##                                         6 
##                           H.num.chars.log 
##                                         1 
## 
## Node number 1: 4475 observations,    complexity param=0.271028
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (4106 obs) right son=3 (369 obs)
##   Primary splits:
##       myCategory.fctrOpEd#Opinion#              < 0.5      to the left,  improve=297.02950, (0 missing)
##       WordCount.log                             < 6.524296 to the left,  improve=104.85530, (0 missing)
##       myCategory.fctrBusiness#Crosswords/Games# < 0.5      to the left,  improve= 85.77765, (0 missing)
##       A.num.chars.log                           < 3.795426 to the right, improve= 77.76207, (0 missing)
##       S.num.chars.log                           < 3.795426 to the right, improve= 77.76207, (0 missing)
##   Surrogate splits:
##       A.num.words.unq.log < 1.497866 to the right, agree=0.928, adj=0.127, (0 split)
##       A.num.words.log     < 1.497866 to the right, agree=0.928, adj=0.125, (0 split)
##       S.num.words.unq.log < 1.497866 to the right, agree=0.928, adj=0.125, (0 split)
##       S.num.words.log     < 1.497866 to the right, agree=0.928, adj=0.122, (0 split)
##       A.num.chars.log     < 3.725621 to the right, agree=0.927, adj=0.117, (0 split)
## 
## Node number 2: 4106 observations,    complexity param=0.08411215
##   predicted class=N  expected loss=0.1127618  P(node) =0.9175419
##     class counts:  3643   463
##    probabilities: 0.887 0.113 
##   left son=4 (4023 obs) right son=5 (83 obs)
##   Primary splits:
##       myCategory.fctrBusiness#Crosswords/Games# < 0.5      to the left,  improve=99.60741, (0 missing)
##       WordCount.log                             < 6.470025 to the left,  improve=94.06998, (0 missing)
##       myCategory.fctrStyles#U.S.#               < 0.5      to the left,  improve=50.94648, (0 missing)
##       H.num.chars.log                           < 2.861793 to the right, improve=21.22589, (0 missing)
##       H.num.words.log                           < 1.242453 to the right, improve=15.33310, (0 missing)
##   Surrogate splits:
##       H.num.chars.log < 2.35024  to the right, agree=0.981, adj=0.06, (0 split)
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

![](NYTBlogs_category_files/figure-html/fit.models_1-11.png) 

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

![](NYTBlogs_category_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rpart.N
## 1            N                                                3633
## 2            Y                                                 390
##   Popular.fctr.predict.Conditional.X.no.rnorm.rpart.Y
## 1                                                  93
## 2                                                 359
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

![](NYTBlogs_category_files/figure-html/fit.models_1-13.png) 

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
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rpart.N
## 1            N                                                1671
## 2            Y                                                 192
##   Popular.fctr.predict.Conditional.X.no.rnorm.rpart.Y
## 1                                                  42
## 2                                                 152
##          Prediction
## Reference    N    Y
##         N 1671   42
##         Y  192  152
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.862421e-01   5.054039e-01   8.717239e-01   8.996488e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.783557e-12   2.026854e-22 
##                       model_id model_method
## 1 Conditional.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      7.071                 1.232
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
## [1] "fitting model: Conditional.X.no.rnorm.rf"
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

![](NYTBlogs_category_files/figure-html/fit.models_1-14.png) 

```
## + : mtry=  2 
## - : mtry=  2 
## + : mtry= 53 
## - : mtry= 53 
## + : mtry=105 
## - : mtry=105 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 53 on full training set
```

![](NYTBlogs_category_files/figure-html/fit.models_1-15.png) ![](NYTBlogs_category_files/figure-html/fit.models_1-16.png) 

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
## importance       105   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           105   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_category_files/figure-html/fit.models_1-17.png) 

```
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.79133650
## 3        0.2 0.92812887
## 4        0.3 0.98358503
## 5        0.4 0.99933289
## 6        0.5 1.00000000
## 7        0.6 0.99866310
## 8        0.7 0.91763006
## 9        0.8 0.78211382
## 10       0.9 0.56099904
## 11       1.0 0.02113606
```

![](NYTBlogs_category_files/figure-html/fit.models_1-18.png) 

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

![](NYTBlogs_category_files/figure-html/fit.models_1-19.png) 

```
##    threshold     f.score
## 1        0.0 0.286547272
## 2        0.1 0.570390554
## 3        0.2 0.627802691
## 4        0.3 0.665789474
## 5        0.4 0.667664671
## 6        0.5 0.662358643
## 7        0.6 0.622144112
## 8        0.7 0.574181118
## 9        0.8 0.444933921
## 10       0.9 0.237974684
## 11       1.0 0.005797101
```

![](NYTBlogs_category_files/figure-html/fit.models_1-20.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1612
## 2            Y                                              121
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                              101
## 2                                              223
##          Prediction
## Reference    N    Y
##         N 1612  101
##         Y  121  223
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.920758e-01   6.033111e-01   8.778610e-01   9.051619e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.761945e-14   2.022397e-01 
##                    model_id model_method
## 1 Conditional.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     161.07                38.778
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.9030168
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.999176                     1     0.6191321   0.9174736
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.6676647        0.8920758
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.877861             0.9051619     0.6033111
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   WordCount.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   WordCount.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   WordCount.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   WordCount.log
## 7                                                                                                                                                                                                                                                                               WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
## 8                                                                                                                                                                                                                                                                 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, S.can, H.has.ebola, S.make, .rnorm, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
## 9  WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, .rnorm, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 10         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 11         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##    max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1                0                      0.661                 0.002
## 2                0                      0.339                 0.002
## 3                0                      0.715                 0.074
## 4                0                      0.598                 0.061
## 5                3                      1.302                 0.073
## 6                1                      1.172                 0.077
## 7                1                      2.572                 0.502
## 8                1                      6.661                 1.855
## 9                1                      9.244                 2.655
## 10               3                      7.071                 1.232
## 11               3                    161.070                38.778
##    max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.5000000                    0.5       0.0000000        0.8326257
## 2    0.5007516                    0.1       0.2867534        0.1673743
## 3    0.5000000                    0.5       0.0000000        0.8326257
## 4    0.7667409                    0.2       0.4618076        0.7937430
## 5    0.5000000                    0.5       0.0000000        0.8214525
## 6    0.7314944                    0.2       0.4150268        0.8308379
## 7    0.8262824                    0.2       0.5209397        0.8505038
## 8    0.9427059                    0.3       0.7342747        0.9074844
## 9    0.9432269                    0.3       0.7339332        0.9056969
## 10   0.7277461                    0.7       0.5978351        0.8934084
## 11   1.0000000                    0.5       1.0000000        0.9030168
##    max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.8213602             0.8434553    0.00000000   0.5000000
## 2              0.1565447             0.1786398    0.00000000   0.4909227
## 3              0.8213602             0.8434553    0.00000000   0.5000000
## 4              0.7815865             0.8055149    0.33675035   0.6722244
## 5              0.8213602             0.8434553    0.07605123   0.5000000
## 6              0.6941397             0.7210078    0.01207987   0.7289876
## 7              0.7781661             0.8022358    0.30598758   0.8094734
## 8              0.8986168             0.9158192    0.64751910   0.9245425
## 9              0.8986168             0.9158192    0.64189293   0.9250838
## 10             0.8826068             0.9010121    0.55666590   0.7084504
## 11             0.9991760             1.0000000    0.61913206   0.9174736
##    opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                     0.5       0.0000000        0.8327662
## 2                     0.1       0.2865473        0.1672338
## 3                     0.5       0.0000000        0.8327662
## 4                     0.2       0.3763838        0.7535246
## 5                     0.5       0.0000000        0.8327662
## 6                     0.2       0.4061896        0.7015070
## 7                     0.2       0.5061315        0.7846378
## 8                     0.3       0.7128713        0.9013126
## 9                     0.4       0.7187970        0.9090909
## 10                    0.7       0.5650558        0.8862421
## 11                    0.4       0.6676647        0.8920758
##    max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.8159247             0.8486533     0.0000000
## 2              0.1513467             0.1840753     0.0000000
## 3              0.8159247             0.8486533     0.0000000
## 4              0.7343044             0.7720195     0.2272928
## 5              0.8159247             0.8486533     0.0000000
## 6              0.6812119             0.7212263     0.2355742
## 7              0.7662310             0.8022297     0.3778434
## 8              0.8876034             0.9138653     0.6533400
## 9              0.8958343             0.9211676     0.6646557
## 10             0.8717239             0.8996488     0.5054039
## 11             0.8778610             0.9051619     0.6033111
##    max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## 1                  NA              NA          NA
## 2                  NA              NA          NA
## 3                  NA              NA          NA
## 4                  NA              NA          NA
## 5        0.0077087794     0.001192306          NA
## 6        0.0008087605     0.009862351    3664.292
## 7        0.0050153091     0.035413484    3178.524
## 8        0.0065017755     0.020926900    2111.473
## 9        0.0058151176     0.018874350    2119.870
## 10       0.0030411362     0.029222934          NA
## 11                 NA              NA          NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          6          1 185.620 385.057 199.437
## 11 fit.models          6          2 385.057      NA      NA
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   WordCount.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   WordCount.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   WordCount.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   WordCount.log
## 7                                                                                                                                                                                                                                                                               WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
## 8                                                                                                                                                                                                                                                                 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, S.can, H.has.ebola, S.make, .rnorm, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
## 9  WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, .rnorm, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 10         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 11         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, myCategory.fctr, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##    max.nTuningRuns max.auc.fit opt.prob.threshold.fit max.f.score.fit
## 1                0   0.5000000                    0.5       0.0000000
## 2                0   0.5007516                    0.1       0.2867534
## 3                0   0.5000000                    0.5       0.0000000
## 4                0   0.7667409                    0.2       0.4618076
## 5                3   0.5000000                    0.5       0.0000000
## 6                1   0.7314944                    0.2       0.4150268
## 7                1   0.8262824                    0.2       0.5209397
## 8                1   0.9427059                    0.3       0.7342747
## 9                1   0.9432269                    0.3       0.7339332
## 10               3   0.7277461                    0.7       0.5978351
## 11               3   1.0000000                    0.5       1.0000000
##    max.Accuracy.fit max.Kappa.fit max.auc.OOB opt.prob.threshold.OOB
## 1         0.8326257    0.00000000   0.5000000                    0.5
## 2         0.1673743    0.00000000   0.4909227                    0.1
## 3         0.8326257    0.00000000   0.5000000                    0.5
## 4         0.7937430    0.33675035   0.6722244                    0.2
## 5         0.8214525    0.07605123   0.5000000                    0.5
## 6         0.8308379    0.01207987   0.7289876                    0.2
## 7         0.8505038    0.30598758   0.8094734                    0.2
## 8         0.9074844    0.64751910   0.9245425                    0.3
## 9         0.9056969    0.64189293   0.9250838                    0.4
## 10        0.8934084    0.55666590   0.7084504                    0.7
## 11        0.9030168    0.61913206   0.9174736                    0.4
##    max.f.score.OOB max.Accuracy.OOB max.Kappa.OOB
## 1        0.0000000        0.8327662     0.0000000
## 2        0.2865473        0.1672338     0.0000000
## 3        0.0000000        0.8327662     0.0000000
## 4        0.3763838        0.7535246     0.2272928
## 5        0.0000000        0.8327662     0.0000000
## 6        0.4061896        0.7015070     0.2355742
## 7        0.5061315        0.7846378     0.3778434
## 8        0.7128713        0.9013126     0.6533400
## 9        0.7187970        0.9090909     0.6646557
## 10       0.5650558        0.8862421     0.5054039
## 11       0.6676647        0.8920758     0.6033111
##    inv.elapsedtime.everything inv.elapsedtime.final  inv.aic.fit
## 1                 1.512859304          500.00000000           NA
## 2                 2.949852507          500.00000000           NA
## 3                 1.398601399           13.51351351           NA
## 4                 1.672240803           16.39344262           NA
## 5                 0.768049155           13.69863014           NA
## 6                 0.853242321           12.98701299 0.0002729040
## 7                 0.388802488            1.99203187 0.0003146114
## 8                 0.150127608            0.53908356 0.0004736031
## 9                 0.108178278            0.37664783 0.0004717270
## 10                0.141422712            0.81168831           NA
## 11                0.006208481            0.02578782           NA
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

![](NYTBlogs_category_files/figure-html/fit.models_2-1.png) 

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

![](NYTBlogs_category_files/figure-html/fit.models_2-2.png) 

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
## 9             Conditional.X.glm        0.9090909   0.9250838     0.6646557
## 8                 Low.cor.X.glm        0.9013126   0.9245425     0.6533400
## 11    Conditional.X.no.rnorm.rf        0.8920758   0.9174736     0.6033111
## 10 Conditional.X.no.rnorm.rpart        0.8862421   0.7084504     0.5054039
## 1             MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3          Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 5               Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 7       Interact.High.cor.Y.glm        0.7846378   0.8094734     0.3778434
## 4     Max.cor.Y.cv.0.cp.0.rpart        0.7535246   0.6722244     0.2272928
## 6                 Max.cor.Y.glm        0.7015070   0.7289876     0.2355742
## 2       Random.myrandom_classfr        0.1672338   0.4909227     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 9     2119.870                    0.4
## 8     2111.473                    0.3
## 11          NA                    0.4
## 10          NA                    0.7
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 7     3178.524                    0.2
## 4           NA                    0.2
## 6     3664.292                    0.2
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

![](NYTBlogs_category_files/figure-html/fit.models_2-3.png) 

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
##   1143, 1930, 3625, 3799, 4408
```

![](NYTBlogs_category_files/figure-html/fit.models_2-4.png) ![](NYTBlogs_category_files/figure-html/fit.models_2-5.png) 

```
## Warning: not plotting observations with leverage one:
##   1143, 1930, 3625, 3799, 4408
```

![](NYTBlogs_category_files/figure-html/fit.models_2-6.png) ![](NYTBlogs_category_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.7676  -0.3432  -0.1717  -0.0001   3.3071  
## 
## Coefficients: (17 not defined because of singularities)
##                                                         Estimate
## (Intercept)                                           -5.705e+00
## WordCount.log                                          1.117e+00
## `PubDate.hour.fctr(7.67,15.3]`                         2.002e-01
## `PubDate.hour.fctr(15.3,23]`                           3.951e-01
## H.is.question                                          1.273e+00
## PubDate.wkend                                         -4.145e-01
## PubDate.last10.log                                     2.700e-01
## PubDate.last1.log                                     -3.307e-02
## A.can                                                  2.798e+01
## S.can                                                 -2.825e+01
## H.has.ebola                                           -5.142e-01
## S.make                                                -1.781e-01
## A.make                                                        NA
## .rnorm                                                -3.031e-03
## `myCategory.fctrForeign#World#Asia Pacific`           -4.491e+00
## `myCategory.fctr#Multimedia#`                         -4.534e+00
## `myCategory.fctrCulture#Arts#`                        -3.156e+00
## `myCategory.fctrBusiness#Business Day#Dealbook`       -2.803e+00
## myCategory.fctrmyOther                                -2.015e+01
## `myCategory.fctrBusiness#Technology#`                 -1.937e+00
## `myCategory.fctrBusiness#Crosswords/Games#`            4.903e-01
## `myCategory.fctrTStyle##`                             -4.315e+00
## `myCategory.fctrForeign#World#`                       -1.850e+01
## `myCategory.fctrOpEd#Opinion#`                         5.636e-01
## `myCategory.fctrStyles##Fashion`                      -1.984e+01
## `myCategory.fctr#Opinion#Room For Debate`             -5.611e+00
## `myCategory.fctr#U.S.#Education`                      -2.042e+01
## `myCategory.fctr##`                                   -2.736e+00
## `myCategory.fctrMetro#N.Y. / Region#`                 -1.886e+00
## `myCategory.fctrBusiness#Business Day#Small Business` -4.311e+00
## `myCategory.fctrStyles#U.S.#`                         -1.470e-01
## `myCategory.fctrTravel#Travel#`                       -4.326e+00
## `myCategory.fctr#Opinion#The Public Editor`            1.071e+00
## S.one                                                  2.256e+00
## S.state                                                5.874e+00
## A.state                                               -5.235e+00
## A.one                                                 -2.368e+00
## S.said                                                 5.767e-01
## A.said                                                        NA
## PubDate.last100.log                                    7.809e-03
## `PubDate.date.fctr(7,13]`                              7.792e-03
## `PubDate.date.fctr(13,19]`                            -1.212e-01
## `PubDate.date.fctr(19,25]`                            -6.261e-02
## `PubDate.date.fctr(25,31]`                             1.529e-01
## `PubDate.second.fctr(14.8,29.5]`                       2.721e-02
## `PubDate.second.fctr(29.5,44.2]`                       1.665e-02
## `PubDate.second.fctr(44.2,59.1]`                      -2.527e-01
## S.presid                                               8.691e-02
## A.presid                                                      NA
## S.take                                                -2.048e-01
## A.take                                                        NA
## `PubDate.minute.fctr(14.8,29.5]`                      -1.478e-01
## `PubDate.minute.fctr(29.5,44.2]`                      -3.364e-01
## `PubDate.minute.fctr(44.2,59.1]`                       2.001e-03
## S.new                                                  9.740e+00
## A.new                                                 -9.663e+00
## PubDate.wkday.fctr1                                   -5.858e-01
## PubDate.wkday.fctr2                                   -1.123e+00
## PubDate.wkday.fctr3                                   -7.650e-01
## PubDate.wkday.fctr4                                   -9.723e-01
## PubDate.wkday.fctr5                                   -9.121e-01
## PubDate.wkday.fctr6                                   -1.191e+00
## S.day                                                 -5.034e-03
## A.day                                                         NA
## H.X2014                                               -2.651e-01
## S.show                                                -2.537e-01
## A.show                                                        NA
## S.report                                              -7.517e-01
## A.report                                                      NA
## S.share                                               -5.393e-01
## A.share                                                       NA
## S.year                                                -3.445e-01
## A.year                                                        NA
## S.compani                                             -2.773e-01
## A.compani                                                     NA
## H.new                                                 -5.485e-01
## S.first                                               -1.499e-01
## A.first                                                       NA
## S.time                                                -2.567e-01
## A.time                                                        NA
## H.newyork                                              1.464e-01
## S.articl                                              -4.168e-01
## A.articl                                                      NA
## S.will                                                 3.644e-01
## A.will                                                -6.930e-01
## H.day                                                 -8.718e-01
## S.newyork                                              3.642e-01
## A.newyork                                                     NA
## H.today                                               -2.456e+00
## H.report                                              -8.198e-01
## S.intern                                              -8.781e-01
## A.intern                                                      NA
## H.week                                                -7.435e-01
## H.fashion                                              6.597e-01
## S.week                                                 6.546e-05
## A.week                                                        NA
## S.fashion                                             -1.438e+01
## A.fashion                                                     NA
## H.num.chars.log                                        1.408e-01
## H.num.words.log                                        1.087e+00
## H.num.words.unq.log                                   -1.763e+00
## A.num.chars.log                                        8.026e-01
## S.num.chars.log                                       -4.142e-01
## A.num.words.log                                       -1.025e+02
## S.num.words.log                                        1.034e+02
## A.num.words.unq.log                                    8.437e+01
## S.num.words.unq.log                                   -8.656e+01
##                                                       Std. Error z value
## (Intercept)                                            2.101e+00  -2.715
## WordCount.log                                          8.622e-02  12.951
## `PubDate.hour.fctr(7.67,15.3]`                         2.339e-01   0.856
## `PubDate.hour.fctr(15.3,23]`                           2.374e-01   1.664
## H.is.question                                          2.123e-01   5.993
## PubDate.wkend                                          4.360e-01  -0.951
## PubDate.last10.log                                     1.322e-01   2.042
## PubDate.last1.log                                      4.294e-02  -0.770
## A.can                                                  1.075e+04   0.003
## S.can                                                  1.075e+04  -0.003
## H.has.ebola                                            4.393e-01  -1.170
## S.make                                                 2.749e-01  -0.648
## A.make                                                        NA      NA
## .rnorm                                                 6.145e-02  -0.049
## `myCategory.fctrForeign#World#Asia Pacific`            6.543e-01  -6.864
## `myCategory.fctr#Multimedia#`                          7.841e-01  -5.783
## `myCategory.fctrCulture#Arts#`                         3.270e-01  -9.653
## `myCategory.fctrBusiness#Business Day#Dealbook`        2.829e-01  -9.909
## myCategory.fctrmyOther                                 1.848e+03  -0.011
## `myCategory.fctrBusiness#Technology#`                  3.094e-01  -6.262
## `myCategory.fctrBusiness#Crosswords/Games#`            4.695e-01   1.044
## `myCategory.fctrTStyle##`                              4.904e-01  -8.797
## `myCategory.fctrForeign#World#`                        8.934e+02  -0.021
## `myCategory.fctrOpEd#Opinion#`                         2.807e-01   2.008
## `myCategory.fctrStyles##Fashion`                       1.047e+03  -0.019
## `myCategory.fctr#Opinion#Room For Debate`              5.870e-01  -9.558
## `myCategory.fctr#U.S.#Education`                       6.033e+02  -0.034
## `myCategory.fctr##`                                    2.663e-01 -10.272
## `myCategory.fctrMetro#N.Y. / Region#`                  4.249e-01  -4.437
## `myCategory.fctrBusiness#Business Day#Small Business`  6.671e-01  -6.463
## `myCategory.fctrStyles#U.S.#`                          3.203e-01  -0.459
## `myCategory.fctrTravel#Travel#`                        1.039e+00  -4.164
## `myCategory.fctr#Opinion#The Public Editor`            1.170e+00   0.915
## S.one                                                  1.523e+04   0.000
## S.state                                                1.075e+04   0.001
## A.state                                                1.075e+04   0.000
## A.one                                                  1.523e+04   0.000
## S.said                                                 2.653e-01   2.174
## A.said                                                        NA      NA
## PubDate.last100.log                                    4.451e-02   0.175
## `PubDate.date.fctr(7,13]`                              1.911e-01   0.041
## `PubDate.date.fctr(13,19]`                             1.896e-01  -0.639
## `PubDate.date.fctr(19,25]`                             1.856e-01  -0.337
## `PubDate.date.fctr(25,31]`                             1.975e-01   0.774
## `PubDate.second.fctr(14.8,29.5]`                       1.688e-01   0.161
## `PubDate.second.fctr(29.5,44.2]`                       1.657e-01   0.100
## `PubDate.second.fctr(44.2,59.1]`                       1.728e-01  -1.462
## S.presid                                               3.001e-01   0.290
## A.presid                                                      NA      NA
## S.take                                                 3.799e-01  -0.539
## A.take                                                        NA      NA
## `PubDate.minute.fctr(14.8,29.5]`                       1.735e-01  -0.852
## `PubDate.minute.fctr(29.5,44.2]`                       1.702e-01  -1.976
## `PubDate.minute.fctr(44.2,59.1]`                       1.769e-01   0.011
## S.new                                                  1.075e+04   0.001
## A.new                                                  1.075e+04  -0.001
## PubDate.wkday.fctr1                                    5.046e-01  -1.161
## PubDate.wkday.fctr2                                    5.510e-01  -2.038
## PubDate.wkday.fctr3                                    5.412e-01  -1.414
## PubDate.wkday.fctr4                                    5.354e-01  -1.816
## PubDate.wkday.fctr5                                    5.421e-01  -1.682
## PubDate.wkday.fctr6                                    4.471e-01  -2.663
## S.day                                                  4.071e-01  -0.012
## A.day                                                         NA      NA
## H.X2014                                                8.563e-01  -0.310
## S.show                                                 4.006e-01  -0.633
## A.show                                                        NA      NA
## S.report                                               4.033e-01  -1.864
## A.report                                                      NA      NA
## S.share                                                4.204e-01  -1.283
## A.share                                                       NA      NA
## S.year                                                 2.958e-01  -1.165
## A.year                                                        NA      NA
## S.compani                                              2.673e-01  -1.037
## A.compani                                                     NA      NA
## H.new                                                  4.227e-01  -1.298
## S.first                                                4.203e-01  -0.357
## A.first                                                       NA      NA
## S.time                                                 2.976e-01  -0.862
## A.time                                                        NA      NA
## H.newyork                                              4.704e-01   0.311
## S.articl                                               7.601e-01  -0.548
## A.articl                                                      NA      NA
## S.will                                                 1.079e+04   0.000
## A.will                                                 1.079e+04   0.000
## H.day                                                  7.091e-01  -1.230
## S.newyork                                              3.313e-01   1.099
## A.newyork                                                     NA      NA
## H.today                                                6.520e-01  -3.766
## H.report                                               6.043e-01  -1.357
## S.intern                                               8.408e-01  -1.044
## A.intern                                                      NA      NA
## H.week                                                 6.388e-01  -1.164
## H.fashion                                              1.230e+00   0.536
## S.week                                                 3.193e-01   0.000
## A.week                                                        NA      NA
## S.fashion                                              6.830e+02  -0.021
## A.fashion                                                     NA      NA
## H.num.chars.log                                        3.574e-01   0.394
## H.num.words.log                                        2.005e+00   0.542
## H.num.words.unq.log                                    1.971e+00  -0.894
## A.num.chars.log                                        1.696e+01   0.047
## S.num.chars.log                                        1.697e+01  -0.024
## A.num.words.log                                        1.454e+02  -0.705
## S.num.words.log                                        1.453e+02   0.711
## A.num.words.unq.log                                    1.479e+02   0.570
## S.num.words.unq.log                                    1.479e+02  -0.585
##                                                       Pr(>|z|)    
## (Intercept)                                           0.006621 ** 
## WordCount.log                                          < 2e-16 ***
## `PubDate.hour.fctr(7.67,15.3]`                        0.391877    
## `PubDate.hour.fctr(15.3,23]`                          0.096093 .  
## H.is.question                                         2.06e-09 ***
## PubDate.wkend                                         0.341749    
## PubDate.last10.log                                    0.041120 *  
## PubDate.last1.log                                     0.441313    
## A.can                                                 0.997924    
## S.can                                                 0.997904    
## H.has.ebola                                           0.241819    
## S.make                                                0.516971    
## A.make                                                      NA    
## .rnorm                                                0.960653    
## `myCategory.fctrForeign#World#Asia Pacific`           6.72e-12 ***
## `myCategory.fctr#Multimedia#`                         7.35e-09 ***
## `myCategory.fctrCulture#Arts#`                         < 2e-16 ***
## `myCategory.fctrBusiness#Business Day#Dealbook`        < 2e-16 ***
## myCategory.fctrmyOther                                0.991298    
## `myCategory.fctrBusiness#Technology#`                 3.80e-10 ***
## `myCategory.fctrBusiness#Crosswords/Games#`           0.296311    
## `myCategory.fctrTStyle##`                              < 2e-16 ***
## `myCategory.fctrForeign#World#`                       0.983481    
## `myCategory.fctrOpEd#Opinion#`                        0.044673 *  
## `myCategory.fctrStyles##Fashion`                      0.984886    
## `myCategory.fctr#Opinion#Room For Debate`              < 2e-16 ***
## `myCategory.fctr#U.S.#Education`                      0.972997    
## `myCategory.fctr##`                                    < 2e-16 ***
## `myCategory.fctrMetro#N.Y. / Region#`                 9.11e-06 ***
## `myCategory.fctrBusiness#Business Day#Small Business` 1.03e-10 ***
## `myCategory.fctrStyles#U.S.#`                         0.646333    
## `myCategory.fctrTravel#Travel#`                       3.13e-05 ***
## `myCategory.fctr#Opinion#The Public Editor`           0.360093    
## S.one                                                 0.999882    
## S.state                                               0.999564    
## A.state                                               0.999612    
## A.one                                                 0.999876    
## S.said                                                0.029718 *  
## A.said                                                      NA    
## PubDate.last100.log                                   0.860739    
## `PubDate.date.fctr(7,13]`                             0.967476    
## `PubDate.date.fctr(13,19]`                            0.522577    
## `PubDate.date.fctr(19,25]`                            0.735827    
## `PubDate.date.fctr(25,31]`                            0.438857    
## `PubDate.second.fctr(14.8,29.5]`                      0.871934    
## `PubDate.second.fctr(29.5,44.2]`                      0.919977    
## `PubDate.second.fctr(44.2,59.1]`                      0.143626    
## S.presid                                              0.772121    
## A.presid                                                    NA    
## S.take                                                0.589795    
## A.take                                                      NA    
## `PubDate.minute.fctr(14.8,29.5]`                      0.394233    
## `PubDate.minute.fctr(29.5,44.2]`                      0.048179 *  
## `PubDate.minute.fctr(44.2,59.1]`                      0.990977    
## S.new                                                 0.999277    
## A.new                                                 0.999283    
## PubDate.wkday.fctr1                                   0.245646    
## PubDate.wkday.fctr2                                   0.041540 *  
## PubDate.wkday.fctr3                                   0.157498    
## PubDate.wkday.fctr4                                   0.069356 .  
## PubDate.wkday.fctr5                                   0.092488 .  
## PubDate.wkday.fctr6                                   0.007745 ** 
## S.day                                                 0.990135    
## A.day                                                       NA    
## H.X2014                                               0.756908    
## S.show                                                0.526616    
## A.show                                                      NA    
## S.report                                              0.062365 .  
## A.report                                                    NA    
## S.share                                               0.199503    
## A.share                                                     NA    
## S.year                                                0.244195    
## A.year                                                      NA    
## S.compani                                             0.299657    
## A.compani                                                   NA    
## H.new                                                 0.194353    
## S.first                                               0.721245    
## A.first                                                     NA    
## S.time                                                0.388427    
## A.time                                                      NA    
## H.newyork                                             0.755691    
## S.articl                                              0.583433    
## A.articl                                                    NA    
## S.will                                                0.999973    
## A.will                                                0.999949    
## H.day                                                 0.218867    
## S.newyork                                             0.271643    
## A.newyork                                                   NA    
## H.today                                               0.000166 ***
## H.report                                              0.174899    
## S.intern                                              0.296329    
## A.intern                                                    NA    
## H.week                                                0.244493    
## H.fashion                                             0.591815    
## S.week                                                0.999836    
## A.week                                                      NA    
## S.fashion                                             0.983199    
## A.fashion                                                   NA    
## H.num.chars.log                                       0.693535    
## H.num.words.log                                       0.587752    
## H.num.words.unq.log                                   0.371142    
## A.num.chars.log                                       0.962257    
## S.num.chars.log                                       0.980529    
## A.num.words.log                                       0.480679    
## S.num.words.log                                       0.476964    
## A.num.words.unq.log                                   0.568448    
## S.num.words.unq.log                                   0.558360    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 1939.9  on 4385  degrees of freedom
## AIC: 2119.9
## 
## Number of Fisher Scoring iterations: 18
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
##                                      id        cor.y exclude.as.feat
## WordCount.log             WordCount.log  0.264960434           FALSE
## myCategory.fctr         myCategory.fctr  0.012345410           FALSE
## H.is.question             H.is.question  0.129154799           FALSE
## H.today                         H.today -0.063723058           FALSE
## PubDate.wkday.fctr   PubDate.wkday.fctr -0.039801288           FALSE
## S.said                           S.said  0.001363226           FALSE
## PubDate.last10.log   PubDate.last10.log  0.049317022           FALSE
## PubDate.minute.fctr PubDate.minute.fctr -0.034073846           FALSE
## S.report                       S.report -0.050211524           FALSE
## PubDate.hour.fctr     PubDate.hour.fctr  0.135436805           FALSE
## PubDate.second.fctr PubDate.second.fctr -0.011879458           FALSE
## H.report                       H.report -0.064948102           FALSE
## H.new                             H.new -0.053121542           FALSE
## S.share                         S.share -0.050329686           FALSE
## H.day                             H.day -0.061669687           FALSE
## H.has.ebola                 H.has.ebola  0.025881397           FALSE
## S.year                           S.year -0.051146178           FALSE
## H.week                           H.week -0.075105216           FALSE
## S.newyork                     S.newyork -0.062117105           FALSE
## S.intern                       S.intern -0.068485701           FALSE
## S.compani                     S.compani -0.053012962           FALSE
## PubDate.wkend             PubDate.wkend  0.106728760           FALSE
## H.num.words.unq.log H.num.words.unq.log -0.204496360           FALSE
## S.time                           S.time -0.057595102           FALSE
## PubDate.date.fctr     PubDate.date.fctr -0.011647558           FALSE
## PubDate.last1.log     PubDate.last1.log  0.046357515           FALSE
## S.num.words.log         S.num.words.log -0.245354135           FALSE
## A.num.words.log         A.num.words.log -0.245073324           FALSE
## S.make                           S.make  0.023138853           FALSE
## S.show                           S.show -0.048801740           FALSE
## S.num.words.unq.log S.num.words.unq.log -0.250796919           FALSE
## A.num.words.unq.log A.num.words.unq.log -0.250601203           FALSE
## S.articl                       S.articl -0.059520554           FALSE
## H.num.words.log         H.num.words.log -0.200686356           FALSE
## S.take                           S.take -0.025762398           FALSE
## H.fashion                     H.fashion -0.081708612           FALSE
## H.num.chars.log         H.num.chars.log -0.171062360           FALSE
## S.first                         S.first -0.053388178           FALSE
## H.newyork                     H.newyork -0.057970095           FALSE
## H.X2014                         H.X2014 -0.046206380           FALSE
## S.presid                       S.presid -0.019828826           FALSE
## PubDate.last100.log PubDate.last100.log -0.007663322           FALSE
## .rnorm                           .rnorm  0.017561723           FALSE
## A.num.chars.log         A.num.chars.log -0.224548821           FALSE
## S.num.chars.log         S.num.chars.log -0.224692967           FALSE
## S.fashion                     S.fashion -0.086446251           FALSE
## S.day                             S.day -0.045649185           FALSE
## S.can                             S.can  0.029999780           FALSE
## A.can                             A.can  0.031498867           FALSE
## S.new                             S.new -0.034948520           FALSE
## A.new                             A.new -0.035359447           FALSE
## S.state                         S.state  0.006069626           FALSE
## A.state                         A.state  0.005702163           FALSE
## S.week                           S.week -0.084814939           FALSE
## A.one                             A.one  0.005696039           FALSE
## S.one                             S.one  0.006342094           FALSE
## A.will                           A.will -0.061025004           FALSE
## S.will                           S.will -0.060575493           FALSE
## A.articl                       A.articl -0.059520554           FALSE
## A.compani                     A.compani -0.053099633           FALSE
## A.day                             A.day -0.045909684           FALSE
## A.fashion                     A.fashion -0.086446251           FALSE
## A.first                         A.first -0.053388178           FALSE
## A.has.http                   A.has.http -0.013592603           FALSE
## A.intern                       A.intern -0.068485701           FALSE
## A.make                           A.make  0.023138853           FALSE
## A.newyork                     A.newyork -0.062117105           FALSE
## A.num.chars                 A.num.chars -0.177037425            TRUE
## A.num.words                 A.num.words -0.204211072            TRUE
## A.num.words.unq         A.num.words.unq -0.210242145            TRUE
## A.presid                       A.presid -0.019828826           FALSE
## A.report                       A.report -0.050211524           FALSE
## A.said                           A.said  0.001363226           FALSE
## A.share                         A.share -0.050329686           FALSE
## A.show                           A.show -0.048801740           FALSE
## A.take                           A.take -0.026086108           FALSE
## A.time                           A.time -0.057790617           FALSE
## A.week                           A.week -0.084814939           FALSE
## A.year                           A.year -0.051146178           FALSE
## H.daili                         H.daili -0.069192975           FALSE
## H.has.http                   H.has.http           NA           FALSE
## H.num.chars                 H.num.chars -0.147211183            TRUE
## H.num.words                 H.num.words -0.186036895            TRUE
## H.num.words.unq         H.num.words.unq -0.189702157            TRUE
## H.X2015                         H.X2015 -0.066584892           FALSE
## Popular                         Popular  1.000000000            TRUE
## Popular.fctr               Popular.fctr           NA            TRUE
## PubDate.last1             PubDate.last1  0.035922671            TRUE
## PubDate.last10           PubDate.last10  0.053980930            TRUE
## PubDate.last100         PubDate.last100  0.039892288            TRUE
## PubDate.month.fctr   PubDate.month.fctr  0.019148739            TRUE
## PubDate.POSIX             PubDate.POSIX  0.015683258            TRUE
## PubDate.year.fctr     PubDate.year.fctr           NA           FALSE
## PubDate.zoo                 PubDate.zoo  0.015683258            TRUE
## S.has.http                   S.has.http           NA           FALSE
## S.num.chars                 S.num.chars -0.179331806            TRUE
## S.num.words                 S.num.words -0.206385049            TRUE
## S.num.words.unq         S.num.words.unq -0.212102717            TRUE
## UniqueID                       UniqueID  0.011824920            TRUE
## WordCount                     WordCount  0.257526549            TRUE
##                       cor.y.abs      cor.high.X is.ConditionalX.y
## WordCount.log       0.264960434            <NA>              TRUE
## myCategory.fctr     0.012345410            <NA>              TRUE
## H.is.question       0.129154799            <NA>              TRUE
## H.today             0.063723058            <NA>              TRUE
## PubDate.wkday.fctr  0.039801288            <NA>              TRUE
## S.said              0.001363226            <NA>              TRUE
## PubDate.last10.log  0.049317022            <NA>              TRUE
## PubDate.minute.fctr 0.034073846            <NA>              TRUE
## S.report            0.050211524            <NA>              TRUE
## PubDate.hour.fctr   0.135436805            <NA>              TRUE
## PubDate.second.fctr 0.011879458            <NA>              TRUE
## H.report            0.064948102            <NA>              TRUE
## H.new               0.053121542            <NA>              TRUE
## S.share             0.050329686            <NA>              TRUE
## H.day               0.061669687            <NA>              TRUE
## H.has.ebola         0.025881397            <NA>              TRUE
## S.year              0.051146178            <NA>              TRUE
## H.week              0.075105216            <NA>              TRUE
## S.newyork           0.062117105            <NA>              TRUE
## S.intern            0.068485701            <NA>              TRUE
## S.compani           0.053012962            <NA>              TRUE
## PubDate.wkend       0.106728760            <NA>              TRUE
## H.num.words.unq.log 0.204496360 H.num.chars.log              TRUE
## S.time              0.057595102            <NA>              TRUE
## PubDate.date.fctr   0.011647558            <NA>              TRUE
## PubDate.last1.log   0.046357515            <NA>              TRUE
## S.num.words.log     0.245354135 A.num.words.log              TRUE
## A.num.words.log     0.245073324            <NA>              TRUE
## S.make              0.023138853            <NA>              TRUE
## S.show              0.048801740            <NA>              TRUE
## S.num.words.unq.log 0.250796919 S.num.chars.log              TRUE
## A.num.words.unq.log 0.250601203            <NA>              TRUE
## S.articl            0.059520554            <NA>              TRUE
## H.num.words.log     0.200686356            <NA>              TRUE
## S.take              0.025762398            <NA>              TRUE
## H.fashion           0.081708612          H.week              TRUE
## H.num.chars.log     0.171062360            <NA>              TRUE
## S.first             0.053388178            <NA>              TRUE
## H.newyork           0.057970095            <NA>              TRUE
## H.X2014             0.046206380            <NA>              TRUE
## S.presid            0.019828826            <NA>              TRUE
## PubDate.last100.log 0.007663322            <NA>              TRUE
## .rnorm              0.017561723            <NA>              TRUE
## A.num.chars.log     0.224548821            <NA>              TRUE
## S.num.chars.log     0.224692967 A.num.chars.log              TRUE
## S.fashion           0.086446251            <NA>              TRUE
## S.day               0.045649185            <NA>              TRUE
## S.can               0.029999780            <NA>              TRUE
## A.can               0.031498867           S.can              TRUE
## S.new               0.034948520            <NA>              TRUE
## A.new               0.035359447           S.new              TRUE
## S.state             0.006069626            <NA>              TRUE
## A.state             0.005702163            <NA>              TRUE
## S.week              0.084814939            <NA>              TRUE
## A.one               0.005696039            <NA>              TRUE
## S.one               0.006342094            <NA>              TRUE
## A.will              0.061025004          S.will              TRUE
## S.will              0.060575493            <NA>              TRUE
## A.articl            0.059520554        S.articl              TRUE
## A.compani           0.053099633       S.compani              TRUE
## A.day               0.045909684           S.day              TRUE
## A.fashion           0.086446251       S.fashion              TRUE
## A.first             0.053388178         S.first              TRUE
## A.has.http          0.013592603            <NA>             FALSE
## A.intern            0.068485701        S.intern              TRUE
## A.make              0.023138853          S.make              TRUE
## A.newyork           0.062117105       S.newyork              TRUE
## A.num.chars         0.177037425            <NA>                NA
## A.num.words         0.204211072            <NA>                NA
## A.num.words.unq     0.210242145            <NA>                NA
## A.presid            0.019828826        S.presid              TRUE
## A.report            0.050211524        S.report              TRUE
## A.said              0.001363226            <NA>              TRUE
## A.share             0.050329686         S.share              TRUE
## A.show              0.048801740          S.show              TRUE
## A.take              0.026086108          S.take              TRUE
## A.time              0.057790617          S.time              TRUE
## A.week              0.084814939          S.week              TRUE
## A.year              0.051146178          S.year              TRUE
## H.daili             0.069192975            <NA>             FALSE
## H.has.http                   NA            <NA>             FALSE
## H.num.chars         0.147211183            <NA>                NA
## H.num.words         0.186036895            <NA>                NA
## H.num.words.unq     0.189702157            <NA>                NA
## H.X2015             0.066584892            <NA>             FALSE
## Popular             1.000000000            <NA>                NA
## Popular.fctr                 NA            <NA>                NA
## PubDate.last1       0.035922671            <NA>                NA
## PubDate.last10      0.053980930            <NA>                NA
## PubDate.last100     0.039892288            <NA>                NA
## PubDate.month.fctr  0.019148739            <NA>                NA
## PubDate.POSIX       0.015683258            <NA>                NA
## PubDate.year.fctr            NA            <NA>             FALSE
## PubDate.zoo         0.015683258            <NA>                NA
## S.has.http                   NA            <NA>             FALSE
## S.num.chars         0.179331806            <NA>                NA
## S.num.words         0.206385049            <NA>                NA
## S.num.words.unq     0.212102717            <NA>                NA
## UniqueID            0.011824920            <NA>                NA
## WordCount           0.257526549            <NA>                NA
##                     is.cor.y.abs.low rsp_var_raw id_var rsp_var
## WordCount.log                  FALSE       FALSE     NA      NA
## myCategory.fctr                 TRUE       FALSE     NA      NA
## H.is.question                  FALSE       FALSE     NA      NA
## H.today                        FALSE       FALSE     NA      NA
## PubDate.wkday.fctr             FALSE       FALSE     NA      NA
## S.said                          TRUE       FALSE     NA      NA
## PubDate.last10.log             FALSE       FALSE     NA      NA
## PubDate.minute.fctr            FALSE       FALSE     NA      NA
## S.report                       FALSE       FALSE     NA      NA
## PubDate.hour.fctr              FALSE       FALSE     NA      NA
## PubDate.second.fctr             TRUE       FALSE     NA      NA
## H.report                       FALSE       FALSE     NA      NA
## H.new                          FALSE       FALSE     NA      NA
## S.share                        FALSE       FALSE     NA      NA
## H.day                          FALSE       FALSE     NA      NA
## H.has.ebola                    FALSE       FALSE     NA      NA
## S.year                         FALSE       FALSE     NA      NA
## H.week                         FALSE       FALSE     NA      NA
## S.newyork                      FALSE       FALSE     NA      NA
## S.intern                       FALSE       FALSE     NA      NA
## S.compani                      FALSE       FALSE     NA      NA
## PubDate.wkend                  FALSE       FALSE     NA      NA
## H.num.words.unq.log            FALSE       FALSE     NA      NA
## S.time                         FALSE       FALSE     NA      NA
## PubDate.date.fctr               TRUE       FALSE     NA      NA
## PubDate.last1.log              FALSE       FALSE     NA      NA
## S.num.words.log                FALSE       FALSE     NA      NA
## A.num.words.log                FALSE       FALSE     NA      NA
## S.make                         FALSE       FALSE     NA      NA
## S.show                         FALSE       FALSE     NA      NA
## S.num.words.unq.log            FALSE       FALSE     NA      NA
## A.num.words.unq.log            FALSE       FALSE     NA      NA
## S.articl                       FALSE       FALSE     NA      NA
## H.num.words.log                FALSE       FALSE     NA      NA
## S.take                         FALSE       FALSE     NA      NA
## H.fashion                      FALSE       FALSE     NA      NA
## H.num.chars.log                FALSE       FALSE     NA      NA
## S.first                        FALSE       FALSE     NA      NA
## H.newyork                      FALSE       FALSE     NA      NA
## H.X2014                        FALSE       FALSE     NA      NA
## S.presid                       FALSE       FALSE     NA      NA
## PubDate.last100.log             TRUE       FALSE     NA      NA
## .rnorm                         FALSE       FALSE     NA      NA
## A.num.chars.log                FALSE       FALSE     NA      NA
## S.num.chars.log                FALSE       FALSE     NA      NA
## S.fashion                      FALSE       FALSE     NA      NA
## S.day                          FALSE       FALSE     NA      NA
## S.can                          FALSE       FALSE     NA      NA
## A.can                          FALSE       FALSE     NA      NA
## S.new                          FALSE       FALSE     NA      NA
## A.new                          FALSE       FALSE     NA      NA
## S.state                         TRUE       FALSE     NA      NA
## A.state                         TRUE       FALSE     NA      NA
## S.week                         FALSE       FALSE     NA      NA
## A.one                           TRUE       FALSE     NA      NA
## S.one                           TRUE       FALSE     NA      NA
## A.will                         FALSE       FALSE     NA      NA
## S.will                         FALSE       FALSE     NA      NA
## A.articl                       FALSE       FALSE     NA      NA
## A.compani                      FALSE       FALSE     NA      NA
## A.day                          FALSE       FALSE     NA      NA
## A.fashion                      FALSE       FALSE     NA      NA
## A.first                        FALSE       FALSE     NA      NA
## A.has.http                      TRUE       FALSE     NA      NA
## A.intern                       FALSE       FALSE     NA      NA
## A.make                         FALSE       FALSE     NA      NA
## A.newyork                      FALSE       FALSE     NA      NA
## A.num.chars                    FALSE       FALSE     NA      NA
## A.num.words                    FALSE       FALSE     NA      NA
## A.num.words.unq                FALSE       FALSE     NA      NA
## A.presid                       FALSE       FALSE     NA      NA
## A.report                       FALSE       FALSE     NA      NA
## A.said                          TRUE       FALSE     NA      NA
## A.share                        FALSE       FALSE     NA      NA
## A.show                         FALSE       FALSE     NA      NA
## A.take                         FALSE       FALSE     NA      NA
## A.time                         FALSE       FALSE     NA      NA
## A.week                         FALSE       FALSE     NA      NA
## A.year                         FALSE       FALSE     NA      NA
## H.daili                        FALSE       FALSE     NA      NA
## H.has.http                        NA       FALSE     NA      NA
## H.num.chars                    FALSE       FALSE     NA      NA
## H.num.words                    FALSE       FALSE     NA      NA
## H.num.words.unq                FALSE       FALSE     NA      NA
## H.X2015                        FALSE       FALSE     NA      NA
## Popular                        FALSE        TRUE     NA      NA
## Popular.fctr                      NA          NA     NA    TRUE
## PubDate.last1                  FALSE       FALSE     NA      NA
## PubDate.last10                 FALSE       FALSE     NA      NA
## PubDate.last100                FALSE       FALSE     NA      NA
## PubDate.month.fctr             FALSE       FALSE     NA      NA
## PubDate.POSIX                   TRUE       FALSE     NA      NA
## PubDate.year.fctr                 NA       FALSE     NA      NA
## PubDate.zoo                     TRUE       FALSE     NA      NA
## S.has.http                        NA       FALSE     NA      NA
## S.num.chars                    FALSE       FALSE     NA      NA
## S.num.words                    FALSE       FALSE     NA      NA
## S.num.words.unq                FALSE       FALSE     NA      NA
## UniqueID                        TRUE       FALSE   TRUE      NA
## WordCount                      FALSE       FALSE     NA      NA
##                       importance Conditional.X.glm.importance
## WordCount.log       1.000000e+02                 1.000000e+02
## myCategory.fctr     7.930984e+01                 7.930984e+01
## H.is.question       4.627526e+01                 4.627526e+01
## H.today             2.908156e+01                 2.908156e+01
## PubDate.wkday.fctr  2.056149e+01                 2.056149e+01
## S.said              1.678465e+01                 1.678465e+01
## PubDate.last10.log  1.576922e+01                 1.576922e+01
## PubDate.minute.fctr 1.525545e+01                 1.525545e+01
## S.report            1.438997e+01                 1.438997e+01
## PubDate.hour.fctr   1.284881e+01                 1.284881e+01
## PubDate.second.fctr 1.129159e+01                 1.129159e+01
## H.report            1.047475e+01                 1.047475e+01
## H.new               1.002056e+01                 1.002056e+01
## S.share             9.905995e+00                 9.905995e+00
## H.day               9.493498e+00                 9.493498e+00
## H.has.ebola         9.037198e+00                 9.037198e+00
## S.year              8.991744e+00                 8.991744e+00
## H.week              8.986065e+00                 8.986065e+00
## S.newyork           8.487719e+00                 8.487719e+00
## S.intern            8.063434e+00                 8.063434e+00
## S.compani           8.008086e+00                 8.008086e+00
## PubDate.wkend       7.340533e+00                 7.340533e+00
## H.num.words.unq.log 6.905214e+00                 6.905214e+00
## S.time              6.659200e+00                 6.659200e+00
## PubDate.date.fctr   5.977029e+00                 5.977029e+00
## PubDate.last1.log   5.945001e+00                 5.945001e+00
## S.num.words.log     5.491119e+00                 5.491119e+00
## A.num.words.log     5.444915e+00                 5.444915e+00
## S.make              5.003336e+00                 5.003336e+00
## S.show              4.888742e+00                 4.888742e+00
## S.num.words.unq.log 4.518887e+00                 4.518887e+00
## A.num.words.unq.log 4.403520e+00                 4.403520e+00
## S.articl            4.233947e+00                 4.233947e+00
## H.num.words.log     4.185458e+00                 4.185458e+00
## S.take              4.162569e+00                 4.162569e+00
## H.fashion           4.139981e+00                 4.139981e+00
## H.num.chars.log     3.042436e+00                 3.042436e+00
## S.first             2.754680e+00                 2.754680e+00
## H.newyork           2.402194e+00                 2.402194e+00
## H.X2014             2.389830e+00                 2.389830e+00
## S.presid            2.235853e+00                 2.235853e+00
## PubDate.last100.log 1.354318e+00                 1.354318e+00
## .rnorm              3.806649e-01                 3.806649e-01
## A.num.chars.log     3.651270e-01                 3.651270e-01
## S.num.chars.log     1.881888e-01                 1.881888e-01
## S.fashion           1.623404e-01                 1.623404e-01
## S.day               9.521177e-02                 9.521177e-02
## S.can               2.002441e-02                 2.002441e-02
## A.can               1.982994e-02                 1.982994e-02
## S.new               6.732769e-03                 6.732769e-03
## A.new               6.677266e-03                 6.677266e-03
## S.state             3.956531e-03                 3.956531e-03
## A.state             3.497952e-03                 3.497952e-03
## S.week              1.322420e-03                 1.322420e-03
## A.one               9.392684e-04                 9.392684e-04
## S.one               8.828017e-04                 8.828017e-04
## A.will              2.350875e-04                 2.350875e-04
## S.will              0.000000e+00                 0.000000e+00
## A.articl                      NA                           NA
## A.compani                     NA                           NA
## A.day                         NA                           NA
## A.fashion                     NA                           NA
## A.first                       NA                           NA
## A.has.http                    NA                           NA
## A.intern                      NA                           NA
## A.make                        NA                           NA
## A.newyork                     NA                           NA
## A.num.chars                   NA                           NA
## A.num.words                   NA                           NA
## A.num.words.unq               NA                           NA
## A.presid                      NA                           NA
## A.report                      NA                           NA
## A.said                        NA                           NA
## A.share                       NA                           NA
## A.show                        NA                           NA
## A.take                        NA                           NA
## A.time                        NA                           NA
## A.week                        NA                           NA
## A.year                        NA                           NA
## H.daili                       NA                           NA
## H.has.http                    NA                           NA
## H.num.chars                   NA                           NA
## H.num.words                   NA                           NA
## H.num.words.unq               NA                           NA
## H.X2015                       NA                           NA
## Popular                       NA                           NA
## Popular.fctr                  NA                           NA
## PubDate.last1                 NA                           NA
## PubDate.last10                NA                           NA
## PubDate.last100               NA                           NA
## PubDate.month.fctr            NA                           NA
## PubDate.POSIX                 NA                           NA
## PubDate.year.fctr             NA                           NA
## PubDate.zoo                   NA                           NA
## S.has.http                    NA                           NA
## S.num.chars                   NA                           NA
## S.num.words                   NA                           NA
## S.num.words.unq               NA                           NA
## UniqueID                      NA                           NA
## WordCount                     NA                           NA
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 57
```

![](NYTBlogs_category_files/figure-html/fit.models_2-8.png) ![](NYTBlogs_category_files/figure-html/fit.models_2-9.png) ![](NYTBlogs_category_files/figure-html/fit.models_2-10.png) ![](NYTBlogs_category_files/figure-html/fit.models_2-11.png) ![](NYTBlogs_category_files/figure-html/fit.models_2-12.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 6018     6018            N                                0.0004286503
## 6370     6370            Y                                0.6248207349
##      Popular.fctr.predict.Conditional.X.glm
## 6018                                      N
## 6370                                      Y
##      Popular.fctr.predict.Conditional.X.glm.accurate
## 6018                                            TRUE
## 6370                                            TRUE
##      Popular.fctr.predict.Conditional.X.glm.error .label
## 6018                                            0   6018
## 6370                                            0   6370
## [1] "Inaccurate: "
##      UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 5486     5486            Y                                2.577097e-09
## 6387     6387            Y                                1.419471e-06
## 1273     1273            Y                                2.218331e-03
## 172       172            Y                                4.429536e-03
## 3076     3076            Y                                9.628630e-03
## 3492     3492            Y                                1.201534e-02
##      Popular.fctr.predict.Conditional.X.glm
## 5486                                      N
## 6387                                      N
## 1273                                      N
## 172                                       N
## 3076                                      N
## 3492                                      N
##      Popular.fctr.predict.Conditional.X.glm.accurate
## 5486                                           FALSE
## 6387                                           FALSE
## 1273                                           FALSE
## 172                                            FALSE
## 3076                                           FALSE
## 3492                                           FALSE
##      Popular.fctr.predict.Conditional.X.glm.error
## 5486                                   -0.4000000
## 6387                                   -0.3999986
## 1273                                   -0.3977817
## 172                                    -0.3955705
## 3076                                   -0.3903714
## 3492                                   -0.3879847
##      UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 6387     6387            Y                                1.419471e-06
## 1928     1928            Y                                1.186265e-01
## 2817     2817            Y                                1.380574e-01
## 3357     3357            Y                                2.498280e-01
## 4738     4738            N                                4.004663e-01
## 2108     2108            N                                5.153146e-01
##      Popular.fctr.predict.Conditional.X.glm
## 6387                                      N
## 1928                                      N
## 2817                                      N
## 3357                                      N
## 4738                                      Y
## 2108                                      Y
##      Popular.fctr.predict.Conditional.X.glm.accurate
## 6387                                           FALSE
## 1928                                           FALSE
## 2817                                           FALSE
## 3357                                           FALSE
## 4738                                           FALSE
## 2108                                           FALSE
##      Popular.fctr.predict.Conditional.X.glm.error
## 6387                                -0.3999985805
## 1928                                -0.2813734667
## 2817                                -0.2619425905
## 3357                                -0.1501720482
## 4738                                 0.0004663107
## 2108                                 0.1153145790
##      UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.glm.prob
## 4763     4763            N                                   0.9464817
## 4771     4771            N                                   0.9564917
## 6479     6479            N                                   0.9607223
## 770       770            N                                   0.9675307
## 4975     4975            N                                   0.9785460
## 4882     4882            N                                   0.9831972
##      Popular.fctr.predict.Conditional.X.glm
## 4763                                      Y
## 4771                                      Y
## 6479                                      Y
## 770                                       Y
## 4975                                      Y
## 4882                                      Y
##      Popular.fctr.predict.Conditional.X.glm.accurate
## 4763                                           FALSE
## 4771                                           FALSE
## 6479                                           FALSE
## 770                                            FALSE
## 4975                                           FALSE
## 4882                                           FALSE
##      Popular.fctr.predict.Conditional.X.glm.error
## 4763                                    0.5464817
## 4771                                    0.5564917
## 6479                                    0.5607223
## 770                                     0.5675307
## 4975                                    0.5785460
## 4882                                    0.5831972
```

![](NYTBlogs_category_files/figure-html/fit.models_2-13.png) 

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
## [2] Popular.fctr.predict.Conditional.X.glm.prob    
## [3] Popular.fctr.predict.Conditional.X.glm         
## [4] Popular.fctr.predict.Conditional.X.glm.accurate
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
## [1] WordCount.log      myCategory.fctr    H.is.question     
## [4] H.today            PubDate.wkday.fctr
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
## 11 fit.models          6          2 385.057 401.906   16.85
## 12 fit.models          6          3 401.907      NA      NA
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

![](NYTBlogs_category_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 12        fit.models          6          3 401.907 465.096  63.189
## 13 fit.data.training          7          0 465.096      NA      NA
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
## WordCount.log             WordCount.log 1.000000e+02
## myCategory.fctr         myCategory.fctr 7.930984e+01
## H.is.question             H.is.question 4.627526e+01
## H.today                         H.today 2.908156e+01
## PubDate.wkday.fctr   PubDate.wkday.fctr 2.056149e+01
## S.said                           S.said 1.678465e+01
## PubDate.last10.log   PubDate.last10.log 1.576922e+01
## PubDate.minute.fctr PubDate.minute.fctr 1.525545e+01
## S.report                       S.report 1.438997e+01
## PubDate.hour.fctr     PubDate.hour.fctr 1.284881e+01
## PubDate.second.fctr PubDate.second.fctr 1.129159e+01
## H.report                       H.report 1.047475e+01
## H.new                             H.new 1.002056e+01
## S.share                         S.share 9.905995e+00
## H.day                             H.day 9.493498e+00
## H.has.ebola                 H.has.ebola 9.037198e+00
## S.year                           S.year 8.991744e+00
## H.week                           H.week 8.986065e+00
## S.newyork                     S.newyork 8.487719e+00
## S.intern                       S.intern 8.063434e+00
## S.compani                     S.compani 8.008086e+00
## PubDate.wkend             PubDate.wkend 7.340533e+00
## H.num.words.unq.log H.num.words.unq.log 6.905214e+00
## S.time                           S.time 6.659200e+00
## PubDate.date.fctr     PubDate.date.fctr 5.977029e+00
## PubDate.last1.log     PubDate.last1.log 5.945001e+00
## S.num.words.log         S.num.words.log 5.491119e+00
## A.num.words.log         A.num.words.log 5.444915e+00
## S.make                           S.make 5.003336e+00
## S.show                           S.show 4.888742e+00
## S.num.words.unq.log S.num.words.unq.log 4.518887e+00
## A.num.words.unq.log A.num.words.unq.log 4.403520e+00
## S.articl                       S.articl 4.233947e+00
## H.num.words.log         H.num.words.log 4.185458e+00
## S.take                           S.take 4.162569e+00
## H.fashion                     H.fashion 4.139981e+00
## H.num.chars.log         H.num.chars.log 3.042436e+00
## S.first                         S.first 2.754680e+00
## H.newyork                     H.newyork 2.402194e+00
## H.X2014                         H.X2014 2.389830e+00
## S.presid                       S.presid 2.235853e+00
## PubDate.last100.log PubDate.last100.log 1.354318e+00
## .rnorm                           .rnorm 3.806649e-01
## A.num.chars.log         A.num.chars.log 3.651270e-01
## S.num.chars.log         S.num.chars.log 1.881888e-01
## S.fashion                     S.fashion 1.623404e-01
## S.day                             S.day 9.521177e-02
## S.can                             S.can 2.002441e-02
## A.can                             A.can 1.982994e-02
## S.new                             S.new 6.732769e-03
## A.new                             A.new 6.677266e-03
## S.state                         S.state 3.956531e-03
## A.state                         A.state 3.497952e-03
## S.week                           S.week 1.322420e-03
## A.one                             A.one 9.392684e-04
## S.one                             S.one 8.828017e-04
## A.will                           A.will 2.350875e-04
## S.will                           S.will 0.000000e+00
## [1] "fitting model: Final.glm"
## [1] "    indep_vars: WordCount.log, myCategory.fctr, H.is.question, H.today, PubDate.wkday.fctr, S.said, PubDate.last10.log, PubDate.minute.fctr, S.report, PubDate.hour.fctr, PubDate.second.fctr, H.report, H.new, S.share, H.day, H.has.ebola, S.year, H.week, S.newyork, S.intern, S.compani, PubDate.wkend, H.num.words.unq.log, S.time, PubDate.date.fctr, PubDate.last1.log, S.num.words.log, A.num.words.log, S.make, S.show, S.num.words.unq.log, A.num.words.unq.log, S.articl, H.num.words.log, S.take, H.fashion, H.num.chars.log, S.first, H.newyork, H.X2014, S.presid, PubDate.last100.log, .rnorm, A.num.chars.log, S.num.chars.log, S.fashion, S.day, S.can, A.can, S.new, A.new, S.state, A.state, S.week, A.one, S.one, A.will, S.will"
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
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: not plotting observations with leverage one:
##   1651, 5299, 5552
```

![](NYTBlogs_category_files/figure-html/fit.data.training_0-1.png) ![](NYTBlogs_category_files/figure-html/fit.data.training_0-2.png) 

```
## Warning: not plotting observations with leverage one:
##   1651, 5299, 5552
```

![](NYTBlogs_category_files/figure-html/fit.data.training_0-3.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](NYTBlogs_category_files/figure-html/fit.data.training_0-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.8447  -0.3471  -0.1786  -0.0013   3.4797  
## 
## Coefficients:
##                                                         Estimate
## (Intercept)                                           -5.998e+00
## WordCount.log                                          1.136e+00
## `myCategory.fctrForeign#World#Asia Pacific`           -4.911e+00
## `myCategory.fctr#Multimedia#`                         -4.809e+00
## `myCategory.fctrCulture#Arts#`                        -3.052e+00
## `myCategory.fctrBusiness#Business Day#Dealbook`       -2.807e+00
## myCategory.fctrmyOther                                -1.903e+01
## `myCategory.fctrBusiness#Technology#`                 -1.998e+00
## `myCategory.fctrBusiness#Crosswords/Games#`            3.066e-01
## `myCategory.fctrTStyle##`                             -4.403e+00
## `myCategory.fctrForeign#World#`                       -1.839e+01
## `myCategory.fctrOpEd#Opinion#`                         7.012e-01
## `myCategory.fctrStyles##Fashion`                      -5.000e+00
## `myCategory.fctr#Opinion#Room For Debate`             -5.489e+00
## `myCategory.fctr#U.S.#Education`                      -1.925e+01
## `myCategory.fctr##`                                   -2.743e+00
## `myCategory.fctrMetro#N.Y. / Region#`                 -1.921e+00
## `myCategory.fctrBusiness#Business Day#Small Business` -4.123e+00
## `myCategory.fctrStyles#U.S.#`                         -2.038e-01
## `myCategory.fctrTravel#Travel#`                       -4.598e+00
## `myCategory.fctr#Opinion#The Public Editor`            2.958e-01
## H.is.question                                          1.222e+00
## H.today                                               -2.819e+00
## PubDate.wkday.fctr1                                   -4.313e-01
## PubDate.wkday.fctr2                                   -8.343e-01
## PubDate.wkday.fctr3                                   -6.145e-01
## PubDate.wkday.fctr4                                   -7.334e-01
## PubDate.wkday.fctr5                                   -8.066e-01
## PubDate.wkday.fctr6                                   -8.679e-01
## S.said                                                 6.854e-01
## PubDate.last10.log                                     2.460e-01
## `PubDate.minute.fctr(14.8,29.5]`                      -4.878e-02
## `PubDate.minute.fctr(29.5,44.2]`                      -3.240e-01
## `PubDate.minute.fctr(44.2,59.1]`                      -3.516e-02
## S.report                                              -4.198e-01
## `PubDate.hour.fctr(7.67,15.3]`                         1.870e-01
## `PubDate.hour.fctr(15.3,23]`                           3.239e-01
## `PubDate.second.fctr(14.8,29.5]`                      -8.710e-02
## `PubDate.second.fctr(29.5,44.2]`                       1.093e-03
## `PubDate.second.fctr(44.2,59.1]`                      -1.833e-01
## H.report                                              -6.828e-01
## H.new                                                 -4.508e-01
## S.share                                               -3.248e-01
## H.day                                                 -4.524e-01
## H.has.ebola                                           -5.676e-01
## S.year                                                -4.889e-01
## H.week                                                -6.787e-01
## S.newyork                                              1.119e-01
## S.intern                                               2.465e-01
## S.compani                                             -3.043e-01
## PubDate.wkend                                         -1.993e-01
## H.num.words.unq.log                                   -1.958e+00
## S.time                                                -3.770e-01
## `PubDate.date.fctr(7,13]`                              1.102e-01
## `PubDate.date.fctr(13,19]`                            -6.080e-02
## `PubDate.date.fctr(19,25]`                             3.321e-02
## `PubDate.date.fctr(25,31]`                             7.250e-02
## PubDate.last1.log                                     -2.646e-02
## S.num.words.log                                        9.685e+01
## A.num.words.log                                       -9.748e+01
## S.make                                                -1.246e-01
## S.show                                                -2.001e-01
## S.num.words.unq.log                                   -8.857e+01
## A.num.words.unq.log                                    8.770e+01
## S.articl                                               1.213e-01
## H.num.words.log                                        1.630e+00
## S.take                                                 6.753e-03
## H.fashion                                             -1.762e-01
## H.num.chars.log                                       -1.059e-01
## S.first                                               -1.663e-01
## H.newyork                                              7.152e-02
## H.X2014                                               -4.490e-01
## S.presid                                              -1.079e-01
## PubDate.last100.log                                   -1.881e-02
## .rnorm                                                 4.070e-03
## A.num.chars.log                                       -7.110e+00
## S.num.chars.log                                        7.753e+00
## S.fashion                                             -1.155e+00
## S.day                                                  5.366e-02
## S.can                                                 -2.776e+01
## A.can                                                  2.732e+01
## S.new                                                  8.401e+00
## A.new                                                 -8.448e+00
## S.state                                                2.348e+00
## A.state                                               -1.896e+00
## S.week                                                 1.121e-01
## A.one                                                 -1.610e-01
## S.one                                                  2.604e-01
## A.will                                                -3.428e-01
## S.will                                                 1.266e-01
##                                                       Std. Error z value
## (Intercept)                                            1.618e+00  -3.707
## WordCount.log                                          7.086e-02  16.037
## `myCategory.fctrForeign#World#Asia Pacific`            6.245e-01  -7.864
## `myCategory.fctr#Multimedia#`                          7.562e-01  -6.359
## `myCategory.fctrCulture#Arts#`                         2.620e-01 -11.648
## `myCategory.fctrBusiness#Business Day#Dealbook`        2.280e-01 -12.309
## myCategory.fctrmyOther                                 9.316e+02  -0.020
## `myCategory.fctrBusiness#Technology#`                  2.525e-01  -7.912
## `myCategory.fctrBusiness#Crosswords/Games#`            3.563e-01   0.861
## `myCategory.fctrTStyle##`                              3.989e-01 -11.038
## `myCategory.fctrForeign#World#`                        4.755e+02  -0.039
## `myCategory.fctrOpEd#Opinion#`                         2.316e-01   3.028
## `myCategory.fctrStyles##Fashion`                       1.110e+00  -4.502
## `myCategory.fctr#Opinion#Room For Debate`              4.856e-01 -11.305
## `myCategory.fctr#U.S.#Education`                       3.112e+02  -0.062
## `myCategory.fctr##`                                    2.142e-01 -12.802
## `myCategory.fctrMetro#N.Y. / Region#`                  3.617e-01  -5.311
## `myCategory.fctrBusiness#Business Day#Small Business`  5.210e-01  -7.913
## `myCategory.fctrStyles#U.S.#`                          2.606e-01  -0.782
## `myCategory.fctrTravel#Travel#`                        1.024e+00  -4.488
## `myCategory.fctr#Opinion#The Public Editor`            6.485e-01   0.456
## H.is.question                                          1.726e-01   7.079
## H.today                                                6.211e-01  -4.539
## PubDate.wkday.fctr1                                    4.010e-01  -1.076
## PubDate.wkday.fctr2                                    4.380e-01  -1.905
## PubDate.wkday.fctr3                                    4.320e-01  -1.423
## PubDate.wkday.fctr4                                    4.259e-01  -1.722
## PubDate.wkday.fctr5                                    4.333e-01  -1.862
## PubDate.wkday.fctr6                                    3.620e-01  -2.398
## S.said                                                 2.123e-01   3.228
## PubDate.last10.log                                     9.791e-02   2.512
## `PubDate.minute.fctr(14.8,29.5]`                       1.415e-01  -0.345
## `PubDate.minute.fctr(29.5,44.2]`                       1.388e-01  -2.334
## `PubDate.minute.fctr(44.2,59.1]`                       1.454e-01  -0.242
## S.report                                               3.032e-01  -1.384
## `PubDate.hour.fctr(7.67,15.3]`                         1.842e-01   1.015
## `PubDate.hour.fctr(15.3,23]`                           1.871e-01   1.731
## `PubDate.second.fctr(14.8,29.5]`                       1.391e-01  -0.626
## `PubDate.second.fctr(29.5,44.2]`                       1.369e-01   0.008
## `PubDate.second.fctr(44.2,59.1]`                       1.397e-01  -1.313
## H.report                                               4.516e-01  -1.512
## H.new                                                  3.285e-01  -1.372
## S.share                                                3.392e-01  -0.958
## H.day                                                  4.752e-01  -0.952
## H.has.ebola                                            3.756e-01  -1.511
## S.year                                                 2.485e-01  -1.968
## H.week                                                 5.360e-01  -1.266
## S.newyork                                              2.910e-01   0.385
## S.intern                                               4.955e-01   0.497
## S.compani                                              2.146e-01  -1.418
## PubDate.wkend                                          3.383e-01  -0.589
## H.num.words.unq.log                                    1.595e+00  -1.228
## S.time                                                 2.367e-01  -1.593
## `PubDate.date.fctr(7,13]`                              1.550e-01   0.711
## `PubDate.date.fctr(13,19]`                             1.547e-01  -0.393
## `PubDate.date.fctr(19,25]`                             1.513e-01   0.219
## `PubDate.date.fctr(25,31]`                             1.632e-01   0.444
## PubDate.last1.log                                      3.489e-02  -0.759
## S.num.words.log                                        6.307e+01   1.536
## A.num.words.log                                        6.307e+01  -1.546
## S.make                                                 2.268e-01  -0.549
## S.show                                                 3.149e-01  -0.635
## S.num.words.unq.log                                    7.120e+01  -1.244
## A.num.words.unq.log                                    7.120e+01   1.232
## S.articl                                               5.729e-01   0.212
## H.num.words.log                                        1.622e+00   1.005
## S.take                                                 2.986e-01   0.023
## H.fashion                                              1.147e+00  -0.154
## H.num.chars.log                                        2.846e-01  -0.372
## S.first                                                3.641e-01  -0.457
## H.newyork                                              4.407e-01   0.162
## H.X2014                                                7.863e-01  -0.571
## S.presid                                               2.693e-01  -0.401
## PubDate.last100.log                                    3.473e-02  -0.542
## .rnorm                                                 4.948e-02   0.082
## A.num.chars.log                                        2.110e+01  -0.337
## S.num.chars.log                                        2.110e+01   0.367
## S.fashion                                              1.116e+00  -1.035
## S.day                                                  3.173e-01   0.169
## S.can                                                  6.523e+03  -0.004
## A.can                                                  6.523e+03   0.004
## S.new                                                  4.277e+03   0.002
## A.new                                                  4.277e+03  -0.002
## S.state                                                6.523e+03   0.000
## A.state                                                6.523e+03   0.000
## S.week                                                 2.614e-01   0.429
## A.one                                                  7.772e+03   0.000
## S.one                                                  7.772e+03   0.000
## A.will                                                 6.540e+03   0.000
## S.will                                                 6.540e+03   0.000
##                                                       Pr(>|z|)    
## (Intercept)                                            0.00021 ***
## WordCount.log                                          < 2e-16 ***
## `myCategory.fctrForeign#World#Asia Pacific`           3.73e-15 ***
## `myCategory.fctr#Multimedia#`                         2.03e-10 ***
## `myCategory.fctrCulture#Arts#`                         < 2e-16 ***
## `myCategory.fctrBusiness#Business Day#Dealbook`        < 2e-16 ***
## myCategory.fctrmyOther                                 0.98370    
## `myCategory.fctrBusiness#Technology#`                 2.53e-15 ***
## `myCategory.fctrBusiness#Crosswords/Games#`            0.38949    
## `myCategory.fctrTStyle##`                              < 2e-16 ***
## `myCategory.fctrForeign#World#`                        0.96915    
## `myCategory.fctrOpEd#Opinion#`                         0.00246 ** 
## `myCategory.fctrStyles##Fashion`                      6.72e-06 ***
## `myCategory.fctr#Opinion#Room For Debate`              < 2e-16 ***
## `myCategory.fctr#U.S.#Education`                       0.95068    
## `myCategory.fctr##`                                    < 2e-16 ***
## `myCategory.fctrMetro#N.Y. / Region#`                 1.09e-07 ***
## `myCategory.fctrBusiness#Business Day#Small Business` 2.52e-15 ***
## `myCategory.fctrStyles#U.S.#`                          0.43412    
## `myCategory.fctrTravel#Travel#`                       7.20e-06 ***
## `myCategory.fctr#Opinion#The Public Editor`            0.64832    
## H.is.question                                         1.45e-12 ***
## H.today                                               5.66e-06 ***
## PubDate.wkday.fctr1                                    0.28207    
## PubDate.wkday.fctr2                                    0.05679 .  
## PubDate.wkday.fctr3                                    0.15486    
## PubDate.wkday.fctr4                                    0.08503 .  
## PubDate.wkday.fctr5                                    0.06263 .  
## PubDate.wkday.fctr6                                    0.01651 *  
## S.said                                                 0.00125 ** 
## PubDate.last10.log                                     0.01200 *  
## `PubDate.minute.fctr(14.8,29.5]`                       0.73026    
## `PubDate.minute.fctr(29.5,44.2]`                       0.01957 *  
## `PubDate.minute.fctr(44.2,59.1]`                       0.80887    
## S.report                                               0.16625    
## `PubDate.hour.fctr(7.67,15.3]`                         0.30996    
## `PubDate.hour.fctr(15.3,23]`                           0.08342 .  
## `PubDate.second.fctr(14.8,29.5]`                       0.53127    
## `PubDate.second.fctr(29.5,44.2]`                       0.99363    
## `PubDate.second.fctr(44.2,59.1]`                       0.18934    
## H.report                                               0.13055    
## H.new                                                  0.17000    
## S.share                                                0.33820    
## H.day                                                  0.34111    
## H.has.ebola                                            0.13081    
## S.year                                                 0.04908 *  
## H.week                                                 0.20541    
## S.newyork                                              0.70046    
## S.intern                                               0.61886    
## S.compani                                              0.15619    
## PubDate.wkend                                          0.55591    
## H.num.words.unq.log                                    0.21950    
## S.time                                                 0.11118    
## `PubDate.date.fctr(7,13]`                              0.47701    
## `PubDate.date.fctr(13,19]`                             0.69426    
## `PubDate.date.fctr(19,25]`                             0.82627    
## `PubDate.date.fctr(25,31]`                             0.65679    
## PubDate.last1.log                                      0.44814    
## S.num.words.log                                        0.12463    
## A.num.words.log                                        0.12217    
## S.make                                                 0.58292    
## S.show                                                 0.52523    
## S.num.words.unq.log                                    0.21352    
## A.num.words.unq.log                                    0.21805    
## S.articl                                               0.83230    
## H.num.words.log                                        0.31486    
## S.take                                                 0.98196    
## H.fashion                                              0.87797    
## H.num.chars.log                                        0.70991    
## S.first                                                0.64792    
## H.newyork                                              0.87107    
## H.X2014                                                0.56797    
## S.presid                                               0.68852    
## PubDate.last100.log                                    0.58808    
## .rnorm                                                 0.93445    
## A.num.chars.log                                        0.73610    
## S.num.chars.log                                        0.71327    
## S.fashion                                              0.30069    
## S.day                                                  0.86569    
## S.can                                                  0.99660    
## A.can                                                  0.99666    
## S.new                                                  0.99843    
## A.new                                                  0.99842    
## S.state                                                0.99971    
## A.state                                                0.99977    
## S.week                                                 0.66811    
## A.one                                                  0.99998    
## S.one                                                  0.99997    
## A.will                                                 0.99996    
## S.will                                                 0.99998    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5900.1  on 6531  degrees of freedom
## Residual deviance: 2885.0  on 6442  degrees of freedom
## AIC: 3065
## 
## Number of Fisher Scoring iterations: 17
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_category_files/figure-html/fit.data.training_0-5.png) 

```
##    threshold   f.score
## 1        0.0 0.2866885
## 2        0.1 0.6438313
## 3        0.2 0.7214938
## 4        0.3 0.7279121
## 5        0.4 0.7293340
## 6        0.5 0.7172619
## 7        0.6 0.6936842
## 8        0.7 0.6431417
## 9        0.8 0.5352645
## 10       0.9 0.3487859
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Final.glm.N
## 1            N                             5187
## 2            Y                              321
##   Popular.fctr.predict.Final.glm.Y
## 1                              252
## 2                              772
##          Prediction
## Reference    N    Y
##         N 5187  252
##         Y  321  772
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.122780e-01   6.770572e-01   9.051557e-01   9.190297e-01   8.326699e-01 
## AccuracyPValue  McnemarPValue 
##   8.517484e-78   4.500889e-03
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](NYTBlogs_category_files/figure-html/fit.data.training_0-6.png) 

```
##    model_id model_method
## 1 Final.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 WordCount.log, myCategory.fctr, H.is.question, H.today, PubDate.wkday.fctr, S.said, PubDate.last10.log, PubDate.minute.fctr, S.report, PubDate.hour.fctr, PubDate.second.fctr, H.report, H.new, S.share, H.day, H.has.ebola, S.year, H.week, S.newyork, S.intern, S.compani, PubDate.wkend, H.num.words.unq.log, S.time, PubDate.date.fctr, PubDate.last1.log, S.num.words.log, A.num.words.log, S.make, S.show, S.num.words.unq.log, A.num.words.unq.log, S.articl, H.num.words.log, S.take, H.fashion, H.num.chars.log, S.first, H.newyork, H.X2014, S.presid, PubDate.last100.log, .rnorm, A.num.chars.log, S.num.chars.log, S.fashion, S.day, S.can, A.can, S.new, A.new, S.state, A.state, S.week, A.one, S.one, A.will, S.will
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     10.137                 2.979
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9401281                    0.4        0.729334        0.9082964
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit min.aic.fit
## 1             0.9051557             0.9190297     0.6481561    3065.049
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01061219      0.04714244
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13 fit.data.training          7          0 465.096 481.573  16.477
## 14 fit.data.training          7          1 481.574      NA      NA
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
##                                      id   importance        cor.y
## WordCount.log             WordCount.log 1.000000e+02  0.264960434
## myCategory.fctr         myCategory.fctr 7.930984e+01  0.012345410
## H.is.question             H.is.question 4.627526e+01  0.129154799
## H.today                         H.today 2.908156e+01 -0.063723058
## PubDate.wkday.fctr   PubDate.wkday.fctr 2.056149e+01 -0.039801288
## S.said                           S.said 1.678465e+01  0.001363226
## PubDate.last10.log   PubDate.last10.log 1.576922e+01  0.049317022
## PubDate.minute.fctr PubDate.minute.fctr 1.525545e+01 -0.034073846
## S.report                       S.report 1.438997e+01 -0.050211524
## PubDate.hour.fctr     PubDate.hour.fctr 1.284881e+01  0.135436805
## PubDate.second.fctr PubDate.second.fctr 1.129159e+01 -0.011879458
## H.report                       H.report 1.047475e+01 -0.064948102
## H.new                             H.new 1.002056e+01 -0.053121542
## S.share                         S.share 9.905995e+00 -0.050329686
## H.day                             H.day 9.493498e+00 -0.061669687
## H.has.ebola                 H.has.ebola 9.037198e+00  0.025881397
## S.year                           S.year 8.991744e+00 -0.051146178
## H.week                           H.week 8.986065e+00 -0.075105216
## S.newyork                     S.newyork 8.487719e+00 -0.062117105
## S.intern                       S.intern 8.063434e+00 -0.068485701
## S.compani                     S.compani 8.008086e+00 -0.053012962
## PubDate.wkend             PubDate.wkend 7.340533e+00  0.106728760
## H.num.words.unq.log H.num.words.unq.log 6.905214e+00 -0.204496360
## S.time                           S.time 6.659200e+00 -0.057595102
## PubDate.date.fctr     PubDate.date.fctr 5.977029e+00 -0.011647558
## PubDate.last1.log     PubDate.last1.log 5.945001e+00  0.046357515
## S.num.words.log         S.num.words.log 5.491119e+00 -0.245354135
## A.num.words.log         A.num.words.log 5.444915e+00 -0.245073324
## S.make                           S.make 5.003336e+00  0.023138853
## S.show                           S.show 4.888742e+00 -0.048801740
## S.num.words.unq.log S.num.words.unq.log 4.518887e+00 -0.250796919
## A.num.words.unq.log A.num.words.unq.log 4.403520e+00 -0.250601203
## S.articl                       S.articl 4.233947e+00 -0.059520554
## H.num.words.log         H.num.words.log 4.185458e+00 -0.200686356
## S.take                           S.take 4.162569e+00 -0.025762398
## H.fashion                     H.fashion 4.139981e+00 -0.081708612
## H.num.chars.log         H.num.chars.log 3.042436e+00 -0.171062360
## S.first                         S.first 2.754680e+00 -0.053388178
## H.newyork                     H.newyork 2.402194e+00 -0.057970095
## H.X2014                         H.X2014 2.389830e+00 -0.046206380
## S.presid                       S.presid 2.235853e+00 -0.019828826
## PubDate.last100.log PubDate.last100.log 1.354318e+00 -0.007663322
## .rnorm                           .rnorm 3.806649e-01  0.017561723
## A.num.chars.log         A.num.chars.log 3.651270e-01 -0.224548821
## S.num.chars.log         S.num.chars.log 1.881888e-01 -0.224692967
## S.fashion                     S.fashion 1.623404e-01 -0.086446251
## S.day                             S.day 9.521177e-02 -0.045649185
## S.can                             S.can 2.002441e-02  0.029999780
## A.can                             A.can 1.982994e-02  0.031498867
## S.new                             S.new 6.732769e-03 -0.034948520
## A.new                             A.new 6.677266e-03 -0.035359447
## S.state                         S.state 3.956531e-03  0.006069626
## A.state                         A.state 3.497952e-03  0.005702163
## S.week                           S.week 1.322420e-03 -0.084814939
## A.one                             A.one 9.392684e-04  0.005696039
## S.one                             S.one 8.828017e-04  0.006342094
## A.will                           A.will 2.350875e-04 -0.061025004
## S.will                           S.will 0.000000e+00 -0.060575493
## A.articl                       A.articl           NA -0.059520554
## A.compani                     A.compani           NA -0.053099633
## A.day                             A.day           NA -0.045909684
## A.fashion                     A.fashion           NA -0.086446251
## A.first                         A.first           NA -0.053388178
## A.has.http                   A.has.http           NA -0.013592603
## A.intern                       A.intern           NA -0.068485701
## A.make                           A.make           NA  0.023138853
## A.newyork                     A.newyork           NA -0.062117105
## A.num.chars                 A.num.chars           NA -0.177037425
## A.num.words                 A.num.words           NA -0.204211072
## A.num.words.unq         A.num.words.unq           NA -0.210242145
## A.presid                       A.presid           NA -0.019828826
## A.report                       A.report           NA -0.050211524
## A.said                           A.said           NA  0.001363226
## A.share                         A.share           NA -0.050329686
## A.show                           A.show           NA -0.048801740
## A.take                           A.take           NA -0.026086108
## A.time                           A.time           NA -0.057790617
## A.week                           A.week           NA -0.084814939
## A.year                           A.year           NA -0.051146178
## H.daili                         H.daili           NA -0.069192975
## H.has.http                   H.has.http           NA           NA
## H.num.chars                 H.num.chars           NA -0.147211183
## H.num.words                 H.num.words           NA -0.186036895
## H.num.words.unq         H.num.words.unq           NA -0.189702157
## H.X2015                         H.X2015           NA -0.066584892
## Popular                         Popular           NA  1.000000000
## Popular.fctr               Popular.fctr           NA           NA
## PubDate.last1             PubDate.last1           NA  0.035922671
## PubDate.last10           PubDate.last10           NA  0.053980930
## PubDate.last100         PubDate.last100           NA  0.039892288
## PubDate.month.fctr   PubDate.month.fctr           NA  0.019148739
## PubDate.POSIX             PubDate.POSIX           NA  0.015683258
## PubDate.year.fctr     PubDate.year.fctr           NA           NA
## PubDate.zoo                 PubDate.zoo           NA  0.015683258
## S.has.http                   S.has.http           NA           NA
## S.num.chars                 S.num.chars           NA -0.179331806
## S.num.words                 S.num.words           NA -0.206385049
## S.num.words.unq         S.num.words.unq           NA -0.212102717
## UniqueID                       UniqueID           NA  0.011824920
## WordCount                     WordCount           NA  0.257526549
##                     exclude.as.feat   cor.y.abs      cor.high.X
## WordCount.log                 FALSE 0.264960434            <NA>
## myCategory.fctr               FALSE 0.012345410            <NA>
## H.is.question                 FALSE 0.129154799            <NA>
## H.today                       FALSE 0.063723058            <NA>
## PubDate.wkday.fctr            FALSE 0.039801288            <NA>
## S.said                        FALSE 0.001363226            <NA>
## PubDate.last10.log            FALSE 0.049317022            <NA>
## PubDate.minute.fctr           FALSE 0.034073846            <NA>
## S.report                      FALSE 0.050211524            <NA>
## PubDate.hour.fctr             FALSE 0.135436805            <NA>
## PubDate.second.fctr           FALSE 0.011879458            <NA>
## H.report                      FALSE 0.064948102            <NA>
## H.new                         FALSE 0.053121542            <NA>
## S.share                       FALSE 0.050329686            <NA>
## H.day                         FALSE 0.061669687            <NA>
## H.has.ebola                   FALSE 0.025881397            <NA>
## S.year                        FALSE 0.051146178            <NA>
## H.week                        FALSE 0.075105216            <NA>
## S.newyork                     FALSE 0.062117105            <NA>
## S.intern                      FALSE 0.068485701            <NA>
## S.compani                     FALSE 0.053012962            <NA>
## PubDate.wkend                 FALSE 0.106728760            <NA>
## H.num.words.unq.log           FALSE 0.204496360 H.num.chars.log
## S.time                        FALSE 0.057595102            <NA>
## PubDate.date.fctr             FALSE 0.011647558            <NA>
## PubDate.last1.log             FALSE 0.046357515            <NA>
## S.num.words.log               FALSE 0.245354135 A.num.words.log
## A.num.words.log               FALSE 0.245073324            <NA>
## S.make                        FALSE 0.023138853            <NA>
## S.show                        FALSE 0.048801740            <NA>
## S.num.words.unq.log           FALSE 0.250796919 S.num.chars.log
## A.num.words.unq.log           FALSE 0.250601203            <NA>
## S.articl                      FALSE 0.059520554            <NA>
## H.num.words.log               FALSE 0.200686356            <NA>
## S.take                        FALSE 0.025762398            <NA>
## H.fashion                     FALSE 0.081708612          H.week
## H.num.chars.log               FALSE 0.171062360            <NA>
## S.first                       FALSE 0.053388178            <NA>
## H.newyork                     FALSE 0.057970095            <NA>
## H.X2014                       FALSE 0.046206380            <NA>
## S.presid                      FALSE 0.019828826            <NA>
## PubDate.last100.log           FALSE 0.007663322            <NA>
## .rnorm                        FALSE 0.017561723            <NA>
## A.num.chars.log               FALSE 0.224548821            <NA>
## S.num.chars.log               FALSE 0.224692967 A.num.chars.log
## S.fashion                     FALSE 0.086446251            <NA>
## S.day                         FALSE 0.045649185            <NA>
## S.can                         FALSE 0.029999780            <NA>
## A.can                         FALSE 0.031498867           S.can
## S.new                         FALSE 0.034948520            <NA>
## A.new                         FALSE 0.035359447           S.new
## S.state                       FALSE 0.006069626            <NA>
## A.state                       FALSE 0.005702163            <NA>
## S.week                        FALSE 0.084814939            <NA>
## A.one                         FALSE 0.005696039            <NA>
## S.one                         FALSE 0.006342094            <NA>
## A.will                        FALSE 0.061025004          S.will
## S.will                        FALSE 0.060575493            <NA>
## A.articl                      FALSE 0.059520554        S.articl
## A.compani                     FALSE 0.053099633       S.compani
## A.day                         FALSE 0.045909684           S.day
## A.fashion                     FALSE 0.086446251       S.fashion
## A.first                       FALSE 0.053388178         S.first
## A.has.http                    FALSE 0.013592603            <NA>
## A.intern                      FALSE 0.068485701        S.intern
## A.make                        FALSE 0.023138853          S.make
## A.newyork                     FALSE 0.062117105       S.newyork
## A.num.chars                    TRUE 0.177037425            <NA>
## A.num.words                    TRUE 0.204211072            <NA>
## A.num.words.unq                TRUE 0.210242145            <NA>
## A.presid                      FALSE 0.019828826        S.presid
## A.report                      FALSE 0.050211524        S.report
## A.said                        FALSE 0.001363226            <NA>
## A.share                       FALSE 0.050329686         S.share
## A.show                        FALSE 0.048801740          S.show
## A.take                        FALSE 0.026086108          S.take
## A.time                        FALSE 0.057790617          S.time
## A.week                        FALSE 0.084814939          S.week
## A.year                        FALSE 0.051146178          S.year
## H.daili                       FALSE 0.069192975            <NA>
## H.has.http                    FALSE          NA            <NA>
## H.num.chars                    TRUE 0.147211183            <NA>
## H.num.words                    TRUE 0.186036895            <NA>
## H.num.words.unq                TRUE 0.189702157            <NA>
## H.X2015                       FALSE 0.066584892            <NA>
## Popular                        TRUE 1.000000000            <NA>
## Popular.fctr                   TRUE          NA            <NA>
## PubDate.last1                  TRUE 0.035922671            <NA>
## PubDate.last10                 TRUE 0.053980930            <NA>
## PubDate.last100                TRUE 0.039892288            <NA>
## PubDate.month.fctr             TRUE 0.019148739            <NA>
## PubDate.POSIX                  TRUE 0.015683258            <NA>
## PubDate.year.fctr             FALSE          NA            <NA>
## PubDate.zoo                    TRUE 0.015683258            <NA>
## S.has.http                    FALSE          NA            <NA>
## S.num.chars                    TRUE 0.179331806            <NA>
## S.num.words                    TRUE 0.206385049            <NA>
## S.num.words.unq                TRUE 0.212102717            <NA>
## UniqueID                       TRUE 0.011824920            <NA>
## WordCount                      TRUE 0.257526549            <NA>
##                     is.ConditionalX.y is.cor.y.abs.low rsp_var_raw id_var
## WordCount.log                    TRUE            FALSE       FALSE     NA
## myCategory.fctr                  TRUE             TRUE       FALSE     NA
## H.is.question                    TRUE            FALSE       FALSE     NA
## H.today                          TRUE            FALSE       FALSE     NA
## PubDate.wkday.fctr               TRUE            FALSE       FALSE     NA
## S.said                           TRUE             TRUE       FALSE     NA
## PubDate.last10.log               TRUE            FALSE       FALSE     NA
## PubDate.minute.fctr              TRUE            FALSE       FALSE     NA
## S.report                         TRUE            FALSE       FALSE     NA
## PubDate.hour.fctr                TRUE            FALSE       FALSE     NA
## PubDate.second.fctr              TRUE             TRUE       FALSE     NA
## H.report                         TRUE            FALSE       FALSE     NA
## H.new                            TRUE            FALSE       FALSE     NA
## S.share                          TRUE            FALSE       FALSE     NA
## H.day                            TRUE            FALSE       FALSE     NA
## H.has.ebola                      TRUE            FALSE       FALSE     NA
## S.year                           TRUE            FALSE       FALSE     NA
## H.week                           TRUE            FALSE       FALSE     NA
## S.newyork                        TRUE            FALSE       FALSE     NA
## S.intern                         TRUE            FALSE       FALSE     NA
## S.compani                        TRUE            FALSE       FALSE     NA
## PubDate.wkend                    TRUE            FALSE       FALSE     NA
## H.num.words.unq.log              TRUE            FALSE       FALSE     NA
## S.time                           TRUE            FALSE       FALSE     NA
## PubDate.date.fctr                TRUE             TRUE       FALSE     NA
## PubDate.last1.log                TRUE            FALSE       FALSE     NA
## S.num.words.log                  TRUE            FALSE       FALSE     NA
## A.num.words.log                  TRUE            FALSE       FALSE     NA
## S.make                           TRUE            FALSE       FALSE     NA
## S.show                           TRUE            FALSE       FALSE     NA
## S.num.words.unq.log              TRUE            FALSE       FALSE     NA
## A.num.words.unq.log              TRUE            FALSE       FALSE     NA
## S.articl                         TRUE            FALSE       FALSE     NA
## H.num.words.log                  TRUE            FALSE       FALSE     NA
## S.take                           TRUE            FALSE       FALSE     NA
## H.fashion                        TRUE            FALSE       FALSE     NA
## H.num.chars.log                  TRUE            FALSE       FALSE     NA
## S.first                          TRUE            FALSE       FALSE     NA
## H.newyork                        TRUE            FALSE       FALSE     NA
## H.X2014                          TRUE            FALSE       FALSE     NA
## S.presid                         TRUE            FALSE       FALSE     NA
## PubDate.last100.log              TRUE             TRUE       FALSE     NA
## .rnorm                           TRUE            FALSE       FALSE     NA
## A.num.chars.log                  TRUE            FALSE       FALSE     NA
## S.num.chars.log                  TRUE            FALSE       FALSE     NA
## S.fashion                        TRUE            FALSE       FALSE     NA
## S.day                            TRUE            FALSE       FALSE     NA
## S.can                            TRUE            FALSE       FALSE     NA
## A.can                            TRUE            FALSE       FALSE     NA
## S.new                            TRUE            FALSE       FALSE     NA
## A.new                            TRUE            FALSE       FALSE     NA
## S.state                          TRUE             TRUE       FALSE     NA
## A.state                          TRUE             TRUE       FALSE     NA
## S.week                           TRUE            FALSE       FALSE     NA
## A.one                            TRUE             TRUE       FALSE     NA
## S.one                            TRUE             TRUE       FALSE     NA
## A.will                           TRUE            FALSE       FALSE     NA
## S.will                           TRUE            FALSE       FALSE     NA
## A.articl                         TRUE            FALSE       FALSE     NA
## A.compani                        TRUE            FALSE       FALSE     NA
## A.day                            TRUE            FALSE       FALSE     NA
## A.fashion                        TRUE            FALSE       FALSE     NA
## A.first                          TRUE            FALSE       FALSE     NA
## A.has.http                      FALSE             TRUE       FALSE     NA
## A.intern                         TRUE            FALSE       FALSE     NA
## A.make                           TRUE            FALSE       FALSE     NA
## A.newyork                        TRUE            FALSE       FALSE     NA
## A.num.chars                        NA            FALSE       FALSE     NA
## A.num.words                        NA            FALSE       FALSE     NA
## A.num.words.unq                    NA            FALSE       FALSE     NA
## A.presid                         TRUE            FALSE       FALSE     NA
## A.report                         TRUE            FALSE       FALSE     NA
## A.said                           TRUE             TRUE       FALSE     NA
## A.share                          TRUE            FALSE       FALSE     NA
## A.show                           TRUE            FALSE       FALSE     NA
## A.take                           TRUE            FALSE       FALSE     NA
## A.time                           TRUE            FALSE       FALSE     NA
## A.week                           TRUE            FALSE       FALSE     NA
## A.year                           TRUE            FALSE       FALSE     NA
## H.daili                         FALSE            FALSE       FALSE     NA
## H.has.http                      FALSE               NA       FALSE     NA
## H.num.chars                        NA            FALSE       FALSE     NA
## H.num.words                        NA            FALSE       FALSE     NA
## H.num.words.unq                    NA            FALSE       FALSE     NA
## H.X2015                         FALSE            FALSE       FALSE     NA
## Popular                            NA            FALSE        TRUE     NA
## Popular.fctr                       NA               NA          NA     NA
## PubDate.last1                      NA            FALSE       FALSE     NA
## PubDate.last10                     NA            FALSE       FALSE     NA
## PubDate.last100                    NA            FALSE       FALSE     NA
## PubDate.month.fctr                 NA            FALSE       FALSE     NA
## PubDate.POSIX                      NA             TRUE       FALSE     NA
## PubDate.year.fctr               FALSE               NA       FALSE     NA
## PubDate.zoo                        NA             TRUE       FALSE     NA
## S.has.http                      FALSE               NA       FALSE     NA
## S.num.chars                        NA            FALSE       FALSE     NA
## S.num.words                        NA            FALSE       FALSE     NA
## S.num.words.unq                    NA            FALSE       FALSE     NA
## UniqueID                           NA             TRUE       FALSE   TRUE
## WordCount                          NA            FALSE       FALSE     NA
##                     rsp_var Conditional.X.glm.importance
## WordCount.log            NA                 1.000000e+02
## myCategory.fctr          NA                 7.930984e+01
## H.is.question            NA                 4.627526e+01
## H.today                  NA                 2.908156e+01
## PubDate.wkday.fctr       NA                 2.056149e+01
## S.said                   NA                 1.678465e+01
## PubDate.last10.log       NA                 1.576922e+01
## PubDate.minute.fctr      NA                 1.525545e+01
## S.report                 NA                 1.438997e+01
## PubDate.hour.fctr        NA                 1.284881e+01
## PubDate.second.fctr      NA                 1.129159e+01
## H.report                 NA                 1.047475e+01
## H.new                    NA                 1.002056e+01
## S.share                  NA                 9.905995e+00
## H.day                    NA                 9.493498e+00
## H.has.ebola              NA                 9.037198e+00
## S.year                   NA                 8.991744e+00
## H.week                   NA                 8.986065e+00
## S.newyork                NA                 8.487719e+00
## S.intern                 NA                 8.063434e+00
## S.compani                NA                 8.008086e+00
## PubDate.wkend            NA                 7.340533e+00
## H.num.words.unq.log      NA                 6.905214e+00
## S.time                   NA                 6.659200e+00
## PubDate.date.fctr        NA                 5.977029e+00
## PubDate.last1.log        NA                 5.945001e+00
## S.num.words.log          NA                 5.491119e+00
## A.num.words.log          NA                 5.444915e+00
## S.make                   NA                 5.003336e+00
## S.show                   NA                 4.888742e+00
## S.num.words.unq.log      NA                 4.518887e+00
## A.num.words.unq.log      NA                 4.403520e+00
## S.articl                 NA                 4.233947e+00
## H.num.words.log          NA                 4.185458e+00
## S.take                   NA                 4.162569e+00
## H.fashion                NA                 4.139981e+00
## H.num.chars.log          NA                 3.042436e+00
## S.first                  NA                 2.754680e+00
## H.newyork                NA                 2.402194e+00
## H.X2014                  NA                 2.389830e+00
## S.presid                 NA                 2.235853e+00
## PubDate.last100.log      NA                 1.354318e+00
## .rnorm                   NA                 3.806649e-01
## A.num.chars.log          NA                 3.651270e-01
## S.num.chars.log          NA                 1.881888e-01
## S.fashion                NA                 1.623404e-01
## S.day                    NA                 9.521177e-02
## S.can                    NA                 2.002441e-02
## A.can                    NA                 1.982994e-02
## S.new                    NA                 6.732769e-03
## A.new                    NA                 6.677266e-03
## S.state                  NA                 3.956531e-03
## A.state                  NA                 3.497952e-03
## S.week                   NA                 1.322420e-03
## A.one                    NA                 9.392684e-04
## S.one                    NA                 8.828017e-04
## A.will                   NA                 2.350875e-04
## S.will                   NA                 0.000000e+00
## A.articl                 NA                           NA
## A.compani                NA                           NA
## A.day                    NA                           NA
## A.fashion                NA                           NA
## A.first                  NA                           NA
## A.has.http               NA                           NA
## A.intern                 NA                           NA
## A.make                   NA                           NA
## A.newyork                NA                           NA
## A.num.chars              NA                           NA
## A.num.words              NA                           NA
## A.num.words.unq          NA                           NA
## A.presid                 NA                           NA
## A.report                 NA                           NA
## A.said                   NA                           NA
## A.share                  NA                           NA
## A.show                   NA                           NA
## A.take                   NA                           NA
## A.time                   NA                           NA
## A.week                   NA                           NA
## A.year                   NA                           NA
## H.daili                  NA                           NA
## H.has.http               NA                           NA
## H.num.chars              NA                           NA
## H.num.words              NA                           NA
## H.num.words.unq          NA                           NA
## H.X2015                  NA                           NA
## Popular                  NA                           NA
## Popular.fctr           TRUE                           NA
## PubDate.last1            NA                           NA
## PubDate.last10           NA                           NA
## PubDate.last100          NA                           NA
## PubDate.month.fctr       NA                           NA
## PubDate.POSIX            NA                           NA
## PubDate.year.fctr        NA                           NA
## PubDate.zoo              NA                           NA
## S.has.http               NA                           NA
## S.num.chars              NA                           NA
## S.num.words              NA                           NA
## S.num.words.unq          NA                           NA
## UniqueID                 NA                           NA
## WordCount                NA                           NA
##                     Final.glm.importance
## WordCount.log               1.000000e+02
## myCategory.fctr             7.930984e+01
## H.is.question               4.627526e+01
## H.today                     2.908156e+01
## PubDate.wkday.fctr          2.056149e+01
## S.said                      1.678465e+01
## PubDate.last10.log          1.576922e+01
## PubDate.minute.fctr         1.525545e+01
## S.report                    1.438997e+01
## PubDate.hour.fctr           1.284881e+01
## PubDate.second.fctr         1.129159e+01
## H.report                    1.047475e+01
## H.new                       1.002056e+01
## S.share                     9.905995e+00
## H.day                       9.493498e+00
## H.has.ebola                 9.037198e+00
## S.year                      8.991744e+00
## H.week                      8.986065e+00
## S.newyork                   8.487719e+00
## S.intern                    8.063434e+00
## S.compani                   8.008086e+00
## PubDate.wkend               7.340533e+00
## H.num.words.unq.log         6.905214e+00
## S.time                      6.659200e+00
## PubDate.date.fctr           5.977029e+00
## PubDate.last1.log           5.945001e+00
## S.num.words.log             5.491119e+00
## A.num.words.log             5.444915e+00
## S.make                      5.003336e+00
## S.show                      4.888742e+00
## S.num.words.unq.log         4.518887e+00
## A.num.words.unq.log         4.403520e+00
## S.articl                    4.233947e+00
## H.num.words.log             4.185458e+00
## S.take                      4.162569e+00
## H.fashion                   4.139981e+00
## H.num.chars.log             3.042436e+00
## S.first                     2.754680e+00
## H.newyork                   2.402194e+00
## H.X2014                     2.389830e+00
## S.presid                    2.235853e+00
## PubDate.last100.log         1.354318e+00
## .rnorm                      3.806649e-01
## A.num.chars.log             3.651270e-01
## S.num.chars.log             1.881888e-01
## S.fashion                   1.623404e-01
## S.day                       9.521177e-02
## S.can                       2.002441e-02
## A.can                       1.982994e-02
## S.new                       6.732769e-03
## A.new                       6.677266e-03
## S.state                     3.956531e-03
## A.state                     3.497952e-03
## S.week                      1.322420e-03
## A.one                       9.392684e-04
## S.one                       8.828017e-04
## A.will                      2.350875e-04
## S.will                      0.000000e+00
## A.articl                              NA
## A.compani                             NA
## A.day                                 NA
## A.fashion                             NA
## A.first                               NA
## A.has.http                            NA
## A.intern                              NA
## A.make                                NA
## A.newyork                             NA
## A.num.chars                           NA
## A.num.words                           NA
## A.num.words.unq                       NA
## A.presid                              NA
## A.report                              NA
## A.said                                NA
## A.share                               NA
## A.show                                NA
## A.take                                NA
## A.time                                NA
## A.week                                NA
## A.year                                NA
## H.daili                               NA
## H.has.http                            NA
## H.num.chars                           NA
## H.num.words                           NA
## H.num.words.unq                       NA
## H.X2015                               NA
## Popular                               NA
## Popular.fctr                          NA
## PubDate.last1                         NA
## PubDate.last10                        NA
## PubDate.last100                       NA
## PubDate.month.fctr                    NA
## PubDate.POSIX                         NA
## PubDate.year.fctr                     NA
## PubDate.zoo                           NA
## S.has.http                            NA
## S.num.chars                           NA
## S.num.words                           NA
## S.num.words.unq                       NA
## UniqueID                              NA
## WordCount                             NA
```

```r
glb_analytics_diag_plots(obs_df=glb_trnent_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnent_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 57
```

![](NYTBlogs_category_files/figure-html/fit.data.training_1-1.png) ![](NYTBlogs_category_files/figure-html/fit.data.training_1-2.png) ![](NYTBlogs_category_files/figure-html/fit.data.training_1-3.png) ![](NYTBlogs_category_files/figure-html/fit.data.training_1-4.png) ![](NYTBlogs_category_files/figure-html/fit.data.training_1-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 1507     1507            N                        0.0003595245
## 6370     6370            Y                        0.7014488561
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
## 2182     2182            Y                         0.002348406
## 4721     4721            Y                         0.003715611
## 1923     1923            Y                         0.005278822
## 6101     6101            Y                         0.007199199
## 5486     5486            Y                         0.008477617
## 863       863            Y                         0.008567067
##      Popular.fctr.predict.Final.glm
## 2182                              N
## 4721                              N
## 1923                              N
## 6101                              N
## 5486                              N
## 863                               N
##      Popular.fctr.predict.Final.glm.accurate
## 2182                                   FALSE
## 4721                                   FALSE
## 1923                                   FALSE
## 6101                                   FALSE
## 5486                                   FALSE
## 863                                    FALSE
##      Popular.fctr.predict.Final.glm.error
## 2182                           -0.3976516
## 4721                           -0.3962844
## 1923                           -0.3947212
## 6101                           -0.3928008
## 5486                           -0.3915224
## 863                            -0.3914329
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 6101     6101            Y                         0.007199199
## 5649     5649            Y                         0.113292818
## 2348     2348            Y                         0.138935131
## 450       450            Y                         0.170978419
## 2665     2665            Y                         0.339238691
## 5563     5563            N                         0.746087463
##      Popular.fctr.predict.Final.glm
## 6101                              N
## 5649                              N
## 2348                              N
## 450                               N
## 2665                              N
## 5563                              Y
##      Popular.fctr.predict.Final.glm.accurate
## 6101                                   FALSE
## 5649                                   FALSE
## 2348                                   FALSE
## 450                                    FALSE
## 2665                                   FALSE
## 5563                                   FALSE
##      Popular.fctr.predict.Final.glm.error
## 6101                          -0.39280080
## 5649                          -0.28670718
## 2348                          -0.26106487
## 450                           -0.22902158
## 2665                          -0.06076131
## 5563                           0.34608746
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 2179     2179            N                           0.9675356
## 4960     4960            N                           0.9694245
## 4975     4975            N                           0.9739778
## 59         59            N                           0.9747921
## 1448     1448            N                           0.9775905
## 4882     4882            N                           0.9825106
##      Popular.fctr.predict.Final.glm
## 2179                              Y
## 4960                              Y
## 4975                              Y
## 59                                Y
## 1448                              Y
## 4882                              Y
##      Popular.fctr.predict.Final.glm.accurate
## 2179                                   FALSE
## 4960                                   FALSE
## 4975                                   FALSE
## 59                                     FALSE
## 1448                                   FALSE
## 4882                                   FALSE
##      Popular.fctr.predict.Final.glm.error
## 2179                            0.5675356
## 4960                            0.5694245
## 4975                            0.5739778
## 59                              0.5747921
## 1448                            0.5775905
## 4882                            0.5825106
```

![](NYTBlogs_category_files/figure-html/fit.data.training_1-6.png) 

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
## 92              Y                         0.014373333
## 693             Y                         0.053057907
## 4020            Y                         0.008854356
## 4721            Y                         0.003715611
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

![](NYTBlogs_category_files/figure-html/fit.data.training_1-7.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          7          1 481.574 563.463   81.89
## 15  predict.data.new          8          0 563.464      NA      NA
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 57
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

![](NYTBlogs_category_files/figure-html/predict.data.new-1.png) 

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

![](NYTBlogs_category_files/figure-html/predict.data.new-2.png) 

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

![](NYTBlogs_category_files/figure-html/predict.data.new-3.png) 

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

![](NYTBlogs_category_files/figure-html/predict.data.new-4.png) 

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

![](NYTBlogs_category_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.glm.prob
## 6753     6753         <NA>                        7.717757e-01
## 7309     7309         <NA>                        6.625997e-08
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

![](NYTBlogs_category_files/figure-html/predict.data.new-6.png) 

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
## [1] 4475  109
```

```r
print(dsp_models_df)
```

```
##                        model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 9             Conditional.X.glm        0.9090909   0.9250838     0.6646557
## 8                 Low.cor.X.glm        0.9013126   0.9245425     0.6533400
## 11    Conditional.X.no.rnorm.rf        0.8920758   0.9174736     0.6033111
## 10 Conditional.X.no.rnorm.rpart        0.8862421   0.7084504     0.5054039
## 1             MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3          Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 5               Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 7       Interact.High.cor.Y.glm        0.7846378   0.8094734     0.3778434
## 4     Max.cor.Y.cv.0.cp.0.rpart        0.7535246   0.6722244     0.2272928
## 6                 Max.cor.Y.glm        0.7015070   0.7289876     0.2355742
## 2       Random.myrandom_classfr        0.1672338   0.4909227     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 9     2119.870                    0.4
## 8     2111.473                    0.3
## 11          NA                    0.4
## 10          NA                    0.7
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 7     3178.524                    0.2
## 4           NA                    0.2
## 6     3664.292                    0.2
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
##         N 1631   82
##         Y  105  239
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
## 9                  Business#Technology#    114    113    0.060427807
## 10                        Culture#Arts#    225    244    0.130481283
## 16                      Science#Health#     66     57    0.030481283
## 8            Business#Crosswords/Games#     40     42    0.022459893
## 13                 Metro#N.Y. / Region#     60     67    0.035828877
## 4            #Opinion#The Public Editor     10     10    0.005347594
## 20                             TStyle##    221    105    0.056149733
## 3              #Opinion#Room For Debate     21     24    0.012834225
## 7  Business#Business Day#Small Business     45     42    0.022459893
## 17                      Styles##Fashion     41     15    0.008021390
## 2                          #Multimedia#     42     52    0.027807487
## 5                       #U.S.#Education     93     90    0.048128342
## 11                       Foreign#World#     47     47    0.025133690
## 12           Foreign#World#Asia Pacific     61     56    0.029946524
## 14                              myOther     13      3    0.001604278
## 19                       Travel#Travel#     31     35    0.018716578
##    .freqRatio.OOB accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 1     0.197860963                 36               371        0.9115479
## 6     0.151677200                 32               280        0.8974359
## 15    0.074866310                 27               127        0.8246753
## 18    0.026251823                 22                32        0.5925926
## 9     0.055420515                 16                98        0.8596491
## 10    0.109382596                 16               209        0.9288889
## 16    0.032085561                 16                50        0.7575758
## 8     0.019445795                  6                34        0.8500000
## 13    0.029168692                  5                55        0.9166667
## 4     0.004861449                  3                 7        0.7000000
## 20    0.107438017                  3               218        0.9864253
## 3     0.010209042                  2                19        0.9047619
## 7     0.021876519                  2                43        0.9555556
## 17    0.019931940                  1                40        0.9756098
## 2     0.020418085                  0                42        1.0000000
## 5     0.045211473                  0                93        1.0000000
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
## [2] Popular.fctr.predict.Conditional.X.glm.prob    
## [3] Popular.fctr.predict.Conditional.X.glm         
## [4] Popular.fctr.predict.Conditional.X.glm.accurate
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
##       PubDate.POSIX             H.X2014             H.X2015 
##                   0                   0                   0 
##             H.daili               H.day           H.fashion 
##                   0                   0                   0 
##               H.new           H.newyork            H.report 
##                   0                   0                   0 
##             H.today              H.week          H.has.http 
##                   0                   0                   0 
##         H.has.ebola       H.is.question         H.num.chars 
##                   0                   0                   0 
##         H.num.words     H.num.words.unq     H.num.chars.log 
##                   0                   0                   0 
##     H.num.words.log H.num.words.unq.log            S.articl 
##                   0                   0                   0 
##               S.can           S.compani               S.day 
##                   0                   0                   0 
##           S.fashion             S.first            S.intern 
##                   0                   0                   0 
##              S.make               S.new           S.newyork 
##                   0                   0                   0 
##               S.one            S.presid            S.report 
##                   0                   0                   0 
##              S.said             S.share              S.show 
##                   0                   0                   0 
##             S.state              S.take              S.time 
##                   0                   0                   0 
##              S.week              S.will              S.year 
##                   0                   0                   0 
##          S.has.http         S.num.chars         S.num.words 
##                   0                   0                   0 
##     S.num.words.unq     S.num.chars.log     S.num.words.log 
##                   0                   0                   0 
## S.num.words.unq.log            A.articl               A.can 
##                   0                   0                   0 
##           A.compani               A.day           A.fashion 
##                   0                   0                   0 
##             A.first            A.intern              A.make 
##                   0                   0                   0 
##               A.new           A.newyork               A.one 
##                   0                   0                   0 
##            A.presid            A.report              A.said 
##                   0                   0                   0 
##             A.share              A.show             A.state 
##                   0                   0                   0 
##              A.take              A.time              A.week 
##                   0                   0                   0 
##              A.will              A.year          A.has.http 
##                   0                   0                   0 
##         A.num.chars         A.num.words     A.num.words.unq 
##                   0                   0                   0 
##     A.num.chars.log     A.num.words.log A.num.words.unq.log 
##                   0                   0                   0
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
print(subset(glb_feats_df, !is.na(importance))[,
    c("is.ConditionalX.y", 
      grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
```

```
##                     is.ConditionalX.y   importance
## WordCount.log                    TRUE 1.000000e+02
## myCategory.fctr                  TRUE 7.930984e+01
## H.is.question                    TRUE 4.627526e+01
## H.today                          TRUE 2.908156e+01
## PubDate.wkday.fctr               TRUE 2.056149e+01
## S.said                           TRUE 1.678465e+01
## PubDate.last10.log               TRUE 1.576922e+01
## PubDate.minute.fctr              TRUE 1.525545e+01
## S.report                         TRUE 1.438997e+01
## PubDate.hour.fctr                TRUE 1.284881e+01
## PubDate.second.fctr              TRUE 1.129159e+01
## H.report                         TRUE 1.047475e+01
## H.new                            TRUE 1.002056e+01
## S.share                          TRUE 9.905995e+00
## H.day                            TRUE 9.493498e+00
## H.has.ebola                      TRUE 9.037198e+00
## S.year                           TRUE 8.991744e+00
## H.week                           TRUE 8.986065e+00
## S.newyork                        TRUE 8.487719e+00
## S.intern                         TRUE 8.063434e+00
## S.compani                        TRUE 8.008086e+00
## PubDate.wkend                    TRUE 7.340533e+00
## H.num.words.unq.log              TRUE 6.905214e+00
## S.time                           TRUE 6.659200e+00
## PubDate.date.fctr                TRUE 5.977029e+00
## PubDate.last1.log                TRUE 5.945001e+00
## S.num.words.log                  TRUE 5.491119e+00
## A.num.words.log                  TRUE 5.444915e+00
## S.make                           TRUE 5.003336e+00
## S.show                           TRUE 4.888742e+00
## S.num.words.unq.log              TRUE 4.518887e+00
## A.num.words.unq.log              TRUE 4.403520e+00
## S.articl                         TRUE 4.233947e+00
## H.num.words.log                  TRUE 4.185458e+00
## S.take                           TRUE 4.162569e+00
## H.fashion                        TRUE 4.139981e+00
## H.num.chars.log                  TRUE 3.042436e+00
## S.first                          TRUE 2.754680e+00
## H.newyork                        TRUE 2.402194e+00
## H.X2014                          TRUE 2.389830e+00
## S.presid                         TRUE 2.235853e+00
## PubDate.last100.log              TRUE 1.354318e+00
## .rnorm                           TRUE 3.806649e-01
## A.num.chars.log                  TRUE 3.651270e-01
## S.num.chars.log                  TRUE 1.881888e-01
## S.fashion                        TRUE 1.623404e-01
## S.day                            TRUE 9.521177e-02
## S.can                            TRUE 2.002441e-02
## A.can                            TRUE 1.982994e-02
## S.new                            TRUE 6.732769e-03
## A.new                            TRUE 6.677266e-03
## S.state                          TRUE 3.956531e-03
## A.state                          TRUE 3.497952e-03
## S.week                           TRUE 1.322420e-03
## A.one                            TRUE 9.392684e-04
## S.one                            TRUE 8.828017e-04
## A.will                           TRUE 2.350875e-04
## S.will                           TRUE 0.000000e+00
##                     Conditional.X.glm.importance Final.glm.importance
## WordCount.log                       1.000000e+02         1.000000e+02
## myCategory.fctr                     7.930984e+01         7.930984e+01
## H.is.question                       4.627526e+01         4.627526e+01
## H.today                             2.908156e+01         2.908156e+01
## PubDate.wkday.fctr                  2.056149e+01         2.056149e+01
## S.said                              1.678465e+01         1.678465e+01
## PubDate.last10.log                  1.576922e+01         1.576922e+01
## PubDate.minute.fctr                 1.525545e+01         1.525545e+01
## S.report                            1.438997e+01         1.438997e+01
## PubDate.hour.fctr                   1.284881e+01         1.284881e+01
## PubDate.second.fctr                 1.129159e+01         1.129159e+01
## H.report                            1.047475e+01         1.047475e+01
## H.new                               1.002056e+01         1.002056e+01
## S.share                             9.905995e+00         9.905995e+00
## H.day                               9.493498e+00         9.493498e+00
## H.has.ebola                         9.037198e+00         9.037198e+00
## S.year                              8.991744e+00         8.991744e+00
## H.week                              8.986065e+00         8.986065e+00
## S.newyork                           8.487719e+00         8.487719e+00
## S.intern                            8.063434e+00         8.063434e+00
## S.compani                           8.008086e+00         8.008086e+00
## PubDate.wkend                       7.340533e+00         7.340533e+00
## H.num.words.unq.log                 6.905214e+00         6.905214e+00
## S.time                              6.659200e+00         6.659200e+00
## PubDate.date.fctr                   5.977029e+00         5.977029e+00
## PubDate.last1.log                   5.945001e+00         5.945001e+00
## S.num.words.log                     5.491119e+00         5.491119e+00
## A.num.words.log                     5.444915e+00         5.444915e+00
## S.make                              5.003336e+00         5.003336e+00
## S.show                              4.888742e+00         4.888742e+00
## S.num.words.unq.log                 4.518887e+00         4.518887e+00
## A.num.words.unq.log                 4.403520e+00         4.403520e+00
## S.articl                            4.233947e+00         4.233947e+00
## H.num.words.log                     4.185458e+00         4.185458e+00
## S.take                              4.162569e+00         4.162569e+00
## H.fashion                           4.139981e+00         4.139981e+00
## H.num.chars.log                     3.042436e+00         3.042436e+00
## S.first                             2.754680e+00         2.754680e+00
## H.newyork                           2.402194e+00         2.402194e+00
## H.X2014                             2.389830e+00         2.389830e+00
## S.presid                            2.235853e+00         2.235853e+00
## PubDate.last100.log                 1.354318e+00         1.354318e+00
## .rnorm                              3.806649e-01         3.806649e-01
## A.num.chars.log                     3.651270e-01         3.651270e-01
## S.num.chars.log                     1.881888e-01         1.881888e-01
## S.fashion                           1.623404e-01         1.623404e-01
## S.day                               9.521177e-02         9.521177e-02
## S.can                               2.002441e-02         2.002441e-02
## A.can                               1.982994e-02         1.982994e-02
## S.new                               6.732769e-03         6.732769e-03
## A.new                               6.677266e-03         6.677266e-03
## S.state                             3.956531e-03         3.956531e-03
## A.state                             3.497952e-03         3.497952e-03
## S.week                              1.322420e-03         1.322420e-03
## A.one                               9.392684e-04         9.392684e-04
## S.one                               8.828017e-04         8.828017e-04
## A.will                              2.350875e-04         2.350875e-04
## S.will                              0.000000e+00         0.000000e+00
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
## A.day                  TRUE         NA                           NA
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
## A.take                 TRUE         NA                           NA
## A.time                 TRUE         NA                           NA
## A.week                 TRUE         NA                           NA
## A.year                 TRUE         NA                           NA
##           Final.glm.importance
## A.articl                    NA
## A.compani                   NA
## A.day                       NA
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
## A.take                      NA
## A.time                      NA
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
## 15     predict.data.new          8          0 563.464 646.016  82.552
## 16 display.session.info          9          0 646.016      NA      NA
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
## 10              fit.models          6          1 185.620 385.057 199.437
## 6         extract.features          3          0  39.854 131.319  91.465
## 15        predict.data.new          8          0 563.464 646.016  82.552
## 14       fit.data.training          7          1 481.574 563.463  81.890
## 12              fit.models          6          3 401.907 465.096  63.189
## 9               fit.models          6          0 142.398 185.619  43.221
## 2             inspect.data          2          0  12.053  30.630  18.577
## 11              fit.models          6          2 385.057 401.906  16.850
## 13       fit.data.training          7          0 465.096 481.573  16.477
## 7          select.features          4          0 131.320 140.925   9.605
## 4      manage.missing.data          2          2  35.014  39.793   4.779
## 3             cleanse.data          2          1  30.630  35.014   4.384
## 8  partition.data.training          5          0 140.925 142.398   1.473
## 1              import.data          1          0  11.062  12.053   0.991
## 5              encode.data          2          3  39.794  39.853   0.060
##    duration
## 10  199.437
## 6    91.465
## 15   82.552
## 14   81.889
## 12   63.189
## 9    43.221
## 2    18.577
## 11   16.849
## 13   16.477
## 7     9.605
## 4     4.779
## 3     4.384
## 8     1.473
## 1     0.991
## 5     0.059
```

```
## [1] "Total Elapsed Time: 646.016 secs"
```

![](NYTBlogs_category_files/figure-html/display.session.info-1.png) 

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
## [13] plyr_1.8.1          zoo_1.7-12          sqldf_0.4-10       
## [16] RSQLite_1.0.0       DBI_0.3.1           gsubfn_0.6-6       
## [19] proto_0.3-10        reshape2_1.4.1      doBy_4.5-13        
## [22] survival_2.38-1     ggplot2_1.0.1      
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
