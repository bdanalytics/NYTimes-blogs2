# NYTimes:Blogs:: Popular classification:: zoo
bdanalytics  

**  **    
**Date: (Wed) May 13, 2015**    

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
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 9.006  NA      NA
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
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 9.006 9.964   0.958
## 2 inspect.data          2          0 9.964    NA      NA
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
#stop("here")
# sav_entity_df <- glb_entity_df
# glb_entity_df <- sav_entity_df

srt_entity_df <- orderBy(~PubDate.POSIX, glb_entity_df)
print(myplot_scatter(subset(srt_entity_df, 
                            PubDate.POSIX < strptime("2014-09-02", "%Y-%m-%d")), 
                            xcol_name="PubDate.POSIX", ycol_name=glb_rsp_var,
                           colorcol_name=glb_rsp_var
                     ))
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-5.png) 

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

![](NYTBlogs_zoo_files/figure-html/inspect.data-6.png) 

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

![](NYTBlogs_zoo_files/figure-html/inspect.data-7.png) 

```r
last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
srt_entity_df[, "PubDate.last10"] <- last10
srt_entity_df[is.na(srt_entity_df$PubDate.last10), "PubDate.last10"] <- 0
srt_entity_df[, "PubDate.last10.log"] <- log(1 + srt_entity_df[, "PubDate.last10"])
print(gp <- myplot_box(df=subset(srt_entity_df, PubDate.last10.log > 0), 
                       ycol_names="PubDate.last10.log", 
                       xcol_name=glb_rsp_var))
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-8.png) 

```r
last100 = as.numeric(merge(z-lag(z, -100), b, all = TRUE))
srt_entity_df[, "PubDate.last100"] <- last100
srt_entity_df[is.na(srt_entity_df$PubDate.last100), "PubDate.last100"] <- 0
srt_entity_df[, "PubDate.last100.log"] <- log(1 + srt_entity_df[, "PubDate.last100"])
print(gp <- myplot_box(df=subset(srt_entity_df, PubDate.last100.log > 0), 
                       ycol_names="PubDate.last100.log", 
                       xcol_name=glb_rsp_var))
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-9.png) 

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

![](NYTBlogs_zoo_files/figure-html/inspect.data-10.png) 

```
## [1] "var: PubDate.date.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-11.png) 

```
## [1] "var: PubDate.wkday.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-12.png) 

```
## [1] "var: PubDate.wkend"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-13.png) 

```
## [1] "var: PubDate.hour.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-14.png) 

```
## [1] "var: PubDate.minute.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-15.png) 

```
## [1] "var: PubDate.second.fctr"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-16.png) 

```
## [1] "var: PubDate.last1.log"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-17.png) 

```
## [1] "var: PubDate.last10.log"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-18.png) 

```
## [1] "var: PubDate.last100.log"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-19.png) 

```
## [1] "var: WordCount.log"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-20.png) 

```
## [1] "var: .rnorm"
```

![](NYTBlogs_zoo_files/figure-html/inspect.data-21.png) 

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
## 2 inspect.data          2          0  9.964 28.416  18.452
## 3 cleanse.data          2          1 28.416     NA      NA
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
#stop("here")
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
## [1] "How a"
```

```r
make_prefix(3882)
```

```
## [1] "Outside"
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
## 1661     1661       0
## 1654     1654       0
## 1829     1829       0
## 1801     1801       0
## 1913     1913       0
## 2051     2051       0
## 2220     2220       0
## 2194     2194       0
## 2324     2324       0
## 2286     2286       0
## 2429     2429       0
## 2397     2397       0
## 2537     2537       0
## 2509     2509       0
## 2501     2501       0
## 2638     2638       0
## 2605     2605       0
## 2773     2773       0
## 2744     2744       0
## 2878     2878       0
## 2850     2850       0
## 2973     2973       0
## 2939     2939       0
## 3071     3071       0
## 3028     3028       0
## 3166     3166       0
## 3110     3110       0
## 3282     3282       0
## 3251     3251       0
## 3374     3374       0
## 3351     3351       0
## 3469     3469       0
## 3569     3569       0
## 3548     3548       0
## 3666     3666       0
## 3648     3648       0
## 3804     3804       0
## 3791     3791       0
## 3905     3905       0
## 3874     3874       0
## 4003     4003       0
## 3970     3970       0
## 4107     4107       0
## 4057     4057       0
## 4203     4203       0
## 4151     4151       0
## 4326     4326       0
## 4293     4293       0
## 4418     4418       0
## 4392     4392       0
## 4520     4520       0
## 4487     4487       0
## 4616     4616       0
## 4567     4567       0
## 4708     4708       0
## 4678     4678       0
## 4829     4829       0
## 4805     4805       0
## 4915     4915       0
## 4886     4886       0
## 4999     4999       0
## 4974     4974       0
## 5107     5107       0
## 5068     5068       0
## 5195     5195       0
## 5162     5162       0
## 5315     5315       0
## 5288     5288       0
## 5396     5396       0
## 5366     5366       0
## 5488     5488       0
## 5449     5449       0
## 5582     5582       0
## 5547     5547       0
## 5676     5676       0
## 5635     5635       0
## 5803     5803       0
## 5768     5768       0
## 5890     5890       0
## 5862     5862       0
## 5985     5985       0
## 5954     5954       0
## 6083     6083       0
## 6045     6045       0
## 6186     6186       0
## 6155     6155       0
## 6296     6296       0
## 6371     6371       0
## 6431     6431       0
## 6617     6617      NA
## 6585     6585      NA
## 6705     6705      NA
## 6668     6668      NA
## 6797     6797      NA
## 6762     6762      NA
## 6889     6889      NA
## 6849     6849      NA
## 6986     6986      NA
## 6954     6954      NA
## 7104     7104      NA
## 7076     7076      NA
## 7191     7191      NA
## 7160     7160      NA
## 7294     7294      NA
## 7261     7261      NA
## 7396     7396      NA
## 7354     7354      NA
## 7483     7483      NA
## 7453     7453      NA
## 7597     7597      NA
## 7563     7563      NA
## 7688     7688      NA
## 7667     7667      NA
## 7813     7813      NA
## 7787     7787      NA
## 7906     7906      NA
## 7880     7880      NA
## 8002     8002      NA
## 7973     7973      NA
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
## 1661                 Today in Small Business: Why Jewelry Stores Hide the Price Tags
## 1654             Today in Congressional Instagram: The Majority Leader Finds Bigfoot
## 1829                                                               Today in Politics
## 1801                   Today in Small Business: A Positive Review on Yelp Goes Viral
## 1913                               Today in Small Business: Mobile Is Not a Priority
## 2051                                     Today in Small Business: Unlimited Vacation
## 2220                                                               Today in Politics
## 2194                       Today in Small Business: Facebook Expands Its Ad Platform
## 2324                                                               Today in Politics
## 2286                                      Today in Small Business: Paper or Plastic?
## 2429                                                               Today in Politics
## 2397      Today in Small Business: 'Bloodletting' at Tony Hsieh's Start-Up Community
## 2537                                                               Today in Politics
## 2509                         Today in Political #ThrowBackThursday: Bloomberg on Ice
## 2501                 Today in Small Business: The Coolest New Businesses in Brooklyn
## 2638                                                               Today in Politics
## 2605                                        Today in Small Business: Hiring Picks Up
## 2773                                                               Today in Politics
## 2744                    Today in Small Business: Is the S.B.A. Going Silicon Valley?
## 2878                                                               Today in Politics
## 2850                                Today in Small Business: A Perfect Yelp Response
## 2973                                                               Today in Politics
## 2939                                         Today in Small Business: The Bacon Boom
## 3071                                                               Today in Politics
## 3028                                 Today in Small Business: When Hashtags Backfire
## 3166                                                               Today in Politics
## 3110                                      Today in Small Business: the Rookie Cookie
## 3282                                                               Today in Politics
## 3251                             Today in Small Business: Why Amazon Must Be Stopped
## 3374                                                               Today in Politics
## 3351                              Today in Small Business: Business Travel and Ebola
## 3469                                                               Today in Politics
## 3569                                                               Today in Politics
## 3548               Today in Small Business: Forget R&eacute;sum&eacute;s. Try Videos
## 3666                                                               Today in Politics
## 3648                 Today in Small Business: Paying Retail Employees $50,000 a Year
## 3804                                                               Today in Politics
## 3791 Today in Small Business: How Hackers Can Stick Businesses With Huge Phone Bills
## 3905                                                               Today in Politics
## 3874                      Today in Small Business: Is Apple Pay the Future of Money?
## 4003                                                               Today in Politics
## 3970                                    Today in Small Business: A Lesson in Pricing
## 4107                                                               Today in Politics
## 4057                          Today in Small Business: 'We're the Uber of Whatever!'
## 4203                                                               Today in Politics
## 4151                    Today in Small Business: Dubious Excuses for Calling in Sick
## 4326                                                               Today in Politics
## 4293                Today in Small Business: When the Ebola Virus Touches a Business
## 4418                                                               Today in Politics
## 4392                        Today in Small Business: Start-Ups With a Social Mission
## 4520                                                               Today in Politics
## 4487                        Today in Small Business: Daring to Close on Thanksgiving
## 4616                                                               Today in Politics
## 4567                      Today in Small Business: Jimmy Kimmel Pitches 'Shark Tank'
## 4708                                                               Today in Politics
## 4678                       Today in Small Business: The Halloween Industrial Complex
## 4829                                                               Today in Politics
## 4805                        Today in Small Business: 'The Yelp of Business Software'
## 4915                                                               Today in Politics
## 4886                             Today in Small Business: Minimum Wage and Marijuana
## 4999                                                               Today in Politics
## 4974                                       Today in Small Business: Election Fallout
## 5107                                                               Today in Politics
## 5068                               Today in Small Business: Veteran-Owned Businesses
## 5195                                                               Today in Politics
## 5162                                        Today in Small Business: Paternity Leave
## 5315                                                               Today in Politics
## 5288                             Today in Small Business: Start-Ups Founded by Women
## 5396                                                               Today in Politics
## 5366                                    Today in Small Business: An S.E.O. Challenge
## 5488                                                               Today in Politics
## 5449                       Today in Small Business: Demise of the Internet Sales Tax
## 5582                                                               Today in Politics
## 5547                              Today in Small Business: Avoiding Bad Yelp Reviews
## 5676                                                               Today in Politics
## 5635            Today in Small Business: 'Next Generation of Lender or Boiler Room?'
## 5803                                                               Today in Politics
## 5768                            Today in Small Business: How Costco Codes Its Prices
## 5890                                                               Today in Politics
## 5862                       Today in Small Business: 'Unrealistic Value Expectations'
## 5985                                                               Today in Politics
## 5954                                Today in Small Business: Pastry, Coffee and Cats
## 6083                                                               Today in Politics
## 6045                        Today in Small Business: Why Typewriters Are Coming Back
## 6186                                                               Today in Politics
## 6155                               Today in Small Business: 'Big Cannabis' Is Coming
## 6296                                                               Today in Politics
## 6371                                                               Today in Politics
## 6431                                                               Today in Politics
## 6617                                                               Today in Politics
## 6585                                     Today in Small Business: 'Mean People Fail'
## 6705                                                               Today in Politics
## 6668                      Today in Small Business: Advance Ticketing for Restaurants
## 6797                                                               Today in Politics
## 6762                               Today in Small Business: Pregnancy Discrimination
## 6889                                                               Today in Politics
## 6849                                                  Today in Small Business: Wages
## 6986                                                               Today in Politics
## 6954                        Today in Small Business: The Best Jobs Numbers in Years?
## 7104                                                               Today in Politics
## 7076                               Today in Small Business: Problems With Apple Pay?
## 7191                                                               Today in Politics
## 7160                                    Today in Small Business: The Mistletoe Drone
## 7294                                                               Today in Politics
## 7261                         Today in Small Business: 'Um, I'm Selling the Business'
## 7396                                                               Today in Politics
## 7354                             Today in Small Business: The Best Start-Ups of 2014
## 7483                                                               Today in Politics
## 7453                                 Today in Small Business: The Future of Payments
## 7597                                                               Today in Politics
## 7563                         Today in Small Business: A Retail Success for Instagram
## 7688                                                               Today in Politics
## 7667                         Today in Small Business: Yelp's Gift to Business Owners
## 7813                                                               Today in Politics
## 7787                             Today in Small Business: The Year's Best Franchises
## 7906                                                               Today in Politics
## 7880                       Today in Small Business: The Best Content Marketing Blogs
## 8002                                                               Today in Politics
## 7973                                  Today in Small Business: Fracking and Gambling
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
## 3        cleanse.data          2          1 28.416 58.677  30.261
## 4 manage.missing.data          2          2 58.677     NA      NA
```

### Step `2.2: manage missing data`

```r
# print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_trnent_df <- na.omit(glb_trnent_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

#stop("here")
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
##  PubDate.last1.log PubDate.last10.log PubDate.last100.log WordCount.log   
##  Min.   : 0.000    Min.   : 0.000     Min.   : 0.00       Min.   :0.6932  
##  1st Qu.: 5.263    1st Qu.: 8.516     1st Qu.:11.37       1st Qu.:5.2679  
##  Median : 6.292    Median : 8.868     Median :11.43       Median :5.9480  
##  Mean   : 6.094    Mean   : 9.048     Mean   :11.49       Mean   :5.8263  
##  3rd Qu.: 7.126    3rd Qu.: 9.424     3rd Qu.:11.78       3rd Qu.:6.6067  
##  Max.   :10.875    Max.   :11.744     Max.   :12.95       Max.   :9.2977  
##                                                           NA's   :109     
##      .rnorm          Headline.pfx       NewsDesk.nb       
##  Min.   :-3.881663   Length:8402        Length:8402       
##  1st Qu.:-0.665043   Class :character   Class :character  
##  Median :-0.004510   Mode  :character   Mode  :character  
##  Mean   :-0.006807                                        
##  3rd Qu.: 0.664125                                        
##  Max.   : 3.356092                                        
##                                                           
##  SectionName.nb     SubsectionName.nb 
##  Length:8402        Length:8402       
##  Class :character   Class :character  
##  Mode  :character   Mode  :character  
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
##  PubDate.last1.log PubDate.last10.log PubDate.last100.log WordCount.log   
##  Min.   : 0.000    Min.   : 0.000     Min.   : 0.00       Min.   :0.6931  
##  1st Qu.: 5.263    1st Qu.: 8.516     1st Qu.:11.37       1st Qu.:5.2679  
##  Median : 6.292    Median : 8.868     Median :11.43       Median :5.9506  
##  Mean   : 6.094    Mean   : 9.048     Mean   :11.49       Mean   :5.8273  
##  3rd Qu.: 7.126    3rd Qu.: 9.424     3rd Qu.:11.78       3rd Qu.:6.6067  
##  Max.   :10.875    Max.   :11.744     Max.   :12.95       Max.   :9.2977  
##                                                                           
##      .rnorm          Headline.pfx       NewsDesk.nb       
##  Min.   :-3.881663   Length:8402        Length:8402       
##  1st Qu.:-0.665043   Class :character   Class :character  
##  Median :-0.004510   Mode  :character   Mode  :character  
##  Mean   :-0.006807                                        
##  3rd Qu.: 0.664125                                        
##  Max.   : 3.356092                                        
##                                                           
##  SectionName.nb     SubsectionName.nb 
##  Length:8402        Length:8402       
##  Class :character   Class :character  
##  Mode  :character   Mode  :character  
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
## 4 manage.missing.data          2          2 58.677 64.974   6.297
## 5         encode.data          2          3 64.975     NA      NA
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
## 5      encode.data          2          3 64.975 65.032   0.057
## 6 extract.features          3          0 65.032     NA      NA
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

#stop("here")
#   Create factors of string variables
print(str_vars <- myfind_chr_cols_df(glb_entity_df))
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
##              label step_major step_minor     bgn     end elapsed
## 6 extract.features          3          0  65.032 151.916  86.884
## 7  select.features          4          0 151.916      NA      NA
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
## WordCount.log                   WordCount.log  0.264960434               0
## WordCount                           WordCount  0.257526549               1
## S.num.words.unq.log       S.num.words.unq.log -0.250796919               0
## A.num.words.unq.log       A.num.words.unq.log -0.250601203               0
## S.num.words.log               S.num.words.log -0.245354135               0
## A.num.words.log               A.num.words.log -0.245073324               0
## S.num.chars.log               S.num.chars.log -0.224692967               0
## A.num.chars.log               A.num.chars.log -0.224548821               0
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
## H.num.chars                       H.num.chars -0.147211183               1
## PubDate.hour.fctr           PubDate.hour.fctr  0.135436805               0
## H.is.question                   H.is.question  0.129154799               0
## PubDate.wkend                   PubDate.wkend  0.106728760               0
## Headline.pfx.fctr           Headline.pfx.fctr -0.100963501               0
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
## PubDate.last10                 PubDate.last10  0.053980930               1
## NewsDesk.nb.fctr             NewsDesk.nb.fctr -0.053443087               0
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
## PubDate.last10.log         PubDate.last10.log  0.049317022               0
## S.show                                 S.show -0.048801740               0
## A.show                                 A.show -0.048801740               0
## PubDate.last1.log           PubDate.last1.log  0.046357515               0
## H.X2014                               H.X2014 -0.046206380               0
## A.day                                   A.day -0.045909684               0
## S.day                                   S.day -0.045649185               0
## SubsectionName.nb.fctr SubsectionName.nb.fctr -0.042936460               0
## PubDate.last100               PubDate.last100  0.039892288               1
## PubDate.wkday.fctr         PubDate.wkday.fctr -0.039801288               0
## PubDate.last1                   PubDate.last1  0.035922671               1
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
## .rnorm                                 .rnorm  0.017561723               0
## PubDate.POSIX                   PubDate.POSIX  0.015683258               1
## PubDate.zoo                       PubDate.zoo  0.015683258               1
## A.has.http                         A.has.http -0.013592603               0
## PubDate.second.fctr       PubDate.second.fctr -0.011879458               0
## UniqueID                             UniqueID  0.011824920               1
## PubDate.date.fctr           PubDate.date.fctr -0.011647558               0
## SectionName.nb.fctr       SectionName.nb.fctr -0.008268577               0
## PubDate.last100.log       PubDate.last100.log -0.007663322               0
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
## WordCount.log          0.264960434
## WordCount              0.257526549
## S.num.words.unq.log    0.250796919
## A.num.words.unq.log    0.250601203
## S.num.words.log        0.245354135
## A.num.words.log        0.245073324
## S.num.chars.log        0.224692967
## A.num.chars.log        0.224548821
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
## H.num.chars            0.147211183
## PubDate.hour.fctr      0.135436805
## H.is.question          0.129154799
## PubDate.wkend          0.106728760
## Headline.pfx.fctr      0.100963501
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
## PubDate.last10         0.053980930
## NewsDesk.nb.fctr       0.053443087
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
## PubDate.last10.log     0.049317022
## S.show                 0.048801740
## A.show                 0.048801740
## PubDate.last1.log      0.046357515
## H.X2014                0.046206380
## A.day                  0.045909684
## S.day                  0.045649185
## SubsectionName.nb.fctr 0.042936460
## PubDate.last100        0.039892288
## PubDate.wkday.fctr     0.039801288
## PubDate.last1          0.035922671
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
## .rnorm                 0.017561723
## PubDate.POSIX          0.015683258
## PubDate.zoo            0.015683258
## A.has.http             0.013592603
## PubDate.second.fctr    0.011879458
## UniqueID               0.011824920
## PubDate.date.fctr      0.011647558
## SectionName.nb.fctr    0.008268577
## PubDate.last100.log    0.007663322
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
## [1] "cor(NewsDesk.nb.fctr, SubsectionName.nb.fctr)=0.9243"
## [1] "cor(Popular.fctr, NewsDesk.nb.fctr)=-0.0534"
## [1] "cor(Popular.fctr, SubsectionName.nb.fctr)=-0.0429"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified SubsectionName.nb.fctr as highly correlated
## with NewsDesk.nb.fctr
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
##                                            id        cor.y exclude.as.feat
## Popular                               Popular  1.000000000               1
## WordCount.log                   WordCount.log  0.264960434               0
## WordCount                           WordCount  0.257526549               1
## PubDate.hour.fctr           PubDate.hour.fctr  0.135436805               0
## H.is.question                   H.is.question  0.129154799               0
## PubDate.wkend                   PubDate.wkend  0.106728760               0
## PubDate.last10                 PubDate.last10  0.053980930               1
## PubDate.last10.log         PubDate.last10.log  0.049317022               0
## PubDate.last1.log           PubDate.last1.log  0.046357515               0
## PubDate.last100               PubDate.last100  0.039892288               1
## PubDate.last1                   PubDate.last1  0.035922671               1
## A.can                                   A.can  0.031498867               0
## S.can                                   S.can  0.029999780               0
## H.has.ebola                       H.has.ebola  0.025881397               0
## S.make                                 S.make  0.023138853               0
## A.make                                 A.make  0.023138853               0
## PubDate.month.fctr         PubDate.month.fctr  0.019148739               1
## .rnorm                                 .rnorm  0.017561723               0
## PubDate.POSIX                   PubDate.POSIX  0.015683258               1
## PubDate.zoo                       PubDate.zoo  0.015683258               1
## UniqueID                             UniqueID  0.011824920               1
## S.one                                   S.one  0.006342094               0
## S.state                               S.state  0.006069626               0
## A.state                               A.state  0.005702163               0
## A.one                                   A.one  0.005696039               0
## S.said                                 S.said  0.001363226               0
## A.said                                 A.said  0.001363226               0
## PubDate.last100.log       PubDate.last100.log -0.007663322               0
## SectionName.nb.fctr       SectionName.nb.fctr -0.008268577               0
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
## SubsectionName.nb.fctr SubsectionName.nb.fctr -0.042936460               0
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
## NewsDesk.nb.fctr             NewsDesk.nb.fctr -0.053443087               0
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
## Headline.pfx.fctr           Headline.pfx.fctr -0.100963501               0
## H.num.chars                       H.num.chars -0.147211183               1
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
## A.num.chars.log               A.num.chars.log -0.224548821               0
## S.num.chars.log               S.num.chars.log -0.224692967               0
## A.num.words.log               A.num.words.log -0.245073324               0
## S.num.words.log               S.num.words.log -0.245354135               0
## A.num.words.unq.log       A.num.words.unq.log -0.250601203               0
## S.num.words.unq.log       S.num.words.unq.log -0.250796919               0
## H.has.http                         H.has.http           NA               0
## S.has.http                         S.has.http           NA               0
## PubDate.year.fctr           PubDate.year.fctr           NA               0
##                          cor.y.abs             cor.high.X
## Popular                1.000000000                   <NA>
## WordCount.log          0.264960434                   <NA>
## WordCount              0.257526549                   <NA>
## PubDate.hour.fctr      0.135436805                   <NA>
## H.is.question          0.129154799                   <NA>
## PubDate.wkend          0.106728760                   <NA>
## PubDate.last10         0.053980930                   <NA>
## PubDate.last10.log     0.049317022                   <NA>
## PubDate.last1.log      0.046357515                   <NA>
## PubDate.last100        0.039892288                   <NA>
## PubDate.last1          0.035922671                   <NA>
## A.can                  0.031498867                  S.can
## S.can                  0.029999780                   <NA>
## H.has.ebola            0.025881397                   <NA>
## S.make                 0.023138853                   <NA>
## A.make                 0.023138853                 S.make
## PubDate.month.fctr     0.019148739                   <NA>
## .rnorm                 0.017561723                   <NA>
## PubDate.POSIX          0.015683258                   <NA>
## PubDate.zoo            0.015683258                   <NA>
## UniqueID               0.011824920                   <NA>
## S.one                  0.006342094                   <NA>
## S.state                0.006069626                   <NA>
## A.state                0.005702163                   <NA>
## A.one                  0.005696039                   <NA>
## S.said                 0.001363226                   <NA>
## A.said                 0.001363226                   <NA>
## PubDate.last100.log    0.007663322                   <NA>
## SectionName.nb.fctr    0.008268577                   <NA>
## PubDate.date.fctr      0.011647558                   <NA>
## PubDate.second.fctr    0.011879458                   <NA>
## A.has.http             0.013592603                   <NA>
## S.presid               0.019828826                   <NA>
## A.presid               0.019828826               S.presid
## S.take                 0.025762398                   <NA>
## A.take                 0.026086108                 S.take
## PubDate.minute.fctr    0.034073846                   <NA>
## S.new                  0.034948520                   <NA>
## A.new                  0.035359447                  S.new
## PubDate.wkday.fctr     0.039801288                   <NA>
## SubsectionName.nb.fctr 0.042936460                   <NA>
## S.day                  0.045649185                   <NA>
## A.day                  0.045909684                  S.day
## H.X2014                0.046206380                   <NA>
## S.show                 0.048801740                   <NA>
## A.show                 0.048801740                 S.show
## S.report               0.050211524                   <NA>
## A.report               0.050211524               S.report
## S.share                0.050329686                   <NA>
## A.share                0.050329686                S.share
## S.year                 0.051146178                   <NA>
## A.year                 0.051146178                 S.year
## S.compani              0.053012962                   <NA>
## A.compani              0.053099633              S.compani
## H.new                  0.053121542                   <NA>
## S.first                0.053388178                   <NA>
## A.first                0.053388178                S.first
## NewsDesk.nb.fctr       0.053443087 SubsectionName.nb.fctr
## S.time                 0.057595102                   <NA>
## A.time                 0.057790617                 S.time
## H.newyork              0.057970095                   <NA>
## S.articl               0.059520554                   <NA>
## A.articl               0.059520554               S.articl
## S.will                 0.060575493                   <NA>
## A.will                 0.061025004                 S.will
## H.day                  0.061669687                   <NA>
## S.newyork              0.062117105                   <NA>
## A.newyork              0.062117105              S.newyork
## H.today                0.063723058                   <NA>
## H.report               0.064948102                   <NA>
## H.X2015                0.066584892                   <NA>
## S.intern               0.068485701                   <NA>
## A.intern               0.068485701               S.intern
## H.daili                0.069192975                   <NA>
## H.week                 0.075105216                   <NA>
## H.fashion              0.081708612                 H.week
## S.week                 0.084814939                   <NA>
## A.week                 0.084814939                 S.week
## S.fashion              0.086446251                   <NA>
## A.fashion              0.086446251              S.fashion
## Headline.pfx.fctr      0.100963501                   <NA>
## H.num.chars            0.147211183                   <NA>
## H.num.chars.log        0.171062360                   <NA>
## A.num.chars            0.177037425                   <NA>
## S.num.chars            0.179331806                   <NA>
## H.num.words            0.186036895                   <NA>
## H.num.words.unq        0.189702157                   <NA>
## H.num.words.log        0.200686356                   <NA>
## A.num.words            0.204211072                   <NA>
## H.num.words.unq.log    0.204496360        H.num.chars.log
## S.num.words            0.206385049                   <NA>
## A.num.words.unq        0.210242145                   <NA>
## S.num.words.unq        0.212102717                   <NA>
## A.num.chars.log        0.224548821                   <NA>
## S.num.chars.log        0.224692967        A.num.chars.log
## A.num.words.log        0.245073324                   <NA>
## S.num.words.log        0.245354135        A.num.words.log
## A.num.words.unq.log    0.250601203                   <NA>
## S.num.words.unq.log    0.250796919        S.num.chars.log
## H.has.http                      NA                   <NA>
## S.has.http                      NA                   <NA>
## PubDate.year.fctr               NA                   <NA>
##                        is.ConditionalX.y is.cor.y.abs.low
## Popular                               NA            FALSE
## WordCount.log                       TRUE            FALSE
## WordCount                             NA            FALSE
## PubDate.hour.fctr                   TRUE            FALSE
## H.is.question                       TRUE            FALSE
## PubDate.wkend                       TRUE            FALSE
## PubDate.last10                        NA            FALSE
## PubDate.last10.log                  TRUE            FALSE
## PubDate.last1.log                   TRUE            FALSE
## PubDate.last100                       NA            FALSE
## PubDate.last1                         NA            FALSE
## A.can                               TRUE            FALSE
## S.can                               TRUE            FALSE
## H.has.ebola                         TRUE            FALSE
## S.make                              TRUE            FALSE
## A.make                              TRUE            FALSE
## PubDate.month.fctr                    NA            FALSE
## .rnorm                              TRUE            FALSE
## PubDate.POSIX                         NA             TRUE
## PubDate.zoo                           NA             TRUE
## UniqueID                              NA             TRUE
## S.one                               TRUE             TRUE
## S.state                             TRUE             TRUE
## A.state                             TRUE             TRUE
## A.one                               TRUE             TRUE
## S.said                              TRUE             TRUE
## A.said                              TRUE             TRUE
## PubDate.last100.log                 TRUE             TRUE
## SectionName.nb.fctr                 TRUE             TRUE
## PubDate.date.fctr                   TRUE             TRUE
## PubDate.second.fctr                 TRUE             TRUE
## A.has.http                         FALSE             TRUE
## S.presid                            TRUE            FALSE
## A.presid                            TRUE            FALSE
## S.take                              TRUE            FALSE
## A.take                              TRUE            FALSE
## PubDate.minute.fctr                 TRUE            FALSE
## S.new                               TRUE            FALSE
## A.new                               TRUE            FALSE
## PubDate.wkday.fctr                  TRUE            FALSE
## SubsectionName.nb.fctr              TRUE            FALSE
## S.day                               TRUE            FALSE
## A.day                               TRUE            FALSE
## H.X2014                             TRUE            FALSE
## S.show                              TRUE            FALSE
## A.show                              TRUE            FALSE
## S.report                            TRUE            FALSE
## A.report                            TRUE            FALSE
## S.share                             TRUE            FALSE
## A.share                             TRUE            FALSE
## S.year                              TRUE            FALSE
## A.year                              TRUE            FALSE
## S.compani                           TRUE            FALSE
## A.compani                           TRUE            FALSE
## H.new                               TRUE            FALSE
## S.first                             TRUE            FALSE
## A.first                             TRUE            FALSE
## NewsDesk.nb.fctr                    TRUE            FALSE
## S.time                              TRUE            FALSE
## A.time                              TRUE            FALSE
## H.newyork                           TRUE            FALSE
## S.articl                            TRUE            FALSE
## A.articl                            TRUE            FALSE
## S.will                              TRUE            FALSE
## A.will                              TRUE            FALSE
## H.day                               TRUE            FALSE
## S.newyork                           TRUE            FALSE
## A.newyork                           TRUE            FALSE
## H.today                             TRUE            FALSE
## H.report                            TRUE            FALSE
## H.X2015                            FALSE            FALSE
## S.intern                            TRUE            FALSE
## A.intern                            TRUE            FALSE
## H.daili                            FALSE            FALSE
## H.week                              TRUE            FALSE
## H.fashion                           TRUE            FALSE
## S.week                              TRUE            FALSE
## A.week                              TRUE            FALSE
## S.fashion                           TRUE            FALSE
## A.fashion                           TRUE            FALSE
## Headline.pfx.fctr                   TRUE            FALSE
## H.num.chars                           NA            FALSE
## H.num.chars.log                     TRUE            FALSE
## A.num.chars                           NA            FALSE
## S.num.chars                           NA            FALSE
## H.num.words                           NA            FALSE
## H.num.words.unq                       NA            FALSE
## H.num.words.log                     TRUE            FALSE
## A.num.words                           NA            FALSE
## H.num.words.unq.log                 TRUE            FALSE
## S.num.words                           NA            FALSE
## A.num.words.unq                       NA            FALSE
## S.num.words.unq                       NA            FALSE
## A.num.chars.log                     TRUE            FALSE
## S.num.chars.log                     TRUE            FALSE
## A.num.words.log                     TRUE            FALSE
## S.num.words.log                     TRUE            FALSE
## A.num.words.unq.log                 TRUE            FALSE
## S.num.words.unq.log                 TRUE            FALSE
## H.has.http                         FALSE               NA
## S.has.http                         FALSE               NA
## PubDate.year.fctr                  FALSE               NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor     bgn     end elapsed
## 7         select.features          4          0 151.916 161.527   9.611
## 8 partition.data.training          5          0 161.527      NA      NA
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
## 1               Business    500    510    0.267379679   0.2479338843
## 2                Culture    243    225    0.129946524   0.1093825960
## 14                  OpEd    205    199    0.109625668   0.0967428294
## 10              myMisc::    199    280    0.106417112   0.1361205639
## 22                TStyle    107    235    0.057219251   0.1142440447
## 5                Foreign    107    111    0.057219251   0.0539620807
## 8            myEducation     93    100    0.049732620   0.0486144871
## 18                Styles     77     81    0.041176471   0.0393777346
## 7                  Metro     66     60    0.035294118   0.0291686923
## 17               Science     57     66    0.030481283   0.0320855615
## 11          myMultimedia     53     42    0.028342246   0.0204180846
## 13          myPolitics::     41     31    0.021925134   0.0150704910
## 21                Travel     31     31    0.016577540   0.0150704910
## 3    Daily Clip Report::     22     17    0.011764706   0.0082644628
## 20   Today in Politics::     21     16    0.011229947   0.0077783179
## 4          First Draft::     14     16    0.007486631   0.0077783179
## 23            Verbatim::     12     13    0.006417112   0.0063198833
## 16 Reporter's Notebook::      7      4    0.003743316   0.0019445795
## 15     Readers Respond::      4      3    0.002139037   0.0014584346
## 6               Magazine      3     13    0.001604278   0.0063198833
## 12        myMultimedia::      3      3    0.001604278   0.0014584346
## 9          myEducation::      3      1    0.001604278   0.0004861449
## 19      The Daily Gift::      2     NA    0.001069519             NA
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
## [1] 102   7
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
## [1] 8402  116
```

```r
print("glb_trnent_df: "); print(dim(glb_trnent_df))
```

```
## [1] "glb_trnent_df: "
```

```
## [1] 6532  115
```

```r
print("glb_fitent_df: "); print(dim(glb_fitent_df))
```

```
## [1] "glb_fitent_df: "
```

```
## [1] 4475  115
```

```r
print("glb_OOBent_df: "); print(dim(glb_OOBent_df))
```

```
## [1] "glb_OOBent_df: "
```

```
## [1] 2057  115
```

```r
print("glb_newent_df: "); print(dim(glb_newent_df))
```

```
## [1] "glb_newent_df: "
```

```
## [1] 1870  115
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
## 8 partition.data.training          5          0 161.527 163.055   1.529
## 9              fit.models          6          0 163.056      NA      NA
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
## 1                       0.66                 0.003         0.5
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
## 1                      0.343                 0.002   0.5007516
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-5.png) 

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
## 1                      0.732                 0.074         0.5
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-7.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-8.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-9.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-10.png) 

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
## 1                      0.599                 0.058   0.7667409
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-11.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-12.png) 

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
## 1                      1.291                 0.069         0.5
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-13.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-14.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-15.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-16.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-17.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-18.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-19.png) 

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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-20.png) 

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
## 1                      1.183                 0.073   0.7314944
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
## [1] "    indep_vars: WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:SubsectionName.nb.fctr, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log"
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
##   3374, 3591, 3755, 4243, 4320
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-21.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-22.png) 

```
## Warning: not plotting observations with leverage one:
##   3374, 3591, 3755, 4243, 4320
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-23.png) 

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
## -2.8192  -0.3997  -0.2077  -0.0001   3.2560  
## 
## Coefficients: (4 not defined because of singularities)
##                                                                                    Estimate
## (Intercept)                                                                      -7.580e+00
## WordCount.log                                                                     1.631e+00
## `WordCount.log:S.can`                                                            -5.402e-03
## `WordCount.log:S.make`                                                            3.636e-03
## `WordCount.log:S.presid`                                                          4.644e-02
## `WordCount.log:S.take`                                                           -3.224e-02
## `WordCount.log:S.new`                                                             1.139e-02
## `WordCount.log:S.day`                                                            -2.683e-02
## `WordCount.log:S.show`                                                           -1.552e-01
## `WordCount.log:S.report`                                                         -2.113e-01
## `WordCount.log:S.share`                                                          -9.771e-02
## `WordCount.log:S.year`                                                           -2.776e-02
## `WordCount.log:S.compani`                                                        -3.938e-02
## `WordCount.log:S.first`                                                          -3.765e-02
## `WordCount.log:SubsectionName.nb.fctrAsia Pacific`                               -6.943e-01
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::Multimedia`                   -6.796e-01
## `WordCount.log:SubsectionName.nb.fctrCulture::Arts`                              -4.746e-01
## `WordCount.log:SubsectionName.nb.fctrDealbook`                                   -4.508e-01
## `WordCount.log:SubsectionName.nb.fctrMagazine::Magazine`                         -3.755e+00
## `WordCount.log:SubsectionName.nb.fctrBusiness::Technology`                       -3.446e-01
## `WordCount.log:SubsectionName.nb.fctrBusiness::Crosswords/Games`                  9.073e-02
## `WordCount.log:SubsectionName.nb.fctrTStyle::TStyle`                             -7.352e-01
## `WordCount.log:SubsectionName.nb.fctrForeign::World`                             -3.712e+00
## `WordCount.log:SubsectionName.nb.fctrOpEd::Opinion`                               1.469e-02
## `WordCount.log:SubsectionName.nb.fctrFashion & Style`                            -1.561e-01
## `WordCount.log:SubsectionName.nb.fctrRoom For Debate`                            -9.636e-01
## `WordCount.log:SubsectionName.nb.fctrEducation`                                  -3.844e+00
## `WordCount.log:SubsectionName.nb.fctrmyMisc::myMisc::`                           -3.872e-01
## `WordCount.log:SubsectionName.nb.fctrMetro::N.Y. / Region`                       -4.657e-01
## `WordCount.log:SubsectionName.nb.fctrSmall Business`                             -7.189e-01
## `WordCount.log:SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`     -3.308e+00
## `WordCount.log:SubsectionName.nb.fctrTravel::Travel`                             -7.516e-01
## `WordCount.log:SubsectionName.nb.fctrThe Public Editor`                           1.057e-01
## `WordCount.log:SubsectionName.nb.fctrmyMisc::Open`                               -4.033e+00
## `WordCount.log:SubsectionName.nb.fctrReaders Respond::Readers Respond::`         -1.664e-01
## `WordCount.log:SubsectionName.nb.fctrPolitics`                                   -3.132e+00
## `WordCount.log:SubsectionName.nb.fctrmyEducation::myEducation::`                  1.415e-01
## `WordCount.log:SubsectionName.nb.fctrSports::Sports`                             -2.630e+00
## `WordCount.log:SubsectionName.nb.fctrmyPolitics::myPolitics::`                   -4.565e-01
## `WordCount.log:SubsectionName.nb.fctrNational::National`                         -3.700e+00
## `WordCount.log:SubsectionName.nb.fctrVerbatim::Verbatim::`                       -4.772e+00
## `WordCount.log:SubsectionName.nb.fctrFirst Draft::First Draft::`                 -5.022e+00
## `WordCount.log:SubsectionName.nb.fctrToday in Politics::Today in Politics::`     -3.084e+00
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::myMultimedia::`               -2.869e+00
## `WordCount.log:SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::` -3.154e+00
## `WordCount.log:SubsectionName.nb.fctrCulture::Culture`                                   NA
## `WordCount.log:SubsectionName.nb.fctrTStyle::Technology`                          2.268e+00
## `WordCount.log:SubsectionName.nb.fctrmyMisc::U.S.`                               -3.459e+00
## `WordCount.log:SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`           -3.870e+00
## `WordCount.log:SubsectionName.nb.fctrmyMisc::Travel`                             -3.914e+00
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `WordCount.log:SubsectionName.nb.fctrmyEducation::Travel`                                NA
## `WordCount.log:SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `WordCount.log:S.time`                                                           -8.225e-02
## `WordCount.log:S.articl`                                                         -3.628e-02
## `WordCount.log:S.will`                                                           -6.283e-02
## `WordCount.log:S.newyork`                                                         1.336e-02
## `WordCount.log:S.intern`                                                         -1.135e-01
## `WordCount.log:H.week`                                                           -7.855e-02
## `WordCount.log:S.week`                                                           -4.382e-03
## `WordCount.log:S.fashion`                                                        -3.570e+00
## `WordCount.log:H.num.chars.log`                                                  -4.168e-02
## `WordCount.log:A.num.chars.log`                                                  -7.308e-01
## `WordCount.log:A.num.words.log`                                                  -3.204e-01
## `WordCount.log:S.num.chars.log`                                                   8.761e-01
##                                                                                  Std. Error
## (Intercept)                                                                       4.970e-01
## WordCount.log                                                                     2.201e-01
## `WordCount.log:S.can`                                                             4.314e-02
## `WordCount.log:S.make`                                                            3.967e-02
## `WordCount.log:S.presid`                                                          4.962e-02
## `WordCount.log:S.take`                                                            5.413e-02
## `WordCount.log:S.new`                                                             3.003e-02
## `WordCount.log:S.day`                                                             5.835e-02
## `WordCount.log:S.show`                                                            6.420e-02
## `WordCount.log:S.report`                                                          6.174e-02
## `WordCount.log:S.share`                                                           6.389e-02
## `WordCount.log:S.year`                                                            4.308e-02
## `WordCount.log:S.compani`                                                         3.851e-02
## `WordCount.log:S.first`                                                           6.152e-02
## `WordCount.log:SubsectionName.nb.fctrAsia Pacific`                                9.644e-02
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::Multimedia`                    1.146e-01
## `WordCount.log:SubsectionName.nb.fctrCulture::Arts`                               4.804e-02
## `WordCount.log:SubsectionName.nb.fctrDealbook`                                    4.297e-02
## `WordCount.log:SubsectionName.nb.fctrMagazine::Magazine`                          4.346e+02
## `WordCount.log:SubsectionName.nb.fctrBusiness::Technology`                        4.762e-02
## `WordCount.log:SubsectionName.nb.fctrBusiness::Crosswords/Games`                  7.171e-02
## `WordCount.log:SubsectionName.nb.fctrTStyle::TStyle`                              7.777e-02
## `WordCount.log:SubsectionName.nb.fctrForeign::World`                              1.826e+02
## `WordCount.log:SubsectionName.nb.fctrOpEd::Opinion`                               4.330e-02
## `WordCount.log:SubsectionName.nb.fctrFashion & Style`                             4.502e-02
## `WordCount.log:SubsectionName.nb.fctrRoom For Debate`                             1.441e-01
## `WordCount.log:SubsectionName.nb.fctrEducation`                                   1.282e+02
## `WordCount.log:SubsectionName.nb.fctrmyMisc::myMisc::`                            4.132e-02
## `WordCount.log:SubsectionName.nb.fctrMetro::N.Y. / Region`                        6.082e-02
## `WordCount.log:SubsectionName.nb.fctrSmall Business`                              9.971e-02
## `WordCount.log:SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`      3.035e+02
## `WordCount.log:SubsectionName.nb.fctrTravel::Travel`                              1.772e-01
## `WordCount.log:SubsectionName.nb.fctrThe Public Editor`                           1.766e-01
## `WordCount.log:SubsectionName.nb.fctrmyMisc::Open`                                1.447e+03
## `WordCount.log:SubsectionName.nb.fctrReaders Respond::Readers Respond::`          1.148e-01
## `WordCount.log:SubsectionName.nb.fctrPolitics`                                    1.208e+03
## `WordCount.log:SubsectionName.nb.fctrmyEducation::myEducation::`                  1.890e-01
## `WordCount.log:SubsectionName.nb.fctrSports::Sports`                              8.972e+02
## `WordCount.log:SubsectionName.nb.fctrmyPolitics::myPolitics::`                    9.446e-02
## `WordCount.log:SubsectionName.nb.fctrNational::National`                          1.460e+03
## `WordCount.log:SubsectionName.nb.fctrVerbatim::Verbatim::`                        6.565e+02
## `WordCount.log:SubsectionName.nb.fctrFirst Draft::First Draft::`                  3.844e+02
## `WordCount.log:SubsectionName.nb.fctrToday in Politics::Today in Politics::`      2.781e+02
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                1.663e+03
## `WordCount.log:SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`  8.174e+02
## `WordCount.log:SubsectionName.nb.fctrCulture::Culture`                                   NA
## `WordCount.log:SubsectionName.nb.fctrTStyle::Technology`                          1.425e+03
## `WordCount.log:SubsectionName.nb.fctrmyMisc::U.S.`                                1.967e+03
## `WordCount.log:SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`            2.325e+03
## `WordCount.log:SubsectionName.nb.fctrmyMisc::Travel`                              2.266e+03
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `WordCount.log:SubsectionName.nb.fctrmyEducation::Travel`                                NA
## `WordCount.log:SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## `WordCount.log:S.time`                                                            4.550e-02
## `WordCount.log:S.articl`                                                          1.178e-01
## `WordCount.log:S.will`                                                            3.535e-02
## `WordCount.log:S.newyork`                                                         4.925e-02
## `WordCount.log:S.intern`                                                          1.207e-01
## `WordCount.log:H.week`                                                            9.491e-02
## `WordCount.log:S.week`                                                            5.021e-02
## `WordCount.log:S.fashion`                                                         1.542e+02
## `WordCount.log:H.num.chars.log`                                                   2.736e-02
## `WordCount.log:A.num.chars.log`                                                   9.116e-01
## `WordCount.log:A.num.words.log`                                                   8.217e-02
## `WordCount.log:S.num.chars.log`                                                   9.099e-01
##                                                                                  z value
## (Intercept)                                                                      -15.252
## WordCount.log                                                                      7.412
## `WordCount.log:S.can`                                                             -0.125
## `WordCount.log:S.make`                                                             0.092
## `WordCount.log:S.presid`                                                           0.936
## `WordCount.log:S.take`                                                            -0.596
## `WordCount.log:S.new`                                                              0.379
## `WordCount.log:S.day`                                                             -0.460
## `WordCount.log:S.show`                                                            -2.418
## `WordCount.log:S.report`                                                          -3.423
## `WordCount.log:S.share`                                                           -1.529
## `WordCount.log:S.year`                                                            -0.645
## `WordCount.log:S.compani`                                                         -1.023
## `WordCount.log:S.first`                                                           -0.612
## `WordCount.log:SubsectionName.nb.fctrAsia Pacific`                                -7.199
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::Multimedia`                    -5.930
## `WordCount.log:SubsectionName.nb.fctrCulture::Arts`                               -9.878
## `WordCount.log:SubsectionName.nb.fctrDealbook`                                   -10.493
## `WordCount.log:SubsectionName.nb.fctrMagazine::Magazine`                          -0.009
## `WordCount.log:SubsectionName.nb.fctrBusiness::Technology`                        -7.236
## `WordCount.log:SubsectionName.nb.fctrBusiness::Crosswords/Games`                   1.265
## `WordCount.log:SubsectionName.nb.fctrTStyle::TStyle`                              -9.454
## `WordCount.log:SubsectionName.nb.fctrForeign::World`                              -0.020
## `WordCount.log:SubsectionName.nb.fctrOpEd::Opinion`                                0.339
## `WordCount.log:SubsectionName.nb.fctrFashion & Style`                             -3.468
## `WordCount.log:SubsectionName.nb.fctrRoom For Debate`                             -6.687
## `WordCount.log:SubsectionName.nb.fctrEducation`                                   -0.030
## `WordCount.log:SubsectionName.nb.fctrmyMisc::myMisc::`                            -9.370
## `WordCount.log:SubsectionName.nb.fctrMetro::N.Y. / Region`                        -7.657
## `WordCount.log:SubsectionName.nb.fctrSmall Business`                              -7.210
## `WordCount.log:SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`      -0.011
## `WordCount.log:SubsectionName.nb.fctrTravel::Travel`                              -4.242
## `WordCount.log:SubsectionName.nb.fctrThe Public Editor`                            0.598
## `WordCount.log:SubsectionName.nb.fctrmyMisc::Open`                                -0.003
## `WordCount.log:SubsectionName.nb.fctrReaders Respond::Readers Respond::`          -1.450
## `WordCount.log:SubsectionName.nb.fctrPolitics`                                    -0.003
## `WordCount.log:SubsectionName.nb.fctrmyEducation::myEducation::`                   0.749
## `WordCount.log:SubsectionName.nb.fctrSports::Sports`                              -0.003
## `WordCount.log:SubsectionName.nb.fctrmyPolitics::myPolitics::`                    -4.833
## `WordCount.log:SubsectionName.nb.fctrNational::National`                          -0.003
## `WordCount.log:SubsectionName.nb.fctrVerbatim::Verbatim::`                        -0.007
## `WordCount.log:SubsectionName.nb.fctrFirst Draft::First Draft::`                  -0.013
## `WordCount.log:SubsectionName.nb.fctrToday in Politics::Today in Politics::`      -0.011
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                -0.002
## `WordCount.log:SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`  -0.004
## `WordCount.log:SubsectionName.nb.fctrCulture::Culture`                                NA
## `WordCount.log:SubsectionName.nb.fctrTStyle::Technology`                           0.002
## `WordCount.log:SubsectionName.nb.fctrmyMisc::U.S.`                                -0.002
## `WordCount.log:SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`            -0.002
## `WordCount.log:SubsectionName.nb.fctrmyMisc::Travel`                              -0.002
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                     NA
## `WordCount.log:SubsectionName.nb.fctrmyEducation::Travel`                             NA
## `WordCount.log:SubsectionName.nb.fctrmyEducation::U.S.`                               NA
## `WordCount.log:S.time`                                                            -1.808
## `WordCount.log:S.articl`                                                          -0.308
## `WordCount.log:S.will`                                                            -1.777
## `WordCount.log:S.newyork`                                                          0.271
## `WordCount.log:S.intern`                                                          -0.941
## `WordCount.log:H.week`                                                            -0.828
## `WordCount.log:S.week`                                                            -0.087
## `WordCount.log:S.fashion`                                                         -0.023
## `WordCount.log:H.num.chars.log`                                                   -1.523
## `WordCount.log:A.num.chars.log`                                                   -0.802
## `WordCount.log:A.num.words.log`                                                   -3.900
## `WordCount.log:S.num.chars.log`                                                    0.963
##                                                                                  Pr(>|z|)
## (Intercept)                                                                       < 2e-16
## WordCount.log                                                                    1.24e-13
## `WordCount.log:S.can`                                                            0.900354
## `WordCount.log:S.make`                                                           0.926980
## `WordCount.log:S.presid`                                                         0.349273
## `WordCount.log:S.take`                                                           0.551435
## `WordCount.log:S.new`                                                            0.704544
## `WordCount.log:S.day`                                                            0.645624
## `WordCount.log:S.show`                                                           0.015607
## `WordCount.log:S.report`                                                         0.000620
## `WordCount.log:S.share`                                                          0.126231
## `WordCount.log:S.year`                                                           0.519236
## `WordCount.log:S.compani`                                                        0.306518
## `WordCount.log:S.first`                                                          0.540607
## `WordCount.log:SubsectionName.nb.fctrAsia Pacific`                               6.07e-13
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::Multimedia`                   3.04e-09
## `WordCount.log:SubsectionName.nb.fctrCulture::Arts`                               < 2e-16
## `WordCount.log:SubsectionName.nb.fctrDealbook`                                    < 2e-16
## `WordCount.log:SubsectionName.nb.fctrMagazine::Magazine`                         0.993107
## `WordCount.log:SubsectionName.nb.fctrBusiness::Technology`                       4.60e-13
## `WordCount.log:SubsectionName.nb.fctrBusiness::Crosswords/Games`                 0.205791
## `WordCount.log:SubsectionName.nb.fctrTStyle::TStyle`                              < 2e-16
## `WordCount.log:SubsectionName.nb.fctrForeign::World`                             0.983783
## `WordCount.log:SubsectionName.nb.fctrOpEd::Opinion`                              0.734324
## `WordCount.log:SubsectionName.nb.fctrFashion & Style`                            0.000525
## `WordCount.log:SubsectionName.nb.fctrRoom For Debate`                            2.28e-11
## `WordCount.log:SubsectionName.nb.fctrEducation`                                  0.976078
## `WordCount.log:SubsectionName.nb.fctrmyMisc::myMisc::`                            < 2e-16
## `WordCount.log:SubsectionName.nb.fctrMetro::N.Y. / Region`                       1.90e-14
## `WordCount.log:SubsectionName.nb.fctrSmall Business`                             5.59e-13
## `WordCount.log:SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`     0.991303
## `WordCount.log:SubsectionName.nb.fctrTravel::Travel`                             2.22e-05
## `WordCount.log:SubsectionName.nb.fctrThe Public Editor`                          0.549602
## `WordCount.log:SubsectionName.nb.fctrmyMisc::Open`                               0.997775
## `WordCount.log:SubsectionName.nb.fctrReaders Respond::Readers Respond::`         0.146970
## `WordCount.log:SubsectionName.nb.fctrPolitics`                                   0.997931
## `WordCount.log:SubsectionName.nb.fctrmyEducation::myEducation::`                 0.454089
## `WordCount.log:SubsectionName.nb.fctrSports::Sports`                             0.997661
## `WordCount.log:SubsectionName.nb.fctrmyPolitics::myPolitics::`                   1.34e-06
## `WordCount.log:SubsectionName.nb.fctrNational::National`                         0.997977
## `WordCount.log:SubsectionName.nb.fctrVerbatim::Verbatim::`                       0.994201
## `WordCount.log:SubsectionName.nb.fctrFirst Draft::First Draft::`                 0.989577
## `WordCount.log:SubsectionName.nb.fctrToday in Politics::Today in Politics::`     0.991153
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::myMultimedia::`               0.998623
## `WordCount.log:SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::` 0.996921
## `WordCount.log:SubsectionName.nb.fctrCulture::Culture`                                 NA
## `WordCount.log:SubsectionName.nb.fctrTStyle::Technology`                         0.998730
## `WordCount.log:SubsectionName.nb.fctrmyMisc::U.S.`                               0.998597
## `WordCount.log:SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`           0.998672
## `WordCount.log:SubsectionName.nb.fctrmyMisc::Travel`                             0.998622
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                      NA
## `WordCount.log:SubsectionName.nb.fctrmyEducation::Travel`                              NA
## `WordCount.log:SubsectionName.nb.fctrmyEducation::U.S.`                                NA
## `WordCount.log:S.time`                                                           0.070640
## `WordCount.log:S.articl`                                                         0.758001
## `WordCount.log:S.will`                                                           0.075529
## `WordCount.log:S.newyork`                                                        0.786252
## `WordCount.log:S.intern`                                                         0.346893
## `WordCount.log:H.week`                                                           0.407908
## `WordCount.log:S.week`                                                           0.930455
## `WordCount.log:S.fashion`                                                        0.981525
## `WordCount.log:H.num.chars.log`                                                  0.127654
## `WordCount.log:A.num.chars.log`                                                  0.422758
## `WordCount.log:A.num.words.log`                                                  9.63e-05
## `WordCount.log:S.num.chars.log`                                                  0.335603
##                                                                                     
## (Intercept)                                                                      ***
## WordCount.log                                                                    ***
## `WordCount.log:S.can`                                                               
## `WordCount.log:S.make`                                                              
## `WordCount.log:S.presid`                                                            
## `WordCount.log:S.take`                                                              
## `WordCount.log:S.new`                                                               
## `WordCount.log:S.day`                                                               
## `WordCount.log:S.show`                                                           *  
## `WordCount.log:S.report`                                                         ***
## `WordCount.log:S.share`                                                             
## `WordCount.log:S.year`                                                              
## `WordCount.log:S.compani`                                                           
## `WordCount.log:S.first`                                                             
## `WordCount.log:SubsectionName.nb.fctrAsia Pacific`                               ***
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::Multimedia`                   ***
## `WordCount.log:SubsectionName.nb.fctrCulture::Arts`                              ***
## `WordCount.log:SubsectionName.nb.fctrDealbook`                                   ***
## `WordCount.log:SubsectionName.nb.fctrMagazine::Magazine`                            
## `WordCount.log:SubsectionName.nb.fctrBusiness::Technology`                       ***
## `WordCount.log:SubsectionName.nb.fctrBusiness::Crosswords/Games`                    
## `WordCount.log:SubsectionName.nb.fctrTStyle::TStyle`                             ***
## `WordCount.log:SubsectionName.nb.fctrForeign::World`                                
## `WordCount.log:SubsectionName.nb.fctrOpEd::Opinion`                                 
## `WordCount.log:SubsectionName.nb.fctrFashion & Style`                            ***
## `WordCount.log:SubsectionName.nb.fctrRoom For Debate`                            ***
## `WordCount.log:SubsectionName.nb.fctrEducation`                                     
## `WordCount.log:SubsectionName.nb.fctrmyMisc::myMisc::`                           ***
## `WordCount.log:SubsectionName.nb.fctrMetro::N.Y. / Region`                       ***
## `WordCount.log:SubsectionName.nb.fctrSmall Business`                             ***
## `WordCount.log:SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`        
## `WordCount.log:SubsectionName.nb.fctrTravel::Travel`                             ***
## `WordCount.log:SubsectionName.nb.fctrThe Public Editor`                             
## `WordCount.log:SubsectionName.nb.fctrmyMisc::Open`                                  
## `WordCount.log:SubsectionName.nb.fctrReaders Respond::Readers Respond::`            
## `WordCount.log:SubsectionName.nb.fctrPolitics`                                      
## `WordCount.log:SubsectionName.nb.fctrmyEducation::myEducation::`                    
## `WordCount.log:SubsectionName.nb.fctrSports::Sports`                                
## `WordCount.log:SubsectionName.nb.fctrmyPolitics::myPolitics::`                   ***
## `WordCount.log:SubsectionName.nb.fctrNational::National`                            
## `WordCount.log:SubsectionName.nb.fctrVerbatim::Verbatim::`                          
## `WordCount.log:SubsectionName.nb.fctrFirst Draft::First Draft::`                    
## `WordCount.log:SubsectionName.nb.fctrToday in Politics::Today in Politics::`        
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                  
## `WordCount.log:SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`    
## `WordCount.log:SubsectionName.nb.fctrCulture::Culture`                              
## `WordCount.log:SubsectionName.nb.fctrTStyle::Technology`                            
## `WordCount.log:SubsectionName.nb.fctrmyMisc::U.S.`                                  
## `WordCount.log:SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`              
## `WordCount.log:SubsectionName.nb.fctrmyMisc::Travel`                                
## `WordCount.log:SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                   
## `WordCount.log:SubsectionName.nb.fctrmyEducation::Travel`                           
## `WordCount.log:SubsectionName.nb.fctrmyEducation::U.S.`                             
## `WordCount.log:S.time`                                                           .  
## `WordCount.log:S.articl`                                                            
## `WordCount.log:S.will`                                                           .  
## `WordCount.log:S.newyork`                                                           
## `WordCount.log:S.intern`                                                            
## `WordCount.log:H.week`                                                              
## `WordCount.log:S.week`                                                              
## `WordCount.log:S.fashion`                                                           
## `WordCount.log:H.num.chars.log`                                                     
## `WordCount.log:A.num.chars.log`                                                     
## `WordCount.log:A.num.words.log`                                                  ***
## `WordCount.log:S.num.chars.log`                                                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 2156.1  on 4414  degrees of freedom
## AIC: 2278.1
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-24.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-25.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.5969499
## 3        0.2 0.6882086
## 4        0.3 0.7101828
## 5        0.4 0.6879886
## 6        0.5 0.6656489
## 7        0.6 0.6370251
## 8        0.7 0.5909879
## 9        0.8 0.5042493
## 10       0.9 0.3354978
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3487
## 2            Y                                            205
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            239
## 2                                            544
##          Prediction
## Reference    N    Y
##         N 3487  239
##         Y  205  544
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.007821e-01   6.503639e-01   8.916480e-01   9.093892e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   5.441999e-39   1.173227e-01 
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
## 2        0.1 0.5910391
## 3        0.2 0.6593407
## 4        0.3 0.6826516
## 5        0.4 0.6850998
## 6        0.5 0.6622735
## 7        0.6 0.6178571
## 8        0.7 0.5518591
## 9        0.8 0.4678112
## 10       0.9 0.3270142
## 11       1.0 0.0000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-28.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1629
## 2            Y                                            121
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             84
## 2                                            223
##          Prediction
## Reference    N    Y
##         N 1629   84
##         Y  121  223
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.003403e-01   6.261297e-01   8.865764e-01   9.129507e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.394337e-18   1.192523e-02 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     feats
## 1 WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:SubsectionName.nb.fctr, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      5.015                 1.273
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9274991                    0.3       0.7101828        0.8994401
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.891648             0.9093892     0.5991558   0.9077031
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.6850998        0.9003403
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8865764             0.9129507     0.6261297    2278.103
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.005097867      0.01354328
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
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, S.can, H.has.ebola, S.make, .rnorm, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log"
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
##   1143, 3374, 3591, 3755, 4243, 4320, 4408
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-29.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-30.png) 

```
## Warning: not plotting observations with leverage one:
##   1143, 3374, 3591, 3755, 4243, 4320, 4408
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-31.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -8.49    0.00    0.00    0.00    8.49  
## 
## Coefficients: (36 not defined because of singularities)
##                                                                      Estimate
## (Intercept)                                                        -1.275e+15
## WordCount.log                                                       4.646e+14
## `PubDate.hour.fctr(7.67,15.3]`                                      1.505e+15
## `PubDate.hour.fctr(15.3,23]`                                        6.988e+14
## H.is.question                                                       8.314e+14
## PubDate.wkend                                                      -3.779e+13
## PubDate.last10.log                                                  6.151e+13
## PubDate.last1.log                                                   4.092e+12
## S.can                                                              -2.058e+14
## H.has.ebola                                                         6.247e+14
## S.make                                                             -1.710e+14
## .rnorm                                                              7.253e+12
## S.one                                                               3.318e+15
## S.state                                                             4.273e+15
## A.state                                                            -3.913e+15
## A.one                                                              -3.441e+15
## S.said                                                              4.028e+14
## A.said                                                                     NA
## PubDate.last100.log                                                -9.467e+12
## SectionName.nb.fctrWorld                                           -2.041e+15
## SectionName.nb.fctrMultimedia                                      -2.971e+15
## SectionName.nb.fctrArts                                            -1.215e+15
## `SectionName.nb.fctrBusiness Day`                                  -2.522e+15
## SectionName.nb.fctrMagazine                                        -4.263e+15
## SectionName.nb.fctrTechnology                                      -1.049e+16
## `SectionName.nb.fctrCrosswords/Games`                               1.522e+15
## SectionName.nb.fctrTStyle                                          -1.651e+15
## SectionName.nb.fctrOpinion                                          1.900e+15
## SectionName.nb.fctrStyles                                          -7.809e+15
## SectionName.nb.fctrU.S.                                            -4.511e+15
## `SectionName.nb.fctrmyMisc::`                                      -2.512e+15
## `SectionName.nb.fctrN.Y. / Region`                                 -3.034e+15
## `SectionName.nb.fctrDaily Clip Report::`                           -1.212e+15
## SectionName.nb.fctrTravel                                          -3.559e+15
## SectionName.nb.fctrOpen                                            -5.058e+15
## `SectionName.nb.fctrReaders Respond::`                             -1.015e+15
## `SectionName.nb.fctrmyEducation::`                                  3.496e+14
## SectionName.nb.fctrSports                                          -2.142e+15
## `SectionName.nb.fctrmyPolitics::`                                  -2.501e+15
## SectionName.nb.fctrNational                                        -4.196e+15
## `SectionName.nb.fctrVerbatim::`                                    -8.370e+14
## `SectionName.nb.fctrFirst Draft::`                                 -1.627e+15
## `SectionName.nb.fctrToday in Politics::`                           -3.470e+15
## `SectionName.nb.fctrmyMultimedia::`                                -5.645e+15
## `SectionName.nb.fctrReporter's Notebook::`                         -2.827e+15
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                               3.127e+15
## `PubDate.date.fctr(7,13]`                                          -3.929e+13
## `PubDate.date.fctr(13,19]`                                         -6.415e+13
## `PubDate.date.fctr(19,25]`                                          4.116e+13
## `PubDate.date.fctr(25,31]`                                          8.934e+13
## `PubDate.second.fctr(14.8,29.5]`                                    1.271e+14
## `PubDate.second.fctr(29.5,44.2]`                                    1.032e+14
## `PubDate.second.fctr(44.2,59.1]`                                   -6.281e+13
## S.presid                                                           -1.580e+14
## S.take                                                             -1.709e+14
## `PubDate.minute.fctr(14.8,29.5]`                                   -7.133e+13
## `PubDate.minute.fctr(29.5,44.2]`                                   -1.965e+14
## `PubDate.minute.fctr(44.2,59.1]`                                   -1.358e+14
## S.new                                                               1.068e+13
## PubDate.wkday.fctr1                                                -3.329e+14
## PubDate.wkday.fctr2                                                -6.058e+14
## PubDate.wkday.fctr3                                                -4.801e+14
## PubDate.wkday.fctr4                                                -5.126e+14
## PubDate.wkday.fctr5                                                -6.612e+14
## PubDate.wkday.fctr6                                                -9.317e+14
## `SubsectionName.nb.fctrAsia Pacific`                               -1.484e+14
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                           NA
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      1.197e+15
## `SubsectionName.nb.fctrMagazine::Magazine`                                 NA
## `SubsectionName.nb.fctrBusiness::Technology`                        8.320e+15
## `SubsectionName.nb.fctrBusiness::Crosswords/Games`                         NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrForeign::World`                                     NA
## `SubsectionName.nb.fctrOpEd::Opinion`                              -1.179e+15
## `SubsectionName.nb.fctrFashion & Style`                             4.946e+15
## `SubsectionName.nb.fctrRoom For Debate`                            -7.580e+15
## SubsectionName.nb.fctrEducation                                    -2.339e+14
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                   NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                               NA
## `SubsectionName.nb.fctrSmall Business`                                     NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`             NA
## `SubsectionName.nb.fctrTravel::Travel`                             -9.250e+14
## `SubsectionName.nb.fctrThe Public Editor`                                  NA
## `SubsectionName.nb.fctrmyMisc::Open`                                       NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                     -9.188e+13
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                          8.191e+15
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## S.day                                                              -2.034e+13
## H.X2014                                                             2.989e+14
## S.show                                                             -2.121e+14
## S.report                                                           -5.304e+14
## S.share                                                            -4.579e+14
## S.year                                                             -1.312e+14
## S.compani                                                          -2.286e+14
## H.new                                                              -5.061e+13
## S.first                                                            -2.832e+13
## S.time                                                             -1.571e+14
## H.newyork                                                           1.146e+14
## S.articl                                                            3.402e+14
## S.will                                                             -4.780e+13
## H.day                                                              -5.093e+13
## S.newyork                                                           3.933e+14
## H.today                                                            -1.389e+15
## H.report                                                            2.443e+14
## S.intern                                                           -2.502e+14
## H.week                                                             -4.402e+14
## S.week                                                             -3.889e+13
## S.fashion                                                          -2.128e+15
## `Headline.pfx.fctrDaily Report::`                                   2.139e+15
## `Headline.pfx.fctrmyEducation::`                                    5.421e+14
## `Headline.pfx.fctr19[0-9][0-9]::`                                  -1.293e+15
## `Headline.pfx.fctrWord of the Day::`                                3.690e+14
## `Headline.pfx.fctrTest Yourself::`                                  1.977e+15
## `Headline.pfx.fctr6 Q's About the News::`                           1.666e+15
## `Headline.pfx.fctrNew York Today::`                                 1.250e+15
## `Headline.pfx.fctrMorning Agenda::`                                -1.598e+15
## `Headline.pfx.fctrDaily Clip Report::`                                     NA
## `Headline.pfx.fctrToday in Small Business::`                       -9.705e+14
## `Headline.pfx.fctr.*Fashion Week::`                                 7.503e+14
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                  -1.008e+14
## `Headline.pfx.fctrWhat We're::`                                    -5.517e+15
## `Headline.pfx.fctrmyTech::`                                         5.922e+14
## `Headline.pfx.fctrmyMultimedia::`                                   8.951e+14
## `Headline.pfx.fctrmyFood::`                                        -8.546e+14
## `Headline.pfx.fctrmyPolitics::`                                     1.011e+15
## `Headline.pfx.fctrAsk Well::`                                      -1.576e+15
## `Headline.pfx.fctrReaders Respond::`                               -2.999e+14
## `Headline.pfx.fctrYour Turn::`                                     -1.990e+15
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           2.196e+15
## `Headline.pfx.fctrVerbatim::`                                              NA
## `Headline.pfx.fctrFirst Draft::`                                           NA
## `Headline.pfx.fctrOn This Day::`                                    5.814e+14
## `Headline.pfx.fctrToday in Politics::`                                     NA
## `Headline.pfx.fctrReporter's Notebook::`                                   NA
## `Headline.pfx.fctrThe Daily Gift::`                                -2.464e+15
## H.num.chars.log                                                    -2.687e+14
## H.num.words.log                                                    -4.886e+14
## A.num.chars.log                                                     3.645e+14
## A.num.words.log                                                    -9.803e+14
## A.num.words.unq.log                                                 1.495e+14
##                                                                    Std. Error
## (Intercept)                                                         3.481e+07
## WordCount.log                                                       1.264e+06
## `PubDate.hour.fctr(7.67,15.3]`                                      3.930e+06
## `PubDate.hour.fctr(15.3,23]`                                        4.171e+06
## H.is.question                                                       4.592e+06
## PubDate.wkend                                                       7.728e+06
## PubDate.last10.log                                                  1.960e+06
## PubDate.last1.log                                                   7.092e+05
## S.can                                                               5.657e+06
## H.has.ebola                                                         9.241e+06
## S.make                                                              5.181e+06
## .rnorm                                                              1.027e+06
## S.one                                                               6.831e+07
## S.state                                                             6.830e+07
## A.state                                                             6.813e+07
## A.one                                                               6.820e+07
## S.said                                                              5.375e+06
## A.said                                                                     NA
## PubDate.last100.log                                                 7.746e+05
## SectionName.nb.fctrWorld                                            1.664e+07
## SectionName.nb.fctrMultimedia                                       1.111e+07
## SectionName.nb.fctrArts                                             7.209e+06
## `SectionName.nb.fctrBusiness Day`                                   1.095e+07
## SectionName.nb.fctrMagazine                                         1.728e+07
## SectionName.nb.fctrTechnology                                       8.350e+07
## `SectionName.nb.fctrCrosswords/Games`                               1.037e+07
## SectionName.nb.fctrTStyle                                           7.287e+06
## SectionName.nb.fctrOpinion                                          2.269e+07
## SectionName.nb.fctrStyles                                           6.862e+07
## SectionName.nb.fctrU.S.                                             6.759e+07
## `SectionName.nb.fctrmyMisc::`                                       6.908e+06
## `SectionName.nb.fctrN.Y. / Region`                                  9.616e+06
## `SectionName.nb.fctrDaily Clip Report::`                            2.039e+07
## SectionName.nb.fctrTravel                                           6.790e+07
## SectionName.nb.fctrOpen                                             4.812e+07
## `SectionName.nb.fctrReaders Respond::`                              3.651e+07
## `SectionName.nb.fctrmyEducation::`                                  3.318e+07
## SectionName.nb.fctrSports                                           4.999e+07
## `SectionName.nb.fctrmyPolitics::`                                   1.700e+07
## SectionName.nb.fctrNational                                         4.833e+07
## `SectionName.nb.fctrVerbatim::`                                     1.666e+07
## `SectionName.nb.fctrFirst Draft::`                                  1.250e+07
## `SectionName.nb.fctrToday in Politics::`                            2.935e+07
## `SectionName.nb.fctrmyMultimedia::`                                 7.036e+07
## `SectionName.nb.fctrReporter's Notebook::`                          3.556e+07
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                               6.977e+07
## `PubDate.date.fctr(7,13]`                                           3.214e+06
## `PubDate.date.fctr(13,19]`                                          3.166e+06
## `PubDate.date.fctr(19,25]`                                          3.072e+06
## `PubDate.date.fctr(25,31]`                                          3.421e+06
## `PubDate.second.fctr(14.8,29.5]`                                    2.859e+06
## `PubDate.second.fctr(29.5,44.2]`                                    2.816e+06
## `PubDate.second.fctr(44.2,59.1]`                                    2.880e+06
## S.presid                                                            5.534e+06
## S.take                                                              5.836e+06
## `PubDate.minute.fctr(14.8,29.5]`                                    2.947e+06
## `PubDate.minute.fctr(29.5,44.2]`                                    2.809e+06
## `PubDate.minute.fctr(44.2,59.1]`                                    3.021e+06
## S.new                                                               3.618e+06
## PubDate.wkday.fctr1                                                 9.389e+06
## PubDate.wkday.fctr2                                                 9.989e+06
## PubDate.wkday.fctr3                                                 9.909e+06
## PubDate.wkday.fctr4                                                 9.777e+06
## PubDate.wkday.fctr5                                                 9.913e+06
## PubDate.wkday.fctr6                                                 7.881e+06
## `SubsectionName.nb.fctrAsia Pacific`                                1.657e+07
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                           NA
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      9.443e+06
## `SubsectionName.nb.fctrMagazine::Magazine`                                 NA
## `SubsectionName.nb.fctrBusiness::Technology`                        8.317e+07
## `SubsectionName.nb.fctrBusiness::Crosswords/Games`                         NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrForeign::World`                                     NA
## `SubsectionName.nb.fctrOpEd::Opinion`                               2.201e+07
## `SubsectionName.nb.fctrFashion & Style`                             6.762e+07
## `SubsectionName.nb.fctrRoom For Debate`                             2.409e+07
## SubsectionName.nb.fctrEducation                                     6.779e+07
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                   NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                               NA
## `SubsectionName.nb.fctrSmall Business`                                     NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`             NA
## `SubsectionName.nb.fctrTravel::Travel`                              6.794e+07
## `SubsectionName.nb.fctrThe Public Editor`                                  NA
## `SubsectionName.nb.fctrmyMisc::Open`                                       NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                      8.305e+07
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                          1.072e+08
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## S.day                                                               6.561e+06
## H.X2014                                                             9.634e+06
## S.show                                                              5.526e+06
## S.report                                                            6.504e+06
## S.share                                                             6.282e+06
## S.year                                                              4.639e+06
## S.compani                                                           4.423e+06
## H.new                                                               5.526e+06
## S.first                                                             5.688e+06
## S.time                                                              4.555e+06
## H.newyork                                                           7.149e+06
## S.articl                                                            1.075e+07
## S.will                                                              3.560e+06
## H.day                                                               9.326e+06
## S.newyork                                                           5.100e+06
## H.today                                                             2.562e+07
## H.report                                                            9.793e+06
## S.intern                                                            9.755e+06
## H.week                                                              1.151e+07
## S.week                                                              4.967e+06
## S.fashion                                                           7.649e+06
## `Headline.pfx.fctrDaily Report::`                                   1.613e+07
## `Headline.pfx.fctrmyEducation::`                                    1.224e+07
## `Headline.pfx.fctr19[0-9][0-9]::`                                   1.540e+07
## `Headline.pfx.fctrWord of the Day::`                                1.995e+07
## `Headline.pfx.fctrTest Yourself::`                                  1.554e+07
## `Headline.pfx.fctr6 Q's About the News::`                           1.298e+07
## `Headline.pfx.fctrNew York Today::`                                 2.915e+07
## `Headline.pfx.fctrMorning Agenda::`                                 1.158e+07
## `Headline.pfx.fctrDaily Clip Report::`                                     NA
## `Headline.pfx.fctrToday in Small Business::`                        2.969e+07
## `Headline.pfx.fctr.*Fashion Week::`                                 1.402e+07
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                   1.816e+07
## `Headline.pfx.fctrWhat We're::`                                     1.512e+07
## `Headline.pfx.fctrmyTech::`                                         8.717e+06
## `Headline.pfx.fctrmyMultimedia::`                                   1.498e+07
## `Headline.pfx.fctrmyFood::`                                         9.125e+06
## `Headline.pfx.fctrmyPolitics::`                                     1.315e+07
## `Headline.pfx.fctrAsk Well::`                                       2.586e+07
## `Headline.pfx.fctrReaders Respond::`                                2.819e+07
## `Headline.pfx.fctrYour Turn::`                                      2.753e+07
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           2.967e+07
## `Headline.pfx.fctrVerbatim::`                                              NA
## `Headline.pfx.fctrFirst Draft::`                                           NA
## `Headline.pfx.fctrOn This Day::`                                    2.602e+07
## `Headline.pfx.fctrToday in Politics::`                                     NA
## `Headline.pfx.fctrReporter's Notebook::`                                   NA
## `Headline.pfx.fctrThe Daily Gift::`                                 1.881e+07
## H.num.chars.log                                                     6.546e+06
## H.num.words.log                                                     7.751e+06
## A.num.chars.log                                                     8.730e+06
## A.num.words.log                                                     2.885e+07
## A.num.words.unq.log                                                 2.765e+07
##                                                                       z value
## (Intercept)                                                         -36642194
## WordCount.log                                                       367606066
## `PubDate.hour.fctr(7.67,15.3]`                                      383055425
## `PubDate.hour.fctr(15.3,23]`                                        167553548
## H.is.question                                                       181058188
## PubDate.wkend                                                        -4890074
## PubDate.last10.log                                                   31386454
## PubDate.last1.log                                                     5770822
## S.can                                                               -36374291
## H.has.ebola                                                          67602894
## S.make                                                              -33011073
## .rnorm                                                                7063392
## S.one                                                                48572805
## S.state                                                              62566540
## A.state                                                             -57431758
## A.one                                                               -50448841
## S.said                                                               74950550
## A.said                                                                     NA
## PubDate.last100.log                                                 -12221024
## SectionName.nb.fctrWorld                                           -122638389
## SectionName.nb.fctrMultimedia                                      -267405845
## SectionName.nb.fctrArts                                            -168520493
## `SectionName.nb.fctrBusiness Day`                                  -230260605
## SectionName.nb.fctrMagazine                                        -246783636
## SectionName.nb.fctrTechnology                                      -125667438
## `SectionName.nb.fctrCrosswords/Games`                               146768878
## SectionName.nb.fctrTStyle                                          -226526138
## SectionName.nb.fctrOpinion                                           83761112
## SectionName.nb.fctrStyles                                          -113798319
## SectionName.nb.fctrU.S.                                             -66743533
## `SectionName.nb.fctrmyMisc::`                                      -363640902
## `SectionName.nb.fctrN.Y. / Region`                                 -315485166
## `SectionName.nb.fctrDaily Clip Report::`                            -59465697
## SectionName.nb.fctrTravel                                           -52409185
## SectionName.nb.fctrOpen                                            -105096437
## `SectionName.nb.fctrReaders Respond::`                              -27806615
## `SectionName.nb.fctrmyEducation::`                                   10537070
## SectionName.nb.fctrSports                                           -42844318
## `SectionName.nb.fctrmyPolitics::`                                  -147090314
## SectionName.nb.fctrNational                                         -86820659
## `SectionName.nb.fctrVerbatim::`                                     -50238679
## `SectionName.nb.fctrFirst Draft::`                                 -130146881
## `SectionName.nb.fctrToday in Politics::`                           -118224091
## `SectionName.nb.fctrmyMultimedia::`                                 -80230928
## `SectionName.nb.fctrReporter's Notebook::`                          -79499999
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                                44820587
## `PubDate.date.fctr(7,13]`                                           -12223378
## `PubDate.date.fctr(13,19]`                                          -20263981
## `PubDate.date.fctr(19,25]`                                           13397421
## `PubDate.date.fctr(25,31]`                                           26118009
## `PubDate.second.fctr(14.8,29.5]`                                     44463237
## `PubDate.second.fctr(29.5,44.2]`                                     36662728
## `PubDate.second.fctr(44.2,59.1]`                                    -21808197
## S.presid                                                            -28545546
## S.take                                                              -29275047
## `PubDate.minute.fctr(14.8,29.5]`                                    -24205131
## `PubDate.minute.fctr(29.5,44.2]`                                    -69971628
## `PubDate.minute.fctr(44.2,59.1]`                                    -44946892
## S.new                                                                 2952838
## PubDate.wkday.fctr1                                                 -35452859
## PubDate.wkday.fctr2                                                 -60645725
## PubDate.wkday.fctr3                                                 -48451206
## PubDate.wkday.fctr4                                                 -52426227
## PubDate.wkday.fctr5                                                 -66693243
## PubDate.wkday.fctr6                                                -118221918
## `SubsectionName.nb.fctrAsia Pacific`                                 -8953187
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                           NA
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      126727341
## `SubsectionName.nb.fctrMagazine::Magazine`                                 NA
## `SubsectionName.nb.fctrBusiness::Technology`                        100040061
## `SubsectionName.nb.fctrBusiness::Crosswords/Games`                         NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrForeign::World`                                     NA
## `SubsectionName.nb.fctrOpEd::Opinion`                               -53566196
## `SubsectionName.nb.fctrFashion & Style`                              73144112
## `SubsectionName.nb.fctrRoom For Debate`                            -314648827
## SubsectionName.nb.fctrEducation                                      -3450148
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                   NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                               NA
## `SubsectionName.nb.fctrSmall Business`                                     NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`             NA
## `SubsectionName.nb.fctrTravel::Travel`                              -13615868
## `SubsectionName.nb.fctrThe Public Editor`                                  NA
## `SubsectionName.nb.fctrmyMisc::Open`                                       NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                       -1106288
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                           76378063
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## S.day                                                                -3099765
## H.X2014                                                              31022221
## S.show                                                              -38379633
## S.report                                                            -81543587
## S.share                                                             -72881869
## S.year                                                              -28291021
## S.compani                                                           -51690138
## H.new                                                                -9158208
## S.first                                                              -4979158
## S.time                                                              -34496151
## H.newyork                                                            16034607
## S.articl                                                             31636529
## S.will                                                              -13428035
## H.day                                                                -5461627
## S.newyork                                                            77126086
## H.today                                                             -54214483
## H.report                                                             24951167
## S.intern                                                            -25652921
## H.week                                                              -38250602
## S.week                                                               -7829195
## S.fashion                                                          -278189283
## `Headline.pfx.fctrDaily Report::`                                   132655395
## `Headline.pfx.fctrmyEducation::`                                     44283602
## `Headline.pfx.fctr19[0-9][0-9]::`                                   -83936506
## `Headline.pfx.fctrWord of the Day::`                                 18496863
## `Headline.pfx.fctrTest Yourself::`                                  127264691
## `Headline.pfx.fctr6 Q's About the News::`                           128382718
## `Headline.pfx.fctrNew York Today::`                                  42881124
## `Headline.pfx.fctrMorning Agenda::`                                -138050308
## `Headline.pfx.fctrDaily Clip Report::`                                     NA
## `Headline.pfx.fctrToday in Small Business::`                        -32688734
## `Headline.pfx.fctr.*Fashion Week::`                                  53511465
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                    -5551213
## `Headline.pfx.fctrWhat We're::`                                    -364960131
## `Headline.pfx.fctrmyTech::`                                          67943255
## `Headline.pfx.fctrmyMultimedia::`                                    59739997
## `Headline.pfx.fctrmyFood::`                                         -93658711
## `Headline.pfx.fctrmyPolitics::`                                      76912075
## `Headline.pfx.fctrAsk Well::`                                       -60948844
## `Headline.pfx.fctrReaders Respond::`                                -10640198
## `Headline.pfx.fctrYour Turn::`                                      -72311622
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                            74027926
## `Headline.pfx.fctrVerbatim::`                                              NA
## `Headline.pfx.fctrFirst Draft::`                                           NA
## `Headline.pfx.fctrOn This Day::`                                     22342367
## `Headline.pfx.fctrToday in Politics::`                                     NA
## `Headline.pfx.fctrReporter's Notebook::`                                   NA
## `Headline.pfx.fctrThe Daily Gift::`                                -131010791
## H.num.chars.log                                                     -41043431
## H.num.words.log                                                     -63041026
## A.num.chars.log                                                      41752054
## A.num.words.log                                                     -33985453
## A.num.words.unq.log                                                   5404762
##                                                                    Pr(>|z|)
## (Intercept)                                                          <2e-16
## WordCount.log                                                        <2e-16
## `PubDate.hour.fctr(7.67,15.3]`                                       <2e-16
## `PubDate.hour.fctr(15.3,23]`                                         <2e-16
## H.is.question                                                        <2e-16
## PubDate.wkend                                                        <2e-16
## PubDate.last10.log                                                   <2e-16
## PubDate.last1.log                                                    <2e-16
## S.can                                                                <2e-16
## H.has.ebola                                                          <2e-16
## S.make                                                               <2e-16
## .rnorm                                                               <2e-16
## S.one                                                                <2e-16
## S.state                                                              <2e-16
## A.state                                                              <2e-16
## A.one                                                                <2e-16
## S.said                                                               <2e-16
## A.said                                                                   NA
## PubDate.last100.log                                                  <2e-16
## SectionName.nb.fctrWorld                                             <2e-16
## SectionName.nb.fctrMultimedia                                        <2e-16
## SectionName.nb.fctrArts                                              <2e-16
## `SectionName.nb.fctrBusiness Day`                                    <2e-16
## SectionName.nb.fctrMagazine                                          <2e-16
## SectionName.nb.fctrTechnology                                        <2e-16
## `SectionName.nb.fctrCrosswords/Games`                                <2e-16
## SectionName.nb.fctrTStyle                                            <2e-16
## SectionName.nb.fctrOpinion                                           <2e-16
## SectionName.nb.fctrStyles                                            <2e-16
## SectionName.nb.fctrU.S.                                              <2e-16
## `SectionName.nb.fctrmyMisc::`                                        <2e-16
## `SectionName.nb.fctrN.Y. / Region`                                   <2e-16
## `SectionName.nb.fctrDaily Clip Report::`                             <2e-16
## SectionName.nb.fctrTravel                                            <2e-16
## SectionName.nb.fctrOpen                                              <2e-16
## `SectionName.nb.fctrReaders Respond::`                               <2e-16
## `SectionName.nb.fctrmyEducation::`                                   <2e-16
## SectionName.nb.fctrSports                                            <2e-16
## `SectionName.nb.fctrmyPolitics::`                                    <2e-16
## SectionName.nb.fctrNational                                          <2e-16
## `SectionName.nb.fctrVerbatim::`                                      <2e-16
## `SectionName.nb.fctrFirst Draft::`                                   <2e-16
## `SectionName.nb.fctrToday in Politics::`                             <2e-16
## `SectionName.nb.fctrmyMultimedia::`                                  <2e-16
## `SectionName.nb.fctrReporter's Notebook::`                           <2e-16
## SectionName.nb.fctrCulture                                               NA
## `SectionName.nb.fctrThe Daily Gift::`                                <2e-16
## `PubDate.date.fctr(7,13]`                                            <2e-16
## `PubDate.date.fctr(13,19]`                                           <2e-16
## `PubDate.date.fctr(19,25]`                                           <2e-16
## `PubDate.date.fctr(25,31]`                                           <2e-16
## `PubDate.second.fctr(14.8,29.5]`                                     <2e-16
## `PubDate.second.fctr(29.5,44.2]`                                     <2e-16
## `PubDate.second.fctr(44.2,59.1]`                                     <2e-16
## S.presid                                                             <2e-16
## S.take                                                               <2e-16
## `PubDate.minute.fctr(14.8,29.5]`                                     <2e-16
## `PubDate.minute.fctr(29.5,44.2]`                                     <2e-16
## `PubDate.minute.fctr(44.2,59.1]`                                     <2e-16
## S.new                                                                <2e-16
## PubDate.wkday.fctr1                                                  <2e-16
## PubDate.wkday.fctr2                                                  <2e-16
## PubDate.wkday.fctr3                                                  <2e-16
## PubDate.wkday.fctr4                                                  <2e-16
## PubDate.wkday.fctr5                                                  <2e-16
## PubDate.wkday.fctr6                                                  <2e-16
## `SubsectionName.nb.fctrAsia Pacific`                                 <2e-16
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                         NA
## `SubsectionName.nb.fctrCulture::Arts`                                    NA
## SubsectionName.nb.fctrDealbook                                       <2e-16
## `SubsectionName.nb.fctrMagazine::Magazine`                               NA
## `SubsectionName.nb.fctrBusiness::Technology`                         <2e-16
## `SubsectionName.nb.fctrBusiness::Crosswords/Games`                       NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                   NA
## `SubsectionName.nb.fctrForeign::World`                                   NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                <2e-16
## `SubsectionName.nb.fctrFashion & Style`                              <2e-16
## `SubsectionName.nb.fctrRoom For Debate`                              <2e-16
## SubsectionName.nb.fctrEducation                                      <2e-16
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                 NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                             NA
## `SubsectionName.nb.fctrSmall Business`                                   NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`           NA
## `SubsectionName.nb.fctrTravel::Travel`                               <2e-16
## `SubsectionName.nb.fctrThe Public Editor`                                NA
## `SubsectionName.nb.fctrmyMisc::Open`                                     NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`               NA
## SubsectionName.nb.fctrPolitics                                       <2e-16
## `SubsectionName.nb.fctrmyEducation::myEducation::`                       NA
## `SubsectionName.nb.fctrSports::Sports`                                   NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                         NA
## `SubsectionName.nb.fctrNational::National`                               NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                             NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`           NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                     NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`       NA
## `SubsectionName.nb.fctrCulture::Culture`                                 NA
## `SubsectionName.nb.fctrTStyle::Technology`                           <2e-16
## `SubsectionName.nb.fctrmyMisc::U.S.`                                     NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                 NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                   NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                      NA
## `SubsectionName.nb.fctrmyEducation::Travel`                              NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                NA
## S.day                                                                <2e-16
## H.X2014                                                              <2e-16
## S.show                                                               <2e-16
## S.report                                                             <2e-16
## S.share                                                              <2e-16
## S.year                                                               <2e-16
## S.compani                                                            <2e-16
## H.new                                                                <2e-16
## S.first                                                              <2e-16
## S.time                                                               <2e-16
## H.newyork                                                            <2e-16
## S.articl                                                             <2e-16
## S.will                                                               <2e-16
## H.day                                                                <2e-16
## S.newyork                                                            <2e-16
## H.today                                                              <2e-16
## H.report                                                             <2e-16
## S.intern                                                             <2e-16
## H.week                                                               <2e-16
## S.week                                                               <2e-16
## S.fashion                                                            <2e-16
## `Headline.pfx.fctrDaily Report::`                                    <2e-16
## `Headline.pfx.fctrmyEducation::`                                     <2e-16
## `Headline.pfx.fctr19[0-9][0-9]::`                                    <2e-16
## `Headline.pfx.fctrWord of the Day::`                                 <2e-16
## `Headline.pfx.fctrTest Yourself::`                                   <2e-16
## `Headline.pfx.fctr6 Q's About the News::`                            <2e-16
## `Headline.pfx.fctrNew York Today::`                                  <2e-16
## `Headline.pfx.fctrMorning Agenda::`                                  <2e-16
## `Headline.pfx.fctrDaily Clip Report::`                                   NA
## `Headline.pfx.fctrToday in Small Business::`                         <2e-16
## `Headline.pfx.fctr.*Fashion Week::`                                  <2e-16
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                    <2e-16
## `Headline.pfx.fctrWhat We're::`                                      <2e-16
## `Headline.pfx.fctrmyTech::`                                          <2e-16
## `Headline.pfx.fctrmyMultimedia::`                                    <2e-16
## `Headline.pfx.fctrmyFood::`                                          <2e-16
## `Headline.pfx.fctrmyPolitics::`                                      <2e-16
## `Headline.pfx.fctrAsk Well::`                                        <2e-16
## `Headline.pfx.fctrReaders Respond::`                                 <2e-16
## `Headline.pfx.fctrYour Turn::`                                       <2e-16
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                            <2e-16
## `Headline.pfx.fctrVerbatim::`                                            NA
## `Headline.pfx.fctrFirst Draft::`                                         NA
## `Headline.pfx.fctrOn This Day::`                                     <2e-16
## `Headline.pfx.fctrToday in Politics::`                                   NA
## `Headline.pfx.fctrReporter's Notebook::`                                 NA
## `Headline.pfx.fctrThe Daily Gift::`                                  <2e-16
## H.num.chars.log                                                      <2e-16
## H.num.words.log                                                      <2e-16
## A.num.chars.log                                                      <2e-16
## A.num.words.log                                                      <2e-16
## A.num.words.unq.log                                                  <2e-16
##                                                                       
## (Intercept)                                                        ***
## WordCount.log                                                      ***
## `PubDate.hour.fctr(7.67,15.3]`                                     ***
## `PubDate.hour.fctr(15.3,23]`                                       ***
## H.is.question                                                      ***
## PubDate.wkend                                                      ***
## PubDate.last10.log                                                 ***
## PubDate.last1.log                                                  ***
## S.can                                                              ***
## H.has.ebola                                                        ***
## S.make                                                             ***
## .rnorm                                                             ***
## S.one                                                              ***
## S.state                                                            ***
## A.state                                                            ***
## A.one                                                              ***
## S.said                                                             ***
## A.said                                                                
## PubDate.last100.log                                                ***
## SectionName.nb.fctrWorld                                           ***
## SectionName.nb.fctrMultimedia                                      ***
## SectionName.nb.fctrArts                                            ***
## `SectionName.nb.fctrBusiness Day`                                  ***
## SectionName.nb.fctrMagazine                                        ***
## SectionName.nb.fctrTechnology                                      ***
## `SectionName.nb.fctrCrosswords/Games`                              ***
## SectionName.nb.fctrTStyle                                          ***
## SectionName.nb.fctrOpinion                                         ***
## SectionName.nb.fctrStyles                                          ***
## SectionName.nb.fctrU.S.                                            ***
## `SectionName.nb.fctrmyMisc::`                                      ***
## `SectionName.nb.fctrN.Y. / Region`                                 ***
## `SectionName.nb.fctrDaily Clip Report::`                           ***
## SectionName.nb.fctrTravel                                          ***
## SectionName.nb.fctrOpen                                            ***
## `SectionName.nb.fctrReaders Respond::`                             ***
## `SectionName.nb.fctrmyEducation::`                                 ***
## SectionName.nb.fctrSports                                          ***
## `SectionName.nb.fctrmyPolitics::`                                  ***
## SectionName.nb.fctrNational                                        ***
## `SectionName.nb.fctrVerbatim::`                                    ***
## `SectionName.nb.fctrFirst Draft::`                                 ***
## `SectionName.nb.fctrToday in Politics::`                           ***
## `SectionName.nb.fctrmyMultimedia::`                                ***
## `SectionName.nb.fctrReporter's Notebook::`                         ***
## SectionName.nb.fctrCulture                                            
## `SectionName.nb.fctrThe Daily Gift::`                              ***
## `PubDate.date.fctr(7,13]`                                          ***
## `PubDate.date.fctr(13,19]`                                         ***
## `PubDate.date.fctr(19,25]`                                         ***
## `PubDate.date.fctr(25,31]`                                         ***
## `PubDate.second.fctr(14.8,29.5]`                                   ***
## `PubDate.second.fctr(29.5,44.2]`                                   ***
## `PubDate.second.fctr(44.2,59.1]`                                   ***
## S.presid                                                           ***
## S.take                                                             ***
## `PubDate.minute.fctr(14.8,29.5]`                                   ***
## `PubDate.minute.fctr(29.5,44.2]`                                   ***
## `PubDate.minute.fctr(44.2,59.1]`                                   ***
## S.new                                                              ***
## PubDate.wkday.fctr1                                                ***
## PubDate.wkday.fctr2                                                ***
## PubDate.wkday.fctr3                                                ***
## PubDate.wkday.fctr4                                                ***
## PubDate.wkday.fctr5                                                ***
## PubDate.wkday.fctr6                                                ***
## `SubsectionName.nb.fctrAsia Pacific`                               ***
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                      
## `SubsectionName.nb.fctrCulture::Arts`                                 
## SubsectionName.nb.fctrDealbook                                     ***
## `SubsectionName.nb.fctrMagazine::Magazine`                            
## `SubsectionName.nb.fctrBusiness::Technology`                       ***
## `SubsectionName.nb.fctrBusiness::Crosswords/Games`                    
## `SubsectionName.nb.fctrTStyle::TStyle`                                
## `SubsectionName.nb.fctrForeign::World`                                
## `SubsectionName.nb.fctrOpEd::Opinion`                              ***
## `SubsectionName.nb.fctrFashion & Style`                            ***
## `SubsectionName.nb.fctrRoom For Debate`                            ***
## SubsectionName.nb.fctrEducation                                    ***
## `SubsectionName.nb.fctrmyMisc::myMisc::`                              
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                          
## `SubsectionName.nb.fctrSmall Business`                                
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`        
## `SubsectionName.nb.fctrTravel::Travel`                             ***
## `SubsectionName.nb.fctrThe Public Editor`                             
## `SubsectionName.nb.fctrmyMisc::Open`                                  
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`            
## SubsectionName.nb.fctrPolitics                                     ***
## `SubsectionName.nb.fctrmyEducation::myEducation::`                    
## `SubsectionName.nb.fctrSports::Sports`                                
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                      
## `SubsectionName.nb.fctrNational::National`                            
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                          
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                    
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`        
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                  
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`    
## `SubsectionName.nb.fctrCulture::Culture`                              
## `SubsectionName.nb.fctrTStyle::Technology`                         ***
## `SubsectionName.nb.fctrmyMisc::U.S.`                                  
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`              
## `SubsectionName.nb.fctrmyMisc::Travel`                                
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                   
## `SubsectionName.nb.fctrmyEducation::Travel`                           
## `SubsectionName.nb.fctrmyEducation::U.S.`                             
## S.day                                                              ***
## H.X2014                                                            ***
## S.show                                                             ***
## S.report                                                           ***
## S.share                                                            ***
## S.year                                                             ***
## S.compani                                                          ***
## H.new                                                              ***
## S.first                                                            ***
## S.time                                                             ***
## H.newyork                                                          ***
## S.articl                                                           ***
## S.will                                                             ***
## H.day                                                              ***
## S.newyork                                                          ***
## H.today                                                            ***
## H.report                                                           ***
## S.intern                                                           ***
## H.week                                                             ***
## S.week                                                             ***
## S.fashion                                                          ***
## `Headline.pfx.fctrDaily Report::`                                  ***
## `Headline.pfx.fctrmyEducation::`                                   ***
## `Headline.pfx.fctr19[0-9][0-9]::`                                  ***
## `Headline.pfx.fctrWord of the Day::`                               ***
## `Headline.pfx.fctrTest Yourself::`                                 ***
## `Headline.pfx.fctr6 Q's About the News::`                          ***
## `Headline.pfx.fctrNew York Today::`                                ***
## `Headline.pfx.fctrMorning Agenda::`                                ***
## `Headline.pfx.fctrDaily Clip Report::`                                
## `Headline.pfx.fctrToday in Small Business::`                       ***
## `Headline.pfx.fctr.*Fashion Week::`                                ***
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                  ***
## `Headline.pfx.fctrWhat We're::`                                    ***
## `Headline.pfx.fctrmyTech::`                                        ***
## `Headline.pfx.fctrmyMultimedia::`                                  ***
## `Headline.pfx.fctrmyFood::`                                        ***
## `Headline.pfx.fctrmyPolitics::`                                    ***
## `Headline.pfx.fctrAsk Well::`                                      ***
## `Headline.pfx.fctrReaders Respond::`                               ***
## `Headline.pfx.fctrYour Turn::`                                     ***
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                          ***
## `Headline.pfx.fctrVerbatim::`                                         
## `Headline.pfx.fctrFirst Draft::`                                      
## `Headline.pfx.fctrOn This Day::`                                   ***
## `Headline.pfx.fctrToday in Politics::`                                
## `Headline.pfx.fctrReporter's Notebook::`                              
## `Headline.pfx.fctrThe Daily Gift::`                                ***
## H.num.chars.log                                                    ***
## H.num.words.log                                                    ***
## A.num.chars.log                                                    ***
## A.num.words.log                                                    ***
## A.num.words.unq.log                                                ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance:  4042.7  on 4474  degrees of freedom
## Residual deviance: 30276.7  on 4353  degrees of freedom
## AIC: 30521
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

![](NYTBlogs_zoo_files/figure-html/fit.models_0-32.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_0-33.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.7091413
## 3        0.2 0.7091413
## 4        0.3 0.7091413
## 5        0.4 0.7091413
## 6        0.5 0.7091413
## 7        0.6 0.7091413
## 8        0.7 0.7091413
## 9        0.8 0.7091413
## 10       0.9 0.7091413
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3543
## 2            Y                                  237
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  183
## 2                                  512
##          Prediction
## Reference    N    Y
##         N 3543  183
##         Y  237  512
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.061453e-01   6.532793e-01   8.972220e-01   9.145342e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.343346e-45   9.705885e-03 
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
## 2        0.1 0.6798780
## 3        0.2 0.6798780
## 4        0.3 0.6798780
## 5        0.4 0.6798780
## 6        0.5 0.6798780
## 7        0.6 0.6798780
## 8        0.7 0.6798780
## 9        0.8 0.6798780
## 10       0.9 0.6798780
## 11       1.0 0.0000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_0-36.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1624
## 2            Y                                  121
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   89
## 2                                  223
##          Prediction
## Reference    N    Y
##         N 1624   89
##         Y  121  223
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.979096e-01   6.193211e-01   8.840103e-01   9.106626e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   2.620182e-17   3.241921e-02 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, S.can, H.has.ebola, S.make, .rnorm, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     18.785                 5.755
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8172319                    0.9       0.7091413        0.9030152
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.897222             0.9145342     0.6317061   0.7981501
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9        0.679878        0.8979096
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8840103             0.9106626     0.6193211    30520.67
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.007074496      0.01924142
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 9  fit.models          6          0 163.056 218.326   55.27
## 10 fit.models          6          1 218.326      NA      NA
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
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, .rnorm, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
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
##   1143, 1930, 3374, 3591, 3625, 3755, 3799, 4243, 4320, 4408
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-1.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_1-2.png) 

```
## Warning: not plotting observations with leverage one:
##   1143, 1930, 3374, 3591, 3625, 3755, 3799, 4243, 4320, 4408
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -8.49    0.00    0.00    0.00    8.49  
## 
## Coefficients: (76 not defined because of singularities)
##                                                                      Estimate
## (Intercept)                                                        -4.670e+15
## WordCount.log                                                       6.943e+14
## `PubDate.hour.fctr(7.67,15.3]`                                      4.258e+14
## `PubDate.hour.fctr(15.3,23]`                                        4.366e+14
## H.is.question                                                       1.129e+15
## PubDate.wkend                                                      -3.147e+14
## PubDate.last10.log                                                  1.652e+14
## PubDate.last1.log                                                  -1.696e+13
## A.can                                                               8.037e+15
## S.can                                                              -8.194e+15
## H.has.ebola                                                        -1.831e+13
## S.make                                                             -5.922e+13
## A.make                                                                     NA
## .rnorm                                                             -4.598e+13
## S.one                                                              -5.000e+15
## S.state                                                            -2.362e+15
## A.state                                                             2.659e+15
## A.one                                                               4.700e+15
## S.said                                                              3.518e+14
## A.said                                                                     NA
## PubDate.last100.log                                                 2.979e+13
## SectionName.nb.fctrWorld                                           -6.756e+14
## SectionName.nb.fctrMultimedia                                      -2.647e+15
## SectionName.nb.fctrArts                                            -2.935e+14
## `SectionName.nb.fctrBusiness Day`                                  -2.217e+15
## SectionName.nb.fctrMagazine                                        -3.739e+15
## SectionName.nb.fctrTechnology                                      -6.062e+15
## `SectionName.nb.fctrCrosswords/Games`                              -4.420e+13
## SectionName.nb.fctrTStyle                                          -1.506e+15
## SectionName.nb.fctrOpinion                                          2.785e+14
## SectionName.nb.fctrStyles                                          -4.471e+15
## SectionName.nb.fctrU.S.                                            -3.341e+15
## `SectionName.nb.fctrmyMisc::`                                      -9.528e+14
## `SectionName.nb.fctrN.Y. / Region`                                 -3.761e+14
## `SectionName.nb.fctrDaily Clip Report::`                           -9.771e+13
## SectionName.nb.fctrTravel                                           1.336e+15
## SectionName.nb.fctrOpen                                            -2.118e+15
## `SectionName.nb.fctrReaders Respond::`                              2.504e+15
## `SectionName.nb.fctrmyEducation::`                                  2.030e+14
## SectionName.nb.fctrSports                                          -1.827e+15
## `SectionName.nb.fctrmyPolitics::`                                  -1.822e+15
## SectionName.nb.fctrNational                                        -3.711e+14
## `SectionName.nb.fctrVerbatim::`                                    -2.494e+14
## `SectionName.nb.fctrFirst Draft::`                                 -7.519e+14
## `SectionName.nb.fctrToday in Politics::`                           -4.328e+14
## `SectionName.nb.fctrmyMultimedia::`                                -4.983e+15
## `SectionName.nb.fctrReporter's Notebook::`                         -3.110e+15
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                              -4.258e+15
## `PubDate.date.fctr(7,13]`                                          -8.681e+13
## `PubDate.date.fctr(13,19]`                                         -8.769e+13
## `PubDate.date.fctr(19,25]`                                         -1.458e+14
## `PubDate.date.fctr(25,31]`                                          1.214e+14
## `PubDate.second.fctr(14.8,29.5]`                                   -2.248e+13
## `PubDate.second.fctr(29.5,44.2]`                                   -8.594e+13
## `PubDate.second.fctr(44.2,59.1]`                                   -2.153e+14
## S.presid                                                            2.614e+13
## A.presid                                                                   NA
## S.take                                                             -2.146e+13
## A.take                                                                     NA
## `PubDate.minute.fctr(14.8,29.5]`                                   -1.617e+13
## `PubDate.minute.fctr(29.5,44.2]`                                   -1.718e+14
## `PubDate.minute.fctr(44.2,59.1]`                                    4.116e+13
## S.new                                                              -3.534e+14
## A.new                                                               3.947e+14
## PubDate.wkday.fctr1                                                -2.681e+14
## PubDate.wkday.fctr2                                                -5.184e+14
## PubDate.wkday.fctr3                                                -3.828e+14
## PubDate.wkday.fctr4                                                -4.796e+14
## PubDate.wkday.fctr5                                                -4.437e+14
## PubDate.wkday.fctr6                                                -4.968e+14
## `SubsectionName.nb.fctrAsia Pacific`                               -7.249e+14
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                           NA
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      2.227e+15
## `SubsectionName.nb.fctrMagazine::Magazine`                                 NA
## `SubsectionName.nb.fctrBusiness::Technology`                        6.156e+15
## `SubsectionName.nb.fctrBusiness::Crosswords/Games`                         NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrForeign::World`                                     NA
## `SubsectionName.nb.fctrOpEd::Opinion`                               9.743e+13
## `SubsectionName.nb.fctrFashion & Style`                             2.861e+15
## `SubsectionName.nb.fctrRoom For Debate`                            -4.610e+15
## SubsectionName.nb.fctrEducation                                     1.053e+15
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                   NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                               NA
## `SubsectionName.nb.fctrSmall Business`                                     NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`             NA
## `SubsectionName.nb.fctrTravel::Travel`                             -4.540e+15
## `SubsectionName.nb.fctrThe Public Editor`                                  NA
## `SubsectionName.nb.fctrmyMisc::Open`                                       NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                     -4.288e+14
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                          9.869e+15
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## S.day                                                              -1.816e+14
## A.day                                                                      NA
## H.X2014                                                            -6.681e+13
## S.show                                                             -2.316e+14
## A.show                                                                     NA
## S.report                                                           -4.361e+14
## A.report                                                                   NA
## S.share                                                            -7.866e+14
## A.share                                                                    NA
## S.year                                                             -2.388e+14
## A.year                                                                     NA
## S.compani                                                          -1.008e+14
## A.compani                                                                  NA
## H.new                                                              -3.057e+14
## S.first                                                            -1.475e+14
## A.first                                                                    NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrBusiness                                                   NA
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrTStyle                                                     NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrStyles                                                     NA
## NewsDesk.nb.fctrmyEducation                                                NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## NewsDesk.nb.fctrTravel                                                     NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## S.time                                                             -2.389e+14
## A.time                                                                     NA
## H.newyork                                                          -2.144e+13
## S.articl                                                            7.280e+13
## A.articl                                                                   NA
## S.will                                                              2.924e+15
## A.will                                                             -3.083e+15
## H.day                                                              -4.783e+14
## S.newyork                                                           3.956e+14
## A.newyork                                                                  NA
## H.today                                                            -1.229e+15
## H.report                                                           -7.034e+14
## S.intern                                                           -8.811e+13
## A.intern                                                                   NA
## H.week                                                             -7.290e+14
## H.fashion                                                           4.143e+14
## S.week                                                              5.287e+13
## A.week                                                                     NA
## S.fashion                                                          -6.071e+14
## A.fashion                                                                  NA
## `Headline.pfx.fctrDaily Report::`                                  -1.673e+15
## `Headline.pfx.fctrmyEducation::`                                    5.549e+14
## `Headline.pfx.fctr19[0-9][0-9]::`                                  -2.485e+15
## `Headline.pfx.fctrWord of the Day::`                                1.209e+15
## `Headline.pfx.fctrTest Yourself::`                                  5.116e+13
## `Headline.pfx.fctr6 Q's About the News::`                           2.165e+15
## `Headline.pfx.fctrNew York Today::`                                -9.887e+14
## `Headline.pfx.fctrMorning Agenda::`                                -8.561e+14
## `Headline.pfx.fctrDaily Clip Report::`                                     NA
## `Headline.pfx.fctrToday in Small Business::`                       -4.845e+13
## `Headline.pfx.fctr.*Fashion Week::`                                 1.655e+15
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                   4.894e+15
## `Headline.pfx.fctrWhat We're::`                                    -2.028e+15
## `Headline.pfx.fctrmyTech::`                                         9.051e+13
## `Headline.pfx.fctrmyMultimedia::`                                   1.333e+15
## `Headline.pfx.fctrmyFood::`                                        -3.667e+14
## `Headline.pfx.fctrmyPolitics::`                                     6.088e+14
## `Headline.pfx.fctrAsk Well::`                                      -4.560e+14
## `Headline.pfx.fctrReaders Respond::`                               -1.073e+15
## `Headline.pfx.fctrYour Turn::`                                      1.666e+15
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           4.110e+15
## `Headline.pfx.fctrVerbatim::`                                              NA
## `Headline.pfx.fctrFirst Draft::`                                           NA
## `Headline.pfx.fctrOn This Day::`                                    1.915e+15
## `Headline.pfx.fctrToday in Politics::`                                     NA
## `Headline.pfx.fctrReporter's Notebook::`                                   NA
## `Headline.pfx.fctrThe Daily Gift::`                                 2.220e+15
## H.num.chars.log                                                     1.277e+14
## H.num.words.log                                                     1.011e+15
## H.num.words.unq.log                                                -1.765e+15
## A.num.chars.log                                                     8.505e+14
## S.num.chars.log                                                    -6.474e+14
## A.num.words.log                                                    -3.896e+16
## S.num.words.log                                                     4.006e+16
## A.num.words.unq.log                                                 3.130e+16
## S.num.words.unq.log                                                -3.305e+16
##                                                                    Std. Error
## (Intercept)                                                         3.493e+07
## WordCount.log                                                       1.269e+06
## `PubDate.hour.fctr(7.67,15.3]`                                      3.936e+06
## `PubDate.hour.fctr(15.3,23]`                                        4.177e+06
## H.is.question                                                       4.598e+06
## PubDate.wkend                                                       7.731e+06
## PubDate.last10.log                                                  1.961e+06
## PubDate.last1.log                                                   7.097e+05
## A.can                                                               7.845e+07
## S.can                                                               7.899e+07
## H.has.ebola                                                         9.245e+06
## S.make                                                              5.181e+06
## A.make                                                                     NA
## .rnorm                                                              1.027e+06
## S.one                                                               1.077e+08
## S.state                                                             9.310e+07
## A.state                                                             9.303e+07
## A.one                                                               1.078e+08
## S.said                                                              5.376e+06
## A.said                                                                     NA
## PubDate.last100.log                                                 7.749e+05
## SectionName.nb.fctrWorld                                            1.705e+07
## SectionName.nb.fctrMultimedia                                       1.112e+07
## SectionName.nb.fctrArts                                             7.216e+06
## `SectionName.nb.fctrBusiness Day`                                   1.096e+07
## SectionName.nb.fctrMagazine                                         1.728e+07
## SectionName.nb.fctrTechnology                                       8.594e+07
## `SectionName.nb.fctrCrosswords/Games`                               1.039e+07
## SectionName.nb.fctrTStyle                                           7.310e+06
## SectionName.nb.fctrOpinion                                          2.270e+07
## SectionName.nb.fctrStyles                                           6.867e+07
## SectionName.nb.fctrU.S.                                             6.760e+07
## `SectionName.nb.fctrmyMisc::`                                       6.915e+06
## `SectionName.nb.fctrN.Y. / Region`                                  9.628e+06
## `SectionName.nb.fctrDaily Clip Report::`                            2.041e+07
## SectionName.nb.fctrTravel                                           6.790e+07
## SectionName.nb.fctrOpen                                             4.813e+07
## `SectionName.nb.fctrReaders Respond::`                              3.652e+07
## `SectionName.nb.fctrmyEducation::`                                  3.318e+07
## SectionName.nb.fctrSports                                           5.000e+07
## `SectionName.nb.fctrmyPolitics::`                                   1.705e+07
## SectionName.nb.fctrNational                                         4.833e+07
## `SectionName.nb.fctrVerbatim::`                                     1.666e+07
## `SectionName.nb.fctrFirst Draft::`                                  1.251e+07
## `SectionName.nb.fctrToday in Politics::`                            2.935e+07
## `SectionName.nb.fctrmyMultimedia::`                                 7.036e+07
## `SectionName.nb.fctrReporter's Notebook::`                          3.556e+07
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                               6.978e+07
## `PubDate.date.fctr(7,13]`                                           3.218e+06
## `PubDate.date.fctr(13,19]`                                          3.168e+06
## `PubDate.date.fctr(19,25]`                                          3.075e+06
## `PubDate.date.fctr(25,31]`                                          3.423e+06
## `PubDate.second.fctr(14.8,29.5]`                                    2.861e+06
## `PubDate.second.fctr(29.5,44.2]`                                    2.818e+06
## `PubDate.second.fctr(44.2,59.1]`                                    2.883e+06
## S.presid                                                            5.573e+06
## A.presid                                                                   NA
## S.take                                                              5.866e+06
## A.take                                                                     NA
## `PubDate.minute.fctr(14.8,29.5]`                                    2.949e+06
## `PubDate.minute.fctr(29.5,44.2]`                                    2.811e+06
## `PubDate.minute.fctr(44.2,59.1]`                                    3.023e+06
## S.new                                                               7.161e+07
## A.new                                                               7.152e+07
## PubDate.wkday.fctr1                                                 9.393e+06
## PubDate.wkday.fctr2                                                 9.995e+06
## PubDate.wkday.fctr3                                                 9.914e+06
## PubDate.wkday.fctr4                                                 9.781e+06
## PubDate.wkday.fctr5                                                 9.919e+06
## PubDate.wkday.fctr6                                                 7.883e+06
## `SubsectionName.nb.fctrAsia Pacific`                                1.699e+07
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                           NA
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      9.456e+06
## `SubsectionName.nb.fctrMagazine::Magazine`                                 NA
## `SubsectionName.nb.fctrBusiness::Technology`                        8.564e+07
## `SubsectionName.nb.fctrBusiness::Crosswords/Games`                         NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrForeign::World`                                     NA
## `SubsectionName.nb.fctrOpEd::Opinion`                               2.202e+07
## `SubsectionName.nb.fctrFashion & Style`                             6.763e+07
## `SubsectionName.nb.fctrRoom For Debate`                             2.410e+07
## SubsectionName.nb.fctrEducation                                     6.780e+07
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                   NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                               NA
## `SubsectionName.nb.fctrSmall Business`                                     NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`             NA
## `SubsectionName.nb.fctrTravel::Travel`                              6.794e+07
## `SubsectionName.nb.fctrThe Public Editor`                                  NA
## `SubsectionName.nb.fctrmyMisc::Open`                                       NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                      8.306e+07
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                          1.091e+08
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## S.day                                                               6.591e+06
## A.day                                                                      NA
## H.X2014                                                             9.670e+06
## S.show                                                              5.531e+06
## A.show                                                                     NA
## S.report                                                            6.513e+06
## A.report                                                                   NA
## S.share                                                             6.285e+06
## A.share                                                                    NA
## S.year                                                              4.642e+06
## A.year                                                                     NA
## S.compani                                                           4.425e+06
## A.compani                                                                  NA
## H.new                                                               5.528e+06
## S.first                                                             5.699e+06
## A.first                                                                    NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrBusiness                                                   NA
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrTStyle                                                     NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrStyles                                                     NA
## NewsDesk.nb.fctrmyEducation                                                NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## NewsDesk.nb.fctrTravel                                                     NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## S.time                                                              4.563e+06
## A.time                                                                     NA
## H.newyork                                                           7.155e+06
## S.articl                                                            1.076e+07
## A.articl                                                                   NA
## S.will                                                              7.050e+07
## A.will                                                              7.046e+07
## H.day                                                               9.341e+06
## S.newyork                                                           5.104e+06
## A.newyork                                                                  NA
## H.today                                                             2.562e+07
## H.report                                                            9.805e+06
## S.intern                                                            9.824e+06
## A.intern                                                                   NA
## H.week                                                              1.152e+07
## H.fashion                                                           1.355e+07
## S.week                                                              4.986e+06
## A.week                                                                     NA
## S.fashion                                                           7.656e+06
## A.fashion                                                                  NA
## `Headline.pfx.fctrDaily Report::`                                   1.613e+07
## `Headline.pfx.fctrmyEducation::`                                    1.224e+07
## `Headline.pfx.fctr19[0-9][0-9]::`                                   1.563e+07
## `Headline.pfx.fctrWord of the Day::`                                1.998e+07
## `Headline.pfx.fctrTest Yourself::`                                  1.556e+07
## `Headline.pfx.fctr6 Q's About the News::`                           1.300e+07
## `Headline.pfx.fctrNew York Today::`                                 2.915e+07
## `Headline.pfx.fctrMorning Agenda::`                                 1.164e+07
## `Headline.pfx.fctrDaily Clip Report::`                                     NA
## `Headline.pfx.fctrToday in Small Business::`                        2.970e+07
## `Headline.pfx.fctr.*Fashion Week::`                                 1.929e+07
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                   1.818e+07
## `Headline.pfx.fctrWhat We're::`                                     1.512e+07
## `Headline.pfx.fctrmyTech::`                                         8.717e+06
## `Headline.pfx.fctrmyMultimedia::`                                   1.499e+07
## `Headline.pfx.fctrmyFood::`                                         9.133e+06
## `Headline.pfx.fctrmyPolitics::`                                     1.317e+07
## `Headline.pfx.fctrAsk Well::`                                       2.588e+07
## `Headline.pfx.fctrReaders Respond::`                                2.819e+07
## `Headline.pfx.fctrYour Turn::`                                      2.756e+07
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           2.972e+07
## `Headline.pfx.fctrVerbatim::`                                              NA
## `Headline.pfx.fctrFirst Draft::`                                           NA
## `Headline.pfx.fctrOn This Day::`                                    2.605e+07
## `Headline.pfx.fctrToday in Politics::`                                     NA
## `Headline.pfx.fctrReporter's Notebook::`                                   NA
## `Headline.pfx.fctrThe Daily Gift::`                                 1.883e+07
## H.num.chars.log                                                     6.583e+06
## H.num.words.log                                                     3.795e+07
## H.num.words.unq.log                                                 3.741e+07
## A.num.chars.log                                                     1.438e+08
## S.num.chars.log                                                     1.440e+08
## A.num.words.log                                                     5.570e+08
## S.num.words.log                                                     5.569e+08
## A.num.words.unq.log                                                 5.638e+08
## S.num.words.unq.log                                                 5.634e+08
##                                                                       z value
## (Intercept)                                                        -133690572
## WordCount.log                                                       547109021
## `PubDate.hour.fctr(7.67,15.3]`                                      108169153
## `PubDate.hour.fctr(15.3,23]`                                        104523651
## H.is.question                                                       245515775
## PubDate.wkend                                                       -40709334
## PubDate.last10.log                                                   84235975
## PubDate.last1.log                                                   -23889939
## A.can                                                               102440212
## S.can                                                              -103737690
## H.has.ebola                                                          -1980492
## S.make                                                              -11429506
## A.make                                                                     NA
## .rnorm                                                              -44754394
## S.one                                                               -46415057
## S.state                                                             -25376241
## A.state                                                              28579586
## A.one                                                                43600044
## S.said                                                               65431192
## A.said                                                                     NA
## PubDate.last100.log                                                  38448552
## SectionName.nb.fctrWorld                                            -39627104
## SectionName.nb.fctrMultimedia                                      -238062544
## SectionName.nb.fctrArts                                             -40670820
## `SectionName.nb.fctrBusiness Day`                                  -202232660
## SectionName.nb.fctrMagazine                                        -216350532
## SectionName.nb.fctrTechnology                                       -70536682
## `SectionName.nb.fctrCrosswords/Games`                                -4253693
## SectionName.nb.fctrTStyle                                          -206058337
## SectionName.nb.fctrOpinion                                           12269798
## SectionName.nb.fctrStyles                                           -65109989
## SectionName.nb.fctrU.S.                                             -49428104
## `SectionName.nb.fctrmyMisc::`                                      -137793501
## `SectionName.nb.fctrN.Y. / Region`                                  -39062975
## `SectionName.nb.fctrDaily Clip Report::`                             -4787769
## SectionName.nb.fctrTravel                                            19676963
## SectionName.nb.fctrOpen                                             -43998334
## `SectionName.nb.fctrReaders Respond::`                               68571443
## `SectionName.nb.fctrmyEducation::`                                    6117651
## SectionName.nb.fctrSports                                           -36537372
## `SectionName.nb.fctrmyPolitics::`                                  -106897256
## SectionName.nb.fctrNational                                          -7678173
## `SectionName.nb.fctrVerbatim::`                                     -14967474
## `SectionName.nb.fctrFirst Draft::`                                  -60091983
## `SectionName.nb.fctrToday in Politics::`                            -14744895
## `SectionName.nb.fctrmyMultimedia::`                                 -70823626
## `SectionName.nb.fctrReporter's Notebook::`                          -87456712
## SectionName.nb.fctrCulture                                                 NA
## `SectionName.nb.fctrThe Daily Gift::`                               -61019593
## `PubDate.date.fctr(7,13]`                                           -26974632
## `PubDate.date.fctr(13,19]`                                          -27676952
## `PubDate.date.fctr(19,25]`                                          -47426925
## `PubDate.date.fctr(25,31]`                                           35474285
## `PubDate.second.fctr(14.8,29.5]`                                     -7857275
## `PubDate.second.fctr(29.5,44.2]`                                    -30492436
## `PubDate.second.fctr(44.2,59.1]`                                    -74683314
## S.presid                                                              4689843
## A.presid                                                                   NA
## S.take                                                               -3657422
## A.take                                                                     NA
## `PubDate.minute.fctr(14.8,29.5]`                                     -5485038
## `PubDate.minute.fctr(29.5,44.2]`                                    -61131677
## `PubDate.minute.fctr(44.2,59.1]`                                     13615361
## S.new                                                                -4935705
## A.new                                                                 5518130
## PubDate.wkday.fctr1                                                 -28540583
## PubDate.wkday.fctr2                                                 -51867183
## PubDate.wkday.fctr3                                                 -38609274
## PubDate.wkday.fctr4                                                 -49039630
## PubDate.wkday.fctr5                                                 -44734970
## PubDate.wkday.fctr6                                                 -63020581
## `SubsectionName.nb.fctrAsia Pacific`                                -42677822
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                           NA
## `SubsectionName.nb.fctrCulture::Arts`                                      NA
## SubsectionName.nb.fctrDealbook                                      235515485
## `SubsectionName.nb.fctrMagazine::Magazine`                                 NA
## `SubsectionName.nb.fctrBusiness::Technology`                         71878372
## `SubsectionName.nb.fctrBusiness::Crosswords/Games`                         NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                     NA
## `SubsectionName.nb.fctrForeign::World`                                     NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                 4425204
## `SubsectionName.nb.fctrFashion & Style`                              42301040
## `SubsectionName.nb.fctrRoom For Debate`                            -191292551
## SubsectionName.nb.fctrEducation                                      15535501
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                   NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                               NA
## `SubsectionName.nb.fctrSmall Business`                                     NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`             NA
## `SubsectionName.nb.fctrTravel::Travel`                              -66824926
## `SubsectionName.nb.fctrThe Public Editor`                                  NA
## `SubsectionName.nb.fctrmyMisc::Open`                                       NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`                 NA
## SubsectionName.nb.fctrPolitics                                       -5162774
## `SubsectionName.nb.fctrmyEducation::myEducation::`                         NA
## `SubsectionName.nb.fctrSports::Sports`                                     NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                           NA
## `SubsectionName.nb.fctrNational::National`                                 NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                               NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                         NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`             NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                       NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`         NA
## `SubsectionName.nb.fctrCulture::Culture`                                   NA
## `SubsectionName.nb.fctrTStyle::Technology`                           90447772
## `SubsectionName.nb.fctrmyMisc::U.S.`                                       NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                   NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                     NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                        NA
## `SubsectionName.nb.fctrmyEducation::Travel`                                NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                  NA
## S.day                                                               -27552503
## A.day                                                                      NA
## H.X2014                                                              -6909018
## S.show                                                              -41862902
## A.show                                                                     NA
## S.report                                                            -66955565
## A.report                                                                   NA
## S.share                                                            -125146363
## A.share                                                                    NA
## S.year                                                              -51443526
## A.year                                                                     NA
## S.compani                                                           -22783611
## A.compani                                                                  NA
## H.new                                                               -55304563
## S.first                                                             -25883745
## A.first                                                                    NA
## NewsDesk.nb.fctrForeign                                                    NA
## NewsDesk.nb.fctrmyMultimedia                                               NA
## NewsDesk.nb.fctrCulture                                                    NA
## NewsDesk.nb.fctrBusiness                                                   NA
## NewsDesk.nb.fctrMagazine                                                   NA
## NewsDesk.nb.fctrTStyle                                                     NA
## NewsDesk.nb.fctrOpEd                                                       NA
## NewsDesk.nb.fctrStyles                                                     NA
## NewsDesk.nb.fctrmyEducation                                                NA
## `NewsDesk.nb.fctrmyMisc::`                                                 NA
## NewsDesk.nb.fctrMetro                                                      NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                      NA
## NewsDesk.nb.fctrTravel                                                     NA
## `NewsDesk.nb.fctrReaders Respond::`                                        NA
## NewsDesk.nb.fctrNational                                                   NA
## `NewsDesk.nb.fctrmyEducation::`                                            NA
## NewsDesk.nb.fctrSports                                                     NA
## `NewsDesk.nb.fctrmyPolitics::`                                             NA
## `NewsDesk.nb.fctrVerbatim::`                                               NA
## `NewsDesk.nb.fctrFirst Draft::`                                            NA
## `NewsDesk.nb.fctrToday in Politics::`                                      NA
## `NewsDesk.nb.fctrmyMultimedia::`                                           NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                    NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                         NA
## S.time                                                              -52353964
## A.time                                                                     NA
## H.newyork                                                            -2996840
## S.articl                                                              6768085
## A.articl                                                                   NA
## S.will                                                               41470005
## A.will                                                              -43758799
## H.day                                                               -51202951
## S.newyork                                                            77501025
## A.newyork                                                                  NA
## H.today                                                             -47966216
## H.report                                                            -71741023
## S.intern                                                             -8968909
## A.intern                                                                   NA
## H.week                                                              -63286841
## H.fashion                                                            30572440
## S.week                                                               10603685
## A.week                                                                     NA
## S.fashion                                                           -79294235
## A.fashion                                                                  NA
## `Headline.pfx.fctrDaily Report::`                                  -103681494
## `Headline.pfx.fctrmyEducation::`                                     45316579
## `Headline.pfx.fctr19[0-9][0-9]::`                                  -159042720
## `Headline.pfx.fctrWord of the Day::`                                 60514536
## `Headline.pfx.fctrTest Yourself::`                                    3287510
## `Headline.pfx.fctr6 Q's About the News::`                           166493953
## `Headline.pfx.fctrNew York Today::`                                 -33916729
## `Headline.pfx.fctrMorning Agenda::`                                 -73573320
## `Headline.pfx.fctrDaily Clip Report::`                                     NA
## `Headline.pfx.fctrToday in Small Business::`                         -1631659
## `Headline.pfx.fctr.*Fashion Week::`                                  85780231
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                   269173940
## `Headline.pfx.fctrWhat We're::`                                    -134097323
## `Headline.pfx.fctrmyTech::`                                          10382844
## `Headline.pfx.fctrmyMultimedia::`                                    88970834
## `Headline.pfx.fctrmyFood::`                                         -40147114
## `Headline.pfx.fctrmyPolitics::`                                      46221951
## `Headline.pfx.fctrAsk Well::`                                       -17622946
## `Headline.pfx.fctrReaders Respond::`                                -38066671
## `Headline.pfx.fctrYour Turn::`                                       60434921
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                           138295633
## `Headline.pfx.fctrVerbatim::`                                              NA
## `Headline.pfx.fctrFirst Draft::`                                           NA
## `Headline.pfx.fctrOn This Day::`                                     73532030
## `Headline.pfx.fctrToday in Politics::`                                     NA
## `Headline.pfx.fctrReporter's Notebook::`                                   NA
## `Headline.pfx.fctrThe Daily Gift::`                                 117907866
## H.num.chars.log                                                      19399597
## H.num.words.log                                                      26642334
## H.num.words.unq.log                                                 -47177261
## A.num.chars.log                                                       5915419
## S.num.chars.log                                                      -4494524
## A.num.words.log                                                     -69944474
## S.num.words.log                                                      71928505
## A.num.words.unq.log                                                  55528573
## S.num.words.unq.log                                                 -58671780
##                                                                    Pr(>|z|)
## (Intercept)                                                          <2e-16
## WordCount.log                                                        <2e-16
## `PubDate.hour.fctr(7.67,15.3]`                                       <2e-16
## `PubDate.hour.fctr(15.3,23]`                                         <2e-16
## H.is.question                                                        <2e-16
## PubDate.wkend                                                        <2e-16
## PubDate.last10.log                                                   <2e-16
## PubDate.last1.log                                                    <2e-16
## A.can                                                                <2e-16
## S.can                                                                <2e-16
## H.has.ebola                                                          <2e-16
## S.make                                                               <2e-16
## A.make                                                                   NA
## .rnorm                                                               <2e-16
## S.one                                                                <2e-16
## S.state                                                              <2e-16
## A.state                                                              <2e-16
## A.one                                                                <2e-16
## S.said                                                               <2e-16
## A.said                                                                   NA
## PubDate.last100.log                                                  <2e-16
## SectionName.nb.fctrWorld                                             <2e-16
## SectionName.nb.fctrMultimedia                                        <2e-16
## SectionName.nb.fctrArts                                              <2e-16
## `SectionName.nb.fctrBusiness Day`                                    <2e-16
## SectionName.nb.fctrMagazine                                          <2e-16
## SectionName.nb.fctrTechnology                                        <2e-16
## `SectionName.nb.fctrCrosswords/Games`                                <2e-16
## SectionName.nb.fctrTStyle                                            <2e-16
## SectionName.nb.fctrOpinion                                           <2e-16
## SectionName.nb.fctrStyles                                            <2e-16
## SectionName.nb.fctrU.S.                                              <2e-16
## `SectionName.nb.fctrmyMisc::`                                        <2e-16
## `SectionName.nb.fctrN.Y. / Region`                                   <2e-16
## `SectionName.nb.fctrDaily Clip Report::`                             <2e-16
## SectionName.nb.fctrTravel                                            <2e-16
## SectionName.nb.fctrOpen                                              <2e-16
## `SectionName.nb.fctrReaders Respond::`                               <2e-16
## `SectionName.nb.fctrmyEducation::`                                   <2e-16
## SectionName.nb.fctrSports                                            <2e-16
## `SectionName.nb.fctrmyPolitics::`                                    <2e-16
## SectionName.nb.fctrNational                                          <2e-16
## `SectionName.nb.fctrVerbatim::`                                      <2e-16
## `SectionName.nb.fctrFirst Draft::`                                   <2e-16
## `SectionName.nb.fctrToday in Politics::`                             <2e-16
## `SectionName.nb.fctrmyMultimedia::`                                  <2e-16
## `SectionName.nb.fctrReporter's Notebook::`                           <2e-16
## SectionName.nb.fctrCulture                                               NA
## `SectionName.nb.fctrThe Daily Gift::`                                <2e-16
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
## A.take                                                                   NA
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
## `SubsectionName.nb.fctrAsia Pacific`                                 <2e-16
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                         NA
## `SubsectionName.nb.fctrCulture::Arts`                                    NA
## SubsectionName.nb.fctrDealbook                                       <2e-16
## `SubsectionName.nb.fctrMagazine::Magazine`                               NA
## `SubsectionName.nb.fctrBusiness::Technology`                         <2e-16
## `SubsectionName.nb.fctrBusiness::Crosswords/Games`                       NA
## `SubsectionName.nb.fctrTStyle::TStyle`                                   NA
## `SubsectionName.nb.fctrForeign::World`                                   NA
## `SubsectionName.nb.fctrOpEd::Opinion`                                <2e-16
## `SubsectionName.nb.fctrFashion & Style`                              <2e-16
## `SubsectionName.nb.fctrRoom For Debate`                              <2e-16
## SubsectionName.nb.fctrEducation                                      <2e-16
## `SubsectionName.nb.fctrmyMisc::myMisc::`                                 NA
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                             NA
## `SubsectionName.nb.fctrSmall Business`                                   NA
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`           NA
## `SubsectionName.nb.fctrTravel::Travel`                               <2e-16
## `SubsectionName.nb.fctrThe Public Editor`                                NA
## `SubsectionName.nb.fctrmyMisc::Open`                                     NA
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`               NA
## SubsectionName.nb.fctrPolitics                                       <2e-16
## `SubsectionName.nb.fctrmyEducation::myEducation::`                       NA
## `SubsectionName.nb.fctrSports::Sports`                                   NA
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                         NA
## `SubsectionName.nb.fctrNational::National`                               NA
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                             NA
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                       NA
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`           NA
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                     NA
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`       NA
## `SubsectionName.nb.fctrCulture::Culture`                                 NA
## `SubsectionName.nb.fctrTStyle::Technology`                           <2e-16
## `SubsectionName.nb.fctrmyMisc::U.S.`                                     NA
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`                 NA
## `SubsectionName.nb.fctrmyMisc::Travel`                                   NA
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                      NA
## `SubsectionName.nb.fctrmyEducation::Travel`                              NA
## `SubsectionName.nb.fctrmyEducation::U.S.`                                NA
## S.day                                                                <2e-16
## A.day                                                                    NA
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
## NewsDesk.nb.fctrForeign                                                  NA
## NewsDesk.nb.fctrmyMultimedia                                             NA
## NewsDesk.nb.fctrCulture                                                  NA
## NewsDesk.nb.fctrBusiness                                                 NA
## NewsDesk.nb.fctrMagazine                                                 NA
## NewsDesk.nb.fctrTStyle                                                   NA
## NewsDesk.nb.fctrOpEd                                                     NA
## NewsDesk.nb.fctrStyles                                                   NA
## NewsDesk.nb.fctrmyEducation                                              NA
## `NewsDesk.nb.fctrmyMisc::`                                               NA
## NewsDesk.nb.fctrMetro                                                    NA
## `NewsDesk.nb.fctrDaily Clip Report::`                                    NA
## NewsDesk.nb.fctrTravel                                                   NA
## `NewsDesk.nb.fctrReaders Respond::`                                      NA
## NewsDesk.nb.fctrNational                                                 NA
## `NewsDesk.nb.fctrmyEducation::`                                          NA
## NewsDesk.nb.fctrSports                                                   NA
## `NewsDesk.nb.fctrmyPolitics::`                                           NA
## `NewsDesk.nb.fctrVerbatim::`                                             NA
## `NewsDesk.nb.fctrFirst Draft::`                                          NA
## `NewsDesk.nb.fctrToday in Politics::`                                    NA
## `NewsDesk.nb.fctrmyMultimedia::`                                         NA
## `NewsDesk.nb.fctrReporter's Notebook::`                                  NA
## `NewsDesk.nb.fctrThe Daily Gift::`                                       NA
## S.time                                                               <2e-16
## A.time                                                                   NA
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
## `Headline.pfx.fctrDaily Report::`                                    <2e-16
## `Headline.pfx.fctrmyEducation::`                                     <2e-16
## `Headline.pfx.fctr19[0-9][0-9]::`                                    <2e-16
## `Headline.pfx.fctrWord of the Day::`                                 <2e-16
## `Headline.pfx.fctrTest Yourself::`                                   <2e-16
## `Headline.pfx.fctr6 Q's About the News::`                            <2e-16
## `Headline.pfx.fctrNew York Today::`                                  <2e-16
## `Headline.pfx.fctrMorning Agenda::`                                  <2e-16
## `Headline.pfx.fctrDaily Clip Report::`                                   NA
## `Headline.pfx.fctrToday in Small Business::`                         <2e-16
## `Headline.pfx.fctr.*Fashion Week::`                                  <2e-16
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                    <2e-16
## `Headline.pfx.fctrWhat We're::`                                      <2e-16
## `Headline.pfx.fctrmyTech::`                                          <2e-16
## `Headline.pfx.fctrmyMultimedia::`                                    <2e-16
## `Headline.pfx.fctrmyFood::`                                          <2e-16
## `Headline.pfx.fctrmyPolitics::`                                      <2e-16
## `Headline.pfx.fctrAsk Well::`                                        <2e-16
## `Headline.pfx.fctrReaders Respond::`                                 <2e-16
## `Headline.pfx.fctrYour Turn::`                                       <2e-16
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                            <2e-16
## `Headline.pfx.fctrVerbatim::`                                            NA
## `Headline.pfx.fctrFirst Draft::`                                         NA
## `Headline.pfx.fctrOn This Day::`                                     <2e-16
## `Headline.pfx.fctrToday in Politics::`                                   NA
## `Headline.pfx.fctrReporter's Notebook::`                                 NA
## `Headline.pfx.fctrThe Daily Gift::`                                  <2e-16
## H.num.chars.log                                                      <2e-16
## H.num.words.log                                                      <2e-16
## H.num.words.unq.log                                                  <2e-16
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
## PubDate.last10.log                                                 ***
## PubDate.last1.log                                                  ***
## A.can                                                              ***
## S.can                                                              ***
## H.has.ebola                                                        ***
## S.make                                                             ***
## A.make                                                                
## .rnorm                                                             ***
## S.one                                                              ***
## S.state                                                            ***
## A.state                                                            ***
## A.one                                                              ***
## S.said                                                             ***
## A.said                                                                
## PubDate.last100.log                                                ***
## SectionName.nb.fctrWorld                                           ***
## SectionName.nb.fctrMultimedia                                      ***
## SectionName.nb.fctrArts                                            ***
## `SectionName.nb.fctrBusiness Day`                                  ***
## SectionName.nb.fctrMagazine                                        ***
## SectionName.nb.fctrTechnology                                      ***
## `SectionName.nb.fctrCrosswords/Games`                              ***
## SectionName.nb.fctrTStyle                                          ***
## SectionName.nb.fctrOpinion                                         ***
## SectionName.nb.fctrStyles                                          ***
## SectionName.nb.fctrU.S.                                            ***
## `SectionName.nb.fctrmyMisc::`                                      ***
## `SectionName.nb.fctrN.Y. / Region`                                 ***
## `SectionName.nb.fctrDaily Clip Report::`                           ***
## SectionName.nb.fctrTravel                                          ***
## SectionName.nb.fctrOpen                                            ***
## `SectionName.nb.fctrReaders Respond::`                             ***
## `SectionName.nb.fctrmyEducation::`                                 ***
## SectionName.nb.fctrSports                                          ***
## `SectionName.nb.fctrmyPolitics::`                                  ***
## SectionName.nb.fctrNational                                        ***
## `SectionName.nb.fctrVerbatim::`                                    ***
## `SectionName.nb.fctrFirst Draft::`                                 ***
## `SectionName.nb.fctrToday in Politics::`                           ***
## `SectionName.nb.fctrmyMultimedia::`                                ***
## `SectionName.nb.fctrReporter's Notebook::`                         ***
## SectionName.nb.fctrCulture                                            
## `SectionName.nb.fctrThe Daily Gift::`                              ***
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
## A.take                                                                
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
## `SubsectionName.nb.fctrAsia Pacific`                               ***
## `SubsectionName.nb.fctrmyMultimedia::Multimedia`                      
## `SubsectionName.nb.fctrCulture::Arts`                                 
## SubsectionName.nb.fctrDealbook                                     ***
## `SubsectionName.nb.fctrMagazine::Magazine`                            
## `SubsectionName.nb.fctrBusiness::Technology`                       ***
## `SubsectionName.nb.fctrBusiness::Crosswords/Games`                    
## `SubsectionName.nb.fctrTStyle::TStyle`                                
## `SubsectionName.nb.fctrForeign::World`                                
## `SubsectionName.nb.fctrOpEd::Opinion`                              ***
## `SubsectionName.nb.fctrFashion & Style`                            ***
## `SubsectionName.nb.fctrRoom For Debate`                            ***
## SubsectionName.nb.fctrEducation                                    ***
## `SubsectionName.nb.fctrmyMisc::myMisc::`                              
## `SubsectionName.nb.fctrMetro::N.Y. / Region`                          
## `SubsectionName.nb.fctrSmall Business`                                
## `SubsectionName.nb.fctrDaily Clip Report::Daily Clip Report::`        
## `SubsectionName.nb.fctrTravel::Travel`                             ***
## `SubsectionName.nb.fctrThe Public Editor`                             
## `SubsectionName.nb.fctrmyMisc::Open`                                  
## `SubsectionName.nb.fctrReaders Respond::Readers Respond::`            
## SubsectionName.nb.fctrPolitics                                     ***
## `SubsectionName.nb.fctrmyEducation::myEducation::`                    
## `SubsectionName.nb.fctrSports::Sports`                                
## `SubsectionName.nb.fctrmyPolitics::myPolitics::`                      
## `SubsectionName.nb.fctrNational::National`                            
## `SubsectionName.nb.fctrVerbatim::Verbatim::`                          
## `SubsectionName.nb.fctrFirst Draft::First Draft::`                    
## `SubsectionName.nb.fctrToday in Politics::Today in Politics::`        
## `SubsectionName.nb.fctrmyMultimedia::myMultimedia::`                  
## `SubsectionName.nb.fctrReporter's Notebook::Reporter's Notebook::`    
## `SubsectionName.nb.fctrCulture::Culture`                              
## `SubsectionName.nb.fctrTStyle::Technology`                         ***
## `SubsectionName.nb.fctrmyMisc::U.S.`                                  
## `SubsectionName.nb.fctrThe Daily Gift::The Daily Gift::`              
## `SubsectionName.nb.fctrmyMisc::Travel`                                
## `SubsectionName.nb.fctrmyMultimedia::N.Y. / Region`                   
## `SubsectionName.nb.fctrmyEducation::Travel`                           
## `SubsectionName.nb.fctrmyEducation::U.S.`                             
## S.day                                                              ***
## A.day                                                                 
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
## NewsDesk.nb.fctrForeign                                               
## NewsDesk.nb.fctrmyMultimedia                                          
## NewsDesk.nb.fctrCulture                                               
## NewsDesk.nb.fctrBusiness                                              
## NewsDesk.nb.fctrMagazine                                              
## NewsDesk.nb.fctrTStyle                                                
## NewsDesk.nb.fctrOpEd                                                  
## NewsDesk.nb.fctrStyles                                                
## NewsDesk.nb.fctrmyEducation                                           
## `NewsDesk.nb.fctrmyMisc::`                                            
## NewsDesk.nb.fctrMetro                                                 
## `NewsDesk.nb.fctrDaily Clip Report::`                                 
## NewsDesk.nb.fctrTravel                                                
## `NewsDesk.nb.fctrReaders Respond::`                                   
## NewsDesk.nb.fctrNational                                              
## `NewsDesk.nb.fctrmyEducation::`                                       
## NewsDesk.nb.fctrSports                                                
## `NewsDesk.nb.fctrmyPolitics::`                                        
## `NewsDesk.nb.fctrVerbatim::`                                          
## `NewsDesk.nb.fctrFirst Draft::`                                       
## `NewsDesk.nb.fctrToday in Politics::`                                 
## `NewsDesk.nb.fctrmyMultimedia::`                                      
## `NewsDesk.nb.fctrReporter's Notebook::`                               
## `NewsDesk.nb.fctrThe Daily Gift::`                                    
## S.time                                                             ***
## A.time                                                                
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
## `Headline.pfx.fctrDaily Report::`                                  ***
## `Headline.pfx.fctrmyEducation::`                                   ***
## `Headline.pfx.fctr19[0-9][0-9]::`                                  ***
## `Headline.pfx.fctrWord of the Day::`                               ***
## `Headline.pfx.fctrTest Yourself::`                                 ***
## `Headline.pfx.fctr6 Q's About the News::`                          ***
## `Headline.pfx.fctrNew York Today::`                                ***
## `Headline.pfx.fctrMorning Agenda::`                                ***
## `Headline.pfx.fctrDaily Clip Report::`                                
## `Headline.pfx.fctrToday in Small Business::`                       ***
## `Headline.pfx.fctr.*Fashion Week::`                                ***
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                  ***
## `Headline.pfx.fctrWhat We're::`                                    ***
## `Headline.pfx.fctrmyTech::`                                        ***
## `Headline.pfx.fctrmyMultimedia::`                                  ***
## `Headline.pfx.fctrmyFood::`                                        ***
## `Headline.pfx.fctrmyPolitics::`                                    ***
## `Headline.pfx.fctrAsk Well::`                                      ***
## `Headline.pfx.fctrReaders Respond::`                               ***
## `Headline.pfx.fctrYour Turn::`                                     ***
## `Headline.pfx.fctrQuiz(.*)([?=|]|[?=:]::`                          ***
## `Headline.pfx.fctrVerbatim::`                                         
## `Headline.pfx.fctrFirst Draft::`                                      
## `Headline.pfx.fctrOn This Day::`                                   ***
## `Headline.pfx.fctrToday in Politics::`                                
## `Headline.pfx.fctrReporter's Notebook::`                              
## `Headline.pfx.fctrThe Daily Gift::`                                ***
## H.num.chars.log                                                    ***
## H.num.words.log                                                    ***
## H.num.words.unq.log                                                ***
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
## Residual deviance: 35971.6  on 4345  degrees of freedom
## AIC: 36232
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
## 2        0.1 0.6289963
## 3        0.2 0.6289963
## 4        0.3 0.6289963
## 5        0.4 0.6289963
## 6        0.5 0.6289963
## 7        0.6 0.6289963
## 8        0.7 0.6289963
## 9        0.8 0.6289963
## 10       0.9 0.6289963
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3553
## 2            Y                                      326
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      173
## 2                                      423
##          Prediction
## Reference    N    Y
##         N 3553  173
##         Y  326  423
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.884916e-01   5.643785e-01   8.789031e-01   8.975698e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   3.774615e-26   1.014328e-11 
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
## 2        0.1 0.6102819
## 3        0.2 0.6102819
## 4        0.3 0.6102819
## 5        0.4 0.6102819
## 6        0.5 0.6102819
## 7        0.6 0.6102819
## 8        0.7 0.6102819
## 9        0.8 0.6102819
## 10       0.9 0.6102819
## 11       1.0 0.0000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-8.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1638
## 2            Y                                      160
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       75
## 2                                      184
##          Prediction
## Reference    N    Y
##         N 1638   75
##         Y  160  184
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.857560e-01   5.449025e-01   8.712130e-01   8.991889e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   9.084435e-12   4.263949e-08 
##            model_id model_method
## 1 Conditional.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, .rnorm, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     32.478                 9.487
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7591613                    0.9       0.6289963        0.8317447
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8789031             0.8975698     0.4576774   0.7455504
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.6102819         0.885756
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1              0.871213             0.8991889     0.5449025    36231.57
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.1140326       0.2620948
## [1] "fitting model: Conditional.X.no.rnorm.rpart"
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
## + Fold1: cp=0.03605 
## - Fold1: cp=0.03605 
## + Fold2: cp=0.03605 
## - Fold2: cp=0.03605 
## + Fold3: cp=0.03605 
## - Fold3: cp=0.03605 
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.036 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
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
## 1 0.23765020      0 1.0000000
## 2 0.08411215      1 0.7623498
## 3 0.03604806      2 0.6782377
## 
## Variable importance
##              SubsectionName.nb.fctrOpEd::Opinion 
##                                               26 
##                             NewsDesk.nb.fctrOpEd 
##                                               23 
##                       SectionName.nb.fctrOpinion 
##                                               23 
##              SectionName.nb.fctrCrosswords/Games 
##                                               10 
## SubsectionName.nb.fctrBusiness::Crosswords/Games 
##                                               10 
##                              A.num.words.unq.log 
##                                                3 
##                                  A.num.words.log 
##                                                3 
##                              S.num.words.unq.log 
##                                                3 
##                                  H.num.chars.log 
##                                                1 
## 
## Node number 1: 4475 observations,    complexity param=0.2376502
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (4079 obs) right son=3 (396 obs)
##   Primary splits:
##       SubsectionName.nb.fctrOpEd::Opinion < 0.5      to the left,  improve=269.93340, (0 missing)
##       SectionName.nb.fctrOpinion          < 0.5      to the left,  improve=242.83080, (0 missing)
##       NewsDesk.nb.fctrOpEd                < 0.5      to the left,  improve=242.83080, (0 missing)
##       WordCount.log                       < 6.524296 to the left,  improve=104.85530, (0 missing)
##       SectionName.nb.fctrCrosswords/Games < 0.5      to the left,  improve= 85.77765, (0 missing)
##   Surrogate splits:
##       SectionName.nb.fctrOpinion < 0.5      to the left,  agree=0.988, adj=0.864, (0 split)
##       NewsDesk.nb.fctrOpEd       < 0.5      to the left,  agree=0.988, adj=0.864, (0 split)
##       A.num.words.unq.log        < 1.497866 to the right, agree=0.922, adj=0.119, (0 split)
##       A.num.words.log            < 1.497866 to the right, agree=0.922, adj=0.116, (0 split)
##       S.num.words.unq.log        < 1.497866 to the right, agree=0.922, adj=0.116, (0 split)
## 
## Node number 2: 4079 observations,    complexity param=0.08411215
##   predicted class=N  expected loss=0.1132631  P(node) =0.9115084
##     class counts:  3617   462
##    probabilities: 0.887 0.113 
##   left son=4 (3996 obs) right son=5 (83 obs)
##   Primary splits:
##       SectionName.nb.fctrCrosswords/Games              < 0.5      to the left,  improve=99.49081, (0 missing)
##       SubsectionName.nb.fctrBusiness::Crosswords/Games < 0.5      to the left,  improve=99.49081, (0 missing)
##       WordCount.log                                    < 6.470025 to the left,  improve=93.12537, (0 missing)
##       SubsectionName.nb.fctrFashion & Style            < 0.5      to the left,  improve=29.17228, (0 missing)
##       NewsDesk.nb.fctrStyles                           < 0.5      to the left,  improve=29.17228, (0 missing)
##   Surrogate splits:
##       SubsectionName.nb.fctrBusiness::Crosswords/Games < 0.5      to the left,  agree=1.000, adj=1.00, (0 split)
##       H.num.chars.log                                  < 2.35024  to the right, agree=0.981, adj=0.06, (0 split)
## 
## Node number 3: 396 observations
##   predicted class=Y  expected loss=0.2752525  P(node) =0.08849162
##     class counts:   109   287
##    probabilities: 0.275 0.725 
## 
## Node number 4: 3996 observations
##   predicted class=N  expected loss=0.09734735  P(node) =0.8929609
##     class counts:  3607   389
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
##   2) SubsectionName.nb.fctrOpEd::Opinion< 0.5 4079 462 N (0.88673695 0.11326305)  
##     4) SectionName.nb.fctrCrosswords/Games< 0.5 3996 389 N (0.90265265 0.09734735) *
##     5) SectionName.nb.fctrCrosswords/Games>=0.5 83  10 Y (0.12048193 0.87951807) *
##   3) SubsectionName.nb.fctrOpEd::Opinion>=0.5 396 109 Y (0.27525253 0.72474747) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.5863192
## 3        0.2 0.5863192
## 4        0.3 0.5863192
## 5        0.4 0.5863192
## 6        0.5 0.5863192
## 7        0.6 0.5863192
## 8        0.7 0.5863192
## 9        0.8 0.1754808
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rpart.N
## 1            N                                                3607
## 2            Y                                                 389
##   Popular.fctr.predict.Conditional.X.no.rnorm.rpart.Y
## 1                                                 119
## 2                                                 360
##          Prediction
## Reference    N    Y
##         N 3607  119
##         Y  389  360
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.864804e-01   5.241911e-01   8.768211e-01   8.956322e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   2.454466e-24   7.781026e-33 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-13.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.5477477
## 3        0.2 0.5477477
## 4        0.3 0.5477477
## 5        0.4 0.5477477
## 6        0.5 0.5477477
## 7        0.6 0.5477477
## 8        0.7 0.5477477
## 9        0.8 0.1562500
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rpart.N
## 1            N                                                1654
## 2            Y                                                 192
##   Popular.fctr.predict.Conditional.X.no.rnorm.rpart.Y
## 1                                                  59
## 2                                                 152
##          Prediction
## Reference    N    Y
##         N 1654   59
##         Y  192  152
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.779776e-01   4.818624e-01   8.630492e-01   8.918192e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   6.646565e-09   7.964517e-17 
##                       model_id model_method
## 1 Conditional.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     12.222                 2.091
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.725263                    0.7       0.5863192        0.8784344
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8768211             0.8956322     0.4705849   0.7039211
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.5477477        0.8779776
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8630492             0.8918192     0.4818624
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01067017      0.04792138
## [1] "fitting model: Conditional.X.no.rnorm.rf"
## [1] "    indep_vars: WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
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
## + : mtry=103 
## - : mtry=103 
## + : mtry=204 
## - : mtry=204 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 103 on full training set
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
## importance       204   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           204   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-17.png) 

```
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.79175476
## 3        0.2 0.92812887
## 4        0.3 0.98100851
## 5        0.4 1.00000000
## 6        0.5 1.00000000
## 7        0.6 1.00000000
## 8        0.7 0.92693410
## 9        0.8 0.79485117
## 10       0.9 0.55961538
## 11       1.0 0.04693611
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
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
##    threshold    f.score
## 1        0.0 0.28654727
## 2        0.1 0.59439252
## 3        0.2 0.66515837
## 4        0.3 0.68929504
## 5        0.4 0.69489051
## 6        0.5 0.66773163
## 7        0.6 0.64583333
## 8        0.7 0.57751938
## 9        0.8 0.43267108
## 10       0.9 0.25628141
## 11       1.0 0.01156069
```

![](NYTBlogs_zoo_files/figure-html/fit.models_1-20.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1610
## 2            Y                                              106
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                              103
## 2                                              238
##          Prediction
## Reference    N    Y
##         N 1610  103
##         Y  106  238
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.983957e-01   6.339412e-01   8.845233e-01   9.111204e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.472757e-17   8.899694e-01 
##                    model_id model_method
## 1 Conditional.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     feats
## 1 WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                    343.826                86.604
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.6               1        0.9063687
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.999176                     1     0.6363297   0.9258645
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.6948905        0.8983957
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8845233             0.9111204     0.6339412
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    WordCount.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    WordCount.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    WordCount.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    WordCount.log
## 7                                                                                                                                                                                                                                                                                                          WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:SubsectionName.nb.fctr, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
## 8                                                                                                                                                                                                                                                                                   WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, S.can, H.has.ebola, S.make, .rnorm, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
## 9  WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, .rnorm, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 10         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 11         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##    max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1                0                      0.660                 0.003
## 2                0                      0.343                 0.002
## 3                0                      0.732                 0.074
## 4                0                      0.599                 0.058
## 5                3                      1.291                 0.069
## 6                1                      1.183                 0.073
## 7                1                      5.015                 1.273
## 8                1                     18.785                 5.755
## 9                1                     32.478                 9.487
## 10               3                     12.222                 2.091
## 11               3                    343.826                86.604
##    max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.5000000                    0.5       0.0000000        0.8326257
## 2    0.5007516                    0.1       0.2867534        0.1673743
## 3    0.5000000                    0.5       0.0000000        0.8326257
## 4    0.7667409                    0.2       0.4618076        0.7937430
## 5    0.5000000                    0.5       0.0000000        0.8214525
## 6    0.7314944                    0.2       0.4150268        0.8308379
## 7    0.9274991                    0.3       0.7101828        0.8994401
## 8    0.8172319                    0.9       0.7091413        0.9030152
## 9    0.7591613                    0.9       0.6289963        0.8317447
## 10   0.7252630                    0.7       0.5863192        0.8784344
## 11   1.0000000                    0.6       1.0000000        0.9063687
##    max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.8213602             0.8434553    0.00000000   0.5000000
## 2              0.1565447             0.1786398    0.00000000   0.4909227
## 3              0.8213602             0.8434553    0.00000000   0.5000000
## 4              0.7815865             0.8055149    0.33675035   0.6722244
## 5              0.8213602             0.8434553    0.07605123   0.5000000
## 6              0.6941397             0.7210078    0.01207987   0.7289876
## 7              0.8916480             0.9093892    0.59915579   0.9077031
## 8              0.8972220             0.9145342    0.63170613   0.7981501
## 9              0.8789031             0.8975698    0.45767738   0.7455504
## 10             0.8768211             0.8956322    0.47058492   0.7039211
## 11             0.9991760             1.0000000    0.63632966   0.9258645
##    opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                     0.5       0.0000000        0.8327662
## 2                     0.1       0.2865473        0.1672338
## 3                     0.5       0.0000000        0.8327662
## 4                     0.2       0.3763838        0.7535246
## 5                     0.5       0.0000000        0.8327662
## 6                     0.2       0.4061896        0.7015070
## 7                     0.4       0.6850998        0.9003403
## 8                     0.9       0.6798780        0.8979096
## 9                     0.9       0.6102819        0.8857560
## 10                    0.7       0.5477477        0.8779776
## 11                    0.4       0.6948905        0.8983957
##    max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.8159247             0.8486533     0.0000000
## 2              0.1513467             0.1840753     0.0000000
## 3              0.8159247             0.8486533     0.0000000
## 4              0.7343044             0.7720195     0.2272928
## 5              0.8159247             0.8486533     0.0000000
## 6              0.6812119             0.7212263     0.2355742
## 7              0.8865764             0.9129507     0.6261297
## 8              0.8840103             0.9106626     0.6193211
## 9              0.8712130             0.8991889     0.5449025
## 10             0.8630492             0.8918192     0.4818624
## 11             0.8845233             0.9111204     0.6339412
##    max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## 1                  NA              NA          NA
## 2                  NA              NA          NA
## 3                  NA              NA          NA
## 4                  NA              NA          NA
## 5        0.0077087794     0.001192306          NA
## 6        0.0008087605     0.009862351    3664.292
## 7        0.0050978667     0.013543281    2278.103
## 8        0.0070744963     0.019241423   30520.669
## 9        0.1140326216     0.262094754   36231.566
## 10       0.0106701729     0.047921379          NA
## 11                 NA              NA          NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          6          1 218.326 626.795 408.469
## 11 fit.models          6          2 626.796      NA      NA
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    WordCount.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    WordCount.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    WordCount.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    WordCount.log
## 7                                                                                                                                                                                                                                                                                                          WordCount.log, WordCount.log:S.can, WordCount.log:S.make, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.show, WordCount.log:S.report, WordCount.log:S.share, WordCount.log:S.year, WordCount.log:S.compani, WordCount.log:S.first, WordCount.log:SubsectionName.nb.fctr, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.newyork, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:S.week, WordCount.log:S.fashion, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
## 8                                                                                                                                                                                                                                                                                   WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, S.can, H.has.ebola, S.make, .rnorm, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, S.take, PubDate.minute.fctr, S.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, A.num.chars.log, A.num.words.log, A.num.words.unq.log
## 9  WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, .rnorm, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 10         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 11         WordCount.log, PubDate.hour.fctr, H.is.question, PubDate.wkend, PubDate.last10.log, PubDate.last1.log, A.can, S.can, H.has.ebola, S.make, A.make, S.one, S.state, A.state, A.one, S.said, A.said, PubDate.last100.log, SectionName.nb.fctr, PubDate.date.fctr, PubDate.second.fctr, S.presid, A.presid, S.take, A.take, PubDate.minute.fctr, S.new, A.new, PubDate.wkday.fctr, SubsectionName.nb.fctr, S.day, A.day, H.X2014, S.show, A.show, S.report, A.report, S.share, A.share, S.year, A.year, S.compani, A.compani, H.new, S.first, A.first, NewsDesk.nb.fctr, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, A.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, A.week, S.fashion, A.fashion, Headline.pfx.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.chars.log, S.num.chars.log, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##    max.nTuningRuns max.auc.fit opt.prob.threshold.fit max.f.score.fit
## 1                0   0.5000000                    0.5       0.0000000
## 2                0   0.5007516                    0.1       0.2867534
## 3                0   0.5000000                    0.5       0.0000000
## 4                0   0.7667409                    0.2       0.4618076
## 5                3   0.5000000                    0.5       0.0000000
## 6                1   0.7314944                    0.2       0.4150268
## 7                1   0.9274991                    0.3       0.7101828
## 8                1   0.8172319                    0.9       0.7091413
## 9                1   0.7591613                    0.9       0.6289963
## 10               3   0.7252630                    0.7       0.5863192
## 11               3   1.0000000                    0.6       1.0000000
##    max.Accuracy.fit max.Kappa.fit max.auc.OOB opt.prob.threshold.OOB
## 1         0.8326257    0.00000000   0.5000000                    0.5
## 2         0.1673743    0.00000000   0.4909227                    0.1
## 3         0.8326257    0.00000000   0.5000000                    0.5
## 4         0.7937430    0.33675035   0.6722244                    0.2
## 5         0.8214525    0.07605123   0.5000000                    0.5
## 6         0.8308379    0.01207987   0.7289876                    0.2
## 7         0.8994401    0.59915579   0.9077031                    0.4
## 8         0.9030152    0.63170613   0.7981501                    0.9
## 9         0.8317447    0.45767738   0.7455504                    0.9
## 10        0.8784344    0.47058492   0.7039211                    0.7
## 11        0.9063687    0.63632966   0.9258645                    0.4
##    max.f.score.OOB max.Accuracy.OOB max.Kappa.OOB
## 1        0.0000000        0.8327662     0.0000000
## 2        0.2865473        0.1672338     0.0000000
## 3        0.0000000        0.8327662     0.0000000
## 4        0.3763838        0.7535246     0.2272928
## 5        0.0000000        0.8327662     0.0000000
## 6        0.4061896        0.7015070     0.2355742
## 7        0.6850998        0.9003403     0.6261297
## 8        0.6798780        0.8979096     0.6193211
## 9        0.6102819        0.8857560     0.5449025
## 10       0.5477477        0.8779776     0.4818624
## 11       0.6948905        0.8983957     0.6339412
##    inv.elapsedtime.everything inv.elapsedtime.final  inv.aic.fit
## 1                 1.515151515          333.33333333           NA
## 2                 2.915451895          500.00000000           NA
## 3                 1.366120219           13.51351351           NA
## 4                 1.669449082           17.24137931           NA
## 5                 0.774593338           14.49275362           NA
## 6                 0.845308538           13.69863014 2.729040e-04
## 7                 0.199401795            0.78554595 4.389618e-04
## 8                 0.053233963            0.17376195 3.276468e-05
## 9                 0.030790073            0.10540740 2.760024e-05
## 10                0.081819669            0.47824008           NA
## 11                0.002908448            0.01154681           NA
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
## 7       Interact.High.cor.Y.glm        0.9003403   0.9077031     0.6261297
## 11    Conditional.X.no.rnorm.rf        0.8983957   0.9258645     0.6339412
## 8                 Low.cor.X.glm        0.8979096   0.7981501     0.6193211
## 9             Conditional.X.glm        0.8857560   0.7455504     0.5449025
## 10 Conditional.X.no.rnorm.rpart        0.8779776   0.7039211     0.4818624
## 1             MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3          Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 5               Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 4     Max.cor.Y.cv.0.cp.0.rpart        0.7535246   0.6722244     0.2272928
## 6                 Max.cor.Y.glm        0.7015070   0.7289876     0.2355742
## 2       Random.myrandom_classfr        0.1672338   0.4909227     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 7     2278.103                    0.4
## 11          NA                    0.4
## 8    30520.669                    0.9
## 9    36231.566                    0.9
## 10          NA                    0.7
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
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
## [1] "Best model id: Interact.High.cor.Y.glm"
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
```

```
## Warning: glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does
## not currently support interaction terms
```

```r
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](NYTBlogs_zoo_files/figure-html/fit.models_2-4.png) 

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
## importance       204   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           204   -none-     character
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
## SubsectionName.nb.fctr SubsectionName.nb.fctr -0.042936460           FALSE
## WordCount.log                   WordCount.log  0.264960434           FALSE
## PubDate.last100.log       PubDate.last100.log -0.007663322           FALSE
## PubDate.last10.log         PubDate.last10.log  0.049317022           FALSE
## SectionName.nb.fctr       SectionName.nb.fctr -0.008268577           FALSE
## NewsDesk.nb.fctr             NewsDesk.nb.fctr -0.053443087           FALSE
## H.num.chars.log               H.num.chars.log -0.171062360           FALSE
## PubDate.last1.log           PubDate.last1.log  0.046357515           FALSE
## S.num.chars.log               S.num.chars.log -0.224692967           FALSE
## A.num.chars.log               A.num.chars.log -0.224548821           FALSE
## Headline.pfx.fctr           Headline.pfx.fctr -0.100963501           FALSE
## H.num.words.log               H.num.words.log -0.200686356           FALSE
## S.num.words.unq.log       S.num.words.unq.log -0.250796919           FALSE
## H.num.words.unq.log       H.num.words.unq.log -0.204496360           FALSE
## A.num.words.unq.log       A.num.words.unq.log -0.250601203           FALSE
## A.num.words.log               A.num.words.log -0.245073324           FALSE
## S.num.words.log               S.num.words.log -0.245354135           FALSE
## H.is.question                   H.is.question  0.129154799           FALSE
## PubDate.hour.fctr           PubDate.hour.fctr  0.135436805           FALSE
## PubDate.minute.fctr       PubDate.minute.fctr -0.034073846           FALSE
## PubDate.wkday.fctr         PubDate.wkday.fctr -0.039801288           FALSE
## PubDate.date.fctr           PubDate.date.fctr -0.011647558           FALSE
## PubDate.second.fctr       PubDate.second.fctr -0.011879458           FALSE
## S.new                                   S.new -0.034948520           FALSE
## A.time                                 A.time -0.057790617           FALSE
## H.today                               H.today -0.063723058           FALSE
## PubDate.wkend                   PubDate.wkend  0.106728760           FALSE
## A.new                                   A.new -0.035359447           FALSE
## S.time                                 S.time -0.057595102           FALSE
## A.make                                 A.make  0.023138853           FALSE
## S.one                                   S.one  0.006342094           FALSE
## A.one                                   A.one  0.005696039           FALSE
## A.state                               A.state  0.005702163           FALSE
## A.can                                   A.can  0.031498867           FALSE
## S.state                               S.state  0.006069626           FALSE
## S.make                                 S.make  0.023138853           FALSE
## A.will                                 A.will -0.061025004           FALSE
## S.can                                   S.can  0.029999780           FALSE
## S.report                             S.report -0.050211524           FALSE
## S.week                                 S.week -0.084814939           FALSE
## A.year                                 A.year -0.051146178           FALSE
## S.said                                 S.said  0.001363226           FALSE
## A.said                                 A.said  0.001363226           FALSE
## S.year                                 S.year -0.051146178           FALSE
## A.week                                 A.week -0.084814939           FALSE
## H.has.ebola                       H.has.ebola  0.025881397           FALSE
## S.will                                 S.will -0.060575493           FALSE
## A.report                             A.report -0.050211524           FALSE
## S.compani                           S.compani -0.053012962           FALSE
## H.newyork                           H.newyork -0.057970095           FALSE
## A.show                                 A.show -0.048801740           FALSE
## S.newyork                           S.newyork -0.062117105           FALSE
## A.compani                           A.compani -0.053099633           FALSE
## S.show                                 S.show -0.048801740           FALSE
## S.take                                 S.take -0.025762398           FALSE
## S.share                               S.share -0.050329686           FALSE
## A.take                                 A.take -0.026086108           FALSE
## A.share                               A.share -0.050329686           FALSE
## A.newyork                           A.newyork -0.062117105           FALSE
## A.day                                   A.day -0.045909684           FALSE
## S.day                                   S.day -0.045649185           FALSE
## A.first                               A.first -0.053388178           FALSE
## S.presid                             S.presid -0.019828826           FALSE
## H.new                                   H.new -0.053121542           FALSE
## A.presid                             A.presid -0.019828826           FALSE
## S.first                               S.first -0.053388178           FALSE
## H.day                                   H.day -0.061669687           FALSE
## S.intern                             S.intern -0.068485701           FALSE
## H.week                                 H.week -0.075105216           FALSE
## A.intern                             A.intern -0.068485701           FALSE
## S.articl                             S.articl -0.059520554           FALSE
## A.articl                             A.articl -0.059520554           FALSE
## H.report                             H.report -0.064948102           FALSE
## H.fashion                           H.fashion -0.081708612           FALSE
## H.X2014                               H.X2014 -0.046206380           FALSE
## A.fashion                           A.fashion -0.086446251           FALSE
## S.fashion                           S.fashion -0.086446251           FALSE
## .rnorm                                 .rnorm  0.017561723           FALSE
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
## PubDate.last1                   PubDate.last1  0.035922671            TRUE
## PubDate.last10                 PubDate.last10  0.053980930            TRUE
## PubDate.last100               PubDate.last100  0.039892288            TRUE
## PubDate.month.fctr         PubDate.month.fctr  0.019148739            TRUE
## PubDate.POSIX                   PubDate.POSIX  0.015683258            TRUE
## PubDate.year.fctr           PubDate.year.fctr           NA           FALSE
## PubDate.zoo                       PubDate.zoo  0.015683258            TRUE
## S.has.http                         S.has.http           NA           FALSE
## S.num.chars                       S.num.chars -0.179331806            TRUE
## S.num.words                       S.num.words -0.206385049            TRUE
## S.num.words.unq               S.num.words.unq -0.212102717            TRUE
## UniqueID                             UniqueID  0.011824920            TRUE
## WordCount                           WordCount  0.257526549            TRUE
##                          cor.y.abs             cor.high.X
## SubsectionName.nb.fctr 0.042936460                   <NA>
## WordCount.log          0.264960434                   <NA>
## PubDate.last100.log    0.007663322                   <NA>
## PubDate.last10.log     0.049317022                   <NA>
## SectionName.nb.fctr    0.008268577                   <NA>
## NewsDesk.nb.fctr       0.053443087 SubsectionName.nb.fctr
## H.num.chars.log        0.171062360                   <NA>
## PubDate.last1.log      0.046357515                   <NA>
## S.num.chars.log        0.224692967        A.num.chars.log
## A.num.chars.log        0.224548821                   <NA>
## Headline.pfx.fctr      0.100963501                   <NA>
## H.num.words.log        0.200686356                   <NA>
## S.num.words.unq.log    0.250796919        S.num.chars.log
## H.num.words.unq.log    0.204496360        H.num.chars.log
## A.num.words.unq.log    0.250601203                   <NA>
## A.num.words.log        0.245073324                   <NA>
## S.num.words.log        0.245354135        A.num.words.log
## H.is.question          0.129154799                   <NA>
## PubDate.hour.fctr      0.135436805                   <NA>
## PubDate.minute.fctr    0.034073846                   <NA>
## PubDate.wkday.fctr     0.039801288                   <NA>
## PubDate.date.fctr      0.011647558                   <NA>
## PubDate.second.fctr    0.011879458                   <NA>
## S.new                  0.034948520                   <NA>
## A.time                 0.057790617                 S.time
## H.today                0.063723058                   <NA>
## PubDate.wkend          0.106728760                   <NA>
## A.new                  0.035359447                  S.new
## S.time                 0.057595102                   <NA>
## A.make                 0.023138853                 S.make
## S.one                  0.006342094                   <NA>
## A.one                  0.005696039                   <NA>
## A.state                0.005702163                   <NA>
## A.can                  0.031498867                  S.can
## S.state                0.006069626                   <NA>
## S.make                 0.023138853                   <NA>
## A.will                 0.061025004                 S.will
## S.can                  0.029999780                   <NA>
## S.report               0.050211524                   <NA>
## S.week                 0.084814939                   <NA>
## A.year                 0.051146178                 S.year
## S.said                 0.001363226                   <NA>
## A.said                 0.001363226                   <NA>
## S.year                 0.051146178                   <NA>
## A.week                 0.084814939                 S.week
## H.has.ebola            0.025881397                   <NA>
## S.will                 0.060575493                   <NA>
## A.report               0.050211524               S.report
## S.compani              0.053012962                   <NA>
## H.newyork              0.057970095                   <NA>
## A.show                 0.048801740                 S.show
## S.newyork              0.062117105                   <NA>
## A.compani              0.053099633              S.compani
## S.show                 0.048801740                   <NA>
## S.take                 0.025762398                   <NA>
## S.share                0.050329686                   <NA>
## A.take                 0.026086108                 S.take
## A.share                0.050329686                S.share
## A.newyork              0.062117105              S.newyork
## A.day                  0.045909684                  S.day
## S.day                  0.045649185                   <NA>
## A.first                0.053388178                S.first
## S.presid               0.019828826                   <NA>
## H.new                  0.053121542                   <NA>
## A.presid               0.019828826               S.presid
## S.first                0.053388178                   <NA>
## H.day                  0.061669687                   <NA>
## S.intern               0.068485701                   <NA>
## H.week                 0.075105216                   <NA>
## A.intern               0.068485701               S.intern
## S.articl               0.059520554                   <NA>
## A.articl               0.059520554               S.articl
## H.report               0.064948102                   <NA>
## H.fashion              0.081708612                 H.week
## H.X2014                0.046206380                   <NA>
## A.fashion              0.086446251              S.fashion
## S.fashion              0.086446251                   <NA>
## .rnorm                 0.017561723                   <NA>
## A.has.http             0.013592603                   <NA>
## A.num.chars            0.177037425                   <NA>
## A.num.words            0.204211072                   <NA>
## A.num.words.unq        0.210242145                   <NA>
## H.daili                0.069192975                   <NA>
## H.has.http                      NA                   <NA>
## H.num.chars            0.147211183                   <NA>
## H.num.words            0.186036895                   <NA>
## H.num.words.unq        0.189702157                   <NA>
## H.X2015                0.066584892                   <NA>
## Popular                1.000000000                   <NA>
## Popular.fctr                    NA                   <NA>
## PubDate.last1          0.035922671                   <NA>
## PubDate.last10         0.053980930                   <NA>
## PubDate.last100        0.039892288                   <NA>
## PubDate.month.fctr     0.019148739                   <NA>
## PubDate.POSIX          0.015683258                   <NA>
## PubDate.year.fctr               NA                   <NA>
## PubDate.zoo            0.015683258                   <NA>
## S.has.http                      NA                   <NA>
## S.num.chars            0.179331806                   <NA>
## S.num.words            0.206385049                   <NA>
## S.num.words.unq        0.212102717                   <NA>
## UniqueID               0.011824920                   <NA>
## WordCount              0.257526549                   <NA>
##                        is.ConditionalX.y is.cor.y.abs.low rsp_var_raw
## SubsectionName.nb.fctr              TRUE            FALSE       FALSE
## WordCount.log                       TRUE            FALSE       FALSE
## PubDate.last100.log                 TRUE             TRUE       FALSE
## PubDate.last10.log                  TRUE            FALSE       FALSE
## SectionName.nb.fctr                 TRUE             TRUE       FALSE
## NewsDesk.nb.fctr                    TRUE            FALSE       FALSE
## H.num.chars.log                     TRUE            FALSE       FALSE
## PubDate.last1.log                   TRUE            FALSE       FALSE
## S.num.chars.log                     TRUE            FALSE       FALSE
## A.num.chars.log                     TRUE            FALSE       FALSE
## Headline.pfx.fctr                   TRUE            FALSE       FALSE
## H.num.words.log                     TRUE            FALSE       FALSE
## S.num.words.unq.log                 TRUE            FALSE       FALSE
## H.num.words.unq.log                 TRUE            FALSE       FALSE
## A.num.words.unq.log                 TRUE            FALSE       FALSE
## A.num.words.log                     TRUE            FALSE       FALSE
## S.num.words.log                     TRUE            FALSE       FALSE
## H.is.question                       TRUE            FALSE       FALSE
## PubDate.hour.fctr                   TRUE            FALSE       FALSE
## PubDate.minute.fctr                 TRUE            FALSE       FALSE
## PubDate.wkday.fctr                  TRUE            FALSE       FALSE
## PubDate.date.fctr                   TRUE             TRUE       FALSE
## PubDate.second.fctr                 TRUE             TRUE       FALSE
## S.new                               TRUE            FALSE       FALSE
## A.time                              TRUE            FALSE       FALSE
## H.today                             TRUE            FALSE       FALSE
## PubDate.wkend                       TRUE            FALSE       FALSE
## A.new                               TRUE            FALSE       FALSE
## S.time                              TRUE            FALSE       FALSE
## A.make                              TRUE            FALSE       FALSE
## S.one                               TRUE             TRUE       FALSE
## A.one                               TRUE             TRUE       FALSE
## A.state                             TRUE             TRUE       FALSE
## A.can                               TRUE            FALSE       FALSE
## S.state                             TRUE             TRUE       FALSE
## S.make                              TRUE            FALSE       FALSE
## A.will                              TRUE            FALSE       FALSE
## S.can                               TRUE            FALSE       FALSE
## S.report                            TRUE            FALSE       FALSE
## S.week                              TRUE            FALSE       FALSE
## A.year                              TRUE            FALSE       FALSE
## S.said                              TRUE             TRUE       FALSE
## A.said                              TRUE             TRUE       FALSE
## S.year                              TRUE            FALSE       FALSE
## A.week                              TRUE            FALSE       FALSE
## H.has.ebola                         TRUE            FALSE       FALSE
## S.will                              TRUE            FALSE       FALSE
## A.report                            TRUE            FALSE       FALSE
## S.compani                           TRUE            FALSE       FALSE
## H.newyork                           TRUE            FALSE       FALSE
## A.show                              TRUE            FALSE       FALSE
## S.newyork                           TRUE            FALSE       FALSE
## A.compani                           TRUE            FALSE       FALSE
## S.show                              TRUE            FALSE       FALSE
## S.take                              TRUE            FALSE       FALSE
## S.share                             TRUE            FALSE       FALSE
## A.take                              TRUE            FALSE       FALSE
## A.share                             TRUE            FALSE       FALSE
## A.newyork                           TRUE            FALSE       FALSE
## A.day                               TRUE            FALSE       FALSE
## S.day                               TRUE            FALSE       FALSE
## A.first                             TRUE            FALSE       FALSE
## S.presid                            TRUE            FALSE       FALSE
## H.new                               TRUE            FALSE       FALSE
## A.presid                            TRUE            FALSE       FALSE
## S.first                             TRUE            FALSE       FALSE
## H.day                               TRUE            FALSE       FALSE
## S.intern                            TRUE            FALSE       FALSE
## H.week                              TRUE            FALSE       FALSE
## A.intern                            TRUE            FALSE       FALSE
## S.articl                            TRUE            FALSE       FALSE
## A.articl                            TRUE            FALSE       FALSE
## H.report                            TRUE            FALSE       FALSE
## H.fashion                           TRUE            FALSE       FALSE
## H.X2014                             TRUE            FALSE       FALSE
## A.fashion                           TRUE            FALSE       FALSE
## S.fashion                           TRUE            FALSE       FALSE
## .rnorm                              TRUE            FALSE       FALSE
## A.has.http                         FALSE             TRUE       FALSE
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
## PubDate.last1                         NA            FALSE       FALSE
## PubDate.last10                        NA            FALSE       FALSE
## PubDate.last100                       NA            FALSE       FALSE
## PubDate.month.fctr                    NA            FALSE       FALSE
## PubDate.POSIX                         NA             TRUE       FALSE
## PubDate.year.fctr                  FALSE               NA       FALSE
## PubDate.zoo                           NA             TRUE       FALSE
## S.has.http                         FALSE               NA       FALSE
## S.num.chars                           NA            FALSE       FALSE
## S.num.words                           NA            FALSE       FALSE
## S.num.words.unq                       NA            FALSE       FALSE
## UniqueID                              NA             TRUE       FALSE
## WordCount                             NA            FALSE       FALSE
##                        id_var rsp_var   importance
## SubsectionName.nb.fctr     NA      NA 100.00000000
## WordCount.log              NA      NA  94.30030184
## PubDate.last100.log        NA      NA  37.29768398
## PubDate.last10.log         NA      NA  35.51625203
## SectionName.nb.fctr        NA      NA  33.06860059
## NewsDesk.nb.fctr           NA      NA  31.25476107
## H.num.chars.log            NA      NA  28.70058185
## PubDate.last1.log          NA      NA  28.11210210
## S.num.chars.log            NA      NA  22.95909353
## A.num.chars.log            NA      NA  22.56904795
## Headline.pfx.fctr          NA      NA  12.17152304
## H.num.words.log            NA      NA  11.06009209
## S.num.words.unq.log        NA      NA  10.86918824
## H.num.words.unq.log        NA      NA  10.76992746
## A.num.words.unq.log        NA      NA  10.61417076
## A.num.words.log            NA      NA  10.14672470
## S.num.words.log            NA      NA  10.06685805
## H.is.question              NA      NA   8.97471155
## PubDate.hour.fctr          NA      NA   7.51625249
## PubDate.minute.fctr        NA      NA   4.56523569
## PubDate.wkday.fctr         NA      NA   4.14933362
## PubDate.date.fctr          NA      NA   3.61764162
## PubDate.second.fctr        NA      NA   3.57886654
## S.new                      NA      NA   2.65358458
## A.time                     NA      NA   2.58312526
## H.today                    NA      NA   2.53264879
## PubDate.wkend              NA      NA   2.47953439
## A.new                      NA      NA   2.38406842
## S.time                     NA      NA   2.38356432
## A.make                     NA      NA   1.80720536
## S.one                      NA      NA   1.80378543
## A.one                      NA      NA   1.79335380
## A.state                    NA      NA   1.76140717
## A.can                      NA      NA   1.72190392
## S.state                    NA      NA   1.66615691
## S.make                     NA      NA   1.60598618
## A.will                     NA      NA   1.58859780
## S.can                      NA      NA   1.57168576
## S.report                   NA      NA   1.57059871
## S.week                     NA      NA   1.53622041
## A.year                     NA      NA   1.47700205
## S.said                     NA      NA   1.44549130
## A.said                     NA      NA   1.41194948
## S.year                     NA      NA   1.39955820
## A.week                     NA      NA   1.39768019
## H.has.ebola                NA      NA   1.38192267
## S.will                     NA      NA   1.36098406
## A.report                   NA      NA   1.25711463
## S.compani                  NA      NA   1.24774975
## H.newyork                  NA      NA   1.21747941
## A.show                     NA      NA   1.13826690
## S.newyork                  NA      NA   1.09731978
## A.compani                  NA      NA   1.09386590
## S.show                     NA      NA   1.06000695
## S.take                     NA      NA   0.97321587
## S.share                    NA      NA   0.97086880
## A.take                     NA      NA   0.93748272
## A.share                    NA      NA   0.92875730
## A.newyork                  NA      NA   0.92041517
## A.day                      NA      NA   0.72851403
## S.day                      NA      NA   0.71351166
## A.first                    NA      NA   0.62529618
## S.presid                   NA      NA   0.58432843
## H.new                      NA      NA   0.57785111
## A.presid                   NA      NA   0.56752811
## S.first                    NA      NA   0.54907531
## H.day                      NA      NA   0.27413575
## S.intern                   NA      NA   0.27180235
## H.week                     NA      NA   0.26550844
## A.intern                   NA      NA   0.23659607
## S.articl                   NA      NA   0.22009993
## A.articl                   NA      NA   0.20321053
## H.report                   NA      NA   0.20308136
## H.fashion                  NA      NA   0.17849990
## H.X2014                    NA      NA   0.12226971
## A.fashion                  NA      NA   0.09074034
## S.fashion                  NA      NA   0.06758402
## .rnorm                     NA      NA           NA
## A.has.http                 NA      NA           NA
## A.num.chars                NA      NA           NA
## A.num.words                NA      NA           NA
## A.num.words.unq            NA      NA           NA
## H.daili                    NA      NA           NA
## H.has.http                 NA      NA           NA
## H.num.chars                NA      NA           NA
## H.num.words                NA      NA           NA
## H.num.words.unq            NA      NA           NA
## H.X2015                    NA      NA           NA
## Popular                    NA      NA           NA
## Popular.fctr               NA    TRUE           NA
## PubDate.last1              NA      NA           NA
## PubDate.last10             NA      NA           NA
## PubDate.last100            NA      NA           NA
## PubDate.month.fctr         NA      NA           NA
## PubDate.POSIX              NA      NA           NA
## PubDate.year.fctr          NA      NA           NA
## PubDate.zoo                NA      NA           NA
## S.has.http                 NA      NA           NA
## S.num.chars                NA      NA           NA
## S.num.words                NA      NA           NA
## S.num.words.unq            NA      NA           NA
## UniqueID                 TRUE      NA           NA
## WordCount                  NA      NA           NA
##                        Conditional.X.no.rnorm.rf.importance
## SubsectionName.nb.fctr                         100.00000000
## WordCount.log                                   94.30030184
## PubDate.last100.log                             37.29768398
## PubDate.last10.log                              35.51625203
## SectionName.nb.fctr                             33.06860059
## NewsDesk.nb.fctr                                31.25476107
## H.num.chars.log                                 28.70058185
## PubDate.last1.log                               28.11210210
## S.num.chars.log                                 22.95909353
## A.num.chars.log                                 22.56904795
## Headline.pfx.fctr                               12.17152304
## H.num.words.log                                 11.06009209
## S.num.words.unq.log                             10.86918824
## H.num.words.unq.log                             10.76992746
## A.num.words.unq.log                             10.61417076
## A.num.words.log                                 10.14672470
## S.num.words.log                                 10.06685805
## H.is.question                                    8.97471155
## PubDate.hour.fctr                                7.51625249
## PubDate.minute.fctr                              4.56523569
## PubDate.wkday.fctr                               4.14933362
## PubDate.date.fctr                                3.61764162
## PubDate.second.fctr                              3.57886654
## S.new                                            2.65358458
## A.time                                           2.58312526
## H.today                                          2.53264879
## PubDate.wkend                                    2.47953439
## A.new                                            2.38406842
## S.time                                           2.38356432
## A.make                                           1.80720536
## S.one                                            1.80378543
## A.one                                            1.79335380
## A.state                                          1.76140717
## A.can                                            1.72190392
## S.state                                          1.66615691
## S.make                                           1.60598618
## A.will                                           1.58859780
## S.can                                            1.57168576
## S.report                                         1.57059871
## S.week                                           1.53622041
## A.year                                           1.47700205
## S.said                                           1.44549130
## A.said                                           1.41194948
## S.year                                           1.39955820
## A.week                                           1.39768019
## H.has.ebola                                      1.38192267
## S.will                                           1.36098406
## A.report                                         1.25711463
## S.compani                                        1.24774975
## H.newyork                                        1.21747941
## A.show                                           1.13826690
## S.newyork                                        1.09731978
## A.compani                                        1.09386590
## S.show                                           1.06000695
## S.take                                           0.97321587
## S.share                                          0.97086880
## A.take                                           0.93748272
## A.share                                          0.92875730
## A.newyork                                        0.92041517
## A.day                                            0.72851403
## S.day                                            0.71351166
## A.first                                          0.62529618
## S.presid                                         0.58432843
## H.new                                            0.57785111
## A.presid                                         0.56752811
## S.first                                          0.54907531
## H.day                                            0.27413575
## S.intern                                         0.27180235
## H.week                                           0.26550844
## A.intern                                         0.23659607
## S.articl                                         0.22009993
## A.articl                                         0.20321053
## H.report                                         0.20308136
## H.fashion                                        0.17849990
## H.X2014                                          0.12226971
## A.fashion                                        0.09074034
## S.fashion                                        0.06758402
## .rnorm                                                   NA
## A.has.http                                               NA
## A.num.chars                                              NA
## A.num.words                                              NA
## A.num.words.unq                                          NA
## H.daili                                                  NA
## H.has.http                                               NA
## H.num.chars                                              NA
## H.num.words                                              NA
## H.num.words.unq                                          NA
## H.X2015                                                  NA
## Popular                                                  NA
## Popular.fctr                                             NA
## PubDate.last1                                            NA
## PubDate.last10                                           NA
## PubDate.last100                                          NA
## PubDate.month.fctr                                       NA
## PubDate.POSIX                                            NA
## PubDate.year.fctr                                        NA
## PubDate.zoo                                              NA
## S.has.http                                               NA
## S.num.chars                                              NA
## S.num.words                                              NA
## S.num.words.unq                                          NA
## UniqueID                                                 NA
## WordCount                                                NA
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 77
```

![](NYTBlogs_zoo_files/figure-html/fit.models_2-5.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_2-6.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_2-7.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_2-8.png) ![](NYTBlogs_zoo_files/figure-html/fit.models_2-9.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr
## 6370     6370            Y
## 6018     6018            N
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 6370                                               0.244
## 6018                                               0.006
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 6370                                              N
## 6018                                              N
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 6370                                                   FALSE
## 6018                                                    TRUE
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.error .label
## 6370                                               -0.156   6370
## 6018                                                0.000   6018
## [1] "Inaccurate: "
##      UniqueID Popular.fctr
## 3074     3074            Y
## 2527     2527            Y
## 4775     4775            Y
## 814       814            Y
## 4090     4090            Y
## 5619     5619            Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 3074                                               0.004
## 2527                                               0.008
## 4775                                               0.008
## 814                                                0.010
## 4090                                               0.010
## 5619                                               0.010
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 3074                                              N
## 2527                                              N
## 4775                                              N
## 814                                               N
## 4090                                              N
## 5619                                              N
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 3074                                                   FALSE
## 2527                                                   FALSE
## 4775                                                   FALSE
## 814                                                    FALSE
## 4090                                                   FALSE
## 5619                                                   FALSE
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.error
## 3074                                               -0.396
## 2527                                               -0.392
## 4775                                               -0.392
## 814                                                -0.390
## 4090                                               -0.390
## 5619                                               -0.390
##      UniqueID Popular.fctr
## 814       814            Y
## 3976     3976            Y
## 3366     3366            Y
## 2088     2088            Y
## 3840     3840            N
## 5834     5834            N
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 814                                                0.010
## 3976                                               0.144
## 3366                                               0.254
## 2088                                               0.296
## 3840                                               0.436
## 5834                                               0.634
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 814                                               N
## 3976                                              N
## 3366                                              N
## 2088                                              N
## 3840                                              Y
## 5834                                              Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 814                                                    FALSE
## 3976                                                   FALSE
## 3366                                                   FALSE
## 2088                                                   FALSE
## 3840                                                   FALSE
## 5834                                                   FALSE
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.error
## 814                                                -0.390
## 3976                                               -0.256
## 3366                                               -0.146
## 2088                                               -0.104
## 3840                                                0.036
## 5834                                                0.234
##      UniqueID Popular.fctr
## 785       785            N
## 1667     1667            N
## 4377     4377            N
## 3258     3258            N
## 4227     4227            N
## 4763     4763            N
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 785                                                0.886
## 1667                                               0.886
## 4377                                               0.894
## 3258                                               0.902
## 4227                                               0.944
## 4763                                               0.976
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 785                                               Y
## 1667                                              Y
## 4377                                              Y
## 3258                                              Y
## 4227                                              Y
## 4763                                              Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 785                                                    FALSE
## 1667                                                   FALSE
## 4377                                                   FALSE
## 3258                                                   FALSE
## 4227                                                   FALSE
## 4763                                                   FALSE
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.error
## 785                                                 0.486
## 1667                                                0.486
## 4377                                                0.494
## 3258                                                0.502
## 4227                                                0.544
## 4763                                                0.576
```

![](NYTBlogs_zoo_files/figure-html/fit.models_2-10.png) 

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
## [2] Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob    
## [3] Popular.fctr.predict.Conditional.X.no.rnorm.rf         
## [4] Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
## [1] SubsectionName.nb.fctr WordCount.log          PubDate.last100.log   
## [4] PubDate.last10.log     SectionName.nb.fctr   
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
## 11 fit.models          6          2 626.796 643.632  16.836
## 12 fit.models          6          3 643.633      NA      NA
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

![](NYTBlogs_zoo_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 12        fit.models          6          3 643.633 717.215  73.582
## 13 fit.data.training          7          0 717.216      NA      NA
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
## SubsectionName.nb.fctr SubsectionName.nb.fctr 100.00000000
## WordCount.log                   WordCount.log  94.30030184
## PubDate.last100.log       PubDate.last100.log  37.29768398
## PubDate.last10.log         PubDate.last10.log  35.51625203
## SectionName.nb.fctr       SectionName.nb.fctr  33.06860059
## NewsDesk.nb.fctr             NewsDesk.nb.fctr  31.25476107
## H.num.chars.log               H.num.chars.log  28.70058185
## PubDate.last1.log           PubDate.last1.log  28.11210210
## S.num.chars.log               S.num.chars.log  22.95909353
## A.num.chars.log               A.num.chars.log  22.56904795
## Headline.pfx.fctr           Headline.pfx.fctr  12.17152304
## H.num.words.log               H.num.words.log  11.06009209
## S.num.words.unq.log       S.num.words.unq.log  10.86918824
## H.num.words.unq.log       H.num.words.unq.log  10.76992746
## A.num.words.unq.log       A.num.words.unq.log  10.61417076
## A.num.words.log               A.num.words.log  10.14672470
## S.num.words.log               S.num.words.log  10.06685805
## H.is.question                   H.is.question   8.97471155
## PubDate.hour.fctr           PubDate.hour.fctr   7.51625249
## PubDate.minute.fctr       PubDate.minute.fctr   4.56523569
## PubDate.wkday.fctr         PubDate.wkday.fctr   4.14933362
## PubDate.date.fctr           PubDate.date.fctr   3.61764162
## PubDate.second.fctr       PubDate.second.fctr   3.57886654
## S.new                                   S.new   2.65358458
## A.time                                 A.time   2.58312526
## H.today                               H.today   2.53264879
## PubDate.wkend                   PubDate.wkend   2.47953439
## A.new                                   A.new   2.38406842
## S.time                                 S.time   2.38356432
## A.make                                 A.make   1.80720536
## S.one                                   S.one   1.80378543
## A.one                                   A.one   1.79335380
## A.state                               A.state   1.76140717
## A.can                                   A.can   1.72190392
## S.state                               S.state   1.66615691
## S.make                                 S.make   1.60598618
## A.will                                 A.will   1.58859780
## S.can                                   S.can   1.57168576
## S.report                             S.report   1.57059871
## S.week                                 S.week   1.53622041
## A.year                                 A.year   1.47700205
## S.said                                 S.said   1.44549130
## A.said                                 A.said   1.41194948
## S.year                                 S.year   1.39955820
## A.week                                 A.week   1.39768019
## H.has.ebola                       H.has.ebola   1.38192267
## S.will                                 S.will   1.36098406
## A.report                             A.report   1.25711463
## S.compani                           S.compani   1.24774975
## H.newyork                           H.newyork   1.21747941
## A.show                                 A.show   1.13826690
## S.newyork                           S.newyork   1.09731978
## A.compani                           A.compani   1.09386590
## S.show                                 S.show   1.06000695
## S.take                                 S.take   0.97321587
## S.share                               S.share   0.97086880
## A.take                                 A.take   0.93748272
## A.share                               A.share   0.92875730
## A.newyork                           A.newyork   0.92041517
## A.day                                   A.day   0.72851403
## S.day                                   S.day   0.71351166
## A.first                               A.first   0.62529618
## S.presid                             S.presid   0.58432843
## H.new                                   H.new   0.57785111
## A.presid                             A.presid   0.56752811
## S.first                               S.first   0.54907531
## H.day                                   H.day   0.27413575
## S.intern                             S.intern   0.27180235
## H.week                                 H.week   0.26550844
## A.intern                             A.intern   0.23659607
## S.articl                             S.articl   0.22009993
## A.articl                             A.articl   0.20321053
## H.report                             H.report   0.20308136
## H.fashion                           H.fashion   0.17849990
## H.X2014                               H.X2014   0.12226971
## A.fashion                           A.fashion   0.09074034
## S.fashion                           S.fashion   0.06758402
## [1] "fitting model: Final.rf"
## [1] "    indep_vars: SubsectionName.nb.fctr, WordCount.log, PubDate.last100.log, PubDate.last10.log, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, PubDate.last1.log, S.num.chars.log, A.num.chars.log, Headline.pfx.fctr, H.num.words.log, S.num.words.unq.log, H.num.words.unq.log, A.num.words.unq.log, A.num.words.log, S.num.words.log, H.is.question, PubDate.hour.fctr, PubDate.minute.fctr, PubDate.wkday.fctr, PubDate.date.fctr, PubDate.second.fctr, S.new, A.time, H.today, PubDate.wkend, A.new, S.time, A.make, S.one, A.one, A.state, A.can, S.state, S.make, A.will, S.can, S.report, S.week, A.year, S.said, A.said, S.year, A.week, H.has.ebola, S.will, A.report, S.compani, H.newyork, A.show, S.newyork, A.compani, S.show, S.take, S.share, A.take, A.share, A.newyork, A.day, S.day, A.first, S.presid, H.new, A.presid, S.first, H.day, S.intern, H.week, A.intern, S.articl, A.articl, H.report, H.fashion, H.X2014, A.fashion, S.fashion"
## + : mtry=103 
## - : mtry=103 
## Aggregating results
## Fitting final model on full training set
```

![](NYTBlogs_zoo_files/figure-html/fit.data.training_0-1.png) 

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
## importance        204  -none-     numeric  
## importanceSD        0  -none-     NULL     
## localImportance     0  -none-     NULL     
## proximity           0  -none-     NULL     
## ntree               1  -none-     numeric  
## mtry                1  -none-     numeric  
## forest             14  -none-     list     
## y                6532  factor     numeric  
## test                0  -none-     NULL     
## inbag               0  -none-     NULL     
## xNames            204  -none-     character
## problemType         1  -none-     character
## tuneValue           1  data.frame list     
## obsLevels           2  -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_zoo_files/figure-html/fit.data.training_0-2.png) 

```
##    threshold   f.score
## 1        0.0 0.2866885
## 2        0.1 0.7926033
## 3        0.2 0.9266638
## 4        0.3 0.9811490
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 0.9986257
## 8        0.7 0.9357352
## 9        0.8 0.8209277
## 10       0.9 0.5978191
## 11       1.0 0.0499554
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

![](NYTBlogs_zoo_files/figure-html/fit.data.training_0-3.png) 

```
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     feats
## 1 SubsectionName.nb.fctr, WordCount.log, PubDate.last100.log, PubDate.last10.log, SectionName.nb.fctr, NewsDesk.nb.fctr, H.num.chars.log, PubDate.last1.log, S.num.chars.log, A.num.chars.log, Headline.pfx.fctr, H.num.words.log, S.num.words.unq.log, H.num.words.unq.log, A.num.words.unq.log, A.num.words.log, S.num.words.log, H.is.question, PubDate.hour.fctr, PubDate.minute.fctr, PubDate.wkday.fctr, PubDate.date.fctr, PubDate.second.fctr, S.new, A.time, H.today, PubDate.wkend, A.new, S.time, A.make, S.one, A.one, A.state, A.can, S.state, S.make, A.will, S.can, S.report, S.week, A.year, S.said, A.said, S.year, A.week, H.has.ebola, S.will, A.report, S.compani, H.newyork, A.show, S.newyork, A.compani, S.show, S.take, S.share, A.take, A.share, A.newyork, A.day, S.day, A.first, S.presid, H.new, A.presid, S.first, H.day, S.intern, H.week, A.intern, S.articl, A.articl, H.report, H.fashion, H.X2014, A.fashion, S.fashion
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                    317.316               158.452
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.9109002
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.9994354                     1     0.6588067
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor      bgn      end elapsed
## 13 fit.data.training          7          0  717.216 1041.151 323.935
## 14 fit.data.training          7          1 1041.152       NA      NA
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
## SubsectionName.nb.fctr SubsectionName.nb.fctr 100.00000000 -0.042936460
## WordCount.log                   WordCount.log  94.30030184  0.264960434
## PubDate.last100.log       PubDate.last100.log  37.29768398 -0.007663322
## PubDate.last10.log         PubDate.last10.log  35.51625203  0.049317022
## SectionName.nb.fctr       SectionName.nb.fctr  33.06860059 -0.008268577
## NewsDesk.nb.fctr             NewsDesk.nb.fctr  31.25476107 -0.053443087
## H.num.chars.log               H.num.chars.log  28.70058185 -0.171062360
## PubDate.last1.log           PubDate.last1.log  28.11210210  0.046357515
## S.num.chars.log               S.num.chars.log  22.95909353 -0.224692967
## A.num.chars.log               A.num.chars.log  22.56904795 -0.224548821
## Headline.pfx.fctr           Headline.pfx.fctr  12.17152304 -0.100963501
## H.num.words.log               H.num.words.log  11.06009209 -0.200686356
## S.num.words.unq.log       S.num.words.unq.log  10.86918824 -0.250796919
## H.num.words.unq.log       H.num.words.unq.log  10.76992746 -0.204496360
## A.num.words.unq.log       A.num.words.unq.log  10.61417076 -0.250601203
## A.num.words.log               A.num.words.log  10.14672470 -0.245073324
## S.num.words.log               S.num.words.log  10.06685805 -0.245354135
## H.is.question                   H.is.question   8.97471155  0.129154799
## PubDate.hour.fctr           PubDate.hour.fctr   7.51625249  0.135436805
## PubDate.minute.fctr       PubDate.minute.fctr   4.56523569 -0.034073846
## PubDate.wkday.fctr         PubDate.wkday.fctr   4.14933362 -0.039801288
## PubDate.date.fctr           PubDate.date.fctr   3.61764162 -0.011647558
## PubDate.second.fctr       PubDate.second.fctr   3.57886654 -0.011879458
## S.new                                   S.new   2.65358458 -0.034948520
## A.time                                 A.time   2.58312526 -0.057790617
## H.today                               H.today   2.53264879 -0.063723058
## PubDate.wkend                   PubDate.wkend   2.47953439  0.106728760
## A.new                                   A.new   2.38406842 -0.035359447
## S.time                                 S.time   2.38356432 -0.057595102
## A.make                                 A.make   1.80720536  0.023138853
## S.one                                   S.one   1.80378543  0.006342094
## A.one                                   A.one   1.79335380  0.005696039
## A.state                               A.state   1.76140717  0.005702163
## A.can                                   A.can   1.72190392  0.031498867
## S.state                               S.state   1.66615691  0.006069626
## S.make                                 S.make   1.60598618  0.023138853
## A.will                                 A.will   1.58859780 -0.061025004
## S.can                                   S.can   1.57168576  0.029999780
## S.report                             S.report   1.57059871 -0.050211524
## S.week                                 S.week   1.53622041 -0.084814939
## A.year                                 A.year   1.47700205 -0.051146178
## S.said                                 S.said   1.44549130  0.001363226
## A.said                                 A.said   1.41194948  0.001363226
## S.year                                 S.year   1.39955820 -0.051146178
## A.week                                 A.week   1.39768019 -0.084814939
## H.has.ebola                       H.has.ebola   1.38192267  0.025881397
## S.will                                 S.will   1.36098406 -0.060575493
## A.report                             A.report   1.25711463 -0.050211524
## S.compani                           S.compani   1.24774975 -0.053012962
## H.newyork                           H.newyork   1.21747941 -0.057970095
## A.show                                 A.show   1.13826690 -0.048801740
## S.newyork                           S.newyork   1.09731978 -0.062117105
## A.compani                           A.compani   1.09386590 -0.053099633
## S.show                                 S.show   1.06000695 -0.048801740
## S.take                                 S.take   0.97321587 -0.025762398
## S.share                               S.share   0.97086880 -0.050329686
## A.take                                 A.take   0.93748272 -0.026086108
## A.share                               A.share   0.92875730 -0.050329686
## A.newyork                           A.newyork   0.92041517 -0.062117105
## A.day                                   A.day   0.72851403 -0.045909684
## S.day                                   S.day   0.71351166 -0.045649185
## A.first                               A.first   0.62529618 -0.053388178
## S.presid                             S.presid   0.58432843 -0.019828826
## H.new                                   H.new   0.57785111 -0.053121542
## A.presid                             A.presid   0.56752811 -0.019828826
## S.first                               S.first   0.54907531 -0.053388178
## H.day                                   H.day   0.27413575 -0.061669687
## S.intern                             S.intern   0.27180235 -0.068485701
## H.week                                 H.week   0.26550844 -0.075105216
## A.intern                             A.intern   0.23659607 -0.068485701
## S.articl                             S.articl   0.22009993 -0.059520554
## A.articl                             A.articl   0.20321053 -0.059520554
## H.report                             H.report   0.20308136 -0.064948102
## H.fashion                           H.fashion   0.17849990 -0.081708612
## H.X2014                               H.X2014   0.12226971 -0.046206380
## A.fashion                           A.fashion   0.09074034 -0.086446251
## S.fashion                           S.fashion   0.06758402 -0.086446251
## .rnorm                                 .rnorm           NA  0.017561723
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
## PubDate.last1                   PubDate.last1           NA  0.035922671
## PubDate.last10                 PubDate.last10           NA  0.053980930
## PubDate.last100               PubDate.last100           NA  0.039892288
## PubDate.month.fctr         PubDate.month.fctr           NA  0.019148739
## PubDate.POSIX                   PubDate.POSIX           NA  0.015683258
## PubDate.year.fctr           PubDate.year.fctr           NA           NA
## PubDate.zoo                       PubDate.zoo           NA  0.015683258
## S.has.http                         S.has.http           NA           NA
## S.num.chars                       S.num.chars           NA -0.179331806
## S.num.words                       S.num.words           NA -0.206385049
## S.num.words.unq               S.num.words.unq           NA -0.212102717
## UniqueID                             UniqueID           NA  0.011824920
## WordCount                           WordCount           NA  0.257526549
##                        exclude.as.feat   cor.y.abs             cor.high.X
## SubsectionName.nb.fctr           FALSE 0.042936460                   <NA>
## WordCount.log                    FALSE 0.264960434                   <NA>
## PubDate.last100.log              FALSE 0.007663322                   <NA>
## PubDate.last10.log               FALSE 0.049317022                   <NA>
## SectionName.nb.fctr              FALSE 0.008268577                   <NA>
## NewsDesk.nb.fctr                 FALSE 0.053443087 SubsectionName.nb.fctr
## H.num.chars.log                  FALSE 0.171062360                   <NA>
## PubDate.last1.log                FALSE 0.046357515                   <NA>
## S.num.chars.log                  FALSE 0.224692967        A.num.chars.log
## A.num.chars.log                  FALSE 0.224548821                   <NA>
## Headline.pfx.fctr                FALSE 0.100963501                   <NA>
## H.num.words.log                  FALSE 0.200686356                   <NA>
## S.num.words.unq.log              FALSE 0.250796919        S.num.chars.log
## H.num.words.unq.log              FALSE 0.204496360        H.num.chars.log
## A.num.words.unq.log              FALSE 0.250601203                   <NA>
## A.num.words.log                  FALSE 0.245073324                   <NA>
## S.num.words.log                  FALSE 0.245354135        A.num.words.log
## H.is.question                    FALSE 0.129154799                   <NA>
## PubDate.hour.fctr                FALSE 0.135436805                   <NA>
## PubDate.minute.fctr              FALSE 0.034073846                   <NA>
## PubDate.wkday.fctr               FALSE 0.039801288                   <NA>
## PubDate.date.fctr                FALSE 0.011647558                   <NA>
## PubDate.second.fctr              FALSE 0.011879458                   <NA>
## S.new                            FALSE 0.034948520                   <NA>
## A.time                           FALSE 0.057790617                 S.time
## H.today                          FALSE 0.063723058                   <NA>
## PubDate.wkend                    FALSE 0.106728760                   <NA>
## A.new                            FALSE 0.035359447                  S.new
## S.time                           FALSE 0.057595102                   <NA>
## A.make                           FALSE 0.023138853                 S.make
## S.one                            FALSE 0.006342094                   <NA>
## A.one                            FALSE 0.005696039                   <NA>
## A.state                          FALSE 0.005702163                   <NA>
## A.can                            FALSE 0.031498867                  S.can
## S.state                          FALSE 0.006069626                   <NA>
## S.make                           FALSE 0.023138853                   <NA>
## A.will                           FALSE 0.061025004                 S.will
## S.can                            FALSE 0.029999780                   <NA>
## S.report                         FALSE 0.050211524                   <NA>
## S.week                           FALSE 0.084814939                   <NA>
## A.year                           FALSE 0.051146178                 S.year
## S.said                           FALSE 0.001363226                   <NA>
## A.said                           FALSE 0.001363226                   <NA>
## S.year                           FALSE 0.051146178                   <NA>
## A.week                           FALSE 0.084814939                 S.week
## H.has.ebola                      FALSE 0.025881397                   <NA>
## S.will                           FALSE 0.060575493                   <NA>
## A.report                         FALSE 0.050211524               S.report
## S.compani                        FALSE 0.053012962                   <NA>
## H.newyork                        FALSE 0.057970095                   <NA>
## A.show                           FALSE 0.048801740                 S.show
## S.newyork                        FALSE 0.062117105                   <NA>
## A.compani                        FALSE 0.053099633              S.compani
## S.show                           FALSE 0.048801740                   <NA>
## S.take                           FALSE 0.025762398                   <NA>
## S.share                          FALSE 0.050329686                   <NA>
## A.take                           FALSE 0.026086108                 S.take
## A.share                          FALSE 0.050329686                S.share
## A.newyork                        FALSE 0.062117105              S.newyork
## A.day                            FALSE 0.045909684                  S.day
## S.day                            FALSE 0.045649185                   <NA>
## A.first                          FALSE 0.053388178                S.first
## S.presid                         FALSE 0.019828826                   <NA>
## H.new                            FALSE 0.053121542                   <NA>
## A.presid                         FALSE 0.019828826               S.presid
## S.first                          FALSE 0.053388178                   <NA>
## H.day                            FALSE 0.061669687                   <NA>
## S.intern                         FALSE 0.068485701                   <NA>
## H.week                           FALSE 0.075105216                   <NA>
## A.intern                         FALSE 0.068485701               S.intern
## S.articl                         FALSE 0.059520554                   <NA>
## A.articl                         FALSE 0.059520554               S.articl
## H.report                         FALSE 0.064948102                   <NA>
## H.fashion                        FALSE 0.081708612                 H.week
## H.X2014                          FALSE 0.046206380                   <NA>
## A.fashion                        FALSE 0.086446251              S.fashion
## S.fashion                        FALSE 0.086446251                   <NA>
## .rnorm                           FALSE 0.017561723                   <NA>
## A.has.http                       FALSE 0.013592603                   <NA>
## A.num.chars                       TRUE 0.177037425                   <NA>
## A.num.words                       TRUE 0.204211072                   <NA>
## A.num.words.unq                   TRUE 0.210242145                   <NA>
## H.daili                          FALSE 0.069192975                   <NA>
## H.has.http                       FALSE          NA                   <NA>
## H.num.chars                       TRUE 0.147211183                   <NA>
## H.num.words                       TRUE 0.186036895                   <NA>
## H.num.words.unq                   TRUE 0.189702157                   <NA>
## H.X2015                          FALSE 0.066584892                   <NA>
## Popular                           TRUE 1.000000000                   <NA>
## Popular.fctr                      TRUE          NA                   <NA>
## PubDate.last1                     TRUE 0.035922671                   <NA>
## PubDate.last10                    TRUE 0.053980930                   <NA>
## PubDate.last100                   TRUE 0.039892288                   <NA>
## PubDate.month.fctr                TRUE 0.019148739                   <NA>
## PubDate.POSIX                     TRUE 0.015683258                   <NA>
## PubDate.year.fctr                FALSE          NA                   <NA>
## PubDate.zoo                       TRUE 0.015683258                   <NA>
## S.has.http                       FALSE          NA                   <NA>
## S.num.chars                       TRUE 0.179331806                   <NA>
## S.num.words                       TRUE 0.206385049                   <NA>
## S.num.words.unq                   TRUE 0.212102717                   <NA>
## UniqueID                          TRUE 0.011824920                   <NA>
## WordCount                         TRUE 0.257526549                   <NA>
##                        is.ConditionalX.y is.cor.y.abs.low rsp_var_raw
## SubsectionName.nb.fctr              TRUE            FALSE       FALSE
## WordCount.log                       TRUE            FALSE       FALSE
## PubDate.last100.log                 TRUE             TRUE       FALSE
## PubDate.last10.log                  TRUE            FALSE       FALSE
## SectionName.nb.fctr                 TRUE             TRUE       FALSE
## NewsDesk.nb.fctr                    TRUE            FALSE       FALSE
## H.num.chars.log                     TRUE            FALSE       FALSE
## PubDate.last1.log                   TRUE            FALSE       FALSE
## S.num.chars.log                     TRUE            FALSE       FALSE
## A.num.chars.log                     TRUE            FALSE       FALSE
## Headline.pfx.fctr                   TRUE            FALSE       FALSE
## H.num.words.log                     TRUE            FALSE       FALSE
## S.num.words.unq.log                 TRUE            FALSE       FALSE
## H.num.words.unq.log                 TRUE            FALSE       FALSE
## A.num.words.unq.log                 TRUE            FALSE       FALSE
## A.num.words.log                     TRUE            FALSE       FALSE
## S.num.words.log                     TRUE            FALSE       FALSE
## H.is.question                       TRUE            FALSE       FALSE
## PubDate.hour.fctr                   TRUE            FALSE       FALSE
## PubDate.minute.fctr                 TRUE            FALSE       FALSE
## PubDate.wkday.fctr                  TRUE            FALSE       FALSE
## PubDate.date.fctr                   TRUE             TRUE       FALSE
## PubDate.second.fctr                 TRUE             TRUE       FALSE
## S.new                               TRUE            FALSE       FALSE
## A.time                              TRUE            FALSE       FALSE
## H.today                             TRUE            FALSE       FALSE
## PubDate.wkend                       TRUE            FALSE       FALSE
## A.new                               TRUE            FALSE       FALSE
## S.time                              TRUE            FALSE       FALSE
## A.make                              TRUE            FALSE       FALSE
## S.one                               TRUE             TRUE       FALSE
## A.one                               TRUE             TRUE       FALSE
## A.state                             TRUE             TRUE       FALSE
## A.can                               TRUE            FALSE       FALSE
## S.state                             TRUE             TRUE       FALSE
## S.make                              TRUE            FALSE       FALSE
## A.will                              TRUE            FALSE       FALSE
## S.can                               TRUE            FALSE       FALSE
## S.report                            TRUE            FALSE       FALSE
## S.week                              TRUE            FALSE       FALSE
## A.year                              TRUE            FALSE       FALSE
## S.said                              TRUE             TRUE       FALSE
## A.said                              TRUE             TRUE       FALSE
## S.year                              TRUE            FALSE       FALSE
## A.week                              TRUE            FALSE       FALSE
## H.has.ebola                         TRUE            FALSE       FALSE
## S.will                              TRUE            FALSE       FALSE
## A.report                            TRUE            FALSE       FALSE
## S.compani                           TRUE            FALSE       FALSE
## H.newyork                           TRUE            FALSE       FALSE
## A.show                              TRUE            FALSE       FALSE
## S.newyork                           TRUE            FALSE       FALSE
## A.compani                           TRUE            FALSE       FALSE
## S.show                              TRUE            FALSE       FALSE
## S.take                              TRUE            FALSE       FALSE
## S.share                             TRUE            FALSE       FALSE
## A.take                              TRUE            FALSE       FALSE
## A.share                             TRUE            FALSE       FALSE
## A.newyork                           TRUE            FALSE       FALSE
## A.day                               TRUE            FALSE       FALSE
## S.day                               TRUE            FALSE       FALSE
## A.first                             TRUE            FALSE       FALSE
## S.presid                            TRUE            FALSE       FALSE
## H.new                               TRUE            FALSE       FALSE
## A.presid                            TRUE            FALSE       FALSE
## S.first                             TRUE            FALSE       FALSE
## H.day                               TRUE            FALSE       FALSE
## S.intern                            TRUE            FALSE       FALSE
## H.week                              TRUE            FALSE       FALSE
## A.intern                            TRUE            FALSE       FALSE
## S.articl                            TRUE            FALSE       FALSE
## A.articl                            TRUE            FALSE       FALSE
## H.report                            TRUE            FALSE       FALSE
## H.fashion                           TRUE            FALSE       FALSE
## H.X2014                             TRUE            FALSE       FALSE
## A.fashion                           TRUE            FALSE       FALSE
## S.fashion                           TRUE            FALSE       FALSE
## .rnorm                              TRUE            FALSE       FALSE
## A.has.http                         FALSE             TRUE       FALSE
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
## PubDate.last1                         NA            FALSE       FALSE
## PubDate.last10                        NA            FALSE       FALSE
## PubDate.last100                       NA            FALSE       FALSE
## PubDate.month.fctr                    NA            FALSE       FALSE
## PubDate.POSIX                         NA             TRUE       FALSE
## PubDate.year.fctr                  FALSE               NA       FALSE
## PubDate.zoo                           NA             TRUE       FALSE
## S.has.http                         FALSE               NA       FALSE
## S.num.chars                           NA            FALSE       FALSE
## S.num.words                           NA            FALSE       FALSE
## S.num.words.unq                       NA            FALSE       FALSE
## UniqueID                              NA             TRUE       FALSE
## WordCount                             NA            FALSE       FALSE
##                        id_var rsp_var Conditional.X.no.rnorm.rf.importance
## SubsectionName.nb.fctr     NA      NA                         100.00000000
## WordCount.log              NA      NA                          94.30030184
## PubDate.last100.log        NA      NA                          37.29768398
## PubDate.last10.log         NA      NA                          35.51625203
## SectionName.nb.fctr        NA      NA                          33.06860059
## NewsDesk.nb.fctr           NA      NA                          31.25476107
## H.num.chars.log            NA      NA                          28.70058185
## PubDate.last1.log          NA      NA                          28.11210210
## S.num.chars.log            NA      NA                          22.95909353
## A.num.chars.log            NA      NA                          22.56904795
## Headline.pfx.fctr          NA      NA                          12.17152304
## H.num.words.log            NA      NA                          11.06009209
## S.num.words.unq.log        NA      NA                          10.86918824
## H.num.words.unq.log        NA      NA                          10.76992746
## A.num.words.unq.log        NA      NA                          10.61417076
## A.num.words.log            NA      NA                          10.14672470
## S.num.words.log            NA      NA                          10.06685805
## H.is.question              NA      NA                           8.97471155
## PubDate.hour.fctr          NA      NA                           7.51625249
## PubDate.minute.fctr        NA      NA                           4.56523569
## PubDate.wkday.fctr         NA      NA                           4.14933362
## PubDate.date.fctr          NA      NA                           3.61764162
## PubDate.second.fctr        NA      NA                           3.57886654
## S.new                      NA      NA                           2.65358458
## A.time                     NA      NA                           2.58312526
## H.today                    NA      NA                           2.53264879
## PubDate.wkend              NA      NA                           2.47953439
## A.new                      NA      NA                           2.38406842
## S.time                     NA      NA                           2.38356432
## A.make                     NA      NA                           1.80720536
## S.one                      NA      NA                           1.80378543
## A.one                      NA      NA                           1.79335380
## A.state                    NA      NA                           1.76140717
## A.can                      NA      NA                           1.72190392
## S.state                    NA      NA                           1.66615691
## S.make                     NA      NA                           1.60598618
## A.will                     NA      NA                           1.58859780
## S.can                      NA      NA                           1.57168576
## S.report                   NA      NA                           1.57059871
## S.week                     NA      NA                           1.53622041
## A.year                     NA      NA                           1.47700205
## S.said                     NA      NA                           1.44549130
## A.said                     NA      NA                           1.41194948
## S.year                     NA      NA                           1.39955820
## A.week                     NA      NA                           1.39768019
## H.has.ebola                NA      NA                           1.38192267
## S.will                     NA      NA                           1.36098406
## A.report                   NA      NA                           1.25711463
## S.compani                  NA      NA                           1.24774975
## H.newyork                  NA      NA                           1.21747941
## A.show                     NA      NA                           1.13826690
## S.newyork                  NA      NA                           1.09731978
## A.compani                  NA      NA                           1.09386590
## S.show                     NA      NA                           1.06000695
## S.take                     NA      NA                           0.97321587
## S.share                    NA      NA                           0.97086880
## A.take                     NA      NA                           0.93748272
## A.share                    NA      NA                           0.92875730
## A.newyork                  NA      NA                           0.92041517
## A.day                      NA      NA                           0.72851403
## S.day                      NA      NA                           0.71351166
## A.first                    NA      NA                           0.62529618
## S.presid                   NA      NA                           0.58432843
## H.new                      NA      NA                           0.57785111
## A.presid                   NA      NA                           0.56752811
## S.first                    NA      NA                           0.54907531
## H.day                      NA      NA                           0.27413575
## S.intern                   NA      NA                           0.27180235
## H.week                     NA      NA                           0.26550844
## A.intern                   NA      NA                           0.23659607
## S.articl                   NA      NA                           0.22009993
## A.articl                   NA      NA                           0.20321053
## H.report                   NA      NA                           0.20308136
## H.fashion                  NA      NA                           0.17849990
## H.X2014                    NA      NA                           0.12226971
## A.fashion                  NA      NA                           0.09074034
## S.fashion                  NA      NA                           0.06758402
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
## PubDate.last1              NA      NA                                   NA
## PubDate.last10             NA      NA                                   NA
## PubDate.last100            NA      NA                                   NA
## PubDate.month.fctr         NA      NA                                   NA
## PubDate.POSIX              NA      NA                                   NA
## PubDate.year.fctr          NA      NA                                   NA
## PubDate.zoo                NA      NA                                   NA
## S.has.http                 NA      NA                                   NA
## S.num.chars                NA      NA                                   NA
## S.num.words                NA      NA                                   NA
## S.num.words.unq            NA      NA                                   NA
## UniqueID                 TRUE      NA                                   NA
## WordCount                  NA      NA                                   NA
##                        Final.rf.importance
## SubsectionName.nb.fctr        100.00000000
## WordCount.log                  94.30030184
## PubDate.last100.log            37.29768398
## PubDate.last10.log             35.51625203
## SectionName.nb.fctr            33.06860059
## NewsDesk.nb.fctr               31.25476107
## H.num.chars.log                28.70058185
## PubDate.last1.log              28.11210210
## S.num.chars.log                22.95909353
## A.num.chars.log                22.56904795
## Headline.pfx.fctr              12.17152304
## H.num.words.log                11.06009209
## S.num.words.unq.log            10.86918824
## H.num.words.unq.log            10.76992746
## A.num.words.unq.log            10.61417076
## A.num.words.log                10.14672470
## S.num.words.log                10.06685805
## H.is.question                   8.97471155
## PubDate.hour.fctr               7.51625249
## PubDate.minute.fctr             4.56523569
## PubDate.wkday.fctr              4.14933362
## PubDate.date.fctr               3.61764162
## PubDate.second.fctr             3.57886654
## S.new                           2.65358458
## A.time                          2.58312526
## H.today                         2.53264879
## PubDate.wkend                   2.47953439
## A.new                           2.38406842
## S.time                          2.38356432
## A.make                          1.80720536
## S.one                           1.80378543
## A.one                           1.79335380
## A.state                         1.76140717
## A.can                           1.72190392
## S.state                         1.66615691
## S.make                          1.60598618
## A.will                          1.58859780
## S.can                           1.57168576
## S.report                        1.57059871
## S.week                          1.53622041
## A.year                          1.47700205
## S.said                          1.44549130
## A.said                          1.41194948
## S.year                          1.39955820
## A.week                          1.39768019
## H.has.ebola                     1.38192267
## S.will                          1.36098406
## A.report                        1.25711463
## S.compani                       1.24774975
## H.newyork                       1.21747941
## A.show                          1.13826690
## S.newyork                       1.09731978
## A.compani                       1.09386590
## S.show                          1.06000695
## S.take                          0.97321587
## S.share                         0.97086880
## A.take                          0.93748272
## A.share                         0.92875730
## A.newyork                       0.92041517
## A.day                           0.72851403
## S.day                           0.71351166
## A.first                         0.62529618
## S.presid                        0.58432843
## H.new                           0.57785111
## A.presid                        0.56752811
## S.first                         0.54907531
## H.day                           0.27413575
## S.intern                        0.27180235
## H.week                          0.26550844
## A.intern                        0.23659607
## S.articl                        0.22009993
## A.articl                        0.20321053
## H.report                        0.20308136
## H.fashion                       0.17849990
## H.X2014                         0.12226971
## A.fashion                       0.09074034
## S.fashion                       0.06758402
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
## PubDate.last1                           NA
## PubDate.last10                          NA
## PubDate.last100                         NA
## PubDate.month.fctr                      NA
## PubDate.POSIX                           NA
## PubDate.year.fctr                       NA
## PubDate.zoo                             NA
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 77
```

![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-1.png) ![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-2.png) ![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-3.png) ![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-4.png) ![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## 1507     1507            N                              0.000
## 6370     6370            Y                              0.756
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
##      Popular.fctr Popular.fctr.predict.Final.rf.prob
## 92              Y                              0.646
## 693             Y                              0.640
## 4020            Y                              0.700
## 4721            Y                              0.626
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

![](NYTBlogs_zoo_files/figure-html/fit.data.training_1-7.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor      bgn      end elapsed
## 14 fit.data.training          7          1 1041.152 1139.099  97.948
## 15  predict.data.new          8          0 1139.100       NA      NA
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 77
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
##      UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## 6753     6753         <NA>                              0.408
## 7309     7309         <NA>                              0.024
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
## NA.642        NA         <NA>                                 NA
## NA.693        NA         <NA>                                 NA
## NA.1529       NA         <NA>                                 NA
## NA.1536       NA         <NA>                                 NA
## NA.1543       NA         <NA>                                 NA
## NA.1800       NA         <NA>                                 NA
##         Popular.fctr.predict.Final.rf
## NA.642                           <NA>
## NA.693                           <NA>
## NA.1529                          <NA>
## NA.1536                          <NA>
## NA.1543                          <NA>
## NA.1800                          <NA>
##         Popular.fctr.predict.Final.rf.accurate
## NA.642                                      NA
## NA.693                                      NA
## NA.1529                                     NA
## NA.1536                                     NA
## NA.1543                                     NA
## NA.1800                                     NA
##         Popular.fctr.predict.Final.rf.error
## NA.642                                   NA
## NA.693                                   NA
## NA.1529                                  NA
## NA.1536                                  NA
## NA.1543                                  NA
## NA.1800                                  NA
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
## [1] 4475  115
```

```r
print(dsp_models_df)
```

```
##                        model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 7       Interact.High.cor.Y.glm        0.9003403   0.9077031     0.6261297
## 11    Conditional.X.no.rnorm.rf        0.8983957   0.9258645     0.6339412
## 8                 Low.cor.X.glm        0.8979096   0.7981501     0.6193211
## 9             Conditional.X.glm        0.8857560   0.7455504     0.5449025
## 10 Conditional.X.no.rnorm.rpart        0.8779776   0.7039211     0.4818624
## 1             MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3          Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 5               Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 4     Max.cor.Y.cv.0.cp.0.rpart        0.7535246   0.6722244     0.2272928
## 6                 Max.cor.Y.glm        0.7015070   0.7289876     0.2355742
## 2       Random.myrandom_classfr        0.1672338   0.4909227     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 7     2278.103                    0.4
## 11          NA                    0.4
## 8    30520.669                    0.9
## 9    36231.566                    0.9
## 10          NA                    0.7
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 4           NA                    0.2
## 6     3664.292                    0.2
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
##         N 1610  103
##         Y  106  238
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
## 1               Business    510    500    0.267379679   0.2479338843
## 14                  OpEd    199    205    0.109625668   0.0967428294
## 10              myMisc::    280    199    0.106417112   0.1361205639
## 17               Science     66     57    0.030481283   0.0320855615
## 18                Styles     81     77    0.041176471   0.0393777346
## 2                Culture    225    243    0.129946524   0.1093825960
## 7                  Metro     60     66    0.035294118   0.0291686923
## 22                TStyle    235    107    0.057219251   0.1142440447
## 15     Readers Respond::      3      4    0.002139037   0.0014584346
## 8            myEducation    100     93    0.049732620   0.0486144871
## 13          myPolitics::     31     41    0.021925134   0.0150704910
## 16 Reporter's Notebook::      4      7    0.003743316   0.0019445795
## 9          myEducation::      1      3    0.001604278   0.0004861449
## 3    Daily Clip Report::     17     22    0.011764706   0.0082644628
## 4          First Draft::     16     14    0.007486631   0.0077783179
## 5                Foreign    111    107    0.057219251   0.0539620807
## 6               Magazine     13      3    0.001604278   0.0063198833
## 11          myMultimedia     42     53    0.028342246   0.0204180846
## 12        myMultimedia::      3      3    0.001604278   0.0014584346
## 20   Today in Politics::     16     21    0.011229947   0.0077783179
## 21                Travel     31     31    0.016577540   0.0150704910
## 23            Verbatim::     13     12    0.006417112   0.0063198833
## 19      The Daily Gift::     NA      2    0.001069519             NA
##    accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 1                  64               446        0.8745098
## 14                 32               167        0.8391960
## 10                 29               251        0.8964286
## 17                 29                37        0.5606061
## 18                 23                58        0.7160494
## 2                  12               213        0.9466667
## 7                   5                55        0.9166667
## 22                  5               230        0.9787234
## 15                  3                 0        0.0000000
## 8                   2                98        0.9800000
## 13                  2                29        0.9354839
## 16                  2                 2        0.5000000
## 9                   1                 0        0.0000000
## 3                   0                17        1.0000000
## 4                   0                16        1.0000000
## 5                   0               111        1.0000000
## 6                   0                13        1.0000000
## 11                  0                42        1.0000000
## 12                  0                 3        1.0000000
## 20                  0                16        1.0000000
## 21                  0                31        1.0000000
## 23                  0                13        1.0000000
## 19                 NA                NA               NA
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
## [1] "Conditional.X.no.rnorm.rf OOB::NewsDesk.nb=myMultimedia confusion matrix & accuracy: "
##          Prediction
## Reference  N  Y
##         N 42  0
##         Y  0  0
## [1] 1
## [1] "Conditional.X.no.rnorm.rf OOB::NewsDesk.nb=myMultimedia errors: "
## [1] Headline.pfx Headline     Popular     
## <0 rows> (or 0-length row.names)
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
## 95         95       1
## 56         56       1
## 163       163       1
## 130       130       1
## 291       291       1
## 364       364       1
## 317       317       1
## 436       436       1
## 493       493       1
## 629       629       1
## 685       685       1
## 809       809       1
## 779       779       1
## 882       882       1
## 983       983       1
## 1132     1132       1
## 1092     1092       1
## 1193     1193       1
## 1156     1156       1
## 1312     1312       1
## 1404     1404       1
## 1596     1596       1
## 1560     1560       1
## 1526     1526       1
## 1489     1489       1
## 1702     1702       1
## 1807     1807       1
## 1767     1767       1
## 1871     1871       1
## 2007     2007       1
## 2088     2088       1
## 2123     2123       1
## 2228     2228       1
## 2196     2196       1
## 2167     2167       1
## 2165     2165       1
## 2330     2330       1
## 2319     2319       1
## 2267     2267       1
## 2438     2438       1
## 2542     2542       1
## 2512     2512       1
## 2603     2603       1
## 2756     2756       1
## 2725     2725       1
## 2883     2883       1
## 2873     2873       1
## 2852     2852       1
## 3074     3074       1
## 3044     3044       1
## 3287     3287       1
## 3263     3263       1
## 3235     3235       1
## 3368     3368       1
## 3472     3472       1
## 3414     3414       1
## 3574     3574       1
## 3521     3521       1
## 3492     3492       1
## 3635     3635       1
## 3809     3809       1
## 3908     3908       1
## 3901     3901       1
## 3851     3851       1
## 4206     4206       1
## 4173     4173       1
## 4281     4281       1
## 4265     4265       1
## 4415     4415       1
## 4398     4398       1
## 4359     4359       1
## 4465     4465       1
## 4540     4540       1
## 4712     4712       1
## 4833     4833       1
## 5058     5058       1
## 5038     5038       1
## 5285     5285       1
## 5276     5276       1
## 5387     5387       1
## 5345     5345       1
## 5649     5649       1
## 5884     5884       1
## 5881     5881       1
## 5923     5923       1
## 6057     6057       1
## 6159     6159       1
## 6221     6221       1
## 6286     6286       1
## 6283     6283       1
## 6252     6252       1
## 6251     6251       1
## 6370     6370       1
## 6357     6357       1
## 6327     6327       1
## 6395     6395       1
## 6484     6484       1
##                                                                                                Headline
## 95                                                                             Phrases We Love Too Much
## 56                                                             How Can Men Help Prevent Sexual Assault?
## 163                                           Why Leaked Nude Photos Are Another Frontier for Feminists
## 130                                    How a Revelation About Hello Kittys Identity Blew Everyones Mind
## 291                                                      A Debate Fueled by Carbs (or the Lack Thereof)
## 364                                                                      Joan Rivers and the Front Page
## 317                                                                    What Does the Middle Class Need?
## 436                                                                 Should an American Man Wear Shorts?
## 493                                                                   When Family Dinner Doesnt Satisfy
## 629                                                                   When Spell-Check Can&rsquo;t Help
## 685                                                                               What Janay Rice Wants
## 809                                                                   What Does It Mean to Be Scottish?
## 779                                                                 Are Biblical Epics Epically Racist?
## 882                                                                        How Women Talk About Clothes
## 983                                                                How Keeping a Diary Can Surprise You
## 1132                                                                                       Wasted Words
## 1092                                                                     How to Fake Your Next Vacation
## 1193                                                                        What Blade Runner Got Wrong
## 1156                                                       Tracking a Microtrend Among Affluent Parents
## 1312                                                                                Boycott the N.F.L.?
## 1404                                                                           When Tips Are Not Enough
## 1596                                               Should We Continue to Prosecute Nazi War Criminals? 
## 1560                                                                   What Is the Right Way to Travel?
## 1526                                                        Why Are We So Obsessed With Gilmore Girls? 
## 1489                                            What to Expect From Narendra Modi at the United Nations
## 1702                                                                                        Do the Math
## 1807                                                           How Fear of Death Could Make You Splurge
## 1767                                                     Why Asexuals Dont Want to Be Invisible Anymore
## 1871                                    A Foreign Policy Turning Point or a Moral-Equivalence Blunder? 
## 2007                                                                 Can a Social Network Stay Ad-Free?
## 2088                                  Narendra Modi, in U.N. Speech, Inserts India Into Terrorism Fight
## 2123                                                                        Dont Do the Things You Love
## 2228                                    Why Are Republicans in Favor of Over-the-Counter Birth Control?
## 2196                                                                                      Beware of Joy
## 2167                                        Netanyahu Links Hamas With ISIS, and Equates ISIS With Iran
## 2165                                                                              California Is Burning
## 2330                                                                              Fighting Human Nature
## 2319                                                                                Close but Not Quite
## 2267                                                  If You Have Unlimited Vacation, Will You Take It?
## 2438                                                                             The Cost of Being Cool
## 2542                                                               Do You Have Time to Read This Story?
## 2512                                                Steven Salaita and the Quagmire of Academic Freedom
## 2603                                    Shamed, Flamed, Harassed: What Its Like To Be Called Fat Online
## 2756                                                                      Is Catalonia Spains Scotland?
## 2725                                                       Do We Get Less Narcissistic as We Get Older?
## 2883                                                                    In the Face of Ebola, Stay Calm
## 2873                                                                                 Ugly Disagreements
## 2852                                                                   When Education Brings Depression
## 3074                                                        Inside the Bounds of a Hasidic Neighborhood
## 3044                                  Joe Bidens Latest Gaffe: the Truth or an Ultimate Embarrassment? 
## 3287                                                     Women Fight ISIS and Sexism in Kurdish Regions
## 3263                                                              Discussion of Pedophilia Turns Heated
## 3235                                                           Columbus Day, or Indigenous Peoples Day?
## 3368                                                                                   The Slang Patrol
## 3472                                                              Yes Means Yes: The Big Consent Debate
## 3414                              Me and David Greenglass, a Man Whose Name Is Synonymous With Betrayal
## 3574                                                                What Is a Nobel Prize Really Worth?
## 3521                                                              Why Indias Muslims Havent Radicalized
## 3492                                                  Life and Death Through the Eyes of an Ebola Nurse
## 3635                                                               Who Was Right About W.M.D.s in Iraq?
## 3809                                                                    How Brain Myths Could Hurt Kids
## 3908                                                             Has Ebola Exposed a Strain of Racism? 
## 3901                                                                                   Tangled Passages
## 3851                                               On Its 20th Anniversary, Does Pulp Fiction Hold Up? 
## 4206                                       Is a Sculpture That Resembles a Sex Toy a National Scandal? 
## 4173                                                    Should a Child Offender Be Treated as an Adult?
## 4281                                                        What Jian Ghomeshis Accusers Were Afraid Of
## 4265                                                               Steering the Climate Change Coverage
## 4415                                                                           Phrases We Love Too Much
## 4398                              Can You Expect Comity or Conflict in a Republican-Controlled Senate? 
## 4359                                                              Taylor Swifts Unwelcome P.R. Campaign
## 4465                                  If David Byrne Does Not Care About Contemporary Art, Should We?  
## 4540                            1983 | Having Claimed 558 Lives, AIDS Finally Made It to the Front Page
## 4712                                                    Is It Voter Fraud or Voter Suppression in 2014?
## 4833                                                   Why Do We Still Care About the Confederate Flag?
## 5058                             Sexual Harassment at Yale: Delicate Subject, High-Impact Investigation
## 5038                                                               Why Migraines Deserve More Attention
## 5285 Please. Don't 'Decry' the 'Divorc&eacute;e.' Or Give Us Your 'CV.' The Times Guide to Modern Usage
## 5276                                                          The Benefits of Being Politically Correct
## 5387                                                                                    Bright Passages
## 5345                                                                   Should Your Child Play Football?
## 5649                                                    How to Be French (When Asking for a Cigarette) 
## 5884                                                                             Brooklyn, Planet Earth
## 5881                                                                 Should You Pack Your Childs Lunch?
## 5923                                     How OkCupid Has Become More Inclusive on Gender and Sexuality 
## 6057                                                                          When Are You Not Working?
## 6159                   Does the University of Virginia Have a Culture of Silence Around Sexual Assault?
## 6221                                                           Brown Family's Lawyer Criticizes Process
## 6286                                                                  A Quiet Wedding for Darren Wilson
## 6283                                                                    Can Brain Science Be Dangerous?
## 6252                                                         Was Chuck Hagel a Failure or a Scapegoat? 
## 6251                 Ferguson and Other Cities React to Grand Jury Decision Not to Indict Darren Wilson
## 6370                          Latest Updates: Protests Nationwide as More Troops Are Called to Ferguson
## 6357                                           What Big Thing Would Reinvigorate the Democratic Party? 
## 6327                                                  What Ferguson Says About the Fear of Social Media
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
## [1] Popular.fctr                                           
## [2] Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob    
## [3] Popular.fctr.predict.Conditional.X.no.rnorm.rf         
## [4] Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
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
dsp_hdlpfx_results(hdlpfx="Ask Well::")
```

```
## [1] "Ask Well::"
##      Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob
## 393             Y                                               0.498
## 1053            Y                                               0.514
## 2507            N                                               0.474
## 4134            N                                               0.448
## 4387            Y                                               0.230
## 5244            N                                               0.640
## 5658            Y                                               0.626
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf
## 393                                               Y
## 1053                                              Y
## 2507                                              Y
## 4134                                              Y
## 4387                                              N
## 5244                                              Y
## 5658                                              Y
##      Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## 393                                                     TRUE
## 1053                                                    TRUE
## 2507                                                   FALSE
## 4134                                                   FALSE
## 4387                                                   FALSE
## 5244                                                   FALSE
## 5658                                                    TRUE
##      Popular.fctr Popular.fctr.predict.Final.rf.prob
## 6558         <NA>                              0.638
## 7535         <NA>                              0.678
## 7864         <NA>                              0.580
##      Popular.fctr.predict.Final.rf
## 6558                             Y
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
## [1] Popular.fctr                                           
## [2] Popular.fctr.predict.Conditional.X.no.rnorm.rf.prob    
## [3] Popular.fctr.predict.Conditional.X.no.rnorm.rf         
## [4] Popular.fctr.predict.Conditional.X.no.rnorm.rf.accurate
## <0 rows> (or 0-length row.names)
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
## SubsectionName.nb.fctr              TRUE 100.00000000
## WordCount.log                       TRUE  94.30030184
## PubDate.last100.log                 TRUE  37.29768398
## PubDate.last10.log                  TRUE  35.51625203
## SectionName.nb.fctr                 TRUE  33.06860059
## NewsDesk.nb.fctr                    TRUE  31.25476107
## H.num.chars.log                     TRUE  28.70058185
## PubDate.last1.log                   TRUE  28.11210210
## S.num.chars.log                     TRUE  22.95909353
## A.num.chars.log                     TRUE  22.56904795
## Headline.pfx.fctr                   TRUE  12.17152304
## H.num.words.log                     TRUE  11.06009209
## S.num.words.unq.log                 TRUE  10.86918824
## H.num.words.unq.log                 TRUE  10.76992746
## A.num.words.unq.log                 TRUE  10.61417076
## A.num.words.log                     TRUE  10.14672470
## S.num.words.log                     TRUE  10.06685805
## H.is.question                       TRUE   8.97471155
## PubDate.hour.fctr                   TRUE   7.51625249
## PubDate.minute.fctr                 TRUE   4.56523569
## PubDate.wkday.fctr                  TRUE   4.14933362
## PubDate.date.fctr                   TRUE   3.61764162
## PubDate.second.fctr                 TRUE   3.57886654
## S.new                               TRUE   2.65358458
## A.time                              TRUE   2.58312526
## H.today                             TRUE   2.53264879
## PubDate.wkend                       TRUE   2.47953439
## A.new                               TRUE   2.38406842
## S.time                              TRUE   2.38356432
## A.make                              TRUE   1.80720536
## S.one                               TRUE   1.80378543
## A.one                               TRUE   1.79335380
## A.state                             TRUE   1.76140717
## A.can                               TRUE   1.72190392
## S.state                             TRUE   1.66615691
## S.make                              TRUE   1.60598618
## A.will                              TRUE   1.58859780
## S.can                               TRUE   1.57168576
## S.report                            TRUE   1.57059871
## S.week                              TRUE   1.53622041
## A.year                              TRUE   1.47700205
## S.said                              TRUE   1.44549130
## A.said                              TRUE   1.41194948
## S.year                              TRUE   1.39955820
## A.week                              TRUE   1.39768019
## H.has.ebola                         TRUE   1.38192267
## S.will                              TRUE   1.36098406
## A.report                            TRUE   1.25711463
## S.compani                           TRUE   1.24774975
## H.newyork                           TRUE   1.21747941
## A.show                              TRUE   1.13826690
## S.newyork                           TRUE   1.09731978
## A.compani                           TRUE   1.09386590
## S.show                              TRUE   1.06000695
## S.take                              TRUE   0.97321587
## S.share                             TRUE   0.97086880
## A.take                              TRUE   0.93748272
## A.share                             TRUE   0.92875730
## A.newyork                           TRUE   0.92041517
## A.day                               TRUE   0.72851403
## S.day                               TRUE   0.71351166
## A.first                             TRUE   0.62529618
## S.presid                            TRUE   0.58432843
## H.new                               TRUE   0.57785111
## A.presid                            TRUE   0.56752811
## S.first                             TRUE   0.54907531
## H.day                               TRUE   0.27413575
## S.intern                            TRUE   0.27180235
## H.week                              TRUE   0.26550844
## A.intern                            TRUE   0.23659607
## S.articl                            TRUE   0.22009993
## A.articl                            TRUE   0.20321053
## H.report                            TRUE   0.20308136
## H.fashion                           TRUE   0.17849990
## H.X2014                             TRUE   0.12226971
## A.fashion                           TRUE   0.09074034
## S.fashion                           TRUE   0.06758402
##                        Conditional.X.no.rnorm.rf.importance
## SubsectionName.nb.fctr                         100.00000000
## WordCount.log                                   94.30030184
## PubDate.last100.log                             37.29768398
## PubDate.last10.log                              35.51625203
## SectionName.nb.fctr                             33.06860059
## NewsDesk.nb.fctr                                31.25476107
## H.num.chars.log                                 28.70058185
## PubDate.last1.log                               28.11210210
## S.num.chars.log                                 22.95909353
## A.num.chars.log                                 22.56904795
## Headline.pfx.fctr                               12.17152304
## H.num.words.log                                 11.06009209
## S.num.words.unq.log                             10.86918824
## H.num.words.unq.log                             10.76992746
## A.num.words.unq.log                             10.61417076
## A.num.words.log                                 10.14672470
## S.num.words.log                                 10.06685805
## H.is.question                                    8.97471155
## PubDate.hour.fctr                                7.51625249
## PubDate.minute.fctr                              4.56523569
## PubDate.wkday.fctr                               4.14933362
## PubDate.date.fctr                                3.61764162
## PubDate.second.fctr                              3.57886654
## S.new                                            2.65358458
## A.time                                           2.58312526
## H.today                                          2.53264879
## PubDate.wkend                                    2.47953439
## A.new                                            2.38406842
## S.time                                           2.38356432
## A.make                                           1.80720536
## S.one                                            1.80378543
## A.one                                            1.79335380
## A.state                                          1.76140717
## A.can                                            1.72190392
## S.state                                          1.66615691
## S.make                                           1.60598618
## A.will                                           1.58859780
## S.can                                            1.57168576
## S.report                                         1.57059871
## S.week                                           1.53622041
## A.year                                           1.47700205
## S.said                                           1.44549130
## A.said                                           1.41194948
## S.year                                           1.39955820
## A.week                                           1.39768019
## H.has.ebola                                      1.38192267
## S.will                                           1.36098406
## A.report                                         1.25711463
## S.compani                                        1.24774975
## H.newyork                                        1.21747941
## A.show                                           1.13826690
## S.newyork                                        1.09731978
## A.compani                                        1.09386590
## S.show                                           1.06000695
## S.take                                           0.97321587
## S.share                                          0.97086880
## A.take                                           0.93748272
## A.share                                          0.92875730
## A.newyork                                        0.92041517
## A.day                                            0.72851403
## S.day                                            0.71351166
## A.first                                          0.62529618
## S.presid                                         0.58432843
## H.new                                            0.57785111
## A.presid                                         0.56752811
## S.first                                          0.54907531
## H.day                                            0.27413575
## S.intern                                         0.27180235
## H.week                                           0.26550844
## A.intern                                         0.23659607
## S.articl                                         0.22009993
## A.articl                                         0.20321053
## H.report                                         0.20308136
## H.fashion                                        0.17849990
## H.X2014                                          0.12226971
## A.fashion                                        0.09074034
## S.fashion                                        0.06758402
##                        Final.rf.importance
## SubsectionName.nb.fctr        100.00000000
## WordCount.log                  94.30030184
## PubDate.last100.log            37.29768398
## PubDate.last10.log             35.51625203
## SectionName.nb.fctr            33.06860059
## NewsDesk.nb.fctr               31.25476107
## H.num.chars.log                28.70058185
## PubDate.last1.log              28.11210210
## S.num.chars.log                22.95909353
## A.num.chars.log                22.56904795
## Headline.pfx.fctr              12.17152304
## H.num.words.log                11.06009209
## S.num.words.unq.log            10.86918824
## H.num.words.unq.log            10.76992746
## A.num.words.unq.log            10.61417076
## A.num.words.log                10.14672470
## S.num.words.log                10.06685805
## H.is.question                   8.97471155
## PubDate.hour.fctr               7.51625249
## PubDate.minute.fctr             4.56523569
## PubDate.wkday.fctr              4.14933362
## PubDate.date.fctr               3.61764162
## PubDate.second.fctr             3.57886654
## S.new                           2.65358458
## A.time                          2.58312526
## H.today                         2.53264879
## PubDate.wkend                   2.47953439
## A.new                           2.38406842
## S.time                          2.38356432
## A.make                          1.80720536
## S.one                           1.80378543
## A.one                           1.79335380
## A.state                         1.76140717
## A.can                           1.72190392
## S.state                         1.66615691
## S.make                          1.60598618
## A.will                          1.58859780
## S.can                           1.57168576
## S.report                        1.57059871
## S.week                          1.53622041
## A.year                          1.47700205
## S.said                          1.44549130
## A.said                          1.41194948
## S.year                          1.39955820
## A.week                          1.39768019
## H.has.ebola                     1.38192267
## S.will                          1.36098406
## A.report                        1.25711463
## S.compani                       1.24774975
## H.newyork                       1.21747941
## A.show                          1.13826690
## S.newyork                       1.09731978
## A.compani                       1.09386590
## S.show                          1.06000695
## S.take                          0.97321587
## S.share                         0.97086880
## A.take                          0.93748272
## A.share                         0.92875730
## A.newyork                       0.92041517
## A.day                           0.72851403
## S.day                           0.71351166
## A.first                         0.62529618
## S.presid                        0.58432843
## H.new                           0.57785111
## A.presid                        0.56752811
## S.first                         0.54907531
## H.day                           0.27413575
## S.intern                        0.27180235
## H.week                          0.26550844
## A.intern                        0.23659607
## S.articl                        0.22009993
## A.articl                        0.20321053
## H.report                        0.20308136
## H.fashion                       0.17849990
## H.X2014                         0.12226971
## A.fashion                       0.09074034
## S.fashion                       0.06758402
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
## 15     predict.data.new          8          0 1139.100 1232.913  93.813
## 16 display.session.info          9          0 1232.914       NA      NA
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
## 10              fit.models          6          1  218.326  626.795 408.469
## 13       fit.data.training          7          0  717.216 1041.151 323.935
## 14       fit.data.training          7          1 1041.152 1139.099  97.948
## 15        predict.data.new          8          0 1139.100 1232.913  93.813
## 6         extract.features          3          0   65.032  151.916  86.884
## 12              fit.models          6          3  643.633  717.215  73.582
## 9               fit.models          6          0  163.056  218.326  55.270
## 3             cleanse.data          2          1   28.416   58.677  30.261
## 2             inspect.data          2          0    9.964   28.416  18.452
## 11              fit.models          6          2  626.796  643.632  16.836
## 7          select.features          4          0  151.916  161.527   9.611
## 4      manage.missing.data          2          2   58.677   64.974   6.297
## 8  partition.data.training          5          0  161.527  163.055   1.529
## 1              import.data          1          0    9.006    9.964   0.958
## 5              encode.data          2          3   64.975   65.032   0.057
##    duration
## 10  408.469
## 13  323.935
## 14   97.947
## 15   93.813
## 6    86.884
## 12   73.582
## 9    55.270
## 3    30.261
## 2    18.452
## 11   16.836
## 7     9.611
## 4     6.297
## 8     1.528
## 1     0.958
## 5     0.057
```

```
## [1] "Total Elapsed Time: 1,232.913 secs"
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
