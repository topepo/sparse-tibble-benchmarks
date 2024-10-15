# sparse-tibble-benchmarks

With the [sparsevctrs](https://github.com/r-lib/sparsevctrs) package, tidymodels can now store rectangular data with different column types in an efficient format. Some model engines can consume sparse _matrices_ and sparsevctrs can quickly convert data from a “sparse tibble” (a tibble with sparse vectors) and a bone-fide sparse matrix. As of this writing, the implementation across tidymodels package has not been mightly optimized. 

This repo houses benchmarks for models that accept sparse matrices. These will help us understand and explain where/if there are benefits to using sparse data encodings. 

Currently, there are results for xgboost and glmnet classification models. 

The data sets: 

 - `caco`: a computational chemistry data set where a three-class outcome is predicted using different molecular descriptors. The [QSARdata](https://github.com/cran/QSARdata) package is the source and it contains several classes of predictors. Our analysis includes predictors most likely to benefit from sparsity: [atom pairs](https://scholar.google.com/scholar?hl=en&as_sdt=0%2C7&q=A+novel+descriptor+based+on+atom-pair+properties&btnG= ) and [binary substructure fingerprints](https://medium.com/@santuchal/molecular-fingerprint-1693111d7b96). There are 3,796 rows and 6,397 predictors in the data set. 
 

