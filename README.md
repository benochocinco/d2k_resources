# D2K Course Modules

In this repository is a collection of course resources, example work, and guides to help you through some of the most common data science topics you will encounter in D2K courses. Currently all code examples are in R.

# Table of Contents 

## Client Reports

* *client_report_template.tex* - LaTeX client report template for consulting.

## Data Cleaning and Wrangling

* *dcw_example.Rmd* - examples of cleaning data and feature engineering.
* *cleaning_and_wrangling.R* - puts all the script code from example to a function that can be used on new raw data.
* *load_dependencies.R* - example of how to check if a local machine has a package installed, and installing if it does not.

## Data Visualization

* *data_viz_concepts.pdf* - understanding the grammar of graphics, and what mistakes to avoid when creating your own graphics.
* *one_dim_vis.pdf* - basic standard graphics for visualizing univariate data.
* *two_dim_vis.pdf* - basic standard graphics for visualizing bivariate (or multivariate) data.

## EDA

### Clustering

* *basic_clustering.pdf* - k-means, hierarchical clustering.
* *convex_clustering.pdf* - a more modern method for finding clusters.

### Dimension Reduction

* *mds.pdf* - multidimensional scaling. Focuses on minimizing total distance between points.
* *nmf.pdf* - non-negative matrix factorization. Incorporates a non-negativity constraint.
* *pca.pdf* - principal components analysis. The most basic dimension reduction method.
* *tsne.pdf* - stochastic neighbor embedding. Focuses on preserving similarity between distribution of points in space.

## Hypothesis Testing

* *anova.pdf* - introduction to hypothesis testing for equivalence of means for more than two groups. One-way ANOVA, two-way ANOVA, MANOVA, ANCOVA.
* *bootstrapping.pdf* - the ins and outs of nonparametric bootstrapping, i.e. using resampling .
* *multiple_testing.pdf* - procedures for adjusting p-values when conducting multiple hypothesis tests on the same data,
* *NP_hypo_tests.pdf* - nonparametric hypothesis testing, when the assumption of Gaussian-distributed residuals is severly violated.
* *t_tests.pdf* - simple 2 way t-tests for comapring differences in means and proportions

## Predictive Modeling

* *Intro_to_Predictive_Modeling.pdf* - slides on the concepts of fitting models for prediction.
* *bagging_and_random_forests.pdf* - extended/improved versions of decision trees for regression or classification.
* *boosting.pdf* - basics of boosting for regression or classification.
* *decision_trees.pdf* - basics of fitting regression/classification trees.
* *lda_and_nb.pdf* - linear discriminant analysis and naive Bayes models for classification.
* *linear_regression_with_penalties.pdf* - Lasso, ridge, elastic net regression. Used to fit sparse regression models.
* *logistic_regression_with_penalties.pdf* - extensions of above to generalized linear models.
* *support_vector_machines.pdf* - SVM models for classification.

## Statistical Modeling

* *GLM.pdf* - generalzed linear models, specifically logistic regression and Poisson regression
* *VAR.pdf* - vector autoregressive models, used in multivariate time series modeling.
* *mixed_effects.pdf* - basics of mixed effects models.
* *smooth_spline.pdf* - basics of smoothing splines in regression.

## Version Control

* *version_control_and_github.pdf* - slides on the concepts of version control and implementation in the Github framework.

## Workflow Management
* *cleaning_and_wrangling.R* - example of using functions rather than hard coding and scripts for your pipeline.
* *load_dependencies.R* - example of how to check if a local machine has a package installed, and installing if it does not.
* *workflow_with_drake.pdf* - an R package that can help you with managing your data science pipeline.
