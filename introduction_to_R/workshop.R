install.packages("xfun")

xfun::pkg_attach2("kernlab", "tidymodels", "tidyverse", "skimr", "ranger", "xgboost", "word2vec", "parallel", "udpipe", "quanteda", "seededlda", "quanteda.textstats", "tidytext")

load(url("https://github.com/stepanpaulik/courts_and_judges/raw/main/introduction_to_R/workshop_data.RData"))

# INTRODUCTION TO R

# Exercise 1: 
# We would like to know whether male and female judges are on average of different age. 
# R has a base function mean(), which takes as an argument numeric/logical vector. 
# Try to apply your knowledge of subsetting and calculate the mean age for both male and female judges 
# [here using the average year of birth suffices but whoever feels fancy may transform it into years].


# Exercise 2:
# The *judge_name* column store information on the name of the judge. 
# # Write a script that will letter by letter reverse the names of judges and store them in a new column *eman_egduj* using for loop 
# [again, there are way more efficient solutions]. 
# To do this, look up the following functions with the ?:
#   str_split()
# paste()


# Exercise 3
# Now look up what functions are and how functions operate. 
# Write a function that will take any character vector as an argument, will do some basic checks and will reverse the order of all of them.


# INTRODUCTION TO TIDYVERSE

# Exercise 1
# # Using the tidyverse group_by(), mutate() and summarise() functions try to calculate how has the caseload
# (i.e. the number of cases submitted to the CCC) developed over time. You can find the information in the metadata object.
# Name the variable of number of cases per year "caseload" and the object, in which you store the information
# "caseload_development"


# If your code worked, the following plot should work too
caseload_development %>%
  ggplot(x = year_decision, y = caseload) +
  geom_point() +
  geom_smooth()

# Exercise 2
# a)
# To get a feeling for the group_by() function, recreate the caseload development but instead of for the CCC as such,
# we are interested in how did the caseload differ between judge rapporteurs.
# Name the object "caseload_judge_development"


# If your code worked, the following plot should work too
caseload_judge_development %>%
  ggplot(x = year_decision, y = caseload) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~judge_rapporteur)

# b)
# # Let's practice joining. Look up the "dissents" object. It contains tidy data information on in which case
# did which judge dissent. We would like to compute their yearly amount of dissents (so the same format as in a) 
# but instead of caseload, we'd like to know the number of dissents). To do so, you firstly need to add the information
# on the year of decision from the "metadata" object and then do the same calculation as in a). Afterwards,
# you need to join both objects together.

# Exercise 3
# Let's try to find out how lenient each chamber is. Try to calculate the average winning chances (i.e., the percent of cases that have outcome "granted") before each chamber each year. 
# Firstly, filter the data so that they contain cases submitted after 2013 and then try to calculate the winning chances.
# The information on outcome is stored in the "outcome" column.


# QUANTITATIVE TEXT ANALYSIS

# Exercise 1:
# Look into the NSS_lawyers tibble. 
# There you will find one column with the name of the lawyer representing a party before the Czech Supreme Administrative Court, 
# including their title, and information whether the case was granted or not. 
# As a first step, try to split the first column into one that contains only the name of the lawyer and one that contains their title 
# and then transform the title into simple boolean if the titles contain PhD or don't. 
# Lastly, build a simple binomial logit model that ascertains the influence of the PhD on your lawyers' winning chances. 

# Create a phd column with a boolean for presence of PhD

# Check the ability distribution
NSS_df %>% 
  ggplot() +
  geom_density(aes(x = ability, group = phd, color = phd))

# The model
# You can add "+ ability" into the formula to control for the lawyers' ability
logit = glm(outcome ~ phd, data = NSS_df, family = "binomial")

plot(logit)

# Exercise 2:
# Extrating with regex. 
# a) The tibble text_corpus contains a sample of CJEU decisions including their texts. 
# Try to extract the names of judge_rapporteurs from the following data

# This is the data you will be working with
judge_rapporteur = text_corpus %>%
  select(ecli, text) %>%
  filter(str_detect(text, "^composed of")) %>%
  as_tibble()

# b) Court decisions are ripe for utilizing intertectuality inherent in the way they are written.
# The texts of the CJEU decisions contain many references to its own decisions.
# The references usually have the form of C- or T-, followed by 2-4 digits, slash ("/"), and 2-4 digits again.
# They are usually also preceded by a reference to the CJEU in some shape.
# Try to mine as many references as possible from the text corpus and if you are feeling fancy look up how a 
# network analysis is conducted.

network_analysis = text_corpus %>%
  filter(section == "grounds") %>%
  select(ecli, text) 

# MACHINE LEARNING 

# Exercise 1
# I prepared data on the CJEU. 
# It includes some basic features of the cases as well as the information on whether the case was granted or not.
# We would like to predict the outcome of a case, which is a typical machine learning classification task.
# To do that, firstly fill out the recipe hashtags. Select the features you think are important for 
# predicting the outcome of the case. Then in the model part, toy around different parameters.
# We're looking for the best accuracy and precision in the room. You can always change the parameters and the formula, fit the model
# and run the whole augment() and precision/accuracy parts to see your result.

# Familiarise yourself with the data
skim(data)
view(data)

# Split the data into testing and training data
data_split = initial_split(data, prop = 3/4)
train_data = training(data_split)
test_data  = testing(data_split)

# Prepare data for k-fold cross-validation
folds = vfold_cv(train_data, v = 6)

# Not all features of this data set are helpful, determine which ones would you like to go in in your formula
rec = recipe(successful ~ #, data = train_data) %>% 
            update_role(#, new_role = "ID") %>%
            update_role(#, new_role = "outcome") %>% 
            update_role(all_predictors(), new_role = "predictor")
rec
                   
mod = rand_forest(mtry = 10,
                  trees = 1000, # This is not a hyperparameter but you can play around a little bit too and see how it impacts the results
                  min_n = 3) %>%
                  set_mode("classification") %>%
                  set_engine("ranger")
                   
wflow = workflow() %>%
        add_recipe(rec) %>%
        add_model(mod)

# Cross-validation - a diagnostic tool, look up, how the function fit_resamples() works and try to run a quick cross-validation
# The fitting
rand_forest_fit = wflow %>% 
  fit(data = train_data)
                   
# Diagnosis
# Measuring the accuracy of the basic model with the augment function or predict, which requires further specifications and more actions
rand_forest_aug =  augment(rand_forest_fit, test_data) %>% 
  select(doc_id, successful, .pred_class)
                   
# Accuracy
rand_forest_aug %>% 
  accuracy(truth = successful, .pred_class) # Accuracy is the proportion of the data that are predicted correctly.
                   
rand_forest_aug %>% 
  conf_mat(truth = successful, .pred_class) # Self-explanatory
                   
                   
save.image(file = "workshop_data.RData")
