library("DALEXtra")
titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
h2o::h2o.init()
h2o::h2o.no_progress()
titanic_h2o <- h2o::as.h2o(titanic_train)
titanic_h2o["survived"] <- h2o::as.factor(titanic_h2o["survived"])
titanic_test_h2o <- h2o::as.h2o(titanic_test)
model <- h2o::h2o.gbm(
  training_frame = titanic_h2o,
  y = "survived",
  distribution = "bernoulli",
  ntrees = 500,
  max_depth = 4,
  min_rows =  12,
  learn_rate = 0.001
)
explainer <- explain_h2o(model, titanic_test[,1:17], titanic_test[,18])

x_dalex <- simple_test %>% select(-response)
y_dalex <- simple_test %>%
  transmute(response = response %>%
              as.numeric()) %>% 
  mutate(response = if_else(response == 1,
                            0,
                            1)) %>% as.data.frame()
y_dalex <- y_dalex[,1]
  
  
explainer <- explain_h2o(aml@leader,x_dalex,y_dalex)


mp_h2o_gbm <- model_performance(explainer)

plot(mp_h2o_gbm)
plot(mp_h2o_gbm, geom = "boxplot")


mpp_h2o_glm <- accumulated_dependency(explainer,variables = "B1_2")
mpp_h2o_glm
plot(mpp_h2o_glm)



library(ingredients)
plot(feature_importance(explainer))
describe(feature_importance(explainer))

library(iBreakDown)
plot(break_down(explainer, x_dalex[1,]))

describe(break_down(explainer,x_dalex[1,]))


library(shapper)
plot(shap(explainer, titanic_test[2,1:17]))