library(tidymodels)
library(poissonreg)
box::use(
  DiagrammeR[grViz],
  collapse[descr]
)
theme_set(theme_light())
theme_update(panel.grid = element_blank())
update_geom_defaults("rect", list(fill = "cornflowerblue", color = "black"))

grViz("
digraph {
  graph [ranksep = 0.2]
  node [shape = plaintext]
    A [label = 'N Renovations' fontcolor = 'brown2']
    Y [label = 'N Rooms']
    B [label = 'N Guests']
    C [label = 'Furniture Quality']
    D [label = 'Location']
    E [label = 'Type Guests']
    F [label = 'Hotel Characteristics ?']
  edge [minlen = 2 color = 'cornflowerblue']
    Y -> A
    B -> A
    Y -> B
    E -> A
    E -> B
    C -> A
    D -> A
    D -> E
    D -> C
    D -> Y
    F -> A
  { rank = min; D ;  C}
  { rank = same; B; Y; }
  { rank = same; E; A }
  { rank = max; F }
}
")

dta <- readr::read_csv("analysisgen/case-hotel-renovations.csv")
dta$price_rank <- ordered(dta$price_rank, levels = 1:5)
dta$hotel_id <- factor(dta$hotel_id)
dta$location_id <- factor(dta$location_id)
glimpse(dta)

dta |> 
  ggplot(aes(x = n_renovations)) + 
  geom_histogram(binwidth = 5) + 
  scale_x_continuous(n.breaks = 25)

# here we use the collapse::descr function
descr(dta$n_renovations)

set.seed(47)  # always set this so that you always get the same split again
# dta <- mutate(dta, avg_nrenov = mean(n_renovations), .by = hotel_id)
# splits  <- group_initial_split(dta, prop = 0.8, group = hotel_id, strata = avg_nrenov)
splits  <- group_initial_split(dta, prop = 0.8, group = hotel_id)
dta_test  <- testing(splits)
dta_train <- training(splits)
splits

set.seed(48)
folds <- group_vfold_cv(dta_train, v = 5, group = hotel_id)
print(folds)

lin_model <- linear_reg(mode = "regression", engine = "lm")
pois_model <- poisson_reg(mode = "regression", engine = "glm")

m1 <- 
  recipe(n_renovations ~ n_rooms + n_guests,  data = dta_train) |> 
  step_normalize(all_numeric_predictors())

m2 <- 
  recipe(n_renovations ~ n_rooms + n_guests + family_guests + young_tourists + other + business_traveller,  data = dta_train) |> 
  step_normalize(all_numeric_predictors())

m3 <- 
  recipe(n_renovations ~ n_rooms + n_guests + young_tourists + price_rank,  data = dta_train) |> 
  step_normalize(all_numeric_predictors())

m4 <- 
  recipe(n_renovations ~ n_rooms + n_guests + price_rank,  data = dta_train) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(price_rank)

m5 <- 
  recipe(n_renovations ~ n_rooms + n_guests + price_rank,  data = dta_train) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_poly(n_guests)|> 
  step_dummy(price_rank)


set1 <- 
  workflow_set(
    preproc = list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5),
    models = list(LR = lin_model, PR = pois_model),
    cross = TRUE
  )

wf_rs_fits <- 
  set1 |> 
  workflow_map("fit_resamples", resamples = folds)

results_ranked <- rank_results(wf_rs_fits, rank_metric ="rmse")
results_ranked

best_model_id <- results_ranked$wflow_id[1]

chosen_wf <- 
  set1 |> 
  extract_workflow(best_model_id)

chosen_wf_fit <- 
  chosen_wf |> 
  fit(data = dta_train)

chosen_wf_pred <- 
  chosen_wf_fit |> 
  predict(new_data = dta_test) |> 
  bind_cols(dta_test)

head(chosen_wf_pred)

rmse(chosen_wf_pred, truth = n_renovations, estimate = .pred)

mae(chosen_wf_pred, truth = n_renovations, estimate = .pred)

chosen_wf_pred |> 
  ggplot(aes(y = .pred, x = n_renovations, color = n_rooms)) + 
  scale_y_continuous(limits = range(chosen_wf_pred$n_renovations)) + 
  scale_x_continuous(limits = range(chosen_wf_pred$n_renovations)) + 
  geom_abline(intercept = 0, slope = 1, color = "coral1") + 
  geom_point(alpha = 0.9, shape = 21) +
  labs(y = "predicted n_renovations")

dta2 <- dta |> 
  mutate(
    prop_renov = n_renovations / n_rooms,
    room_freq = n_guests / n_rooms
  )

dta2 |> 
  ggplot(aes(x = prop_renov)) + 
  geom_histogram(binwidth = 0.025) + 
  scale_x_continuous(n.breaks = 10)

descr(dta2$prop_renov)

set.seed(47)
splits2  <- group_initial_split(dta2, prop = 0.8, group = hotel_id)
dta_test2  <- testing(splits2)
dta_train2 <- training(splits2)

set.seed(48)
folds2 <- group_vfold_cv(dta_train2, v = 5, group = hotel_id)
print(folds2)

m6 <- 
  recipe(prop_renov ~ room_freq,  data = dta_train2) |> 
  step_normalize(all_numeric_predictors())

m7 <- 
  recipe(prop_renov ~ room_freq + family_guests + young_tourists + other + business_traveller,  data = dta_train2) |> 
  step_normalize(all_numeric_predictors())

m8 <- 
  recipe(prop_renov ~ room_freq + price_rank,  data = dta_train2) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(price_rank)

m9 <- 
  recipe(prop_renov ~ room_freq + family_guests + young_tourists + other + business_traveller + price_rank,  data = dta_train2) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(price_rank)

m10 <- 
  recipe(prop_renov ~ room_freq + price_rank,  data = dta_train2) |> 
  step_sqrt(room_freq) |> 
  step_dummy(price_rank)

m11 <- 
  recipe(prop_renov ~ room_freq + price_rank + family_guests + young_tourists,  data = dta_train2) |> 
  step_sqrt(room_freq) |> 
  step_dummy(price_rank)

m12 <- 
  recipe(prop_renov ~ room_freq + price_rank + family_guests + young_tourists,  data = dta_train2) |> 
  step_sqrt(room_freq) |> 
  step_poly(room_freq) |> 
  step_dummy(price_rank)

set2 <- 
  workflow_set(
    preproc = list(m6 = m6, m7 = m7, m8 = m8, m9 = m9, m10 = m10, m11 = m11, m12 = m12),
    models = list(LR = lin_model),
    cross = TRUE
  )

wf_rs_fits2 <- 
  set2 |> 
  workflow_map("fit_resamples", resamples = folds2)

results_ranked2 <- rank_results(wf_rs_fits2, rank_metric ="rmse")
results_ranked2

best_model_id2 <- results_ranked2$wflow_id[1]

chosen_wf2 <- 
  set2 |> 
  extract_workflow(best_model_id2)

chosen_wf_fit2 <- 
  chosen_wf2 |> 
  fit(data = dta_train2)

chosen_wf_pred2 <- 
  chosen_wf_fit2 |> 
  predict(new_data = dta_test2) |> 
  bind_cols(dta_test2) |> 
  mutate(pred_renov = n_rooms * .pred)

head(chosen_wf_pred2)

chosen_wf_pred2 |> 
  filter(.pred < 0)

rmse(chosen_wf_pred2, truth = n_renovations, estimate = pred_renov)

mae(chosen_wf_pred2, truth = n_renovations, estimate = pred_renov)

chosen_wf_fit2 |> tidy()

chosen_wf_pred2 |> 
  ggplot(aes(y = pred_renov, x = n_renovations, color = n_rooms)) + 
  scale_y_continuous(limits = range(chosen_wf_pred$n_renovations)) + 
  scale_x_continuous(limits = range(chosen_wf_pred$n_renovations)) + 
  geom_abline(intercept = 0, slope = 1, color = "coral1") + 
  geom_point(alpha = 0.9, shape = 21) +
  labs(y = "predicted n_renovations")

chosen_wf_pred2 |> 
  summarize(
    agg_n_renov = sum(n_renovations),
    agg_pred_n_renov = sum(pred_renov),
    .by = year
  )
