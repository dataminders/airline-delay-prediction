# Airline delay prediction competition
## Final code


``` bash
git clone https://github.com/dataminders/airline-delay-prediction
cd airline-delay-prediction/code

# Initialize features
Rscript initial_features_add.R
Rscript initial_features_add_weather.R
Rscript initial_features_add_weather_all.R

# Python FTRL Model
pypy ftrl-two-level.py

# R xgboost with different data
Rscript xgb_0005.R
Rscript xgb_0009_weather.R
Rscript xgb_0010_weather.R
Rscript xgb_0013_weather.R

# Weighting
Rscript blend.R

```

# Summary

Best single model result - 0.6957.

Blending used precomputed values from out of sample validation data - last three month of train data.

Evaluation Metric - AUC ROC.

Public score - 0.70355

Private score - 0.70257
