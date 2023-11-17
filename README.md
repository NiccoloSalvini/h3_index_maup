# h3_index_maup


verify MAUP on H3 spatial index at different resolution


## contents

```.
├── LICENSE
├── README.md
├── app
│   ├── app.R
│   └── app_2.R
├── data
│   ├── deepnudenow.com-0be15599-0ce2-4fb2-b4ae-c610e7b6a5d3.png
│   └── defibrillatori.geojson
├── h3_index_maup.Rproj
└── notebooks
    ├── 01_test_how_h3_works.R
    ├── 02_smaup.R
    ├── 03_full_analysis.R
    ├── 04_defibrillatori_analysis.R
    ├── 05_lisa_outlier_sens.R
    └── utils.R
```


- notebooks contains analysis, look at `03_full_analysis`.
- `/app` folder contains 2 Shiny apps
  - `app.R` visualises **MAUP** across resolutions and that's it
  - `app_2.R` visualises **MAUP** across resolutions + different grid options (which may be a further paper)
