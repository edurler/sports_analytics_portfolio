# Sports Analytics Portfolio

This repository now hosts a Shiny portfolio with a home screen that links to multiple projects.
The first project explores 4th down decision-making in the NFL using `nflfastR` play-by-play data from 2018 to 2023.
It compares actual coaching decisions to an analytics-based recommendation model and visualizes trends, success rates, and league-wide behavior.

🔗 **Live App**: [https://yourname.shinyapps.io/sports_analytics_portfolio](https://yourname.shinyapps.io/sports_analytics_portfolio)

## Features

- Filter plays by team, quarter, field position, and score differential
- Compare actual vs. recommended “go for it” rates
- Visualize team and league success trends
- Explore individual 4th down play decisions

## Technologies Used

- **R** / **Shiny**
- `nflfastR`, `tidyverse`, `gt`, `plotly`, `ggimage`, `ggrepel`, `DT`
- Hosted on **[shinyapps.io](https://shinyapps.io)**
- Version-controlled via **GitHub**

## Supabase Integration

Play-by-play data must be stored in a Supabase table named `pbp`. Create a
`.Renviron` file in the project directory containing the variables
`SUPABASE_URL` and `SUPABASE_KEY`. The app reads these credentials at startup
and fetches the data using Supabase's REST API. If the request fails, the app
will display an error.

## Screenshots

![screenshot](https://yourname.github.io/sports_analytics_portfolio/screenshots/summary_tab.png)  
*Team summary view comparing actual vs. recommended decisions*

## About

Created and maintained by [Your Name](https://github.com/edurler).  
Project completed as part of a senior portfolio in sports analytics.

---

To use or adapt this project, please credit the original author.



