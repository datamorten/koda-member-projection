# Koda Member Projection

This R project forecasts female membership trends for Koda using historical data, logistic growth models, and Monte Carlo simulations. It preprocesses data, fits a logistic model, simulates future predictions, calculates confidence intervals, and iteratively forecasts until a target female membership percentage is reached.

## Features

- **Data Preprocessing:** Imputes missing values and computes key membership metrics.
- **Model Fitting:** Uses a logistic growth model (via `nls.multstart`) to capture trends.
- **Simulation & CI Calculation:** Employs Monte Carlo simulation and exponential scaling to estimate confidence intervals.
- **Iterative Forecasting:** Projects future female membership

## Requirements

- R (version 3.6+ recommended)
- R packages: `tidyverse`, `nls.multstart`, `showtext`, `zoo`

## Installation

1. **Clone the Repository:**
   ```bash
   git clone https://github.com/datamorten/koda-member-projection.git

2. **Install Required Packages:**  
   In R, run:
   ```r
   install.packages(c("tidyverse", "nls.multstart", "showtext", "zoo"))
   ```

---

## Usage

1. Open the project in your R environment (e.g., RStudio).
3. Run the main script (e.g., `koda_member_projection.R`) to execute the analysis, generate forecasts, and produce visualizations.

---

## Contributing

Contributions are welcome! To contribute:
1. Fork the repository.
2. Create a feature branch (e.g., `feature/my-new-feature`).
3. Commit your changes with clear, descriptive messages.
4. Submit a pull request for review.

---

## License

This project is licensed under the MIT License. The full license text is below:

```
MIT License

Copyright (c) [2025] [Morten Ogstrup Nielsen]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
