# CELIDA User Interface

This repository contains an R Shiny user interface for the CODEX-CELIDA project. It provides an example of how to display patient data and guideline recommendations in an interactive way.

## Overview

The user interface allows users to explore individual patients and the applicability of specific guideline recommendations for each patient. The data is visualized through dynamic tables and plots, providing a clear overview of the patient's status and treatment options.

## Usage

### Run with docker

1. Pull image from docker hub

   ```bash
   docker pull glichtner/celida-user-interface:main
   ```

2. Run container with specified environment variables
   ```bash
      docker run -p 8000:3838 glichtner/celida-user-interface:main
   ```

3. Open `http://localhost:8000` in your browser.

### Run from source

The [CELIDA Execution Engine](https://github.com/CODEX-CELIDA/execution-engine) FastAPI interface needs to be running for the user interface to fetch data from it.

To launch the user interface, simply run the following code in shell:

``` bash
R -e "shiny::runApp(host='0.0.0.0', port=3838)"
```

## Contributing

We welcome contributions to this repository! If you have any suggestions or bug reports, please open an issue or a pull request.

## Contact

For more information about the CODEX-CELIDA project, please visit <https://github.com/CODEX-CELIDA>.
