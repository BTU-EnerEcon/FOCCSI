<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/BTU-EnerEcon/FOCCSI">
    <img src="https://github.com/BTU-EnerEcon/FOCCSI/blob/main/images/logo.PNG" alt="Logo">
  </a>

  <h2 align="center">FOCCSI Meta Forecaster</h2>

  <p align="center">
FOCCSI Meta Forecaster tool allows users to combine individual forecasts of photovoltaic or wind feed-in into a meta forecast. The tool is open to all users interested to obtain more precise combined forecast of renewable energy.
  </p>
</p>


<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li><a href="#about-the-project">About The Project</a></li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#Methodology">Methodology</a></li></li>
    <li><a href="#prerequisites">Prerequisites</a></li></li>
    <li><a href="#registration">Registration</a></li></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#citing-us">Citing US</a></li>
    <li><a href="#acknowledgements">Acknowledgements</a></li>
    <li><a href="#license">License</a></li>
  </ol>
</details>


<!-- ABOUT THE PROJECT -->
## About The Project

### Correction and combination methods for forecasting to improve system integration of renewable energies (FOCCSI)

The project FOCCSI (Forecast Optimisation by Correction and Combination methods for System Integration) is a three-year research project funded by the Federal Ministry of Economic Affairs and Energy. It pursues the improvement of energy-related forecasts, such as the prediction of generated wind energy, over all time horizons. It thus contributes to the system integration of renewable energies into existing energy systems at the national and international levels. Therefore, better forecasts indirectly contribute to the achievement of the fundamental energy transition goals: They reduce dependence on energy imports, ensure affordable energy costs and make an important contribution to solving global climate problems.

Precise forecasts for the feed-in of fluctuating renewable energies and current electricity prices are key elements to the planning and control of power generation, grid operation and commercialization of electricity. The project FOCCSI is devoted to the research of efficient methods for predicting highly dynamic forecast variables, such as wind energy or electricity prices. The central research aspect is the development of statistical methods for the correction and combination of forecasts, which are used to generate superior meta-forecasts. Due to the generality of the investigated methods, they are also interesting for further applications.

### Cooperation and Funding: 

Transmission operator 50Hertz as associated cooperation partner

Federal Ministry of Economic Affairs and Energy (BMWi)

<!-- USAGE EXAMPLES -->
## Usage

Among other things, the users have the option to plot the forecasting errors of each of the individual forecasts entering the "meta-forecast".

<!-- Teaser Image 1 -->
<br />
<p align="center">
    <img src="https://github.com/BTU-EnerEcon/FOCCSI/blob/main/images/Teaser charts.png" alt="Logo">
  </a>

  <p align="center">
 
 The users also have the option to plot the forecasting errors of the BTU "meta-forecast" (also known as DELNET) and the benchmarks such as the simple average or an initial combined forecast they already have in their data. The number of forecasts entering the "meta-forecast" is also included in the figure.
 
  <!-- Teaser Image 2 -->
<br />
<p align="center">
    <img src="https://github.com/BTU-EnerEcon/FOCCSI/blob/main/images/teaser chart 2.png" alt="Logo">
  </a>

  <p align="center">

More features and examples could be found in the [FOCCSI Meta Forecaster](https://nb5.ew.tu-cottbus.de/) web-page.

<!-- METHODOLOGY -->
### Methodology

The [FOCCSI Meta Forecaster](https://nb5.ew.tu-cottbus.de/)  is based on Dynamic Elastic Net (DELNET) Model with Dynamic Data Pre-Processing (DDP). Details on the metodology can be found here:

[DELNET](https://github.com/BTU-EnerEcon/FOCCSI/blob/main/Nikodinoska_DELNET.pdf)

<!-- Prerequisites -->
### Prerequisites

Supported browsers include: Google Chrome and Mozilla Firefox.

<!-- USAGE EXAMPLES -->
### Registration

Before accsessing the [FOCCSI Meta Forecaster](https://nb5.ew.tu-cottbus.de/) , users have to get registered with their preferred email address. In this step, users also have the option to choose their prefered language: English or German. The chosen language is then avaiable throughout the usage of the application and can not be changed afterwards. After registration, users will be sent a confirmation email from the address: fg-energiewirtschaft+foccsi@b-tu.de, including their username and temporary passsword. Password can be updated after the first login. 

<!-- Registration Image -->
<br />
<p align="center">
    <img src="https://github.com/BTU-EnerEcon/FOCCSI/blob/main/images/registration.png" alt="Logo">
  </a>

  <p align="center">


<!-- CONTACT -->
## Contact

Prof. Felix Müsgens: https://www.b-tu.de/en/fg-energiewirtschaft/team/chairholder


<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements

This work has been realized in the framework of the public research project FOCCSI (Forecast Optimization by Correction and Combination methods for System Integration; 03ET4056) funded by the German Federal Ministry for Economic Affairs and Energy. 

Methodology for the [FOCCSI Meta Forecaster](https://nb5.ew.tu-cottbus.de/)  have been developed by Dr. Mathias Käso and Dr. Dragana Nikodinoska.

The user interface and software development has been undertaken by Duktil UG (Matthias Tylkowski and Alexandru Giurca).


## Citing us

If you use [FOCCSI Meta Forecaster](https://nb5.ew.tu-cottbus.de/), we would appreciate it if you would cite the following paper:

* [reference to paper]

Please use the following BibTeX: ::

   @article{
   BibTex
   }


## License

Copyright 2021 Felix Müsgens, Dragana Nikodinoska, Mathias Käso (BTU)

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />The FOCCSI Meta Forecaster tool is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a> that provides everyone with free and perpetual permission to access, edit and share copies of this software.
