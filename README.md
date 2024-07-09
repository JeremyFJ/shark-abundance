# Shark Species Relative Abundance Estimation

This repository contains data mining and predictive modeling work using iNaturalist data to estimate the relative abundance of various shark species in Hawaii and the Bahamas. The analysis employs zero-truncated negative binomial predictive modeling.

## Reports

**Context**\\
Sharks remain one of the most vulnerable and exploited groups of large marine animals on the planet and it is largely due to knowledge gaps in their population ecology. To empower conservation, we can utilize untapped resources for reconstructing population trends. Social Networks contain vast amounts of observations of sharks in the wild. Here, we use iNaturalist to source shark sightings, develop proxies of effort, and predict time series of relative abundance indices.

**Model rationale**\\
We sourced opportunistic shark sightings uploaded by iNaturalist users in two representative regions that are highly trafficked for tourism, underwater photography, and are popular among social media: Hawaii and the Bahamas. We used the entire presence of iNaturalist users in the respective regions as a proxy for observation effort.

The biases concerning this data are popular among most social network platforms. First, there is no way to discern the level of effort at which users were targeting sharks and not other animals. This means that without surveying the users, we cannot gauge their affinity to capture shark presence as opposed to other wildlife, which is important for weighting effort. Second, the opportunistic nature of this data means absences cannot be trusted and therefor only if a shark was observed, the data is approximately correct.

We based our modeling methods on the Baum et al., 2003 report to predict abundance trends of various Northwest Atlantic shark populations. Here, they used Generalized Linear Models (GLMs) with zero-truncated negative binomial distribution to account for unreporting in pelagic longline logbook data. We adopted this method here because our data likewise represents unreported and sparse shark sightings. 

**The Data**

### Hawaii Species Relative Abundance

The report for the relative abundance of shark species in Hawaii can be found in the following PDF:

- [Hawaii Species Relative Abundance Report](figures/Hawaii_species_relative_abundance.pdf)

### Bahamas Species Relative Abundance

The report for the relative abundance of shark species in the Bahamas can be found in the following PDF:

- [Bahamas Species Relative Abundance Report](figures/Bahamas_species_relative_abundance.pdf)

## Repository Structure

- `./`: Contains the R scripts used for data processing and modeling.
- `data/`: Contains the raw and processed data files.
- `figures/`: Contains the generated reports and plots.
- `README.md`: This file, providing an overview of the repository.

## Usage

To replicate the analysis, follow these steps:

1. Clone the repository:
    ```bash
    git clone https://github.com/yourusername/shark-abundance.git
    cd shark-abundance
    ```
2. Source iNaturalist with `rinat` package and `inat_cron.R` 

3. Run `inat_species.R` to organize data, create and implement ZTNB model, and output reports

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- iNaturalist for providing the data.
- The R community for the powerful tools and packages.
