# Shark Species Relative Abundance Estimation

This repository contains data mining and predictive modeling work using iNaturalist data to estimate the relative abundance of shark species in Hawaii and the Bahamas. The analysis employs zero-truncated negative binomial predictive modeling.

## Summary

This project involves data mining from iNaturalist and using zero-truncated negative binomial predictive modeling to estimate the relative abundance of various shark species. The main focus areas are Hawaii and the Bahamas.

## Reports

### Hawaii Species Relative Abundance

The report for the relative abundance of shark species in Hawaii can be found in the following PDF:

- [Hawaii Species Relative Abundance Report](figures/Hawaii_species_relative_abundance.pdf)

### Bahamas Species Relative Abundance

The report for the relative abundance of shark species in the Bahamas can be found in the following PDF:

- [Bahamas Species Relative Abundance Report](figures/Bahamas_species_relative_abundance.pdf)

## Repository Structure

- `data/`: Contains the raw and processed data files.
- `scripts/`: Contains the R scripts used for data processing and modeling.
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
