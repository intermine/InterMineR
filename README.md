
Master: [![Build Status: master][travis-badge-master]][ci] Dev: [![Build Status: dev][travis-badge-dev]][ci]

# InterMine-R

R package for accessing InterMine instances

## Installation

Working on adding to Bioconductor. For now, install with devtools: 

devtools::install_github('intermine/intermineR')

## Usage

See HTML vignette for detailed API and tutorial:

 vignettes/InterMineR.Rmd

## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request :D

## Credits

Bing Wang:

* InterMineR package creation (first edition)
* Established the first query system (queries as list objects)
* Vignette creation (first edition)

Konstantinos Kyritsis:

* InterMineR package update to fit with the standards of Bioconductor submission.
* Established the second query system (queries as InterMineR-class objects)
* Addition of enrichment analysis functionality and convertion of the results to GeneAnswer-class objects
* Addition of functionality for converting InterMineR-retrieved data to GRanges-class and RangedSummarizedExperiment-class objects
* Vignette update (second edition) and tutorials addition

## License

LGPL. See LICENSE for details.

[travis-badge-master]: https://travis-ci.org/intermine/InterMineR.svg?branch=master
[ci]: https://travis-ci.org/intermine/InterMineR
[travis-badge-dev]: https://travis-ci.org/intermine/InterMineR.svg?branch=dev
