Master: [![Build Status: master][travis-badge-master]][ci] Dev: [![Build Status: dev][travis-badge-dev]][ci]

# InterMine-R

<img src="https://cdn.rawgit.com/Bioconductor/BiocStickers/e3a0fb08/InterMineR/InterMineR.png" width="200" />

R package for accessing InterMine instances 

## Installation

InterMineR has been added to [Bioconductor](https://bioconductor.org/packages/release/bioc/html/InterMineR.html).

To install this package, start R and enter:

`## try http:// if https:// URLs are not supported`

`if (!requireNamespace("BiocManager", quietly=TRUE))`
    `install.packages("BiocManager")`

`BiocManager::install("InterMineR")`

In case installation for RCurl fails showing the error`installation of package ‘RCurl’ had non-zero exit status`, install libxml2-dev, libcurl4-openssl-dev and aptitude on you system using the following commands :
 
 `sudo apt-get install aptitude`

 `sudo apt-get install libcurl4-openssl-dev`

 `sudo apt-get install libxml2-dev`

## Usage

See HTML vignettes for detailed API and tutorials:

* vignettes/InterMineR.Rmd
* vignettes/Enrichment_Analysis_and_Visualization.Rmd
* vignettes/FlyMine_Genomic_Visualizations.Rmd

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

* InterMineR package update to:
  1. Operate with the [InterMine web services](http://intermine.readthedocs.io/en/latest/web-services/).
  2. Comply with the standards of [Bioconductor submission](https://www.bioconductor.org/developers/package-submission/).
  3. Retrieve the available Mines from the [InterMine registry](http://registry.intermine.org/).
* Established the second query system (queries as `InterMineR-class` objects)
* Addition of enrichment analysis functionality and convertion of the results to `GeneAnswer-class` objects
* Addition of functionality for converting InterMineR-retrieved data to `GRanges-class` and `RangedSummarizedExperiment-class` objects
* Additional functions:
  1. `simplifyResult` function for flattened results display.
  2. `listDatasets` and `getDatasets` to retrieve information about the available datasets in each Mine.
* Vignette update (second edition)
* Addition of tutorials for *Enrichment Analysis and Visualization* and *FlyMine Genomic Visualizations* with InterMineR

## License

LGPL. See LICENSE for details.

[travis-badge-master]: https://travis-ci.org/intermine/InterMineR.svg?branch=master
[ci]: https://travis-ci.org/intermine/InterMineR
[travis-badge-dev]: https://travis-ci.org/intermine/InterMineR.svg?branch=dev

## Stickers
Sticker templates are available from [BiocStickers](https://github.com/Bioconductor/BiocStickers/tree/master/InterMineR) - or find us at a conference to pick some up in person. 
