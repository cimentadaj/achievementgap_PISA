## An analysis of the achievement gap between top-bottom SES students over time.

In this repository I keep the files for the second chapter of my PhD thesis.

Below is an explanation of all the files in the repo.

* The main files are `.paper/Paper.Rnw` and `transform_data.R`. The first document contains all the text and analysis of the paper and the second one downloads, manipulates and saves the data.

* If you open the `*.Rproj` file you should open the previous two scripts (although I simply load the data produced by `transform_data.R` and don't run it in the `.Rnw` file. Check `transform_data.R` for details on how I download the data)

* The data used in the analysis is in the folder `./paper/data` (not in the Github repo, but it contains all data created from `transform_data.R`) and it is called `pisa_listcol.Rdata` because it is actually a `tibble` with a list column containing the six PISA waves that will be used. This file is nearly 9GB.

* All PISA waves I use are taken from [Przemyslaw Biecek](https://github.com/pbiecek?tab=repositories) and his `PISA*lite` packages, except for PISA 2012 and PISA 2015, which I download in the `transform_data.R` script (as temporary files) and store everything in `pisa_listcol.Rdata`.

* All other `.Rdata` in `.paper/data` belong to estimations that take too much time to compute, so I saved provisional versions in this folder. For more details, see the `modeling` chunk in the `Paper.Rnw`.

* Inside `./paper/` there is a `packrat` document containing all the packages used in this analysis together with it's versions.

* Don't worry about overloading Github because I excluded both the `cache` and `data` folders in `.gitignore`

* The `playing_around` folder has a script I was playing around. See comments inside script for details.

* Finally, the paper should run seamlessly by compiling  `Paper.Rnw` with `Sweave` and a pdf file should be saved in the `./paper` folder. Without messing up the `cache` chunks, it currently takes around 4 minutes to compile.


