LFtools
=======

LFtools is a set of four programmes to compute luminosity functions
(LF). One programme corrects the catalogue for absorption, photometric
redshift uncertainties, and redshift incompleteness; all corrections
are optional. The other three programmes compute LFs as binned
estimates, maximum-likelihood fit, or Bayesian posterior
samples. Companion software includes plotting routines.


Installation
------------

### Binary installation ###

Binary files are provided for Linux and Mac at the address http://www.astro.lu.se/~piero/LFtools/index.html .

The maximum-likelihood programme (`lf-ml`) also needs
[CERNLIB](http://cernlib.web.cern.ch/cernlib/). Ubuntu users can
install it from the standard repositories; all other users should get
them from CERN (link above).

The following libraries are also needed for `lf-mn`:

  * [BLAS](http://en.wikipedia.org/wiki/Blas) (any version);
  * openmpi

They are available, for Mageia and Ubuntu, from the standard
repositories and can be therefore installed from there. For CentOS,
which does not provide up-to-date compilers and libraries, we offer
a tar (`centos6-libextra.tgz`) containing precompiled versions of
libopenmpi and libgfortran.


### Source installation ###

LFtools has the following requirements:

  * gfortran, at least version 4.7;
  * CERNLIB (for maximum-likelihood fits);
  * MultiNest (for Bayesian inference).

MultiNest itself requires: cmake, openmpi, BLAS. Please refer to the
MultiNest documentation.

You can get the LFtools sources as a zip file from the
repository. Then unzip and cd into the LFtools directory:

    unzip LFtools.zip
    cd LFtools

Start by compiling the FSON and m_option_parser libraries:
    cd libextra/fson
    make
    cd ../m_option_parser
    gfortran -c m_option_parser.F90
    cd ../..

Next, compile `lf-catcorrect` and `lf-binned`:

    cd src1
    make

If the programmes compile successfully, the binaries will be in
LFtools/bin. Next, compile `lf-ml` and `lf-mn`. These are compiled
separately. Move to the second source directory:

    cd ../src2

and edit the Makefile, making sure to set up correctly the LDFLAGS
line at its beginning: it should points to the location where the
required libraries are. There are a few examples included as
comments. Compile:

    make

Finally, compile the Bayesian inference tools:

    cd Multinest
    make

If the programmes compile successfully, all the binaries will be in
LFtools/bin.

We acknowledge the use of the following Fortran modules, whose
source we redistribute together with LFtools, and which whose copyrights
are of the respective authors:

  * m_option_parser  (BSD license, http://users.telenet.be/tuinbels/fortran_cmd_line_parser.html )
  * FSON             (MIT license, https://github.com/josephalevin/fson )


Preliminary steps
-----------------

### Prepare catalogue, area, and photoz files ###

The first step is to prepare the input catalogue and area files.

The input catalogue should be an ASCII table with the following columns:

 1. ID number
 2. 2-10 keV flux
 3. specflag  (1 if spectroscopic redshift; 0 if photometric)
 4. redshift
 5. match probability (use 1 if you are uncertain)
 6. 0.5-2 / 2-10 keV flux ratio
 7. optical ID number
 8. intrinsic flux (optional; see below)

Comment lines are recognised if they start with a hash symbol (#).
Optical ID numbers will be used to locate the file containing the
photometric redshift probability distribution (one file per source in
the catalogue).

The column "intrinsic flux" can be used if you want to experiment with
your own method for correcting absorption. In that case, fill this
column, and when calling lf-catcorrect disable the programme's absorption
correction and signal that you are providing your own by using the
options: --nhprob=F --intrinsicflux=T .

An example of how to prepare the catalogue can be found in
`src1/convert-cosmos.f90`; this programme reads the XMM-COSMOS catalogue,
extracts the relevant information, and writes it in the LFtools input
format. To compile and run it:

    make convert
    ./convert-cosmos



The area file should be an ASCII table with the following columns:

 1. log10( flux )
 2. coverage (in deg$^{-2}$)

where the coverage is the area over which a source brighter than
*flux* could be observed. Comment lines are recognised if they start
with a hash symbol (#).

If you want to correct for photometric redshift uncertainties, you
need a file for each source in the catalogue. These files should be
named "Id number .spec" where number is the optical ID, padded with
zeroes to 9 digits. Example: source with optical ID = 123 has its
photoz distribution in the file "Id000000123.spec".  These files
should contain a section, starting at line LSTART and with a length of
LLEN, with two columns:

 1. redshift
 2. probability

The programme `lf-catcorrect` will need to know LSTART and LLEN when
run. In this way, you can use photoz files directly as they come from
the photoz software (e.g.,
[LePHARE](http://www.cfht.hawaii.edu/~arnouts/LEPHARE/lephare.html)).
You can put the photoz files in a separate directory from the
catalogue.


### Correct and format the catalogue for downstream tools ###

Having prepared the input catalogue, the area files, and the photoz
files, you can now run `lf-catcorrect` to do the corrections. Using
again XMM-COSMOS as an example:

    ./lf-catcorrect --infile=xmmcosmos-formatted-cat.dat
                   --outfile=xmmcosmos-corrected-cat.dat
                   --nhcorr=T  --savenhcorr
                   --photozpdf=T
                   --pdfstart=63 --pdfstop=713
                   --pdfpath=/home/piero/Dati/Teoria/XMMLSS/SPEC_files_cosmos/
                   --kcorrgamma=1.7
		   --complcorr=T
		   --corrfile=xmmcosmos-fluxhisto.dat
		   --intrinsicflux=F

In this example we have specified all possible options:

  * input and output files (infile, outfile);
  * corrections for absorption (nhprob);
  * save the applied absorption corrections as a further column in outfile;
  * corrections for photoz uncertainties (photozpdf);
  * photoz files are in .../SPEC_files_cosmos;
  * photoz information starts at line 63 and goes on to line 713;
  * K-corrections assume a power-law spectrum with Gamma=1.7;
  * completeness corrections (complcorr, corrfile);
  * use of user-provided intrinsic fluxes (disabled by default, and in the example).

To turn off corrections, use --nhprob=F and/or --photozpdf=F and/or
--complcorr=F (but corrections are anyway off by default).

Completeness corrections are considered experimental, and their use is
discouraged, hence it is not documented. If you need to correct for
incompleteness, a better way is to modify the survey coverage to
reflect this. See e.g. how the XMM-LSS area is treated in Ranalli et
al. 2016.

The only mandatory options are infile and outfile.

The output catalogue file will have many more records and a different
structure with respect to the input catalogue, so be aware that the
two are not interchangeable. The output catalogue is what is needed by
the downstream tools. Its format is, for documentation's sake, the
following:

  1. ID number
  2. absorption-corrected flux
  3. weight
  4. redshift
  5. luminosity
  6. match probability
 [7. optional: applied correction for absorption, in Log-scale]

In fact, what `lf-catcorrect` does is to split each source into all
the possible values of luminosity and redshift that it could have,
given the probability distributions of the photoz and of the
absorption.


Binned luminosity functions
---------------------------

The programme `lf-binned` computes non-parametric, binned estimates of
the LF, using the method described in [Ranalli et
al. 2015](http://arxiv.org/abs/1512.05563) which is a variant of the
method by Page & Carrera (2000, MNRAS 311, 433). Both methods have
their root in the 1/V<sub>max</sub> method (Schmidt 1968, ApJ 151,
393).

As the input for this programme, you need at least a corrected
catalogue produced by `lf-catcorrect`, and an area file. This should
be specified in a configuration file, which allows multiple catalogues
to be read together. Following the previous example:

    ./lf-binned  xmmcosmos.json

The configuration file is in the [JSON](http://en.wikipedia.org/wiki/JSON) format.
See below for [how to write a configuration file](#Configuration files).

The programme will then ask for the bin boundaries: log Lmin, log
Lmax, zmin, zmax. The output, marked by a per cent sign (%) at the end
of the line, will be the LF estimate in Mpc$^{-3}$, followed by the
lower and upper ends of the 68.3% confidence interval. The estimate of
the confidence interval is done assuming Gaussianity if there are at
least 50 sources in the bin; or otherwise using the Gehrels
approximation.

After computation, the programme will ask for a new bin. So, a useful
idiom to calculate an entire LF is:

    ./lf-binned xmmcosmos.json < inputbinned.dat | grep %

Where the file inputbinned.dat contains the definitions of the
interesting bins, and the grep command will remove all the inessential
information and only catch the LF result.


Parametric luminosity functions
-------------------------------

Parametric LFs allow to estimate model features such as the *knee*
luminosity Lstar, critical redshifts, etc., by means of fitting a
model to the data.  The likelihood is a function of the data and of
the model, and measures how well the model describes the
data. (Formally, it is the probability of observing the data, given
the model).

The most common model for LFs is a double powerlaw, which has the
following parameters:

  * gamma1 (slope at L < $\sim$ Lstar);
  * gamma2 (slope at L > $\sim$ Lstar);
  * Lstar;
  * normalisation.

LFs evolve, i.e. they change their parameters with the
redshift. Several models have been proposed in the literature;
currently the most favoured ones are:

  * LDDE (luminosity-dependent density evolution), requiring 5
    parameters to describe the evolution;
  * LADE (luminosity and density evolution), requiring 4 parameters.

An in-depth description of these models, their parameters, and their
astrophysical meaning is beyond the scope of this document. More
information can be found in [Ranalli et
al. 2015](http://arxiv.org/abs/1512.05563) or in other literature
papers (e.g. Aird et al. 2010, ...).


LFtools currently implements the following models:

  * double powerlaw + LDDE;
  * double powerlaw + LADE;
  * double powerlaw + LADEBPL.

We note that LADE here uses a different normalisation than what used
by Aird et al. (2010). Our normalisation is such that density
evolution is zero at z=0, as in Fotopoulou et al. (submitted). For
more information, see [Ranalli et al. 2015](http://arxiv.org/abs/1512.05563).

LADEBPL is a variant on LADE: instead of using the double powerlaw as
in Aird et al. (2010), it uses a broken poweraw as in Ueda et
al. (2003); everything else is the same.


Maximum-likelihood estimates
----------------------------

The programme `lf-ml` finds the best-fit model parameters by
maximising the likelihood. The same model can be fit to several data
sets (currently, up to 10) at the same timing, giving the parameters
that fit best all the data.

To pass a variable number of catalogue and area files to `lf-ml`, a
*configuration file* is used. Also, the user has the possibility of
putting constraints on the parameters and/or fixing them at any value;
this is done through a *command file*.

The programme can be run as:

    ./lf-ml xmmcosmos.json  minuit.ldde

The configuration file (xmmcosmos.json in the example) is a text file
containing the choice of evolution, and the list of catalogue and area
files. The [JSON](http://en.wikipedia.org/wiki/JSON) format is used.
See below for [how to write a configuration file](#Configuration files).

The [MINUIT](http://wwwasdoc.web.cern.ch/wwwasdoc/minuit/minmain.html)
library is used to maximise the likelihood (technically, the opposite
of the likelihood is minimised). The command file (minuit.ldde) is
used by MINUIT to know the range of validity of each parameter, and to
start the minimisation. Two command files, `minuit.ldde` and
`minuit.lade`, are provided with LFtools and can be used as they
are. They contain reasonable defaults and commands, and can be
modified by the user if needed (please refer to the MINUIT
documentation).  MINUIT is a part of the
[CERNLIB](http://cernlib.web.cern.ch/cernlib/) libraries, which must
be installed in order to use `lf-ml`.

The output of `lf-ml` is directly provided by MINUIT; it is usually
self-explanatory.

The running time of `lf-ml` depends on the catalogue size, on
whether absorption and photoz corrections have been applied, and on
the model; in worst cases it may require several tens of minutes.


Bayesian estimates
------------------

While maximum-likelihood gives a *point estimate* of the parameters,
with errors computed assuming Gaussianity in the neighbourhood of the
best-fit values, Bayesian estimates provide a more accurate
description of the best-fitting parameter space. It requires, however,
much longer computational times than maximum-likelihood.

The starting point for Bayesian estimation is the same likelihood
function used for maximum-likelihood. *Prior distributions* are also
required for the parameters under analysis. The priors and the
likelihood are combined to produce *posterior distributions* for the
parameters. The posterior measures the probability of a given set of
parameter values, after having extracted all information from the
data. For more information, we refer the user to any textbook on
Bayesian statistics (e.g. Gregory, "Bayesian logical data analysis for
the physical sciences", Cambridge, 2005), or to astrophysics-oriented
introductions such as Andreon 2011 (arXiv:1112.3652).

An important aspect of Bayesian computation is the method used to get
the posterior distributions. Most times, these distributions cannot be
obtained in closed form. Instead, they are usually obtained in the
form of *posterior samples*, i.e. large sets of parameter values whose
statistical distribution follows the posterior. The posterior
distribution is then reconstructed empirically from the posterior
samples.

Several methods have been devised to obtain posterior samples, the
best-known one probably being MonteCarlo Markov Chains
(MCMC). Recently, *nested sampling* has been proposed as a more
efficient alternative (Skilling 2006, Bayesian Analysis, 1, 833) and
used for LFs by Aird et al. (2010). A growingly-used implementation of
nested sampling is offered by the *MultiNest* libraries (Feroz et
al. 2008, arXiv:0809.3437; Feroz et al. 2013, arXiv:1306.2144).


### lf-mn ###

The programme `lf-mn` performs Bayesian estimation of LFs, using the
MultiNest libraries for posterior sampling. It can be run as a
single-processor job as:

    ./lf-mn xmmcosmos.json

or as a parallel job as (for example, on 4 processor cores):

    mpirun -n 4 ./lf-mn xmmcosmos.json

The configuration file (xmmcosmos.json in the example) is a text file
containing the choice of evolution, the prior distributions, and the
list of catalogue and area files. The same configuration file may be
used for maximum-likelihood and for Bayesian analysis (see below for
details). The catalogue and area files are specified in the the
configuration; they should be in the same format used for `lf-binned`
and `lf-ml`.

The prior distributions can be either flat within a range, or a
combination of Cauchy and Gamma functions. See [below](#Prior
distributions).

The output of `lf-mn` is governed by the MultiNest libraries. A series
of files will be created. Below we only describe two of them, and refer
to the MultiNest documentation for further information.

A nice feature of the MultiNest libraries is that they allow resuming
an interrupted run. As long as the output files are not tampered with,
and the [root]resume.dat file is present, you can restart the
computation using the same command given when starting.


### Prior distributions ###

MultiNest assumes that all parameters follow a uniform distribution
bounded in the [0,1] interval. These parameters must be mapped to
physical-world parameters (e.g. normalisation, Lstar, ...) by the
likelihood function.

LFtools can use either flat priors, where the parameters are uniformly
distributed in a physically-relevant interval, or a combination of
Cauchy and Gamma function.

An example of flat priors is the following:

    parameter              min      max
    -----------------------------------
    A                       -1        3
    gamma1                  .3        3
    gamma2                  .3        5
    log Lstar             41         47
    zc                      .01       5
    p1                   -10         10
    p2                   -10         10
    alpha (LDDE only)     -1          3
    La (LDDE only)        41         47
    d (LADE only)         -1          5

They should be specified in the configuration file by assigning the
keyword:

        "priors"    : "flat"

and listing the min-maxes of the parameters as follows:

    "ladelimits" : {
	"A"     : [ -1., 5. ],
	"gamma1": [ -7, 7. ],
	"gamma2": [ -7, 7. ],
	"Lstar" : [ 41, 47 ],
	"zc"    : [ 0.01, 5 ],
	"p1"    : [-10, 10 ],
	"p2"    : [-10, 10 ],
	"d"     : [ -1, 5  ]
    },
    "lddelimits" : {
	"A"     : [ -1., 5. ],
	"gamma1": [ -7, 7. ],
	"gamma2": [ -7, 7. ],
	"Lstar" : [ 41, 47 ],
	"zc"    : [ 0.01, 5 ],
	"p1"    : [-10, 10 ],
	"p2"    : [-10, 10 ],
	"alpha" : [ -1, 3  ],
	"La"    : [ 41, 47 ]
    }


Cauchy/Gamma priors do not have hard bounds as the flat
prios. Instead, they have a location (Cauchy only) and scale. Cauchy
priors have the following probability density distribution:

    P_Cauchy(x) = 1 / { PI * [1 + ((x-m)/s)^2] }

where PI is 3.141593..., m is the location, and s is the scale. The
Cauchy distribution is also known as the Lorentz distribution.
Gamma priors follow a Gamma(k=1,theta=1) distribution:

    P_Gamma(x) = exp(-x/s)

The main difference is that the Cauchy distribution allows negative
values, while the Gamma does not. Therefore the Gamma is especially
useful for quantities such as redshift. This kind of prior should be
specified as follows:

    "ladecauchygamma" : {
	"A"     : [ 1.5,  2.5 , 1],
	"gamma1": [ 0.6,  2.5 , 1],
	"gamma2": [ 3,    2.5 , 1],
	"Lstar" : [ 45,   2.0 , 1],
	"zc"    : [  2,     0 , 0],
	"p1"    : [ 6.4,    5 , 1],
	"p2"    : [ -0.24,  5 , 1],
	"d"     : [ -0.2, 2.5 , 1]
    }

Each triplet of numbers specifies location, scale, and switch.  The
switch is the last element in the triplet, and commands whether the
Cauchy is to be used (switch=1) or the Gamma (switch=0).
For Cauchy, the two first and second elements in the
triplet are location and scale, respectively. For Gamma, the first
element is scale and the second is not used.

Location can be thought as the mean of the distribution, and scale has
the same role that the sigma has for Gaussians. Location should be set
to some "best-guess" value, e.g. results from a previous paper. Scale
is a bit trickier because Cauchy distributions have long tails;
treating them as they were Gaussian sigmas is a reasonable guess.

The LADEBPL model uses the same priors of LADE, so they shold be
specified with "ladelimits" or "ladecauchygamma".
    

### Output: posterior samples file ###

The file `[root]post_equal_weights.dat` contains the posterior
samples. Each line is a draw from the posterior distribution of the LF
parameters. The structure of the file is as follows:

 1. parameter no. 1 (A = LF normalisation)g
 2. parameter no. 2 (gamma1)
 3. parameter no. 3 (gamma2)
 4. parameter no. 4 (log Lstar)
 5. parameter no. 5 (zc)
 6. parameter no. 6 (p1)
 7. parameter no. 7 (p2)
 8. parameter no. 8 (if LDDE: alfa; if LADE: d)
 9. if LDDE: parameter no. 9 (La); if LADE: log likelihood
10. if LDDE: log likelihood;       if LADE: not used

The parameters are in MultiNest's scale, i.e. in the 0,1 interval. The
Perl scripts `readchains.pl` and `readchains-lade.pl` (see the section
[Companion programmes](#Companion programmes)) can be used to scale
the parameters to their phyisical-world scales:

    ./readchains.pl my_post_equal_weights.dat > my_rescaled_posteriors.dat
         (or)
    ./readchains-lade.pl my_post_equal_weights.dat > my_rescaled_posteriors.dat

The rescaled posteriors can be inspected for example with
[TOPCAT](http://www.star.bris.ac.uk/~mbt/topcat/), which can also plot
the marginal densities.  To plot the LF, the `hpdchains3.pl` and
`hplplot5.pl` programmes can be use (see [Companion
programmes](#Companion programmes)).

As further possiblities, we note that the MultiNest documentation
mentions the following options for visualising its output:

  * the getdist package which is part of
    [CosmoMC](http://cosmologist.info/cosmomc/readme.html#Analysing);
  * [PyMultiNest](https://github.com/JohannesBuchner/PyMultiNest).


### Livepoints file ###

The file `[root]physlive.dat` (where *[root]* is that specified in the
configuration file) contains the *live points* used by MultiNest for
its sampling. The live points are continuously updated during
computation, and represent the current best estimate of the LF. This
file is useful for monitoring MultiNest's progress. The columns in the
file contain first the parameters, than the log-likelihood and the
mode number. The format is therfore the same as for
post_equal_weights.dat, just with a further column. The same set of
companion programmes (`readchains.pl`, `hpdchains3.pl`, `hpdplot5.pl`)
can be used.


### MultiNest ###

The basic ("vanilla") MultiNest algorithm is used, with multi-modal
search turned on. Switching to Importance Nested Sampling, or to
constant-efficiency mode, requires editing the file params.f90 and
recompiling `lf-mn`. Other hard-coded parameters of interest are:
tol=0.5, efr=0.5. See the MultiNest documentation.


Configuration files
-------------------

The following is an example of a configuration file:

    {
        "catalogues" : [
	    {
		"cat"  : "catalogue-cdfs.dat",
    	    	"area" : "xmmcdfs-completeness210.dat"
    	    },
	    {
		"cat"  : "catalogue-cosmos.dat",
    	    	"area" : "xmmcosmos-area.dat"
    	    }
        ],
	"cosmology" : {
		    "H0" : 70.,
		    "OL" : 0.7,
		    "OM" : 0.3
		    },
        "nhcorr" : 1,
	"evolution" : "ldde",
        "root" : "chains/test-",
        "seed" : 8471
    }

Not all keywords need to be specified, since this depends on which
programme in LFtools is being run. The requirements are:

  * `lf-binned` just needs catalogues and cosmology;
  * `lf-ml` needs catalogues, cosmology, and evolution;
  * `lf-mn` needs everything.

The example above includes:

  * the seed for the number generator;
  * a root name for the output files (in this case, files will be put
    in a `chains` subdirectory, and their name will start with
    `test-`. The `chains` directory should be created before running
    `lf-mn`);
  * the choice of evolution (ldde or lade);
  * a flag (nhcorr) to specify that absorption corrections should be
    considered when computing the coverage (see [Ranalli et al. 2015](http://arxiv.org/abs/1512.05563)
    nhcorr=0 means using Eq.(6), nhcorr=1 means using Eq.(9);
  * a list of catalogues and area files. Note the use of square
    brackets to contain the list, and of braces to contain the
    {cat,area} pairs.

If there is any error in the format of the configuration file, the
program will exit with an error referring to "object creation" or
"unexpected character".

Note that the JSON standard specifies that number start with a number,
that is, a quantity like 0.1 should be written as "0.1" and not as
".1". A common error may therefore be the following:

    ERROR: Unexpected character while parsing value. '.' ASCII=          46

which is usually due to some number written with a leading dot (e.g.: .1).

For more information on the JSON format, see
e.g. [the wikipedia article](http://en.wikipedia.org/wiki/JSON) or the
[official website](http://json.org/).



How to add more evolution models
--------------------------------

Editing the code is required.

1. In src2/lumf_funcs.f90, add a new evolution type by subclassing
'evolution' (see how ldde, lade etc. work).  Non-power-law models can
be added by subclassing z0function (see the doublepowerlaw
class). Models that break the assumption that evolution can be broken
into a luminosity-evolution part and a density-evolution part
(e.g. the flexible double power-law by Aird et al. 2015) may require
a complete rework of the class hierarchy.

2. In src2/startup.f90, subroutine allocateLF, add a proper case to
the if/elseif/endif sequence to allocate the newly defined class.

3. In src2/ml-fit.f90, subroutine FCN, add a proper case to the select
type statement. You need to pass to ev%set() the proper amount of
parameters: start with xval(5) and increment up to what you need.

4. In src2/Multinest/lfmnconfig.f90, subroutine configure, add a
proper case to the if/elseif/endif sequence right after the 'set prior
parameters' comment. The variable numparams is the total number of
parameters (4 for the double powerlaw, plus what you need for
evolution). End the block with a call to a new subroutine
('set_yourevolution_params'). Write the latter subroutine, looking at
the others as examples. The variable 'keyword' is the one which you
will need to specify in the configuration .json file.

5. In src2/Multinest/mn_glue.f90, subroutine calclikelihood, add a
proper case to the select types, to pass the parameters from MultiNest
to the luminosity function code.

6. Recompile, calling make both in src2/ and in src2/Multinest/.



Companion programmes
--------------------

### Convert MultiNest chains to physical-world scale ###

    ./lf-readchains  config.json 1-post_equal_weights.txt > 1-.rescaled

lf-readchains converts the unity hypercube used by MultiNest into
physical coordinates. The post_equal_weights output from MultiNest is
read, rescaled according to the user choice of evolution and parameter
limits, and output to the terminal. (The output is redirected on a
file in the example above). Two parameters are needed on the command
line: the config file, and the post_equal_weights from MultiNest.

lf-readchains is written in Perl and uses the JSON::Tiny and
File::Slurp modules, which can be installed from CPAN.


### Plot all chains ###


### Compute and plot Highest Posterior Densities (HPD) ###



Caveats
-------

  * Coverage curves are currently limited to <=150 data points




