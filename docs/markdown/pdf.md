# CYCO PDF Plugin

The PDF plugin defines several probability density functions.   Most of
these functions were ported from
[Nyquist](https://en.wikipedia.org/wiki/Nyquist_(programming_language)). <br>


All PDF functions take an optional :RNF keyword to set a random-number
generator.  If specified an RNF function should take a single argument n and
return a float between 0 and n.

    (lambda n) --> q   where 0 <= q < n

The default RNF function is RANDOM.


## Continuous Distributions

**(LINEAR-DIST (&key (scale 1.0)(rnf #'random))** <br><br>
**(EXPONENTIAL-DIST (&key (delta 2)(rnf #'random))** <br><br>
**(GAMMA-DIST (&key (mu 5)(rnf #'random))** <br><br>
**(BILATERAL-DIST (&key (mu 0)(tau 1)(rnf #'random))** <br><br>
**(CAUCHY-DIST (&key (tau 1.0)(rnf #'random))** <br><br>
**(HYPERBOLIC-COSINE-DIST (&key (rnf #'random))** <br><br>
**(GAUSSIAN-DIST (&key (mu 0)(sigma 1)(rnf #'random))** <br><br>
**(BETA-DIST (&key (a 1)(b 1)(rnf #'random))** <br><br>

## Discrete Distributions

**(BERNOULLI-DIST (&key (p 0.5)(a 1)(b 0)(rnf #'random))** <br><br>
**(BINOMIAL-DIST (&key (p 0.5)(count 10)(rnf #'random))** <br><br>
**(GEOMETRIC-DIST (&key (p 0.5)(count 10)(rnf #'random))** <br><br>
**(POISSON-DIST (&key (mu 5)(rnf #'random))** <br><br>

## Factories

For each distribution function described above there is a corresponding
factory function which takes the same parameters and returns a new
parameter-less function.

For distribution function (FOO-DIST &key a b c) the corresponding factory is<br><br>
(FOO-PDF &key a b c minmax)    The minmax argument is a list (min max)
which sets the allowed range of the results.


**(LINEAR-PDF (&key (scale 1.0)(rnf #'random) minmax)** <br><br>
**(EXPONENTIAL-PDF (&key (delta 2)(rnf #'random) minmax)** <br><br>
**(GAMMA-PDF (&key (mu 5)(rnf #'random) minmax)** <br><br>
**(BILATERAL-PDF (&key (mu 0)(tau 1)(rnf #'random) minmax)** <br><br>
**(CAUCHY-PDF (&key (tau 1.0)(rnf #'random) minmax)** <br><br>
**(HYPERBOLIC-COSINE-PDF (&key (rnf #'random) minmax)** <br><br>
**(GAUSSIAN-PDF (&key (mu 0)(sigma 1)(rnf #'random) minmax)** <br><br>
**(BETA-PDF (&key (a 1)(b 1)(rnf #'random) minmax)** <br><br>
**(BERNOULLI-PDF (&key (p 0.5)(a 1)(b 0)(rnf #'random) minmax)** <br><br>
**(BINOMIAL-PDF (&key (p 0.5)(count 10)(rnf #'random) minmax)** <br><br>
**(GEOMETRIC-PDF (&key (p 0.5)(count 10)(rnf #'random) minmax)** <br><br>
**(POISSON-PDF (&key (mu 5)(rnf #'random) minmax)** <br><br>
