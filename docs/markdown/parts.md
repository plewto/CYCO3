# CYCO Parts

A **PART** is an extension of **TIME-SIGNATURE** which combines a set of
instruments with instructions of what they are to play.  <br>

There are several types of PART and they form the bulk of a project's
code.  Parts are always used with a SECTION from which they inherit timing
parameters.

## General Part Types

- QBALL
- STRUMMER
- SIMPLE-PART
- CONTROLLERS

## Specialized Part Types

- RAW-PART
- METRONOME
- PROGRAMS

### Transformations

Some part types are subject to transpose, key-inversion and retrograde
operations.  If a part's TRANSPOSABLE property is false, then it is not
effected by either the TRANSPOSE or INVERT methods.  If a parts
REVERSIBLE property is false, it is not effected by the RETROGRADE method.

### Part Repetitions

Each part is repeated as needed to fill the section duration.  IE a 4-bar
part in an 8-bar section is repeated twice.  If the part duration does nor
evenly divide the section, the final partial-part is skipped. <br>

Thing get more complex if the part's time-signature (other then bar-count)
does not match the section.  It is currently undefined how a part behaves
if it's time-signature does not match the sections.

### One Shot 

If a part's RENDER-ONCE property is true, then it *is not* repeated within
the section.  The part is played once only regardless of it's bar-count.

