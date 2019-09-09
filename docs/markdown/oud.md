# CYCO Oud Plugins

The OUD plugin is used to define chord-models based on fretted
instruments.   OUD is not used directly, insted it is loaded by other
plugins to define specific fretted instrument models. <br>

Plugins which use Oud include:

- **BASS-CHORDS** bass guitar exprots **\*BASS-CHORD-MODEL\*** and defines
  the follwoing cords.
  
  
    [SOLO]   - Single notes
    [OCT]    - Octaves
    [POWER]  - Root & 5th
    [POWER2] - Root, 5th & Octave
    [PER4]   - Perfect 4th
    [MAJ]    - Major
    [7]      - Dominate 7th
    [MIN]    - Minor

- **GUITAR-CHORDS**  exports **\*GUITAR-CHORD-MODEL\***


    [SOLO]      - Single notes            [7+5]       - Dom 7 sharp 5
    [OCT]       - Octaves                 [7+9]       - Dom 7 sharp 9
    [POWER2]    - Root & 5th              [7-9]       - Dom 7 flat 9
    [POWER]     - Root, 5th & octave      [7-SUS4]    - Dom 7 suspended 4th
    [MAJ]       - Major                   [9]         - Dom 9th
    [MAJ-SLASH] - Inverted major          [9-SUS4]    - Dom 9 suspended 4th
    [MAJ6]      - Major 6th               [11]        - Dom 11th
    [MAJ7]      - Major 7th               [13]        - Dom 13th
    [MAJ9]      - Major 9th               [AUG]       - Augmented
    [ADD9]      - Major add 9             [DIM7]      - Diminished 7th
    [MAJ11]     - Major 11th              [DIM]       - Diminished
    [MAJ13]     - Major 13th              [MIN]       - Minor
    [7]         - Dominate 7th            [MIN+7]     - Minor sharp-7
    [7+5+9]     - Dom 7 sharp 5 sharp 9   [MIN-ADD9]  - Minor add 9th
    [7+5-9]     - Dom 7 sharp 5 flat 9    [MIN-SLASH] - Inverted minor
    [MIN6]      - Minor 6th
    [MIN7]      - Minor 7th
    [MIN7-5]    - Minor 7th flat 5
    [SUS2]      - Suspend 2
    [SUS4]      - Suspend 4
	
- **MANDOLIN** exports **\*MANDOLIN-CHORD-MODEL\***


    [SOLO]   - Single note 
    [OCT]    - Octaves
    [POWER]  - Root & 5th
    [MAJ]    - Major
    [MAJ7]   - Major 7th
    [MAJ9]   - Major 9th
    [69]     - Dominate 6/9
    [6]      - Dominate 6th
    [7]      - Dominate 7th 
    [7+5]    - Dominate 7th sharp-5  
    [7-5]    - Dominate 7th flat-5
    [7-9]    - Dominate 7th flat-9 
    [9]      - Dominate 9th
    [DIM]    - Diminished
    [MIN]    - Minor
    [MIN6]   - Minor 6th
    [MIN7]   - Minor 7th
    [MIN+7]  - Minor sharp-7
    [MIN7-5] - Minor 7th flat-5
    [MIN9]   - Minor 9th
    [SUS4]   - Suspended 4th
	
- **UKULELE** exports **\*UKULELE-CHORD-MODEL\***


    [SOLO] - Single notes
    [MAJ]  - Major
    [MAJ7] - Major 7th
    [6]    - Dominate 6th
    [7]    - Dominate 7th
    [9]    - Dominate 9th
    [MIN]  - Minor
    [MIN7] - Minor 7th
    [AUG]  - Augmented
    [DIM]  - Diminished
