# Phonologize

## Author
Daniel J. Rothblatt, April-May 2016

## Description
This project simulates the phonological portion of the grammar of
human language, the portion of the grammar that governs the
interaction of speech sounds (or the equivalent in a sign
language).
Phonology is a context-sensitive language--which surface form (allophone) a
phoneme takes is dependent upon its neighbors. We use a rule-based
(Sound Pattern of English) approach, representing phonological rules
as pattern-matching functions on symbols. We generate arbitrary
pattern-matching functions using the _rule_ macro.
Currently Phonologize reads in the CMU Dictionary to have access to a
large corpus of words in transcription. (Since this is the CMU
Dictionary, the transcription is ARPAbet, though any transcription
will do.)

## Purpose
phonologize.rkt is useful in testing a list of phonological rules
against the data of a particular language. (If that language is not
English, it will be necessary to use a different transcription from
ARPAbet, which is inadequate even for representing the surface forms
of many dialects of English anyway.)
Another potential use of phonologize.rkt is to couple it with a
text-to-speech program--if the TTS can read the transcription given,
Phonologize can produce surface forms that will sound natural to
listeners, provided the list of rules is correct.

## Use
Run phonologize.rkt in any Racket REPL (such as is provided in
DrRacket). Use sentence->phonemes to turn a list of words in
\*dictionary\* into a list of phonemes. sentence->surface-form turns a
list of words in \*dictionary\* into a list of phonemes and additionally
applies \*rules\* to transform the list into a list of allophones.
Redefine \*rules\* to contain whatever rules are required in
your grammar. \*dictionary\* reads in from a CMU Dictionary that has had
stress indicators stripped away, so another file will be needed if
stress is important to you.

## To Do
1. Phonological rules work at a lower level than simple symbol
manipulation: A phoneme is actually a bundle of features (e.g., vowel
height and frontness, consonant voicing and place of articulation). By
treating phonemes as symbols, we lose access to a large class of rules
that works on classes of phonemes rather than single phonemes. The
effect of this is that the rules we can write are tedious, repetitive
and error-prone--we have to write basically the same rule multiple
times to capture its specific cases. This makes our grammar more
space-intensive, more complicated, and much harder to understand.
The solution is to implement phonemes as bundles of features--structs!
I (Daniel Rothblatt) am working on this solution now.

2. ARPAbet is an extremely flawed phonetic alphabet. First of all, it
only represents the phonemes of one language, English. Second, it only
represents *phonemes*, which is problematic if one wishes to do
phonology with a system that can capture precise phonetic details. The
solution is to translate ARPAbet to SAMPA, IPA, or some other, more
complete phonetic alphabet, either when reading in the CMU Dictionary,
or before, by transforming the text itself to such an alphabet. A
function to do the former will be added to phonologize.rkt, and a
script to do the latter will be added to the project folder, along
with a translated version of cmu_dictionary_stressless.txt.
