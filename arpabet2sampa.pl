# Translates the CMU Dictionary's ARPAbet transcription to SAMPA, the Speech Assessment Methods Phonetic Alphabet,
# an ASCII-compliant phonetic alphabet
use v5.22;

my %phonemes = (
    "AO" => "O", # begin pure vowels
    "AA" => "A",
    "IY" => "i",
    "UW" => "u",
    "EH" => "E",
    "IH" => "I",
    "UH" => "U",
    "AH" => "V",
    "AX" => "@",
    "AE" => "{", # end pure vowels
    "EY" => "ej", # begin diphthongs
    "AY" => "Aj",
    "OW" => "oU",
    "AW" => "aU",
    "OY" => "Oj", # end diphthongs
    "ER" => "\@ r\\",
    "AXR" => "\@ r\\",
    "P" => "p", # begin stops
    "B" => "b",
    "T" => "t",
    "D" => "d",
    "K" => "k",
    "G" => "g", # end stops
    "CH" => "tS", # begin affricates
    "JH" => "dZ", # end affricates
    "F" => "f", # begin fricatives
    "V" => "v",
    "TH" => "T",
    "DH" => "D",
    "S" => "s",
    "Z" => "z",
    "SH" => "S",
    "ZH" => "Z",
    "HH" => "h", # end fricatives
    "M" => "m", #begin nasals
    "EM" => "m",
    "N" => "n",
    "EN" => "n",
    "NG" => "N",
    "ENG" => "N", # end nasals
    "L" => "l", # begin liquids
    "EL" => "l",
    "R" => "r\\",
    "DX" => "4", # alveolar tap
    "NX" => "4~", # end liquids
    "Y" => "j", # begin semivowels/glides
    "W" => "w",
    "Q" => "?",
    );
      
while (<>) {
    print join " ", map {$phonemes{$_} ? $phonemes{$_} : $_} split / /;
}
