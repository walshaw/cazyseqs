# CAZy access

## Purpose

To automatically download all of the sequence records belonging to a particular CAZy family.

The CAZy database home is http://www.cazy.org .

An example family page is http://www.cazy.org/GH95_all.html .

## More details of the CAZy website

The website does not appear to have a facility for bulk download of all of the sequence IDs
and/or the sequence records, for any given family.

Some of the families are large, with thousands of sequences, and furthermore these are not
all displayed on a single page (unless there are few enough). However, the rules about
how many are shown per page, seem a little inconsistent and confusing.

If a family is considered to consist of records, then each record represents a protein.

The fields of each record, shown as columns, are as follows; in effect, the descriptions
below supplement the information at http://www.cazy.org/Help.html , which does not
provide a full explanation of all of the content of the records.

* Protein Name

 In fact, this strings appear to be gene locus names. (See below for confirmation of
 this in one example). However, it appears that records which have a non-blank PDB/3D
 field tend to have a longer format name, where the locus name is in parentheses and
 preceded by a descriptive name of the enzyme (or other protein). See below for an
 example.

 Some of the values of this field appear strange - for example one of the records in
 GH95 simply has a Protein Name of 'ORF' (sic). The Organism and GenBank fields of
 this record appear normal (for reference, respectively "bacterium enrichment culture
 clone g13" and "AEP84398.1".

* EC#

 For some (many? all?) families, all the records have a blank EC# field. It is unclear
 to me why the EC# is record (protein) specific, rather than specific to an entire
 family; but this may depend on the depth of the classification used.

* Organism

 Appears to be always present (even if it is "uncultured organism" or "unidentified" etc).

 Note that the value is hyperlinked on the web page, __usually__ to a **CAZy** web 
 page pertaining to the organism. In some cases, it is a link to an NCBI taxonomy
 page (e.g. the oddly-named "ORF" record above").

* GenBank

 This usually seems to be present; specifies one or more identifiers. These are generally the
 IDs in the NCBI-GenBank protein database ('GenPept'). In some cases, there is an
 additional RefSeq identifier (at least, the format appears to be that, but in some
 cases these appear to be obsolete). Note that these secondary identifiers are **not**
 hyperlinked.

 Note though that the Help page (see above) states:

 > "Sequences issued from patents and other external information may have no
   corresponding GenBank (or nucleotide) code. Sequences deposited with no identification
   of coding sequence or issued from pseudogenes may have no corresponding PID."
 
 An example of a record
 with two identifiers is the one with Protein Name 'BT4682' in the
 GH95 family (see above for URL). The stated 'GenBank' values for this are 'AA078279.1'
 and 'NP_812085.1'. The former is linked to:

 https://www.ncbi.nlm.nih.gov/protein/AAO79787.1

 As mentioned, NP_812085.1 is not hyperlinked; but there is no sign of this ID in
 RefSeq. So it is uncertain whether such secondary IDs are never hyperlinked from the
 CAZy pages, or whether these are sometimes hyperlinked (if the RefSeq link still
 exists).

 Looking at the entry for AAO79787.1 (see URL above), it is evident that the current
 RefSeq ID corresponding to this protein is NP_813593.1 (as at 16th March 2018). The
 link provided there (at the NCBI page) to the RefSeq entry is
 https://www.ncbi.nlm.nih.gov/protein/29350090 .

 Also note for what it's worth, that in the AAO79787.1 record, the CDS feature has
 a value of 'BT_4682' for the locus_tag field - and a value of 'BT4682' for the
 old_locus_tag field.

 Note that there is no mention of these RefSeq-style IDs in http://www.cazy.org/Help.html .

* UniProt

 Usually absent, but it may depend on the family.

* PDB/3D

 Again this will often be absent for a given record. Where present, it specifies the
 4-letter PDB code followed immediately by the protein chain(s) to which this protein
 sequence applies, in brackets. Such strings are hyperlinks to the entry for the PDB
 code, at RCSB.

 One example in GH95 is the record with Protein Name BH0842 (so this is a 'simple format'
 protein name) - this has a PDB/3D value of '2RDY[A,B]'. A different example with
 a 'long format' Protein Name is "α-L-galactosidase (BACOVA_03438)", which has the
PDB/3D value '4UFC[A,B]'.

### Subsets for a given CAZy family

Note that the website provides a nice facility for viewing all the members of a family,
or members of a subset. Some subsets are taxonomically defined while others are
associated with whether or not there is a solved structure.

For the GH95 family at any rate, these subsets are 'Archaea', 'Bacteria', 'Eukaryota',
'unclassified', 'Structure', 'Characterized'. The superset is 'All', which is shown
by default. Each page is accessed via tabs which link to URLs of the form:

http://www.cazy.org/**familyID**_**setname**.html

e.g.

* http://www.cazy.org/GH95_all.html
* http://www.cazy.org/GH95_bacteria.html

__setname__ in this context is always in lower case (c.f. the strings on the tabs,
stated above).

Notably, the pagination is quite different for these different sets (see below).

### Pagination of each (sub)set

This refers to how the table of entries for a given family is displayed in terms
of how many pages the table is split over, and the number of rows per page.

The pagination is strange, as illustrated by the GH95 family. The 'All' set
consists of 1,392 entries, and is split over two pages; each page can be
accessed thus:

1. http://www.cazy.org/GH95_all.html#pagination_PRINC
2. http://www.cazy.org/GH95_all.html?debut_PRINC=1000#pagination_PRINC

(from this, one could guess that the first record on the page is number
1,000 in the list, which most logically would be the 1,001st entry -
assuming numbering begins at 0 - because this would result in 1,000 entries
on the first page and not 999).

Whereas the Bacteria subset consists (of course) of fewer entries: 1,343.
However, these are shown over 14 pages, from which it could be guessed that
there are 100 per page. The URLs are of a different form:

1. http://www.cazy.org/GH95_bacteria.html#pagination_TAXO
2. http://www.cazy.org/GH95_bacteria.html?debut_TAXO=100#pagination_TAXO
3. http://www.cazy.org/GH95_bacteria.html?debut_TAXO=200#pagination_TAXO
4. http://www.cazy.org/GH95_bacteria.html?debut_TAXO=300#pagination_TAXO

...etc.

Again, one assumes that numbering starts at 0, meaning that page one
would contain entries 0 .. 99, i.e. the first 100; page 2 100 .. 199,
etc.

It's hard to completely understand the URLs, because formulating one
from scratch, such as:

http://www.cazy.org/GH95_all.html?debut_TAXO=100#pagination_TAXO

results in a layout identical to the default for the All set, i.e. only
2 pages, with certainly more than 100 (presumably 1,000) per page.

(However, the above 'meddled with' URL does produce a different format
link in the tabs, e.g.
 http://www.cazy.org/GH95_all.html?debut_TAXO=100&debut_PRINC=1000#pagination_PRINC 
 -but it's best not to go there.)

Refer to the inline manual (./cazyseqs.pl -man) or see the MANUAL file,
for further information.

John Walshaw.



