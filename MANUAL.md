
# cazyseqs.pl manual

The plain-text version of the manual below is what you get if you run the script
with the -man option.


## Usage:
./cazyseqs.pl [ options ] [ -cazyid ] CAZyID [ [ -outdir ] PAGESDIR ]

	-cazyid		STRING	Identifier of CAZy family ID, e.g. "GH95",
				"CBM40".
				This is case sensitive because it must match
				URL at the CAZy website, e.g.:
				http://www.cazy.org/GH33_all.html;
				(see -host and -suffix; and the explanation
				below of 'pagination'). There is no default.
				If -cazyid is omitted, then the first
				non-option argument will be used.

	-outdir		STRING	Output directory (default is '.').
				The web page(s) corresponding to the CAZy
				family specified will be expected to be present
				here, so if they are absent they will be
				downloaded here first (see -force).
				If -outdir is omitted, then the second
				non-option argument will be used.

	-category	STRING	The category of sequences whose IDs (and
				possibly sequence records too) to download.
				This is expected to be one of:
				'All', 'Archaea', 'Bacteria', 'Eukaryota',
				'Viruses', 'Structure', 'Characterized'.
				Default is 'All'.

	-get		STRING	Data to output: either 'id', 'url', or
				'sequence' (can be pluralised); determines the
				results sent to standard output, i.e. sequence
				ID strings only (as retrieved from the data
				column - see -columnindex); or URLs obtained
				from the same column (but see -urlprefix and
				-urlsuffix); or the sequence data (obtained by
				using the URLs).
				The names can also be abbreviated, e.g. 'seq'.
				Default is 'id'.
				N.B. the resulting data is *always sent to
				standard output*; so to save this in a file,
				please redirect (e.g. '> myfile.fasta' ). Note
				that other output (progress commentary etc.)
				is sent to a different stream (standard error)
				so to save that, redirect to a different file
				(e.g. '2> getseqs.log').

	-ncbiurls		Same as specifying:
				-urlprefix https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=protein&id=
				-urlsuffix '&rettype=fasta&retmode=text'
				(note the quotes around the suffix, because it
				contains '&' characters).
				The underlying API used is the Entrez E-utils;
				in this case, using Efetch; see:
				https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch

	-force			If specified, then an attempt will always be
				made to download the webpages to the output dir
				(see -outdir). If -force is not used, then the
				download of each page will be done only if the
				page is absent from there.

        -verbosity	INTEGER	Verbosity level; larger values produce more
				verbose output. Default 0.

	-usage			Show brief usage summary, then exit.

	-help			Show list of options, then exit.

	-man			Show more detailed manual, then exit.

## Advanced options:

	-tableindex	INTEGER	The number of the table to parse from the web
				pages. The pages appear to have more than one
				HTML table encoded (for formatting purposes)
				but only one contains the data of interest.
				Default is table 2.

	-ignoreclass	STRING	In a non-taxonomic specific view (such as 'All'
				or 'Structure') a header ("title") row is
				present for each taxon (e.g. one each for the
				Bacteria, Archaea and Eukaryota entries).
				The 'class' row (not Class in the taxonomy
				sense) states the name of the taxon, i.e.
				Kingdom. Therefore these rows are identified
				in the HTML by a 'class' attribute, which has
				the value 'royaume'.
				All rows with a class matching this value will
				be ignored.
				(Note that the CAZy data tables do not appear
				to use table headings in the HTML sense, i.e.
				there are no TH tags used.)

	-ignoretitle	STRING	In a non-taxonomic specific view (such as 'All'
				or 'Structure') a header ("title") row is
				present for each taxon (e.g. one each for the
				Bacteria, Archaea and Eukaryota entries).
				The title row itself does not contain the name
				of the taxon, but the row headings
				("Protein Name", "EC#", "Organism" etc).
				These are identified in the HTML by an 'id'
				attribute, which has the value 'line_titre'.
				This can be changed by using -ignoretitle.
				All rows with an ID matching this will be
				ignored.
				(Note that the CAZy data tables do not appear
				to use table headings in the HTML sense, i.e.
				there are no TH tags used.)

	-ignorerows	INTEGER	In the table parsed (see -tableindex), ignore
				this number of rows, i.e. treat them as the
				heading, not data. Default is 0.
				It is probably best not to use this unless
				you have a specific reason. Use -ignoretitle
				instead.
				If non-zero, the value of -ignorerows will be
				applied before any check for title rows (see
				-ignoretitle). So if a value of 1 is used,
				and the first row is a title row, then only
				the title row will be ignored.

	-columnindex	INTEGER	The number of the column from which to extract
				data. Default is column 4. If you
				change this, you are advised to not use
				-ncbiurls (see above).

	-urlprefix	STRING	If '-get url' is specified then by default,
				the URLs stated explicitly in the data table
				will be used. However, use of -urlprefix and/
				or -urlsuffix can override that, and the
				strings specified will be prepended/appended
				to the sequence ID string, to form the URL in
				each case. This can cause *more* URLs to be
				generated, because the number of explicit
				URLs in the data tables may be fewer than the
				number of sequence IDs listed, possibly due to
				redundancy.
				See also -ncbiurls, above.

	-urlsuffix	STRING	See -urlprefix, above.
				

	-browser	STRING	This should be the full command-line prefix of
				the command to fetch each page. A single
				argument is appended, i.e. the URL of each
				page. Default is wget -O -.
				So for example, a command-line would be:
				wget -O - ./GH33_all.html

	-wwwhost	STRING	Web host name. Default is www.cazy.org

	-suffix		STRING	Suffix to append to CAZy family ID, to form
				name of (first) web page (see 'pagination'
				below). Default is _all.html .
				Therefore, an example page name is GH33_all.html

	-pagesize	INTEGER	This appears to be the maximum number of rows
				(sequence records) displayed on one page (in
				'all' context; for the Kingdom-level data,
				(e.g. 'Bacteria' context), this number appears
				to be much smaller (i.e. 100).
				The pagesize number appears to be the value of
				the 'debut_PRINC' variable in the HTTP GET
				string. However, it does not seem certain that
				varying this to a non-multiple of 1000 would
				provide the expected effect. Therefore, this
				should not be changed unless you are certain
				of the result.
				The specification of the different pages of
				data (where the total number of rows i.e.
				sequences is > 1000) simply uses
				'debut_PRINC=<x>' where <x> is a whole
				multiple of 1000 . This seems to
				work correctly.

	-context	STRING	The CAZy website appears to display the tables
				of data in either a 'taxonomic' context (where
				the entries for a particular Kingdom are
				shown), or the different context where all
				entries are shown. These contexts are
				respectively the 'TAXO' and 'PRINC' contexts.
				Default is PRINC . These also seem to
				correspond to different numbers of records per
				page (respectively 100 and 1000).

	-pause		INTEGER	Refer to 'Fair usage guidelines' below. This
				is relevant only to '-get sequence'.
				Pause in seconds which occurs after each
				sequence record has been downloaded.
				Default: 1 

	-bigpause	INTEGER	Refer to 'Fair usage guidelines' below. This
				is relevant only to '-get sequence'.
				Pause in seconds which occurs after each batch
				of sequence records has been downloaded.
				Default: 300

	-batchsize	INTEGER	Refer to 'Fair usage guidelines' below. This
				is relevant only to '-get sequence'.
				Number of sequence records per batch.
				Default: 100


## Pagination

The tabulated data for each family has a 'front page', e.g. http://www.cazy.org/GH33_all.html 
(more correctly referred to as the 'Summary' page, because there is a link of
this name on the various pages which points to this page).

On the front page, no rows of sequence record data are shown. A table is
displayed which contains data about the CAZy family itself. There are links
to pages which show different categories of records, such as all of them
('All'); and taxonomic categories ('Archaea', 'Bacteria', 'Eukaryota',
'Viruses'). The taxonomic categories collectively cover all of the entries
in 'All'. Further sub-categories, which tend to represent far smaller subsets,
are 'Structure' and 'Characterized'.

The 'All', 'Structure' and 'Characterized' lists therefore can contain entries
from multiple taxonomic categories. In these lists, a 'heading' precedes the
entries of each of these major taxa. These headings basically consist of 2
rows (in the HTML sense) - the first row belongs to the class (as in the
stylesheet sense) named `"royaume"`, while the second is not assigned to a
stylesheet class, but has an `"id"` attribute, which has the value "line_titre"
(conversely, the "royaume" row has no id).

Therefore, rows with `class="royaume"` and `id="line_titre"` are identified as
non-data rows, and so are (by default; see `-ignoreclass` and `-ignoretitle`
above) ignored.

Many of the 'All' or sub-category listings have too many entries (rows) to
sensibly depict on one web page, and so these are split into numbered pages
(each in fact corresponds to a distinct web page).

It seems that the number of rows shown per page is quite different, depending
on the category viewed. For the 'All' listing, it is 1,000. For the taxonomic
categories, it is 100.

These two styles appear to be specified in the URL by the
`'#pagination_<style>'` clause, where `<style>` is either `'PRINC'` (for 'All') and
`'TAXO'` (for the taxonomic categories). This can be specified by using the
`-context` directive for this script.

By default, the script assumes 'All' is required, and so the value of
`-pagesize` is by default 1000. This can be changed (e.g. to 100, if the style
is TAXO) by using `-pagesize.`


## Fair usage guidelines

The principal protein IDs/Accessions used by CAZy are those of the NCBI
protein database. By default, this script will use those IDs (see
`-columnindex`), and so will either extract those IDs from the CAZy web pages
(`-get id`); *or* extract the original URLs (`-get url` , as long as *none* of
`-urlprefix`, `-urlsuffix` or `-ncbiurls` are used); or, if `-get url` and any of
`-urlprefix`, `-urlsuffix` or `-ncbiurls` *are* used, then a URL will be constructed
for each sequence ID. In all these cases, only the CAZy website will be
interrogated (or simply the local copies of the relevant pages, if they
exist and `-force` is not used).

In contrast, `-get seq` will interrogate a third-party website, to retrieve the
sequence records. If there are thousands of sequence records to retrieve, then
the immediate downloading of all of these serially would risk breaching the
fair usage guidelines (of the NCBI for example, but it depends on `-columnindex`,
`-urlprefix`, `-urlsuffix` and `-ncbiurls` - you can direct the script to retrieve
them from somewhere else if available, for example a local copy of the protein
database, if one exists).

Please refer to **"Guidelines for Scripting Calls to NCBI Servers"** at
https://www.ncbi.nlm.nih.gov/home/about/policies/

Also refer to **"Frequency, Timing and Registration of E-utility URL Requests"**
at https://www.ncbi.nlm.nih.gov/books/NBK25497/#_chapter2_Usage_Guidelines_and_Requiremen_
-an extract follows:

>	"In order not to overload the E-utility servers, NCBI recommends that
	 users post no more than three URL requests per second and limit large
	 jobs to either weekends or between 9:00 PM and 5:00 AM Eastern time
	 during weekdays. Failure to comply with this policy may result in an
	 IP address being blocked from accessing NCBI. If NCBI blocks an IP
	 address, service will not be restored unless the developers of the
	 software accessing the E-utilities register values of the tool and
	 email parameters with NCBI."

etc.

**PLEASE NOTE THE ABOVE WARNING ABOUT IP ADDRESSES BEING BLOCKED!**

The script will help to make any large downloads more "polite". Of course, an
alternative is to simply extract the IDs instead (`-get id`, i.e. the default)
and then use these as input to batch Entrez retrieval
(https://www.ncbi.nlm.nih.gov/sites/batchentrez).



