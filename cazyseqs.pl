#!/usr/bin/perl

# John Walshaw, March 2018.

use strict;
use warnings;
use autodie;

use Carp;
use Data::Dumper;
use Getopt::Long;
use List::Util qw( first );
use HTML::Parser;

my $cazy_id; # mandatory user-specified

my $host = qq{www.cazy.org};
my $ext = qq{.html}; # currently *not* user-specifiable (should be, really)
my $category = qq{All};

# 'default' because $category can be user-respecified:
my $default_category_lc = lc $category;
my $default_suffix = qq{_}.$default_category_lc.$ext;
my $suffix; # can be directly user-specified; but if not, then a default value
            # is derived from $category (which can also be user-respecified)

my @expected_categories = ($category, # this WON'T be user-respecified
  qw(Archaea Bacteria Eukaryota Viruses Structure Characterized unclassified));

my $browser = qq{wget -O -}; # alternative might be e.g. qq{lynx -source -dump -width=1024};

my $records_per_page = 1000; # seems to be the default for the CAZy website's
                             # 'PRINC' format, which is what you get by
                             # default in the <CAZyID>_all.html page
my $out_pages_dir = q{.}; # copies are kept of the web pages; this
                                   # seems sensible for debugging; so they are
                                   # not read on the fly
my $clobber;
my $page_context = q{PRINC}; # 'TAXO' is the only other option I'm aware of
			     # (which really requires $records_per_page to be
			     # 100)

my $use_table   = 2; # table-numbering in this context starts at 1!
my $title_row_id = q{line_titre};
my $title_row_class = q{royaume};
my $ignore_rows = 0; # specifies N: ignore the first N rows of that table
# This specifies the NCBI protein IDs column:
my $use_column  = 4; # column-numbering also starts at 1 in this context

my @retrieval_types = qw( id url sequence );
my $retrieve = q{id};

my $url_prefix = q{};
my $url_suffix = q{};
my $use_ncbi_urls; # boolean; mutually exclusive wrt above 2 vars
# these don't work... (they do using a GUI browser though)
#my $ncbi_prefix = q{https://www.ncbi.nlm.nih.gov/protein/};
#my $ncbi_suffix = q{?report=fasta&log$=seqview&format=text};
my $ncbi_prefix = q{https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=protein&id=};
my $ncbi_suffix = q{&rettype=fasta&retmode=text};

# see https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch

=pod
    these are for specifying the 'header' part of the CAZy family page,
    which states the number of sequence records in each category; note that
    these are not in an HTML table, but within a <SPAN>..</SPAN> which has a
    "class" attribute, whose value is "choix"; there is one such span per
    category (All, Bacteria, Structure, etc).
=cut

my $stats_tag         = q{span};
my $stats_attr_name   = q{class};
my $stats_attr_value  = q{choix};

# specify the tags to look for when parsing the data tables - it is possible
# to be too absracted....
my $table_tag  = q{table};
my $row_tag    = q{tr};
my $cell_tag = q{td}; # note that <TH></TH> tags are thus ignored as far as
                        # data parsing s concerned; but they are noted:
my $heading_tag = q{th};
my $anchor_tag  = q{a}; # ... yes, too abstracted...

my @data_associated_tags = ($table_tag  , $row_tag, $cell_tag,
                            $heading_tag, $anchor_tag);

# along the same lines as $stats_attr_name, $stats_attr_value
my $table_attr_name  = q{id};
my $table_attr_value = q{pos_onglet};

=pod
    Note that the column required is specified by number (see $use_column
    above), rather than header name; header name would indeed be possible to
    use to determine the correct number, as long as there are no colspan
    attributes, and indeed there are not in these CAZy pages, e.g.:

    <tr id="line_titre"><td>Protein Name</td> <td>EC#</td><td>Organism</td><td>GenBank</td>
                        <td>Uniprot</td><td>PDB/3D</td><td></td></tr>

    note that such a line is ignored due to the value of the "id" attribute
    matching $title_row_id, i.e. 'line_titre'
=cut

# counters of which table, row, column we are in
my $table_index  = 0;
my $row_index    = 0;
my $column_index = 0;

my @content_ids;  # the IDs harvested from the table data
my @content_urls; # the URLs *harvested from the table data*; there is not
		  # necessarily one explicit URL per explicit ID.

# These relate to 'sociability' regarding use of the NCBI Entrez resource
my $pause_between_items   =   1; # sleep time in seconds
my $pause_between_batches = 300; # ditto; but only after each batch has downloaded
my $batch_size            = 100; # number of items
# 'items' means sequence records

my $verbosity = 0;
my $show_usage;
my $show_help;
my $show_manual;

my $example_web_page = qq{GH33$default_suffix};
my $example_url = qq{http://$host/$example_web_page};

my $usage = qq{Usage:\n$0 [ options ] [ -cazyid ] CAZyID [ PAGESDIR ]};
my $usage_plus = qq{$usage\n(use -help for brief help or -man for manual)\n};

my $help = qq{$usage

	-cazyid		STRING
	-outdir		STRING
        -category	STRING
        -get		STRING 
	-ncbiurls
	-force
        -verbosity	INTEGER
	-usage
	-help
	-man

Advanced options:

	-tableindex	INTEGER
	-ignoreclass	STRING
	-ignoretitle	STRING
	-ignorerows	INTEGER
	-columnindex	INTEGER
	-urlprefix	STRING
	-urlsuffix	STRING
	-browser	STRING
	-wwwhost	STRING
	-suffix		STRING
	-pagesize	INTEGER
	-context	STRING
	-pause		INTEGER
	-bigpause	INTEGER
	-batchsize	INTEGER

};

my $man = qq{$usage

	-cazyid		STRING	Identifier of CAZy family ID, e.g. "GH95",
				"CBM40".
				This is case sensitive because it must match
				URL at the CAZy website, e.g.:
				$example_url;
				(see -host and -suffix; and the explanation
				below of 'pagination').
				If -cazyid is omitted, then the first
				non-option argument will be used.

	-outdir		STRING	Output directory (default is '$out_pages_dir').
				The web page(s) corresponding to the CAZy
				family specified will be expected to be present
				here, so if they are absent they will be
				downloaded here first (see -force).

	-category	STRING	The category of sequences whose IDs (and
				possibly sequence records too) to download.
				This is expected to be one of:
				'All', 'Archaea', 'Bacteria', 'Eukaryota',
				'Viruses', 'Structure', 'Characterized'.
				Default is '$category'.

	-get		STRING	Data to output: either 'id', 'url', or
				'sequence' (can be pluralised); determines the
				results sent to standard output, i.e. sequence
				ID strings only (as retrieved from the data
				column - see -columnindex); or URLs obtained
				from the same column (but see -urlprefix and
				-urlsuffix); or the sequence data (obtained by
				using the URLs).
				The names can also be abbreviated, e.g. 'seq'.
				Default is '$retrieve'.
				N.B. the resulting data is *always sent to
				standard output*; so to save this in a file,
				please redirect (e.g. '> myfile.fasta' ). Note
				that other output (progress commentary etc.)
				is sent to a different stream (standard error)
				so to save that, redirect to a different file
				(e.g. '2> getseqs.log').

	-ncbiurls		Same as specifying:
				-urlprefix $ncbi_prefix
				-urlsuffix '$ncbi_suffix'
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
				verbose output. Default $verbosity.

	-usage			Show brief usage summary, then exit.

	-help			Show list of options, then exit.

	-man			Show more detailed manual, then exit.

Advanced options:

	-tableindex	INTEGER	The number of the table to parse from the web
				pages. The pages appear to have more than one
				HTML table encoded (for formatting purposes)
				but only one contains the data of interest.
				Default is table $use_table.

	-ignoreclass	STRING	In a non-taxonomic specific view (such as 'All'
				or 'Structure') a header ("title") row is
				present for each taxon (e.g. one each for the
				Bacteria, Archaea and Eukaryota entries).
				The 'class' row (not Class in the taxonomy
				sense) states the name of the taxon, i.e.
				Kingdom. Therefore these rows are identified
				in the HTML by a 'class' attribute, which has
				the value '$title_row_class'.
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
				attribute, which has the value '$title_row_id'.
				This can be changed by using -ignoretitle.
				All rows with an ID matching this will be
				ignored.
				(Note that the CAZy data tables do not appear
				to use table headings in the HTML sense, i.e.
				there are no TH tags used.)

	-ignorerows	INTEGER	In the table parsed (see -tableindex), ignore
				this number of rows, i.e. treat them as the
				heading, not data. Default is $ignore_rows.
				It is probably best not to use this unless
				you have a specific reason. Use -ignoretitle
				instead.
				If non-zero, the value of -ignorerows will be
				applied before any check for title rows (see
				-ignoretitle). So if a value of 1 is used,
				and the first row is a title row, then only
				the title row will be ignored.

	-columnindex	INTEGER	The number of the column from which to extract
				data. Default is column $use_column. If you
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
				page. Default is $browser.
				So for example, a command-line would be:
				$browser $out_pages_dir/$example_web_page

	-wwwhost	STRING	Web host name. Default is $host

	-suffix		STRING	Suffix to append to CAZy family ID, to form
				name of (first) web page (see 'pagination'
				below). Default is $default_suffix .
				Therefore, an example page name is $example_web_page

	-pagesize	INTEGER	This appears to be the maximum number of rows
				(sequence records) displayed on one page (in
				'all' context; for the Kingdom-level data,
				(e.g. 'Bacteria' context), this number appears
				to be much smaller (i.e. 100).
				The pagesize number appears to be the value of
				the 'debut_$page_context' variable in the HTTP GET
				string. However, it does not seem certain that
				varying this to a non-multiple of $records_per_page would
				provide the expected effect. Therefore, this
				should not be changed unless you are certain
				of the result.
				The specification of the different pages of
				data (where the total number of rows i.e.
				sequences is > $records_per_page) simply uses
				'debut_$page_context=<x>' where <x> is a whole
				multiple of $records_per_page . This seems to
				work correctly.

	-context	STRING	The CAZy website appears to display the tables
				of data in either a 'taxonomic' context (where
				the entries for a particular Kingdom are
				shown), or the different context where all
				entries are shown. These contexts are
				respectively the 'TAXO' and 'PRINC' contexts.
				Default is $page_context . These also seem to
				correspond to different numbers of records per
				page (respectively 100 and 1000).

	-pause		INTEGER	Refer to 'Fair usage guidelines' below. This
				is relevant only to '-get sequence'.
				Pause in seconds which occurs after each
				sequence record has been downloaded.
				Default: $pause_between_items 

	-bigpause	INTEGER	Refer to 'Fair usage guidelines' below. This
				is relevant only to '-get sequence'.
				Pause in seconds which occurs after each batch
				of sequence records has been downloaded.
				Default: $pause_between_batches

	-batchsize	INTEGER	Refer to 'Fair usage guidelines' below. This
				is relevant only to '-get sequence'.
				Number of sequence records per batch.
				Default: $batch_size


Pagination

The tabulated data for each family has a 'front page', e.g. $example_url 
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
stylesheet sense) named "royaume", while the second is not assigned to a
stylesheet class, but has an 'id' attribute, which has the value "line_titre"
(conversely, the "royaume" row has no id).

Therefore, rows with class="royaume" and id="line_titre" are identified as
non-data rows, and so are (by default; see -ignoreclass and -ignoretitle
above) ignored.

Many of the 'All' or sub-category listings have too many entries (rows) to
sensibly depict on one web page, and so these are split into numbered pages
(each in fact corresponds to a distinct web page).

It seems that the number of rows shown per page is quite different, depending
on the category viewed. For the 'All' listing, it is 1,000. For the taxonomic
categories, it is 100.

These two styles appear to be specified in the URL by the
'#pagination_<style>' clause, where <style> is either 'PRINC' (for 'All') and
'TAXO' (for the taxonomic categories). This can be specified by using the
-context directive for this script.

By default, the script assumes 'All' is required, and so the value of
-pagesize is by default 1000. This can be changed (e.g. to 100, if the style
is TAXO) by using -pagesize.


Fair usage guidelines

The principal protein IDs/Accessions used by CAZy are those of the NCBI
protein database. By default, this script will use those IDs (see
-columnindex), and so will either extract those IDs from the CAZy web pages
(-get id); *or* extract the original URLs (-get url , as long as none of
-urlprefix, -urlsuffix or -ncbiurls are used); or, if -get url and any of
-urlprefix, -urlsuffix or -ncbiurls *are* used, then a URL will be constructed
for each sequence ID. In all these cases, only the CAZy website will be
interrogated (or simply the local copies of the relevant pages, if they
exist and -force is not used).

In contrast, -get seq will interrogate a third-party website, to retrieve the
sequence records. If there are thousands of sequence records to retrieve, then
the immediate downloading of all of these serially would risk breaching the
fair usage guidelines (of the NCBI for example, but it depends on -columnindex,
-urlprefix, -urlsuffix and -ncbiurls - you can direct the script to retrieve
them from somewhere else if available, for example a local copy of the protein
database, if one exists).

Please refer to "Guidelines for Scripting Calls to NCBI Servers" at
https://www.ncbi.nlm.nih.gov/home/about/policies/

Also refer to "Frequency, Timing and Registration of E-utility URL Requests"
at https://www.ncbi.nlm.nih.gov/books/NBK25497/#_chapter2_Usage_Guidelines_and_Requiremen_
- an extract follows:

	"In order not to overload the E-utility servers, NCBI recommends that
	 users post no more than three URL requests per second and limit large
	 jobs to either weekends or between 9:00 PM and 5:00 AM Eastern time
	 during weekdays. Failure to comply with this policy may result in an
	 IP address being blocked from accessing NCBI. If NCBI blocks an IP
	 address, service will not be restored unless the developers of the
	 software accessing the E-utilities register values of the tool and
	 email parameters with NCBI."

etc.

*** PLEASE NOTE THE ABOVE WARNING ABOUT IP ADDRESSES BEING BLOCKED! ***

The script will help to make any large downloads more "polite". Of course, an
alternative is to simply extract the IDs instead (-get id, i.e. the default)
and then use these as input to batch Entrez retrieval
(https://www.ncbi.nlm.nih.gov/sites/batchentrez).

See 

};


croak "$usage_plus" if !GetOptions(
    "cazyid=s"		=> \$cazy_id         ,
    "outdir=s"		=> \$out_pages_dir   ,
    "category=s"	=> \$category        ,
    "get=s"             => \$retrieve        ,
    "force"		=> \$clobber         ,
    "verbosity=i"	=> \$verbosity       ,
    "usage"		=> \$show_usage      ,
    "help"		=> \$show_help       ,
    "man"		=> \$show_manual     ,
    "tableindex=i"	=> \$use_table       ,
    "ignoreclass=s"	=> \$title_row_id    ,
    "ignoretitle=s"	=> \$title_row_class ,
    "ignorerows=i"	=> \$ignore_rows     ,
    "columnindex=i"	=> \$use_column      ,
    "urlprefix=s"	=> \$url_prefix      ,
    "urlsuffix=s"	=> \$url_suffix      ,
    "ncbiurls"		=> \$use_ncbi_urls   ,
    "browser=s"		=> \$browser         ,
    "wwwhost=s"		=> \$host            ,
    "suffix=s"		=> \$suffix          ,
    "pagesize=i"	=> \$records_per_page,
    "context=s"		=> \$page_context    ,

);


$show_manual && do { print $man       ; exit 0; };
$show_help   && do { print $help      ; exit 0; };
$show_usage  && do { print $usage_plus; exit 0; };

$cazy_id ||= shift or croak $usage_plus;

my $extra = shift;

$out_pages_dir = $extra if $extra;

my $capital   = uc(substr $category, 0, 1);
my $remainder = lc(substr $category, 1   );

if (($category =~ s{ \A [a-z] .* \z }{${capital}$remainder}xms) ||
    ($category =~ s{ \A . .*[A-Z].* \z }{${capital}$remainder}xms)) {
    print STDERR qq{WARNING: modified case of category name: is now '$category'\n};
}

print STDERR qq{WARNING: '$category' is not a known category - so this will probably fail\n}
  if !first { $_ eq $category } @expected_categories;

(my $retrieval = lc $retrieve) =~ s{ s \z }{}xms;

# so for example, 'seqs' is ok because 'sequences' =~ m{ \A seq }

my $retrieve_type = first { m{ \A $retrieval }xms } @retrieval_types;

croak qq{unrecognized data retrieval type '$retrieve'}
    if !defined $retrieve_type;

croak qq{Use either: one or both of -urlprefix, -urlsuffix; OR -ncbiurls; not both.}
    if $use_ncbi_urls && ($url_prefix || $url_suffix);

if ($use_ncbi_urls) {
    $url_prefix = $ncbi_prefix;
    $url_suffix = $ncbi_suffix;
}

# Note that $category may now have a non-default value, due to user-spec
my $category_lc = lc $category;
# $suffix can be specified *directly* by the user; but if it hasn't, then
# it is derived from the user-selected (or default) category
$suffix ||= qq{_}.$category_lc.$ext;

my $url_root       = qq{http://$host/${cazy_id}};

my $summary_url    = $url_root.$ext;

my $summary_page   = $cazy_id.$ext; # just the filename

my $category_url0  = $url_root.$suffix; # full URL of 1st page of req'd category

my $category_page0 = ${cazy_id}.q{_}.$category_lc.$ext;

print STDERR qq{Summary of pagination parameters - check the manual (use -man) if you are unsure:

	category:		$category
	records per page:	$records_per_page
	pagination style:	$page_context
	URL of first page:	$category_url0
	URL of Summary page:	$summary_url


};


=pod

    These globals are used by several subroutines, including get_cat_stats() and
    the HTML::Parser event-handlers, which are called via HTML::Parser->parse()
    within get_cat_stats().

=cut

my $get_text; # boolean
my @text_items;

my $in_data_table; # boolean
my $in_data_rows;  # boolean
my $in_data_columns; # boolean; plural is used, to signal possible future
                     # expansion where > 1 column is a 'data column'

=pod

    Fetch/read the *Summary* page, to get the stats on the number of records
    in each category (this information is also present on the category-
    specific pages too, but they can be quite large; so avoid unnecessary
    download)

=cut


my $seqs_per_category_href = get_cat_stats(
    url         => $summary_url     ,
    tag         => $stats_tag       ,
    attr        => $stats_attr_name ,
    val         => $stats_attr_value,
    get_command => $browser         ,
    outfile     => $summary_page    ,
    outdir      => $out_pages_dir   ,
    errfile     => qq{$summary_page.stderr},
    clobber     => $clobber         ,
);

=pod

    Sanity-check the resulting per-category stats, and display a summary

=cut

my @unretrieved_cat;

for my $cat (@expected_categories) {

    if (!exists $seqs_per_category_href->{$cat}) {
        print STDERR qq{NO SEQUENCE TOTAL RETRIEVED FOR CATEGORY '$cat'\n};
        push @unretrieved_cat, $cat;
        next;
    }

    printf STDERR qq{category %-20s : %6d sequence}, $cat, $seqs_per_category_href->{$cat};
    print STDERR qq{}, ($seqs_per_category_href->{$cat} == 1) ? qq{} : qq{s}, qq{\n};
}

my @unexpected_cat = grep {
    my $rcat = $_;
    !first { $_ eq $rcat } @expected_categories
} (keys %{$seqs_per_category_href});

if (@unexpected_cat) {

    print STDERR qq{THESE UNEXPECTED CATEGORIES WERE FOUND IN THE WEB PAGE:\n\t},
        join(qq{\n\t}, @unexpected_cat), qq{\n\n};

}

print Dumper $seqs_per_category_href if $verbosity > 2;

parse_pages(
        first_url        => $category_url0   ,
        first_page       => $category_page0  ,
        page_context     => $page_context    ,
        records_per_page => $records_per_page,
        total_records    => $seqs_per_category_href->{$category},
        outdir           => $out_pages_dir   ,
        clobber          => $clobber         ,
        get_command      => $browser         ,
);

output_results(
    sequence_ids  => \@content_ids         ,
    original_urls => \@content_urls        ,
    url_prefix    => $url_prefix           ,
    retrieve_type => $retrieve_type        ,
    batch_size    => $batch_size           ,
    pause_records => $pause_between_items  ,
    pause_batches => $pause_between_batches,
);

exit 0;

sub fetch_read_write {

=pod

    Note the logic:

    outfile is 	outfile	clobber	read action		write action
    specified   already
		exists

	F	F	F	fetch remote content	none
	F	F	T	fetch remote content	none
	F	T	F	fetch remote content*	none
	F	T	T	fetch remote content	none
(1)	T	F	F	fetch remote content	create local copy
	T	F	T	fetch remote content	create local copy
(2)	T	T	F	read local content	none
	T	T	T	fetch remote content	(re-)create local copy

*(because outfile name is unspecified)

(1) and (2) are likely to be the most common scenarios; no clobbering is done,
and so if a copy of the web page already exists locally, then read it; otherwise
read the remote content, and write that content to a local copy

=cut

    my %arg = @_;

    my $local_file = $arg{local_file};
    my $clobber    = $arg{clobber};

    my $open_expr;
    my $mode;
    my $ofh;

    if ($local_file && (-f $local_file) && !$clobber) {
        print STDERR qq{local copy ($local_file) already exists; will not fetch new copy\n};
        $mode = '<';
        $open_expr = $local_file;
    }
    else {
        $mode = '-|';
        $open_expr  = qq{$arg{get_command} $arg{url}};
        $open_expr .= qq{ 2> $arg{errfile}} if $arg{errfile};
    }

    if ($local_file && ((!-f $local_file) || $clobber)) {
        print STDERR qq{creating local copy ($local_file)},
            (-f $local_file) ? qq{ - OVERWRITING EXISTING COPY} : qq{}, qq{\n};
        open $ofh, '>', $local_file;
    }

    return ($open_expr, $mode, $ofh);
}


sub get_cat_stats {

    my %arg = @_;

    my $parser = HTML::Parser->new( api_version => 3,
                                    start_h => [\&start_hsectn, "tagname, attr"],
                                    end_h   => [\&end_hsectn  , "tagname"      ],
                                    text_h  => [\&text_hsectn , "dtext"        ],
                                    marked_sections => 1,
                              );

    my ($local_file, $clobber) = @arg{'outfile','clobber'};

    $local_file = qq{$arg{outdir}/$arg{outfile}}
      if $local_file && $arg{outdir};


    my ($open_expr, $mode, $ofh) = fetch_read_write(
        local_file  => $local_file      , clobber => $clobber ,
        get_command => $arg{get_command}, url     => $arg{url},
    );

=pod
    Read the HTML from STDOUT and conditionally capture STDERR;
    note that the command is *assumed to deliver the HTML
    content to STDOUT*. This may seem counterintuitive if using wget, which
    would by default write it to a file (name determined by URL). But the
    way it's done here is more flexible (a completely different command
    could be substituted for the wget...., e.g. lynx -dump -source).
    Also, this gives the option of retaining the Summary web page (if
    $arg{outfile} is defined) or not.
=cut

    print STDERR qq{opening: $open_expr\n};
    open(my $fh, $mode, $open_expr);
    # should be easily small enough to be slurp-safe
    #my @summary_content = <$fh>;

    # Note that GTHML::Parser->parse() won't play ball with this form:
    # $parser->parse(<$fh>); so read line-by-line

    while (defined (my $line = <$fh>)) {
        #print qq{LINE: $line};
        $parser->parse($line);
        print $ofh $line if $ofh;
    }

    close $fh;
    close $ofh if $ofh;
    $parser->eof();

    print STDERR qq{closing\n};

    # sanity check
    croak qq{Unexpected category-stats content:\n\t}
          .join(qq{,\n\t}, @text_items)
          .qq{\n}
      if (@text_items % 2);

    my %cat_frequency = @text_items;
    for my $tally (values %cat_frequency) {
        $tally =~ s{ (?: \A \s* [(] \s* | \s* [)] \s* \z ) }{}gxms;
    }

    return \%cat_frequency;

    # these 3 subroutines are intentionally inside the scope of the current
    # subroutine get_cat_stats()
}

sub start_hsectn {

    my ($tagname, $attr_href) = @_;

    return if $tagname ne $stats_tag;
    return if !defined $attr_href->{$stats_attr_name};
    return if $attr_href->{$stats_attr_name} ne $stats_attr_value;

    print STDERR qq{found $tagname with $stats_attr_name == "$stats_attr_value"\n}
      if $verbosity > 1;
    $get_text++; # ought to only ever reach a max of 1
}

sub end_hsectn {

    my ($tagname) = @_;
    return if $tagname ne $stats_tag;
    return if !$get_text;
    print STDERR qq{closing parsed $tagname\n}
      if $verbosity > 1;
    $get_text = 0;
}

sub text_hsectn {

    my ($content_text) = @_;

    return if !$get_text;

    push @text_items, $content_text;
    print STDERR qq{\tfound text: '$content_text'\n}
      if $verbosity > 1;
}


sub data_related_tag {

    my $tag_name = shift;

    # use a global; because, naturally, this script has 'growed' and I've not
    # bothered to design it properly (OO approach would be much better here)

    my $use_tag = first { lc $_ eq lc $tag_name } (@data_associated_tags);
    return $use_tag;
}

sub start_dsectn {

    my ($tagname, $attr_href) = @_;

    if ($verbosity > 2) {
        print STDERR qq{found tag: $tagname\n};
        print STDERR qq{\tinside data table: },
          ($in_data_table) ? qq{true} : qq{false}, qq{\n};
        print STDERR qq{\tinside data row: },
          ($in_data_rows) ? qq{true} : qq{false}, qq{\n};
        print STDERR qq{\tinside data cell (column): },
          ($in_data_columns) ? qq{true} : qq{false}, qq{\n};
    }

    return if !data_related_tag($tagname);

    TAGNAME: {
        (lc $tagname eq lc $table_tag) && do {
            print STDERR qq{found table } if $verbosity;
            $table_index++; # note that the first table is table 1 not table 0
            my $table_id;

            if (defined $attr_href->{$table_attr_name}) {

                $table_id = $attr_href->{$table_attr_name};

=pod
    a sanity check; if the table with the expected ID also has the expected
    'index' (i.e. ordinal number of the table of all the tables in the page)
    then all is ok; otherwise flag a big warning - it indicates that the format
    of the html pages at the remote end may have changed since this script was
    written
=cut

                if ($table_id eq $table_attr_value) {
                    print STDERR qq{with },
                      qq{$table_attr_name = '$table_attr_value'; },
                      qq{table index = $table_index; },
                      ($table_index == $use_table) ?
                          qq{ as expected} :
                          qq{ WARNING - EXPECTED TABLE WITH INDEX $table_index}.
                          qq{ TO BE THE TABLE WITH THIS $table_attr_name},
                      qq{\n} if $verbosity;
                      # but process the table anyway:
                      $in_data_table++; # should never exceed 1
                      print STDERR qq{reading from table\n};
                    last TAGNAME;
                }
            }

            # note that in this case, the table ISN'T processed
            print STDERR qq{: WARNING - EXPECTED TABLE WITH INDEX $table_index}.
                  qq{TO HAVE ATTRIBUTE $table_attr_name = '$table_attr_value'\n}
            if ($table_index == $use_table) && !$table_id;

            print STDERR qq{ - ignoring this table\n} if $verbosity;
            last TAGNAME;
        };

        (lc $tagname eq lc $row_tag) && do {
            return if !$in_data_table; # 'last TAGNAME' will do instead of return?

            $column_index = 0; # this is redundant really

            # note that $row_index counts the rows which are in a data-associated
            # table only; and that it counts *all* rows in that table,
            # irrespective of whether they are ignored; this isn't used for
            # anything of note though
            $row_index++;      # note that the first row is row 1 not row 0

            # ignore this row if a number of rows to ignore has been specified
            # and there are still some remaining to ignore...
            if ($ignore_rows) {# only decrease if it's nonzero
                $ignore_rows--;
                print STDERR qq{\tignoring this row (irrespective of whether it's a }.
                      qq{title row; $ignore_rows more to ignore\n};
                undef $in_data_rows;
                last TAGNAME; # or return ?
            }

            # ignore title rows
            if (
            # yep, it's inconsistent; attr name is hardcoded here (c.f. above)
                 ( (defined $attr_href->{class}) &&
                   ($attr_href->{class} eq $title_row_class))
               ||
                 ( (defined $attr_href->{id}) &&
                   ($attr_href->{id} eq $title_row_id))
               ) {
               print STDERR qq{\tignoring title row\n} if $verbosity > 1;
               undef $in_data_rows;
               last TAGNAME;
            }

            print STDERR qq{\t}, ($in_data_rows) ? qq{next} : qq{first},
                  qq{ data row found ($in_data_rows)\n} if $verbosity > 1;
            $in_data_rows++; # this actually keeps a count of how many data rows
            last TAGNAME;
        };

        (lc $tagname eq lc $cell_tag) && do {
            return if !$in_data_table || !$in_data_rows; # 'last TAGNAME' will do instead?

            # note that there is intentionally no 'ignore columns' option
            # (unlike 'ignore rows'); so no tests are needed for that; but a
            # counter of columns is required, since the column-of-interest is
            # specified by an index
            $column_index++; # note that the first column is column 1 not column 0

            print STDERR qq{\t\tcolumn: $column_index\n} if $verbosity > 2;

            # simple test here; may later be replaced by checking whether the
            # index is in a particular range or is a member of a list of (not
            # necessarily consecutive) columns; but for now, it has to be "the"
            # column
            if ($column_index == $use_column) {
                $in_data_columns++;
            }
            else {
                undef $in_data_columns;
                last TAGNAME;
            }

            print STDERR qq{\t\tfound data cell ($tagname)\n} if $verbosity > 1;
            last TAGNAME;
        };

        (lc $tagname eq lc $anchor_tag) && do {
            return if !$in_data_table || !$in_data_rows || !$in_data_columns;
            if (defined $attr_href->{href}) {
                push @content_urls, $attr_href->{href};
            }
            else {
                print STDERR qq{WARNING: anchor in data cell contains no HREF\n};
            }
            last TAGNAME if $verbosity < 2;
            print STDERR qq{\t\t\trequired data cell contains anchor: };
            print STDERR qq{\t\t\t\t$_ = $attr_href->{$_}\n}
                for (keys %{$attr_href});
            last TAGNAME;
        };

    } # TAGNAME end

    # Any actions that should be taken after opening *any data-related tag*
    # should be placed here; note that non data-related tags won't get this
    # far (due to 'return' instead of 'last' above)

}

sub end_dsectn {

    my ($tagname) = @_;

    return if !data_related_tag($tagname);

    my $padding = qq{};

    TAGNAME: {
        (lc $tagname eq lc $table_tag) && do {
            last TAGNAME if !$in_data_table;
            print STDERR qq{end of data table\n};
            undef $in_data_table;
            undef $in_data_rows;
            undef $in_data_columns;
            $row_index = 0; # N.B. this is also set to zero when a row *starts*
            $column_index = 0;
            last TAGNAME;
        };

        (lc $tagname eq lc $row_tag) && do {
            return if !$in_data_table || !$in_data_rows;
            # note that $in_data_rows is *not* made false just because the end of
            # the row is reached; that occurs if either a new row starts which is
            # identified as a non-data row, or if the table ends (see above)
            undef $in_data_columns; # however, this *is* set to false (but it may
                                    # well be already)
            $padding = qq{\t};
            $column_index = 0;
            last TAGNAME;
        };

        (lc $tagname eq lc $cell_tag) && do {
            return if !$in_data_table || !$in_data_rows || !$in_data_columns;
            print STDERR qq{  end data cell ($cell_tag)\n} if $verbosity > 1;
            # best to explicitly set status to 'not in a data column' lest
            # additional pieces of *text* occur *after* this cell (and would be
            # therefore treated as if they are part of a data column)
            undef $in_data_columns;
            $padding = qq{\t\t};
            last TAGNAME;
        };

        (lc $tagname eq lc $heading_tag) && do {
            return if !$in_data_table || !$in_data_rows || !$in_data_columns;
            $padding = qq{\t\t};
            last TAGNAME;
        };

        (lc $tagname eq lc $anchor_tag) && do {
            return if !$in_data_table || !$in_data_rows || !$in_data_columns;
            $padding = qq{\t\t};
            last TAGNAME;
        };
    }
    print STDERR qq{${padding}closing parsed $tagname\n}
      if $verbosity > 1;

}

sub text_dsectn {

    my ($content_text) = @_;

    return if !$in_data_columns;

    print STDERR qq{\t} x 6, qq{DATA: '$content_text'\n} if $verbosity > 1;

    push @content_ids, $content_text;

}

sub parse_pages {

    my %arg = @_;

    #YATSSBOO (yet another This Script Should Be OO)
    my ($first_page_url  , $pagination_style,
        $records_per_page, $n_records_to_get,
        $out_pages_dir   , $first_file      ,
        $clobber         ,
        )
        = @arg{'first_url', 'page_context',
               'records_per_page', 'total_records',
               'outdir' , 'first_page', 'clobber'};

    # note that this 'fails' if the number of records to get is 0
    my $n_pages = int(($n_records_to_get - 1) / $records_per_page) + 1; 

    my $url = $first_page_url; 
    (my $file_root = $first_file) =~ s{ \A ( .* ) ([.] [^.]+) \z }{$1}xms;
    my $ext = $2;


    for my $page ( 0 .. $n_pages - 1) {

        my $local_file = $first_file if defined $first_file;

        if ($page) {

            $url = $first_page_url .qq{?debut_$page_context=} . $page * $records_per_page . qq{#pagination_$page_context};
            $local_file = $file_root.q{_}.$page.$ext if defined $local_file;
        }

        my ($open_expr, $mode, $ofh) = fetch_read_write(
            local_file  => $local_file      , clobber => $clobber ,
            get_command => $arg{get_command}, url     => $url     ,
        );

        # note that if the output file is to be opened at all,
        # then it will have already been done; but the input is not open,
        # and is specified by $open_expr and $mode
        print STDERR qq{$open_expr, $mode},
                     (defined $ofh) ?  qq{, $ofh} : qq{},
                     qq{\n} if $verbosity > 1;
        parse_single_page($open_expr, $mode, $ofh);
    }
}

sub parse_single_page {

    my $open_expr = shift;
    my $mode = shift;
    my $ofh = shift;

    my $parser = HTML::Parser->new( api_version => 3,
                                    start_h => [\&start_dsectn, "tagname, attr"],
                                    end_h   => [\&end_dsectn  , "tagname"      ],
                                    text_h  => [\&text_dsectn , "dtext"        ],
                                    marked_sections => 1,
                              );

    # Note that these are globals
    $table_index = 0;
    $row_index = 0;
    $column_index = 0;


    print STDERR qq{opening: $open_expr\n};
    open(my $fh, $mode, $open_expr);

    # Note that GTHML::Parser->parse() won't play ball with this form:
    # $parser->parse(<$fh>); so read line-by-line

    while (defined (my $line = <$fh>)) {
        #print qq{LINE: $line};
        $parser->parse($line);
        print $ofh $line if $ofh;
    }

    close $fh;
    close $ofh if $ofh;
    $parser->eof();

    print STDERR qq{closing: $open_expr\n};

}

sub output_results {

    my %arg = @_;

    my $ids_lref      = $arg{sequence_ids} ;
    my $urls_lref     = $arg{original_urls};
    my $url_prefix    = $arg{url_prefix}   ;
    my $retrieve_type = $arg{retrieve_type};
    my $batch_size    = $arg{batch_size};
    my $pause_record  = $arg{pause_records};
    my $pause_batch   = $arg{pause_batches};

    my @seq_ids = @{$ids_lref};
    my @urls    = @{$urls_lref};
    my $n_urls  = @urls;

    print STDERR scalar @seq_ids, qq{ sequence IDs retrieved; },
                 qq{$n_urls original URLs retrieved\n};

    my $url_type = qq{(original)};

    print STDERR qq{Retrieving ${retrieve_type}s};


    SHOW_DATA: {

        ($retrieve_type eq 'id') && do {
            print join(qq{\n}, @seq_ids), qq{\n};
            last SHOW_DATA;
        };

        ($url_prefix || $url_suffix) && do {
            $url_type = qq{derived from prepending '$url_prefix' and appending '$url_suffix') to sequence IDs\n};
            @urls = map { $url_prefix . $_ . $url_suffix } @seq_ids;
            my $n_urls = @urls;
        };

        ($retrieve_type eq 'url') && do {
            print STDERR qq{ $url_type};
            print join(qq{\n}, @urls), qq{\n};
            last SHOW_DATA;
        };

        ($retrieve_type eq 'sequence') && do {
            print STDERR qq{ obtained from URLS $url_type};
            my $n_downloads = 0;
            for my $url (@urls) {
                my $get_to_stdout = qq{$browser '$url'};
                print STDERR qq{$get_to_stdout\n};
                system $get_to_stdout;
                # this to avoid pauses if there's nothing left to follow
		next if (++$n_downloads == $n_urls); # should immediately terminate
                sleep $pause_record;
                next if $n_downloads % $batch_size;
                print STDERR qq{pausing for $pause_batch seconds\n};
                sleep $pause_batch;
            }
            last SHOW_DATA;
        };
    }# end SHOW_DATA

    print STDERR qq{\n};

}


