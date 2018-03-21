#!/usr/bin/perl

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
my $suffix = qq{_all.html};
#my $browser = qq{lynx -dump -width=1024};
my $browser = qq{wget};
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
my $use_column  = 1; # column-numbering also starts at 1 in this context

my $show_usage;
my $show_help;
my $show_manual;

my $example_web_page = qq{GH33_$suffix};
my $example_url = qq{http://$host/$example_web_page};

my $usage = qq{Usage:\n$0 [ options ] [ -cazyid ] CAZyID [ PAGESDIR ]};
my $usage_plus = qq{$usage\n(use -help for brief help or -man for manual)\n};

my $help = qq{$usage

	-cazyid		STRING
	-outdir		STRING
	-force
	-usage
	-help
	-man

Advanced options:

	-tableindex	INTEGER
	-ignoreclass	STRING
	-ignoretitle	STRING
	-ignorerows	INTEGER
	-columnindex	INTEGER
	-browser	STRING
	-wwwhost	STRING
	-suffix		STRING
	-pagesize	INTEGER
	-context	STRING

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

	-force			If specified, then an attempt will always be
				made to download the webpages to the output dir
				(see -outdir) will always be made. If -force is
				not used, then the download of each page will be
				done only if the page is absent from there.

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
				data. Default is column $use_column.

	-browser	STRING	This should be the full command-line prefix of
				the command to fetch each page. A single
				argument is appended, i.e. the URL of each
				page. Default is $browser.
				So for example, a command-line would be:
				$browser $out_pages_dir/$example_url

	-wwwhost	STRING	Web host name. Default is $host

	-suffix		STRING	Suffix to append to CAZy family ID, to form
				name of (first) web page (see 'pagination'
				below). Default is $suffix .
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

};


croak "$usage_plus" if !GetOptions(
    "cazyid=s"		=> \$cazy_id         ,
    "outdir=s"		=> \$out_pages_dir   ,
    "force"		=> \$clobber         ,
    "usage"		=> \$show_usage      ,
    "help"		=> \$show_help       ,
    "man"		=> \$show_manual     ,
    "tableindex=i"	=> \$use_table       ,
    "ignoreclass=s"	=> \$title_row_id    ,
    "ignoretitle=s"	=> \$title_row_class ,
    "ignorerows=i"	=> \$ignore_rows     ,
    "columnindex=i"	=> \$use_column      ,
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

my $default_url = qq{http://$host/${cazy_id}$suffix};
my $get_first_page = qq{$browser $default_url};

my $outfile0 = qq{${cazy_id}_0.html};
my $outpath0 = qq{$out_pages_dir/$outfile0};

my $command_first = qq{$get_first_page > $outpath0}; # assuming single-digit sufficient...

print STDERR qq{$command_first\n};
`$command_first` if ! -f $outpath0;

my $n_records_to_get = get_total($outpath0);
my $n_pages = int($n_records_to_get / $records_per_page) + 1; 

my @seq_ids;

for my $page ( 0 .. $n_pages - 1) {
    my $outfile = qq{${cazy_id}_$page.html};
    my $outpath = qq{$out_pages_dir/$outfile};
    if ($page) {
        my $url = $default_url; 
        $url .= qq{?debut_$page_context=} . $page * $records_per_page . qq{#pagination_$page_context};
        my $command = qq{$browser $url > $outpath};
        print STDERR qq{$command\n};
        `$command` if ! -f $outpath;
    }
    print qq{get_seq_ids($outpath)\n};
}

exit 0;

sub get_total {

    my $text_page = shift;
    my $total_records;

    open my $fh, '<', $text_page;
    LINE:
    while (defined (my $line = <$fh>)) {

=pod
    a sanity check involving an arbitrary number of lines; e.g. a typical
    arrangement is:

...
...
   Statistics                    GenBank accession (1453); Uniprot accession (226); PDB accession (6); 3D entries (3); cryst (0)
   [19]Summary
   [20]All (1392) [21]Archaea (3) [22]Bacteria (1343) [23]Eukaryota (40) [24]unclassified (6) [25]Structure (3) [26]Characterized (8)
   < | 1 | [27]2 | [28]>
...
...
    where the final line above is the menu of pages; however, all that's
    really needed is the number in parentheses following the 'All' string
    (third line above). But the 'Statistics', 'Summary' and 'All' lines
    are explicitly checked for occurrence.
=cut
        next LINE if $line !~ m{ \A \s* Statistics \s+ }xms;

        my $passage = $line;
        my $next_line = <$fh>;
        $passage .= $next_line;
        croak qq{unexpected format in file $text_page:\n...\n$passage\n}
          if $next_line !~ m{ \A \s* \[ \d+ \] Summary \s }xms;

        $next_line = <$fh>;
        $passage .= $next_line;
        croak qq{unexpected format in file $text_page:\n...\n$passage\n}
          if $next_line !~ m{ \A \s* \[ \d+ \] All \s* [(](\d+)[)] \s }xms;

        $total_records = $1;
        ### insert further parsing/sanity-checking here, if required
        last LINE;
    }
    return $total_records;
}

sub get_seq_ids {

    my $text_page = shift;
    print STDERR qq{extracting sequence IDs from $text_page\n};

    my $class;
    my %records_in_class; # elements are array refs; each element of the
                          # arrays represents a single sequence ID
    my @ids_of_record; # used to store multiple IDs for the current record

    my $n_records;

    open my $fh, '<', $text_page;
=pod

    Example content:
....
   Statistics                    GenBank accession (1453); Uniprot accession (226); PDB accession (6); 3D entries (3); cryst (0)
   [19]Summary
   [20]All (1392) [21]Archaea (3) [22]Bacteria (1343) [23]Eukaryota (40) [24]unclassified (6) [25]Structure (3) [26]Characterized (8)
   < | 1 | [27]2 | [28]>
   Archaea
   Protein Name EC# Organism GenBank Uniprot PDB/3D
    Halxa_0483   [29]Halopiger xanaduensis SH-6 [30]AEH39084.1
    HTIA_2443   [31]Halorhabdus tiamatea SARL4B type strain: SARL4B [32]CCQ34551.1
    Huta_2700   [33]Halorhabdus utahensis DSM 12940 [34]ACV12861.1 [35]C7NQD5
   Bacteria
   Protein Name EC# Organism GenBank Uniprot PDB/3D
    LuPra_01786   [36]Acidobacteria bacterium DSM 100886 [37]AMY08582.1
    AHOG_11850   [38]Actinoalloteichus hoggarensis DSM 45943 [39]ASO20013.1
    AHOG_16195   [40]Actinoalloteichus hoggarensis DSM 45943 [41]ASO20865.1
    TL08_13400   [42]Actinoalloteichus hymeniacidonis HPA177(T) (=DSM 45092(T)) [43]AOS63494.1
    UA74_18230   [44]Actinoalloteichus sp. ADI127-7 [45]APU15674.1
    UA74_12580   [46]Actinoalloteichus sp. ADI127-7 [47]APU14576.1
    C1701_23000   [48]Actinoalloteichus sp. AHMU CJ021 [49]AUS80736.1
....
....
    SOR_0343   [2084]Streptococcus oralis Uo5 [2085]CBZ00029.1
    Sequence 3214 from patent US 6699703   [2086]Streptococcus pneumoniae [2087]AAT15844.1
   AAW09246.1
   ABI03175.1
   ABI08216.1
   ABJ31732.1
   ABJ49345.1
   ABL17088.1
   ACK13634.1
   ACW44353.1
    BUM80_05795   [2088]Streptococcus pneumoniae 11A [2089]AUC45907.1
    BUM80_08585   [2090]Streptococcus pneumoniae 11A [2091]AUC46367.1
....
....
    BOC72_01499   [2212]Streptococcus pneumoniae SP64 [2213]APJ35011.1
    BOC72_02052   [2214]Streptococcus pneumoniae SP64 [2215]APJ35502.1
    SPN034156_09850   [2216]Streptococcus pneumoniae SPN034156 SNP034156 [2217]CCP36654.1
   [2218]Top
   Last update: 2018-03-13 Â© Copyright 1998-2018
   [2219]AFMB - CNRS - UniversitÃ© d'Aix-Marseille

References

   1. http://www.cazy.org/spip.php?page=backend
   2. http://www.cazy.org/Welcome-to-the-Carbohydrate-Active.html
....
....
etc.
Note that one of the records above has 9 sequence IDs (only the first of which
is hyperlinked).

The end of the table on the page is indicated by the 'Top' link.
=cut

    LINE:
    while (defined (my $line = <$fh>)) {
        FORMAT: {
            ($line =~ m{ \A \s* ( Archaea | Bacteria |
                              Eukaryota | unclassified ) \s* \z }xms) && do {
                my $new_class = $1;
print STDERR qq{** $new_class **\n};
                # deal with previous record, if there is one
print STDERR qq{\@ids_of_record:\n}, Dumper \@ids_of_record;
                # @ids_of_record will change in a moment, so a ref to it cannot
                # safely be added to the list; so make a copy, and add a ref
                # to that instead
                my @this_record_ids = @ids_of_record;
                push @{$records_in_class{$class}}, \@this_record_ids
                   if (@ids_of_record);
                undef @ids_of_record;

                $class = $new_class;

                my $next_line = <$fh>;
                croak qq{expecting 'Protein Name ...' line: }.
                      qq{unexpected format in $text_page:\n${line}$next_line\n}
                  if $next_line !~ m{ \A \s* Protein \s Name \s EC[#] \s Organism
                                 \s GenBank \s Uniprot \s PDB[/]3D \s* \n? \z}xms;
                undef @ids_of_record;
                last FORMAT;
            };
            ($line =~ m{ \A \s* \[ \d+ \] Top \s* \n? \z }xms) && do {
                # it's the end of table
                last LINE;
            };
            next LINE if !$class;

=pod
            this represents a record line; examples (see above) are:

    SOR_0343   [2084]Streptococcus oralis Uo5 [2085]CBZ00029.1

    Sequence 3214 from patent US 6699703   [2086]Streptococcus pneumoniae [2087]AAT15844.1
   AAW09246.1
   ABI03175.1

   Huta_2700   [33]Halorhabdus utahensis DSM 12940 [34]ACV12861.1 [35]C7NQD5

    - i.e. that's 3 records; the first has 1 sequence ID, the second has 3
    (actually it's really 9 for this record, but the above is illustrative).
    The third record has a non-blank UniProt field (unlike the other 2).

    Therefore, a 'one-field' line is assumed to be a continuation of the
    previous line.

    The first line of a record is assumed to have a minimum of 3 non-
    whitespace fields, because it has a minimum of 3 columns (but the second,
    i.e. Organism, can often, and will usually, contain whitespace.
    Note, it can be assumed that in the first row of a record, all the fields
    from the Organism onwards, which are non-blank, will begin with a link
    (shown as the [..] placeholders containing the link index).

    An alternative to this would be to use HTML::Parser, because the source
    of the CAZy pages shows that all empty cells are coded explicitly (which
    would not be the only way of doing it; e.g. multi-column spanning cells
    would be another way, which would make parsing more fiddly). So using
    HTML::Parser would be relatively straightforward; but possibly still
    more involved than the way implemented here.

    Note also that (as is evident only by looking at the HTML source of the
    web pages), every row appears to end with an empty column; i.e. after
    the PDB/3D column. This is always literally empty, whereas an empty
    UniProt or PDB/3D column actually always contains a non-breaking space
    (i.e. '&nbsp;').

=cut
#            pos $line = 0;
#            my @fields_with_link;
#            while ( $line =~ m{ \G !(?:\[\d+\]) }gcxms
             chomp $line;
##print STDERR qq{examining line : '$line'\n};
             ($line !~ m{ \[\d+\] }xms) && do {
                 croak qq{unexpected data row:\n$line}
                   if (!@ids_of_record);
                 $line =~ s{ (?: \A \s+ | \s+ \z ) }{}gxms;
                 my @field = split m{ \s+ }, $line;
                 croak qq{expecting single sequence ID: unexpected format:}.
                       qq{$line} if @field > 1;
                 push @ids_of_record, @field;
print STDERR Dumper \@ids_of_record;
                 last FORMAT;
             };

             my @linked_fields = split m{ \[\d+\] }xms, $line;
             for (@linked_fields) { s{ (?: \A \s+ | \s+ \z ) }{}gxms }
             # the first field is the one that ISN'T linked
             my $prefix = shift @linked_fields;

             # EC # may often be absent; but this spots it
             shift @linked_fields
                 if $linked_fields[0] =~ m{ \A (?:(?:\d+|[-])[.]){3}(?:\d+|[-]) \z }xms;
             my ($organism, $genbank, $uniprot, $pdb) = @linked_fields;

             # deal with previous (now complete) record
print STDERR Dumper \@ids_of_record;
             # @ids_of_record will change in a moment, so a ref to it cannot
             # safely be added to the list; so make a copy, and add a ref
             # to that instead
             my @this_record_ids = @ids_of_record;
             push @{$records_in_class{$class}}, \@this_record_ids
                 if (@ids_of_record);

             # now the current record
             $n_records++;
             # $prefix represents the 'key', if this needs to be used
             # explicitly in later versions of the script
             print STDERR qq{Protein Name '$prefix'\n};
             @ids_of_record = ($genbank);

        } # end FORMAT

    } # end LINE

    print STDERR Dumper \%records_in_class;
}


