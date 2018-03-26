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
  qw(Archaea Bacteria Eukaryota Viruses Structure Characterized));

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
my $use_column  = 1; # column-numbering also starts at 1 in this context

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
my $column_tag = q{td}; # note that <TH></TH> tags are thus ignored as far as
                        # data parsing s concerned; but they are noted:
my $heading_tag = q{th};

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

my $verbosity = 0;
my $show_usage;
my $show_help;
my $show_manual;

my $example_web_page = qq{GH33_$default_suffix};
my $example_url = qq{http://$host/$example_web_page};

my $usage = qq{Usage:\n$0 [ options ] [ -cazyid ] CAZyID [ PAGESDIR ]};
my $usage_plus = qq{$usage\n(use -help for brief help or -man for manual)\n};

my $help = qq{$usage

	-cazyid		STRING
	-outdir		STRING
        -category	STRING
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

        -category	STRING	The category of sequences whose IDs (and
				possibly sequence records too) to download.
				This is expected to be one of:

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
    "category=s"	=> \$category        ,
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

my $capital   = uc(substr $category, 0, 1);
my $remainder = lc(substr $category, 1   );

if (($category =~ s{ \A [a-z] .* \z }{${capital}$remainder}xms) ||
    ($category =~ s{ \A . .*[A-Z].* \z }{${capital}$remainder}xms)) {
    print STDERR qq{WARNING: modified case of category name: is now '$category'\n};
}

print STDERR qq{WARNING: '$category' is not a known category - so this will probably fail\n}
  if !first { $_ eq $category } @expected_categories;

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

print qq{Summary of pagination parameters - check the manual (use -man) if you are unsure:

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
        print qq{NO SEQUENCE TOTAL RETRIEVED FOR CATEGORY '$cat'\n};
        push @unretrieved_cat, $cat;
        next;
    }

    printf qq{category %-20s : %6d sequence}, $cat, $seqs_per_category_href->{$cat};
    print qq{}, ($seqs_per_category_href->{$cat} == 1) ? qq{} : qq{s}, qq{\n};
}

my @unexpected_cat = grep {
    my $rcat = $_;
    !first { $_ eq $rcat } @expected_categories
} (keys %{$seqs_per_category_href});

if (@unexpected_cat) {

    print qq{THESE UNEXPECTED CATEGORIES WERE FOUND IN THE WEB PAGE:\n\t},
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

exit 1;

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

    print qq{opening: $open_expr\n};
    open(my $fh, $mode, $open_expr);
    # should be easily small enough to be slurp-safe
    #my @summary_content = <$fh>;

    # Note that GTHML::Parser->parse() won't play ball with this form:
    # $parser->parse(<$fh>); so read line-by-line

# These have now been made global:
#    my $get_text; # boolean
#    my @text_items;

    while (defined (my $line = <$fh>)) {
        #print qq{LINE: $line};
        $parser->parse($line);
        print $ofh $line if $ofh;
    }

    close $fh;
    close $ofh if $ofh;
    $parser->eof();

    print qq{closing\n};

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
###print qq{start_hsectn($tagname, $attr_href); looking for $stats_tag\n};
    return if $tagname ne $stats_tag;
    return if !defined $attr_href->{$stats_attr_name};
    return if $attr_href->{$stats_attr_name} ne $stats_attr_value;

    print qq{found $tagname with $stats_attr_name == "$stats_attr_value"\n}
      if $verbosity > 1;
    $get_text++; # ought to only ever reach a max of 1
}

sub end_hsectn {

    my ($tagname) = @_;
###print qq{end_hsectn()\n};
    return if $tagname ne $stats_tag;
    return if !$get_text;
    print qq{closing parsed $tagname\n}
      if $verbosity > 1;
    $get_text = 0;
}

sub text_hsectn {

    my ($content_text) = @_;
###print qq{text_hsectn()\n};

    return if !$get_text;

    push @text_items, $content_text;
    print qq{\tfound text: '$content_text'\n}
      if $verbosity > 0;
}
#}

sub start_dsectn {

    my ($tagname, $attr_href) = @_;
###print qq{start_hsectn($tagname, $attr_href); looking for $stats_tag\n};
    my $use_tag = first { lc $_ eq lc $tagname } (
        $table_tag, $row_tag, $column_tag, $heading_tag
    );
    return if !defined $use_tag;

    TAGNAME: {
        (lc $tagname eq lc $table_tag) && do {
            print qq{found table };
            $table_index++;
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
                    print qq{with },
                      qq{$table_attr_name = '$table_attr_value';},
                      qq{table index = $table_index; },
                      ($table_index == $use_table) ?
                          qq{ as expected} :
                          qq{ WARNING - EXPECTED TABLE WITH INDEX $table_index}.
                          qq{ TO BE THE TABLE WITH THIS $table_attr_name},
                      qq{\n};
                      # but process the table anyway:
                      $in_data_table++; # should never exceed 1
                    last TAGNAME;
                }
            }

            # note that in this case, the table ISN'T processed
            print qq{: WARNING - EXPECTED TABLE WITH INDEX $table_index}.
                  qq{TO HAVE ATTRIBUTE $table_attr_name = '$table_attr_value'\n}
            if ($table_index == $use_table) && !$table_id;

            print qq{ - ignoring this table\n};
            last TAGNAME;
        };

        (lc $tagname eq lc $row_tag) && do {
            return if !$in_data_table;

            # ignore this row if a number of rows to ignore has been specified
            # and there are still some remaining to ignore...
            if ($ignore_rows) {# only decrease if it's nonzero
                $ignore_rows--;
                print qq{\tignoring this row (irrespective of whether it's a }.
                      qq{title row; $ignore_rows more to ignore\n};
                undef $in_data_rows;
                last TAGNAME; # or return ?
            }
###print Dumper($attr_href);
            # ignore title rows
            if (
                 ( (defined $attr_href->{class}) &&
                   ($attr_href->{class} eq $title_row_class))
               ||
                 ( (defined $attr_href->{id}) &&
                   ($attr_href->{id} eq $title_row_id))
               ) {
                   print qq{\tignoring title row\n};
               undef $in_data_rows;
               last TAGNAME;
            }
            # risky postfix '++' in a print statement...
            print qq{\t}, ($in_data_rows++) ? qq{next} : qq{first},
                  qq{ data row found ($in_data_rows)\n};
            #$in_data_rows++; # this actually keeps a count of how many data rows
            # yep, it's inconsistent; attr name is hardcoded here (c.f. above)
            last TAGNAME;
        };

        (lc $tagname eq lc $column_tag) && do {
            return if !$in_data_table || !$in_data_rows;
            print qq{\t\tfound data cell ($tagname)\n};
            last TAGNAME;
        };
    }

#    print qq{found $tagname\n} # with $stats_attr_name == "$stats_attr_value"\n}
#      if $verbosity > 1;
###    $get_text++; # ought to only ever reach a max of 1
}

sub end_dsectn {

    my ($tagname) = @_;
    my $use_tag = first { lc $_ eq lc $tagname } (
        $table_tag, $row_tag, $column_tag, $heading_tag
    );
    return if !defined $use_tag;

    my $padding = qq{};

    TAGNAME: {
        (lc $tagname eq lc $table_tag) && do {
            last TAGNAME if !$in_data_table;
            print qq{end of data table\n};
            undef $in_data_table;
            undef $in_data_rows;
            last TAGNAME;
        };

        (lc $tagname eq lc $row_tag) && do {
            return if !$in_data_table || !$in_data_rows;
            $padding = qq{\t};
            last TAGNAME;
        };
        (lc $tagname eq lc $column_tag) && do {
            return if !$in_data_table || !$in_data_rows;;
            $padding = qq{\t\t};
            last TAGNAME;
        };
        (lc $tagname eq lc $heading_tag) && do {
            return if !$in_data_table || !$in_data_rows;;
            $padding = qq{\t\t};
            last TAGNAME;
        };
    }
    print qq{${padding}closing parsed $tagname\n}
      if $verbosity > 1;

}

sub text_dsectn {

    my ($content_text) = @_;
###print qq{text_hsectn()\n};

#    return if !$get_text;

#    push @text_items, $content_text;
#    print qq{\tfound text: '$content_text'\n}
#      if $verbosity > 0;
}

##----------------------------------------------------------
exit 0;

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
###    my $outfile = qq{${cazy_id}_$page.html};
###    my $outpath = qq{$out_pages_dir/$outfile};

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
        print qq{$open_expr, $mode}, (defined $ofh) ?  qq{, $ofh} : qq{}, qq{\n};
        parse_single_page($open_expr, $mode, $ofh);
        ###print qq{get_seq_ids($outpath)\n};
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
    

    print qq{opening: $open_expr\n};
    open(my $fh, $mode, $open_expr);
    # should be easily small enough to be slurp-safe
    #my @summary_content = <$fh>;

    # Note that GTHML::Parser->parse() won't play ball with this form:
    # $parser->parse(<$fh>); so read line-by-line

# These have now been made global:
#    my $get_text; # boolean
#    my @text_items;

    while (defined (my $line = <$fh>)) {
        #print qq{LINE: $line};
        $parser->parse($line);
        print $ofh $line if $ofh;
    }

    close $fh;
    close $ofh if $ofh;
    $parser->eof();

    print qq{closing\n};

}

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


