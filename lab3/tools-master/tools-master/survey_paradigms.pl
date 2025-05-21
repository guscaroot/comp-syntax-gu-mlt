#!/usr/bin/env -S perl -CDSA
# Scans a treebank for combinations of lemmas, UPOS tags, forms and features.
# Prints observed paradigm tables.
# Copyright © 2022 Dan Zeman <zeman@ufal.mff.cuni.cz>
# License: GNU GPL

use utf8;
use open ':utf8';
binmode(STDIN, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDERR, ':utf8');
use Encode qw(decode); # to be able to decode the command line arguments
use Getopt::Long;
# Make sure that the tools folder is searched for Perl modules. Then use udlib from there.
# Without it, we could not run this script from other folders.
BEGIN
{
    our $toolsdir = $0;
    unless($toolsdir =~ s:[/\\][^/\\]*$::)
    {
        $toolsdir = '.';
    }
}
use lib "$toolsdir";
use udlib;

sub usage
{
    print STDERR ("$0: Scan a UD treebank for morphological paradigms and print them. A paradigm is a set of word forms sharing the same lemma and UPOS.\n\n");
    print STDERR ("Usage: $0 --udpath <path-to-ud> --treebank <UD_Lang-XXX>\n");
    print STDERR ("       <path-to-ud> is the path to the folder where all UD treebanks reside; default: current folder\n");
    print STDERR ("       <UD_Lang-XXX> is the name of the treebank folder to be surveyed\n");
    print STDERR ("       If none of the above is provided, the default is --udpath .. --treebank . (current folder)\n");
    print STDERR ("       If only the treebank name is given, the default for --udpath is '.' (current folder)\n");
    print STDERR ("       If --udpath is different from '..', then the treebank name must not be '.'\n");
    print STDERR ("       The paradigm tables will be printed to STDOUT.\n");
    print STDERR ("Options:\n");
    print STDERR ("       --help: Print this help text and exit.\n");
    print STDERR ("       --lemma='RE': Print only paradigms whose lemma matches regular expression RE. The expression\n");
    print STDERR ("           is matched against the entire lemma. Pad with '.*' if partial match is desired. If there is\n");
    print STDERR ("           the LId attribute in MISC, it will be used to separate paradigms of different lexemes, but\n");
    print STDERR ("           --lemma is still matched against the LEMMA column. Perl may have to be invoked with the -CA\n");
    print STDERR ("           option to interpret arguments as UTF-8.\n");
    print STDERR ("       --upos=X|--tag=X: Print only paradigms with the UPOS tag X. Multiple --upos options can be given.\n");
    print STDERR ("           Output paradigms may satisfy any of them.\n");
    print STDERR ("       --feats='RE': Print only paradigms containing feature annotation that matches regular expression RE.\n");
    print STDERR ("           Multiple --feats options can be given. Output paradigms must satisfy all of them, presumably\n");
    print STDERR ("           each will be satisfied with a different word form.\n");
    print STDERR ("       --form=='RE': Print only paradigms containing word form RE. Like in --feats, RE is a regular\n");
    print STDERR ("           expression, but unlike in --feats it is matched against the entire form. Pad it with '.*' if\n");
    print STDERR ("           partial match is desired. Matching is case-insensitive. Multiple --form options can be given.\n");
    print STDERR ("           Output paradigms must satisfy all of them, presumably each will be satisfied with a different\n");
    print STDERR ("           word form. Perl may have to be invoked with the -CA option to interpret arguments as UTF-8.\n");
    print STDERR ("       --minforms=N: Print only paradigms that contain N or more distinct word forms. Default N=1.\n");
    print STDERR ("       --sortbysize: Print the paradigms with most forms, slots and occurrences first. Default: sort alphabetically.\n");
    print STDERR ("       --onlyone: Print only the first matching paradigm. Useful together with --sortbysize.\n");
    print STDERR ("       --newfeats: Print only paradigms that contain a Feature=Value pair not seen previously.\n");
    print STDERR ("       --translit=replace|add|none: Use transliteration if available. Default: replace.\n");
}

# https://stackoverflow.com/questions/2037467/how-can-i-treat-command-line-arguments-as-utf-8-in-perl
# suggests that we should install the module I18::Langinfo and then query the
# locale like this:
#use I18N::Langinfo qw(langinfo CODESET);
#my $codeset = langinfo(CODESET);
#@ARGV = map {decode($codeset, $_)} (@ARGV);
# However, instead of depending on that module, we will simply require that the
# locale uses the UTF-8 encoding. We could also call perl with -CA to do this.
# Note: This does not work in Windows command line even with chcp 65001, so I
# am introducing this dirty hack for the time being:
use Cwd;
my $running_on_dans_laptop = getcwd() =~ m-^C:/Users/Dan/-;
if($running_on_dans_laptop)
{
    @ARGV = map {decode('cp1250', $_)} (@ARGV);
}
my $help = 0;
my $udpath;
my $folder;
my $lemma;
my @upos;
my @featsre;
my @formsre;
my $minforms = 1;
my $sort_by_size = 0;
my $only_one = 0;
my $require_newfeats = 0;
my $translit = 'replace'; # recognized: replace|add|none
GetOptions
(
    'help'        => \$help,
    'udpath=s'    => \$udpath,
    'treebank=s'  => \$folder,
    'tbk=s'       => \$folder, # alternative shortcut for --treebank
    'lemma=s'     => \$lemma,
    'upos=s'      => \@upos,
    'tag=s'       => \@upos,
    'feats=s'     => \@featsre,
    'form=s'      => \@formsre,
    'minforms=i'  => \$minforms,
    'sortbysize'  => \$sort_by_size,
    'onlyone'     => \$only_one,
    'newfeats'    => \$require_newfeats,
    'translit=s'  => \$translit
);
if($help)
{
    usage();
    exit(0);
}
if(!defined($folder))
{
    $folder = '.';
}
if(!defined($udpath))
{
    $udpath = $folder eq '.' ? '..' : '.';
}
if($udpath ne '..' && $folder eq '.')
{
    usage();
    die("Treebank folder is '.', hence UD path must be '..'");
}
my $tbkpath = $folder eq '.' ? '.' : "$udpath/$folder";
my $languages = udlib::get_language_hash("$udpath/docs-automation/codes_and_flags.yaml");
my $ltcode = udlib::get_ltcode_from_repo_name($folder, $languages);
# Debugging.
#print STDERR ("UD path = $udpath, treebank = $folder, treebank path = $tbkpath, ltcode = $ltcode\n");

my %stats =
(
    'config' =>
    {
        'translit' => $translit
    }
);
udlib::collect_examples_from_ud_treebank($tbkpath, $ltcode, \%stats);
lowercase_forms($stats{ltwf});

my @paradigms = ();
my @lemmas = sort(keys(%{$stats{ltwf}}));
foreach my $l (@lemmas)
{
    my $l0 = $l;
    if($l =~ m/^(.+) LId=.+$/)
    {
        $l0 = $1;
    }
    next if(defined($lemma) && $l0 !~ m/^$lemma$/i);
    my @tags = sort(keys(%{$stats{ltwf}{$l}}));
    foreach my $t (@tags)
    {
        next if(scalar(@upos) > 0 && !grep {$_ eq $t} (@upos));
        my $nforms = scalar(keys(%{$stats{ltwf}{$l}{$t}}));
        next if($nforms < $minforms);
        # Collect all annotations from all word forms.
        my %annotations;
        my @amatch;
        my @fmatch;
        my $nocc = 0;
        my $nslots = 0;
        my %fvmap;
        foreach my $f (keys(%{$stats{ltwf}{$l}{$t}}))
        {
            foreach my $a (keys(%{$stats{ltwf}{$l}{$t}{$f}}))
            {
                # Deserialize $a to individual Feature=Value pairs.
                my @fv = split(/\|/, $a);
                # Remember all Feature=Value pairs found in the paradigm.
                map {$fvmap{$_}++} (@fv);
                # Reorder features within the annotation so that we can later sort the annotations.
                my $sa = join('|', sort_features(@fv));
                $annotations{$sa}{$f} = $stats{ltwf}{$l}{$t}{$f}{$a};
                $nocc += $annotations{$sa}{$f};
                $nslots++;
                # If there are @featsre requirements, check which of them are satisfied by the current annotation.
                for(my $i = 0; $i <= $#featsre; $i++)
                {
                    $amatch[$i] = 1 if(!$amatch[$i] && ($sa =~ m/$featsre[$i]/i || $a =~ m/$featsre[$i]/i));
                }
            }
            # If there are @formsre requirements, check which of them are satisfied by the current annotation.
            for(my $i = 0; $i <= $#formsre; $i++)
            {
                $fmatch[$i] = 1 if(!$fmatch[$i] && $f =~ m/^$formsre[$i]$/i);
            }
        }
        # Check that all @featsre requirements are satisfied by the current paradigm.
        my $amatch = 1;
        for(my $i = 0; $i <= $#featsre; $i++)
        {
            if(!$amatch[$i])
            {
                $amatch = 0;
                last;
            }
        }
        next if(!$amatch);
        # Check that all @formsre requirements are satisfied by the current paradigm.
        my $fmatch = 1;
        for(my $i = 0; $i <= $#formsre; $i++)
        {
            if(!$fmatch[$i])
            {
                $fmatch = 0;
                last;
            }
        }
        next if(!$fmatch);
        # If the paradigm passed the above filters, save it but do not print it yet.
        # There may be other constraints, such as to print only the paradigm with
        # the highest number of forms, so we must wait until we have read it all.
        push(@paradigms, {'lemma' => $l, 'tag' => $t, 'nocc' => $nocc, 'nslots' => $nslots, 'nforms' => $nforms, 'annotations' => \%annotations, 'feats' => [keys(%fvmap)]});
    }
}
# If only the largest paradigm is requested, sort the paradigms by their size.
if($sort_by_size)
{
    @paradigms = sort
    {
        my $r = $b->{nforms} <=> $a->{nforms};
        unless($r)
        {
            $r = $b->{nslots} <=> $a->{nslots};
            unless($r)
            {
                $r = $b->{nocc} <=> $a->{nocc};
                unless($r)
                {
                    $r = $a->{lemma} cmp $b->{lemma};
                    unless($r)
                    {
                        $r = $a->{tag} cmp $b->{tag};
                    }
                }
            }
        }
        $r
    }
    (@paradigms);
}
# Print the collected paradigms.
my %fvmap; # remember features that have been already shown in a paradigm
foreach my $p (@paradigms)
{
    if($require_newfeats)
    {
        # Does this paradigm have a Feature=Value pair that has not been observed previously?
        my $newfv = 0;
        foreach my $fv (@{$p->{feats}})
        {
            if(!exists($fvmap{$fv}))
            {
                $newfv = 1;
            }
            $fvmap{$fv}++;
        }
        next if(!$newfv);
    }
    print("LEMMA $p->{lemma} $p->{tag} $p->{nocc} occurrences $p->{nslots} slots $p->{nforms} forms\n");
    # Sort annotations according to our custom feature priorities and print them.
    my @annotations = sort_annotations(keys(%{$p->{annotations}}));
    my @table;
    foreach my $a (@annotations)
    {
        my @forms = sort(keys(%{$p->{annotations}{$a}}));
        foreach my $f (@forms)
        {
            push(@table, [$f, $a, $p->{annotations}{$a}{$f}]);
        }
    }
    print_table(scalar(@table), 3, @table);
    print("\n");
    last if($only_one);
}



#------------------------------------------------------------------------------
# For each word form in the dictionary, if it is not in lowercase and a lower-
# case variant exists under the same lemma and tag, merges it with the lowerase
# instance.
#------------------------------------------------------------------------------
sub lowercase_forms
{
    my $ltwf = shift; # hash ref to the lemma-tag-word-feats dictionary
    my @lemmas = keys(%{$ltwf});
    foreach my $l (@lemmas)
    {
        my @tags = keys(%{$ltwf->{$l}});
        foreach my $t (@tags)
        {
            my @forms = keys(%{$ltwf->{$l}{$t}});
            foreach my $f (@forms)
            {
                my $lcf = lc($f);
                my $capf = uc(substr($f, 0, 1)).lc(substr($f, 1));
                if($f ne $lcf)
                {
                    if(exists($ltwf->{$l}{$t}{$lcf}))
                    {
                        # Merge $f with $lcf.
                        my @annotations = keys(%{$ltwf->{$l}{$t}{$f}});
                        foreach my $a (@annotations)
                        {
                            $ltwf->{$l}{$t}{$lcf}{$a} += $ltwf->{$l}{$t}{$f}{$a};
                        }
                        delete($ltwf->{$l}{$t}{$f});
                    }
                    elsif($f ne $capf && exists($ltwf->{$l}{$t}{$capf}))
                    {
                        # Merge $f with $capf.
                        my @annotations = keys(%{$ltwf->{$l}{$t}{$f}});
                        foreach my $a (@annotations)
                        {
                            $ltwf->{$l}{$t}{$capf}{$a} += $ltwf->{$l}{$t}{$f}{$a};
                        }
                        delete($ltwf->{$l}{$t}{$f});
                    }
                }
            }
        }
    }
}



#------------------------------------------------------------------------------
# We want to be able to order features and values according to custom
# priorities rather than alphabetically.
#------------------------------------------------------------------------------
BEGIN
{
    @feature_order =
    (
        'PronType'  => ['Prs', 'Rcp', 'Art', 'Dem', 'Rel', 'Int', 'Ind', 'Tot', 'Neg', 'Emp', 'Exc'],
        'Poss'      => ['Yes'],
        'Reflex'    => ['Yes'],
        'NumType'   => ['Card', 'Sets', 'Frac', 'Ord', 'Mult', 'Dist', 'Range'],
        'NumForm'   => ['Word', 'Digit', 'Roman'],
        'VerbForm'  => ['Fin', 'Inf', 'Sup', 'Part', 'Conv', 'Ger', 'Gdv', 'Vnoun'],
        'Mood'      => ['Ind', 'Irr', 'Sub', 'Cnd', 'Imp', 'Jus', 'Opt', 'Nec', 'Des', 'Pot', 'Adm', 'Prp', 'Qot', 'Int'],
        'Evident'   => ['Fh', 'Nfh'],
        'Tense'     => ['Pres', 'Fut', 'Past', 'Imp', 'Pqp'],
        'Aspect'    => ['Hab', 'Imp', 'Prog', 'Perf', 'Prosp', 'Iter'],
        'Voice'     => ['Act', 'Mid', 'Pass', 'Antip', 'Lfoc', 'Bfoc', 'Rcp', 'Cau', 'Dir', 'Inv'],
        'Degree'    => ['Pos', 'Equ', 'Cmp', 'Sup', 'Abs', 'Dim', 'Aug'],
        'Number'    => ['Sing', 'Coll', 'Dual', 'Tri', 'Pauc', 'Grpa', 'Plur', 'Ptan', 'Grpl', 'Count', 'Inv'],
        'Person'    => ['0', '1', '2', '3', '4'],
        'Clusivity' => ['In', 'Ex'],
        'Polite'    => ['Infm', 'Form', 'Elev', 'Humb'],
        'Gender'    => ['Masc', 'Fem', 'Com', 'Neut'],
        'Animacy'   => ['Anim', 'Hum', 'Nhum', 'Inan'],
        'NounClass' => ['Bantu1', 'Bantu2', 'Bantu3', 'Bantu4', 'Bantu5', 'Bantu6', 'Bantu7', 'Bantu8', 'Bantu9', 'Bantu10', 'Bantu11', 'Bantu12', 'Bantu13', 'Bantu14', 'Bantu15', 'Bantu16', 'Bantu17', 'Bantu18', 'Bantu19', 'Bantu20', 'Bantu21', 'Bantu22', 'Bantu23', 'Wol1', 'Wol2', 'Wol3', 'Wol4','Wol5', 'Wol6', 'Wol7', 'Wol8', 'Wol9', 'Wol10', 'Wol11', 'Wol12'],
        'Definite'  => ['Ind', 'Spec', 'Def', 'Cons', 'Com'],
        'Case'      => ['Nom', 'Abs', 'Acc', 'Erg', 'Gen', 'Dat', 'Ins', 'Par', 'Dis', 'Ess', 'Tra', 'Com', 'Abe', 'Cau', 'Ben', 'Cns', 'Equ', 'Cmp', 'Voc', 'Loc', 'Lat', 'Ter', 'Ine', 'Ill', 'Ela', 'Add', 'Ade', 'All', 'Abl', 'Sup', 'Spl', 'Del', 'Sub', 'Sbl', 'Sbe', 'Per', 'Tem'],
        'PrepCase'  => ['Npr', 'Pre'],
        'Polarity'  => ['Pos', 'Neg'],
        'Variant'   => ['Short', 'Long', 'Bound', 'Uncontr', 'Greek', 'Nomin', 'Vclause'],
        'Foreign'   => ['Yes'],
        'Abbr'      => ['Yes'],
        'Typo'      => ['Yes']
    );
    %feature_oval;
    my $oval = 1;
    for(my $i = 0; $i <= $#feature_order; $i += 2)
    {
        my $feature = $feature_order[$i];
        my $fvalues = $feature_order[$i+1];
        for(my $j = 0; $j <= $#{$fvalues}; $j++)
        {
            $feature_oval{$feature.'='.$fvalues->[$j]} = $oval++;
        }
        # Order unknown and empty values of the feature after all known values of the feature.
        $feature_oval{$feature} = $oval++;
    }
    # Order unknown fetures after all known features.
    $feature_oval{''} = $oval++;
}



#------------------------------------------------------------------------------
# Returns ordering value for a Feature=Value pair.
#------------------------------------------------------------------------------
sub get_foval
{
    my $fv = shift; # Feature=Value
    if(exists($feature_oval{$fv}))
    {
        return $feature_oval{$fv};
    }
    my $f = $fv;
    my $v = '';
    if($fv =~ m/^(.+?)=(.+)$/)
    {
        $f = $1;
        $v = $2;
    }
    if($v =~ m/,/)
    {
        my @ovals = map {get_foval("$f=$_")} (split(/,/, $v));
        my $sum = 0; map {$sum += $_} (@ovals);
        # Cache the result.
        $feature_oval{$fv} = $sum/scalar(@ovals);
        return $feature_oval{$fv};
    }
    if(exists($feature_oval{$f}))
    {
        # Cache the result.
        $feature_oval{$fv} = $feature_oval{$f};
        return $feature_oval{$fv};
    }
    # Cache the result.
    $feature_oval{$fv} = $feature_oval{''};
    return $feature_oval{$fv};
}



#------------------------------------------------------------------------------
# Sorts an array of Feature=Value pairs according to their ordering values.
#------------------------------------------------------------------------------
sub sort_features
{
    my @features = @_;
    return sort
    {
        my $r = get_foval($a) <=> get_foval($b);
        unless($r)
        {
            $r = $a cmp $b;
        }
        $r
    }
    (@features);
}



#------------------------------------------------------------------------------
# Sorts an array of annotations where each annotation is a string of Feature=
# Value pairs delimited by vertical bars (Feature1=Value1|Feature2=Value2...).
# The features within the annotation must be already ordered by their ovals,
# defined above, i.e., not alphabetically as in CoNLL-U! We don't sort them
# here because it could unnecessarily slow down the computation.
#
# Note: Even this implementation may be too slow, as we must deserialize the
# same annotation every time it is compared with another. A possible improve-
# ment could be to first find all possible annotation strings in our data,
# deserialize them, order the deserialized versions, then infer and remember
# ordering values of the serialized strings. Cached ordering values would be
# used throughout the rest of the script.
#------------------------------------------------------------------------------
sub sort_annotations
{
    my @annotations = @_; # each annotation is a string "Feat1=Val1|Feat2=Val2..."
    return sort
    {
        my @a = split(/\|/, $a);
        my @b = split(/\|/, $b);
        my $r;
        for(my $i = 0; $i <= $#feature_order; $i += 2)
        {
            my $aa = '';
            if($a[0] =~ m/^$feature_order[$i]=/)
            {
                $aa = shift(@a);
            }
            my $bb = '';
            if($b[0] =~ m/^$feature_order[$i]=/)
            {
                $bb = shift(@b);
            }
            $r = get_foval($aa) <=> get_foval($bb);
            last if($r);
        }
        unless($r)
        {
            $r = $a cmp $b;
        }
        $r
    }
    (@annotations);
}



#------------------------------------------------------------------------------
# Prints a table of M rows and N columns. Pads columns with spaces.
#------------------------------------------------------------------------------
sub print_table
{
    my $m = shift;
    my $n = shift;
    my @table = @_;
    # Find the maximum length of a value in each column.
    my @lengths;
    for(my $j = 0; $j < $n; $j++)
    {
        for(my $i = 0; $i < $m; $i++)
        {
            my $l = vlength($table[$i][$j]);
            if($l > $lengths[$j])
            {
                $lengths[$j] = $l;
            }
        }
    }
    # Now print it.
    for(my $i = 0; $i < $m; $i++)
    {
        for(my $j = 0; $j < $n; $j++)
        {
            print(' ') if($j>0);
            my $l = vlength($table[$i][$j]);
            my $pad = ' ' x ($lengths[$j]-$l);
            my $string = $table[$i][$j] =~ m/^[-+0-9\.,]+$/ ? $pad.$table[$i][$j] : $table[$i][$j].$pad;
            print($string);
        }
        print("\n");
    }
}



#------------------------------------------------------------------------------
# Estimate visual length of string, i.e., do not count combining diacritics.
# Note: This works in Putty to a Linux/UTF-8 machine. It does not work in
# Windows cmd.exe, which wrongly displays the combining character in its own
# slot.
#------------------------------------------------------------------------------
sub vlength
{
    my $x = shift;
    # Get rid of characters from the M category (combining diacritics belong here).
    unless($running_on_dans_laptop) # global variable
    {
        $x =~ s/\pM//g;
    }
    return length($x);
}
